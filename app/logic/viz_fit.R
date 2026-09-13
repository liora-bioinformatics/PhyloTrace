# app/logic/viz_fit.R
#
# The fitting vocabulary shared by the plots drawn as ggplot images (the Epi
# curve and the AMR views, and the rules the Tree was built on): one legibility
# floor, one reader-set text-size bias, one fixed canvas resolution, and the
# arithmetic that sizes a label to the room it actually has.
#
# Every engine that uses this fits its figure the same way:
#
#   as big as possible    - a label is set as large as the design asks for,
#                           times the reader's text size, but never larger
#                           than the room its slot has, so turning the text
#                           size up fills gaps and then stops. It cannot make
#                           two labels collide.
#   as small as necessary - a label shrinks only as far as `MIN_PRINT_PT`.
#                           Where the room will not hold even that, the label
#                           is left off the figure entirely rather than drawn
#                           as a smudge.
#
# And the figure is a physical object: each engine draws on a canvas of known
# inches at `PLOT_RES`, never at whatever width the browser reports, so the
# preview, the saved Analysis and the exported file are the same drawing.

box::use(
  rlang[`%||%`],
)

# --- Legibility --------------------------------------------------------------

#' Smallest type a printed figure should carry, in points.
#'
#' The number journals converge on: Nature, Science and PLOS set their floor
#' between 5 and 7 pt, and 5 is the common minimum for a label. Below it the
#' figure is not dense, it is unreadable on paper.
#' @export
MIN_PRINT_PT <- 5

#' Points per millimetre of glyph.
#'
#' ggplot2 sizes geom text in millimetres and theme text in points, so the two
#' have to be converted before a fitted size can be compared with a floor.
#' @export
PT_PER_MM <- 72 / 25.4

#' Clamp a number into a range.
#'
#' @param x Numeric.
#' @param lo,hi Numeric bounds.
#' @return Numeric.
#' @export
clamp <- function(x, lo, hi) min(max(x, lo), hi)

# --- The reader's own text size ----------------------------------------------
#
# One control over every piece of type on a figure, and over nothing else. It
# multiplies what each fitted label *asks* for; the room each label has is
# geometry, which does not move with it, so "does this still fit?" always has
# an answer.

#' Range the text-size bias is held to, as a multiplier.
#' @export
TEXT_SCALE_MIN <- 0.6

#' @export
TEXT_SCALE_MAX <- 2

#' Text size a figure is drawn at when the reader has not said otherwise.
#' @export
TEXT_SCALE_DEFAULT <- 1

#' The "Text size" slider's own units, in percent.
#'
#' The engines reason in a multiplier; the sidebar states it as a percentage,
#' because "110%" says what it does to the figure and "1.1" does not.
#' @export
TEXT_SIZE_DEFAULT <- 100

#' @export
TEXT_SIZE_MIN <- TEXT_SCALE_MIN * 100

#' @export
TEXT_SIZE_MAX <- TEXT_SCALE_MAX * 100

#' @export
TEXT_SIZE_STEP <- 5

#' A text-size bias, cleaned and held to its range.
#'
#' Anything unusable (NULL, NA, non-numeric, not positive) is the default, so
#' a snapshot saved before the control existed draws at the fitted size.
#'
#' @param k Numeric multiplier, 1 = the fitted size.
#' @return Numeric multiplier.
#' @export
text_scale <- function(k) {
  k <- suppressWarnings(as.numeric(k %||% TEXT_SCALE_DEFAULT))
  if (length(k) != 1L || is.na(k) || !is.finite(k) || k <= 0) {
    return(TEXT_SCALE_DEFAULT)
  }
  clamp(k, TEXT_SCALE_MIN, TEXT_SCALE_MAX)
}

#' The multiplier a "Text size" percentage stands for.
#'
#' @param pct Numeric percent, as the slider reports it.
#' @return Numeric multiplier.
#' @export
text_scale_percent <- function(pct) {
  text_scale(suppressWarnings(as.numeric(pct %||% TEXT_SIZE_DEFAULT)) / 100)
}

#' Type size for a label fitted to a slot, under the two rules.
#'
#' `want` is what the design asks for at the reader's text size, `room` the
#' largest size the slot can hold. The answer is `want` cut back to `room`,
#' and never shrunk past `floor` on its own account. When `room` itself is
#' under the floor the answer is `room` — below the floor, which is exactly
#' what `type_drawn()` tests for.
#'
#' @param want Numeric. Size the design asks for.
#' @param room Numeric. Largest size the slot holds; `Inf` for no limit.
#' @param floor Numeric. Smallest size worth reading.
#' @return Numeric, in whatever unit the three arguments share.
#' @export
fit_type <- function(want, room = Inf, floor = MIN_PRINT_PT) {
  room <- suppressWarnings(as.numeric(room))
  if (!length(room) || is.na(room)) {
    room <- Inf
  }
  if (room < floor) {
    return(room)
  }
  clamp(want, floor, room)
}

#' Whether a fitted label is worth drawing at the size it came out at.
#'
#' @param size Numeric. Size from `fit_type()`.
#' @param floor Numeric. Smallest size worth reading.
#' @return Logical.
#' @export
type_drawn <- function(size, floor = MIN_PRINT_PT) {
  isTRUE(length(size) == 1L && is.finite(size) && size >= floor - 1e-9)
}

#' The largest size in `[floor, want]` for which `fits(size)` holds.
#'
#' For the solves that have no closed form — a legend whose row count depends
#' on its own type size, a set of labels that pack into lanes. Bisected rather
#' than stepped, because shrinking often *changes the demand* (more keys fit a
#' column, fewer lanes are needed) and stepping into that feedback converges
#' slowly. Returns `floor` when even the floor does not fit; the caller decides
#' what that means (drop the element, grow the canvas).
#'
#' @param want Numeric. Size asked for.
#' @param fits Function of one size, returning TRUE when it fits.
#' @param floor Numeric. Smallest size allowed.
#' @param steps Integer. Bisection steps.
#' @return Numeric size.
#' @export
largest_fitting <- function(want, fits, floor = MIN_PRINT_PT, steps = 12L) {
  if (want <= floor || isTRUE(fits(want))) {
    return(max(want, floor))
  }
  if (!isTRUE(fits(floor))) {
    return(floor)
  }
  lo <- floor
  hi <- want
  for (i in seq_len(steps)) {
    mid <- (lo + hi) / 2
    if (isTRUE(fits(mid))) lo <- mid else hi <- mid
  }
  lo
}

#' Smallest drawn type on a figure, in points.
#'
#' @param ... Numeric sizes in points; NULL, NA and non-positive entries (a
#'   label that is not drawn) are ignored.
#' @return Numeric points, or Inf when nothing is drawn.
#' @export
min_type_pt <- function(...) {
  pt <- suppressWarnings(as.numeric(unlist(list(...))))
  pt <- pt[is.finite(pt) & pt > 0]
  if (length(pt)) min(pt) else Inf
}

#' What the export dialog says about a figure whose smallest type is under the
#' print floor.
#'
#' Legibility is a property of the figure, not of the export: the file is the
#' design as drawn. Said at export because that is when it starts to matter,
#' with the remedy that lives in the sidebar behind the dialog.
#'
#' @param pt Numeric. Smallest drawn type, from `min_type_pt()`.
#' @param remedy Character. What the reader can change to fix it.
#' @return Character sentence, or NULL when the figure prints legibly.
#' @export
legibility_note <- function(pt, remedy = NULL) {
  if (!isTRUE(is.finite(pt)) || pt >= MIN_PRINT_PT) {
    return(NULL)
  }
  paste(
    sprintf(
      "Smallest text prints at %.1f pt — under the %g pt most journals ask for.",
      pt,
      MIN_PRINT_PT
    ),
    remedy %||% ""
  )
}

# --- Measuring text ----------------------------------------------------------

#' Mean character advance, in ems, for labels nobody has typed yet.
#'
#' A mean is the right measure for a reserve that has to cover a column of
#' labels of a fixed shape (an accession, a gene symbol). A single known string
#' is measured with `string_em()` instead.
#' @export
MEAN_CHAR_EM <- 0.6

# Helvetica's own advances, which the export devices' sans faces are within a
# percent of. A mean runs a fifth short on an all-capital word, and a fifth
# short of a slot is letters drawn past its edge.
CHAR_EM <- c(
  " " = 0.278, "!" = 0.278, "\"" = 0.355, "#" = 0.556, "$" = 0.556,
  "%" = 0.889, "&" = 0.667, "'" = 0.191, "(" = 0.333, ")" = 0.333,
  "*" = 0.389, "+" = 0.584, "," = 0.278, "-" = 0.333, "." = 0.278,
  "/" = 0.278,
  "0" = 0.556, "1" = 0.556, "2" = 0.556, "3" = 0.556, "4" = 0.556,
  "5" = 0.556, "6" = 0.556, "7" = 0.556, "8" = 0.556, "9" = 0.556,
  ":" = 0.278, ";" = 0.278, "<" = 0.584, "=" = 0.584, ">" = 0.584,
  "?" = 0.556, "@" = 1.015,
  "A" = 0.667, "B" = 0.667, "C" = 0.722, "D" = 0.722, "E" = 0.667,
  "F" = 0.611, "G" = 0.778, "H" = 0.722, "I" = 0.278, "J" = 0.5,
  "K" = 0.667, "L" = 0.556, "M" = 0.833, "N" = 0.722, "O" = 0.778,
  "P" = 0.667, "Q" = 0.778, "R" = 0.722, "S" = 0.667, "T" = 0.611,
  "U" = 0.722, "V" = 0.667, "W" = 0.944, "X" = 0.667, "Y" = 0.667,
  "Z" = 0.611,
  "[" = 0.278, "\\" = 0.278, "]" = 0.278, "^" = 0.469, "_" = 0.556,
  "`" = 0.333,
  "a" = 0.556, "b" = 0.556, "c" = 0.5, "d" = 0.556, "e" = 0.556,
  "f" = 0.278, "g" = 0.556, "h" = 0.556, "i" = 0.222, "j" = 0.222,
  "k" = 0.5, "l" = 0.222, "m" = 0.833, "n" = 0.556, "o" = 0.556,
  "p" = 0.556, "q" = 0.556, "r" = 0.333, "s" = 0.5, "t" = 0.278,
  "u" = 0.556, "v" = 0.5, "w" = 0.722, "x" = 0.5, "y" = 0.5,
  "z" = 0.5,
  "{" = 0.334, "|" = 0.26, "}" = 0.334, "~" = 0.584,
  "…" = 1.0
)

# An unknown character (a Greek letter, an en dash, a CJK glyph) is booked at
# the widest a character gets: too wide costs a little white space, too narrow
# loses the end of the label.
CHAR_EM_UNKNOWN <- 1

#' Ems each string sets in, measured character by character.
#'
#' @param x Character vector.
#' @return Numeric vector, one width per string.
#' @export
string_em <- function(x) {
  x <- as.character(x %||% character(0))
  vapply(
    x,
    function(s) {
      if (is.na(s) || !nzchar(s)) {
        return(0)
      }
      em <- CHAR_EM[strsplit(s, "", fixed = TRUE)[[1]]]
      em[is.na(em)] <- CHAR_EM_UNKNOWN
      sum(em)
    },
    numeric(1),
    USE.NAMES = FALSE
  )
}

#' Inches the widest of some strings sets in at a type size.
#'
#' Multi-line strings are measured by their longest line.
#'
#' @param x Character vector.
#' @param pt Numeric points.
#' @return Numeric inches; 0 for no strings.
#' @export
text_width_in <- function(x, pt) {
  lines <- unlist(strsplit(as.character(x %||% character(0)), "\n", fixed = TRUE))
  if (!length(lines)) {
    return(0)
  }
  max(string_em(lines)) * pt / 72
}

#' Inches one line of type takes vertically, leading included.
#'
#' @param pt Numeric points.
#' @return Numeric inches.
#' @export
line_height_in <- function(pt) pt * 1.2 / 72

# --- The canvas --------------------------------------------------------------

#' Pixels per inch every fixed-canvas plot is rasterised at on screen.
#'
#' Twice the CSS reference, so the image stays sharp on a HiDPI display, and
#' fixed rather than read from the browser: a size report from the client is
#' not allowed to trigger a redraw, and a figure drawn at a known resolution
#' looks the same on every screen it is opened on.
#' @export
PLOT_RES <- 192

#' Ceiling on either side of an on-screen image, in pixels.
#' @export
PLOT_MAX_PX <- 12000

#' Tallest aspect ratio a figure may be drawn at, as height over width.
#'
#' The sidebar sliders' ceiling and the fits' own: a thousand isolates need a
#' tall page to give each one a row that can be told apart, and a fit that
#' stopped short of what the slider allows would draw rows half the height its
#' type was chosen for.
#' @export
ASPECT_MAX <- 8

#' How far an engine may grow its canvas past its base width for what it has to
#' set beside the drawing (annotation columns, labels, a guide box).
#' @export
CANVAS_MAX_FACTOR <- 2.6

#' Pixels a canvas side of `inches` is drawn at.
#'
#' @param inches Numeric.
#' @return Integer pixels, capped at `PLOT_MAX_PX`.
#' @export
canvas_px <- function(inches) {
  as.integer(min(PLOT_MAX_PX, max(1, round(inches * PLOT_RES))))
}
