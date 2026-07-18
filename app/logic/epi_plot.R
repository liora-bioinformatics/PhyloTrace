# app/logic/epi_plot.R
#
# Epidemiological-curve computation and plot building for
# app/view/visualization_epi.R. Follows the split every other engine uses
# (app/logic/phylo.R for the MST, app/logic/tree_plot.R for the Tree): the data
# reshaping, palette and plot construction live here so the view module only
# wires reactives to them, and the pure parts stay unit-testable without Shiny.
#
# The plot is a ggplot2 object rendered by shiny::renderPlot, exactly like the
# Tree engine — so the same export path (ggsave) gives real PNG/JPEG/PDF/SVG
# rather than HTML only. Playback is driven by the view's own tick loop rather
# than gganimate: each step just rebuilds the curve with `reveal_to` set one
# interval further on, which needs no renderer and stays responsive.
#
# The curve follows the conventions in
# https://outbreaktools.ca/background/epidemic-curves/ :
#   * x = date of collection, y = number of cases, counts are whole cases, so
#     the y axis only ever carries integer breaks (see .count_breaks).
#   * "Bars should touch each other (unless there are periods of time with no
#     cases)" — hence every bar is exactly one interval wide; a gap in the curve
#     then means a genuine gap in the data rather than a drawing artefact.
#   * The unit-block style draws one cell per case stacked into its interval's
#     column, so the reader can count cases straight off the plot.

box::use(
  RColorBrewer[brewer.pal, brewer.pal.info],
  dplyr[arrange, group_by, n, summarise],
  ggplot2[
    .data,
    aes,
    annotate,
    coord_fixed,
    element_blank,
    element_rect,
    element_text,
    expansion,
    geom_col,
    geom_step,
    geom_text,
    geom_tile,
    ggplot,
    ggsave,
    guide_legend,
    guides,
    labs,
    scale_colour_manual,
    scale_fill_manual,
    scale_x_date,
    scale_y_continuous,
    sec_axis,
    theme,
    theme_minimal,
  ],
  grDevices[col2rgb, colorRampPalette, rgb],
  grid[unit],
  stats[ave, setNames],
  viridisLite[viridis],
)
box::use(
  app / logic / viz_helpers[color_scales],
)

`%||%` <- function(a, b) if (is.null(a)) b else a

# Stratum label used when no metadata field is mapped (a plain total curve),
# and for isolates whose mapped field is empty/NA. Exported so the view module
# and the tests agree on them rather than re-spelling the strings.
#' @export
EPI_ALL_LABEL <- "All isolates"

#' @export
EPI_UNKNOWN_LABEL <- "(unknown)"

# Separator joining the values of a multi-field stratification into one
# composite category ("E. coli | Germany").
#' @export
EPI_STRATUM_SEP <- " | "

# The collection-date column every PhyloTrace database carries (see
# make_metadata_table in app/logic/database_functions.R). The engine plots this
# field and no other, so it lives here rather than being a user-facing choice.
#' @export
EPI_DATE_FIELD <- "sample_collection_date"

# --- date binning ------------------------------------------------------------

# The bin widths on offer, smallest first. Ordered, because the adaptive
# default walks them from the finest upwards and takes the first that fits.
# Single source of truth: the view builds its "Time interval" select from this.
#' @export
EPI_INTERVALS <- c(Day = "day", Week = "week", Month = "month", Year = "year")

# Above this many bars the curve stops being readable: the bars thin to
# hairlines and the shape of the outbreak is lost in the noise.
#' @export
EPI_MAX_BARS <- 150L

# Used when there is no date to fit against (an empty or undated database).
#' @export
EPI_INTERVAL_FALLBACK <- "week"

# Round collection dates down to the start of their bin. Uses the same
# Day/Week/Month/Year vocabulary the Map's timeline already offers, so the two
# engines describe time identically. Weeks start Monday (ISO-8601, "%u" is
# 1=Monday). NA in, NA out: `sample_collection_date` is a free-text TEXT column
# and unparseable values are expected — callers drop them (see build_epi_data).
#' @export
floor_date_bin <- function(dates, interval = "day") {
  d <- as.Date(dates)
  switch(
    tolower(interval),
    day = d,
    week = d - (as.numeric(format(d, "%u")) - 1),
    month = as.Date(format(d, "%Y-%m-01")),
    year = as.Date(format(d, "%Y-01-01")),
    stop("Unknown interval: ", interval, call. = FALSE)
  )
}

# Nominal width of one bin, in days. Months and years are averages, so this is
# only good for sizing decisions that need a single number (fitting the
# interval, the square aspect ratio) — never for drawing a bar. See
# exact_bin_widths().
#' @export
bin_width_days <- function(interval) {
  switch(
    tolower(interval %||% "day"),
    day = 1,
    week = 7,
    month = 30.4,
    year = 365.25,
    1
  )
}

# The true width of each bin, in days: the distance to where the next one
# starts.
#
# Bars have to touch (the article's rule) without overlapping, and a month is
# not 30.4 days long — drawing February 30.4 days wide runs it two days into
# March, which ggplot rightly complains about ("position_stack() requires
# non-overlapping x intervals") and which stacks the overlap wrongly. Days and
# weeks are genuinely constant; months and years are measured.
#' @export
exact_bin_widths <- function(dates, interval) {
  unit <- tolower(interval %||% "day")
  if (unit %in% c("day", "week")) {
    return(rep(bin_width_days(unit), length(dates)))
  }
  vapply(
    dates,
    function(d) as.numeric(seq(d, by = unit, length.out = 2)[2] - d),
    numeric(1)
  )
}

# How many bars an interval would put on the axis for a given date span.
#
# Counts the *slots* across the span, not the bins that happen to be occupied.
# That is deliberate: the x axis is a continuous date axis, so the bars are
# drawn one interval wide against the whole span. Eighty isolates scattered over
# three years occupy only 80 daily bins, but each is 1/1100th of the axis — the
# hairlines you get from binning too finely. It's the span-to-width ratio that
# decides whether a curve is readable, so that is what gets measured.
.n_slots <- function(dates, interval) {
  span <- as.numeric(diff(range(dates)))
  if (!is.finite(span)) {
    return(NA_real_)
  }
  ceiling(span / bin_width_days(interval)) + 1
}

# The finest interval that keeps the curve under `max_bars` bars.
#
# Epi curves are read by shape, and the interval that shows it best depends on
# how far the cases are spread (the article's rule of thumb ties it to the
# incubation period and "the length of time over which cases are distributed").
# Walking from the finest interval upwards keeps as much detail as the span
# allows: a month-long outbreak lands on days, a decade of surveillance on
# months. Falls back to the coarsest interval when even that overflows — a
# century of data has nowhere finer to go.
#' @export
epi_default_interval <- function(dates, max_bars = EPI_MAX_BARS) {
  d <- .parse_dates(dates)
  d <- d[!is.na(d)]
  if (!length(d)) {
    return(EPI_INTERVAL_FALLBACK)
  }
  intervals <- unname(EPI_INTERVALS)
  for (iv in intervals) {
    if (isTRUE(.n_slots(d, iv) <= max_bars)) {
      return(iv)
    }
  }
  intervals[length(intervals)]
}

# Parse a metadata column to Date, tolerating the two ways this fails.
# as.Date.character() returns NA for values it can't read *only as long as at
# least one value in the vector parses* — hand it a column where none do (an
# isolate name, an ST number) and it throws "character string is not in a
# standard unambiguous format" instead. Since the column is free text, a column
# that is entirely unparseable has to come back as all-NA rather than abort.
.parse_dates <- function(x) {
  if (inherits(x, "Date")) {
    return(x)
  }
  tryCatch(
    suppressWarnings(as.Date(as.character(x))),
    error = function(e) rep(as.Date(NA), length(x))
  )
}

# --- data reshaping ----------------------------------------------------------

# Collapse the mapped metadata fields into one composite category per isolate.
# Zero fields is the "no stratification" case (a single total series); one field
# behaves as a plain colour-by; two or more join their values with
# EPI_STRATUM_SEP. Empty strings are treated like NA — a metadata field the user
# never filled in reads as unknown, not as a category named "".
epi_stratum <- function(meta, stratify_by) {
  if (!length(stratify_by)) {
    return(rep(EPI_ALL_LABEL, nrow(meta)))
  }
  parts <- lapply(stratify_by, function(f) {
    v <- as.character(meta[[f]])
    v[is.na(v) | !nzchar(trimws(v))] <- EPI_UNKNOWN_LABEL
    v
  })
  do.call(paste, c(parts, list(sep = EPI_STRATUM_SEP)))
}

.empty_epi_data <- function(dropped = 0L) {
  out <- data.frame(
    date_bin = as.Date(character()),
    stratum = character(),
    count = integer(),
    stringsAsFactors = FALSE
  )
  attr(out, "dropped") <- dropped
  out
}

# Bin isolates into (date_bin, stratum) counts.
#
# Returns a data frame of date_bin / stratum / count, carrying a "dropped"
# attribute: how many isolates were discarded because their date was NA or
# didn't parse. The view module surfaces that count rather than silently
# under-reporting the curve.
#' @export
build_epi_data <- function(
  meta,
  date_field = EPI_DATE_FIELD,
  stratify_by = character(),
  interval = "day"
) {
  if (
    is.null(meta) ||
      !nrow(meta) ||
      is.null(date_field) ||
      !length(date_field) ||
      !date_field %in% names(meta)
  ) {
    return(.empty_epi_data())
  }

  parsed <- .parse_dates(meta[[date_field]])
  keep <- !is.na(parsed)
  dropped <- sum(!keep)
  if (!any(keep)) {
    return(.empty_epi_data(dropped))
  }
  meta <- meta[keep, , drop = FALSE]
  parsed <- parsed[keep]

  # Ignore fields that aren't actually in this database's metadata (a stale
  # selection surviving a database switch).
  stratify_by <- intersect(stratify_by, names(meta))

  binned <- data.frame(
    date_bin = floor_date_bin(parsed, interval),
    stratum = epi_stratum(meta, stratify_by),
    stringsAsFactors = FALSE
  )

  out <- binned |>
    group_by(date_bin, stratum) |>
    summarise(count = n(), .groups = "drop") |>
    arrange(date_bin, stratum) |>
    as.data.frame()

  attr(out, "dropped") <- dropped
  out
}

# Running total per stratum. Every stratum is evaluated at every bin (missing
# combinations count 0) so each line is monotone and the strata stay comparable
# at any x — a cumulative series that only had points where that stratum
# happened to occur would jump across gaps and misread.
#' @export
epi_cumulate <- function(binned) {
  if (is.null(binned) || !nrow(binned)) {
    return(.empty_epi_data(attr(binned, "dropped") %||% 0L))
  }
  grid <- expand.grid(
    date_bin = sort(unique(binned$date_bin)),
    stratum = sort(unique(binned$stratum)),
    stringsAsFactors = FALSE
  )
  out <- merge(grid, binned, by = c("date_bin", "stratum"), all.x = TRUE)
  out$count[is.na(out$count)] <- 0L
  out <- out[order(out$stratum, out$date_bin), , drop = FALSE]
  out$count <- as.integer(ave(out$count, out$stratum, FUN = cumsum))
  rownames(out) <- NULL
  attr(out, "dropped") <- attr(binned, "dropped") %||% 0L
  out
}

# The bins in play, in order. The view's playback walks these.
#' @export
epi_bins <- function(binned) {
  if (is.null(binned) || !nrow(binned)) {
    return(as.Date(character()))
  }
  sort(unique(binned$date_bin))
}

# --- palette -----------------------------------------------------------------

.viridis_scales <- color_scales$Gradient

# Map category levels to hex colours. Mirrors the shape of phylo.R's private
# mst_palette(), but resolves the ColorBrewer names too, so the same helper can
# serve a qualitative palette and a continuous ramp.
# brewer.pal caps out at 8-12 colours depending on the palette, so anything
# wider is interpolated rather than recycled — repeating a colour would make two
# different strata look identical in the legend.
#' @export
epi_palette <- function(cats, scale = "Set2") {
  n <- length(cats)
  if (!n) {
    return(setNames(character(), character()))
  }
  scale <- scale %||% "Set2"

  cols <- if (scale %in% .viridis_scales) {
    viridis(n, option = scale)
  } else if (scale %in% rownames(brewer.pal.info)) {
    # brewer.pal() errors below 3 even when fewer are asked for; take the
    # smallest legal ramp and trim.
    max_n <- brewer.pal.info[scale, "maxcolors"]
    base <- brewer.pal(max(3L, min(max_n, n)), scale)
    if (n <= length(base)) base[seq_len(n)] else colorRampPalette(base)(n)
  } else {
    viridis(n)
  }

  setNames(cols[seq_len(n)], cats)
}

# Which colour scales can actually carry `n_strata` categories.
#
# The other engines restrict their scale pickers by the mapped variable's *type*
# (viz_helpers.R's suitable_scale_categories: a factor gets Qualitative). That is
# the wrong axis here, because an epi curve's strata are always categorical, but
# there can be any number of them: stratify 341 isolates by country and you ask
# a nine-swatch palette like Set1 to cover forty values. What comes back is
# forty interpolated neighbours nobody can tell apart.
#
# So filter by cardinality instead: only offer a qualitative palette that has at
# least as many real swatches as there are strata (Set2 stops at 8, Set3 and
# Paired at 12 — see brewer.pal.info), and always offer the continuous ramps,
# which are defined for any n. The order a gradient implies over countries is
# arbitrary, but an arbitrary order you can read beats a dozen identical pinks.
#' @export
epi_scale_choices <- function(n_strata) {
  qualitative <- color_scales$Qualitative
  fits <- qualitative[vapply(
    qualitative,
    function(p) brewer.pal.info[p, "maxcolors"] >= n_strata,
    logical(1)
  )]

  out <- list()
  if (length(fits)) {
    out[["Qualitative"]] <- fits
  }
  out[["Gradient"]] <- color_scales$Gradient
  out[["Sequential"]] <- color_scales$Sequential
  out
}

# Coerce a chosen scale to one that can carry `n_strata`, keeping it when it
# already can. The picker is kept in step separately (apply_scale_choices in the
# view), but the plot resolves this itself so it can never draw with a scale the
# data has outgrown — the picker's correction only lands a client round-trip
# later, which would otherwise leave one frame rendered in mush.
#' @export
epi_fit_scale <- function(scale, n_strata) {
  valid <- unlist(epi_scale_choices(n_strata), use.names = FALSE)
  if (isTRUE(scale %in% valid)) scale else valid[1]
}

# --- timeline annotations ----------------------------------------------------

# The colour an annotation falls back to when none was stored on it (older
# rows, or a call site that hasn't set one). The orange the period wash used
# before annotations became individually colourable.
#' @export
EPI_ANNO_COLOR_DEFAULT <- "#f39c12"

#' @export
empty_epi_annotations <- function() {
  data.frame(
    id = character(),
    type = character(),
    start = as.Date(character()),
    end = as.Date(character()),
    label = character(),
    color = character(),
    stringsAsFactors = FALSE
  )
}

# What the eye actually sees when `fg` is drawn at opacity `alpha` (0..1) over
# `bg`: the alpha-composited hex. Used to judge the contrast of label text on a
# semi-transparent wash, where the true backdrop is the blend, not `fg` itself.
.blend <- function(fg, bg, alpha) {
  f <- col2rgb(fg)
  b <- col2rgb(bg)
  mix <- f * alpha + b * (1 - alpha)
  rgb(mix[1], mix[2], mix[3], maxColorValue = 255)
}

# Black or white text, whichever stays legible on `bg` — so a label keeps its
# contrast whatever colour the user picks. Thresholds the perceived luminance
# (ITU-R BT.601): a light backdrop gets black text, a dark one white.
.contrast_text <- function(bg) {
  c <- col2rgb(bg)
  luma <- (0.299 * c[1] + 0.587 * c[2] + 0.114 * c[3]) / 255
  if (luma > 0.55) "#000000" else "#ffffff"
}

# The colour stored on annotation row `i`, falling back to the default when the
# column is absent (an empty frame) or the value is missing/blank.
.anno_color <- function(annos, i) {
  col <- if ("color" %in% names(annos)) annos$color[i] else NA_character_
  if (is.na(col) || !nzchar(col)) EPI_ANNO_COLOR_DEFAULT else col
}

# Approx. rendered width of one label character, in px, for geom text drawn at
# `size` (ggplot's mm-based size, so the point size is size * .pt). At renderPlot's
# 96 dpi a point is 96/72 px; ~0.5 em wide per character. Mirrors the estimate
# epi_legend_ncol makes for the legend.
.GG_PT <- 72.27 / 25.4
.anno_char_px <- function(size = 3) size * .GG_PT * (96 / 72) * 0.5

# Fit `label` into a period's bracket by wrapping it over up to `max_lines`
# lines that each hold `max_chars` characters, so a long label uses the
# bracket's height rather than bleeding past the dates it marks. Wraps on spaces
# where it can and hard-breaks a single over-long token; when even the wrapped
# text won't fit, the last line is truncated with an ellipsis. Returns a string
# with embedded newlines, which annotate("text") renders as separate lines.
.wrap_label <- function(label, max_chars, max_lines = 2L) {
  max_chars <- max(1L, as.integer(max_chars))
  if (nchar(label) <= max_chars) {
    return(label)
  }
  # strwrap breaks on whitespace but leaves an over-long word on its own line
  # untouched, so hard-break any line that is still wider than the bracket.
  lines <- unlist(lapply(strwrap(label, width = max_chars), function(l) {
    if (nchar(l) <= max_chars) {
      return(l)
    }
    starts <- seq(1L, nchar(l), by = max_chars)
    substring(l, starts, pmin(starts + max_chars - 1L, nchar(l)))
  }))
  if (length(lines) > max_lines) {
    lines <- lines[seq_len(max_lines)]
    last <- lines[max_lines]
    lines[max_lines] <- if (nchar(last) >= max_chars && max_chars >= 2L) {
      paste0(substr(last, 1L, max_chars - 1L), "…")
    } else {
      paste0(last, "…")
    }
  }
  paste(lines, collapse = "\n")
}

# Fraction of the plot's height one annotation lane occupies, measured down from
# the top.
.lane_height <- 0.075
.max_lanes <- 6L

# How much of the plotted x span a label is assumed to occupy. Collisions are a
# *screen* phenomenon: two milestones 15 days apart overlap completely on a plot
# spanning ten years but not on one spanning a month, so the clearance has to
# scale with the span rather than being a fixed number of days.
.label_pad_frac <- 0.07

ANNO_PERIOD <- "period"

# Is this row a shaded time period (as opposed to a milestone line)? A period
# with no end date can't be shaded, so it degrades to a milestone.
.is_period <- function(annos, i) {
  identical(annos$type[i], ANNO_PERIOD) && !is.na(annos$end[i])
}

# Greedy first-fit lane packing over [left, right] extents, which must be sorted
# by `left`. An annotation drops to the first lane whose occupant ends before it
# starts, so non-overlapping annotations share the top lane and only genuinely
# clashing ones are pushed down. This is what keeps two identical or partly
# intersecting periods readable: they can never land in the same lane, so their
# brackets and labels are always drawn one above the other.
.assign_lanes <- function(left, right) {
  lanes <- integer(length(left))
  lane_right <- numeric(0)
  for (i in seq_along(left)) {
    placed <- FALSE
    for (k in seq_along(lane_right)) {
      if (lane_right[k] < left[i]) {
        lanes[i] <- k
        lane_right[k] <- right[i]
        placed <- TRUE
        break
      }
    }
    if (!placed) {
      lane_right <- c(lane_right, right[i])
      lanes[i] <- length(lane_right)
    }
  }
  lanes
}

# The x span the lanes are laid out against: the plotted data's range where
# there is one, widened to cover any annotation outside it.
.anno_span <- function(annos, x_range) {
  xs <- c(
    as.numeric(annos$start),
    as.numeric(annos$end[!is.na(annos$end)]),
    as.numeric(x_range)
  )
  span <- diff(range(xs, na.rm = TRUE))
  if (!is.finite(span) || span <= 0) 1 else span
}

# Assign each annotation a lane so that no two labels overlap.
#
# Returns the annotations with `lane` (1-based) and `period` columns. Pure
# geometry — no ggplot involved — which keeps the packing testable independently
# of how it ends up being drawn.
#' @export
epi_annotation_lanes <- function(annos, x_range = NULL) {
  if (is.null(annos) || !nrow(annos)) {
    out <- empty_epi_annotations()
    out$lane <- integer()
    out$period <- logical()
    return(out)
  }
  annos <- annos[order(annos$start), , drop = FALSE]
  pad <- .anno_span(annos, x_range) * .label_pad_frac

  annos$period <- vapply(
    seq_len(nrow(annos)),
    function(i) {
      .is_period(annos, i)
    },
    logical(1)
  )

  # A milestone's label extends to the right of its line; a period's bracket
  # spans its dates, but never less than a label's worth of room.
  left <- as.numeric(annos$start)
  right <- vapply(
    seq_len(nrow(annos)),
    function(i) {
      end <- if (annos$period[i]) as.numeric(annos$end[i]) else left[i]
      max(end, left[i] + pad)
    },
    numeric(1)
  )

  annos$lane <- .assign_lanes(left, right)
  rownames(annos) <- NULL
  annos
}

# Draw the annotations onto a curve whose y axis tops out at `y_max`.
#
# A period is a full-height wash — so the bars inside it stay readable, and two
# overlapping washes simply darken where they intersect — plus an opaque bracket
# in its own lane carrying the label. The bracket is what disambiguates
# overlapping periods: the wash may be shared, but each period's extent and
# label are drawn once, at their own height.
#
# `background` is the plot's own background colour, and `xlim`/`plot_width` the
# drawn x-axis extent (Dates) and panel width (px): together they let a period's
# label be trimmed to the bracket it sits in, and its colour chosen for contrast
# against the wash (which is the user's annotation colour, so it can be any
# lightness). Both degrade gracefully when unknown.
.annotation_layers <- function(
  annos,
  x_range,
  y_max,
  background = "#ffffff",
  xlim = NULL,
  plot_width = NULL
) {
  lanes <- epi_annotation_lanes(annos, x_range)
  if (!nrow(lanes)) {
    return(list())
  }
  # Lanes are placed in data units, against the headroom the y scale leaves.
  top <- max(y_max, 1)
  layers <- list()

  # For fitting a period label to its bracket: how many px the panel is wide,
  # and how wide one label character is, so a bracket's date-span can be turned
  # into a character budget (see .wrap_label). The panel is narrower than
  # the whole output by the y axis and margins.
  char_px <- .anno_char_px(3)
  panel_px <- (plot_width %||% 900) - 70
  axis_days <- if (!is.null(xlim)) as.numeric(diff(range(xlim))) else NA_real_

  for (i in seq_len(nrow(lanes))) {
    lane <- ((lanes$lane[i] - 1L) %% .max_lanes)
    y_top <- top * (1 - lane * .lane_height)
    y_bot <- y_top - top * .lane_height * 0.72
    y_mid <- (y_top + y_bot) / 2

    # The user's chosen colour for this annotation.
    col <- .anno_color(lanes, i)

    if (lanes$period[i]) {
      # The label sits on the bracket — a wash of `col` over the background — so
      # its colour is chosen for contrast against that blend, not against `col`
      # alone (a dark pick would otherwise get near-black text on a dark wash).
      label_col <- .contrast_text(.blend(col, background, 0.55))
      # And it is wrapped to the bracket's own width, so a long label uses the
      # bracket's height instead of spilling past the dates it describes.
      label <- lanes$label[i]
      if (is.finite(axis_days) && axis_days > 0) {
        bracket_days <- as.numeric(lanes$end[i] - lanes$start[i])
        avail_px <- (bracket_days / axis_days) * panel_px
        label <- .wrap_label(label, floor(avail_px / char_px), max_lines = 2L)
      }
      # Grow the bracket downward to enclose every wrapped line: text spilling
      # onto the fainter full-height wash (0.10) would lose the contrast the
      # bracket blend (0.55) was chosen against, so it must stay on the bracket.
      n_lines <- length(strsplit(label, "\n", fixed = TRUE)[[1]])
      if (n_lines > 1L) {
        y_bot <- y_top - (y_top - y_bot) * n_lines
        y_mid <- (y_top + y_bot) / 2
      }
      layers <- c(
        layers,
        list(
          annotate(
            "rect",
            xmin = lanes$start[i],
            xmax = lanes$end[i],
            ymin = -Inf,
            ymax = Inf,
            fill = col,
            alpha = 0.10
          ),
          annotate(
            "rect",
            xmin = lanes$start[i],
            xmax = lanes$end[i],
            ymin = y_bot,
            ymax = y_top,
            fill = col,
            alpha = 0.55
          ),
          annotate(
            "text",
            x = lanes$start[i] + (lanes$end[i] - lanes$start[i]) / 2,
            y = y_mid,
            label = label,
            colour = label_col,
            size = 3,
            vjust = 0.5
          )
        )
      )
    } else {
      # A milestone's label sits beside its line on the plain background, not on
      # a wash, so it reads in the annotation's own colour — matching the line —
      # and extends freely to the right (it stands in for a legend entry).
      layers <- c(
        layers,
        list(
          annotate(
            "segment",
            x = lanes$start[i],
            xend = lanes$start[i],
            y = -Inf,
            yend = y_mid,
            colour = col,
            linetype = "dashed",
            linewidth = 0.4
          ),
          annotate(
            "text",
            x = lanes$start[i],
            y = y_mid,
            label = paste0(" ", lanes$label[i]),
            colour = col,
            size = 3,
            hjust = 0,
            vjust = 0.5
          )
        )
      )
    }
  }
  layers
}

# --- plot building -----------------------------------------------------------

.date_labels <- function(interval) {
  switch(
    tolower(interval %||% "day"),
    day = "%d %b %Y",
    week = "%d %b %Y",
    month = "%b %Y",
    year = "%Y",
    "%b %Y"
  )
}

# Breaks for a case-count axis. Cases are whole things, so a fractional break
# ("0.6 isolates") is meaningless — this picks a 1/2/5/10-style step that is
# always a whole number, which keeps every break an integer even when the
# tallest bar is 1.
.count_step <- function(maxv) {
  if (!is.finite(maxv) || maxv <= 6) {
    return(1)
  }
  raw <- maxv / 6
  mag <- 10^floor(log10(raw))
  for (m in c(1, 2, 5, 10)) {
    if (raw <= m * mag) {
      return(max(1, m * mag))
    }
  }
  max(1, 10 * mag)
}

#' @export
count_breaks <- function(maxv) {
  seq(0, max(1, ceiling(maxv)), by = .count_step(maxv))
}

# How many columns the bottom legend can have before it overflows the plot.
#
# guide_legend(ncol =) is rigid: ggplot honours the column count even when the
# resulting legend is wider than the device, so a fixed count with long stratum
# names (full institution names, say) runs off both edges of the panel and is
# clipped — which no time-window or aspect change fixes, because the legend
# always carries every category. Estimate instead how many of the *widest*
# label fit across `width_px`, so the legend wraps to as many rows as it needs
# rather than being cut off. `width_px` NULL (before the browser reports the
# panel width) falls back to a typical panel so the first draw is still sane.
#' @export
epi_legend_ncol <- function(cats, width_px = NULL, base_size = 13) {
  n <- length(cats)
  if (n <= 1L) {
    return(1L)
  }
  w <- if (is.null(width_px) || !is.finite(width_px) || width_px <= 0) {
    900
  } else {
    width_px
  }
  # Rendered width of one entry, in px at renderPlot's 96 dpi: the colour key
  # and its padding, plus ~half an em per character of the widest label.
  # legend.text is ~0.8 * base_size pt, and a point is 96/72 px at res = 96.
  char_px <- base_size * 0.8 * (96 / 72) * 0.5
  entry_px <- 34 + char_px * max(nchar(cats))
  cols <- floor(w / entry_px)
  max(1L, min(n, as.integer(cols)))
}

# The guide the strata scales carry: a wrapped legend when it should show, or
# "none" to suppress it. Applied to both fill and colour in build_epi_ggplot so
# whichever aesthetic a mode maps the strata to (fill for the bars, colour for
# the cumulative curve) is governed identically — the legend can neither leak in
# when hidden nor render with the wrong column count when shown.
.strata_guide <- function(show_legend, cats, plot_width) {
  if (show_legend) {
    guide_legend(ncol = epi_legend_ncol(cats, plot_width))
  } else {
    "none"
  }
}

# Expand each (date_bin, stratum, count) row into `count` unit rows, numbered
# 1..n *within a date bin* so the cells stack into one column per interval.
# Rows are pre-sorted by stratum so each bin's categories form contiguous bands
# rather than interleaving.
.unit_blocks <- function(binned) {
  blocks <- binned[binned$count > 0, , drop = FALSE]
  if (!nrow(blocks)) {
    return(data.frame(
      date_bin = as.Date(character()),
      stratum = character(),
      y_idx = integer(),
      stringsAsFactors = FALSE
    ))
  }
  blocks <- blocks[order(blocks$date_bin, blocks$stratum), , drop = FALSE]
  expanded <- blocks[
    rep(seq_len(nrow(blocks)), blocks$count),
    c("date_bin", "stratum"),
    drop = FALSE
  ]
  expanded$y_idx <- ave(
    seq_len(nrow(expanded)),
    expanded$date_bin,
    FUN = seq_along
  )
  expanded
}

.epi_theme <- function(background, text_color) {
  theme_minimal(base_size = 13) +
    theme(
      # No gridlines: the bars are the data, and a grid behind them is just ink.
      panel.grid = element_blank(),
      panel.background = element_rect(fill = background, colour = NA),
      plot.background = element_rect(fill = background, colour = NA),
      legend.background = element_rect(fill = background, colour = NA),
      legend.key = element_rect(fill = background, colour = NA),
      text = element_text(colour = text_color),
      axis.text = element_text(colour = text_color),
      axis.title = element_text(colour = text_color),
      legend.position = "bottom",
      legend.title = element_blank(),
      legend.key.size = unit(0.8, "lines"),
      plot.margin = unit(c(6, 12, 6, 6), "pt")
    )
}

# Build the epi-curve ggplot.
#
# `opts`: mode ("stacked" | "cumulative"), square, col_scale, single_color,
# cumulative_color, background, text_color, interval, annos, reveal_from,
# reveal_to, fixed_axis, label_ends, plot_width. `col_scale` colours the strata when
# stratified; `single_color` colours the lone "All isolates" series when not;
# `cumulative_color` colours the running-total overlay (Stacked/Square only).
#
# `square` applies to the stacked mode only, and is what turns it from solid
# bars into the textbook block curve: one countable square per case.
#
# `reveal_from`/`reveal_to` are what playback and the Time tab's date-range
# window ride on: bins outside [reveal_from, reveal_to] are dropped from the
# drawn data. By default (`fixed_axis` TRUE or unset) the scales stay fixed to
# the whole dataset regardless, so the curve grows (or is windowed) inside a
# stable frame instead of the axes crawling under it. With `fixed_axis` FALSE
# the frame itself follows reveal_from/reveal_to instead, zooming the axis to
# the windowed span. Either reveal bound may be NULL, meaning unbounded on
# that side.
#
# `label_ends` (cumulative only) writes each stratum's name directly beside the
# most recently plotted point on its line, instead of relying on the legend.
#
# `plot_width` is the panel width in px (the browser-reported render width). It
# only sizes the legend: the number of columns is picked so a many-stratum
# legend wraps to fit that width rather than overflowing the panel edges — see
# epi_legend_ncol. Unset (NULL) falls back to a typical width.
#' @export
build_epi_ggplot <- function(binned, opts = list()) {
  mode <- opts$mode %||% "stacked"
  # Squares only mean anything for the stacked curve; a step line has no cells.
  square <- isTRUE(opts$square) && identical(mode, "stacked")
  # Likewise, a bar chart has no "line" to label the end of.
  label_ends <- isTRUE(opts$label_ends) && identical(mode, "cumulative")
  # Square blocks/Stacked only: Cumulative already *is* the running total,
  # drawn as the main curve, so overlaying one on top of it would just repeat
  # it.
  show_cumulative <- isTRUE(opts$show_cumulative) && identical(mode, "stacked")
  background <- opts$background %||% "#ffffff"
  text_color <- opts$text_color %||% "#000000"
  # The overlaid running total's own colour, independent of the axis/text so it
  # can be picked out against the bars it sits on. Defaults to text_color, which
  # is what it was drawn in before it became a separate control.
  cumulative_color <- opts$cumulative_color %||% text_color
  interval <- opts$interval %||% "day"
  width_days <- bin_width_days(interval)

  if (identical(mode, "cumulative")) {
    binned <- epi_cumulate(binned)
  }
  cats <- sort(unique(binned$stratum))
  # A single unstratified series (the "All isolates" sentinel) is coloured by a
  # plain colour picker, not the scale: there is one bar colour to set, not a
  # palette to divide between strata. The scale only means anything once there
  # is more than one series to tell apart — see the Colors panel in the view,
  # which swaps the scale select for a colour picker on the same condition.
  # Otherwise resolve the scale against the strata actually present rather than
  # trusting the picker: a stratification can outgrow its palette a round-trip
  # before the picker hears about it.
  pal <- if (identical(cats, EPI_ALL_LABEL) && !is.null(opts$single_color)) {
    setNames(opts$single_color, cats)
  } else {
    epi_palette(cats, epi_fit_scale(opts$col_scale, length(cats)))
  }
  # A lone "All isolates" series has nothing to distinguish, so its legend
  # would just be noise — and when the lines are labelled directly, the legend
  # would just be saying the same thing twice.
  show_legend <- opts$show_legend %||%
    (!identical(cats, EPI_ALL_LABEL) && !label_ends)

  # Limits come from the *whole* dataset, before any reveal, so that playback
  # and the finished curve share one frame — unless `fixed_axis` is off, in
  # which case the frame itself tracks the reveal window (the Time tab's
  # "Adapt to selection" switch), zooming the axis to whatever span is
  # currently windowed instead of holding it to the dataset's full extent.
  x_range <- if (nrow(binned)) range(binned$date_bin) else NULL
  # Unset means fixed: isTRUE(NULL) is FALSE, so a caller that never mentions
  # fixed_axis (every existing build_epi_ggplot() call before the Time tab's
  # zoom switch was added) would otherwise silently get the zoom-to-selection
  # behaviour instead of the documented default.
  fixed_axis <- if (is.null(opts$fixed_axis)) TRUE else isTRUE(opts$fixed_axis)
  axis_range <- if (fixed_axis || is.null(x_range)) {
    x_range
  } else {
    c(opts$reveal_from %||% x_range[1], opts$reveal_to %||% x_range[2])
  }
  y_max <- .y_max(binned, mode)

  shown <- binned
  if (!is.null(opts$reveal_from) && nrow(shown)) {
    shown <- shown[shown$date_bin >= opts$reveal_from, , drop = FALSE]
  }
  if (!is.null(opts$reveal_to) && nrow(shown)) {
    shown <- shown[shown$date_bin <= opts$reveal_to, , drop = FALSE]
  }

  # The overlay's running total, summed across every stratum regardless of how
  # the bars are coloured — a stratum-by-stratum cumulative curve on top of a
  # stacked bar chart would need as many lines as there are strata, cluttering
  # the very thing the overlay is meant to clarify. Scaled against the *whole*
  # dataset's grand total (not the windowed one) for the same reason the y axis
  # itself is fixed before any reveal: a playback frame's secondary axis
  # shouldn't rescale out from under the line as the window moves.
  cum_totals <- NULL
  cum_scale <- 1
  if (show_cumulative && nrow(binned)) {
    cum_totals <- binned |>
      group_by(date_bin) |>
      summarise(count = sum(count), .groups = "drop") |>
      arrange(date_bin) |>
      as.data.frame()
    cum_totals$cum <- cumsum(cum_totals$count)
    cum_scale <- max(cum_totals$cum, 1) / max(1, y_max)
  }
  cum_shown <- cum_totals
  if (!is.null(cum_shown) && nrow(cum_shown)) {
    if (!is.null(opts$reveal_from)) {
      cum_shown <- cum_shown[
        cum_shown$date_bin >= opts$reveal_from,
        ,
        drop = FALSE
      ]
    }
    if (!is.null(opts$reveal_to)) {
      cum_shown <- cum_shown[
        cum_shown$date_bin <= opts$reveal_to,
        ,
        drop = FALSE
      ]
    }
  }

  # Worked out ahead of the y scale (rather than left to .cumulative_end_labels
  # alone) so the panel can reserve enough room below its lowest label —
  # otherwise a stratum whose curve ends near 0 gets its label clipped by the
  # panel's bottom edge, which sits exactly on the 0 line with no expansion.
  end_positions <- if (label_ends) {
    .cumulative_end_positions(shown, y_max)
  } else {
    NULL
  }
  bottom_room <- if (!is.null(end_positions)) {
    max(0, .label_half_height(y_max) - min(end_positions$y_label))
  } else {
    0
  }

  # A second axis rather than a shared one: the running total climbs to the
  # dataset's grand total while each bar only ever reaches one bin's count, so
  # sharing a scale would flatten the bars to a sliver at the bottom.
  y_scale_args <- list(
    breaks = count_breaks(y_max),
    limits = c(0, max(1, y_max)),
    expand = expansion(mult = c(0, 0.08), add = c(bottom_room, 0))
  )
  if (!is.null(cum_totals)) {
    y_scale_args$sec.axis <- sec_axis(
      ~ . * cum_scale,
      name = "Cumulative cases"
    )
  }

  # The drawn x-axis extent, computed once so the annotation layer can trim a
  # period's label to the fraction of the panel its bracket actually spans.
  xlim <- .x_limits(
    axis_range,
    opts$annos,
    interval,
    # Line-end labels need room to the right of the last point; reserved
    # as a fraction of the span since text width can't be known in data
    # units up front.
    extra_right_frac = if (label_ends) 0.22 else 0
  )

  p <- ggplot() +
    .epi_theme(background, text_color) +
    scale_fill_manual(values = pal, drop = FALSE, limits = cats) +
    scale_colour_manual(values = pal, drop = FALSE, limits = cats) +
    do.call(scale_y_continuous, y_scale_args) +
    scale_x_date(
      date_labels = .date_labels(interval),
      limits = xlim
    ) +
    labs(
      x = "Date of collection",
      y = if (identical(mode, "cumulative")) {
        "Cumulative cases"
      } else {
        "Number of cases"
      }
    ) +
    # Both fill and colour carry the strata, but which one is actually mapped
    # depends on the mode: the bar modes colour by `fill` (geom_col/geom_tile),
    # the cumulative curve by `colour` (geom_step). Only the mapped aesthetic
    # produces a legend, so the guide setting has to cover both — silencing only
    # `fill` left the cumulative curve's colour legend to render regardless of
    # show_legend, which is what put a full stratum legend (and the whitespace it
    # letterboxes the curve into) under a "Label lines" cumulative plot.
    guides(
      fill = .strata_guide(show_legend, cats, opts$plot_width),
      colour = .strata_guide(show_legend, cats, opts$plot_width)
    )

  p <- p + .mode_layers(shown, mode, square, interval)

  if (label_ends) {
    p <- p + .cumulative_end_labels(end_positions)
  }

  layers <- .annotation_layers(
    opts$annos,
    x_range,
    y_max,
    background = background,
    xlim = xlim,
    plot_width = opts$plot_width
  )
  for (l in layers) {
    p <- p + l
  }

  if (!is.null(cum_shown) && nrow(cum_shown)) {
    # Added last so it sits in front of every other layer, including
    # annotations — a running total is only useful if it's never the thing
    # buried under the bars/tiles (or a period's shading) it's overlaid on.
    mapping <- aes(x = .data$date_bin, y = .data$cum / cum_scale)
    p <- p +
      geom_step(
        data = cum_shown,
        mapping = mapping,
        colour = background,
        linewidth = 2
      ) +
      geom_step(
        data = cum_shown,
        mapping = mapping,
        colour = cumulative_color,
        linewidth = 0.8
      )
  }

  if (square) {
    # One case tall must measure the same as one interval wide. coord_fixed's
    # ratio is in data units, and a Date axis counts days — so the ratio *is*
    # the bin width. (plotly's equivalent, scaleanchor, silently does nothing
    # against a date axis; ggplot's coord system has no such gap.)
    #
    # This hands the panel's aspect to the data, so the plot can no longer be
    # any shape the caller likes — see square_panel_ratio(), which the view uses
    # to give the output the height the squares actually need. Without that the
    # panel gets letterboxed into a strip and the squares shrink to nothing.
    p <- p + coord_fixed(ratio = width_days)
  }
  p
}

# How tall the square-mode panel needs to be, as a fraction of its width.
#
# Square cells mean panel_height / y_max == panel_width / n_bins, so the panel's
# shape is fixed by the data: a decade of months against a handful of cases is a
# long thin strip, and asking for anything else just letterboxes it. The view
# sizes the plot output from this instead of from the aspect slider.
#' @export
square_panel_ratio <- function(binned, mode = "stacked") {
  if (is.null(binned) || !nrow(binned) || identical(mode, "cumulative")) {
    return(NULL)
  }
  n_bins <- length(unique(binned$date_bin))
  y_max <- .y_max(binned, mode)
  if (!n_bins || !is.finite(y_max) || y_max <= 0) {
    return(NULL)
  }
  y_max / n_bins
}

# The date range the x axis has to cover.
#
# A bar starts at its bin and runs one interval to the right, so the span is
# from the first bin to the last bin *plus its own width* — using the bin dates
# alone would clip the final bar and warn about "values outside the scale
# range". Widened again to take in any annotation outside the data: without that
# an annotation dated beyond the last isolate would be silently dropped — you'd
# add a milestone and simply never see it. `extra_right_frac` reserves further
# room for the cumulative curve's line-end labels, as a fraction of the span
# (text width isn't known in data units up front).
.x_limits <- function(x_range, annos, interval, extra_right_frac = 0) {
  if (is.null(x_range)) {
    return(NULL)
  }
  pad <- bin_width_days(interval) / 4
  last_width <- exact_bin_widths(x_range[2], interval)
  span <- as.numeric(diff(x_range))
  right_extra <- if (extra_right_frac > 0) {
    max(span, 1) * extra_right_frac
  } else {
    0
  }
  lims <- c(x_range[1] - pad, x_range[2] + last_width + pad + right_extra)
  if (!is.null(annos) && nrow(annos)) {
    dates <- c(annos$start, annos$end[!is.na(annos$end)])
    lims <- c(min(lims[1], dates), max(lims[2], dates))
  }
  as.Date(lims, origin = "1970-01-01")
}

# Where each cumulative curve's end-of-line label lands: each stratum's name
# positioned right after the most recently plotted point on its own line.
# Greedy vertical separation (the same idea as the annotation lanes) keeps two
# labels whose lines finish at similar counts from landing on top of each
# other — the label reads the line directly, so it stands in for the legend
# rather than repeating it (see show_legend in build_epi_ggplot). Split out
# from .cumulative_end_labels so build_epi_ggplot can see where the lowest
# label lands *before* fixing the y axis, and reserve enough room below it.
.cumulative_end_positions <- function(shown, y_max) {
  if (!nrow(shown)) {
    return(NULL)
  }
  last_bin <- max(shown$date_bin)
  ends <- shown[shown$date_bin == last_bin, , drop = FALSE]
  ends <- ends[order(-ends$count), , drop = FALSE]

  min_gap <- max(1, y_max * 0.045)
  y_label <- numeric(nrow(ends))
  for (i in seq_len(nrow(ends))) {
    y <- ends$count[i]
    if (i > 1 && (y_label[i - 1] - y) < min_gap) {
      y <- y_label[i - 1] - min_gap
    }
    y_label[i] <- y
  }
  ends$y_label <- y_label

  # A little clear of the point so the text doesn't sit on top of the line's
  # own endpoint marker.
  span <- as.numeric(if (nrow(shown) > 1) diff(range(shown$date_bin)) else 1)
  offset <- max(1, span * 0.015)
  ends$x_label <- ends$date_bin + offset

  ends
}

# Half a line of text, in y-axis data units — used to keep the lowest label's
# text clear of the panel's bottom clipping edge (see build_epi_ggplot's
# bottom_room), since geom_text draws it vertically centred on y_label.
.label_half_height <- function(y_max) max(1, y_max * 0.05)

.cumulative_end_labels <- function(end_positions) {
  if (is.null(end_positions)) {
    return(NULL)
  }
  geom_text(
    data = end_positions,
    aes(
      x = .data$x_label,
      y = .data$y_label,
      label = .data$stratum,
      colour = .data$stratum
    ),
    hjust = 0,
    size = 3,
    show.legend = FALSE
  )
}

# The tallest the y axis has to reach for the *whole* dataset.
.y_max <- function(binned, mode) {
  if (!nrow(binned)) {
    return(1)
  }
  if (identical(mode, "cumulative")) {
    return(max(binned$count))
  }
  totals <- tapply(binned$count, binned$date_bin, sum)
  if (length(totals)) max(totals) else 1
}

.mode_layers <- function(shown, mode, square, interval) {
  if (identical(mode, "cumulative")) {
    # A step, not an interpolation: nothing happens between two bins, so a
    # sloped line would draw case counts that never existed.
    return(geom_step(
      data = shown,
      aes(x = .data$date_bin, y = .data$count, colour = .data$stratum),
      linewidth = 0.7
    ))
  }
  if (square) {
    blocks <- .unit_blocks(shown)
    blocks$width <- exact_bin_widths(blocks$date_bin, interval)
    return(geom_tile(
      data = blocks,
      aes(
        # Each cell spans its own interval, and x marks that interval's start,
        # so the cell has to be nudged half a width right to sit over it.
        x = .data$date_bin + .data$width / 2,
        # y_idx - 0.5 centres each cell between its case index and the one
        # below, so the column starts on the axis rather than half a case under
        # it.
        y = .data$y_idx - 0.5,
        fill = .data$stratum,
        width = .data$width
      ),
      height = 1,
      colour = "#FFFFFF",
      linewidth = 0.3
    ))
  }
  shown$width <- exact_bin_widths(shown$date_bin, interval)
  geom_col(
    data = shown,
    aes(
      x = .data$date_bin + .data$width / 2,
      y = .data$count,
      fill = .data$stratum,
      width = .data$width
    ),
    # "Bars should touch each other (unless there are periods of time with no
    # cases)" — a visible gap then means no cases, not a drawing style.
    colour = "#FFFFFF",
    linewidth = 0.15
  )
}

# --- export ------------------------------------------------------------------

# Save the current curve. Unlike the plotly build this replaces, ggsave needs no
# external toolchain, so every raster and vector format is genuinely available
# rather than HTML only. Mirrors save_tree_plot() in app/logic/tree_plot.R.
#' @export
save_epi_plot <- function(
  plot,
  file,
  filetype = "png",
  aspect_ratio = 0.55,
  dpi = 192
) {
  width <- 12
  ggsave(
    filename = file,
    plot = plot,
    device = filetype,
    width = width,
    height = width * aspect_ratio,
    dpi = dpi,
    limitsize = FALSE
  )
}
