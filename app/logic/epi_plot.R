# app/logic/epi_plot.R
#
# Epidemiological curve computation and visualization engine.
# Provides date binning, data reshaping, stratification, palette mapping,
# timeline annotations, and ggplot2 construction for visualization_epi.R.

box::use(
  RColorBrewer[brewer.pal, brewer.pal.info],
  dplyr[arrange, group_by, n, summarise],
  ggplot2[
    .data,
    aes,
    annotate,
    coord_fixed,
    element_blank,
    element_line,
    element_rect,
    element_text,
    expansion,
    geom_col,
    geom_rect,
    geom_line,
    geom_step,
    geom_text,
    geom_tile,
    ggplot,
    ggsave,
    guide_axis,
    guide_legend,
    guides,
    labs,
    margin,
    rel,
    scale_colour_manual,
    scale_fill_manual,
    scale_linetype_manual,
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
  rlang[`%||%`],
)
box::use(
  app / logic / date_bins[bin_date_values, floor_date_bin, parse_dates],
  app / logic / viz_fit,
  app / logic / viz_helpers[color_scales],
)

#' Default label for unstratified isolate series.
#' @export
EPI_ALL_LABEL <- "All isolates"

#' Default label for isolates missing metadata field values.
#' @export
EPI_UNKNOWN_LABEL <- "(unknown)"

#' Delimiter for multi-field composite stratification categories.
#' @export
EPI_STRATUM_SEP <- " | "

#' Expected metadata date column name across all isolate records.
#' @export
EPI_DATE_FIELD <- "sample_collection_date"

# --- Date Binning Configuration ----------------------------------------------

#' Supported time aggregation intervals ordered by fine-to-coarse granularity.
#' @export
EPI_INTERVALS <- c(Day = "day", Week = "week", Month = "month", Year = "year")

#' Maximum target threshold for rendered bars before recommending coarser bins.
#' @export
EPI_MAX_BARS <- 150L

#' Default interval when no valid collection dates are available.
#' @export
EPI_INTERVAL_FALLBACK <- "week"

#' Nominal Bin Width in Days
#'
#' Returns nominal day equivalents for sizing heuristic comparisons.
#'
#' @param interval String; target interval name.
#' @return Numeric nominal width in days.
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

#' Exact Bin Widths in Days
#'
#' Calculates true physical day durations per binned date to handle variable
#' month and leap-year boundaries without bar overlap.
#'
#' @param dates Vector of floored Date objects.
#' @param interval String; time binning unit.
#' @return Numeric vector of exact day durations per bin entry.
#' @export
exact_bin_widths <- function(dates, interval) {
  unit <- tolower(interval %||% "day")
  if (unit %in% c("day", "week")) {
    return(rep(bin_width_days(unit), length(dates)))
  }
  # Calculate actual step difference for variable length periods (months/years)
  vapply(
    dates,
    function(d) as.numeric(seq(d, by = unit, length.out = 2)[2] - d),
    numeric(1)
  )
}

.n_slots <- function(dates, interval) {
  span <- as.numeric(diff(range(dates)))
  if (!is.finite(span)) {
    return(NA_real_)
  }
  ceiling(span / bin_width_days(interval)) + 1
}

#' Determine Optimal Default Binning Interval
#'
#' Evaluates intervals from finest to coarsest to select the highest-resolution
#' binning option that remains under `max_bars`.
#'
#' @param dates Vector of collection dates.
#' @param max_bars Integer; maximum acceptable bar count threshold.
#' @return String representing selected interval name.
#' @export
epi_default_interval <- function(dates, max_bars = EPI_MAX_BARS) {
  d <- parse_dates(dates)
  d <- d[!is.na(d)]
  if (!length(d)) {
    return(EPI_INTERVAL_FALLBACK)
  }
  intervals <- unname(EPI_INTERVALS)
  # Pick the finest interval that fits within max_bars limit
  for (iv in intervals) {
    if (isTRUE(.n_slots(d, iv) <= max_bars)) {
      return(iv)
    }
  }
  intervals[length(intervals)]
}

# --- Data Reshaping ----------------------------------------------------------

epi_stratum <- function(meta, stratify_by) {
  if (!length(stratify_by)) {
    return(rep(EPI_ALL_LABEL, nrow(meta)))
  }
  # Replace empty or NA values with default unknown label
  parts <- lapply(stratify_by, function(f) {
    v <- as.character(meta[[f]])
    v[is.na(v) | !nzchar(trimws(v))] <- EPI_UNKNOWN_LABEL
    v
  })
  do.call(paste, c(parts, list(sep = EPI_STRATUM_SEP)))
}

#' Order Strata for the Legend and the Stack
#'
#' Alphabetical, except that the "value not recorded" category is always last —
#' "(unknown)" sorts ahead of every letter, which put the one category carrying
#' no information at the head of the legend and gave it the palette's first
#' (loudest) colour. Every place that decides an order — the palette, the fill
#' scale's limits, the stack, the cumulative grid — goes through this, or the
#' legend and the bars disagree about which colour is which.
#'
#' @param x Character vector of stratum labels.
#' @return Sorted unique labels, `EPI_UNKNOWN_LABEL` last when present.
#' @export
epi_strata_levels <- function(x) {
  lv <- sort(unique(as.character(x)))
  c(setdiff(lv, EPI_UNKNOWN_LABEL), intersect(lv, EPI_UNKNOWN_LABEL))
}

.empty_epi_data <- function(dropped = 0L) {
  out <- data.frame(
    date_bin = as.Date(character()),
    stratum = character(),
    count = integer(),
    stringsAsFactors = FALSE
  )
  attr(out, "dropped") <- dropped
  attr(out, "date_range") <- as.Date(c(NA, NA))
  out
}

#' Aggregate Isolate Counts into Binned Intervals
#'
#' Parses dates, constructs composite stratifications, and calculates case
#' counts grouped by `(date_bin, stratum)`. Unparseable dates are excluded
#' and recorded in the `"dropped"` attribute.
#'
#' @param meta Data frame of isolate metadata.
#' @param date_field Column name holding collection dates.
#' @param stratify_by Character vector of metadata columns for grouping.
#' @param interval Time step unit for grouping ("day", "week", "month", "year").
#' @return Data frame with `date_bin`, `stratum`, and `count` columns.
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

  # Filter out rows with unparseable dates
  parsed <- parse_dates(meta[[date_field]])
  keep <- !is.na(parsed)
  dropped <- sum(!keep)
  if (!any(keep)) {
    return(.empty_epi_data(dropped))
  }
  meta <- meta[keep, , drop = FALSE]
  parsed <- parsed[keep]

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
  # The observed extent, NOT the bin extent: a bin is named by its first day,
  # so range(date_bin) understates the last bin by up to an interval. Anything
  # showing the user a date span (the window slider) wants this instead.
  attr(out, "date_range") <- range(parsed)
  out
}

#' Compute Cumulative Counts Across Strata
#'
#' Transforms per-interval counts into running total cumulative series per
#' stratum across all distinct dates.
#'
#' @param binned Binned epi data frame output from `build_epi_data()`.
#' @return Data frame of running totals per `(date_bin, stratum)`.
#' @export
epi_cumulate <- function(binned) {
  if (is.null(binned) || !nrow(binned)) {
    return(.empty_epi_data(attr(binned, "dropped") %||% 0L))
  }
  # Create complete grid to ensure missing interval steps are zero-filled before cumulative sum
  grid <- expand.grid(
    date_bin = sort(unique(binned$date_bin)),
    stratum = epi_strata_levels(binned$stratum),
    stringsAsFactors = FALSE
  )
  out <- merge(grid, binned, by = c("date_bin", "stratum"), all.x = TRUE)
  out$count[is.na(out$count)] <- 0L
  # By the grid's own level order, not alphabetically: the running total has to
  # accumulate within each stratum, and the rows must come out in the same
  # order the legend lists them.
  out <- out[
    order(match(out$stratum, grid$stratum), out$date_bin),
    ,
    drop = FALSE
  ]
  out$count <- as.integer(ave(out$count, out$stratum, FUN = cumsum))
  rownames(out) <- NULL
  attr(out, "dropped") <- attr(binned, "dropped") %||% 0L
  attr(out, "date_range") <- epi_date_range(binned)
  out
}

#' Extract Ordered Unique Date Bins
#'
#' @param binned Binned epi data frame.
#' @return Vector of sorted unique `Date` entries.
#' @export
epi_bins <- function(binned) {
  if (is.null(binned) || !nrow(binned)) {
    return(as.Date(character()))
  }
  sort(unique(binned$date_bin))
}

#' Observed Collection-Date Extent of a Binned Frame
#'
#' The first and last date actually collected, as recorded by
#' `build_epi_data()` before flooring — not `range(date_bin)`, which names the
#' last bin by its first day and so can fall short of the real last date by
#' almost a whole interval.
#'
#' @param binned Binned epi data frame.
#' @return Length-2 `Date` vector, `NA` when the extent is unknown.
#' @export
epi_date_range <- function(binned) {
  rng <- attr(binned, "date_range")
  if (is.null(rng) || length(rng) != 2) {
    # Pre-attribute frames (a restored analysis, a hand-built fixture) still
    # have their bins, which bound the extent from the inside.
    bins <- epi_bins(binned)
    rng <- if (length(bins)) range(bins) else as.Date(c(NA, NA))
  }
  as.Date(rng)
}

#' Last Day Covered by a Bin
#'
#' @param bin Bin start `Date` (or vector of them).
#' @param interval Interval key the bin was floored to.
#' @return `Date` of the bin's final day.
#' @export
bin_end_date <- function(bin, interval) {
  if (!length(bin)) {
    return(as.Date(character()))
  }
  bin + exact_bin_widths(bin, interval) - 1
}

# --- Moving Average Computation ----------------------------------------------

#' Default moving average window size in intervals.
#' @export
EPI_MOVING_AVG_WINDOW_DEFAULT <- 7L

#' Format Interval Name for Display Labels
#'
#' @param interval Target interval key.
#' @param n Interval count for pluralization check.
#' @return Character string formatted interval noun (e.g., "week" vs "weeks").
#' @export
epi_interval_noun <- function(interval, n = 1) {
  nm <- names(EPI_INTERVALS)[match(tolower(interval %||% "day"), EPI_INTERVALS)]
  word <- tolower(if (is.na(nm)) "interval" else nm)
  if (as.integer(n) == 1L) word else paste0(word, "s")
}

#' Format Moving Average Legend Label
#'
#' @param window Integer window size.
#' @param interval Target interval unit.
#' @return Formatted label string for plot legend.
#' @export
epi_moving_avg_label <- function(window, interval) {
  sprintf(
    "%d-%s moving average",
    as.integer(window),
    epi_interval_noun(interval, 1)
  )
}

.rolling_mean <- function(x, window, align = "center") {
  n <- length(x)
  w <- max(1L, as.integer(window))
  if (w <= 1L || n == 0L) {
    return(as.numeric(x))
  }
  half <- (w - 1L) %/% 2L
  vapply(
    seq_len(n),
    function(i) {
      if (identical(align, "trailing")) {
        lo <- max(1L, i - w + 1L)
        hi <- i
      } else {
        # Centered alignment window calculation
        lo <- max(1L, i - half)
        hi <- min(n, i + (w - 1L - half))
      }
      mean(x[lo:hi])
    },
    numeric(1)
  )
}

#' Calculate Moving Average Over Aggregate Interval Counts
#'
#' Fills missing temporal grid gaps with zero and calculates a rolling mean
#' across total case counts across all strata.
#'
#' @param binned Binned epi data frame.
#' @param window Integer window width.
#' @param interval Binning time step unit.
#' @param align Window alignment strategy ("center" or "trailing").
#' @return Data frame with `date_bin` and `avg` columns.
#' @export
epi_moving_average <- function(
  binned,
  window = EPI_MOVING_AVG_WINDOW_DEFAULT,
  interval = "day",
  align = "center"
) {
  empty <- data.frame(
    date_bin = as.Date(character()),
    avg = numeric(),
    stringsAsFactors = FALSE
  )
  if (is.null(binned) || !nrow(binned)) {
    return(empty)
  }
  totals <- binned |>
    group_by(date_bin) |>
    summarise(count = sum(count), .groups = "drop") |>
    arrange(date_bin) |>
    as.data.frame()

  # Create continuous temporal sequence to account for zero-count periods
  grid <- data.frame(
    date_bin = seq(
      min(totals$date_bin),
      max(totals$date_bin),
      by = tolower(interval %||% "day")
    ),
    stringsAsFactors = FALSE
  )
  filled <- merge(grid, totals, by = "date_bin", all.x = TRUE)
  filled$count[is.na(filled$count)] <- 0L
  filled <- filled[order(filled$date_bin), , drop = FALSE]

  data.frame(
    date_bin = filled$date_bin,
    avg = .rolling_mean(filled$count, window, align),
    stringsAsFactors = FALSE
  )
}

# --- Palette Utilities -------------------------------------------------------

.viridis_scales <- color_scales$Gradient

#' Map Categorical Stratum Levels to Color Hex Codes
#'
#' Interpolates palette colors when requested category count exceeds standard
#' qualitative Brewer palette bounds.
#'
#' @param cats Character vector of category names.
#' @param scale String palette identifier.
#' @return Named character vector mapping categories to hex color codes.
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
    max_n <- brewer.pal.info[scale, "maxcolors"]
    base <- brewer.pal(max(3L, min(max_n, n)), scale)
    # Ramp palette if categories exceed max native colors
    if (n <= length(base)) base[seq_len(n)] else colorRampPalette(base)(n)
  } else {
    viridis(n)
  }

  setNames(cols[seq_len(n)], cats)
}

#' Filter Suitable Color Scales by Stratum Cardinality
#'
#' Restricts available qualitative palette options to those that natively
#' support `n_strata` levels without forced interpolation.
#'
#' @param n_strata Integer count of distinct categories.
#' @return Named list of valid scale identifiers grouped by palette class.
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

#' Resolve Scale Palette for High-Cardinality Data
#'
#' Ensures the selected scale can represent `n_strata` categories, falling back
#' to a valid gradient/sequential scale when necessary.
#'
#' @param scale Requested palette name.
#' @param n_strata Total category count.
#' @return Valid palette string identifier.
#' @export
epi_fit_scale <- function(scale, n_strata) {
  valid <- unlist(epi_scale_choices(n_strata), use.names = FALSE)
  if (isTRUE(scale %in% valid)) scale else valid[1]
}

# --- Timeline Annotations ----------------------------------------------------

#' Default hex color for visual period annotations.
#' @export
EPI_ANNO_COLOR_DEFAULT <- "#f39c12"

#' Construct Empty Annotations Data Frame Schema
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

#' Coerce Restored Annotations To The Schema
#'
#' Rebuilds `empty_epi_annotations()`'s shape from whatever a saved snapshot
#' hands back. JSON has no date type, so `start` and `end` return as ISO
#' strings; assigning those straight into the annotations reactiveVal leaves the
#' plot code holding character columns where it expects Dates, and the damage
#' surfaces far from here — `Summary.Date` re-classes the character result of
#' `min()`/`max()` as a Date, so `.x_limits` produces a Date whose storage is
#' text and any arithmetic on it fails. Rows with no readable start are dropped:
#' an annotation that cannot be placed on the axis has nothing to draw.
#'
#' @param x Annotations as restored (a data.frame, or NULL).
#' @return A data.frame in the annotations schema, possibly with zero rows.
#' @export
as_epi_annotations <- function(x) {
  empty <- empty_epi_annotations()
  if (is.null(x) || !is.data.frame(x) || !nrow(x)) {
    return(empty)
  }

  chr <- function(col, default = NA_character_) {
    v <- if (col %in% names(x)) as.character(x[[col]]) else default
    rep_len(v, nrow(x))
  }
  # A live frame arrives with real Dates and is handed straight back. Anything
  # else is parsed as the ISO text jsonlite writes — via `format`, because bare
  # as.Date() on an unreadable string throws where a dropped row is wanted. The
  # storage check catches the corrupt in-between state: text carrying the Date
  # class, which is what a restored frame turns into once it reaches min()/max().
  dt <- function(col) {
    if (!col %in% names(x)) {
      return(rep_len(as.Date(NA), nrow(x)))
    }
    v <- x[[col]]
    if (inherits(v, "Date") && is.numeric(unclass(v))) {
      return(v)
    }
    if (is.numeric(v)) {
      return(as.Date(v, origin = "1970-01-01"))
    }
    suppressWarnings(as.Date(as.character(v), format = "%Y-%m-%d"))
  }

  out <- data.frame(
    id = chr("id"),
    type = chr("type", ANNO_PERIOD),
    start = dt("start"),
    end = dt("end"),
    label = chr("label", ""),
    color = chr("color", EPI_ANNO_COLOR_DEFAULT),
    stringsAsFactors = FALSE
  )
  out$color[is.na(out$color) | !nzchar(out$color)] <- EPI_ANNO_COLOR_DEFAULT
  out$label[is.na(out$label)] <- ""
  # Ids only have to be unique within the frame — they key the delete buttons.
  missing_id <- is.na(out$id) | !nzchar(out$id)
  out$id[missing_id] <- paste0("a_restored_", which(missing_id))

  out[!is.na(out$start), , drop = FALSE]
}

.blend <- function(fg, bg, alpha) {
  f <- col2rgb(fg)
  b <- col2rgb(bg)
  mix <- f * alpha + b * (1 - alpha)
  rgb(mix[1], mix[2], mix[3], maxColorValue = 255)
}

# Select readable text color (black/white) based on background luminance
.contrast_text <- function(bg) {
  c <- col2rgb(bg)
  luma <- (0.299 * c[1] + 0.587 * c[2] + 0.114 * c[3]) / 255
  if (luma > 0.55) "#000000" else "#ffffff"
}

.anno_color <- function(annos, i) {
  col <- if ("color" %in% names(annos)) annos$color[i] else NA_character_
  if (is.na(col) || !nzchar(col)) EPI_ANNO_COLOR_DEFAULT else col
}

# ggplot2 sizes geom text in millimetres of its own points (1/72.27 in), so a
# size fitted in points is divided by this before it reaches a geom.
.GG_PT <- 72.27 / 25.4

# Lanes of annotation labels stacked over the curve before the rest are left
# unlabelled. Past this the band is taller than the reader will follow.
.max_lanes <- 6L

# Clearance a label keeps from the next one in its lane when nothing has
# measured it, as a share of the plotted span.
.label_pad_frac <- 0.07

ANNO_PERIOD <- "period"

.is_period <- function(annos, i) {
  identical(annos$type[i], ANNO_PERIOD) && !is.na(annos$end[i])
}

# Greedily pack labels into vertical lanes to minimize visual collisions
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

.anno_span <- function(annos, x_range) {
  xs <- c(
    as.numeric(annos$start),
    as.numeric(annos$end[!is.na(annos$end)]),
    as.numeric(x_range)
  )
  span <- diff(range(xs, na.rm = TRUE))
  if (!is.finite(span) || span <= 0) 1 else span
}

#' Assign Non-Overlapping Layout Lanes for Annotations
#'
#' Determines vertical offset packing for timeline milestone markers and
#' period shading overlays to avoid visual label collisions.
#'
#' Two annotations share a lane only where their *footprints* do not overlap.
#' `left`/`right` are those footprints measured from the labels at the size
#' they will be drawn (see `epi_layout()`); without them each annotation is
#' booked a fixed share of the plotted span past its own dates.
#'
#' @param annos Data frame of annotations.
#' @param x_range Plot x-axis domain range bounds.
#' @param left,right Optional numeric footprints (days since the epoch), in
#'   the row order of `annos`.
#' @return Input data frame, sorted by start, augmented with `lane`, `period`,
#'   `left` and `right`.
#' @export
epi_annotation_lanes <- function(
  annos,
  x_range = NULL,
  left = NULL,
  right = NULL
) {
  if (is.null(annos) || !nrow(annos)) {
    out <- empty_epi_annotations()
    out$lane <- integer()
    out$period <- logical()
    out$left <- numeric()
    out$right <- numeric()
    return(out)
  }
  annos$period <- vapply(
    seq_len(nrow(annos)),
    function(i) .is_period(annos, i),
    logical(1)
  )
  if (is.null(left) || is.null(right)) {
    pad <- .anno_span(annos, x_range) * .label_pad_frac
    left <- as.numeric(annos$start)
    right <- vapply(
      seq_len(nrow(annos)),
      function(i) {
        end <- if (annos$period[i]) as.numeric(annos$end[i]) else left[i]
        max(end, left[i] + pad)
      },
      numeric(1)
    )
  }
  annos$left <- as.numeric(left)
  annos$right <- as.numeric(right)
  annos <- annos[order(annos$left, annos$start), , drop = FALSE]
  annos$lane <- .assign_lanes(annos$left, annos$right)
  rownames(annos) <- NULL
  annos
}

# --- Automatic layout --------------------------------------------------------
#
# The curve is a physical figure: `EPI_CANVAS_IN` wide, as tall as the aspect
# ratio says, and every piece of type on it fitted to the room it has under
# the two rules in app/logic/viz_fit.R. Nothing here reads the browser.
#
# The aspect ratio is the height of the *plot* - axes, titles, curve and the
# annotation band - over its width. The legend is set below that and added to
# the canvas rather than taken out of it, so mapping a variable adds a legend
# without squashing the curve it explains (the Tree grows its canvas sideways
# for its guide box by the same rule).

#' Width the curve is drawn at, in inches: a journal's double column.
#' @export
EPI_CANVAS_IN <- 7

#' Lowest aspect ratio the curve may be drawn at.
#' @export
EPI_ASPECT_MIN <- 0.3

# The curve's own panel, height over width, where nothing else asks for more:
# wide enough to read a trend across, tall enough to read a count off.
EPI_PANEL_ASPECT <- 0.5

# Type the design asks for at 100% text size, in points. The axis numbers and
# the legend are the figure's furniture and match; titles are a step larger;
# the labels set *inside* the panel are a half step smaller, since they sit
# among the marks rather than beside them.
EPI_AXIS_PT <- 9
EPI_TITLE_PT <- 10
EPI_LEGEND_PT <- 9
EPI_ANNO_PT <- 8.5
EPI_END_PT <- 8.5

# Geometry, in points. Deliberately not moved by the text size, or "does this
# still fit" has no answer.
EPI_MARGIN_PT <- c(top = 6, right = 12, bottom = 6, left = 6)
EPI_TICK_PT <- 4
EPI_TEXT_GAP_PT <- 2.5
EPI_TITLE_GAP_PT <- 4
EPI_LEGEND_GAP_PT <- 6
EPI_KEY_TEXT_GAP_PT <- 3
EPI_BADGE_PAD_PT <- 1.6

# A key square follows its label's size; the air between two keys does not
# scale with the key but with the type beside it.
EPI_KEY_FRAC <- 1.15
EPI_KEY_SPACING_X_FRAC <- 1
EPI_KEY_SPACING_Y_FRAC <- 0.35

# Shares of the plot height the legend and the annotation band may take before
# their type is shrunk. Past the floor the legend grows the canvas and the band
# drops its outermost lanes' labels.
EPI_LEGEND_FRAC <- 0.35
EPI_ANNO_BAND_FRAC <- 0.4

# Most of the panel's width the end-of-line labels may reserve at its right.
EPI_END_RESERVE_MAX <- 0.3

# The last legend key when the categories outnumber the keys the canvas holds.
EPI_LEGEND_MORE <- "+ %d more"

# Tallest panel, as a share of its width, the fit will draw to give the
# end-of-line labels a pitch each.
EPI_END_PANEL_MAX <- 1.2

# Clearance between a label and what it names (a line's end, a milestone's
# rule, a period's bracket), in inches.
EPI_LABEL_GAP_IN <- 0.05

# Pitch between two neighbouring end-of-line labels, in lines of their type.
EPI_END_PITCH_LINES <- 1.05

# ggplot2's default expansion of a date axis, each side.
.X_EXPAND <- 0.05

# The top headroom above the tallest bar, as a share of the count range.
.Y_HEADROOM <- 0.08

.positive <- function(x, default) {
  x <- suppressWarnings(as.numeric(x %||% default))
  if (length(x) != 1L || !is.finite(x) || x <= 0) default else x
}

# The widest a date on this axis sets, measured over a year of month names at
# a two-digit day.
.date_label_in <- function(interval, pt) {
  sample <- seq(as.Date("2021-01-28"), by = "month", length.out = 12L)
  viz_fit$text_width_in(format(sample, .date_labels(interval)), pt)
}

# Date breaks, no more than `n` of them inside the limits.
.date_breaks <- function(xlim, n) {
  n <- max(1L, as.integer(n))
  repeat {
    b <- pretty(xlim, n = n)
    b <- b[b >= xlim[1] & b <= xlim[2]]
    if (length(b) <= max(n, 2L) || n <= 1L) {
      return(as.Date(b))
    }
    n <- n - 1L
  }
}

# The legend's geometry at one type size: columns, rows and inches of height.
.legend_geometry <- function(cats, pt, width_in, extra_rows = 0L) {
  n <- length(cats)
  if (!n && !extra_rows) {
    return(list(ncol = 1L, rows = 0L, height_in = 0))
  }
  key_in <- pt * EPI_KEY_FRAC / 72
  sx <- pt * EPI_KEY_SPACING_X_FRAC / 72
  sy <- pt * EPI_KEY_SPACING_Y_FRAC / 72
  entry_in <- key_in + EPI_KEY_TEXT_GAP_PT / 72 + viz_fit$text_width_in(cats, pt) + sx
  avail_in <- width_in - sum(EPI_MARGIN_PT[c("left", "right")]) / 72
  ncol <- if (n) {
    max(1L, min(n, as.integer(floor((avail_in + sx) / entry_in))))
  } else {
    1L
  }
  rows <- (if (n) ceiling(n / ncol) else 0L) + extra_rows
  row_in <- max(key_in, viz_fit$line_height_in(pt)) + sy
  list(
    ncol = as.integer(ncol),
    rows = as.integer(rows),
    height_in = rows * row_in - sy + EPI_LEGEND_GAP_PT / 72
  )
}

#' Legend Columns for the Width the Curve Is Drawn At
#'
#' As many columns as the widest entry allows across the canvas, so long
#' category names wrap into more rows rather than running off the edges.
#'
#' @param cats Character vector of category names.
#' @param width_in Numeric. Canvas width in inches.
#' @param pt Numeric. Legend type size in points.
#' @return Integer column count.
#' @export
epi_legend_ncol <- function(cats, width_in = EPI_CANVAS_IN, pt = EPI_LEGEND_PT) {
  if (length(cats) <= 1L) {
    return(1L)
  }
  .legend_geometry(cats, pt, .positive(width_in, EPI_CANVAS_IN))$ncol
}

# The room the axes and their titles take around the panel, in inches.
.epi_chrome <- function(axis_pt, title_pt, y_limit, sec_max, x_title, sec) {
  m <- EPI_MARGIN_PT / 72
  title_in <- viz_fit$line_height_in(title_pt) + EPI_TITLE_GAP_PT / 72
  number_in <- function(v) {
    viz_fit$text_width_in(format(max(1, ceiling(v)), big.mark = ""), axis_pt) +
      EPI_TEXT_GAP_PT / 72
  }
  list(
    top = m[["top"]],
    bottom = m[["bottom"]] +
      EPI_TICK_PT / 72 +
      EPI_TEXT_GAP_PT / 72 +
      viz_fit$line_height_in(axis_pt) +
      (if (x_title) title_in else 0),
    left = m[["left"]] + title_in + number_in(y_limit),
    right = m[["right"]] + (if (sec) title_in + number_in(sec_max) else 0)
  )
}

# Each annotation label's footprint along the axis at one type size, and
# where its text goes: inside a period's bracket where it fits, otherwise
# beside it, and beside a milestone's rule on whichever side has room.
.annotation_footprints <- function(annos, pt, xlim, day_in) {
  n <- nrow(annos)
  if (!n) {
    return(data.frame(
      left = numeric(),
      right = numeric(),
      inside = logical(),
      x_text = numeric(),
      hjust = numeric()
    ))
  }
  pad_days <- EPI_BADGE_PAD_PT / 72 / day_in
  gap_days <- EPI_LABEL_GAP_IN / day_in
  label_days <- vapply(
    as.character(annos$label),
    function(l) viz_fit$text_width_in(l, pt) / day_in + 2 * pad_days,
    numeric(1),
    USE.NAMES = FALSE
  )
  x_hi <- as.numeric(xlim[2])
  x_lo <- as.numeric(xlim[1])
  out <- data.frame(
    left = numeric(n),
    right = numeric(n),
    inside = logical(n),
    x_text = numeric(n),
    hjust = numeric(n)
  )
  for (i in seq_len(n)) {
    start <- as.numeric(annos$start[i])
    period <- .is_period(annos, i)
    end <- if (period) as.numeric(annos$end[i]) else start
    if (period && label_days[i] + 2 * pad_days <= end - start) {
      out[i, ] <- list(start, end, TRUE, (start + end) / 2, 0.5)
      next
    }
    after <- end + gap_days
    if (after + label_days[i] <= x_hi || start - gap_days - label_days[i] < x_lo) {
      out[i, ] <- list(start, after + label_days[i], FALSE, after, 0)
    } else {
      before <- start - gap_days
      out[i, ] <- list(before - label_days[i], end, FALSE, before, 1)
    }
  }
  out
}

#' Fit the Epi Curve's Layout to Its Data
#'
#' Solves, once, everything about the figure that depends on its shape: the
#' canvas, the aspect ratio the data calls for, every type size, which labels
#' can be drawn legibly, the date breaks, the annotation lanes and the legend.
#' The view reads the canvas from this and hands the same answer to the
#' builder, so the image and the drawing on it cannot disagree.
#'
#' Each label is fitted under the two rules (see app/logic/viz_fit.R): set at
#' the design size times `text_scale`, cut back to the room it has, shrunk no
#' further than the print floor, and left off where even the floor does not
#' fit. What stands in for a label that is left off: the legend for the
#' end-of-line labels, the coloured wash and bracket for an annotation.
#'
#' @param binned Binned epi data frame, as passed to `build_epi_ggplot()`.
#' @param opts Plot options, as for `build_epi_ggplot()`, plus `width_in`,
#'   `aspect` (plot height over width; NULL fits one) and `text_scale`.
#' @return A list of geometry, sizes and drawn flags; see the fields below.
#' @export
epi_layout <- function(binned, opts = list()) {
  mode <- opts$mode %||% "stacked"
  square <- isTRUE(opts$square) && identical(mode, "stacked")
  interval <- opts$interval %||% "day"
  k <- viz_fit$text_scale(opts$text_scale)
  w <- .positive(opts$width_in, EPI_CANVAS_IN)
  if (is.null(binned)) {
    binned <- .empty_epi_data()
  }

  totals <- if (nrow(binned)) sum(binned$count) else 0
  if (identical(mode, "cumulative")) {
    binned <- epi_cumulate(binned)
  }
  cats <- epi_strata_levels(binned$stratum)
  y_max <- .y_max(binned, mode)
  y_limit <- max(1, y_max)
  x_range <- if (nrow(binned)) range(binned$date_bin) else NULL
  fixed_axis <- if (is.null(opts$fixed_axis)) TRUE else isTRUE(opts$fixed_axis)
  axis_range <- if (fixed_axis || is.null(x_range)) {
    x_range
  } else {
    c(opts$reveal_from %||% x_range[1], opts$reveal_to %||% x_range[2])
  }
  sec <- isTRUE(opts$show_cumulative) && identical(mode, "stacked") &&
    nrow(binned) > 0
  wants_end <- isTRUE(opts$label_ends) && identical(mode, "cumulative")
  moving_avg <- isTRUE(opts$show_moving_avg) &&
    !identical(mode, "cumulative") &&
    nrow(binned) > 0
  x_title_on <- !isFALSE(opts$show_x_label)
  annos <- opts$annos
  if (is.null(annos)) {
    annos <- empty_epi_annotations()
  }

  axis_want <- EPI_AXIS_PT * k
  title_want <- EPI_TITLE_PT * k
  chrome <- .epi_chrome(axis_want, title_want, y_limit, totals, x_title_on, sec)
  panel_w <- max(w - chrome$left - chrome$right, 0.5)

  # The date axis, and the inches one day takes across the panel. The
  # end-of-line reserve widens the axis, so it is solved first.
  base_xlim <- .x_limits(axis_range, annos, interval)
  base_days <- if (is.null(base_xlim)) 1 else max(as.numeric(diff(base_xlim)), 1)

  end_width_in <- function(pt) viz_fit$text_width_in(cats, pt) + EPI_LABEL_GAP_IN
  end_stack_in <- function(pt) {
    length(cats) * viz_fit$line_height_in(pt) * EPI_END_PITCH_LINES
  }

  xlim_for <- function(end_in) {
    if (is.null(base_xlim)) {
      return(NULL)
    }
    share <- min(end_in / panel_w, EPI_END_RESERVE_MAX)
    extra <- base_days * (1 + 2 * .X_EXPAND) * share / (1 - share)
    as.Date(c(base_xlim[1], base_xlim[2] + extra), origin = "1970-01-01")
  }
  day_in_for <- function(xlim) {
    if (is.null(xlim)) {
      return(panel_w)
    }
    panel_w / (max(as.numeric(diff(xlim)), 1) * (1 + 2 * .X_EXPAND))
  }

  # Annotation lanes at one type size: how many lanes, and how tall a band.
  lane_in <- function(pt) {
    viz_fit$line_height_in(pt) + 2 * EPI_BADGE_PAD_PT / 72 + 2 / 72
  }
  anno_at <- function(pt, xlim) {
    if (!nrow(annos) || is.null(xlim)) {
      return(list(lanes = NULL, n = 0L, band_in = 0))
    }
    fp <- .annotation_footprints(annos, pt, xlim, day_in_for(xlim))
    lanes <- epi_annotation_lanes(annos, x_range, fp$left, fp$right)
    ord <- order(fp$left, annos$start)
    lanes$inside <- fp$inside[ord]
    lanes$x_text <- fp$x_text[ord]
    lanes$hjust <- fp$hjust[ord]
    n <- max(lanes$lane)
    list(lanes = lanes, n = n, band_in = n * lane_in(pt))
  }

  # --- The aspect this data calls for --------------------------------------
  xlim_want <- xlim_for(if (wants_end) end_width_in(EPI_END_PT * k) else 0)
  band_want <- anno_at(EPI_ANNO_PT * k, xlim_want)
  # End labels buy height only up to EPI_END_PANEL_MAX of the panel's width,
  # and only where their stack would fit there at a legible size: a curve with
  # more lines than that is read off its legend, and a page made tall for
  # labels that are never drawn is only white space.
  end_room_max <- EPI_END_PANEL_MAX * panel_w
  end_panel_in <- function(pt) end_stack_in(pt) / (1 - .Y_HEADROOM)
  end_buy_in <- if (
    wants_end && end_panel_in(viz_fit$MIN_PRINT_PT) <= end_room_max
  ) {
    min(end_panel_in(EPI_END_PT * k), end_room_max)
  } else {
    0
  }
  panel_h_fit <- max(
    EPI_PANEL_ASPECT * panel_w,
    min(band_want$n, .max_lanes) * lane_in(EPI_ANNO_PT * k) / EPI_ANNO_BAND_FRAC,
    end_buy_in
  )
  fitted_aspect <- viz_fit$clamp(
    ceiling((panel_h_fit + chrome$top + chrome$bottom) / w * 20) / 20,
    EPI_ASPECT_MIN,
    viz_fit$ASPECT_MAX
  )

  aspect <- suppressWarnings(as.numeric(opts$aspect %||% NA))
  if (length(aspect) != 1L || !is.finite(aspect) || aspect <= 0) {
    aspect <- fitted_aspect
  }
  plot_h <- aspect * w
  panel_h <- max(plot_h - chrome$top - chrome$bottom, 0.5)

  # --- End-of-line labels --------------------------------------------------
  end_fits <- function(pt) {
    end_width_in(pt) <= EPI_END_RESERVE_MAX * panel_w &&
      end_stack_in(pt) <= panel_h * (1 - .Y_HEADROOM)
  }
  end_pt <- viz_fit$largest_fitting(EPI_END_PT * k, end_fits)
  end_drawn <- wants_end && length(cats) > 0 && end_fits(end_pt)
  xlim <- xlim_for(if (end_drawn) end_width_in(end_pt) else 0)
  day_in <- day_in_for(xlim)

  # --- Annotation labels ---------------------------------------------------
  band_room <- EPI_ANNO_BAND_FRAC * panel_h
  anno_fits <- function(pt) {
    a <- anno_at(pt, xlim)
    a$n <= .max_lanes && a$band_in <= band_room
  }
  anno_pt <- viz_fit$largest_fitting(EPI_ANNO_PT * k, anno_fits)
  anno <- anno_at(anno_pt, xlim)
  lanes <- anno$lanes
  kept_lanes <- 0L
  if (!is.null(lanes)) {
    kept_lanes <- min(
      anno$n,
      .max_lanes,
      as.integer(floor(band_room / lane_in(anno_pt)))
    )
    lanes$drawn <- lanes$lane <= kept_lanes
  }
  band_in <- kept_lanes * lane_in(anno_pt)

  if (square && nrow(binned)) {
    # coord_fixed ties one case to one interval, so the panel's height is
    # the data's; the annotation band is inches on top of it.
    y_top <- y_limit * (1 + .Y_HEADROOM)
    in_per_case <- day_in * bin_width_days(interval)
    panel_h <- y_top * in_per_case + band_in
    plot_h <- min(panel_h + chrome$top + chrome$bottom, viz_fit$ASPECT_MAX * w)
    aspect <- plot_h / w
  }

  # --- Axes ----------------------------------------------------------------
  x_label_fits <- function(pt) {
    panel_w / (.date_label_in(interval, pt) * 1.4) >= 2
  }
  axis_pt <- viz_fit$largest_fitting(axis_want, x_label_fits)
  n_x <- max(1L, as.integer(floor(panel_w / (.date_label_in(interval, axis_pt) * 1.4))))
  x_breaks <- if (is.null(xlim)) NULL else .date_breaks(xlim, n_x)
  data_h <- max(panel_h - band_in, 0.1)
  n_y <- max(2L, min(6L, as.integer(floor(data_h / (viz_fit$line_height_in(axis_pt) * 1.8)))))

  y_title <- if (identical(mode, "cumulative")) "Cumulative cases" else "Number of cases"
  title_room <- function(label, length_in) {
    72 * length_in / max(viz_fit$string_em(label), 1)
  }
  x_title_pt <- viz_fit$fit_type(title_want, title_room("Date of collection", panel_w))
  y_title_pt <- viz_fit$fit_type(title_want, title_room(y_title, panel_h))
  sec_title_pt <- viz_fit$fit_type(title_want, title_room("Cumulative cases", panel_h))
  x_title_drawn <- x_title_on && viz_fit$type_drawn(x_title_pt)
  y_title_drawn <- viz_fit$type_drawn(y_title_pt)
  sec_title_drawn <- sec && viz_fit$type_drawn(sec_title_pt)

  # --- Legend --------------------------------------------------------------
  show_legend <- opts$show_legend %||%
    (!identical(cats, EPI_ALL_LABEL) && !end_drawn)
  legend_cats <- if (isTRUE(show_legend)) cats else character(0)
  legend_rows_extra <- if (moving_avg) 1L else 0L
  legend_fits <- function(pt) {
    .legend_geometry(legend_cats, pt, w, legend_rows_extra)$height_in <=
      EPI_LEGEND_FRAC * plot_h
  }
  legend_pt <- if (length(legend_cats) || moving_avg) {
    viz_fit$largest_fitting(EPI_LEGEND_PT * k, legend_fits)
  } else {
    EPI_LEGEND_PT * k
  }
  legend <- .legend_geometry(legend_cats, legend_pt, w, legend_rows_extra)

  # Past the print floor the legend grows the canvas, but only so far: a
  # variable with a hundred categories would otherwise hang a page of keys
  # under the curve. What does not fit is listed as a count in a final key.
  legend_keys <- legend_cats
  legend_cap_in <- (viz_fit$CANVAS_MAX_FACTOR - 1) * plot_h
  if (legend$height_in > legend_cap_in && length(legend_cats) > 1L) {
    row_in <- legend$height_in / max(legend$rows, 1L)
    rows_room <- max(1L, as.integer(floor(legend_cap_in / row_in)) - legend_rows_extra)
    keep <- max(1L, min(length(legend_cats) - 1L, rows_room * legend$ncol - 1L))
    legend_keys <- c(
      legend_cats[seq_len(keep)],
      sprintf(EPI_LEGEND_MORE, length(legend_cats) - keep)
    )
    legend <- .legend_geometry(legend_keys, legend_pt, w, legend_rows_extra)
  }

  height_in <- plot_h + legend$height_in

  list(
    width_in = w,
    height_in = height_in,
    plot_height_in = plot_h,
    legend_height_in = legend$height_in,
    aspect = round(aspect, 3),
    fitted_aspect = fitted_aspect,
    panel_width_in = panel_w,
    panel_height_in = panel_h,
    square = square,
    text_scale = k,
    axis_pt = axis_pt,
    x_breaks = x_breaks,
    n_y_breaks = n_y,
    xlim = xlim,
    x_title_pt = if (x_title_drawn) x_title_pt else NULL,
    y_title_pt = if (y_title_drawn) y_title_pt else NULL,
    sec_title_pt = if (sec_title_drawn) sec_title_pt else NULL,
    title_pt = title_want,
    end_pt = end_pt,
    end_drawn = end_drawn,
    end_hidden = wants_end && !end_drawn,
    end_pitch_in = viz_fit$line_height_in(end_pt) * EPI_END_PITCH_LINES,
    day_in = day_in,
    anno = lanes,
    anno_pt = anno_pt,
    n_lanes = kept_lanes,
    band_in = band_in,
    anno_hidden = !is.null(lanes) && any(!lanes$drawn),
    show_legend = isTRUE(show_legend),
    legend_pt = legend_pt,
    legend_ncol = legend$ncol,
    legend_keys = legend_keys,
    min_pt = viz_fit$min_type_pt(
      axis_pt,
      if (x_title_drawn) x_title_pt,
      if (y_title_drawn) y_title_pt,
      if (sec_title_drawn) sec_title_pt,
      if (length(legend_cats) || moving_avg) legend_pt,
      if (end_drawn) end_pt,
      if (!is.null(lanes) && any(lanes$drawn)) anno_pt
    )
  )
}

# The annotation band's height in count units, for inches of band on a panel
# whose count range below it is `y_span`.
.band_counts <- function(lay, y_span) {
  band_in <- lay$band_in %||% 0
  if (band_in <= 0) {
    return(0)
  }
  if (isTRUE(lay$square)) {
    in_per_case <- lay$day_in * lay$case_days
    return(band_in / in_per_case)
  }
  data_in <- max(lay$panel_height_in - band_in, 0.1)
  band_in * y_span / data_in
}

.annotation_layers <- function(lay, y_top, band, background = "#ffffff") {
  lanes <- lay$anno
  if (is.null(lanes) || !nrow(lanes)) {
    return(list())
  }
  size <- lay$anno_pt / .GG_PT
  n_lanes <- max(lay$n_lanes, 1L)
  lane_h <- if (band > 0) band / n_lanes else 0
  layers <- list()

  for (i in seq_len(nrow(lanes))) {
    col <- .anno_color(lanes, i)
    drawn <- isTRUE(lanes$drawn[i]) && lane_h > 0
    y_mid <- y_top + band - (lanes$lane[i] - 0.5) * lane_h
    half <- lane_h * 0.4

    badge <- function(x, hjust) {
      annotate(
        "label",
        x = as.Date(x, origin = "1970-01-01"),
        y = y_mid,
        label = lanes$label[i],
        colour = col,
        fill = background,
        size = size,
        hjust = hjust,
        vjust = 0.5,
        label.size = 0,
        label.padding = unit(EPI_BADGE_PAD_PT, "pt"),
        label.r = unit(1.5, "pt")
      )
    }

    if (lanes$period[i]) {
      layers <- c(
        layers,
        list(annotate(
          "rect",
          xmin = lanes$start[i],
          xmax = lanes$end[i],
          ymin = -Inf,
          ymax = Inf,
          fill = col,
          alpha = 0.10
        ))
      )
      if (!drawn) {
        next
      }
      layers <- c(
        layers,
        list(annotate(
          "rect",
          xmin = lanes$start[i],
          xmax = lanes$end[i],
          ymin = y_mid - half,
          ymax = y_mid + half,
          fill = col,
          alpha = 0.55
        ))
      )
      layers <- c(
        layers,
        list(if (isTRUE(lanes$inside[i])) {
          annotate(
            "text",
            x = lanes$start[i] + (lanes$end[i] - lanes$start[i]) / 2,
            y = y_mid,
            label = lanes$label[i],
            colour = .contrast_text(.blend(col, background, 0.55)),
            size = size,
            vjust = 0.5
          )
        } else {
          badge(lanes$x_text[i], lanes$hjust[i])
        })
      )
    } else {
      # A timestamp lands wherever its date falls, regularly on top of a bar,
      # so its label is a badge filled with the plot background: it reads as a
      # hole punched in the bars rather than as a coloured box of its own.
      layers <- c(
        layers,
        list(annotate(
          "segment",
          x = lanes$start[i],
          xend = lanes$start[i],
          y = -Inf,
          yend = if (drawn) y_mid else Inf,
          colour = col,
          linetype = "dashed",
          linewidth = 0.4
        ))
      )
      if (drawn) {
        layers <- c(layers, list(badge(lanes$x_text[i], lanes$hjust[i])))
      }
    }
  }
  layers
}

# --- Plot Building -----------------------------------------------------------

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

# Scale y-axis tick intervals to the value magnitude and the ticks that fit.
.count_step <- function(maxv, n = 6L) {
  n <- max(1L, as.integer(n))
  if (!is.finite(maxv) || maxv <= n) {
    return(1)
  }
  raw <- maxv / n
  mag <- 10^floor(log10(raw))
  for (m in c(1, 2, 5, 10)) {
    if (raw <= m * mag) {
      return(max(1, m * mag))
    }
  }
  max(1, 10 * mag)
}

#' Generate Discrete Integer Axis Breaks
#'
#' Calculates integer tick step bounds for discrete case-count y-axes.
#'
#' @param maxv Maximum numeric value on y-axis scale.
#' @param n Integer. Most intervals the axis has room to label.
#' @return Numeric vector of discrete axis breaks.
#' @export
count_breaks <- function(maxv, n = 6L) {
  seq(0, max(1, ceiling(maxv)), by = .count_step(maxv, n))
}

.strata_guide <- function(show_legend, ncol) {
  if (show_legend) {
    guide_legend(ncol = ncol)
  } else {
    "none"
  }
}

# Seed invisible zero-count points to force all categories into the legend key
.legend_seed_layer <- function(cats, mode, x0) {
  if (length(cats) < 2L || is.null(x0)) {
    return(NULL)
  }
  seed <- data.frame(
    date_bin = rep(x0, 2L * length(cats)),
    count = 0,
    stratum = rep(cats, each = 2L),
    stringsAsFactors = FALSE
  )
  if (identical(mode, "cumulative")) {
    geom_step(
      data = seed,
      aes(x = .data$date_bin, y = .data$count, colour = .data$stratum),
      na.rm = TRUE
    )
  } else {
    geom_col(
      data = seed,
      aes(x = .data$date_bin, y = .data$count, fill = .data$stratum),
      na.rm = TRUE
    )
  }
}

# Unroll counts into discrete unit blocks for tile unit display
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

# Every text size and gap the layout was solved against, set explicitly so the
# drawing takes exactly the room that was budgeted for it.
.epi_theme <- function(background, text_color, lay) {
  axis_pt <- lay$axis_pt
  title_pt <- lay$title_pt
  legend_pt <- lay$legend_pt
  theme_minimal(base_size = axis_pt) +
    theme(
      panel.grid = element_blank(),
      panel.background = element_rect(fill = background, colour = NA),
      plot.background = element_rect(fill = background, colour = NA),
      legend.background = element_rect(fill = background, colour = NA),
      legend.key = element_rect(fill = background, colour = NA),
      text = element_text(colour = text_color),
      axis.text = element_text(colour = text_color, size = axis_pt),
      axis.text.x = element_text(margin = margin(t = EPI_TEXT_GAP_PT)),
      axis.text.y = element_text(margin = margin(r = EPI_TEXT_GAP_PT)),
      axis.text.y.right = element_text(margin = margin(l = EPI_TEXT_GAP_PT)),
      axis.title = element_text(colour = text_color, size = title_pt),
      axis.title.x = element_text(
        size = lay$x_title_pt %||% title_pt,
        margin = margin(t = EPI_TITLE_GAP_PT)
      ),
      axis.title.y = element_text(
        size = lay$y_title_pt %||% title_pt,
        margin = margin(r = EPI_TITLE_GAP_PT)
      ),
      axis.title.y.right = element_text(
        size = lay$sec_title_pt %||% title_pt,
        margin = margin(l = EPI_TITLE_GAP_PT)
      ),
      # theme_minimal draws no ticks, and panel.grid is off above, so without
      # these the date labels float with nothing marking where they point.
      # The half-length minor ticks subdivide the gap between two labels, which
      # is what makes a date readable off a span running over several years.
      axis.ticks.x = element_line(colour = text_color, linewidth = 0.4),
      axis.ticks.length.x = unit(EPI_TICK_PT, "pt"),
      axis.ticks.length.y = unit(0, "pt"),
      axis.minor.ticks.length.x = rel(0.55),
      legend.position = "bottom",
      legend.box = "vertical",
      legend.title = element_blank(),
      legend.text = element_text(
        size = legend_pt,
        margin = margin(l = EPI_KEY_TEXT_GAP_PT)
      ),
      legend.key.size = unit(legend_pt * EPI_KEY_FRAC, "pt"),
      legend.key.spacing.x = unit(legend_pt * EPI_KEY_SPACING_X_FRAC, "pt"),
      legend.key.spacing.y = unit(legend_pt * EPI_KEY_SPACING_Y_FRAC, "pt"),
      legend.margin = margin(0, 0, 0, 0),
      legend.box.spacing = unit(EPI_LEGEND_GAP_PT, "pt"),
      plot.margin = unit(EPI_MARGIN_PT, "pt")
    )
}

#' Build Epidemiological Curve ggplot
#'
#' Constructs a customizable ggplot object for epidemiological data visualization.
#' Supports stacked bars, unit-block square cells, cumulative curves, rolling
#' averages, overlays, and milestone annotations.
#'
#' Every size on the figure comes from `opts$layout` — `epi_layout()`'s answer
#' for this data and canvas — or is solved here the same way when it is absent,
#' so a caller that sizes the image from the layout and a caller that does not
#' draw the same figure.
#'
#' @param binned Binned epi data frame.
#' @param opts Named list containing plot configuration flags (mode, square,
#'   interval, col_scale, reveal bounds, show_cumulative, show_moving_avg,
#'   layout, etc.).
#' @return Configured `ggplot` object.
#' @export
build_epi_ggplot <- function(binned, opts = list()) {
  lay <- opts$layout %||% epi_layout(binned, opts)
  mode <- opts$mode %||% "stacked"
  square <- isTRUE(opts$square) && identical(mode, "stacked")
  label_ends <- isTRUE(lay$end_drawn)
  show_cumulative <- isTRUE(opts$show_cumulative) && identical(mode, "stacked")
  show_moving_avg <- isTRUE(opts$show_moving_avg) &&
    !identical(mode, "cumulative")
  background <- opts$background %||% "#ffffff"
  text_color <- opts$text_color %||% "#000000"
  cumulative_color <- opts$cumulative_color %||% text_color
  moving_avg_color <- opts$moving_avg_color %||% text_color
  moving_avg_window <- opts$moving_avg_window %||% EPI_MOVING_AVG_WINDOW_DEFAULT
  moving_avg_align <- opts$moving_avg_align %||% "center"
  interval <- opts$interval %||% "day"
  width_days <- bin_width_days(interval)
  lay$case_days <- width_days

  if (identical(mode, "cumulative")) {
    binned <- epi_cumulate(binned)
  }
  cats <- epi_strata_levels(binned$stratum)
  pal <- if (identical(cats, EPI_ALL_LABEL) && !is.null(opts$single_color)) {
    setNames(opts$single_color, cats)
  } else {
    epi_palette(cats, epi_fit_scale(opts$col_scale, length(cats)))
  }
  show_legend <- isTRUE(lay$show_legend)

  x_range <- if (nrow(binned)) range(binned$date_bin) else NULL
  y_max <- .y_max(binned, mode)
  y_limit <- max(1, y_max)

  # Filter data to display range if animation reveal options are set
  shown <- binned
  if (!is.null(opts$reveal_from) && nrow(shown)) {
    shown <- shown[shown$date_bin >= opts$reveal_from, , drop = FALSE]
  }
  if (!is.null(opts$reveal_to) && nrow(shown)) {
    shown <- shown[shown$date_bin <= opts$reveal_to, , drop = FALSE]
  }

  # Prepare secondary cumulative trend overlay when requested
  cum_totals <- NULL
  cum_scale <- 1
  if (show_cumulative && nrow(binned)) {
    cum_totals <- binned |>
      group_by(date_bin) |>
      summarise(count = sum(count), .groups = "drop") |>
      arrange(date_bin) |>
      as.data.frame()
    cum_totals$cum <- cumsum(cum_totals$count)
    cum_scale <- max(cum_totals$cum, 1) / y_limit
    # The overlay rides the primary axis, so pre-scale it here and clamp: the
    # round-trip through cum_scale can land the final point a float hair above
    # y_limit, where the y scale would censor it as out of range.
    cum_totals$scaled <- pmin(cum_totals$cum / cum_scale, y_limit)
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

  # Prepare rolling moving average trend line overlay when requested
  mov_shown <- NULL
  if (show_moving_avg && nrow(binned)) {
    mov_shown <- epi_moving_average(
      binned,
      moving_avg_window,
      interval,
      moving_avg_align
    )
    if (!is.null(opts$reveal_from)) {
      mov_shown <- mov_shown[
        mov_shown$date_bin >= opts$reveal_from,
        ,
        drop = FALSE
      ]
    }
    if (!is.null(opts$reveal_to)) {
      mov_shown <- mov_shown[
        mov_shown$date_bin <= opts$reveal_to,
        ,
        drop = FALSE
      ]
    }
  }

  # The end labels' pitch is a length on the page, so it is converted to
  # counts against the panel the layout measured.
  y_span <- y_limit * (1 + .Y_HEADROOM)
  data_in <- max(lay$panel_height_in - (lay$band_in %||% 0), 0.1)
  end_positions <- if (label_ends) {
    .cumulative_end_positions(
      shown,
      y_max,
      min_gap = lay$end_pitch_in * y_span / data_in,
      offset = EPI_LABEL_GAP_IN / lay$day_in
    )
  } else {
    NULL
  }
  bottom_room <- if (!is.null(end_positions)) {
    half <- lay$end_pitch_in * y_span / data_in / 2
    min(half, max(0, half - min(end_positions$y_label)))
  } else {
    0
  }
  band <- .band_counts(lay, y_span + bottom_room)

  # The limits themselves run past the counts rather than an expansion doing
  # it: ggplot2 censors anything outside `limits`, and an end label pushed
  # below zero or an annotation lane above the headroom is exactly that.
  y_scale_args <- list(
    breaks = count_breaks(y_max, lay$n_y_breaks %||% 6L),
    limits = c(-bottom_room, y_span + band),
    expand = expansion(0, 0)
  )
  if (!is.null(cum_totals)) {
    y_scale_args$sec.axis <- sec_axis(
      ~ . * cum_scale,
      name = if (is.null(lay$sec_title_pt)) NULL else "Cumulative cases"
    )
  }

  x_scale_args <- list(
    date_labels = .date_labels(interval),
    limits = lay$xlim,
    guide = guide_axis(minor.ticks = TRUE)
  )
  if (length(lay$x_breaks)) {
    x_scale_args$breaks <- lay$x_breaks
  }

  # A trimmed legend lists its kept keys and then a count of the rest; that
  # last key is a level no data carries, so it is drawn as a label with no
  # swatch beside it.
  keys <- lay$legend_keys %||% cats
  extra <- setdiff(keys, cats)
  limits <- c(cats, extra)
  values <- c(pal, setNames(rep("transparent", length(extra)), extra))
  p <- ggplot() +
    .epi_theme(background, text_color, lay) +
    scale_fill_manual(
      values = values,
      drop = FALSE,
      limits = limits,
      breaks = if (length(keys)) keys else cats
    ) +
    scale_colour_manual(
      values = values,
      drop = FALSE,
      limits = limits,
      breaks = if (length(keys)) keys else cats
    ) +
    do.call(scale_y_continuous, y_scale_args) +
    do.call(scale_x_date, x_scale_args) +
    labs(
      x = if (is.null(lay$x_title_pt)) NULL else "Date of collection",
      y = if (is.null(lay$y_title_pt)) {
        NULL
      } else if (identical(mode, "cumulative")) {
        "Cumulative cases"
      } else {
        "Number of cases"
      }
    ) +
    guides(
      fill = .strata_guide(show_legend, lay$legend_ncol),
      colour = .strata_guide(show_legend, lay$legend_ncol)
    )

  if (show_legend) {
    seed <- .legend_seed_layer(
      cats,
      mode,
      if (!is.null(x_range)) x_range[1] else NULL
    )
    if (!is.null(seed)) {
      p <- p + seed
    }
  }

  p <- p + .mode_layers(shown, mode, square, interval)

  if (label_ends) {
    p <- p + .cumulative_end_labels(end_positions, lay$end_pt)
  }

  layers <- .annotation_layers(lay, y_span, band, background)
  for (l in layers) {
    p <- p + l
  }

  # Overlay double-line moving average (background outline + foreground stroke)
  if (!is.null(mov_shown) && nrow(mov_shown) > 1) {
    mov_shown$x <- mov_shown$date_bin +
      exact_bin_widths(mov_shown$date_bin, interval) / 2
    ma_label <- epi_moving_avg_label(moving_avg_window, interval)
    mov_shown$series <- ma_label
    p <- p +
      geom_line(
        data = mov_shown,
        mapping = aes(x = .data$x, y = .data$avg),
        colour = background,
        linewidth = 2
      ) +
      geom_line(
        data = mov_shown,
        mapping = aes(x = .data$x, y = .data$avg, linetype = .data$series),
        colour = moving_avg_color,
        linewidth = 0.9
      ) +
      scale_linetype_manual(
        name = NULL,
        values = setNames("solid", ma_label),
        guide = guide_legend(
          order = 99,
          override.aes = list(colour = moving_avg_color, linewidth = 0.9)
        )
      )
  }

  # Overlay double-line cumulative trend step curve
  if (!is.null(cum_shown) && nrow(cum_shown)) {
    mapping <- aes(x = .data$date_bin, y = .data$scaled)
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
    p <- p + coord_fixed(ratio = width_days)
  }
  p
}

#' Compute Aspect Ratio for Square Unit Block Mode
#'
#' Calculates the height-to-width ratio required to maintain square cell aspect
#' rendering under fixed coordinate constraints.
#'
#' @param binned Binned epi data frame.
#' @param mode Selected rendering mode string.
#' @return Numeric panel aspect ratio or NULL.
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

.x_limits <- function(x_range, annos, interval) {
  if (is.null(x_range)) {
    return(NULL)
  }
  pad <- bin_width_days(interval) / 4
  last_width <- exact_bin_widths(x_range[2], interval)
  lims <- c(x_range[1] - pad, x_range[2] + last_width + pad)
  # Expand scale limits if milestone dates fall outside bin data bounds
  if (!is.null(annos) && nrow(annos)) {
    dates <- c(annos$start, annos$end[!is.na(annos$end)])
    lims <- c(min(lims[1], dates), max(lims[2], dates))
  }
  as.Date(lims, origin = "1970-01-01")
}

# Where each end-of-line label goes: beside its line's last value, pushed down
# where it would sit closer than one label pitch (`min_gap`, in counts) to the
# label above it.
.cumulative_end_positions <- function(shown, y_max, min_gap, offset) {
  if (!nrow(shown)) {
    return(NULL)
  }
  last_bin <- max(shown$date_bin)
  ends <- shown[shown$date_bin == last_bin, , drop = FALSE]
  ends <- ends[order(-ends$count), , drop = FALSE]

  y_label <- numeric(nrow(ends))
  for (i in seq_len(nrow(ends))) {
    y <- ends$count[i]
    if (i > 1 && (y_label[i - 1] - y) < min_gap) {
      y <- y_label[i - 1] - min_gap
    }
    y_label[i] <- y
  }
  ends$y_label <- y_label
  ends$x_label <- ends$date_bin + offset
  ends
}

.cumulative_end_labels <- function(end_positions, pt) {
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
    size = pt / .GG_PT,
    show.legend = FALSE
  )
}

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
        x = .data$date_bin + .data$width / 2,
        y = .data$y_idx - 0.5,
        fill = .data$stratum,
        width = .data$width
      ),
      height = 1,
      colour = "#FFFFFF",
      linewidth = 0.3
    ))
  }
  # Drawn as rectangles with the stack solved here rather than as stacked
  # columns, because a calendar bin is not a fixed width: February is shorter
  # than March, and a `width` aesthetic is not one `geom_col()` declares — it
  # honoured it while warning about it on every draw, which is what filled the
  # console when the window was dragged.
  shown$width <- exact_bin_widths(shown$date_bin, interval)
  geom_rect(
    data = .stacked_bars(shown),
    aes(
      xmin = .data$date_bin,
      xmax = .data$date_bin + .data$width,
      ymin = .data$ymin,
      ymax = .data$ymax,
      fill = .data$stratum
    ),
    colour = "#FFFFFF",
    linewidth = 0.15
  )
}

# Where each stratum's block sits within its bin.
#
# `position_stack()` lays the *first* factor level on top, so the running total
# is accumulated in reverse level order to match — the fill scale and the bars
# have to agree about which colour is where.
.stacked_bars <- function(shown) {
  lv <- if (is.factor(shown$stratum)) {
    levels(shown$stratum)
  } else {
    epi_strata_levels(shown$stratum)
  }
  order_in_bin <- match(as.character(shown$stratum), rev(lv))
  out <- shown[order(shown$date_bin, order_in_bin), , drop = FALSE]
  top <- ave(out$count, out$date_bin, FUN = cumsum)
  out$ymax <- top
  out$ymin <- top - out$count
  out
}

# File export and the on-screen image both go through app/logic/viz_export.R
# (save_plot_export(), render_canvas_png()), which owns the device settings for
# every plot type.
