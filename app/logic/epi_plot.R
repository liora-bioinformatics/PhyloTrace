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
    element_rect,
    element_text,
    expansion,
    geom_col,
    geom_line,
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

# Conversion constants for estimating font pixel dimensions in ggplot
.GG_PT <- 72.27 / 25.4
.anno_char_px <- function(size = 3) size * .GG_PT * (96 / 72) * 0.5

.wrap_label <- function(label, max_chars, max_lines = 2L) {
  max_chars <- max(1L, as.integer(max_chars))
  if (nchar(label) <= max_chars) {
    return(label)
  }
  lines <- unlist(lapply(strwrap(label, width = max_chars), function(l) {
    if (nchar(l) <= max_chars) {
      return(l)
    }
    starts <- seq(1L, nchar(l), by = max_chars)
    substring(l, starts, pmin(starts + max_chars - 1L, nchar(l)))
  }))
  # Truncate lines exceeding max limit with ellipsis
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

.lane_height <- 0.075
.max_lanes <- 6L
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
#' @param annos Data frame of annotations.
#' @param x_range Plot x-axis domain range bounds.
#' @return Input data frame augmented with `lane` and `period` indicators.
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
  top <- max(y_max, 1)
  layers <- list()

  char_px <- .anno_char_px(3)
  panel_px <- (plot_width %||% 900) - 70
  axis_days <- if (!is.null(xlim)) as.numeric(diff(range(xlim))) else NA_real_

  for (i in seq_len(nrow(lanes))) {
    # Wrap lane placement within max allowable lane limits
    lane <- ((lanes$lane[i] - 1L) %% .max_lanes)
    y_top <- top * (1 - lane * .lane_height)
    y_bot <- y_top - top * .lane_height * 0.72
    y_mid <- (y_top + y_bot) / 2

    col <- .anno_color(lanes, i)

    if (lanes$period[i]) {
      label_col <- .contrast_text(.blend(col, background, 0.55))
      label <- lanes$label[i]
      # Auto-wrap text if rendering width constraints are present
      if (is.finite(axis_days) && axis_days > 0) {
        bracket_days <- as.numeric(lanes$end[i] - lanes$start[i])
        avail_px <- (bracket_days / axis_days) * panel_px
        label <- .wrap_label(label, floor(avail_px / char_px), max_lines = 2L)
      }
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
      # Render vertical milestone line and offset text label
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

# Scale y-axis tick intervals dynamically based on value magnitude
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

#' Generate Discrete Integer Axis Breaks
#'
#' Calculates integer tick step bounds for discrete case-count y-axes.
#'
#' @param maxv Maximum numeric value on y-axis scale.
#' @return Numeric vector of discrete axis breaks.
#' @export
count_breaks <- function(maxv) {
  seq(0, max(1, ceiling(maxv)), by = .count_step(maxv))
}

#' Estimate Legend Column Capacity
#'
#' Calculates maximum legend column count given element text widths and total
#' rendering width in order to wrap long keys cleanly.
#'
#' @param cats Character vector of category names.
#' @param width_px Render target width in pixels.
#' @param base_size Font size base setting.
#' @return Integer target column count.
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
  char_px <- base_size * 0.8 * (96 / 72) * 0.5
  entry_px <- 34 + char_px * max(nchar(cats))
  cols <- floor(w / entry_px)
  max(1L, min(n, as.integer(cols)))
}

.strata_guide <- function(show_legend, cats, plot_width) {
  if (show_legend) {
    guide_legend(ncol = epi_legend_ncol(cats, plot_width))
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

.epi_theme <- function(background, text_color) {
  theme_minimal(base_size = 13) +
    theme(
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

#' Build Epidemiological Curve ggplot
#'
#' Constructs a customizable ggplot object for epidemiological data visualization.
#' Supports stacked bars, unit-block square cells, cumulative curves, rolling
#' averages, overlays, and milestone annotations.
#'
#' @param binned Binned epi data frame.
#' @param opts Named list containing plot configuration flags (mode, square,
#'   interval, col_scale, reveal bounds, show_cumulative, show_moving_avg, etc.).
#' @return Configured `ggplot` object.
#' @export
build_epi_ggplot <- function(binned, opts = list()) {
  mode <- opts$mode %||% "stacked"
  square <- isTRUE(opts$square) && identical(mode, "stacked")
  label_ends <- isTRUE(opts$label_ends) && identical(mode, "cumulative")
  show_cumulative <- isTRUE(opts$show_cumulative) && identical(mode, "stacked")
  show_moving_avg <- isTRUE(opts$show_moving_avg) &&
    !identical(mode, "cumulative")
  show_x_label <- opts$show_x_label %||% TRUE
  background <- opts$background %||% "#ffffff"
  text_color <- opts$text_color %||% "#000000"
  cumulative_color <- opts$cumulative_color %||% text_color
  moving_avg_color <- opts$moving_avg_color %||% text_color
  moving_avg_window <- opts$moving_avg_window %||% EPI_MOVING_AVG_WINDOW_DEFAULT
  moving_avg_align <- opts$moving_avg_align %||% "center"
  interval <- opts$interval %||% "day"
  width_days <- bin_width_days(interval)

  if (identical(mode, "cumulative")) {
    binned <- epi_cumulate(binned)
  }
  cats <- sort(unique(binned$stratum))
  pal <- if (identical(cats, EPI_ALL_LABEL) && !is.null(opts$single_color)) {
    setNames(opts$single_color, cats)
  } else {
    epi_palette(cats, epi_fit_scale(opts$col_scale, length(cats)))
  }
  show_legend <- opts$show_legend %||%
    (!identical(cats, EPI_ALL_LABEL) && !label_ends)

  x_range <- if (nrow(binned)) range(binned$date_bin) else NULL
  fixed_axis <- if (is.null(opts$fixed_axis)) TRUE else isTRUE(opts$fixed_axis)
  axis_range <- if (fixed_axis || is.null(x_range)) {
    x_range
  } else {
    c(opts$reveal_from %||% x_range[1], opts$reveal_to %||% x_range[2])
  }
  y_max <- .y_max(binned, mode)

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

  end_positions <- if (label_ends) {
    .cumulative_end_positions(shown, y_max)
  } else {
    NULL
  }
  bottom_room <- if (!is.null(end_positions)) {
    half <- .label_half_height(y_max)
    min(half, max(0, half - min(end_positions$y_label)))
  } else {
    0
  }

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

  xlim <- .x_limits(
    axis_range,
    opts$annos,
    interval,
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
      x = if (show_x_label) "Date of collection" else NULL,
      y = if (identical(mode, "cumulative")) {
        "Cumulative cases"
      } else {
        "Number of cases"
      }
    ) +
    guides(
      fill = .strata_guide(show_legend, cats, opts$plot_width),
      colour = .strata_guide(show_legend, cats, opts$plot_width)
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
  # Expand scale limits if milestone dates fall outside bin data bounds
  if (!is.null(annos) && nrow(annos)) {
    dates <- c(annos$start, annos$end[!is.na(annos$end)])
    lims <- c(min(lims[1], dates), max(lims[2], dates))
  }
  as.Date(lims, origin = "1970-01-01")
}

# Adjust end-of-line label positions on cumulative plots to prevent overlaps
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

  span <- as.numeric(if (nrow(shown) > 1) diff(range(shown$date_bin)) else 1)
  offset <- max(1, span * 0.015)
  ends$x_label <- ends$date_bin + offset

  ends
}

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
  shown$width <- exact_bin_widths(shown$date_bin, interval)
  geom_col(
    data = shown,
    aes(
      x = .data$date_bin + .data$width / 2,
      y = .data$count,
      fill = .data$stratum,
      width = .data$width
    ),
    colour = "#FFFFFF",
    linewidth = 0.15
  )
}

# --- Export Utilities --------------------------------------------------------

# File export goes through app/logic/viz_export.R's save_plot_export(), which
# owns the device settings for every plot type; what stays here is the on-screen
# render below, which is sized in pixels rather than in physical units.

#' Render Epi Curve PNG Buffer for Display
#'
#' Writes the plot to a temporary PNG matching exact screen target resolution bounds.
#'
#' @param plot ggplot2 instance.
#' @param file Target temporary output file path.
#' @param width_px Target width in pixels.
#' @param height_px Target height in pixels.
#' @param res Base display resolution in DPI (defaults to 96).
#' @param scale High-DPI scaling factor (e.g. devicePixelRatio).
#' @export
render_epi_png <- function(
  plot,
  file,
  width_px,
  height_px,
  res = 96,
  scale = 1
) {
  ggsave(
    filename = file,
    plot = plot,
    device = "png",
    width = width_px / res,
    height = height_px / res,
    dpi = res * scale,
    limitsize = FALSE
  )
}
