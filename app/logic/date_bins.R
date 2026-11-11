# app/logic/date_bins.R
#
# Shared date parsing (full dates and bare years) and calendar binning. Turns a
# date column into a coarser categorical grouping (day, week, month or year) so a
# near-unique date field can drive a legend, a pie or a palette instead of one
# level per isolate, and never splits a year-only value finer than a year.
#
# Deliberately free of app dependencies so every engine — including the pure
# plotting modules — can import it without pulling in database code.

#' Granularities a mapped date variable can be grouped by, fine to coarse.
#' @export
DATE_GRANULARITIES <- c(
  Day = "day",
  Week = "week",
  Month = "month",
  Year = "year"
)

#' Sentinel for a date variable left ungrouped, so it stays a continuous scale.
#' @export
DATE_GRANULARITY_NONE <- "none"

#' Types whose columns can be grouped by a calendar interval.
#' @export
DATE_TYPES <- c("date", "datetime", "year")

# A full calendar date, optionally followed by a time of day ("2024-03-15",
# "2024/3/5", "2024-03-15 10:30:00", "2024-03-15T10:30:00Z").
.DAY_RE <- "^[0-9]{4}[-/][0-9]{1,2}[-/][0-9]{1,2}([ T].*)?$"

# A bare four-digit year ("2024"): a date recorded only to the year.
.YEAR_RE <- "^[0-9]{4}$"

#' Which values are recorded only to the year
#'
#' A bare year ("2021") is a date known to the year, not 1 January of it. Every
#' function that groups or filters dates checks this so such a value is never
#' placed in a month, week or day it was not recorded at.
#'
#' @param x Raw column values.
#' @return Logical vector, `FALSE` for missing values and for `Date` input.
#' @export
year_only <- function(x) {
  if (inherits(x, c("Date", "POSIXt"))) {
    return(rep(FALSE, length(x)))
  }
  v <- trimws(as.character(x))
  !is.na(v) & grepl(.YEAR_RE, v)
}

#' Safely Parse Input to Date Vector
#'
#' Parses each value on its own, so one unreadable entry never blanks the rest
#' (`as.Date()` on a character vector errors, or returns all NA, when its first
#' value fits no format). Accepts ISO dates (`-` or `/` separated; a time of
#' day is dropped) and bare years, which are anchored at 1 January — see
#' `date_period_end()` for where such a year ends.
#'
#' @param x Vector of date strings, timestamps, or raw input.
#' @return Vector of `Date` values with unparseable entries set to NA.
#' @export
parse_dates <- function(x) {
  if (inherits(x, "Date")) {
    return(x)
  }
  if (inherits(x, "POSIXt")) {
    return(as.Date(format(x, "%Y-%m-%d")))
  }
  v <- trimws(as.character(x))
  out <- rep(as.Date(NA), length(v))
  day <- !is.na(v) & grepl(.DAY_RE, v)
  if (any(day)) {
    ymd <- chartr("/", "-", sub("[ T].*$", "", v[day]))
    out[day] <- as.Date(ymd, format = "%Y-%m-%d")
  }
  year <- year_only(v)
  if (any(year)) {
    out[year] <- as.Date(paste0(v[year], "-01-01"), format = "%Y-%m-%d")
  }
  out
}

#' Last Day Each Date Value Covers
#'
#' A full date covers itself; a bare year runs to 31 December. With
#' `parse_dates()` (the first day) this is the period a value stands for.
#'
#' @param x Raw column values.
#' @return Date vector, NA where the value is missing or unreadable.
#' @export
date_period_end <- function(x) {
  out <- parse_dates(x)
  year <- year_only(x) & !is.na(out)
  if (any(year)) {
    out[year] <- as.Date(paste0(format(out[year], "%Y"), "-12-31"), format = "%Y-%m-%d")
  }
  out
}

#' Which Values Fall Inside a Date Window
#'
#' A value is kept when the period it stands for overlaps the window, so a bare
#' year "2021" filtered to March 2021 is kept: it may have been collected then,
#' and dropping it would hide it from every window but a whole-year one.
#' Missing and unreadable values are never kept.
#'
#' @param x Raw column values.
#' @param rng Length-2 vector of the window's first and last day.
#' @return Logical vector.
#' @export
in_date_range <- function(x, rng) {
  rng <- as.Date(rng)
  start <- parse_dates(x)
  !is.na(start) & start <= rng[[2]] & date_period_end(x) >= rng[[1]]
}

#' First and Last Day a Date Column Covers
#'
#' @param x Raw column values.
#' @return Length-2 Date vector, or NULL when nothing parses.
#' @export
date_extent <- function(x) {
  start <- parse_dates(x)
  if (all(is.na(start))) {
    return(NULL)
  }
  c(min(start, na.rm = TRUE), max(date_period_end(x), na.rm = TRUE))
}

#' Precision a Date Column Is Recorded At
#'
#' "year" when every readable value is a bare year, otherwise "day". A column
#' mixing both is "day"; its year-only values drop out of finer groupings (see
#' `floor_date_bin()`) rather than being pinned to 1 January.
#'
#' @param x Raw column values.
#' @return "day" or "year".
#' @export
date_precision <- function(x) {
  readable <- !is.na(parse_dates(x))
  if (any(readable) && all(year_only(x)[readable])) "year" else "day"
}

#' Granularities a Date Column Can Be Grouped By
#'
#' Nothing finer than the column's own precision: a year cannot be split into
#' months.
#'
#' @param x Raw column values.
#' @return Named subset of `DATE_GRANULARITIES`.
#' @export
available_granularities <- function(x) {
  if (identical(date_precision(x), "year")) {
    DATE_GRANULARITIES["Year"]
  } else {
    DATE_GRANULARITIES
  }
}

#' Whether a granularity actually asks for binning.
#'
#' NULL, NA, "" and the explicit "none" sentinel all mean "leave it alone",
#' which is what a restored snapshot from before this option existed carries.
#'
#' @param granularity A granularity slug, or NULL.
#' @return TRUE when the value names a real calendar interval.
#' @export
is_binned <- function(granularity) {
  !is.null(granularity) &&
    length(granularity) == 1L &&
    !is.na(granularity) &&
    nzchar(granularity) &&
    !identical(as.character(granularity), DATE_GRANULARITY_NONE) &&
    as.character(granularity) %in% DATE_GRANULARITIES
}

#' Floor Collection Dates to Interval Starts
#'
#' Truncates input dates to the start of the specified temporal interval.
#' ISO-8601 week starts on Monday. Unparseable dates return NA, and so does a
#' bare year floored to anything finer than a year: which month or week it
#' fell in is unknown, and 1 January would be a fabricated answer.
#'
#' @param dates Vector of dates or date-like character strings.
#' @param interval String; target bin width ("day", "week", "month", "year").
#' @return Date vector floored to interval bounds.
#' @export
floor_date_bin <- function(dates, interval = "day") {
  d <- parse_dates(dates)
  interval <- tolower(interval)
  out <- switch(
    interval,
    day = d,
    week = d - (as.numeric(format(d, "%u")) - 1),
    month = as.Date(format(d, "%Y-%m-01")),
    year = as.Date(format(d, "%Y-01-01")),
    stop("Unknown interval: ", interval, call. = FALSE)
  )
  if (!identical(interval, "year")) {
    out[year_only(dates)] <- NA
  }
  out
}

#' Display label for each floored date, at the interval's own resolution.
#'
#' Labels are chosen to sort lexicographically in calendar order, so a legend
#' built from `sort(unique(...))` reads chronologically without extra work.
#' The ISO week label uses the ISO year (%G), which can differ from the
#' calendar year in the days either side of New Year.
#'
#' @param floored Date vector already passed through `floor_date_bin()`.
#' @param interval String; the interval those dates were floored to.
#' @return Character vector of labels, NA preserved.
#' @export
bin_labels <- function(floored, interval) {
  switch(
    tolower(interval),
    day = format(floored, "%Y-%m-%d"),
    week = ifelse(
      is.na(floored),
      NA_character_,
      paste0(format(floored, "%G"), "-W", format(floored, "%V"))
    ),
    month = format(floored, "%Y-%m"),
    year = format(floored, "%Y"),
    stop("Unknown interval: ", interval, call. = FALSE)
  )
}

#' Group a date column into an ordered categorical variable.
#'
#' The one function the engines call. Without a granularity it parses only, so
#' the value stays a `Date` and continues to drive a continuous scale exactly
#' as before. With one, it returns an ordered factor whose levels run
#' chronologically — every engine's discrete path (legend, pie, palette) then
#' treats it as the category it now is.
#'
#' A column recorded only to the year is always grouped by year, whatever finer
#' interval is asked for (a layer saved before the column held bare years).
#'
#' @param x Raw column values, typically character out of SQLite.
#' @param granularity One of `DATE_GRANULARITIES`, or NULL/"none" for no binning.
#' @return A `Date` vector, or an ordered factor of interval labels.
#' @export
bin_date_values <- function(x, granularity = NULL) {
  if (!is_binned(granularity)) {
    return(parse_dates(x))
  }
  if (identical(date_precision(x), "year")) {
    granularity <- "year"
  }
  floored <- floor_date_bin(x, granularity)
  present <- sort(unique(floored[!is.na(floored)]))
  factor(
    bin_labels(floored, granularity),
    levels = bin_labels(present, granularity),
    ordered = TRUE
  )
}

#' How many groups a column would fall into at a given granularity.
#'
#' Used to re-rank a date variable's palette once it stops being continuous:
#' twelve months fit a qualitative palette, six hundred days do not.
#'
#' @param x Raw column values.
#' @param granularity One of `DATE_GRANULARITIES`, or NULL/"none".
#' @return Integer count of non-missing distinct groups.
#' @export
binned_levels <- function(x, granularity = NULL) {
  v <- bin_date_values(x, granularity)
  if (is.factor(v)) {
    return(length(levels(v)))
  }
  length(unique(v[!is.na(v)]))
}

#' Most groups a date should fall into before a coarser interval is preferred.
#'
#' Twelve so that a year of collection dates lands on months rather than being
#' pushed straight to a single year bar.
#' @export
DATE_MAX_GROUPS <- 12L

#' Finest granularity that still groups a date column readably.
#'
#' Picking a date variable should just work: the raw column is near-unique per
#' isolate and groups nothing, so something has to be chosen for it. This walks
#' fine to coarse and takes the first interval that gets under `max_groups`,
#' falling back to the coarsest when even years are too many.
#'
#' @param x Raw column values.
#' @param max_groups Upper bound on how many groups are acceptable.
#' @return A granularity slug.
#' @export
default_granularity <- function(x, max_groups = DATE_MAX_GROUPS) {
  for (g in available_granularities(x)) {
    if (binned_levels(x, g) <= max_groups) {
      return(unname(g))
    }
  }
  unname(DATE_GRANULARITIES[length(DATE_GRANULARITIES)])
}

#' The interval a mapped date is grouped by unless the reader says otherwise.
#'
#' The one place this decision is made, because every engine has to make it and
#' they were each making a different one: a date arrived ungrouped and drew a
#' *continuous* scale, whose legend is three quantiles (first, middle, last)
#' whatever the column holds. On the reference database that is 213 collection
#' dates rendered as 213 all-but-identical colours under a three-line key —
#' nothing a reader can match a node to. Grouping first is what makes a date
#' behave like the categorical variable a legend can carry.
#'
#' `default_granularity()` picks the *finest* interval that stays inside
#' `max_groups`, so a fortnight of dates still comes out by day and a decade
#' comes out by year — the coarseness follows the data rather than a fixed rule.
#' "Exact date" remains available in every engine's own control; this is the
#' starting point, not a restriction.
#'
#' @param x Raw column values.
#' @param max_groups Upper bound on how many groups are acceptable.
#' @return A granularity slug, or NULL when there is no date to group.
#' @export
mapped_granularity <- function(x, max_groups = DATE_MAX_GROUPS) {
  if (is.null(x) || !length(x)) {
    return(NULL)
  }
  # Nothing parseable is not a date column, whatever the schema says of it.
  if (!binned_levels(x, DATE_GRANULARITY_NONE)) {
    return(NULL)
  }
  default_granularity(x, max_groups)
}

#' Human-readable name for a granularity, for a layer card or a legend title.
#'
#' @param granularity One of `DATE_GRANULARITIES`, or NULL/"none".
#' @return Single string, or NULL when nothing is binned.
#' @export
granularity_label <- function(granularity) {
  if (!is_binned(granularity)) {
    return(NULL)
  }
  hit <- names(DATE_GRANULARITIES)[
    DATE_GRANULARITIES == as.character(granularity)
  ]
  if (length(hit)) hit[[1]] else NULL
}

#' Types a Timeline Can Be Drawn Along
#'
#' Calendar dates and years. A timestamp (`called_at`, when an isolate was
#' typed) is left out: it records the lab's work, not the sample, and offering
#' it would quietly stand in for a missing collection date.
#' @export
TIMELINE_TYPES <- c("date", "year")

#' Date Variables a Timeline Can Be Drawn Along
#'
#' Standard metadata before custom variables (the profile frame's own group
#' order), with `preferred` — the collection date — always first.
#'
#' @param profiles Field profile frame with `field` and `type` columns.
#' @param preferred Column to list first when present.
#' @return Character vector of column names.
#' @export
timeline_fields <- function(profiles, preferred = "sample_collection_date") {
  if (!is.data.frame(profiles) || !nrow(profiles)) {
    return(character(0))
  }
  fields <- profiles$field[profiles$type %in% TIMELINE_TYPES]
  c(intersect(preferred, fields), setdiff(fields, preferred))
}

#' First Date Variable That Holds Any Readable Date
#'
#' @param meta Data frame of isolate metadata.
#' @param fields Candidate columns, in order of preference.
#' @return A column name, or NULL when none holds a readable date.
#' @export
first_dated_field <- function(meta, fields) {
  for (f in fields) {
    if (f %in% names(meta) && any(!is.na(parse_dates(meta[[f]])))) {
      return(f)
    }
  }
  NULL
}
