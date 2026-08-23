# app/logic/date_bins.R
#
# Shared date parsing and calendar binning. Turns a date column into a coarser
# categorical grouping (day, week, month or year) so a near-unique date field
# can drive a legend, a pie or a palette instead of one level per isolate.
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
DATE_TYPES <- c("date", "datetime")

#' Safely Parse Input to Date Vector
#'
#' Converts character or raw metadata vectors into `Date` objects. Returns an
#' all-NA Date vector if parsing fails completely, avoiding execution errors.
#'
#' @param x Vector of date strings, timestamps, or raw input.
#' @return Vector of `Date` values with unparseable entries set to NA.
#' @export
parse_dates <- function(x) {
  if (inherits(x, "Date")) {
    return(x)
  }
  tryCatch(
    suppressWarnings(as.Date(as.character(x))),
    error = function(e) rep(as.Date(NA), length(x))
  )
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
#' ISO-8601 week starts on Monday. Unparseable dates return NA.
#'
#' @param dates Vector of dates or date-like character strings.
#' @param interval String; target bin width ("day", "week", "month", "year").
#' @return Date vector floored to interval bounds.
#' @export
floor_date_bin <- function(dates, interval = "day") {
  d <- parse_dates(dates)
  switch(
    tolower(interval),
    day = d,
    week = d - (as.numeric(format(d, "%u")) - 1),
    month = as.Date(format(d, "%Y-%m-01")),
    year = as.Date(format(d, "%Y-01-01")),
    stop("Unknown interval: ", interval, call. = FALSE)
  )
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
#' @param x Raw column values, typically character out of SQLite.
#' @param granularity One of `DATE_GRANULARITIES`, or NULL/"none" for no binning.
#' @return A `Date` vector, or an ordered factor of interval labels.
#' @export
bin_date_values <- function(x, granularity = NULL) {
  parsed <- parse_dates(x)
  if (!is_binned(granularity)) {
    return(parsed)
  }
  floored <- floor_date_bin(parsed, granularity)
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
  for (g in DATE_GRANULARITIES) {
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
