box::use(
  testthat[
    expect_error,
    expect_false,
    expect_identical,
    expect_length,
    expect_null,
    expect_s3_class,
    expect_true,
    test_that
  ],
)
box::use(
  app / logic / date_bins,
)

# --- parse_dates -------------------------------------------------------------

test_that("parse_dates leaves an unparseable entry as NA rather than erroring", {
  # The collection-date column is free text, so junk reaches the parser.
  out <- date_bins$parse_dates(c("2026-01-14", "not a date", NA))

  expect_s3_class(out, "Date")
  expect_identical(is.na(out), c(FALSE, TRUE, TRUE))
})

test_that("parse_dates passes a Date through untouched", {
  d <- as.Date(c("2026-01-14", "2026-02-03"))
  expect_identical(date_bins$parse_dates(d), d)
})

# --- floor_date_bin ----------------------------------------------------------

test_that("floor_date_bin rounds down to each interval's start", {
  d <- as.Date(c("2026-01-14", "2026-02-03"))

  expect_identical(date_bins$floor_date_bin(d, "day"), d)
  expect_identical(
    date_bins$floor_date_bin(d, "month"),
    as.Date(c("2026-01-01", "2026-02-01"))
  )
  expect_identical(
    date_bins$floor_date_bin(d, "year"),
    as.Date(c("2026-01-01", "2026-01-01"))
  )
})

test_that("floor_date_bin weeks start on the ISO Monday", {
  # Wednesday and the Sunday of the same ISO week collapse onto one Monday;
  # the next day (Monday) must start a new bin rather than join it.
  week <- date_bins$floor_date_bin(
    as.Date(c("2026-01-14", "2026-01-18", "2026-01-19")),
    "week"
  )

  expect_identical(week, as.Date(c("2026-01-12", "2026-01-12", "2026-01-19")))
  expect_identical(unique(format(week, "%u")), "1")
})

test_that("floor_date_bin is NA-safe for every interval", {
  for (interval in date_bins$DATE_GRANULARITIES) {
    binned <- date_bins$floor_date_bin(as.Date(c("2026-01-14", NA)), interval)
    expect_length(binned, 2L)
    expect_false(is.na(binned[1]))
    expect_true(is.na(binned[2]))
  }
})

test_that("floor_date_bin rejects an unknown interval", {
  expect_error(
    date_bins$floor_date_bin(as.Date("2026-01-14"), "fortnight"),
    "fortnight"
  )
})

# --- is_binned ---------------------------------------------------------------

test_that("only a real interval counts as binned", {
  # NULL and NA are what a snapshot saved before this option existed restores
  # to, so they must mean "leave the variable continuous".
  expect_false(date_bins$is_binned(NULL))
  expect_false(date_bins$is_binned(NA_character_))
  expect_false(date_bins$is_binned(""))
  expect_false(date_bins$is_binned("none"))
  expect_false(date_bins$is_binned("fortnight"))

  for (interval in date_bins$DATE_GRANULARITIES) {
    expect_true(date_bins$is_binned(interval))
  }
})

# --- bin_date_values ---------------------------------------------------------

test_that("an unbinned date stays a Date, so its scale stays continuous", {
  out <- date_bins$bin_date_values(c("2026-01-14", "2026-02-03"), NULL)

  expect_s3_class(out, "Date")
  expect_identical(out, as.Date(c("2026-01-14", "2026-02-03")))
})

test_that("binning collapses distinct dates into one level per interval", {
  # The reported problem: a collection date is near-unique per isolate, so it
  # groups nothing until it is binned.
  d <- c("2026-01-14", "2026-01-28", "2026-02-03")

  months <- date_bins$bin_date_values(d, "month")
  expect_s3_class(months, "factor")
  expect_identical(as.character(months), c("2026-01", "2026-01", "2026-02"))
  expect_identical(levels(months), c("2026-01", "2026-02"))

  expect_identical(
    as.character(date_bins$bin_date_values(d, "year")),
    rep("2026", 3)
  )
})

test_that("levels run in calendar order, not alphabetical accident", {
  # A legend is built from the factor's levels, so their order is the order a
  # reader sees. Crossing a year boundary is where a plain sort goes wrong.
  out <- date_bins$bin_date_values(
    c("2026-02-03", "2025-12-01", "2026-01-14"),
    "month"
  )

  expect_identical(levels(out), c("2025-12", "2026-01", "2026-02"))
  expect_true(is.ordered(out))
})

test_that("a week label carries the ISO year, not the calendar one", {
  # 2027-01-01 is a Friday inside ISO week 2026-W53, which starts in December.
  out <- date_bins$bin_date_values(c("2026-12-30", "2027-01-01"), "week")

  expect_identical(as.character(out), c("2026-W53", "2026-W53"))
})

test_that("an unparseable date bins to NA rather than its own group", {
  out <- date_bins$bin_date_values(c("2026-01-14", "junk", NA), "month")

  expect_identical(is.na(out), c(FALSE, TRUE, TRUE))
  # And it does not become a level, so it cannot claim a colour.
  expect_identical(levels(out), "2026-01")
})

# --- binned_levels and granularity_label -------------------------------------

test_that("binned_levels counts the groups a granularity would produce", {
  d <- c("2026-01-14", "2026-01-28", "2026-02-03")

  expect_identical(date_bins$binned_levels(d, "day"), 3L)
  expect_identical(date_bins$binned_levels(d, "month"), 2L)
  expect_identical(date_bins$binned_levels(d, "year"), 1L)
  # Unbinned counts distinct dates, which is what the profile already reports.
  expect_identical(date_bins$binned_levels(d, NULL), 3L)
})

test_that("granularity_label names the interval, and nothing when unbinned", {
  expect_identical(date_bins$granularity_label("month"), "Month")
  expect_identical(date_bins$granularity_label("week"), "Week")
  expect_null(date_bins$granularity_label(NULL))
  expect_null(date_bins$granularity_label("none"))
})

test_that("a mapped date starts at the interval its own spread warrants", {
  # The rule every engine shares. Not "always exact" — a decade of collection
  # dates drawn exactly is one colour per isolate under a three-line key — and
  # not a fixed interval either, which would coarsen a fortnight of sampling
  # into a single bar.
  fortnight <- as.character(as.Date("2024-03-01") + 0:9)
  expect_identical(date_bins$mapped_granularity(fortnight), "day")

  # Ten weeks sampled three days apiece: thirty distinct days is past the
  # budget, ten weeks is not, so it stops at the first interval that fits.
  starts <- as.Date("2024-01-01") + seq(0, 63, by = 7)
  weeks <- as.character(sort(c(starts, starts + 1, starts + 2)))
  expect_identical(date_bins$mapped_granularity(weeks), "week")

  # Ten years of near-unique dates — the reference database's shape — comes out
  # by year rather than as 213 shades of one ramp.
  set.seed(3)
  decade <- as.character(as.Date("2011-01-01") + sample(3650, 213))
  expect_identical(date_bins$mapped_granularity(decade), "year")
  expect_true(date_bins$binned_levels(decade, "year") <= date_bins$DATE_MAX_GROUPS)

  # Nothing to group is not a date, whatever the schema says of the column.
  expect_null(date_bins$mapped_granularity(NULL))
  expect_null(date_bins$mapped_granularity(character(0)))
  expect_null(date_bins$mapped_granularity(c("not a date", NA)))

  # Past the coarsest interval it stops rather than giving up: a century of
  # sampling has more years than the budget and years is still the answer.
  century <- as.character(as.Date("1925-06-01") + seq(0, 36500, by = 365))
  expect_identical(date_bins$mapped_granularity(century), "year")
})

# --- year precision -----------------------------------------------------------

test_that("parse_dates reads each value on its own, whatever comes first", {
  out <- date_bins$parse_dates(c("junk", "2026-01-14", "2026/3/5", "2026-02-01 10:30:00"))
  expect_true(is.na(out[[1]]))
  expect_identical(out[-1], as.Date(c("2026-01-14", "2026-03-05", "2026-02-01")))
})

test_that("parse_dates anchors a bare year at 1 January, and its period ends 31 December", {
  x <- c("2021", "2026-01-14", NA)
  expect_identical(date_bins$parse_dates(x), as.Date(c("2021-01-01", "2026-01-14", NA)))
  expect_identical(date_bins$date_period_end(x), as.Date(c("2021-12-31", "2026-01-14", NA)))
  expect_identical(date_bins$year_only(x), c(TRUE, FALSE, FALSE))
})

test_that("date_precision is year only when every readable value is a bare year", {
  expect_identical(date_bins$date_precision(c("2020", "2021", NA, "junk")), "year")
  expect_identical(date_bins$date_precision(c("2020", "2021-03-01")), "day")
  expect_identical(date_bins$date_precision(character(0)), "day")
  expect_identical(
    unname(date_bins$available_granularities(c("2020", "2021"))),
    "year"
  )
  expect_length(date_bins$available_granularities(c("2020-01-01")), 4L)
})

test_that("a bare year is never placed in a finer interval than a year", {
  floored <- date_bins$floor_date_bin(c("2021", "2021-03-15"), "month")
  expect_identical(floored, as.Date(c(NA, "2021-03-01")))
  expect_identical(
    date_bins$floor_date_bin(c("2021", "2021-03-15"), "year"),
    as.Date(c("2021-01-01", "2021-01-01"))
  )
})

test_that("a year column groups by year whatever granularity is asked for", {
  out <- date_bins$bin_date_values(c("2021", "2019", "2021"), "month")
  expect_identical(levels(out), c("2019", "2021"))
  expect_identical(as.character(out), c("2021", "2019", "2021"))
  expect_identical(date_bins$mapped_granularity(c("2021", "2019")), "year")
  expect_identical(date_bins$default_granularity(as.character(1990:2020)), "year")
})

test_that("in_date_range keeps a bare year whose year overlaps the window", {
  x <- c("2021", "2021-06-15", "2022", NA)
  keep <- date_bins$in_date_range(x, as.Date(c("2021-03-01", "2021-03-31")))
  expect_identical(keep, c(TRUE, FALSE, FALSE, FALSE))
  expect_identical(
    date_bins$date_extent(x),
    as.Date(c("2021-01-01", "2022-12-31"))
  )
  expect_null(date_bins$date_extent(c(NA, "junk")))
})

# --- timeline fields --------------------------------------------------------

test_that("timeline_fields lists date and year variables, collection date first", {
  profiles <- data.frame(
    field = c("custom_visit", "sample_collection_date", "called_at", "country"),
    type = c("year", "date", "datetime", "category"),
    stringsAsFactors = FALSE
  )
  expect_identical(
    date_bins$timeline_fields(profiles),
    c("sample_collection_date", "custom_visit")
  )
  expect_identical(date_bins$timeline_fields(NULL), character(0))
})

test_that("first_dated_field skips variables holding no readable date", {
  meta <- data.frame(a = c(NA, "junk"), b = c("2021", NA), stringsAsFactors = FALSE)
  expect_identical(date_bins$first_dated_field(meta, c("a", "b")), "b")
  expect_null(date_bins$first_dated_field(meta, "a"))
  expect_null(date_bins$first_dated_field(meta, character(0)))
})
