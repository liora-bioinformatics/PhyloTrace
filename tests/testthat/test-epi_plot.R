box::use(
  testthat[
    expect_equal,
    expect_error,
    expect_false,
    expect_identical,
    expect_length,
    expect_s3_class,
    expect_true,
    test_that
  ],
)
box::use(
  app / logic / epi_plot,
)

# A small metadata frame shaped like the `metadata` table's output: ISO date
# strings (free text, so unparseable and NA values are expected).
meta_fixture <- function() {
  data.frame(
    isolate = paste0("ISO-", 1:6),
    # 2026-01-14 is a Wednesday, 2026-01-18 the Sunday of the same ISO week.
    sample_collection_date = c(
      "2026-01-14",
      "2026-01-18",
      "2026-02-03",
      "not-a-date",
      NA,
      "2026-02-04"
    ),
    organism = c("E. coli", "E. coli", "S. enterica", "E. coli", "E. coli", ""),
    geo_loc_name_country = c(
      "Germany",
      "France",
      "Germany",
      "Germany",
      "France",
      NA
    ),
    stringsAsFactors = FALSE
  )
}

# --- floor_date_bin ----------------------------------------------------------

test_that("floor_date_bin rounds down to each interval's start", {
  d <- as.Date(c("2026-01-14", "2026-02-03"))

  expect_identical(epi_plot$floor_date_bin(d, "day"), d)
  expect_identical(
    epi_plot$floor_date_bin(d, "month"),
    as.Date(c("2026-01-01", "2026-02-01"))
  )
  expect_identical(
    epi_plot$floor_date_bin(d, "year"),
    as.Date(c("2026-01-01", "2026-01-01"))
  )
})

test_that("floor_date_bin weeks start on the ISO Monday", {
  # Wednesday and the Sunday of the same ISO week collapse onto one Monday;
  # the next day (Monday) must start a new bin rather than join it.
  week <- epi_plot$floor_date_bin(
    as.Date(c("2026-01-14", "2026-01-18", "2026-01-19")),
    "week"
  )

  expect_identical(week, as.Date(c("2026-01-12", "2026-01-12", "2026-01-19")))
  expect_identical(unique(format(week, "%u")), "1")
})

test_that("floor_date_bin is NA-safe for every interval", {
  # The collection-date column is free text, so NA reaches the binner.
  for (interval in c("day", "week", "month", "year")) {
    binned <- epi_plot$floor_date_bin(as.Date(c("2026-01-14", NA)), interval)
    expect_length(binned, 2L)
    expect_false(is.na(binned[1]))
    expect_true(is.na(binned[2]))
  }
})

test_that("floor_date_bin rejects an unknown interval", {
  expect_error(epi_plot$floor_date_bin(as.Date("2026-01-14"), "fortnight"), "fortnight")
})

# --- build_epi_data ----------------------------------------------------------

test_that("build_epi_data with no stratify field gives one total series", {
  out <- epi_plot$build_epi_data(meta_fixture(), "sample_collection_date", character(), "week")

  expect_identical(unique(out$stratum), epi_plot$EPI_ALL_LABEL)
  expect_identical(out$date_bin, as.Date(c("2026-01-12", "2026-02-02")))
  expect_identical(out$count, c(2L, 2L))
})

test_that("build_epi_data reports isolates dropped for unreadable dates", {
  # One "not-a-date" plus one NA of six isolates.
  out <- epi_plot$build_epi_data(meta_fixture(), "sample_collection_date", character(), "week")

  expect_identical(attr(out, "dropped"), 2L)
  expect_identical(sum(out$count), 4L)
})

test_that("build_epi_data colours by a single field", {
  out <- epi_plot$build_epi_data(
    meta_fixture(),
    "sample_collection_date",
    "organism",
    "week"
  )

  expect_true("E. coli" %in% out$stratum)
  expect_true("S. enterica" %in% out$stratum)
  # The empty-string organism reads as unknown, not as a category named "".
  expect_true(epi_plot$EPI_UNKNOWN_LABEL %in% out$stratum)
})

test_that("build_epi_data joins two fields into a composite category", {
  out <- epi_plot$build_epi_data(
    meta_fixture(),
    "sample_collection_date",
    c("organism", "geo_loc_name_country"),
    "month"
  )

  expect_true("E. coli | Germany" %in% out$stratum)
  expect_true("E. coli | France" %in% out$stratum)
  expect_true("S. enterica | Germany" %in% out$stratum)
  # Row 6 has an empty organism and an NA country: both sides read as unknown.
  expect_true(
    paste(epi_plot$EPI_UNKNOWN_LABEL, epi_plot$EPI_UNKNOWN_LABEL, sep = " | ") %in% out$stratum
  )
})

test_that("build_epi_data ignores a stratify field the metadata lacks", {
  # A selection can outlive a database switch that dropped the column.
  out <- epi_plot$build_epi_data(
    meta_fixture(),
    "sample_collection_date",
    c("organism", "gone_missing"),
    "week"
  )

  expect_false(any(grepl("|", out$stratum, fixed = TRUE)))
})

test_that("build_epi_data returns empty for an absent date field", {
  out <- epi_plot$build_epi_data(meta_fixture(), "nope", character(), "day")

  expect_identical(nrow(out), 0L)
  expect_s3_class(out$date_bin, "Date")
})

test_that("build_epi_data defaults to the collection-date column", {
  # The date field is no longer user-pickable — every database carries exactly
  # one collection-date column, so the engine plots that and nothing else.
  out <- epi_plot$build_epi_data(meta_fixture(), stratify_by = character(), interval = "week")

  expect_identical(nrow(out), 2L)
  expect_identical(attr(out, "dropped"), 2L)
})

test_that("build_epi_data survives a date field where nothing parses", {
  # as.Date() throws rather than returning NA when no value in the column
  # parses, and the date field is user-pickable — so this must not error.
  meta <- meta_fixture()

  out <- epi_plot$build_epi_data(meta, "isolate", character(), "day")

  expect_identical(nrow(out), 0L)
  expect_identical(attr(out, "dropped"), 6L)
})

test_that("build_epi_data handles empty metadata", {
  out <- epi_plot$build_epi_data(
    meta_fixture()[0, , drop = FALSE],
    "sample_collection_date",
    character(),
    "day"
  )

  expect_identical(nrow(out), 0L)
})

# --- epi_default_interval ----------------------------------------------------

# `n` dates spread evenly between two bounds, as the free-text ISO strings the
# metadata actually stores.
date_span <- function(from, to, n = 50) {
  format(seq(as.Date(from), as.Date(to), length.out = n))
}

test_that("epi_default_interval takes the finest interval that fits", {
  # A short outbreak keeps daily resolution; a decade of surveillance would need
  # 3653 daily bars, so it steps up until the count fits.
  expect_identical(epi_plot$epi_default_interval(date_span("2026-01-01", "2026-04-30")), "day")
  expect_identical(epi_plot$epi_default_interval(date_span("2026-01-01", "2026-12-31")), "week")
  expect_identical(epi_plot$epi_default_interval(date_span("2015-01-01", "2024-12-31")), "month")
})

test_that("epi_default_interval never exceeds the bar budget", {
  # The point of the fit: whatever the span, the chosen interval draws no more
  # than EPI_MAX_BARS bars across it.
  for (span in list(
    c("2026-01-01", "2026-01-02"),
    c("2026-01-01", "2026-06-30"),
    c("2020-01-01", "2026-01-01"),
    c("1995-01-01", "2026-01-01")
  )) {
    dates <- as.Date(date_span(span[1], span[2]))
    interval <- epi_plot$epi_default_interval(format(dates))
    width <- c(day = 1, week = 7, month = 30.4, year = 365.25)[[interval]]
    bars <- ceiling(as.numeric(diff(range(dates))) / width) + 1

    expect_true(bars <= epi_plot$EPI_MAX_BARS)
  }
})

test_that("epi_default_interval fits the span, not the number of isolates", {
  # Eighty isolates over three years occupy only 80 daily bins — under any
  # count-based budget — but each bar would be 1/1100th of the axis. It's the
  # span that decides readability.
  sparse <- date_span("2015-01-01", "2017-12-31", n = 80)

  expect_identical(epi_plot$epi_default_interval(sparse), "month")
})

test_that("epi_default_interval falls back to the coarsest when nothing fits", {
  expect_identical(epi_plot$epi_default_interval(date_span("1825-01-01", "2025-01-01")), "year")
})

test_that("epi_default_interval copes with one date and with no dates", {
  expect_identical(epi_plot$epi_default_interval("2026-01-01"), "day")
  expect_identical(epi_plot$epi_default_interval(c(NA, "junk")), epi_plot$EPI_INTERVAL_FALLBACK)
  expect_identical(epi_plot$epi_default_interval(character()), epi_plot$EPI_INTERVAL_FALLBACK)
})

test_that("epi_default_interval honours a custom budget", {
  year <- date_span("2026-01-01", "2026-12-31")

  # 365 daily bars fit a generous budget but not a tight one.
  expect_identical(epi_plot$epi_default_interval(year, max_bars = 400L), "day")
  expect_identical(epi_plot$epi_default_interval(year, max_bars = 20L), "month")
})

# --- epi_scale_choices -------------------------------------------------------

test_that("epi_scale_choices only offers palettes with enough real swatches", {
  # Set2, Dark2 and Accent stop at 8 colours, so they drop out at 9 strata;
  # Set3 and Paired carry 12.
  eight <- epi_plot$epi_scale_choices(8)
  nine <- epi_plot$epi_scale_choices(9)

  expect_true("Set2" %in% eight$Qualitative)
  expect_false("Set2" %in% nine$Qualitative)
  expect_true("Set3" %in% nine$Qualitative)
})

test_that("epi_scale_choices withdraws qualitative palettes past their cap", {
  # Forty countries against a nine-swatch palette is forty interpolated
  # neighbours nobody can tell apart — offer only the continuous ramps.
  choices <- epi_plot$epi_scale_choices(40)

  expect_false("Qualitative" %in% names(choices))
  expect_true(all(c("Gradient", "Sequential") %in% names(choices)))
  # ... and default to one that is defined for any number of categories.
  expect_identical(unlist(choices, use.names = FALSE)[1], "viridis")
})

test_that("epi_scale_choices keeps qualitative first for few strata", {
  choices <- epi_plot$epi_scale_choices(3)

  expect_identical(names(choices)[1], "Qualitative")
  expect_identical(unlist(choices, use.names = FALSE)[1], "Set1")
})

test_that("epi_palette fills a gradient across many strata", {
  # The scale a high-cardinality stratification falls back to must actually
  # produce one distinct colour per stratum.
  pal <- epi_plot$epi_palette(paste0("country", 1:40), "viridis")

  expect_length(pal, 40L)
  expect_identical(anyDuplicated(pal), 0L)
})

# --- epi_palette -------------------------------------------------------------

test_that("epi_palette names one colour per category", {
  pal <- epi_plot$epi_palette(c("a", "b", "c"), "Set2")

  expect_length(pal, 3L)
  expect_identical(names(pal), c("a", "b", "c"))
  expect_true(all(grepl("^#[0-9A-Fa-f]{6}", pal)))
})

test_that("epi_palette interpolates past a brewer palette's cap", {
  # Set2 tops out at 8; 12 strata must still get 12 distinguishable colours
  # rather than a recycled palette that makes two strata look identical.
  cats <- paste0("c", 1:12)
  pal <- epi_plot$epi_palette(cats, "Set2")

  expect_length(pal, 12L)
  expect_identical(names(pal), cats)
  expect_identical(anyDuplicated(pal), 0L)
})

test_that("epi_palette handles fewer categories than brewer's minimum", {
  # brewer.pal() errors below n = 3.
  expect_length(epi_plot$epi_palette("only", "Set2"), 1L)
  expect_length(epi_plot$epi_palette(c("a", "b"), "Set2"), 2L)
})

test_that("epi_palette accepts a viridis scale and falls back for unknowns", {
  expect_length(epi_plot$epi_palette(c("a", "b"), "viridis"), 2L)
  expect_length(epi_plot$epi_palette(c("a", "b"), "not-a-palette"), 2L)
})

test_that("epi_palette handles no categories", {
  expect_length(epi_plot$epi_palette(character(), "Set2"), 0L)
})

# --- epi_legend_ncol ---------------------------------------------------------

test_that("epi_legend_ncol narrows the legend so long labels fit the width", {
  # The failure this guards against: a fixed ncol overflows the panel and the
  # legend is clipped. Full institution names two-per-column already fill a
  # typical panel, so eleven of them must wrap to well under the old fixed six.
  long <- c(
    "Broad Institute of MIT and Harvard",
    "US Centers for Disease Control and Prevention",
    "Wellcome Centre for Human Genetics"
  )
  wide <- epi_plot$epi_legend_ncol(long, width_px = 1180)
  expect_true(wide >= 1L && wide < 6L)
  # A narrower panel takes fewer columns still.
  expect_true(epi_plot$epi_legend_ncol(long, width_px = 400) <= wide)
})

test_that("epi_legend_ncol never asks for more columns than categories", {
  expect_identical(epi_plot$epi_legend_ncol(c("A", "B", "C"), 4000), 3L)
  expect_identical(epi_plot$epi_legend_ncol("All isolates", 1180), 1L)
})

test_that("epi_legend_ncol falls back for an unknown width", {
  short <- LETTERS[1:8]
  # NULL / non-finite widths must still yield a usable positive column count.
  expect_gte(epi_plot$epi_legend_ncol(short, NULL), 1L)
  expect_gte(epi_plot$epi_legend_ncol(short, NA_real_), 1L)
})

test_that("the fill legend carries the adaptive column count", {
  binned <- data.frame(
    date_bin = as.Date(rep(c("2023-01-02", "2023-01-09"), each = 2)),
    stratum = rep(
      c("US Centers for Disease Control and Prevention", "EMBL-EBI"),
      2
    ),
    count = 1L,
    stringsAsFactors = FALSE
  )
  p <- epi_plot$build_epi_ggplot(
    binned,
    list(mode = "stacked", interval = "week", plot_width = 500)
  )
  expect_identical(
    p$guides$guides$fill$params$ncol,
    epi_plot$epi_legend_ncol(c("EMBL-EBI", "US Centers for Disease Control and Prevention"), 500)
  )
})

# --- epi_annotation_layout ---------------------------------------------------

annos_fixture <- function() {
  data.frame(
    id = c("a", "b", "c"),
    type = c("milestone", "period", "milestone"),
    start = as.Date(c("2026-01-20", "2026-02-10", "2026-01-22")),
    end = as.Date(c(NA, "2026-03-01", NA)),
    label = c("Intervention", "Outbreak window", "Second event"),
    stringsAsFactors = FALSE
  )
}

test_that("epi_annotation_lanes is empty for no annotations", {
  expect_identical(nrow(epi_plot$epi_annotation_lanes(epi_plot$empty_epi_annotations())), 0L)
})

test_that("epi_annotation_lanes assigns every annotation a lane", {
  lanes <- epi_plot$epi_annotation_lanes(annos_fixture())

  expect_identical(nrow(lanes), 3L)
  # Only the row with an end date is a shaded period; the rest are milestones.
  expect_identical(sum(lanes$period), 1L)
  expect_true(all(lanes$lane >= 1L))
})

test_that("identical periods never share a lane", {
  # Two periods covering exactly the same span cannot be told apart by their
  # shading, so their brackets and labels must be drawn at different heights.
  annos <- data.frame(
    id = c("a", "b"),
    type = c("period", "period"),
    start = as.Date(c("2022-07-17", "2022-07-17")),
    end = as.Date(c("2025-07-18", "2025-07-18")),
    label = c("dd", "cddd"),
    stringsAsFactors = FALSE
  )
  lanes <- epi_plot$epi_annotation_lanes(annos)$lane

  expect_false(lanes[1] == lanes[2])
})

test_that("partly intersecting periods get separate lanes", {
  annos <- data.frame(
    id = c("a", "b"),
    type = c("period", "period"),
    start = as.Date(c("2022-01-01", "2023-01-01")),
    end = as.Date(c("2024-01-01", "2025-01-01")),
    label = c("first", "second"),
    stringsAsFactors = FALSE
  )
  lanes <- epi_plot$epi_annotation_lanes(annos)$lane

  expect_false(lanes[1] == lanes[2])
})

test_that("disjoint periods share the top lane", {
  # Nothing overlaps, so there is no reason to push either one down.
  annos <- data.frame(
    id = c("a", "b"),
    type = c("period", "period"),
    start = as.Date(c("2016-01-01", "2023-01-01")),
    end = as.Date(c("2017-01-01", "2024-01-01")),
    label = c("first", "second"),
    stringsAsFactors = FALSE
  )
  lanes <- epi_plot$epi_annotation_lanes(
    annos,
    as.Date(c("2015-01-01", "2026-01-01"))
  )$lane

  expect_identical(lanes[1], lanes[2])
})

test_that("label clearance scales with the plotted span, not a fixed gap", {
  # Two milestones nine days apart are indistinguishable on a plot spanning ten
  # years but comfortably apart on one spanning three months. A fixed
  # day-count rule got the wide case wrong and overlapped the labels.
  annos <- data.frame(
    id = c("x", "y"),
    type = c("milestone", "milestone"),
    start = as.Date(c("2020-01-01", "2020-01-10")),
    end = as.Date(c(NA, NA)),
    label = c("A", "B"),
    stringsAsFactors = FALSE
  )
  lanes_for <- function(range) {
    epi_plot$epi_annotation_lanes(annos, as.Date(range))$lane
  }

  wide <- lanes_for(c("2015-01-01", "2026-01-01"))
  narrow <- lanes_for(c("2019-12-01", "2020-03-01"))

  expect_false(wide[1] == wide[2])
  expect_identical(narrow[1], narrow[2])
})

test_that("epi_annotation_lanes treats a period without an end as a milestone", {
  annos <- epi_plot$empty_epi_annotations()
  annos <- rbind(annos, data.frame(
    id = "a",
    type = "period",
    start = as.Date("2026-01-20"),
    end = as.Date(NA),
    label = "No end",
    stringsAsFactors = FALSE
  ))

  expect_false(epi_plot$epi_annotation_lanes(annos)$period[1])
})

test_that("the x axis widens to take in annotations outside the data", {
  # Otherwise an annotation dated past the last isolate is silently clipped —
  # you add a milestone and simply never see it.
  binned <- epi_plot$build_epi_data(meta_fixture(), interval = "week")
  far <- data.frame(
    id = "a",
    type = "milestone",
    start = as.Date("2026-06-01"),
    end = as.Date(NA),
    label = "Well after the data",
    stringsAsFactors = FALSE
  )
  x_max <- function(annos) {
    max(ggplot2::ggplot_build(
      epi_plot$build_epi_ggplot(binned, list(interval = "week", annos = annos))
    )$layout$panel_params[[1]]$x.range)
  }

  expect_true(x_max(far) >= as.numeric(as.Date("2026-06-01")))
  expect_true(x_max(far) > x_max(epi_plot$empty_epi_annotations()))
})

test_that("annotations are drawn onto the curve", {
  binned <- epi_plot$build_epi_data(meta_fixture(), interval = "week")
  n_layers <- function(annos) {
    length(ggplot2::ggplot_build(
      epi_plot$build_epi_ggplot(binned, list(interval = "week", annos = annos))
    )$plot$layers)
  }

  # A period adds a wash, a bracket and a label; a milestone a line and a label.
  expect_true(n_layers(annos_fixture()) > n_layers(epi_plot$empty_epi_annotations()))
})

# --- annotations lanes -------------------------------------------------------


# --- epi_cumulate ------------------------------------------------------------

test_that("epi_cumulate runs a monotone total per stratum", {
  binned <- epi_plot$build_epi_data(
    meta_fixture(),
    stratify_by = "organism",
    interval = "week"
  )
  cu <- epi_plot$epi_cumulate(binned)

  by_stratum <- split(cu$count, cu$stratum)
  expect_true(all(vapply(by_stratum, function(x) all(diff(x) >= 0), logical(1))))
  # Every stratum is evaluated at every bin, so the lines stay comparable.
  expect_identical(
    nrow(cu),
    length(unique(binned$date_bin)) * length(unique(binned$stratum))
  )
  # The running totals end at the true per-stratum totals.
  expect_equal(
    sum(vapply(by_stratum, max, numeric(1))),
    sum(binned$count)
  )
})

test_that("epi_cumulate handles empty data", {
  expect_identical(nrow(epi_plot$epi_cumulate(epi_plot$build_epi_data(meta_fixture(), "nope"))), 0L)
})

# --- build_epi_ggplot --------------------------------------------------------

# The layer classes a mode draws with, as ggplot resolves them.
built_geoms <- function(binned, opts = list()) {
  vapply(
    ggplot2::ggplot_build(epi_plot$build_epi_ggplot(binned, opts))$plot$layers,
    function(l) class(l$geom)[1],
    character(1)
  )
}

test_that("each style draws with the geom that suits it", {
  binned <- epi_plot$build_epi_data(
    meta_fixture(),
    stratify_by = "organism",
    interval = "week"
  )

  expect_true("GeomCol" %in% built_geoms(binned, list(mode = "stacked")))
  # One tile per case, so the column can be counted off the plot.
  expect_true("GeomTile" %in% built_geoms(binned, list(mode = "stacked", square = TRUE)))
  # A step, not an interpolation: nothing happens between two bins, so a sloped
  # line would draw case counts that never existed.
  expect_true("GeomStep" %in% built_geoms(binned, list(mode = "cumulative")))
})

test_that("every style renders to a real image", {
  binned <- epi_plot$build_epi_data(
    meta_fixture(),
    stratify_by = "organism",
    interval = "week"
  )

  # The on-screen path. File export across every offered format is
  # save_plot_export()'s job — see test-viz_export.R.
  for (mode in c("stacked", "cumulative")) {
    file <- tempfile(fileext = ".png")
    epi_plot$render_epi_png(
      epi_plot$build_epi_ggplot(binned, list(mode = mode, interval = "week")),
      file,
      width_px = 800,
      height_px = 440
    )
    expect_true(file.exists(file))
    expect_true(file.size(file) > 0)
  }
})

test_that("build_epi_ggplot handles empty data and absent options", {
  empty <- epi_plot$build_epi_data(meta_fixture(), "nope")

  expect_s3_class(epi_plot$build_epi_ggplot(empty, list()), "ggplot")
})

test_that("unit blocks draw one cell per case", {
  binned <- epi_plot$build_epi_data(meta_fixture(), interval = "week")
  tiles <- ggplot2::ggplot_build(
    epi_plot$build_epi_ggplot(binned, list(mode = "stacked", interval = "week", square = TRUE))
  )$data[[1]]

  # Four datable isolates across two bins of two.
  expect_identical(nrow(tiles), 4L)
  # Cells are interval-wide so the columns touch, and one case tall.
  expect_true(all(tiles$ymax - tiles$ymin == 1))
})

test_that("the case axis only ever carries integer breaks", {
  # Cases are whole things: a "0.6 isolates" break is meaningless, and a
  # tallest-bar-of-1 curve used to get ticked 0, 0.2, 0.4 ...
  one_per_bin <- data.frame(
    sample_collection_date = format(as.Date("2015-01-01") + (0:9) * 180),
    stringsAsFactors = FALSE
  )
  binned <- epi_plot$build_epi_data(one_per_bin, interval = "day")
  breaks <- ggplot2::ggplot_build(
    epi_plot$build_epi_ggplot(binned, list(interval = "day"))
  )$layout$panel_params[[1]]$y$breaks
  breaks <- breaks[!is.na(breaks)]

  expect_identical(max(binned$count), 1L)
  expect_identical(breaks, c(0, 1))
})

test_that("square is ignored for the cumulative curve", {
  # A step line has no cells to square off, so the option can't apply — and
  # coord_fixed would otherwise squash the curve for no reason.
  binned <- epi_plot$build_epi_data(meta_fixture(), interval = "week")
  coord <- ggplot2::ggplot_build(epi_plot$build_epi_ggplot(
    binned,
    list(mode = "cumulative", square = TRUE, interval = "week")
  ))$layout$coord

  expect_true(is.null(coord$ratio))
})

test_that("square_panel_ratio gives the shape the squares force", {
  # Square cells mean panel_height/y_max == panel_width/n_bins, so the data
  # fixes the panel's shape. The view sizes the plot from this instead of the
  # aspect slider — honouring the slider letterboxes the curve into a strip and
  # shrinks the squares to nothing.
  binned <- epi_plot$build_epi_data(meta_fixture(), interval = "week")

  # Two bins, tallest column two cases.
  expect_equal(epi_plot$square_panel_ratio(binned), 1)

  # A long, low dataset is legitimately a long, low plot.
  wide <- epi_plot$build_epi_data(
    data.frame(
      sample_collection_date = format(as.Date("2020-01-01") + (0:99) * 7),
      stringsAsFactors = FALSE
    ),
    interval = "week"
  )
  expect_true(epi_plot$square_panel_ratio(wide) < 0.05)

  # Doesn't apply where squares don't.
  expect_true(is.null(epi_plot$square_panel_ratio(binned, "cumulative")))
  expect_true(is.null(epi_plot$square_panel_ratio(epi_plot$build_epi_data(meta_fixture(), "nope"))))
})

test_that("square blocks lock one case to one interval", {
  # coord_fixed's ratio is in data units and a Date axis counts days, so the
  # ratio is the bin width. (plotly's equivalent, scaleanchor, silently did
  # nothing against a date axis — which is why the square style never worked
  # there.)
  binned <- epi_plot$build_epi_data(meta_fixture(), interval = "week")
  coord <- function(square) {
    ggplot2::ggplot_build(epi_plot$build_epi_ggplot(
      binned,
      list(mode = "stacked", interval = "week", square = square)
    ))$layout$coord
  }

  expect_equal(coord(TRUE)$ratio, 7)
  # Off by default: it hands the plot's aspect to the data, so the curve stops
  # filling the panel.
  expect_true(is.null(coord(FALSE)$ratio))
})

# --- line-end labels ---------------------------------------------------------

test_that("label_ends only applies to the cumulative curve", {
  binned <- epi_plot$build_epi_data(
    meta_fixture(),
    stratify_by = "organism",
    interval = "week"
  )
  n_text <- function(mode) {
    p <- epi_plot$build_epi_ggplot(binned, list(mode = mode, label_ends = TRUE))
    sum(vapply(p$layers, function(l) inherits(l$geom, "GeomText"), logical(1)))
  }

  # A bar chart has no line to label the end of.
  expect_identical(n_text("stacked"), 0L)
  expect_identical(n_text("cumulative"), 1L)
})

test_that("label_ends suppresses the legend, which would just repeat it", {
  binned <- epi_plot$build_epi_data(
    meta_fixture(),
    stratify_by = "organism",
    interval = "week"
  )
  guide <- function(label_ends) {
    p <- epi_plot$build_epi_ggplot(
      binned,
      list(mode = "cumulative", label_ends = label_ends)
    )
    p$guides$guides$fill
  }

  expect_identical(guide(TRUE), "none")
  expect_false(identical(guide(FALSE), "none"))
})

test_that("label_ends suppresses the cumulative curve's colour legend too", {
  # The cumulative curve maps its strata to `colour` (geom_step), not `fill`, so
  # silencing only the fill guide left a full stratum legend rendering under a
  # "Label lines" plot — the legend that letterboxed the curve into a band of
  # empty space. The colour guide has to be suppressed on the same condition.
  binned <- epi_plot$build_epi_data(
    meta_fixture(),
    stratify_by = "organism",
    interval = "week"
  )
  colour_guide <- function(label_ends) {
    p <- epi_plot$build_epi_ggplot(
      binned,
      list(mode = "cumulative", label_ends = label_ends)
    )
    p$guides$guides$colour
  }

  expect_identical(colour_guide(TRUE), "none")
  expect_false(identical(colour_guide(FALSE), "none"))
})

test_that("label_ends does not balloon the y axis below zero with many strata", {
  # Many strata clustered at low cumulative counts push the greedy end-label
  # stacking hundreds of units below 0. Reserving that whole distance as axis
  # expansion used to open a huge empty band under the curve; the reservation
  # is now capped at one label's worth of height. The panel floor must stay
  # close to 0 rather than plunging to the lowest stacked label.
  set.seed(1)
  countries <- paste("Country", sprintf("%02d", 1:40))
  meta <- data.frame(
    isolate = paste0("ISO-", 1:400),
    sample_collection_date = as.character(
      as.Date("2015-01-01") + sample(0:3650, 400, TRUE)
    ),
    country = sample(countries, 400, TRUE),
    stringsAsFactors = FALSE
  )
  binned <- epi_plot$build_epi_data(
    meta,
    stratify_by = "country",
    interval = "month"
  )
  p <- epi_plot$build_epi_ggplot(
    binned,
    list(mode = "cumulative", label_ends = TRUE, interval = "month")
  )
  y_max <- max(epi_plot$epi_cumulate(binned)$count)
  y_range <- suppressWarnings(
    ggplot2::ggplot_build(p)$layout$panel_params[[1]]$y.range
  )
  # The floor must be a small fraction below 0 (one label height), never a
  # multiple of the data's own extent.
  expect_gt(y_range[1], -y_max * 0.2)
  expect_lt(y_range[1], 0)
})

test_that("a windowed curve keeps a legend key for every stratum in the data", {
  # Reveal windowing drops the bins outside the span, so strata that only occur
  # elsewhere vanish from the drawn layer. ggplot then renders their legend keys
  # blank. An invisible seed layer carrying every stratum keeps the legend
  # complete and coloured — the same fixed-scale behaviour the Map offers.
  binned <- data.frame(
    date_bin = as.Date(c(
      "2020-01-01", "2020-01-01",
      "2020-06-01", "2020-06-01"
    )),
    stratum = c("early-A", "early-B", "late-C", "late-D"),
    count = 1L,
    stringsAsFactors = FALSE
  )
  cats <- sort(unique(binned$stratum))
  seeds <- function(mode) {
    p <- epi_plot$build_epi_ggplot(
      binned,
      list(
        mode = mode,
        interval = "month",
        # Window to the first bin only: "late-C"/"late-D" fall outside it.
        reveal_from = as.Date("2020-01-01"),
        reveal_to = as.Date("2020-01-01")
      )
    )
    # A layer whose data carries every stratum at a zero value is the seed that
    # keeps the out-of-window keys coloured.
    vapply(
      p$layers,
      function(l) {
        d <- l$data
        is.data.frame(d) &&
          all(c("stratum", "count") %in% names(d)) &&
          all(cats %in% d$stratum) &&
          all(d$count == 0)
      },
      logical(1)
    )
  }
  expect_true(any(seeds("stacked")))
  expect_true(any(seeds("cumulative")))
})

test_that("label_ends widens the x axis to leave room for the text", {
  binned <- epi_plot$build_epi_data(
    meta_fixture(),
    stratify_by = "organism",
    interval = "week"
  )
  x_max <- function(label_ends) {
    max(ggplot2::ggplot_build(epi_plot$build_epi_ggplot(
      binned,
      list(mode = "cumulative", label_ends = label_ends, interval = "week")
    ))$layout$panel_params[[1]]$x.range)
  }

  expect_true(x_max(TRUE) > x_max(FALSE))
})

# --- cumulative overlay -------------------------------------------------------

test_that("show_cumulative overlays a step line on the stacked and square styles", {
  binned <- epi_plot$build_epi_data(meta_fixture(), interval = "week")
  n_steps <- function(opts) {
    p <- epi_plot$build_epi_ggplot(binned, opts)
    sum(vapply(p$layers, function(l) inherits(l$geom, "GeomStep"), logical(1)))
  }

  expect_identical(n_steps(list(mode = "stacked", interval = "week")), 0L)
  # A background-coloured halo pass plus the dashed line drawn over it — see
  # the comment in build_epi_ggplot on why the line needs the halo to stay
  # legible over the bars.
  expect_identical(
    n_steps(list(mode = "stacked", show_cumulative = TRUE, interval = "week")),
    2L
  )
  expect_identical(
    n_steps(list(mode = "stacked", square = TRUE, show_cumulative = TRUE, interval = "week")),
    2L
  )
  # Cumulative already draws the running total as its own main curve (also a
  # GeomStep) — the overlay must not add to it.
  expect_identical(
    n_steps(list(mode = "cumulative", show_cumulative = TRUE, interval = "week")),
    1L
  )
})

test_that("the cumulative overlay is scaled to reach the panel's top at its final value", {
  # cum_scale is picked so the running total's last (highest) point lands
  # exactly on the tallest bar, however far the true grand total actually
  # climbs — that's what lets it share the stacked curve's axis at all.
  binned <- epi_plot$build_epi_data(
    meta_fixture(),
    stratify_by = "organism",
    interval = "week"
  )
  built <- ggplot2::ggplot_build(
    epi_plot$build_epi_ggplot(
      binned,
      list(mode = "stacked", show_cumulative = TRUE, interval = "week")
    )
  )
  step_layer <- which(vapply(
    built$plot$layers,
    function(l) inherits(l$geom, "GeomStep"),
    logical(1)
  ))
  overlay_y <- built$data[[step_layer]]$y

  totals <- tapply(binned$count, binned$date_bin, sum)
  expect_equal(max(overlay_y), max(totals))
})

# --- moving average ----------------------------------------------------------

test_that("epi_moving_average fills the gaps and smooths across strata", {
  binned <- epi_plot$build_epi_data(
    meta_fixture(),
    stratify_by = "organism",
    interval = "week"
  )
  # A window of 1 is just the per-interval totals, so nothing is lost or added —
  # but the empty weeks between the two occupied ones are filled in, so the
  # frame is wider than the two occupied bins.
  raw <- epi_plot$epi_moving_average(binned, window = 1, interval = "week")

  expect_true(all(c("date_bin", "avg") %in% names(raw)))
  expect_true(nrow(raw) > 2L)
  # Four datable isolates spread over the (now gap-filled) weeks.
  expect_equal(sum(raw$avg), 4)
})

test_that("epi_moving_average counts empty intervals as zero", {
  # A quiet stretch has to pull the average down, not be skipped over: a
  # trailing three-week mean landing on a lone final case, with the two weeks
  # before it empty, is 1/3 — not 1.
  meta <- data.frame(
    sample_collection_date = c("2026-01-05", "2026-02-02"),
    stringsAsFactors = FALSE
  )
  binned <- epi_plot$build_epi_data(meta, interval = "week")
  ma <- epi_plot$epi_moving_average(
    binned,
    window = 3,
    interval = "week",
    align = "trailing"
  )

  # Five weekly slots: the three empty ones between the two cases are filled.
  expect_identical(nrow(ma), 5L)
  expect_equal(ma$avg[ma$date_bin == as.Date("2026-02-02")], 1 / 3)
})

test_that("epi_moving_average aligns the window centre or trailing", {
  # Three weeks, a spike of five in the middle flanked by one either side. A
  # centred window straddles both neighbours; a trailing one looks only back.
  meta <- data.frame(
    sample_collection_date = c(
      "2026-01-05",
      rep("2026-01-12", 5),
      "2026-01-19"
    ),
    stringsAsFactors = FALSE
  )
  binned <- epi_plot$build_epi_data(meta, interval = "week")
  peak <- as.Date("2026-01-12")
  centred <- epi_plot$epi_moving_average(binned, 3, "week", "center")
  trailing <- epi_plot$epi_moving_average(binned, 3, "week", "trailing")

  # Centred at the peak averages both neighbours: (1 + 5 + 1) / 3.
  expect_equal(centred$avg[centred$date_bin == peak], 7 / 3)
  # Trailing at the peak sees only it and the (partial) week before: (1 + 5) / 2.
  expect_equal(trailing$avg[trailing$date_bin == peak], 3)
})

test_that("epi_moving_average handles empty data", {
  expect_identical(
    nrow(epi_plot$epi_moving_average(epi_plot$build_epi_data(meta_fixture(), "nope"))),
    0L
  )
})

test_that("show_moving_avg overlays a trend line on the bar styles only", {
  binned <- epi_plot$build_epi_data(meta_fixture(), interval = "week")
  # GeomStep is a GeomPath but not a GeomLine, so this counts only the moving
  # average's own lines, never the cumulative curve's steps.
  n_lines <- function(opts) {
    p <- epi_plot$build_epi_ggplot(binned, opts)
    sum(vapply(p$layers, function(l) inherits(l$geom, "GeomLine"), logical(1)))
  }

  expect_identical(n_lines(list(mode = "stacked", interval = "week")), 0L)
  # A background-coloured halo pass plus the line drawn over it — the same halo
  # the cumulative overlay uses to stay legible over the bars.
  expect_identical(
    n_lines(list(mode = "stacked", show_moving_avg = TRUE, interval = "week")),
    2L
  )
  expect_identical(
    n_lines(list(
      mode = "stacked",
      square = TRUE,
      show_moving_avg = TRUE,
      interval = "week"
    )),
    2L
  )
  # Cumulative is a running total, with no per-interval counts to smooth.
  expect_identical(
    n_lines(list(mode = "cumulative", show_moving_avg = TRUE, interval = "week")),
    0L
  )
})

test_that("the moving-average line rides the primary case-count axis", {
  # Unlike the cumulative overlay it needs no secondary axis: a mean of counts
  # is a count. Its drawn y values must fall within the count axis, not be
  # rescaled like the cumulative line is.
  binned <- epi_plot$build_epi_data(meta_fixture(), interval = "week")
  built <- ggplot2::ggplot_build(
    epi_plot$build_epi_ggplot(
      binned,
      list(mode = "stacked", show_moving_avg = TRUE, interval = "week")
    )
  )
  line_layer <- which(vapply(
    built$plot$layers,
    function(l) inherits(l$geom, "GeomLine"),
    logical(1)
  ))[1]
  y_max <- max(tapply(binned$count, binned$date_bin, sum))

  expect_true(all(built$data[[line_layer]]$y <= y_max))
})

test_that("epi_interval_noun names the interval, pluralised for a count", {
  expect_identical(epi_plot$epi_interval_noun("week"), "week")
  expect_identical(epi_plot$epi_interval_noun("week", 7), "weeks")
  expect_identical(epi_plot$epi_interval_noun("month", 3), "months")
  expect_identical(epi_plot$epi_interval_noun("day", 1), "day")
})

test_that("epi_moving_avg_label reads the span in the curve's own unit", {
  expect_identical(
    epi_plot$epi_moving_avg_label(7, "week"),
    "7-week moving average"
  )
  expect_identical(
    epi_plot$epi_moving_avg_label(3, "month"),
    "3-month moving average"
  )
})

test_that("the moving average earns its own legend entry", {
  # A linetype scale, separate from the strata colour/fill legend, keyed by the
  # span-and-unit label — so the reader sees a line labelled "3-week moving
  # average" rather than an unexplained curve over the bars.
  binned <- epi_plot$build_epi_data(meta_fixture(), interval = "week")
  p <- epi_plot$build_epi_ggplot(
    binned,
    list(
      mode = "stacked",
      show_moving_avg = TRUE,
      moving_avg_window = 3,
      interval = "week"
    )
  )
  lt <- p$scales$get_scales("linetype")
  expect_false(is.null(lt))
  # A manual scale's labels are only known once it has seen the mapped value;
  # train it with the label the line is mapped to, then read it back.
  label <- epi_plot$epi_moving_avg_label(3, "week")
  lt$train(label)
  expect_identical(lt$get_labels(), label)
})

# --- playback ----------------------------------------------------------------

test_that("reveal_to draws only the bins up to it", {
  binned <- epi_plot$build_epi_data(meta_fixture(), interval = "week")
  bins <- epi_plot$epi_bins(binned)
  bars <- function(to) {
    nrow(ggplot2::ggplot_build(
      epi_plot$build_epi_ggplot(binned, list(interval = "week", reveal_to = to))
    )$data[[1]])
  }

  expect_length(bins, 2L)
  expect_identical(bars(bins[1]), 1L)
  expect_identical(bars(bins[2]), 2L)
  # NULL means the whole curve.
  expect_identical(bars(NULL), 2L)
})

test_that("reveal_from draws only the bins from it onwards", {
  # This is what the Time tab's date-range slider windows the curve with —
  # reveal_to alone (the old single-slider playback) never needed a lower
  # bound, but a genuine start/end window does.
  many <- data.frame(
    sample_collection_date = format(seq(as.Date("2020-01-01"), by = "month", length.out = 6)),
    stringsAsFactors = FALSE
  )
  binned <- epi_plot$build_epi_data(many, interval = "month")
  bins <- epi_plot$epi_bins(binned)
  bars <- function(from, to = NULL) {
    nrow(ggplot2::ggplot_build(
      epi_plot$build_epi_ggplot(
        binned,
        list(interval = "month", reveal_from = from, reveal_to = to)
      )
    )$data[[1]])
  }

  expect_length(bins, 6L)
  # From the second-to-last bin, unbounded above: the last two bins.
  expect_identical(bars(bins[5]), 2L)
  # Windowed on both sides: exactly the bins in between.
  expect_identical(bars(bins[3], bins[4]), 2L)
  # NULL means unbounded on that side.
  expect_identical(bars(NULL, NULL), 6L)
})

test_that("playback keeps one fixed frame", {
  # The scales come from the whole dataset, before any reveal — otherwise the
  # axes would crawl under the curve as it grows instead of it growing into
  # place.
  binned <- epi_plot$build_epi_data(meta_fixture(), interval = "week")
  bins <- epi_plot$epi_bins(binned)
  ranges <- function(to) {
    pp <- ggplot2::ggplot_build(
      epi_plot$build_epi_ggplot(binned, list(interval = "week", reveal_to = to))
    )$layout$panel_params[[1]]
    list(x = pp$x.range, y = pp$y.range)
  }

  expect_identical(ranges(bins[1]), ranges(NULL))
})

test_that("fixed_axis defaults to fixed when the caller never mentions it", {
  # isTRUE(NULL) is FALSE, so a naive `if (isTRUE(opts$fixed_axis))` would treat
  # every caller that predates the Time tab's zoom switch — including every
  # other test in this file — as having asked to zoom instead of leaving them
  # on the documented default.
  binned <- epi_plot$build_epi_data(meta_fixture(), interval = "week")
  bins <- epi_plot$epi_bins(binned)
  x_range <- function(opts) {
    ggplot2::ggplot_build(
      epi_plot$build_epi_ggplot(binned, opts)
    )$layout$panel_params[[1]]$x.range
  }

  unset <- x_range(list(interval = "week", reveal_to = bins[1]))
  explicit_fixed <- x_range(list(interval = "week", reveal_to = bins[1], fixed_axis = TRUE))
  full <- x_range(list(interval = "week"))

  expect_identical(unset, explicit_fixed)
  expect_identical(unset, full)
})

test_that("fixed_axis = FALSE zooms to the windowed span instead", {
  binned <- epi_plot$build_epi_data(meta_fixture(), interval = "week")
  bins <- epi_plot$epi_bins(binned)
  x_range <- function(fixed_axis) {
    ggplot2::ggplot_build(
      epi_plot$build_epi_ggplot(
        binned,
        list(interval = "week", reveal_to = bins[1], fixed_axis = fixed_axis)
      )
    )$layout$panel_params[[1]]$x.range
  }

  # Zoomed to just the first bin's window is a narrower axis than the fixed
  # (whole-dataset) frame.
  expect_true(diff(x_range(FALSE)) < diff(x_range(TRUE)))
})

test_that("epi_bins lists the bins in order", {
  binned <- epi_plot$build_epi_data(meta_fixture(), interval = "week")

  expect_identical(epi_plot$epi_bins(binned), as.Date(c("2026-01-12", "2026-02-02")))
  expect_length(epi_plot$epi_bins(epi_plot$build_epi_data(meta_fixture(), "nope")), 0L)
})
