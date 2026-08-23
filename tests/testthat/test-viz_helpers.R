box::use(
  shiny[MockShinySession],
  testthat[
    expect_equal,
    expect_false,
    expect_match,
    expect_no_error,
    expect_true,
    test_that
  ],
)
box::use(
  app / logic / field_profile[field_profiles],
  app / logic / viz_helpers,
)

impl <- attr(viz_helpers, "namespace")

meta <- function() {
  data.frame(
    isolate = sprintf("ISO-%02d", 1:8),
    organism = rep("P. aeruginosa", 8),
    purpose = rep(c("outbreak", "surveillance"), 4),
    stringsAsFactors = FALSE
  )
}

# These call shinyWidgets for real rather than through an engine's observer.
# That matters: an error raised inside shiny$observe() is logged and swallowed,
# so a testServer assertion about the resulting state stays green while the
# control it was meant to fill never gets populated. That is exactly how an
# inverted argument order reached a running app — updateVirtualSelect() takes
# `inputId` first and `session` last, the opposite of updatePickerInput(), so
# passing the session positionally sends it in as the id.

test_that("a field picker builds from a profile frame", {
  expect_no_error(
    viz_helpers$field_select(
      function(x) x,
      "some_id",
      "Variable",
      profiles = field_profiles(meta())
    )
  )
})

test_that("a field picker builds with a sentinel entry", {
  expect_no_error(
    viz_helpers$field_select(
      function(x) x,
      "some_id",
      "Stratify by",
      profiles = field_profiles(meta()),
      extra = c(`No stratification` = "")
    )
  )
})

test_that("the sentinel entry is not a group of its own", {
  # virtual-select draws a title row for every group it is handed, so carrying
  # the sentinel as a one-entry group put a blank, unselectable line above
  # "No annotation". It belongs at the top level, ungrouped.
  choices <- impl$.field_choices(
    field_profiles(meta()),
    extra = c(`No annotation` = "__none__")
  )
  first <- choices$choices[[1]]
  expect_equal(first$value, "__none__")
  expect_false("options" %in% names(first))
  groups <- vapply(
    choices$choices,
    function(x) if (is.null(x$options)) "" else x$label,
    character(1)
  )
  expect_false(any(!nzchar(trimws(groups[nzchar(groups)]))))
  expect_true("Sample metadata" %in% groups)
})

test_that("every field lands in the group its profile names", {
  choices <- impl$.field_choices(field_profiles(meta()))
  labelled <- vapply(choices$choices, function(x) x$label, character(1))
  expect_equal(labelled, "Sample metadata")
  values <- vapply(choices$choices[[1]]$options, function(o) o$value, character(1))
  expect_true(all(c("organism", "purpose", "isolate") %in% values))
})

# The same frame with an AMR screen on it: two drug classes, three genes.
amr_meta <- function() {
  meta <- meta()
  meta$`amr_Beta-lactam` <- rep(c("blaOXA-2", "Absent"), 4)
  meta$amr_Metal <- rep(c("merA", "merA, merD"), 4)
  meta$amr_g1 <- rep(c("Match", "Absent"), 4)
  meta$amr_g2 <- rep(c("Absent", "Match"), 4)
  meta$amr_g3 <- rep(c("Match", "Absent"), 4)
  attr(meta, "amr_cols") <- c(
    "amr_Beta-lactam", "amr_Metal", "amr_g1", "amr_g2", "amr_g3"
  )
  attr(meta, "amr_class_sections") <- c(
    `amr_Beta-lactam` = "Resistance",
    amr_Metal = "Virulence / stress"
  )
  attr(meta, "amr_gene_labels") <- c(
    amr_g1 = "blaOXA-2", amr_g2 = "blaPDC-5", amr_g3 = "merD"
  )
  attr(meta, "amr_gene_groups") <- c(
    amr_g1 = "Beta-lactam", amr_g2 = "Beta-lactam", amr_g3 = "Metal"
  )
  attr(meta, "amr_gene_sections") <- c(
    amr_g1 = "Resistance",
    amr_g2 = "Resistance",
    amr_g3 = "Virulence / stress"
  )
  meta
}

.headings <- function(choices) {
  vapply(
    choices$choices,
    function(x) if (is.null(x$options)) "" else x$label,
    character(1)
  )
}

test_that("an AMR screen is filed one heading per drug class", {
  # A screen of any size is dozens of columns, and under one "AMR screening"
  # heading they are a wall of names with nothing saying which gene belongs to
  # which class.
  meta <- amr_meta()
  choices <- impl$.field_choices(
    field_profiles(meta, amr_cols = attr(meta, "amr_cols"))
  )
  headings <- .headings(choices)
  expect_true("AMR \u00b7 Beta-lactam" %in% headings)
  expect_true("Virulence / stress \u00b7 Metal" %in% headings)
  # Not the flat group it would otherwise be filed under.
  expect_false("AMR screening" %in% headings)
  # Everything else keeps its single heading.
  expect_true("Sample metadata" %in% headings)

  block <- choices$choices[[which(headings == "AMR \u00b7 Beta-lactam")]]
  labels <- vapply(block$options, function(o) o$label, character(1))
  values <- vapply(block$options, function(o) o$value, character(1))
  # The whole class first, then the genes in it — so the choice between mapping
  # a class and mapping one determinant is made in one place.
  expect_equal(labels, c("Beta-lactam genes", "blaOXA-2", "blaPDC-5"))
  expect_equal(values, c("amr_Beta-lactam", "amr_g1", "amr_g2"))
  # The sub-text says which of the two a row is, since both are "Category".
  descriptions <- vapply(block$options, function(o) o$description, character(1))
  expect_match(descriptions[[1]], "Genes in class")
  expect_match(descriptions[[2]], "Gene call")
})

test_that("a field picker builds before any database is loaded", {
  expect_no_error(viz_helpers$field_select(function(x) x, "some_id", "Variable"))
})

test_that("refilling a field picker reaches shinyWidgets without erroring", {
  session <- MockShinySession$new()
  expect_no_error(
    viz_helpers$update_field_select(
      session,
      "some_id",
      field_profiles(meta()),
      selected = "purpose"
    )
  )
})

test_that("refilling with a sentinel and a disabled column both survive", {
  session <- MockShinySession$new()
  # `organism` is one value for every isolate, so it is listed but disabled —
  # the path that passes disabledChoices through.
  expect_no_error(
    viz_helpers$update_field_select(
      session,
      "some_id",
      field_profiles(meta()),
      selected = "",
      extra = c(`No annotation` = "")
    )
  )
})

test_that("refilling with no profiles is a no-op rather than an error", {
  session <- MockShinySession$new()
  expect_no_error(viz_helpers$update_field_select(session, "some_id", NULL))
})

test_that("a colour swatch row carries an id the module can toggle", {
  html <- as.character(viz_helpers$viz_color(
    function(x) paste0("mod-", x),
    "nj_tiplab_color",
    "Tip Label",
    "#000000"
  ))
  # The row, not the hidden input: colorPickr renders its swatch as a sibling,
  # so disabling the input would leave the swatch live.
  expect_true(grepl('id="mod-nj_tiplab_color_row"', html, fixed = TRUE))
})

test_that("a scale select renders its picker into <body>", {
  # Without this, a long palette list opened from a small modal has nowhere
  # to grow but the modal itself — see main.scss's "Dropdown overflow" rules,
  # which only cap/scroll menus that opted into container = "body".
  html <- as.character(viz_helpers$scale_select(function(x) x, "col_scale"))
  expect_true(grepl('data-container="body"', html, fixed = TRUE))
})

test_that("every palette option carries a gradient swatch style", {
  html <- as.character(viz_helpers$scale_select(function(x) x, "col_scale"))
  n_palettes <- length(unlist(viz_helpers$color_scales, use.names = FALSE))
  expect_true(lengths(regmatches(html, gregexpr("linear-gradient", html))) >= n_palettes)
})

test_that("a Brewer swatch bands its tabulated colours with hard stops", {
  style <- impl$.scale_swatch_style("Set1")
  expect_match(style, "linear-gradient", fixed = TRUE)
  expect_match(style, "%,", fixed = TRUE)
  expect_match(style, "color: black", fixed = TRUE)
})

test_that("a viridis swatch blends its sampled ramp smoothly", {
  style <- impl$.scale_swatch_style("viridis")
  expect_match(style, "linear-gradient", fixed = TRUE)
  expect_true(!grepl("%,", style, fixed = TRUE))
  expect_match(style, "color: white", fixed = TRUE)
})

test_that("the interval control opens at what the dates warrant", {
  # Every engine builds this control, so the default belongs here rather than
  # in each of them — and "Exact date" is the wrong default for anything but a
  # very short collection.
  pick <- function(...) {
    ui <- viz_helpers$granularity_select(function(x) x, "g", ...)
    grep("selected", as.character(ui), value = TRUE)
  }
  set.seed(8)
  decade <- as.character(as.Date("2011-01-01") + sample(3650, 200))
  expect_match(paste(pick(values = decade), collapse = " "), "year")
  # A fortnight of sampling is legible day by day.
  expect_match(
    paste(pick(values = as.character(as.Date("2024-03-01") + 0:9)),
      collapse = " "),
    "day"
  )
  # An explicit choice always wins over the suggestion.
  expect_match(paste(pick("none", values = decade), collapse = " "), "none")
  # And with nothing to go on it stays where it always was.
  expect_match(paste(pick(), collapse = " "), "none")
})
