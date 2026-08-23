box::use(
  testthat[
    expect_identical,
    expect_length,
    expect_match,
    expect_null,
    expect_true,
    test_that
  ],
)
box::use(
  app / logic / viz_layers,
)

impl <- attr(viz_layers, "namespace")

ns <- function(id) paste0("mod-", id)

a_layer <- function(...) {
  rec <- list(
    id = "L1",
    field = "country",
    title = "Country",
    aesthetic = "annotation",
    palette = "Set1",
    family = "Qualitative",
    n_levels = 3L,
    continuous = FALSE,
    transform = NULL,
    granularity = NULL,
    auto = TRUE
  )
  overrides <- list(...)
  for (f in names(overrides)) rec[[f]] <- overrides[[f]]
  rec
}

test_that("layer_defaults opens each medium on a channel it actually has", {
  expect_identical(viz_layers$layer_defaults("amr")$aesthetic, "annotation")
  expect_identical(viz_layers$layer_defaults("mst")$aesthetic, "node_fill")
  # An engine may override the opening channel without restating the record.
  tree <- viz_layers$layer_defaults("tree", aesthetic = "tiplab_color")
  expect_identical(tree$aesthetic, "tiplab_color")
  expect_identical(tree$auto, TRUE)
})

test_that("normalize_layers rebuilds records from a data frame", {
  # jsonlite hands a saved JSON array back as a data frame, not a list of lists.
  saved <- data.frame(
    id = c("L1", "L2"),
    field = c("country", "host"),
    title = c("Country", "Host"),
    aesthetic = "annotation",
    palette = c("Set1", "Set2"),
    n_levels = c(3L, 2L),
    stringsAsFactors = FALSE
  )
  out <- viz_layers$normalize_layers(
    saved,
    viz_layers$layer_defaults("amr"),
    "amr"
  )
  expect_length(out, 2L)
  expect_identical(out[[2]]$field, "host")
  # Anything the snapshot did not carry comes from the defaults.
  expect_identical(out[[1]]$auto, TRUE)
})

test_that("normalize_layers hands a withdrawn channel back to the engine", {
  out <- viz_layers$normalize_layers(
    list(a_layer(aesthetic = "tippoint_shape", auto = FALSE)),
    viz_layers$layer_defaults("amr"),
    "amr"
  )
  # The AMR heatmap has no tip points, so the saved channel cannot be honoured —
  # but dropping the layer would silently lose a mapping the user asked for.
  expect_identical(out[[1]]$aesthetic, "annotation")
  expect_identical(out[[1]]$auto, TRUE)
})

test_that("normalize_layers drops a record with no field to draw", {
  out <- viz_layers$normalize_layers(
    list(a_layer(), a_layer(id = "L2", field = NA_character_)),
    viz_layers$layer_defaults("amr"),
    "amr"
  )
  expect_length(out, 1L)
  expect_null(viz_layers$normalize_layers(NULL, list(), "amr"))
})

test_that("the card's summary line names channel, count, grouping and palette", {
  meta <- impl$.layer_meta(
    a_layer(granularity = "month", n_levels = 30L),
    c(annotation = "Annotation strip"),
    legend_max = 9L
  )
  expect_identical(
    meta,
    "Annotation strip · 30 values · by month · Set1 · 9 listed"
  )
  # Nothing absent leaves a stray separator behind.
  expect_identical(
    impl$.layer_meta(a_layer(), c(annotation = "Annotation strip")),
    "Annotation strip · 3 values · Set1"
  )
})

test_that("layer_cards puts the layer id in each button's value", {
  html <- paste(
    as.character(
      viz_layers$layer_cards(ns, list(a_layer()), "amr", "edit", "del")
    ),
    collapse = ""
  )
  expect_match(html, "mod-edit", fixed = TRUE)
  expect_match(html, "mod-del", fixed = TRUE)
  expect_match(html, "&#39;L1&#39;", fixed = TRUE)
})

test_that("an empty layer set renders its own message", {
  html <- paste(
    as.character(
      viz_layers$layer_cards(
        ns, list(), "amr", "edit", "del",
        empty_text = "No variables mapped."
      )
    ),
    collapse = ""
  )
  expect_match(html, "No variables mapped.", fixed = TRUE)
})

test_that("the list helpers find, drop and test membership by id", {
  layers <- list(a_layer(), a_layer(id = "L2", field = "host"))
  expect_identical(viz_layers$find_layer(layers, "L2")$field, "host")
  expect_null(viz_layers$find_layer(layers, "L9"))
  expect_length(viz_layers$drop_layer(layers, "L1"), 1L)
  expect_true(viz_layers$layer_has_field(layers, "host"))
  expect_true(!viz_layers$layer_has_field(layers, "city"))
})

test_that("layer ids are never reused", {
  counter <- local({
    n <- 0L
    function(v) {
      if (missing(v)) n else n <<- v
    }
  })
  nxt <- viz_layers$layer_id_source(counter)
  expect_identical(c(nxt(), nxt(), nxt()), c("L1", "L2", "L3"))
})
