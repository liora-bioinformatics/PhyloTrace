box::use(
  testthat[expect_identical, expect_match, expect_true, test_that],
)
box::use(
  app / logic / overview_ui,
)

test_that("fold_top keeps the top rows and folds the rest into one", {
  bars <- overview_ui$fold_top(c("a", "b", "c", "d"), c(5, 3, 2, 1), 2, "%d others")

  expect_identical(bars$labels, c("a", "b", "2 others"))
  expect_identical(bars$counts, c(5, 3, 3))
  expect_identical(bars$tones, c("primary", "primary", "secondary"))
  expect_identical(overview_ui$fold_top("a", 1, 3, "%d others")$labels, "a")
})

test_that("bar_rows scales bars to the largest count and shows shares", {
  bars <- overview_ui$bar_rows(c("x", "y"), c(10, 5), 20, tones = c("success", "danger"))
  html <- as.character(bars)

  expect_match(html, "width: 100.0%", fixed = TRUE)
  expect_match(html, "width: 50.0%", fixed = TRUE)
  expect_match(html, "50.0%</span>", fixed = TRUE)
  expect_match(html, "overview-bar_fill is-danger", fixed = TRUE)
})

test_that("checks_ui renders one card per check with its tone", {
  checks <- data.frame(
    check = c("Broken", "Fine"),
    tone = c("danger", "success"),
    detail = c("bad", "ok"),
    n = c(2L, NA),
    stringsAsFactors = FALSE
  )
  html <- as.character(overview_ui$checks_ui(checks))

  expect_match(html, "overview-check is-danger", fixed = TRUE)
  expect_match(html, "overview-check is-success", fixed = TRUE)
  expect_identical(lengths(regmatches(html, gregexpr("overview-check_count", html))), 1L)
})

test_that("key_value_table drops unrecorded rows and escapes labels", {
  widget <- overview_ui$key_value_table(c("A<b>", "B"), c("<a href='x'>x</a>", NA))

  expect_identical(nrow(widget$x$data), 1L)
  expect_true(grepl("A&lt;b&gt;", widget$x$data$key[1], fixed = TRUE))
})

test_that("count_label pluralizes its noun", {
  expect_identical(overview_ui$count_label(1), "1 isolate")
  expect_identical(overview_ui$count_label(1204), "1,204 isolates")
  expect_identical(overview_ui$count_label(0), "0 isolates")
})
