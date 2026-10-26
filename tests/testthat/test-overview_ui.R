box::use(
  shiny[moduleServer, observe, reactive, testServer],
  testthat[expect_false, expect_identical, expect_match, expect_true, test_that],
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

test_that("data_table draws rows on demand and truncates only text columns", {
  widget <- overview_ui$data_table(
    data.frame(name = c("a", "b"), n = 1:2, flag = c("x", "y"), stringsAsFactors = FALSE)
  )
  defs <- widget$x$options$columnDefs

  expect_true("dt-ext-scroller" %in% vapply(widget$dependencies, `[[`, "", "name"))
  expect_false(isFALSE(widget$x$options$paging))
  expect_identical(defs[[2]]$targets, c(0L, 2L))
})

test_that("selected_row re-runs its dependents only when the selection changes", {
  testServer(
    function(id) {
      moduleServer(id, function(input, output, session) {
        rows <- data.frame(x = c("a", "b"), stringsAsFactors = FALSE)
        selected <- overview_ui$selected_row(input, "tbl", function() rows)
        counter <- new.env()
        counter$runs <- 0L
        shown <- reactive({
          x <- selected()$x
          counter$runs <- counter$runs + 1L
          x
        })
        observe(shown())
      })
    },
    {
      session$setInputs(tbl_rows_selected = 1L)
      expect_identical(shown(), "a")
      session$setInputs(tbl_rows_selected = integer(0))
      session$setInputs(tbl_rows_selected = 1L)
      expect_identical(counter$runs, 1L)
      session$setInputs(tbl_rows_selected = 2L)
      expect_identical(shown(), "b")
      expect_identical(counter$runs, 2L)
    }
  )
})

test_that("sequence_view wraps each run of one base in a single span", {
  html <- as.character(overview_ui$sequence_view("AATGGGC"))

  expect_identical(
    html,
    paste0(
      "<span class='base-a'>AA</span><span class='base-t'>T</span>",
      "<span class='base-g'>GGG</span><span class='base-c'>C</span>"
    )
  )
})

test_that("section pairs a caption with its content inside a plain stack", {
  html <- as.character(overview_ui$sections(overview_ui$section("Origin", "rows")))

  expect_match(html, "overview-sections", fixed = TRUE)
  expect_match(html, "overview-section-title\">Origin", fixed = TRUE)
  expect_match(html, "rows", fixed = TRUE)
})
