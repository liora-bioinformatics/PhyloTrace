box::use(
  jsonlite[toJSON],
  shiny[moduleServer, observe, reactive, testServer],
  testthat[expect_false, expect_identical, expect_match, expect_true, test_that],
)
box::use(
  app / logic / overview_ui,
)

impl <- attr(overview_ui, "namespace")

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

test_that("scheme_value_html escapes stored text and links plain web addresses", {
  html <- overview_ui$scheme_value_html(c(
    "https://example.org/scheme",
    "<a href=\"https://example.org\">x</a>",
    "Tom & Jerry",
    NA
  ))

  expect_identical(
    html[1],
    paste0(
      "<a href=\"https://example.org/scheme\" target=\"_blank\" rel=\"noopener\">",
      "https://example.org/scheme</a>"
    )
  )
  # Stored markup is shown as text, never rendered or turned into a link.
  expect_identical(html[2], "&lt;a href=\"https://example.org\"&gt;x&lt;/a&gt;")
  expect_identical(html[3], "Tom &amp; Jerry")
  expect_identical(html[4], NA_character_)
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

test_that("data_table sends each column's longest values as width hints", {
  widget <- overview_ui$data_table(data.frame(
    id = c("GCA_1.1", "GCA_2.1_ASM2v1_genomic", NA, "GCA_3.1"),
    n = c(1L, 100L, 7L, 7L),
    one = "x"
  ))
  hints <- widget$x$options$widthHints

  expect_identical(length(hints), 3L)
  expect_identical(as.character(hints[[1]][1]), "GCA_2.1_ASM2v1_genomic")
  expect_false(anyNA(hints[[1]]))
  expect_identical(as.character(hints[[2]]), c("100", "1", "7"))
  # A single value must still serialise as an array for the JavaScript side.
  expect_match(as.character(toJSON(hints[[3]], auto_unbox = TRUE)), "^\\[")
})

test_that("width_hints keeps at most n values per column", {
  hints <- impl$width_hints(data.frame(x = letters), n = 3L)

  expect_identical(length(hints[[1]]), 3L)
})

test_that("data_table shows whole-number doubles as integers", {
  widget <- overview_ui$data_table(data.frame(
    start = c(1, 1338040),
    share = c(34.4, 0.8),
    day = as.Date(c("2024-01-01", "2024-01-02")),
    big = c(1, 3e9)
  ))
  data <- widget$x$data

  expect_true(is.integer(data$start))
  expect_true(is.double(data$share))
  expect_true(inherits(data$day, "Date"))
  expect_true(is.double(data$big))
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

test_that("chips shows every label in full with its tone", {
  html <- as.character(overview_ui$chips(c("blaTEM-1", "aph(3'')-Ib"), "secondary"))

  expect_match(html, "overview-chip is-secondary\">blaTEM-1", fixed = TRUE)
  expect_match(html, "aph(3'')-Ib", fixed = TRUE)
  expect_identical(lengths(regmatches(html, gregexpr("overview-chip ", html))), 2L)
})

test_that("side_gate hides the side panel's namespaced id only while it has nothing to show", {
  server <- function(id) {
    moduleServer(id, function(input, output, session) {
      overview_ui$side_gate(output, session, "side", reactive(input$rows))
    })
  }

  testServer(server, {
    session$setInputs(rows = FALSE)
    expect_match(output$side_gate$html, "#mod-side { display: none", fixed = TRUE)
    session$setInputs(rows = TRUE)
    expect_identical(length(as.character(output$side_gate$html)), 0L)
  }, args = list(id = "mod"))
})

test_that("side_gate_ui is a hidden placeholder bound to the gate output", {
  html <- as.character(overview_ui$side_gate_ui(shiny::NS("mod"), "side"))

  expect_match(html, "id=\"mod-side_gate\"", fixed = TRUE)
  expect_match(html, "display: none;", fixed = TRUE)
})
