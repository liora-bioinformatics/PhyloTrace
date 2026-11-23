box::use(
  shiny[testServer],
  testthat[expect_false, expect_match, expect_no_warning, expect_null, test_that],
)

box::use(
  app / logic / setup_ui,
)

test_that("task state rules are built per state, and 'open' adds none", {
  got <- new.env()
  expect_no_warning(
    testServer(
      function(input, output, session) {
        output$states <- setup_ui$task_states(function() {
          c(a = "current", b = "done", c = "locked", d = "open")
        })
      },
      got$css <- output$states$html
    )
  )
  expect_match(got$css, "#a \\{ border-color")
  expect_match(got$css, "#b .setup-task_check")
  expect_match(got$css, "#c .setup-task_lock")
  expect_false(grepl("#d {", got$css, fixed = TRUE))
  # Locked task bodies are inert, all others are not.
  expect_match(got$css, "#c > .setup-task_body'); if (b) b.inert = true", fixed = TRUE)
  expect_match(got$css, "#a > .setup-task_body'); if (b) b.inert = false", fixed = TRUE)
})

test_that("scheme cards offer the download guide only when asked to", {
  rows <- data.frame(
    type = "cgMLST",
    provider = "pubmlst",
    scheme = "Test cgMLST",
    loci = 10L,
    url = "https://pubmlst.org/bigsdb?db=pubmlst_test_seqdef",
    note = NA_character_,
    stringsAsFactors = FALSE
  )
  plain <- as.character(setup_ui$scheme_links(rows))
  expect_false(grepl("How to download", plain, fixed = TRUE))
  guided <- as.character(setup_ui$scheme_links(rows, guide_input = "x-guide"))
  expect_match(guided, "Shiny.setInputValue(&#39;x-guide&#39;, {i: 1", fixed = TRUE)
  expect_match(guided, "How to download", fixed = TRUE)
})

test_that("a task status is dropped when it has no text", {
  expect_null(setup_ui$task_status(""))
  expect_null(setup_ui$task_status(NA_character_))
  expect_match(as.character(setup_ui$task_status("12 loci", "good")), "is-good")
})
