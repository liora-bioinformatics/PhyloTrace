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
  expect_false(grepl("#d", got$css, fixed = TRUE))
})

test_that("a task status is dropped when it has no text", {
  expect_null(setup_ui$task_status(""))
  expect_null(setup_ui$task_status(NA_character_))
  expect_match(as.character(setup_ui$task_status("12 loci", "good")), "is-good")
})
