box::use(
  shiny[testServer],
  testthat[expect_null, test_that],
)
box::use(
  app/main[server],
)

test_that("main server works", {
  testServer(server, {
    # No database is loaded and no typing run is active, so the navbar
    # spinner renders nothing (see output$typing_indicator's definition).
    expect_null(output$typing_indicator$html)
  })
})
