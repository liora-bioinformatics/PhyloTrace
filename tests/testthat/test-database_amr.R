box::use(
  testthat[expect_identical, expect_match, expect_no_match, test_that],
)
box::use(
  app / view / database_amr,
)

impl <- attr(database_amr, "namespace")

profile_row <- function(profile, classes) {
  data.frame(
    Profile = profile,
    Elements = 2L,
    Classes = classes,
    Isolates = 35L,
    `Share (%)` = 15.5,
    check.names = FALSE,
    stringsAsFactors = FALSE
  )
}

test_that("profile_detail lists every element and class in full", {
  row <- profile_row("blaTEM-1, fosA", "Beta-lactam, Fosfomycin")
  html <- as.character(impl$profile_detail(row, TRUE))

  expect_match(html, "Elements (2)", fixed = TRUE)
  expect_match(html, "overview-chip is-primary\">blaTEM-1", fixed = TRUE)
  expect_match(html, "overview-chip is-secondary\">Fosfomycin", fixed = TRUE)
})

test_that("profile_detail leaves out classes for types without them", {
  html <- as.character(impl$profile_detail(profile_row("stxA", "Beta-lactam"), FALSE))

  expect_no_match(html, "Drug Classes", fixed = TRUE)
})

test_that("profile_detail explains the (none) profile instead of listing elements", {
  html <- as.character(impl$profile_detail(profile_row("(none)", NA_character_), TRUE))

  expect_match(html, "carry no element", fixed = TRUE)
  expect_no_match(html, "overview-chip", fixed = TRUE)
  expect_identical(grepl("Elements (", html, fixed = TRUE), FALSE)
})
