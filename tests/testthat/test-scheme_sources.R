box::use(
  jsonlite[fromJSON],
  testthat[expect_false, expect_identical, expect_match, expect_null, expect_true, test_that],
)
box::use(
  app / logic / scheme_sources,
)

test_that("providers are offered only for the scheme types they publish", {
  mlst <- scheme_sources$source_choices("MLST")
  expect_false(any(c("cgmlst_org", "chewie_ns") %in% mlst))
  expect_true(all(c("pubmlst", "pasteur", "own", "other") %in% mlst))
  expect_identical(length(scheme_sources$source_choices()), 7L)
})

test_that("only plain http(s) addresses are safe links", {
  expect_true(scheme_sources$safe_url("https://www.cgmlst.org/ncs/schema/1/"))
  expect_false(scheme_sources$safe_url("javascript:alert(1)"))
  expect_false(scheme_sources$safe_url("https://x.org/\"onmouseover=\"a"))
  expect_false(scheme_sources$safe_url(NA_character_))
})

test_that("versions with shell or control characters are refused", {
  expect_null(scheme_sources$version_problem(""))
  expect_null(scheme_sources$version_problem("v2.1 build 3"))
  expect_null(scheme_sources$version_problem("2026-11-16"))
  expect_match(scheme_sources$version_problem("$(rm -rf ~)"), "may only contain")
  expect_match(scheme_sources$version_problem("1`id`"), "may only contain")
})

test_that("every provider but an own scheme shows a rule for commercial use", {
  # The register of reviewed provider terms; each provider in it must reach the picker.
  register <- fromJSON("../../dev/scheme_index/providers.json")$providers
  ids <- unname(scheme_sources$source_choices())
  expect_true(all(names(register) %in% ids))
  for (id in setdiff(ids, "own")) {
    rule <- scheme_sources$scheme_source(id)$commercial_use
    expect_true(is.character(rule) && !is.na(rule) && nzchar(rule), info = id)
  }
  expect_true(is.na(scheme_sources$scheme_source("own")$commercial_use))
})
