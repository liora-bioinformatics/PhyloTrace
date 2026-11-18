box::use(
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
