box::use(
  testthat[expect_equal, expect_identical, expect_true, test_that],
  withr[local_tempdir],
)
box::use(
  app / view / landing_page,
)

impl <- attr(landing_page, "namespace")

test_that("format_size picks a decimal unit", {
  expect_identical(impl$format_size(512), "512 Bytes")
  expect_identical(impl$format_size(1500), "1.5 KB")
  expect_identical(impl$format_size(333930000), "333.93 MB")
  expect_identical(impl$format_size(2.5e9), "2.5 GB")
  expect_true(is.na(impl$format_size(NA)))
})

test_that("truncate_middle keeps both ends of a long path", {
  short <- "/home/user/db"
  expect_identical(impl$truncate_middle(short), short)

  long <- paste0("/home/", strrep("a", 100), "/datasets/end")
  out <- impl$truncate_middle(long, 40L)
  expect_equal(nchar(out), 40L)
  expect_true(startsWith(out, "/home/"))
  expect_true(endsWith(out, "/datasets/end"))
  expect_true(grepl("…", out, fixed = TRUE))
})

test_that("the database card names the file and its species", {
  dir <- local_tempdir()
  db <- file.path(dir, "local.db")
  build_db(db, default_local(), metadata = meta_df(c("A", "B")))

  html <- as.character(impl$db_entry("Last used", db))
  expect_true(grepl("local.db", html, fixed = TRUE))
  expect_true(grepl("Testus organismus", html, fixed = TRUE))
  expect_true(grepl("Last used", html, fixed = TRUE))
})

test_that("an unloadable file shows its reason", {
  html <- as.character(impl$db_entry_invalid("Selected", "/x/y/broken.db", "Not a database."))
  expect_true(grepl("broken.db", html, fixed = TRUE))
  expect_true(grepl("Not a database.", html, fixed = TRUE))
})
