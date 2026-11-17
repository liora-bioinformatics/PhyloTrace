box::use(
  testthat[expect_error, expect_false, expect_identical, expect_true, test_that],
  withr[local_tempdir],
)
box::use(
  app / logic / file_browser,
)

test_that("name_problem refuses names one of the platforms cannot hold", {
  expect_identical(file_browser$name_problem("My_database-1"), NA_character_)
  for (bad in c("", " ", "a/b", "a\\b", "a:b", "a*b", "a?b", "con", "NUL.db", "name.", "..")) {
    expect_false(is.na(file_browser$name_problem(bad)), info = bad)
  }
})

test_that("list_dir lists folders first and marks, never hides, unexpected files", {
  dir <- local_tempdir()
  dir.create(file.path(dir, "zeta"))
  dir.create(file.path(dir, ".hidden"))
  file.create(file.path(dir, c("a.fasta", "B.zip", "notes.doc", ".secret.fasta")))

  all <- file_browser$list_dir(dir)
  expect_identical(all$name, c("zeta", "a.fasta", "B.zip", "notes.doc"))
  expect_identical(all$is_dir, c(TRUE, FALSE, FALSE, FALSE))

  # A browser can save a download without an extension, so a file that does
  # not match the pattern stays listed and pickable.
  fasta <- file_browser$list_dir(dir, pattern = "\\.fasta$")
  expect_identical(fasta$name, c("zeta", "a.fasta", "B.zip", "notes.doc"))
  expect_identical(fasta$match, c(FALSE, TRUE, FALSE, FALSE))

  dirs <- file_browser$list_dir(dir, files = FALSE, hidden = TRUE)
  expect_identical(dirs$name, c(".hidden", "zeta"))
})

test_that("paths are normalised, walked up and split into breadcrumbs", {
  dir <- normalizePath(local_tempdir(), winslash = "/")
  sub <- file.path(dir, "inner")
  dir.create(sub)

  expect_identical(file_browser$normalize_dir(paste0(sub, "/")), sub)
  expect_identical(file_browser$normalize_dir(file.path(dir, "missing")), NA_character_)
  expect_identical(file_browser$parent_dir(sub), dir)
  expect_identical(file_browser$parent_dir("/"), "/")

  crumbs <- file_browser$path_crumbs(sub)
  expect_identical(crumbs$label[1], "/")
  expect_identical(tail(crumbs$label, 2), c(basename(dir), "inner"))
  expect_identical(tail(crumbs$path, 1), sub)
})

test_that("create_folder makes a valid new folder and refuses the rest", {
  dir <- local_tempdir()
  made <- file_browser$create_folder(dir, "new")
  expect_true(dir.exists(made))
  expect_error(file_browser$create_folder(dir, "new"), "already exists")
  expect_error(file_browser$create_folder(dir, "a/b"), "not allowed")
})

test_that("places always include an existing home folder", {
  rows <- file_browser$places()
  expect_identical(rows$label[1], "Home")
  expect_true(all(dir.exists(rows$path)))
})
