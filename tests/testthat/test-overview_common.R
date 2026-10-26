box::use(
  testthat[expect_identical, expect_true, test_that],
  withr[local_tempdir],
)
box::use(
  app / logic / overview_common,
)

test_that("blank_na collapses empty strings", {
  expect_identical(overview_common$blank_na(c("a", "", " ", NA)), c("a", NA, NA, NA))
})

test_that("modal picks the most frequent value, first seen on a tie", {
  expect_identical(overview_common$modal(c("b", "a", "a", NA)), "a")
  expect_identical(overview_common$modal(c("b", "a")), "b")
  expect_identical(overview_common$modal(NA), NA_character_)
})

test_that("tally names the most frequent values first and counts the rest", {
  expect_identical(overview_common$tally(c("x", "y", "y")), "y (2), x (1)")
  expect_identical(
    overview_common$tally(letters[1:6], limit = 2),
    "a (1), b (1), 4 more"
  )
})

test_that("threshold_pairs describes recorded thresholds only", {
  expect_identical(
    overview_common$threshold_pairs(c(0.9, NA, NA), c(0.85, 0.9, NA)),
    c("identity 0.9 / coverage 0.85", "identity - / coverage 0.9", NA)
  )
})

test_that("check_list orders checks by severity", {
  checks <- overview_common$check_list()
  checks$add("Fine", "success", "ok")
  checks$add("Broken", "danger", "bad", 3)
  checks$add("Odd", "info", "hm", 1)
  out <- checks$table()

  expect_identical(out$check, c("Broken", "Odd", "Fine"))
  expect_identical(out$n, c(3L, 1L, NA))
  empty <- overview_common$check_list()
  expect_identical(nrow(empty$table()), 0L)
})

test_that("database_isolates prefers metadata and drops the reference", {
  path <- file.path(local_tempdir(), "db.sqlite")
  con <- DBI::dbConnect(RSQLite::SQLite(), path)
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  DBI::dbWriteTable(con, "mlst", data.frame(souche = c("ref", "A", "B"), gene = "g"))

  expect_identical(
    sort(overview_common$database_isolates(con, DBI::dbListTables(con))),
    c("A", "B")
  )
  DBI::dbWriteTable(con, "metadata", data.frame(isolate = "A"))
  expect_identical(overview_common$database_isolates(con, DBI::dbListTables(con)), "A")
})

test_that("read_cols fills columns an older table lacks", {
  path <- file.path(local_tempdir(), "db.sqlite")
  con <- DBI::dbConnect(RSQLite::SQLite(), path)
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  DBI::dbWriteTable(con, "t", data.frame(a = 1:2))
  out <- overview_common$read_cols(con, "t", c("a", "b"))

  expect_identical(names(out), c("a", "b"))
  expect_true(all(is.na(out$b)))
})

test_that("assembly_outliers flags size and fragmentation against the median", {
  sizes <- c(3e6, 3.02e6, 2.98e6, 3.01e6, 4.5e6, 2e6, NA)
  contigs <- c(50, 55, 60, 45, 400, 52, NA)
  out <- overview_common$assembly_outliers(sizes, contigs)

  expect_identical(out$odd_size, c(FALSE, FALSE, FALSE, FALSE, TRUE, TRUE, FALSE))
  expect_identical(out$fragmented, c(FALSE, FALSE, FALSE, FALSE, TRUE, FALSE, FALSE))
})

test_that("assembly_outliers flags nothing below five assemblies", {
  out <- overview_common$assembly_outliers(c(1e6, 9e6, NA), c(1, 900, NA))

  expect_identical(out$odd_size, c(FALSE, FALSE, FALSE))
  expect_identical(out$fragmented, c(FALSE, FALSE, FALSE))
})
