# The lock wait every connection arrives with, and the option a caller uses to
# lower it for a bounded stretch of work.

box::use(
  DBI[dbDisconnect, dbGetQuery],
  testthat[expect_error, expect_false, expect_identical, expect_true, test_that],
  withr[local_options, local_tempdir],
)
box::use(
  app / logic / db_connect,
)

# The pragma as the connection actually reports it back. create = TRUE because
# these paths are throwaway files that do not exist yet; connect() refuses a
# missing database otherwise.
armed_timeout <- function(path) {
  con <- db_connect$connect(path, create = TRUE)
  on.exit(dbDisconnect(con))
  dbGetQuery(con, "PRAGMA busy_timeout")[[1]]
}

test_that("a connection arrives with the interactive wait armed", {
  path <- file.path(local_tempdir(), "db.db")
  expect_identical(armed_timeout(path), db_connect$BUSY_TIMEOUT_MS)
})

test_that("the option lowers the wait, and only while it is set", {
  path <- file.path(local_tempdir(), "db.db")

  local({
    local_options(phylotrace.busy_timeout = db_connect$LIVE_BUSY_TIMEOUT_MS)
    expect_identical(armed_timeout(path), db_connect$LIVE_BUSY_TIMEOUT_MS)
  })

  expect_identical(armed_timeout(path), db_connect$BUSY_TIMEOUT_MS)
})

test_that("connect refuses a database that is not there", {
  path <- file.path(local_tempdir(), "gone.db")

  expect_error(db_connect$connect(path), "not found")
  # And crucially it did not conjure one on the way out: SQLite's default flags
  # would have left an empty file here, which then lies to every file.exists()
  # check the app makes about whether the loaded database still exists.
  expect_false(file.exists(path))
})

test_that("create = TRUE is how a caller that builds a database opts in", {
  path <- file.path(local_tempdir(), "made.db")

  con <- db_connect$connect(path, create = TRUE)
  dbDisconnect(con)

  expect_true(file.exists(path))
})
