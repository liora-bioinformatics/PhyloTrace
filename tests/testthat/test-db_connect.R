# The lock wait every connection arrives with, and the option a caller uses to
# lower it for a bounded stretch of work.

box::use(
  DBI[dbDisconnect, dbGetQuery],
  testthat[expect_identical, test_that],
  withr[local_options, local_tempdir],
)
box::use(
  app / logic / db_connect,
)

# The pragma as the connection actually reports it back.
armed_timeout <- function(path) {
  con <- db_connect$connect(path)
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
