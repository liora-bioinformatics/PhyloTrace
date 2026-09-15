# A database error inside guarded work is reported and handed back as a
# failure, never raised - that is what keeps it from ending a Shiny session.

box::use(
  DBI[dbDisconnect, dbExecute, dbGetQuery],
  shiny[req],
  testthat[expect_error, expect_false, expect_identical, expect_match, expect_message],
  testthat[expect_true, test_that],
  withr[local_options, local_tempdir],
)
box::use(
  app / logic / db_connect,
  app / logic / db_guard,
)

# A database that a second connection holds an exclusive lock on - the state a
# typing run's pyMLST commit, or another session's write, leaves it in.
lock_database <- function(dir) {
  path <- file.path(dir, "locked.db")
  holder <- db_connect$connect(path, create = TRUE)
  dbExecute(holder, "CREATE TABLE t (x INTEGER)")
  dbExecute(holder, "BEGIN EXCLUSIVE")
  list(path = path, holder = holder)
}

release <- function(db) {
  dbExecute(db$holder, "ROLLBACK")
  dbDisconnect(db$holder)
}

read_t <- function(path) {
  con <- db_connect$connect(path, synchronous = NULL)
  on.exit(dbDisconnect(con))
  dbGetQuery(con, "SELECT * FROM t")
}

test_that("a lock held by another connection is reported, not raised", {
  db <- lock_database(local_tempdir())
  on.exit(release(db))
  local_options(phylotrace.busy_timeout = 50L)

  expect_message(
    res <- db_guard$guard_db("Reading t", read_t(db$path)),
    "Reading t failed.*database is locked"
  )
  expect_true(db_guard$db_failed(res))
})

test_that("guarded work that succeeds hands back its value", {
  res <- db_guard$guard_db("Answering", 42)

  expect_identical(res, 42)
  expect_false(db_guard$db_failed(res))
})

test_that("a lock is described as something the user can act on", {
  locked <- db_guard$db_failure_reason(simpleError("database is locked"))
  expect_match(locked, "another process")

  other <- db_guard$db_failure_reason(simpleError("no such table: mlst"))
  expect_identical(other, "no such table: mlst")
})

test_that("req() inside guarded work stays a silent cancel, not a failure", {
  expect_error(
    db_guard$guard_db("Bailing out", req(FALSE)),
    class = "shiny.silent.error"
  )
})

test_that("a failed reactive read cancels quietly instead of raising", {
  expect_error(
    suppressMessages(
      db_guard$guard_db_read("Loading t", stop("database is locked"))
    ),
    class = "shiny.output.cancel"
  )
})
