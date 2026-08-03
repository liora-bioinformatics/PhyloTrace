box::use(
  app / view / database_browser,
)

impl <- attr(database_browser, "namespace")

# paste0() recycles a zero-length vector to "", so the prefix is applied only
# when there is something to prefix - otherwise a zero-isolate frame would come
# back with one row.
.frame <- function(isolates, prefix) {
  isolates <- as.character(isolates)
  data.frame(
    isolate = isolates,
    host = if (length(isolates)) paste0(prefix, isolates) else character(0),
    stringsAsFactors = FALSE
  )
}

# The grid's snapshot: what the user has been editing.
edited_frame <- function(isolates) .frame(isolates, "edited-")

# The metadata table as it stands in the database right now.
current_frame <- function(isolates) .frame(isolates, "db-")

test_that("an unchanged pool keeps every edit", {
  got <- impl$.reconcile_metadata(
    edited_frame(c("a", "b")),
    current_frame(c("a", "b"))
  )
  expect_identical(got$isolate, c("a", "b"))
  expect_identical(got$host, c("edited-a", "edited-b"))
})

test_that("an isolate added since the grid was filled survives the save", {
  # The regression this guards: save_metadata_table() overwrites the whole
  # table, so writing the grid's snapshot unreconciled would delete the
  # metadata row of an isolate typed while the user was editing.
  got <- impl$.reconcile_metadata(
    edited_frame(c("a", "b")),
    current_frame(c("a", "b", "typed_meanwhile"))
  )
  expect_identical(got$isolate, c("a", "b", "typed_meanwhile"))
  expect_identical(got$host, c("edited-a", "edited-b", "db-typed_meanwhile"))
})

test_that("an isolate removed since the grid was filled is not resurrected", {
  got <- impl$.reconcile_metadata(
    edited_frame(c("a", "removed", "b")),
    current_frame(c("a", "b"))
  )
  expect_identical(got$isolate, c("a", "b"))
  expect_false("removed" %in% got$isolate)
})

test_that("edits apply to the right rows when the pool changed order", {
  # match() on isolate, not row position: the fresh read may order rows
  # differently, and aligning by position would write A's host onto B.
  got <- impl$.reconcile_metadata(
    edited_frame(c("a", "b", "c")),
    current_frame(c("c", "a", "b"))
  )
  expect_identical(got$isolate, c("c", "a", "b"))
  expect_identical(got$host, c("edited-c", "edited-a", "edited-b"))
})

test_that("simultaneous addition and removal are both handled", {
  got <- impl$.reconcile_metadata(
    edited_frame(c("a", "gone")),
    current_frame(c("a", "fresh"))
  )
  expect_identical(got$isolate, c("a", "fresh"))
  expect_identical(got$host, c("edited-a", "db-fresh"))
})

test_that("the database's column set wins for columns the grid never had", {
  edited <- edited_frame("a")
  current <- current_frame("a")
  current$country <- "DE"
  got <- impl$.reconcile_metadata(edited, current)
  expect_true("country" %in% names(got))
  expect_identical(got$country, "DE")
  expect_identical(got$host, "edited-a")
})

test_that("a column the grid holds but the database lacks is not written back", {
  # Derived columns (MLST / AMR / custom) are stripped before this point, but
  # the guard matters: they must never reach the metadata table.
  edited <- edited_frame("a")
  edited$mlst_ST <- "7"
  got <- impl$.reconcile_metadata(edited, current_frame("a"))
  expect_false("mlst_ST" %in% names(got))
})

test_that("the isolate key itself is never overwritten from the snapshot", {
  got <- impl$.reconcile_metadata(
    edited_frame(c("a", "b")),
    current_frame(c("a", "b"))
  )
  expect_identical(got$isolate, c("a", "b"))
})

test_that("no overlap at all leaves the database untouched", {
  got <- impl$.reconcile_metadata(
    edited_frame(c("x", "y")),
    current_frame(c("a", "b"))
  )
  expect_identical(got$isolate, c("a", "b"))
  expect_identical(got$host, c("db-a", "db-b"))
})

test_that("an empty or missing current table falls back to the snapshot", {
  # A database with no metadata rows yet: there is nothing to reconcile
  # against, so the grid is the only source.
  expect_identical(
    impl$.reconcile_metadata(edited_frame("a"), NULL)$isolate,
    "a"
  )
  expect_identical(
    impl$.reconcile_metadata(edited_frame("a"), current_frame(character(0)))$isolate,
    "a"
  )
})

test_that("an empty snapshot yields the current table unchanged", {
  got <- impl$.reconcile_metadata(edited_frame(character(0)), current_frame("a"))
  expect_identical(got$isolate, "a")
  expect_identical(got$host, "db-a")
})
