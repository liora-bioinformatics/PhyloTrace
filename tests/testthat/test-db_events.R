box::use(
  app / logic / db_events,
)

impl <- attr(db_events, "namespace")

# --- domains ----------------------------------------------------------------

test_that("a fresh bus starts every domain at zero", {
  bus <- db_events$new_bus()
  shiny::isolate({
    expect_setequal(names(db_events$revision(bus)), db_events$DOMAINS)
    expect_true(all(db_events$revision(bus) == 0L))
  })
})

test_that("an unknown domain is an error rather than a silent no-op", {
  bus <- db_events$new_bus()
  # The whole point of the module is that a reader and a writer agree on a
  # name; a typo that quietly bumped nothing would recreate the staleness bug.
  expect_error(db_events$bump(bus, "isolate"), "unknown domain")
  expect_error(db_events$depend(bus, "custom_field"), "unknown domain")
  expect_error(db_events$bump(bus), "no domain given")
})

test_that("the known-domain list is reported back in the error", {
  bus <- db_events$new_bus()
  expect_error(db_events$bump(bus, "nope"), "isolates")
})

# --- bumping ----------------------------------------------------------------

test_that("bump advances only the domains named", {
  bus <- db_events$new_bus()
  shiny::isolate({
    db_events$bump(bus, "isolates", "amr")
    expect_identical(unname(db_events$revision(bus, "isolates")), 1L)
    expect_identical(unname(db_events$revision(bus, "amr")), 1L)
    expect_identical(unname(db_events$revision(bus, "metadata")), 0L)
    expect_identical(unname(db_events$revision(bus, "analyses")), 0L)
  })
})

test_that("bumps accumulate", {
  bus <- db_events$new_bus()
  shiny::isolate({
    db_events$bump(bus, "staged")
    db_events$bump(bus, "staged")
    db_events$bump(bus, "staged")
    expect_identical(unname(db_events$revision(bus, "staged")), 3L)
  })
})

test_that("bump_all advances every domain exactly once", {
  bus <- db_events$new_bus()
  shiny::isolate({
    db_events$bump_all(bus)
    expect_true(all(db_events$revision(bus) == 1L))
  })
})

test_that("revision defaults to all domains and preserves the asked-for order", {
  bus <- db_events$new_bus()
  shiny::isolate({
    db_events$bump(bus, "schema")
    expect_identical(names(db_events$revision(bus)), db_events$DOMAINS)
    got <- db_events$revision(bus, "amr", "schema")
    expect_identical(names(got), c("amr", "schema"))
    expect_identical(unname(got), c(0L, 1L))
  })
})

# --- reactivity -------------------------------------------------------------

test_that("a reader that depends on a domain re-runs when it is bumped", {
  bus <- db_events$new_bus()
  runs <- 0L
  r <- shiny::reactive({
    db_events$depend(bus, "isolates")
    runs <<- runs + 1L
    runs
  })

  shiny::isolate(r())
  expect_identical(runs, 1L)

  # Same value re-read without a bump: the reactive is still valid.
  shiny::isolate(r())
  expect_identical(runs, 1L)

  db_events$bump(bus, "isolates")
  shiny::isolate(r())
  expect_identical(runs, 2L)
})

test_that("a reader is not woken by a domain it did not name", {
  bus <- db_events$new_bus()
  runs <- 0L
  r <- shiny::reactive({
    db_events$depend(bus, "analyses")
    runs <<- runs + 1L
  })

  shiny::isolate(r())
  db_events$bump(bus, "isolates", "metadata", "amr", "staged", "schema")
  shiny::isolate(r())
  expect_identical(runs, 1L)
})

test_that("bumping from inside a reactive does not make it depend on itself", {
  # A writer that bumps in an observer must not thereby subscribe to its own
  # bump, which would re-trigger it forever.
  bus <- db_events$new_bus()
  runs <- 0L
  r <- shiny::reactive({
    runs <<- runs + 1L
    db_events$bump(bus, "metadata")
    runs
  })

  shiny::isolate(r())
  shiny::isolate(r())
  expect_identical(runs, 1L)
})

# --- reconcile_names --------------------------------------------------------

test_that("NULL selection means no restriction and stays NULL", {
  # NULL and character(0) mean opposite things throughout the app: "everything"
  # versus "nothing". Collapsing one into the other silently widens or empties
  # an export or a plot.
  got <- db_events$reconcile_names(NULL, c("a", "b"))
  expect_null(got$kept)
  expect_false(got$changed)
  expect_identical(got$dropped, character(0))
})

test_that("a fully surviving selection is unchanged", {
  got <- db_events$reconcile_names(c("b", "a"), c("a", "b", "c"))
  expect_identical(got$kept, c("b", "a"))
  expect_identical(got$dropped, character(0))
  expect_false(got$changed)
})

test_that("selection order is preserved rather than pool order", {
  got <- db_events$reconcile_names(c("c", "a"), c("a", "b", "c"))
  expect_identical(got$kept, c("c", "a"))
})

test_that("names missing from the pool are reported as dropped", {
  got <- db_events$reconcile_names(c("a", "gone", "b"), c("a", "b"))
  expect_identical(got$kept, c("a", "b"))
  expect_identical(got$dropped, "gone")
  expect_true(got$changed)
})

test_that("a selection wiped out entirely yields an empty kept vector", {
  # Not NULL: callers decide whether "nothing survived" means "restrict to
  # nothing" or "stop restricting", and they cannot tell the two apart if this
  # collapses to NULL on its own.
  got <- db_events$reconcile_names(c("x", "y"), c("a"))
  expect_identical(got$kept, character(0))
  expect_identical(got$dropped, c("x", "y"))
  expect_true(got$changed)
})

test_that("an empty pool drops everything", {
  got <- db_events$reconcile_names(c("a"), character(0))
  expect_identical(got$kept, character(0))
  expect_true(got$changed)
})

test_that("non-character selections and pools are compared as names", {
  got <- db_events$reconcile_names(c(1, 2), c("2", "3"))
  expect_identical(got$kept, "2")
  expect_identical(got$dropped, "1")
})

# --- internals --------------------------------------------------------------

test_that(".check returns the domains it validated", {
  expect_identical(impl$.check(c("amr", "schema")), c("amr", "schema"))
})
