box::use(
  jsonlite[toJSON],
  shiny[isolate, reactive, testServer],
  testthat[expect_identical, expect_null, expect_setequal, test_that],
  withr[local_tempdir],
)
box::use(
  app / logic / analysis_store,
  app / logic / db_events,
  app / logic / db_store,
  app / view / analysis_dashboard / group,
)

# A database with an Analysis whose recorded universe is the isolates that
# existed when it was set up.
fixture <- function(dir, universe = c("A", "B")) {
  path <- file.path(dir, "db.sqlite")
  build_db(path, default_local(), metadata = meta_df(c("A", "B")))
  analysis_store$ensure_schema(path)
  id <- analysis_store$add_analysis(
    path,
    "Analysis 1",
    isolate_universe = as.character(toJSON(universe))
  )
  list(path = path, id = id)
}

# Add an isolate to `mlst` the way a typing run does: allele calls only, with
# no metadata row and no revision bump.
type_isolate <- function(path, name) {
  con <- DBI::dbConnect(RSQLite::SQLite(), path)
  on.exit(DBI::dbDisconnect(con))
  DBI::dbExecute(
    con,
    sprintf(
      "INSERT INTO mlst (souche, gene, seqid)
         SELECT '%s', gene, seqid FROM mlst WHERE souche = 'A'",
      name
    )
  )
}

with_group <- function(fx, bus, expr) {
  store <- db_store$new_store(db_path = reactive(fx$path), db_rev = bus)
  testServer(
    group$server,
    args = list(
      analysis_id = fx$id,
      db_path = reactive(fx$path),
      db_rev = bus,
      store = store
    ),
    {
      session$flushReact()
      eval(expr)
    }
  )
}

test_that("current_isolates falls back to the whole pool", {
  dir <- local_tempdir()
  fx <- fixture(dir)
  with_group(fx, db_events$new_bus(), quote({
    expect_setequal(isolate(current_isolates()), c("A", "B"))
  }))
})

test_that("a fresh Analysis reports no universe drift", {
  dir <- local_tempdir()
  fx <- fixture(dir)
  with_group(fx, db_events$new_bus(), quote({
    expect_null(isolate(universe_drift()))
  }))
})

test_that("drift is reported after a typing run, which bumps isolates only", {
  # The dependency bug this guards: both reactives read the isolate pool but
  # declared `analyses` only. Typing finalize bumps isolates/metadata/schema
  # and never analyses, so the drift indicator - whose entire purpose is to
  # report that the pool moved - stayed silent through exactly the event it
  # exists to report.
  dir <- local_tempdir()
  fx <- fixture(dir)
  bus <- db_events$new_bus()

  with_group(fx, bus, quote({
    expect_null(isolate(universe_drift()))

    type_isolate(fx$path, "TYPED_SINCE")
    db_events$bump(bus, "isolates", "metadata", "schema")
    session$flushReact()

    drift <- isolate(universe_drift())
    expect_identical(drift$added, "TYPED_SINCE")
    expect_identical(drift$removed, character(0))
  }))
})

test_that("current_isolates follows the pool after a typing run too", {
  dir <- local_tempdir()
  fx <- fixture(dir)
  bus <- db_events$new_bus()

  with_group(fx, bus, quote({
    expect_setequal(isolate(current_isolates()), c("A", "B"))

    type_isolate(fx$path, "TYPED_SINCE")
    db_events$bump(bus, "isolates", "metadata", "schema")
    session$flushReact()

    expect_setequal(
      isolate(current_isolates()),
      c("A", "B", "TYPED_SINCE")
    )
  }))
})

test_that("an Analysis that restricts isolates ignores the pool entirely", {
  dir <- local_tempdir()
  path <- file.path(dir, "db.sqlite")
  build_db(path, default_local(), metadata = meta_df(c("A", "B")))
  analysis_store$ensure_schema(path)
  id <- analysis_store$add_analysis(
    path,
    "Restricted",
    isolate_selection = as.character(toJSON("A"))
  )
  bus <- db_events$new_bus()
  fx <- list(path = path, id = id)

  with_group(fx, bus, quote({
    expect_identical(isolate(current_isolates()), "A")

    type_isolate(fx$path, "TYPED_SINCE")
    db_events$bump(bus, "isolates")
    session$flushReact()

    # Still just its own fixed set - a new isolate does not join a restricted
    # Analysis.
    expect_identical(isolate(current_isolates()), "A")
  }))
})
