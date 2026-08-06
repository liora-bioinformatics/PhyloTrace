box::use(
  shiny[isolate, reactive, reactiveVal],
  testthat[
    expect_identical,
    expect_true,
    test_that
  ],
  withr[local_tempdir],
)
box::use(
  app / logic / db_events,
  app / logic / db_store,
)

test_that("metadata() is req()'d away without a database path", {
  # req() stops reactive execution rather than returning a value - a real
  # error outside of Shiny's own render loop, where it is normally the thing
  # that silently suppresses the output. Every actual caller (Visualization,
  # the Database Browser, ...) already guards with its own req(db_path())
  # before ever reaching the store, so this only exercises the store's own
  # defensive copy of that guard.
  store <- db_store$new_store()
  err <- tryCatch(
    { isolate(store$metadata()); NULL },
    error = function(e) e
  )
  expect_true(inherits(err, "shiny.silent.error"))
})

test_that("metadata() reads the loaded database's metadata table", {
  dir <- local_tempdir()
  path <- file.path(dir, "db.sqlite")
  build_db(path, default_local(), metadata = meta_df(c("A", "B")))

  store <- db_store$new_store(db_path = reactive(path))
  isolate(expect_identical(store$metadata()$isolate, c("A", "B")))
})

test_that("metadata() re-reads when the isolates domain is bumped", {
  dir <- local_tempdir()
  path <- file.path(dir, "db.sqlite")
  build_db(path, default_local(), metadata = meta_df("A"))

  bus <- db_events$new_bus()
  store <- db_store$new_store(db_path = reactive(path), db_rev = bus)
  isolate(expect_identical(store$metadata()$isolate, "A"))

  # A row appears on disk without db_path() changing - the same shape as
  # typing or a merge adding an isolate mid-session.
  con <- DBI::dbConnect(RSQLite::SQLite(), path)
  DBI::dbExecute(
    con,
    "INSERT INTO metadata (isolate, organism) VALUES ('B', 'Testus organismus')"
  )
  DBI::dbDisconnect(con)

  db_events$bump(bus, "isolates")
  isolate(expect_identical(sort(store$metadata()$isolate), c("A", "B")))
})

test_that("metadata() re-reads when the metadata domain is bumped", {
  dir <- local_tempdir()
  path <- file.path(dir, "db.sqlite")
  build_db(path, default_local(), metadata = meta_df("A"))

  bus <- db_events$new_bus()
  store <- db_store$new_store(db_path = reactive(path), db_rev = bus)
  isolate(store$metadata())

  con <- DBI::dbConnect(RSQLite::SQLite(), path)
  DBI::dbExecute(
    con,
    "UPDATE metadata SET geo_loc_name_country = 'France' WHERE isolate = 'A'"
  )
  DBI::dbDisconnect(con)

  db_events$bump(bus, "metadata")
  isolate(
    expect_identical(store$metadata()$geo_loc_name_country, "France")
  )
})

test_that("metadata() does not re-read on an unrelated domain", {
  # This is the property the store exists for: exactly one dependency list to
  # get right, checked once, here - rather than trusting every one of its
  # consumers (the Database Browser, Visualization, the Dashboard, Export) to
  # each declare the same two domains correctly on their own.
  dir <- local_tempdir()
  path <- file.path(dir, "db.sqlite")
  build_db(path, default_local(), metadata = meta_df("A"))

  bus <- db_events$new_bus()
  store <- db_store$new_store(db_path = reactive(path), db_rev = bus)

  runs <- 0L
  r <- reactive({
    store$metadata()
    runs <<- runs + 1L
  })
  isolate(r())
  expect_identical(runs, 1L)

  db_events$bump(bus, "amr", "staged", "analyses", "custom_fields", "schema")
  isolate(r())
  expect_identical(runs, 1L)
})

test_that("a fresh database path is picked up without a bump", {
  # db_path() changing invalidates on its own, the ordinary Shiny way - the
  # store adds no extra gate on top of that.
  dir <- local_tempdir()
  first <- file.path(dir, "first.sqlite")
  second <- file.path(dir, "second.sqlite")
  build_db(first, default_local(), metadata = meta_df("A"))
  build_db(second, default_local(), metadata = meta_df("Z"))

  path <- reactiveVal(first)
  store <- db_store$new_store(db_path = path)
  isolate(expect_identical(store$metadata()$isolate, "A"))

  path(second)
  isolate(expect_identical(store$metadata()$isolate, "Z"))
})
