box::use(
  testthat[test_that, expect_null, expect_identical],
  withr[local_tempdir],
)
box::use(
  app / logic / database_functions[load_classical_mlst, remove_isolates],
  app / logic / field_labels[MLST_COL_PREFIX],
)

# A subset of the real pymlst `classical_mlst` schema: one row per (isolate,
# locus), with the strain-level ST and status repeated across the isolate's
# rows.
add_classical_mlst <- function(path, rows) {
  con <- DBI::dbConnect(RSQLite::SQLite(), path)
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  DBI::dbExecute(
    con,
    "CREATE TABLE classical_mlst (
       id INTEGER PRIMARY KEY AUTOINCREMENT,
       isolate TEXT, gene TEXT, allele TEXT, st TEXT, status TEXT)"
  )
  for (i in seq_len(nrow(rows))) {
    DBI::dbExecute(
      con,
      "INSERT INTO classical_mlst (isolate, gene, allele, st, status)
         VALUES (?, ?, ?, ?, ?)",
      params = list(
        rows$isolate[i],
        rows$gene[i],
        rows$allele[i],
        rows$st[i],
        rows$status[i]
      )
    )
  }
}

test_that("load_classical_mlst returns NULL without a classical_mlst table", {
  dir <- local_tempdir()
  db <- file.path(dir, "db.sqlite")
  build_db(db, default_local())
  expect_null(load_classical_mlst(db))
})

test_that("load_classical_mlst pivots to one row per isolate", {
  dir <- local_tempdir()
  db <- file.path(dir, "db.sqlite")
  build_db(db, default_local())

  add_classical_mlst(
    db,
    data.frame(
      isolate = c("A", "A", "B", "B"),
      gene = c("adk", "fumC", "adk", "fumC"),
      allele = c("1", "3", "2", "5"),
      # A is a known ST; B is a novel call with no registered ST.
      st = c("42", "42", NA, NA),
      status = c("known", "known", "novel", "novel"),
      stringsAsFactors = FALSE
    )
  )

  out <- load_classical_mlst(db)

  # One row per isolate, isolates and loci in first-typed order.
  expect_identical(out$isolate, c("A", "B"))
  expect_identical(
    names(out),
    c("isolate", paste0(MLST_COL_PREFIX, c("st", "adk", "fumC")))
  )
  # The novel isolate surfaces its status word in place of a missing ST.
  expect_identical(out[[paste0(MLST_COL_PREFIX, "st")]], c("42", "novel"))
  expect_identical(out[[paste0(MLST_COL_PREFIX, "adk")]], c("1", "2"))
  expect_identical(out[[paste0(MLST_COL_PREFIX, "fumC")]], c("3", "5"))
})

test_that("load_classical_mlst omits isolates absent from classical_mlst", {
  dir <- local_tempdir()
  db <- file.path(dir, "db.sqlite")
  build_db(db, default_local())
  add_classical_mlst(
    db,
    data.frame(
      isolate = "A", gene = "adk", allele = "1", st = "7", status = "known",
      stringsAsFactors = FALSE
    )
  )

  out <- load_classical_mlst(db)
  expect_identical(out$isolate, "A")

  # B has no classical typing, so a display-time match() by isolate yields NA
  # for it - exactly what the browse view fills in as an empty cell.
  idx <- match(c("A", "B"), out$isolate)
  expect_identical(out[[paste0(MLST_COL_PREFIX, "adk")]][idx], c("1", NA))
})

test_that("migrate_isolate_key renames souche and is idempotent", {
  # Databases written before the key was renamed carry `souche` on PhyloTrace's
  # own tables, which every query now misses with "no such column: isolate".
  dir <- local_tempdir()
  path <- file.path(dir, "old.db")
  build_db(path, default_local(), metadata = meta_df(c("A", "B")))

  con <- DBI::dbConnect(RSQLite::SQLite(), path)
  DBI::dbExecute(
    con,
    "CREATE TABLE amr_summary (
       id INTEGER PRIMARY KEY AUTOINCREMENT,
       souche TEXT, section TEXT, drug_class TEXT, genes TEXT, called_at TEXT)"
  )
  DBI::dbExecute(
    con,
    "INSERT INTO amr_summary (souche, section, drug_class, genes)
     VALUES ('A', 'matches', 'Beta-lactam', 'blaTEST')"
  )
  # Already-current table: must be left alone, not renamed twice.
  DBI::dbExecute(
    con,
    "CREATE TABLE genome_hashes (isolate TEXT PRIMARY KEY, genome_digest TEXT)"
  )
  DBI::dbDisconnect(con)

  renamed <- suppressMessages(migrate_isolate_key(path))
  expect_identical(renamed, "amr_summary")

  # The rename is metadata-only: the row survives, reachable under the new name.
  expect_identical(q1(path, "SELECT isolate FROM amr_summary"), "A")
  expect_identical(q1(path, "SELECT genes FROM amr_summary"), "blaTEST")

  # pyMLST's own table keeps its spelling.
  expect_true("souche" %in% DBI::dbListFields(
    DBI::dbConnect(RSQLite::SQLite(), path),
    "mlst"
  ))

  # Second run is a no-op.
  expect_identical(suppressMessages(migrate_isolate_key(path)), character(0))
})

test_that("migrate_isolate_key tolerates a missing or unreadable database", {
  dir <- local_tempdir()
  expect_identical(migrate_isolate_key(file.path(dir, "absent.db")), character(0))
  expect_identical(migrate_isolate_key(NULL), character(0))
  expect_identical(migrate_isolate_key(NA_character_), character(0))
})

test_that("removing an isolate drops its custom-variable values", {
  path <- file.path(withr::local_tempdir(), "db.db")
  build_db(path, default_local(), metadata = meta_df(c("A", "B")))
  seed_custom(
    path,
    list(ward = list(type = "text", values = c(A = "ICU", B = "ER")))
  )

  remove_isolates(path, "A")

  left <- qdf(path, "SELECT isolate FROM phylotrace_custom_values")
  expect_identical(left$isolate, "B")
})

test_that("removing an isolate drops its analysis-result rows too", {
  # classical_mlst / amr_results / amr_summary key on `isolate` with no foreign
  # key onto mlst, so nothing cascades. `isolate` is a name: leaving the rows
  # behind means a later isolate typed under the same name silently inherits the
  # removed one's ST and AMR calls.
  path <- file.path(withr::local_tempdir(), "db.db")
  build_db(path, default_local(), metadata = meta_df(c("A", "B")))
  seed_results(path, c("A", "B"))

  remove_isolates(path, "A")

  for (tbl in c("classical_mlst", "amr_results", "amr_summary")) {
    left <- qdf(path, sprintf("SELECT DISTINCT isolate FROM %s", tbl))
    expect_identical(left$isolate, "B")
  }
})

test_that("remove_isolates copes with a database missing the optional tables", {
  # A database that never ran classical MLST / AMR / custom variables must not
  # trip the prune loop.
  path <- file.path(withr::local_tempdir(), "db.db")
  build_db(path, default_local(), metadata = meta_df(c("A", "B")))

  expect_identical(remove_isolates(path, "A"), TRUE)
  expect_identical(q1(path, "SELECT COUNT(DISTINCT souche) FROM mlst"), 2L)
})
