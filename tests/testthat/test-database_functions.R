box::use(
  testthat[test_that, expect_null, expect_identical],
  withr[local_tempdir],
)
box::use(
  app / logic / database_functions[load_classical_mlst, remove_isolates],
  app / logic / field_labels[MLST_COL_PREFIX],
)

# A subset of the real pymlst `classical_mlst` schema: one row per (souche,
# locus), with the strain-level ST and status repeated across the isolate's
# rows.
add_classical_mlst <- function(path, rows) {
  con <- DBI::dbConnect(RSQLite::SQLite(), path)
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  DBI::dbExecute(
    con,
    "CREATE TABLE classical_mlst (
       id INTEGER PRIMARY KEY AUTOINCREMENT,
       souche TEXT, gene TEXT, allele TEXT, st TEXT, status TEXT)"
  )
  for (i in seq_len(nrow(rows))) {
    DBI::dbExecute(
      con,
      "INSERT INTO classical_mlst (souche, gene, allele, st, status)
         VALUES (?, ?, ?, ?, ?)",
      params = list(
        rows$souche[i],
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
      souche = c("A", "A", "B", "B"),
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
      souche = "A", gene = "adk", allele = "1", st = "7", status = "known",
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

test_that("removing an isolate drops its custom-variable values", {
  path <- file.path(withr::local_tempdir(), "db.db")
  build_db(path, default_local(), metadata = meta_df(c("A", "B")))
  seed_custom(
    path,
    list(ward = list(type = "text", values = c(A = "ICU", B = "ER")))
  )

  remove_isolates(path, "A")

  left <- qdf(path, "SELECT souche FROM phylotrace_custom_values")
  expect_identical(left$souche, "B")
})
