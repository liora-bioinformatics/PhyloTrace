box::use(
  testthat[
    expect_false,
    expect_identical,
    expect_null,
    expect_setequal,
    expect_true,
    test_that
  ],
  withr[local_tempdir],
)
box::use(
  app /
    logic /
    database_functions[
      load_classical_mlst,
      migrate_species_name,
      read_metadata_table,
      remove_isolates,
      save_metadata_table,
      sync_metadata_table,
      AMR_ABSENT,
      append_amr,
      append_amr_matrix
    ],
  app / logic / field_labels[amr_field_map, MLST_COL_PREFIX],
)

# An amr_summary shaped like abritamr's own output: two drug classes, one of
# them virulence, and two genes inside one class. `blank` is an isolate with no
# AMR rows at all — never screened, which is not the same as screened clean.
seed_amr_summary <- function(path, rows) {
  con <- DBI::dbConnect(RSQLite::SQLite(), path)
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  DBI::dbExecute(
    con,
    "CREATE TABLE IF NOT EXISTS amr_summary (
       id INTEGER PRIMARY KEY AUTOINCREMENT,
       isolate TEXT, section TEXT, drug_class TEXT, genes TEXT, called_at TEXT)"
  )
  for (i in seq_len(nrow(rows))) {
    DBI::dbExecute(
      con,
      "INSERT INTO amr_summary (isolate, section, drug_class, genes)
         VALUES (?, ?, ?, ?)",
      params = as.list(unname(rows[i, ]))
    )
  }
  invisible(path)
}

amr_db <- function(dir) {
  path <- file.path(dir, "amr.db")
  seed_amr_summary(path, data.frame(
    isolate = c("A", "A", "A", "B"),
    section = c("matches", "matches", "virulence", "matches"),
    drug_class = c("Beta-lactam", "Beta-lactam", "Metal", "Beta-lactam"),
    genes = c("blaOXA-2", "blaPDC-5*", "merA", "blaOXA-2"),
    stringsAsFactors = FALSE
  ))
  path
}

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

test_that("migrate_species_name repairs the mangled scheme species", {
  # pyMLST's lstrip("Species") eats the leading "p" of "pneumoniae" on its way
  # into mlst_type - the name classical MLST lookup and AMR species mapping
  # both read.
  dir <- local_tempdir()
  path <- file.path(dir, "kleb.db")
  build_db(
    path,
    default_local(),
    species = "Klebsiella neumoniae/variicola/quasipneumoniae"
  )

  expect_identical(
    migrate_species_name(path),
    "Klebsiella pneumoniae/variicola/quasipneumoniae"
  )
  expect_identical(
    q1(path, "SELECT species FROM mlst_type"),
    "Klebsiella pneumoniae/variicola/quasipneumoniae"
  )
  # Nothing left to repair on a second pass.
  expect_identical(migrate_species_name(path), NA_character_)
})

test_that("migrate_species_name leaves unknown species and broken paths alone", {
  dir <- local_tempdir()
  path <- file.path(dir, "other.db")
  build_db(path, default_local(), species = "Testus organismus")

  expect_identical(migrate_species_name(path), NA_character_)
  expect_identical(q1(path, "SELECT species FROM mlst_type"), "Testus organismus")
  expect_identical(migrate_species_name(file.path(dir, "absent.db")), NA_character_)
  expect_identical(migrate_species_name(NULL), NA_character_)
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

# A database carrying every isolate-keyed table at once, so a removal can be
# checked across the whole schema rather than one table at a time.
full_db <- function(dir, isolates = c("A", "B")) {
  path <- file.path(dir, "full.db")
  build_db(path, default_local(), metadata = meta_df(isolates))
  seed_results(path, isolates)
  seed_custom(
    path,
    list(ward = list(type = "text", values = c(A = "ICU", B = "ER")))
  )
  con <- DBI::dbConnect(RSQLite::SQLite(), path)
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  DBI::dbExecute(
    con,
    "CREATE TABLE genome_hashes (
       isolate TEXT PRIMARY KEY, genome_digest TEXT NOT NULL,
       file_sha256 TEXT, algorithm TEXT NOT NULL, n_contigs INTEGER,
       total_length INTEGER, file_bytes INTEGER, hashed_at TEXT)"
  )
  for (s in isolates) {
    DBI::dbExecute(
      con,
      "INSERT INTO genome_hashes (isolate, genome_digest, algorithm)
       VALUES (?, ?, 'ga4gh-sorted-sequences-v1')",
      list(s, paste0("digest-", s))
    )
  }
  path
}

# Every place an isolate name is stored. `mlst` is queried by `souche` because
# that is pyMLST's spelling; everything else is PhyloTrace's own and says
# `isolate`. That split is the point of the test.
isolate_footprint <- function(path, name) {
  c(
    mlst = q1(path, "SELECT COUNT(*) FROM mlst WHERE souche = ?", list(name)),
    metadata = q1(
      path, "SELECT COUNT(*) FROM metadata WHERE isolate = ?", list(name)
    ),
    classical_mlst = q1(
      path, "SELECT COUNT(*) FROM classical_mlst WHERE isolate = ?", list(name)
    ),
    amr_results = q1(
      path, "SELECT COUNT(*) FROM amr_results WHERE isolate = ?", list(name)
    ),
    amr_summary = q1(
      path, "SELECT COUNT(*) FROM amr_summary WHERE isolate = ?", list(name)
    ),
    custom = q1(
      path,
      "SELECT COUNT(*) FROM phylotrace_custom_values WHERE isolate = ?",
      list(name)
    ),
    genome_hashes = q1(
      path, "SELECT COUNT(*) FROM genome_hashes WHERE isolate = ?", list(name)
    )
  )
}

test_that("remove_isolates clears an isolate from every table that stores it", {
  dir <- local_tempdir()
  path <- full_db(dir)

  # Precondition: A really is present everywhere, or the test proves nothing.
  expect_true(all(isolate_footprint(path, "A") > 0L))

  expect_true(remove_isolates(path, "A"))

  expect_identical(
    unname(isolate_footprint(path, "A")),
    rep(0L, 7)
  )
  # ...and B is untouched in all of them.
  expect_true(all(isolate_footprint(path, "B") > 0L))
})

test_that("remove_isolates never deletes the scheme reference", {
  # `ref` is not an isolate: its mlst rows *are* the scheme. Removing it would
  # leave a file with isolates but no scheme to compare them against.
  dir <- local_tempdir()
  path <- full_db(dir)
  ref_rows <- q1(path, "SELECT COUNT(*) FROM mlst WHERE souche = 'ref'")
  expect_true(ref_rows > 0L)

  # Asked for on its own: nothing to do.
  expect_false(remove_isolates(path, "ref"))
  expect_identical(q1(path, "SELECT COUNT(*) FROM mlst WHERE souche = 'ref'"), ref_rows)

  # ...and smuggled in alongside a real isolate: A goes, ref stays.
  expect_true(remove_isolates(path, c("A", "ref")))
  expect_identical(q1(path, "SELECT COUNT(*) FROM mlst WHERE souche = 'ref'"), ref_rows)
  expect_identical(q1(path, "SELECT COUNT(*) FROM mlst WHERE souche = 'A'"), 0L)
})

test_that("remove_isolates handles multiple names and ignores unknown ones", {
  dir <- local_tempdir()
  path <- full_db(dir)

  # An unknown name is a harmless no-op alongside a real one.
  expect_true(remove_isolates(path, c("A", "never_typed")))
  expect_identical(unname(isolate_footprint(path, "A")), rep(0L, 7))
  expect_true(all(isolate_footprint(path, "B") > 0L))

  # Duplicates in the request collapse rather than double-deleting.
  expect_true(remove_isolates(path, c("B", "B")))
  expect_identical(unname(isolate_footprint(path, "B")), rep(0L, 7))
  expect_identical(q1(path, "SELECT COUNT(DISTINCT souche) FROM mlst"), 1L)
})

test_that("remove_isolates is atomic: a mid-way failure leaves nothing removed", {
  # The isolate key is a *name*, so a half-applied removal is worse than none:
  # the next isolate typed under that name inherits whatever survived. Make one
  # of the later deletes impossible and check the earlier ones were rolled back.
  dir <- local_tempdir()
  path <- full_db(dir)
  before <- isolate_footprint(path, "A")

  con <- DBI::dbConnect(RSQLite::SQLite(), path)
  # A trigger that always raises turns the amr_summary delete - which runs after
  # mlst and metadata - into a failure.
  DBI::dbExecute(
    con,
    "CREATE TRIGGER boom BEFORE DELETE ON amr_summary
     BEGIN SELECT RAISE(ABORT, 'boom'); END"
  )
  DBI::dbDisconnect(con)

  expect_false(remove_isolates(path, "A"))

  # Nothing removed, including from the tables visited before the failure.
  expect_identical(isolate_footprint(path, "A"), before)
})

# ---------------------------------------------------------------------------
# read_metadata_table(): the pure read half of the old make_metadata_table()
# ---------------------------------------------------------------------------

test_that("read_metadata_table returns NULL without a metadata table", {
  dir <- local_tempdir()
  path <- file.path(dir, "db.sqlite")
  build_db(path, default_local(), metadata = NULL)
  expect_null(read_metadata_table(path))
})

test_that("read_metadata_table never creates the table it did not find", {
  dir <- local_tempdir()
  path <- file.path(dir, "db.sqlite")
  build_db(path, default_local(), metadata = NULL)
  read_metadata_table(path)
  tables <- qdf(path, "SELECT name FROM sqlite_master WHERE type='table'")$name
  expect_false("metadata" %in% tables)
})

test_that("read_metadata_table does not backfill isolates mlst already has", {
  # The defining difference from sync_metadata_table(): mlst has A and B, the
  # metadata table only has A. A pure read must report exactly what the table
  # holds, not what the isolate pool implies it should hold.
  dir <- local_tempdir()
  path <- file.path(dir, "db.sqlite")
  build_db(path, default_local(), metadata = meta_df("A"))
  md <- read_metadata_table(path)
  expect_identical(md$isolate, "A")
})

# ---------------------------------------------------------------------------
# save_metadata_table(): a tidy isolate/column/value diff, one UPDATE per
# cell - not a whole-table read-diff-overwrite.
# ---------------------------------------------------------------------------

# One row per cell, matching what database_browser.R's State$metadata_dirty
# accumulates from individual DT cell edits.
cell_edit <- function(isolate, column, value) {
  data.frame(isolate = isolate, column = column, value = value, stringsAsFactors = FALSE)
}

test_that("save_metadata_table writes exactly the named cell", {
  dir <- local_tempdir()
  path <- file.path(dir, "db.sqlite")
  build_db(path, default_local(), metadata = meta_df(c("A", "B")))
  before <- read_metadata_table(path)

  n <- save_metadata_table(
    path,
    cell_edit("A", "geo_loc_name_country", "France")
  )
  expect_identical(n, 1L)

  after <- read_metadata_table(path)
  expect_identical(after$geo_loc_name_country[after$isolate == "A"], "France")
  # Every other cell, including B's whole row, is untouched.
  a <- after$isolate == "A"
  after$geo_loc_name_country[a] <- before$geo_loc_name_country[a]
  expect_identical(after, before)
})

test_that("an isolate named in no edit is left completely alone", {
  # The actual fix: the old dbWriteTable(overwrite = TRUE) replaced the whole
  # table with whatever full snapshot was passed in, so an isolate simply not
  # present in it - typed by someone else after the grid was read, or never
  # displayed in the first place - was deleted outright. A tidy diff never
  # names such an isolate at all, so there is nothing for it to delete.
  dir <- local_tempdir()
  path <- file.path(dir, "db.sqlite")
  build_db(path, default_local(), metadata = meta_df(c("A", "B")))
  before_b <- read_metadata_table(path)
  before_b <- before_b[before_b$isolate == "B", ]

  save_metadata_table(path, cell_edit("A", "geo_loc_name_country", "France"))

  after <- read_metadata_table(path)
  expect_setequal(after$isolate, c("A", "B"))
  expect_identical(after[after$isolate == "B", ], before_b)
})

test_that("two sessions editing different isolates do not clobber each other", {
  # The concurrency scenario the tidy-diff design exists for: two open
  # sessions, each editing a different isolate, each unaware of the other.
  # A whole-grid diff (comparing a full stale snapshot against the database)
  # cannot tell "the user changed this" apart from "someone else changed this
  # since I read it" - a tidy diff carries only what its own session actually
  # edited, so the question never arises.
  dir <- local_tempdir()
  path <- file.path(dir, "db.sqlite")
  build_db(path, default_local(), metadata = meta_df(c("A", "B")))

  save_metadata_table(path, cell_edit("A", "geo_loc_name_country", "France"))
  save_metadata_table(path, cell_edit("B", "geo_loc_name_country", "Spain"))

  final <- read_metadata_table(path)
  expect_identical(final$geo_loc_name_country[final$isolate == "A"], "France")
  expect_identical(final$geo_loc_name_country[final$isolate == "B"], "Spain")
})

test_that("two sessions editing the same cell resolve as last-write-wins", {
  # The one case a database with no per-cell locking cannot do better than -
  # documented as the accepted limit in save_metadata_table()'s own docs.
  dir <- local_tempdir()
  path <- file.path(dir, "db.sqlite")
  build_db(path, default_local(), metadata = meta_df("A"))

  save_metadata_table(path, cell_edit("A", "geo_loc_name_country", "France"))
  save_metadata_table(path, cell_edit("A", "geo_loc_name_country", "Spain"))

  expect_identical(read_metadata_table(path)$geo_loc_name_country, "Spain")
})

test_that("an isolate named in an edit but absent from the database is skipped", {
  dir <- local_tempdir()
  path <- file.path(dir, "db.sqlite")
  build_db(path, default_local(), metadata = meta_df("A"))

  n <- save_metadata_table(
    path,
    rbind(
      cell_edit("A", "geo_loc_name_country", "France"),
      cell_edit("ghost", "geo_loc_name_country", "Nowhere")
    )
  )
  expect_identical(n, 1L)
  expect_identical(read_metadata_table(path)$isolate, "A")
})

test_that("a column absent from the database table is skipped", {
  dir <- local_tempdir()
  path <- file.path(dir, "db.sqlite")
  build_db(path, default_local(), metadata = meta_df("A"))

  n <- save_metadata_table(path, cell_edit("A", "mlst_st", "42"))
  expect_identical(n, 0L)
  expect_false("mlst_st" %in% names(read_metadata_table(path)))
})

test_that("NA overwrites an existing value", {
  dir <- local_tempdir()
  path <- file.path(dir, "db.sqlite")
  build_db(path, default_local(), metadata = meta_df("A"))

  n <- save_metadata_table(
    path,
    cell_edit("A", "geo_loc_name_country", NA_character_)
  )
  expect_identical(n, 1L)
  expect_true(is.na(read_metadata_table(path)$geo_loc_name_country))
})

test_that("save_metadata_table never creates the table, unlike the old overwrite", {
  # dbWriteTable(overwrite = TRUE) would have created the table if it did not
  # exist; a pure updater must not. Table creation belongs to
  # sync_metadata_table() alone.
  dir <- local_tempdir()
  path <- file.path(dir, "db.sqlite")
  build_db(path, default_local(), metadata = NULL)

  n <- save_metadata_table(path, cell_edit("A", "geo_loc_name_country", "France"))
  expect_identical(n, 0L)
  tables <- qdf(path, "SELECT name FROM sqlite_master WHERE type='table'")$name
  expect_false("metadata" %in% tables)
})

test_that("save_metadata_table tolerates empty or malformed input", {
  dir <- local_tempdir()
  path <- file.path(dir, "db.sqlite")
  build_db(path, default_local(), metadata = meta_df("A"))

  expect_identical(save_metadata_table(path, data.frame()), 0L)
  expect_identical(
    save_metadata_table(path, data.frame(isolate = character(0))),
    0L
  )
  # Missing the `column` / `value` shape entirely.
  expect_identical(
    save_metadata_table(path, data.frame(isolate = "A", country = "France")),
    0L
  )
})

test_that("save_metadata_table is atomic: a mid-way failure writes nothing", {
  dir <- local_tempdir()
  path <- file.path(dir, "db.sqlite")
  build_db(path, default_local(), metadata = meta_df(c("A", "B")))
  before <- read_metadata_table(path)

  con <- DBI::dbConnect(RSQLite::SQLite(), path)
  # Fires only on B's row, so A's UPDATE (first in the edits data frame) has
  # already run by the time this aborts the transaction.
  DBI::dbExecute(
    con,
    "CREATE TRIGGER boom BEFORE UPDATE ON metadata
     WHEN NEW.isolate = 'B'
     BEGIN SELECT RAISE(ABORT, 'boom'); END"
  )
  DBI::dbDisconnect(con)

  n <- save_metadata_table(
    path,
    rbind(
      cell_edit("A", "geo_loc_name_country", "France"),
      cell_edit("B", "geo_loc_name_country", "Spain")
    )
  )

  expect_identical(n, 0L)
  expect_identical(read_metadata_table(path), before)
})

# --- AMR columns -------------------------------------------------------------

test_that("AMR arrives at both levels, and the profile summary is gone", {
  path <- amr_db(local_tempdir())
  meta <- data.frame(isolate = c("A", "B"), stringsAsFactors = FALSE)
  out <- append_amr_matrix(append_amr(meta, path), path)

  map <- amr_field_map(out)
  # One column per drug class, and one per gene inside it.
  expect_setequal(map$field[map$role == "class"], c("amr_Beta-lactam", "amr_Metal"))
  expect_setequal(map$gene[map$role == "gene"], c("blaOXA-2", "blaPDC-5", "merA"))
  # The comma-joined summary of every class is no longer built at all.
  expect_false("amr_profile" %in% names(out))

  # Running both appenders keeps both sets: each records what it added under the
  # same attribute, and the second used to overwrite the first.
  expect_setequal(attr(out, "amr_cols"), map$field)
})

test_that("a class column holds the isolate's genes, a gene column the call", {
  path <- amr_db(local_tempdir())
  meta <- data.frame(isolate = c("A", "B"), stringsAsFactors = FALSE)
  out <- append_amr_matrix(append_amr(meta, path), path)
  map <- amr_field_map(out)

  expect_identical(out$`amr_Beta-lactam`, c("blaOXA-2, blaPDC-5*", "blaOXA-2"))
  # abritamr's own quality flag comes off the name and becomes the call state.
  inexact <- map$field[!is.na(map$gene) & map$gene == "blaPDC-5"]
  expect_identical(as.character(out[[inexact]]), c("Inexact", NA))
})

test_that("a screened isolate that carries nothing says so; an unscreened one does not", {
  # Both reach a plot as NA and only one of them is an answer: "the screen ran
  # and found nothing" against "this isolate was never screened".
  path <- amr_db(local_tempdir())
  meta <- data.frame(isolate = c("A", "B", "C"), stringsAsFactors = FALSE)

  raw <- append_amr_matrix(append_amr(meta, path), path)
  filled <- append_amr_matrix(
    append_amr(meta, path, absent = AMR_ABSENT), path, absent = AMR_ABSENT
  )
  map <- amr_field_map(filled)
  gene <- map$field[!is.na(map$gene) & map$gene == "merA"]

  # The browser keeps the NA it draws an empty cell from.
  expect_true(is.na(raw[[gene]][[2]]))
  # B was screened and has no merA; C was never screened at all.
  expect_identical(as.character(filled[[gene]]), c("Match", AMR_ABSENT, NA))
  expect_identical(filled$amr_Metal, c("merA", AMR_ABSENT, NA))
})
