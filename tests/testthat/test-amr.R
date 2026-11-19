box::use(
  DBI[dbConnect, dbDisconnect, dbExecute, dbGetQuery],
  RSQLite[SQLite],
  testthat[expect_identical, expect_true, test_that],
  withr[local_tempdir],
)
box::use(
  app / logic / amr[AMR_ORGANISM_KEY, amr_species, db_amr_organism, store_amr_results],
)

test_that("amr_species maps a plain binomial to its abritamr token", {
  expect_identical(amr_species("Staphylococcus aureus"), "Staphylococcus_aureus")
  expect_identical(amr_species("Acinetobacter baumannii"), "Acinetobacter_baumannii")
})

test_that("amr_species reads every species a cgMLST scheme covers", {
  # Scheme names carry the whole complex, so the epithet is not the last word:
  # taking "pneumoniae/variicola/quasipneumoniae" whole matched nothing and
  # point-mutation screening was silently skipped.
  expect_identical(
    amr_species("Klebsiella pneumoniae/variicola/quasipneumoniae"),
    "Klebsiella_pneumoniae"
  )
  expect_identical(
    amr_species("Klebsiella oxytoca/grimontii/michiganensis/pasteurii"),
    "Klebsiella_oxytoca"
  )
})

test_that("amr_species falls back to a genus-level token", {
  expect_identical(amr_species("Escherichia coli"), "Escherichia")
  expect_identical(amr_species("Salmonella enterica"), "Salmonella")
})

test_that("amr_species returns NA for anything abritamr does not support", {
  expect_identical(amr_species("Testus organismus"), NA_character_)
  expect_identical(amr_species(NA_character_), NA_character_)
  expect_identical(amr_species(""), NA_character_)
  expect_identical(amr_species(NULL), NA_character_)
})

test_that("re-storing a strain's AMR screen replaces its rows", {
  # Results are written per isolate while the run is live and swept again at the
  # end, so the same screen can be stored twice - it must not accumulate.
  dir <- local_tempdir()
  db <- file.path(dir, "db.sqlite")
  con <- dbConnect(SQLite(), db)
  dbExecute(con, "CREATE TABLE mlst_type (name TEXT)")
  dbDisconnect(con)

  amr_dir <- file.path(dir, "strainA")
  dir.create(amr_dir)
  writeLines(
    c(
      paste(
        c(
          "Protein identifier", "Contig id", "Start", "Stop", "Strand",
          "Gene symbol", "Sequence name", "Scope", "Element type",
          "Element subtype", "Class", "Subclass", "Method", "Target length",
          "Reference sequence length", "% Coverage of reference sequence",
          "% Identity to reference sequence", "Alignment length",
          "Accession of closest sequence", "Name of closest sequence"
        ),
        collapse = "\t"
      ),
      paste(
        c(
          "NA", "contig1", "100", "900", "+", "blaTEST",
          "beta-lactamase TEST", "core", "AMR", "AMR", "BETA-LACTAM",
          "CEPHALOSPORIN", "BLASTX", "266", "266", "100.00", "99.62", "266",
          "WP_000027057.1", "beta-lactamase TEST"
        ),
        collapse = "\t"
      )
    ),
    file.path(amr_dir, "amrfinder.out")
  )

  count <- function() {
    con <- dbConnect(SQLite(), db)
    on.exit(dbDisconnect(con), add = TRUE)
    dbGetQuery(
      con,
      "SELECT COUNT(*) AS n FROM amr_results WHERE isolate = 'strainA'"
    )$n
  }

  expect_true(store_amr_results(db, "strainA", amr_dir))
  expect_identical(count(), 1L)
  expect_true(store_amr_results(db, "strainA", amr_dir))
  expect_identical(count(), 1L)
})

test_that("db_amr_organism reads the organism chosen at setup, nothing else", {
  db <- file.path(local_tempdir(), "db.sqlite")
  con <- dbConnect(SQLite(), db)
  dbExecute(con, "CREATE TABLE phylotrace_meta (key TEXT PRIMARY KEY, value TEXT)")
  key <- AMR_ORGANISM_KEY
  set_organism <- function(value) {
    dbExecute(
      con,
      "INSERT OR REPLACE INTO phylotrace_meta (key, value) VALUES (?, ?)",
      params = list(key, value)
    )
  }

  set_organism("Staphylococcus_aureus")
  expect_identical(db_amr_organism(db), "Staphylococcus_aureus")
  # Empty means acquired genes only; a token abritamr does not know is ignored.
  set_organism("")
  expect_identical(db_amr_organism(db), NA_character_)
  set_organism("Testus_organismus")
  expect_identical(db_amr_organism(db), NA_character_)
  dbDisconnect(con)

  # No setting at all is acquired genes only, not a guess from the species.
  bare <- file.path(local_tempdir(), "bare.sqlite")
  dbDisconnect(dbConnect(SQLite(), bare))
  expect_identical(db_amr_organism(bare), NA_character_)
})
