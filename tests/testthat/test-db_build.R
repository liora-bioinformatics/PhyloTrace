box::use(
  DBI[dbConnect, dbDisconnect, dbGetQuery, dbListTables],
  processx[process],
  RSQLite[SQLite],
  testthat[expect_error, expect_false, expect_identical, expect_true, test_that],
  withr[local_tempdir],
)
box::use(
  app / logic / db_build,
)

# A setup plan for the miniature database the test helper builds (loci g1-g3).
toy_plan <- function(target) {
  list(
    species = "Testus organismus/secundus",
    taxid = 42L,
    target = target,
    amr_organism = "Escherichia",
    cg = list(
      loci = data.frame(locus = c("g1", "g2", "g3", "g4"), sequence = "ATGAAATAA"),
      targets = data.frame(
        Locus = c("g1", "g2", "g3", "g4"),
        Gene = c("abcA", NA, NA, NA),
        Start = NA_character_,
        Length = NA_character_,
        Product = NA_character_
      ),
      info = list(
        name = "Toy cgMLST",
        source = "own",
        source_label = "Own / in-house scheme",
        version = "1.0",
        url = "https://example.org/scheme",
        cluster_distance = 7,
        citation = "A <b>paper</b>"
      )
    ),
    mlst = NULL
  )
}

# A build whose pyMLST step "succeeded": the partial file is a pyMLST
# reference (from the test helper) and the process exited with `status`.
fake_build <- function(target, status = 0L) {
  partial <- db_build$partial_path(target)
  build_db(partial, list(ref = ref_alleles()))
  proc <- process$new("sh", c("-c", sprintf("exit %d", status)))
  proc$wait()
  list(
    proc = proc,
    target = target,
    partial = partial,
    coregene = tempfile(),
    log = tempfile()
  )
}

test_that("overview rows keep the keys every reader expects, as plain text", {
  rows <- db_build$overview_rows(toy_plan("x.db"), 3L)
  value <- function(key) rows$value[rows$key == key]
  expect_identical(value("Genus"), "Testus")
  expect_identical(value("Species"), "organismus/secundus")
  expect_identical(value("Locus Count"), "3")
  expect_identical(value("Complex Type Distance"), "7")
  expect_identical(value("Publication"), "A <b>paper</b>")
  expect_false("Seed Genome" %in% rows$key)
})

test_that("a finished build lands under its name with scheme, annotation and settings", {
  dir <- local_tempdir()
  target <- file.path(dir, "new.db")
  plan <- toy_plan(target)
  build <- fake_build(target)

  res <- db_build$finish_db_build(build, plan)

  expect_identical(res$path, target)
  expect_identical(res$loci, 3L)
  expect_identical(res$skipped, "g4")
  expect_true(file.exists(target))
  expect_false(file.exists(build$partial))

  con <- dbConnect(SQLite(), target)
  on.exit(dbDisconnect(con))
  type <- dbGetQuery(con, "SELECT source, species FROM mlst_type")
  expect_identical(type$source, "Own / in-house scheme")
  expect_identical(type$species, "Testus organismus/secundus")
  # Only loci pyMLST accepted are annotated.
  expect_identical(dbGetQuery(con, "SELECT Locus FROM targets")$Locus, c("g1", "g2", "g3"))
  meta <- dbGetQuery(con, "SELECT key, value FROM phylotrace_meta")
  expect_identical(meta$value[meta$key == "amr_organism"], "Escherichia")
  expect_identical(meta$value[meta$key == "species_taxid"], "42")
  expect_true("uuid" %in% meta$key)
  expect_false(any(grepl("^cla_scheme", dbListTables(con))))
})

test_that("a failed pyMLST step leaves nothing behind", {
  dir <- local_tempdir()
  target <- file.path(dir, "new.db")
  build <- fake_build(target, status = 1L)
  writeLines("Error: Duplicated sequence for gene g2", build$log)

  expect_error(db_build$finish_db_build(build, toy_plan(target)), "Duplicated sequence")
  expect_false(file.exists(target))
  expect_false(file.exists(build$partial))
})

test_that("a file appearing at the target is never overwritten", {
  dir <- local_tempdir()
  target <- file.path(dir, "new.db")
  build <- fake_build(target)
  writeLines("somebody else's file", target)

  expect_error(db_build$finish_db_build(build, toy_plan(target)), "appeared")
  expect_identical(readLines(target), "somebody else's file")
  expect_error(db_build$start_db_build(toy_plan(target)), "already exists")
})
