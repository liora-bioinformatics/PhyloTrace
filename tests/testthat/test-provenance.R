box::use(
  testthat[expect_false, expect_identical, expect_true, test_that],
  withr[local_tempdir],
)
box::use(
  app / logic / provenance,
)

impl <- attr(provenance, "namespace")

# The scheme_overview table as pyMLST/cgmlst.org leaves it: long format, counts
# written with thousands separators.
OVERVIEW <- data.frame(
  key = c(
    "Name", "Database", "Version", "Seed Genome", "Genus", "Species",
    "Locus Count", "Complex Type Distance", "Complex Type Count"
  ),
  value = c(
    "K. pneumoniae sensu lato cgMLST",
    "cgMLST.org Nomenclature Server (h25)",
    "1.0",
    "NTUH-K2044 (NC_012731.1, 15-JUN-2016)",
    "Klebsiella",
    "pneumoniae/variicola/quasipneumoniae",
    "2,358",
    "15",
    "23,933"
  ),
  stringsAsFactors = FALSE
)

new_db <- function(dir, name = "db.sqlite", overview = NULL) {
  path <- file.path(dir, name)
  con <- DBI::dbConnect(RSQLite::SQLite(), path)
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  DBI::dbExecute(con, "CREATE TABLE mlst_type (name TEXT)")
  if (!is.null(overview)) {
    DBI::dbWriteTable(con, "scheme_overview", overview)
  }
  path
}

row_of <- function(path, isolate) {
  con <- DBI::dbConnect(RSQLite::SQLite(), path)
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  DBI::dbGetQuery(
    con,
    "SELECT * FROM typing_provenance WHERE isolate = ?",
    params = list(isolate)
  )
}

test_that("overview_value reads keys and parses formatted counts", {
  expect_identical(provenance$overview_value(OVERVIEW, "Genus"), "Klebsiella")
  expect_identical(
    provenance$overview_value(OVERVIEW, "Locus Count", numeric = TRUE),
    2358
  )
  expect_identical(
    provenance$overview_value(OVERVIEW, "Complex Type Count", numeric = TRUE),
    23933
  )
  expect_identical(provenance$overview_value(OVERVIEW, "Absent"), NA_character_)
  expect_identical(provenance$overview_value(NULL, "Genus"), NA_character_)
})

test_that("scheme_provenance reads the database's scheme context", {
  dir <- local_tempdir()
  path <- new_db(dir, overview = OVERVIEW)

  context <- provenance$scheme_provenance(path)
  expect_identical(context$cg_scheme_database, "cgMLST.org Nomenclature Server (h25)")
  expect_identical(context$cg_scheme_version, "1.0")
  expect_identical(context$cg_genus, "Klebsiella")
  expect_identical(context$cg_species, "pneumoniae/variicola/quasipneumoniae")
  expect_identical(context$cg_locus_count, 2358)
  expect_identical(context$cg_complex_type_distance, 15)
})

test_that("scheme_provenance tolerates a database without an overview", {
  dir <- local_tempdir()
  context <- provenance$scheme_provenance(new_db(dir))
  expect_true(all(vapply(context, function(x) is.na(x), logical(1))))
  expect_true(all(vapply(
    provenance$scheme_provenance(file.path(dir, "absent.db")),
    function(x) is.na(x),
    logical(1)
  )))
})

test_that("a provenance row grows step by step without losing earlier fields", {
  dir <- local_tempdir()
  path <- new_db(dir)

  # Allele calling reports first: assembly and scheme context.
  expect_true(provenance$store_provenance(path, "A", list(
    run_id = "typing_20260805_090000_ab.log",
    phylotrace_version = "1.6.1",
    genome_digest = "abc123",
    cg_locus_count = 2358,
    cg_identity = 0.95
  )))
  first <- row_of(path, "A")
  expect_identical(first$genome_digest, "abc123")

  # The classical MLST search follows, then the AMR screen.
  provenance$store_provenance(path, "A", list(cla_scheme = "Klebsiella (MLST)"))
  provenance$store_provenance(path, "A", list(amr_elements = 4L))

  final <- row_of(path, "A")
  expect_identical(nrow(final), 1L)
  expect_identical(final$genome_digest, "abc123")
  expect_identical(final$run_id, "typing_20260805_090000_ab.log")
  expect_identical(final$cla_scheme, "Klebsiella (MLST)")
  expect_identical(final$amr_elements, 4L)
  # Written once, kept from the first pass.
  expect_identical(final$typed_at, first$typed_at)
})

test_that("a step that has not run cannot blank what an earlier one recorded", {
  dir <- local_tempdir()
  path <- new_db(dir)

  provenance$store_provenance(path, "A", list(cla_scheme = "Klebsiella (MLST)"))
  # A later pass with classical MLST switched off carries NA for those fields.
  provenance$store_provenance(path, "A", list(
    cla_scheme = NA_character_,
    amr_elements = 2L
  ))

  final <- row_of(path, "A")
  expect_identical(final$cla_scheme, "Klebsiella (MLST)")
  expect_identical(final$amr_elements, 2L)
})

test_that("store_provenance ignores unknown columns and bad input", {
  dir <- local_tempdir()
  path <- new_db(dir)

  expect_true(provenance$store_provenance(path, "A", list(
    genome_digest = "abc123",
    definitely_not_a_column = "x",
    isolate = "somebody_else"
  )))
  final <- row_of(path, "A")
  expect_identical(final$isolate, "A")
  expect_false("definitely_not_a_column" %in% names(final))

  expect_false(provenance$store_provenance(file.path(dir, "absent.db"), "A", list()))
  expect_false(provenance$store_provenance(path, NA_character_, list()))
})

test_that("the column list is derived from the table definition", {
  # PROVENANCE_COLUMNS gates what may be written, so it must track the DDL.
  expect_true(all(
    c(
      "isolate", "run_id", "typed_at", "phylotrace_version", "elapsed_seconds",
      "genome_digest", "file_sha256", "algorithm", "n_contigs", "total_length",
      "file_bytes", "cg_scheme_database", "cg_scheme_version", "cg_seed_genome",
      "cg_genus", "cg_species", "cg_locus_count", "cg_complex_type_distance",
      "cg_complex_type_count", "cg_identity", "cg_coverage", "cg_loci_found",
      "cg_alleles_added", "cg_partial_genes", "cg_filled_genes",
      "cg_removed_genes", "cg_completeness", "cla_scheme", "cla_scheme_version",
      "cla_alembic_version", "cla_repository", "cla_identity", "cla_coverage",
      "amr_organism", "amr_point_mutations", "amr_elements",
      "amr_abritamr_version", "amr_amrfinder_version",
      "amr_amrfinder_db_version", "pymlst_version", "blat_version",
      "mafft_version"
    ) %in%
      impl$PROVENANCE_COLUMNS
  ))
  expect_identical(length(impl$PROVENANCE_COLUMNS), 42L)
})
