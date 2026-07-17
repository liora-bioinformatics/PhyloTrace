box::use(
  testthat[
    expect_equal,
    expect_error,
    expect_false,
    expect_identical,
    expect_setequal,
    expect_true,
    test_that
  ],
  withr[local_tempdir],
)
box::use(
  app / logic / db_export[export_preview, export_database],
  app / logic / db_compat[check_import_compatibility],
)

fixture <- function(dir, ...) {
  db <- file.path(dir, "source.db")
  build_db(
    db,
    default_local(),
    metadata = meta_df(
      c("A", "B"),
      extra = list(sample_collected_by = c("lab-x", "lab-y"))
    ),
    ...
  )
  db
}

test_that("export_preview counts the subset without writing anything", {
  dir <- local_tempdir()
  src <- fixture(dir)
  before <- list.files(dir)

  p <- export_preview(src, "A", "sample_collection_date", TRUE)

  expect_equal(p$n_isolates, 1L)
  expect_equal(p$n_loci, 3L)
  # 3 ref alleles + A's 3, none shared in this fixture
  expect_equal(p$n_calls, 6L)
  expect_identical(
    p$columns,
    c("isolate", "organism", "sample_collection_date")
  )
  expect_identical(list.files(dir), before)
})

test_that("export carries the scheme reference and prunes to used alleles", {
  dir <- local_tempdir()
  src <- fixture(dir)
  dest <- file.path(dir, "out.db")

  res <- export_database(src, dest, "A", "sample_collection_date")

  expect_true(file.exists(dest))
  expect_equal(res$n_isolates, 1L)

  # `ref` must survive regardless of the isolate selection: without it the file
  # has no scheme.
  expect_setequal(q1(dest, "SELECT DISTINCT souche FROM mlst"), c("ref", "A"))

  # sequences/hashes pruned to exactly what mlst references, and still aligned
  expect_equal(
    q1(dest, "SELECT COUNT(*) FROM sequences"),
    q1(dest, "SELECT COUNT(DISTINCT seqid) FROM mlst")
  )
  expect_equal(
    q1(dest, "SELECT COUNT(*) FROM hashes"),
    q1(dest, "SELECT COUNT(*) FROM sequences")
  )
  expect_equal(
    q1(
      dest,
      "SELECT COUNT(*) FROM mlst m LEFT JOIN sequences s ON s.id = m.seqid WHERE s.id IS NULL"
    ),
    0L
  )
})

test_that("exported hashes still equal sha256 of the sequence", {
  dir <- local_tempdir()
  src <- fixture(dir)
  dest <- file.path(dir, "out.db")
  export_database(src, dest, c("A", "B"))

  rows <- qdf(
    dest,
    "SELECT s.sequence AS seq, h.hash AS hash
       FROM sequences s JOIN hashes h ON h.id = s.id"
  )
  expect_identical(as.character(openssl::sha256(rows$seq)), rows$hash)
})

test_that("export preserves the DDL pyMLST depends on", {
  dir <- local_tempdir()
  src <- fixture(dir)
  dest <- file.path(dir, "out.db")
  export_database(src, dest, "A")

  mlst_sql <- q1(
    dest,
    "SELECT sql FROM sqlite_master WHERE type='table' AND name='mlst'"
  )
  expect_true(grepl("PRIMARY KEY", mlst_sql, fixed = TRUE))
  expect_true(grepl("FOREIGN KEY", mlst_sql, fixed = TRUE))

  idx <- qdf(
    dest,
    "SELECT name FROM sqlite_master WHERE type='index' AND tbl_name='mlst'"
  )$name
  expect_setequal(
    idx,
    c("ix_gene", "ix_seqid", "ix_souche", "ix_souche_gene_seqid")
  )

  # alembic_version is pyMLST's migration stamp; it must travel verbatim.
  expect_identical(q1(dest, "SELECT version_num FROM alembic_version"), "a793f8f3fd83")
  expect_equal(q1(dest, "SELECT COUNT(*) FROM mlst_type"), 1L)
})

test_that("withheld metadata columns never reach the destination", {
  dir <- local_tempdir()
  src <- fixture(dir)
  dest <- file.path(dir, "out.db")

  export_database(src, dest, c("A", "B"), metadata_cols = "sample_collection_date")

  cols <- names(qdf(dest, "SELECT * FROM metadata LIMIT 0"))
  expect_identical(cols, c("isolate", "organism", "sample_collection_date"))
  expect_false("sample_collected_by" %in% cols)
  expect_false("geo_loc_name_country" %in% cols)
  expect_equal(q1(dest, "SELECT COUNT(*) FROM metadata"), 2L)
})

test_that("a full export is a structural clone of the source", {
  dir <- local_tempdir()
  src <- fixture(dir)
  dest <- file.path(dir, "out.db")

  all_cols <- names(qdf(src, "SELECT * FROM metadata LIMIT 0"))
  export_database(src, dest, c("A", "B"), metadata_cols = all_cols)

  # Same column *order*, not merely the same column set: a peer diffing the two
  # files should see the same table shape.
  expect_identical(
    names(qdf(dest, "SELECT * FROM metadata LIMIT 0")),
    all_cols
  )

  for (tbl in c("mlst", "sequences", "hashes", "metadata", "targets", "mlst_type")) {
    expect_identical(
      qdf(dest, sprintf("SELECT * FROM %s", tbl)),
      qdf(src, sprintf("SELECT * FROM %s", tbl)),
      info = tbl
    )
  }
})

test_that("a partial export keeps the source's column order", {
  dir <- local_tempdir()
  src <- fixture(dir)
  dest <- file.path(dir, "out.db")

  # `organism` is carried automatically, and stays where the source put it
  # rather than being hoisted to the front.
  export_database(src, dest, "A", metadata_cols = "geo_loc_name_country")

  src_cols <- names(qdf(src, "SELECT * FROM metadata LIMIT 0"))
  out_cols <- names(qdf(dest, "SELECT * FROM metadata LIMIT 0"))

  expect_true("organism" %in% out_cols)
  expect_identical(out_cols, src_cols[src_cols %in% out_cols])
})

test_that("metadata can be withheld entirely", {
  dir <- local_tempdir()
  src <- fixture(dir)
  dest <- file.path(dir, "out.db")

  export_database(src, dest, "A", include_metadata = FALSE)

  tables <- qdf(dest, "SELECT name FROM sqlite_master WHERE type='table'")$name
  expect_false("metadata" %in% tables)
  expect_true(all(c("mlst", "mlst_type", "sequences") %in% tables))
})

test_that("only the selected isolates' metadata rows are written", {
  dir <- local_tempdir()
  src <- fixture(dir)
  dest <- file.path(dir, "out.db")

  export_database(src, dest, "A")

  expect_identical(q1(dest, "SELECT isolate FROM metadata"), "A")
})

test_that("an exported subset is importable back into its source", {
  dir <- local_tempdir()
  src <- fixture(dir)
  dest <- file.path(dir, "out.db")
  export_database(src, dest, "A")

  checks <- check_import_compatibility(src, dest)
  expect_false(attr(checks, "blocked"))
  expect_true(all(checks$status == "pass"))
})

test_that("export rejects an unknown or empty isolate selection", {
  dir <- local_tempdir()
  src <- fixture(dir)
  dest <- file.path(dir, "out.db")

  expect_error(export_database(src, dest, "does_not_exist"), "Unknown isolate")
  expect_error(export_database(src, dest, character(0)), "at least one isolate")
  expect_false(file.exists(dest))
})

test_that("a failed export leaves no partial file behind", {
  dir <- local_tempdir()
  src <- fixture(dir)
  dest <- file.path(dir, "out.db")

  expect_error(export_database(src, dest, "nope"))
  expect_identical(list.files(dir, pattern = "\\.part$"), character(0))
  expect_false(file.exists(dest))
})
