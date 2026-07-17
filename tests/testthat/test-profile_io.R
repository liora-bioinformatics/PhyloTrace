box::use(
  testthat[
    expect_equal,
    expect_error,
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
    profile_io[
      allele_fasta,
      build_profile_table,
      clean_cell,
      detect_value_kind,
      export_typing_results,
      format_profile,
      parse_fasta_headers,
      parse_profile_file,
      read_fasta,
      scheme_loci,
      typing_export_target,
      write_delim_table
    ],
)

fixture <- function(dir) {
  db <- file.path(dir, "local.db")
  build_db(db, default_local(), metadata = meta_df(c("A", "B")))
  db
}

test_that("scheme_loci returns every locus of the reference, in its order", {
  dir <- local_tempdir()
  expect_identical(scheme_loci(fixture(dir)), c("g1", "g2", "g3"))
})

test_that("the profile table is a fixed-width view of the scheme", {
  dir <- local_tempdir()
  db <- fixture(dir)

  p <- build_profile_table(db, c("A", "B"), "hash")

  expect_identical(names(p), c("isolate", "g1", "g2", "g3"))
  expect_identical(p$isolate, c("A", "B"))
  # The synthetic reference is never an isolate.
  expect_false("ref" %in% p$isolate)
  expect_true(all(nchar(p$g1) == 64L))
})

test_that("a locus with no call anywhere still gets a column", {
  dir <- local_tempdir()
  db <- file.path(dir, "sparse.db")
  alleles <- default_local()
  # Only the reference carries g3; no isolate does.
  alleles$A <- alleles$A[c("g1", "g2")]
  alleles$B <- alleles$B[c("g1", "g2")]
  build_db(db, alleles)

  p <- build_profile_table(db, c("A", "B"), "hash")

  expect_identical(names(p), c("isolate", "g1", "g2", "g3"))
  expect_true(all(is.na(p$g3)))
})

test_that("hash values are the sha256 of the allele's sequence", {
  dir <- local_tempdir()
  db <- fixture(dir)

  p <- build_profile_table(db, "A", "hash")

  expect_identical(
    p$g1,
    as.character(openssl::sha256(default_local()$A[["g1"]]))
  )
})

test_that("index values are the database's own seqid, not a per-locus number", {
  dir <- local_tempdir()
  db <- fixture(dir)

  p <- build_profile_table(db, c("A", "B"), "index")
  expected <- qdf(
    db,
    "SELECT souche, gene, seqid FROM mlst WHERE souche = 'A' AND gene = 'g2'"
  )

  expect_identical(p$g2[p$isolate == "A"], as.character(expected$seqid))
})

test_that("each preset writes its own ID header and missing sentinel", {
  dir <- local_tempdir()
  db <- file.path(dir, "gap.db")
  alleles <- default_local()
  alleles$A <- alleles$A[c("g1", "g3")] # A has no call for g2
  build_db(db, alleles)

  p <- build_profile_table(db, c("A", "B"), "index")

  gt <- format_profile(p, "grapetree")
  expect_identical(names(gt)[[1]], "#Name")
  expect_identical(gt$g2[gt[[1]] == "A"], "0")

  chew <- format_profile(p, "chewbbaca")
  expect_identical(names(chew)[[1]], "FILE")
  expect_identical(chew$g2[chew[[1]] == "A"], "LNF")

  pt <- format_profile(p, "phylotrace")
  expect_identical(names(pt)[[1]], "isolate")
  expect_identical(pt$g2[pt[[1]] == "A"], "")
})

test_that("the pyMLST preset transposes loci onto the rows", {
  dir <- local_tempdir()
  db <- fixture(dir)

  out <- format_profile(build_profile_table(db, c("A", "B"), "index"), "pymlst")

  expect_identical(names(out), c("#GeneId", "A", "B"))
  expect_identical(out[["#GeneId"]], c("g1", "g2", "g3"))
})

test_that("clean_cell recognises every dialect's way of saying 'no call'", {
  expect_true(all(is.na(clean_cell(c("", "0", "-", "-1", "LNF", "PLOT3", "ASM")))))
  # chewBBACA marks an inferred allele — a real call, with the prefix stripped.
  expect_identical(clean_cell("INF-2"), "2")
  expect_identical(clean_cell(" 7 "), "7")
})

test_that("detect_value_kind separates sha256 from integers and other hashes", {
  expect_identical(detect_value_kind(c(strrep("a", 64), strrep("f", 64))), "hash")
  expect_identical(detect_value_kind(c("1", "42")), "index")
  expect_identical(detect_value_kind(c("deadbeef", "cafebabe")), "hash_other")
  expect_identical(detect_value_kind(c("1", "x!")), "mixed")
  expect_identical(detect_value_kind(character(0)), "empty")
})

test_that("the parser round-trips every preset it writes", {
  dir <- local_tempdir()
  db <- fixture(dir)
  loci <- scheme_loci(db)
  p <- build_profile_table(db, c("A", "B"), "hash")

  for (preset in c("phylotrace", "grapetree", "chewbbaca", "pymlst")) {
    f <- file.path(dir, paste0(preset, ".tsv"))
    write_delim_table(format_profile(p, preset), f)

    parsed <- parse_profile_file(f, loci)

    expect_identical(parsed$value_kind, "hash", info = preset)
    expect_setequal(parsed$isolates, c("A", "B"))
    expect_equal(nrow(parsed$long), 6L, info = preset)
    # pyMLST is the only dialect that puts the loci on the rows.
    expect_identical(parsed$loci_as_rows, preset == "pymlst", info = preset)
  }
})

test_that("the parser reads a comma-separated table", {
  dir <- local_tempdir()
  db <- fixture(dir)
  f <- file.path(dir, "p.csv")
  write_delim_table(
    format_profile(build_profile_table(db, c("A", "B"), "hash"), "phylotrace"),
    f,
    sep = ","
  )

  parsed <- parse_profile_file(f, scheme_loci(db))
  expect_equal(nrow(parsed$long), 6L)
})

test_that("the parser rejects a table from a different scheme", {
  dir <- local_tempdir()
  db <- fixture(dir)
  f <- file.path(dir, "alien.tsv")
  write_delim_table(
    data.frame(isolate = "X", zzz1 = "1", zzz2 = "2", stringsAsFactors = FALSE),
    f
  )

  expect_error(parse_profile_file(f, scheme_loci(db)), "match this scheme")
})

test_that("missing calls are dropped from the long form and counted", {
  dir <- local_tempdir()
  db <- file.path(dir, "gap.db")
  alleles <- default_local()
  alleles$A <- alleles$A[c("g1", "g3")]
  build_db(db, alleles)

  f <- file.path(dir, "p.tsv")
  write_delim_table(
    format_profile(build_profile_table(db, c("A", "B"), "hash"), "grapetree"),
    f
  )

  parsed <- parse_profile_file(f, scheme_loci(db))

  expect_equal(parsed$n_missing, 1L)
  expect_equal(nrow(parsed$long), 5L)
  expect_false(any(parsed$long$isolate == "A" & parsed$long$gene == "g2"))
})

test_that("allele_fasta emits one record per distinct allele in use", {
  dir <- local_tempdir()
  db <- fixture(dir)

  records <- allele_fasta(db, "A")

  # A's three alleles; the reference's are not exported, only what A carries.
  expect_equal(length(records), 3L)

  f <- file.path(dir, "a.fasta")
  writeLines(records, f)
  fa <- read_fasta(f)
  hdr <- parse_fasta_headers(fa$header)

  expect_setequal(hdr$gene, c("g1", "g2", "g3"))
  # The header's identity IS the sha256 of the record's sequence.
  expect_identical(as.character(openssl::sha256(fa$sequence)), hdr$allele)
})

test_that("parse_fasta_headers accepts the conventions in the wild", {
  ours <- parse_fasta_headers("PA0001|abc123")
  expect_identical(ours$gene, "PA0001")
  expect_identical(ours$allele, "abc123")

  # chewBBACA / cgMLST.org schema files
  chew <- parse_fasta_headers("PA0001_7")
  expect_identical(chew$gene, "PA0001")
  expect_identical(chew$allele, "7")

  # a per-locus file whose *name* carries the locus
  hinted <- parse_fasta_headers(c("3", "4"), locus_hint = "PA0001")
  expect_identical(hinted$gene, c("PA0001", "PA0001"))
  expect_identical(hinted$allele, c("3", "4"))

  # trailing description is ignored (pyMLST writes the strain list there)
  py <- parse_fasta_headers("g1|12 strainA;strainB")
  expect_identical(py$allele, "12")
})

test_that("read_fasta handles multi-line sequences", {
  dir <- local_tempdir()
  f <- file.path(dir, "wrapped.fasta")
  writeLines(c(">g1|h1", "ATGC", "GGTT", ">g2|h2", "TTTT"), f)

  fa <- read_fasta(f)

  expect_equal(nrow(fa), 2L)
  expect_identical(fa$sequence, c("ATGCGGTT", "TTTT"))
})

test_that("the export target is a bare table, or a zip when several files are needed", {
  t <- typing_export_target("/x/out.bin", "xlsx", TRUE, "none")
  expect_identical(basename(t$path), "out.xlsx")
  expect_false(t$bundled)

  # Sequences cannot live in a workbook, so they force a bundle.
  t <- typing_export_target("/x/out.bin", "xlsx", TRUE, "fasta")
  expect_identical(basename(t$path), "out.zip")
  expect_true(t$bundled)

  # A delimited export needs one file per table.
  t <- typing_export_target("/x/out.bin", "tsv", FALSE, "none")
  expect_identical(basename(t$path), "out.tsv")
  expect_false(t$bundled)

  t <- typing_export_target("/x/out.bin", "tsv", TRUE, "none")
  expect_identical(basename(t$path), "out.zip")
  expect_true(t$bundled)
})

test_that("a workbook export carries the profile and metadata as sheets", {
  dir <- local_tempdir()
  db <- fixture(dir)
  dest <- file.path(dir, "out.xlsx")

  r <- export_typing_results(
    db,
    dest,
    c("A", "B"),
    metadata = meta_df(c("A", "B")),
    format = "xlsx",
    value_kind = "hash"
  )

  expect_identical(r$path, dest)
  expect_setequal(openxlsx::getSheetNames(dest), c("profile", "metadata"))

  parsed <- parse_profile_file(dest, scheme_loci(db))
  expect_identical(parsed$value_kind, "hash")
  expect_setequal(parsed$isolates, c("A", "B"))
})

test_that("a bundled export zips the pieces together", {
  dir <- local_tempdir()
  db <- fixture(dir)

  r <- export_typing_results(
    db,
    file.path(dir, "out.tsv"),
    c("A", "B"),
    metadata = meta_df(c("A", "B")),
    format = "tsv",
    sequences = "fasta"
  )

  expect_identical(basename(r$path), "out.zip")
  expect_setequal(
    zip::zip_list(r$path)$filename,
    c("profile.tsv", "metadata.tsv", "alleles.fasta")
  )
})

test_that("the per-locus layout writes one FASTA per locus", {
  dir <- local_tempdir()
  db <- fixture(dir)

  r <- export_typing_results(
    db,
    file.path(dir, "out.tsv"),
    c("A", "B"),
    format = "tsv",
    sequences = "per_locus"
  )

  files <- zip::zip_list(r$path)$filename
  expect_true(all(paste0("alleles/", c("g1", "g2", "g3"), ".fasta") %in% files))
})

test_that("export refuses an empty isolate selection", {
  dir <- local_tempdir()
  db <- fixture(dir)
  expect_error(
    export_typing_results(db, file.path(dir, "o.tsv"), character(0)),
    "at least one isolate"
  )
})
