box::use(
  DBI[dbConnect, dbDisconnect, dbExecute, dbGetQuery, dbListTables],
  RSQLite[SQLite],
  testthat[
    expect_error,
    expect_false,
    expect_identical,
    expect_match,
    expect_null,
    expect_true,
    test_that,
  ],
  withr[local_tempdir],
)
box::use(
  app / logic / mlst_scheme,
)

# A two-locus scheme in the shape scheme_files$assemble_mlst_scheme() returns.
toy_scheme <- function() {
  list(
    loci = c("adk", "fumC"),
    profiles = data.frame(
      st = c(1L, 1L, 2L, 2L),
      locus = c("adk", "fumC", "adk", "fumC"),
      allele = c(1L, 1L, 1L, 2L)
    ),
    alleles = data.frame(
      locus = c("adk", "fumC", "fumC"),
      allele = c(1L, 1L, 2L),
      sequence = c("ATGAAATAA", "ATGCCCTAA", "ATGCCGTAA")
    )
  )
}

toy_info <- function() {
  list(
    name = "Toy MLST",
    source = "pubmlst",
    source_label = "PubMLST (University of Oxford)",
    version = "2024-12-31",
    species = "Testus organismus"
  )
}

test_that("a database without a stored scheme reads as none", {
  dir <- local_tempdir()
  path <- file.path(dir, "db.db")
  build_db(path, default_local())

  expect_null(mlst_scheme$mlst_scheme_info(path))
  expect_false(mlst_scheme$materialize_mlst_scheme(path, dir)$resolved)
  expect_match(mlst_scheme$describe_resolution(list(resolved = FALSE)), "no classical MLST scheme")
})

test_that("a stored scheme round-trips into the files claMLST builds from", {
  dir <- local_tempdir()
  path <- file.path(dir, "db.db")
  build_db(path, default_local())
  mlst_scheme$store_mlst_scheme(path, toy_scheme(), toy_info())

  info <- mlst_scheme$mlst_scheme_info(path)
  expect_identical(info$name, "Toy MLST")
  expect_identical(info$loci, c("adk", "fumC"))
  expect_identical(info$profiles, "2")

  out <- file.path(dir, "scheme")
  dir.create(out)
  spec <- mlst_scheme$materialize_mlst_scheme(path, out)
  expect_true(spec$resolved)
  expect_identical(spec$repository, "PubMLST (University of Oxford)")
  expect_identical(spec$version, "2024-12-31")
  expect_identical(spec$species, "Testus organismus")
  expect_identical(spec$dir, out)
  expect_identical(
    readLines(file.path(out, "profiles.tsv")),
    c("ST\tadk\tfumC", "1\t1\t1", "2\t1\t2")
  )
  expect_identical(
    readLines(file.path(out, "locus", "fumC.fasta")),
    c(">fumC_1", "ATGCCCTAA", ">fumC_2", "ATGCCGTAA")
  )
  expect_identical(sort(list.files(file.path(out, "locus"))), c("adk.fasta", "fumC.fasta"))

  # The same scheme in memory: the profile table the ST lookup reads, as text
  # like the file, and the sequences the stored calls carry.
  expect_identical(
    spec$profiles,
    data.frame(ST = c("1", "2"), adk = c("1", "1"), fumC = c("1", "2"))
  )
  expect_identical(
    spec$sequences,
    c("adk\t1" = "ATGAAATAA", "fumC\t1" = "ATGCCCTAA", "fumC\t2" = "ATGCCGTAA")
  )
  expect_match(
    mlst_scheme$describe_resolution(spec),
    "Toy MLST (version 2024-12-31) from PubMLST (University of Oxford), 2 loci",
    fixed = TRUE
  )
})

test_that("sequences are keyed by the gene id claMLST reports", {
  dir <- local_tempdir()
  path <- file.path(dir, "db.db")
  build_db(path, default_local())
  scheme <- toy_scheme()
  scheme$loci[2] <- "fum_C"
  scheme$profiles$locus[scheme$profiles$locus == "fumC"] <- "fum_C"
  scheme$alleles$locus[scheme$alleles$locus == "fumC"] <- "fum_C"
  mlst_scheme$store_mlst_scheme(path, scheme, toy_info())

  out <- file.path(dir, "scheme")
  dir.create(out)
  spec <- mlst_scheme$materialize_mlst_scheme(path, out)
  # pyMLST stores fum_C as fum-C; the file keeps the name the profile uses.
  expect_identical(names(spec$sequences), c("adk\t1", "fum-C\t1", "fum-C\t2"))
  expect_true(file.exists(file.path(out, "locus", "fum_C.fasta")))
  expect_identical(names(spec$profiles), c("ST", "adk", "fum_C"))
})

test_that("storing a scheme again replaces the earlier one", {
  dir <- local_tempdir()
  path <- file.path(dir, "db.db")
  build_db(path, default_local())
  mlst_scheme$store_mlst_scheme(path, toy_scheme(), toy_info())

  smaller <- toy_scheme()
  smaller$profiles <- smaller$profiles[smaller$profiles$st == 1L, ]
  mlst_scheme$store_mlst_scheme(path, smaller, modifyList(toy_info(), list(name = "Second")))

  info <- mlst_scheme$mlst_scheme_info(path)
  expect_identical(info$name, "Second")
  expect_identical(info$profiles, "1")
  con <- dbConnect(SQLite(), path)
  on.exit(dbDisconnect(con))
  expect_identical(dbGetQuery(con, "SELECT COUNT(*) AS n FROM cla_scheme_profiles")$n, 2L)
  expect_true(all(mlst_scheme$MLST_SCHEME_TABLES %in% dbListTables(con)))
})

test_that("a stored scheme with unusable locus names is not written out", {
  dir <- local_tempdir()
  path <- file.path(dir, "db.db")
  build_db(path, default_local())
  mlst_scheme$store_mlst_scheme(path, toy_scheme(), toy_info())
  con <- dbConnect(SQLite(), path)
  dbExecute(con, "UPDATE cla_scheme_loci SET locus = '../../escaped' WHERE locus = 'adk'")
  dbDisconnect(con)

  out <- file.path(dir, "scheme")
  dir.create(out)
  expect_error(mlst_scheme$materialize_mlst_scheme(path, out), "unusable locus names")
  expect_false(file.exists(file.path(dir, "escaped.fasta")))
})
