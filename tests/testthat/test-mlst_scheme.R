box::use(
  DBI[dbConnect, dbDisconnect, dbGetQuery, dbListTables],
  RSQLite[SQLite],
  testthat[expect_false, expect_identical, expect_match, expect_null, expect_true, test_that],
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
  expect_identical(
    readLines(file.path(out, "profiles.tsv")),
    c("ST\tadk\tfumC", "1\t1\t1", "2\t1\t2")
  )
  expect_identical(
    readLines(file.path(out, "locus", "fumC.fasta")),
    c(">fumC_1", "ATGCCCTAA", ">fumC_2", "ATGCCGTAA")
  )

  spec_file <- file.path(dir, "cla.spec")
  mlst_scheme$write_scheme_spec(spec, spec_file)
  lines <- readLines(spec_file)
  expect_true(paste0("local_dir=", out) %in% lines)
  expect_identical(grep("^locus=", lines, value = TRUE), c("locus=adk", "locus=fumC"))
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

test_that("spec values cannot smuggle extra keys in through line breaks", {
  path <- file.path(local_tempdir(), "x.spec")
  mlst_scheme$write_scheme_spec(
    list(repository = "A\nlocal_dir=/etc", species = "S", scheme = "X", version = NA,
      local_dir = "/tmp/ok", loci = "adk"),
    path
  )
  lines <- readLines(path)
  expect_identical(grep("^local_dir=", lines, value = TRUE), "local_dir=/tmp/ok")
})
