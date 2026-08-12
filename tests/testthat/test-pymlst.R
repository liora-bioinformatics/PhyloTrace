box::use(
  testthat[expect_false, expect_identical, expect_true, test_that],
  utils[tail],
  withr[local_tempdir],
)
box::use(
  app / logic / pymlst,
)

impl <- attr(pymlst, "namespace")

# The exact shape `claMLST search` prints for a genome whose gapA never got
# called: the ST column holds every ST compatible with the six remaining loci.
AMBIGUOUS_STS <- "1953;3684;5029;5510;6853;1384;11;2193;5809;690;3348;1460;7123;7252;6843"

test_that("typing_args asks for classical MLST only with a resolved scheme", {
  args <- function(...) {
    impl$typing_args("/db/x.db", "/genomes/a.fna", 0.95, 0.9, "PhyloTrace", ...)
  }

  # Both halves are needed: the reference path to build into and the spec that
  # says what to build. A species alone buys nothing.
  with_scheme <- args(species = "Enterococcus faecium", cla_db = "/tmp/c.db",
                      cla_spec = "/tmp/c.spec")
  expect_true(all(c("-m", "/tmp/c.db", "-M", "/tmp/c.spec") %in% with_scheme))

  no_spec <- args(species = "Enterococcus faecium", cla_db = "/tmp/c.db")
  expect_false(any(c("-m", "-M") %in% no_spec))
  expect_true(all(c("-s", "Enterococcus faecium") %in% no_spec))

  # Genome paths always sit behind the separator so they cannot be read as flags.
  expect_identical(tail(with_scheme, 2), c("--", "/genomes/a.fna"))
})

test_that("typing_args passes the classical search its own thresholds", {
  args <- impl$typing_args(
    "/db/x.db", "/genomes/a.fna", 0.95, 0.9, "PhyloTrace",
    species = "Acinetobacter baumannii",
    cla_identity = 0.9,
    cla_coverage = 0.85,
    cla_db = "/tmp/c.db",
    cla_spec = "/tmp/c.spec"
  )

  # The allele-calling pair and the classical pair travel under separate flags,
  # so the script can search the seven classical loci at its own cutoffs.
  expect_identical(args[match("-i", args) + 1L], "0.95")
  expect_identical(args[match("-c", args) + 1L], "0.9")
  expect_identical(args[match("-I", args) + 1L], "0.9")
  expect_identical(args[match("-C", args) + 1L], "0.85")
})

test_that("typing_args defaults the classical thresholds to pyMLST's own", {
  args <- impl$typing_args(
    "/db/x.db", "/genomes/a.fna", 0.95, 0.9, "PhyloTrace",
    species = "Acinetobacter baumannii",
    cla_db = "/tmp/c.db",
    cla_spec = "/tmp/c.spec"
  )
  expect_identical(
    args[match("-I", args) + 1L],
    as.character(pymlst$CLA_IDENTITY_DEFAULT)
  )
  expect_identical(
    args[match("-C", args) + 1L],
    as.character(pymlst$CLA_COVERAGE_DEFAULT)
  )
  # pyMLST's own CLI defaults: looser identity for the classical search than for
  # allele calling, same coverage.
  expect_identical(pymlst$CLA_IDENTITY_DEFAULT, 0.9)
  expect_identical(pymlst$CG_IDENTITY_DEFAULT, 0.95)
})

test_that("typing_args omits classical thresholds when no scheme is built", {
  # Nothing to search, so the flags would be meaningless - and their absence is
  # what makes the script fall back to the allele-calling pair.
  args <- impl$typing_args(
    "/db/x.db", "/genomes/a.fna", 0.95, 0.9, "PhyloTrace",
    species = "Acinetobacter baumannii"
  )
  expect_false(any(c("-I", "-C") %in% args))
})

test_that("clamlst_status reports a single ST with a complete profile as known", {
  expect_identical(
    pymlst$clamlst_status("11", "gapA=3,infB=3,mdh=1,pgi=1,phoE=1,rpoB=1,tonB=4"),
    "known"
  )
})

test_that("clamlst_status treats an uncalled locus as partial, not a call", {
  expect_identical(
    pymlst$clamlst_status(NA_character_, "gapA=,infB=3,mdh=1"),
    "partial"
  )
  # An ST candidate list next to a missing locus must never read as "known".
  expect_identical(
    pymlst$clamlst_status(AMBIGUOUS_STS, "gapA=,infB=3,mdh=1,pgi=1,phoE=1,rpoB=1,tonB=4"),
    "partial"
  )
})

test_that("clamlst_status flags several STs over a complete profile as ambiguous", {
  expect_identical(
    pymlst$clamlst_status("11;12", "gapA=3,infB=3,mdh=1,pgi=1,phoE=1,rpoB=1,tonB=4"),
    "ambiguous"
  )
  # Two alleles at one locus leave the profile undecided as well.
  expect_identical(
    pymlst$clamlst_status("", "gapA=3;5,infB=3,mdh=1,pgi=1,phoE=1,rpoB=1,tonB=4"),
    "ambiguous"
  )
})

test_that("clamlst_status reports a complete but unregistered profile as novel", {
  expect_identical(
    pymlst$clamlst_status("", "gapA=3,infB=3,mdh=1,pgi=1,phoE=1,rpoB=1,tonB=4"),
    "novel"
  )
  expect_identical(
    pymlst$clamlst_status(NA_character_, "gapA=new,infB=3,mdh=1"),
    "novel"
  )
})

test_that("clamlst_status returns NA when nothing was called", {
  expect_identical(
    pymlst$clamlst_status(NA_character_, "gapA=,infB=,mdh="),
    NA_character_
  )
  expect_identical(pymlst$clamlst_status(NA_character_, NA_character_), NA_character_)
})

# Two Enterococcus faecium profile rows sharing every allele but pstS, one of
# them carrying the scheme's "locus absent" marker.
EFM_PROFILES <- data.frame(
  ST = c("386", "1421", "17"),
  atpA = c("1", "1", "1"),
  ddl = c("1", "1", "1"),
  gdh = c("1", "1", "1"),
  purK = c("1", "1", "1"),
  gyd = c("1", "1", "1"),
  pstS = c("6", "0", "1"),
  adk = c("1", "1", "1"),
  stringsAsFactors = FALSE
)

test_that("cg_outcome reports allele calling before the strain is finished", {
  # A strain sits at "Running" all through classical MLST and the AMR screen,
  # yet the mother database has already accepted it - which is what decides
  # whether its results may be written.
  results <- data.frame(
    strain = c("a", "b", "c", "d"),
    status = c("Running", "Running", "Pending", "Duplicate"),
    cg_done = c(TRUE, FALSE, FALSE, FALSE),
    stringsAsFactors = FALSE
  )
  expect_identical(
    pymlst$cg_outcome(results),
    c("Added", "Running", "Pending", "Duplicate")
  )
})

test_that("cg_outcome never pre-empts a failure verdict", {
  results <- data.frame(
    strain = c("a", "b"),
    status = c("Incompatible", "Error"),
    cg_done = c(TRUE, TRUE),
    stringsAsFactors = FALSE
  )
  expect_identical(pymlst$cg_outcome(results), c("Incompatible", "Error"))
  expect_identical(pymlst$cg_outcome(NULL), character(0))
})

test_that("st_from_profile reads an absent locus as the scheme's zero allele", {
  # pstS is deleted in a large part of the E. faecium population, so the scheme
  # records those STs with pstS = 0 - and claMLST cannot call them at all.
  expect_identical(
    pymlst$st_from_profile(
      "adk=1,atpA=1,ddl=1,gdh=1,gyd=1,pstS=,purK=1",
      EFM_PROFILES
    ),
    "1421"
  )
  # A called pstS still decides on its own.
  expect_identical(
    pymlst$st_from_profile(
      "adk=1,atpA=1,ddl=1,gdh=1,gyd=1,pstS=6,purK=1",
      EFM_PROFILES
    ),
    "386"
  )
})

test_that("st_from_profile refuses anything but a single matching row", {
  # Two loci uncalled: no row carries a zero for both.
  expect_identical(
    pymlst$st_from_profile("adk=1,atpA=1,ddl=1,gdh=1,gyd=,pstS=,purK=1", EFM_PROFILES),
    NA_character_
  )
  # An unregistered allele has no profile to match.
  expect_identical(
    pymlst$st_from_profile("adk=99,atpA=1,ddl=1,gdh=1,gyd=1,pstS=,purK=1", EFM_PROFILES),
    NA_character_
  )
  expect_identical(
    pymlst$st_from_profile("adk=new,atpA=1,ddl=1,gdh=1,gyd=1,pstS=1,purK=1", EFM_PROFILES),
    NA_character_
  )
  # A locus the profile table does not describe.
  expect_identical(pymlst$st_from_profile("xyzA=1", EFM_PROFILES), NA_character_)
  expect_identical(pymlst$st_from_profile("adk=1", NULL), NA_character_)
})

test_that("resolve_clamlst_call keeps the search's own call authoritative", {
  complete <- "adk=1,atpA=1,ddl=1,gdh=1,gyd=1,pstS=6,purK=1"
  expect_identical(
    pymlst$resolve_clamlst_call("386", complete, EFM_PROFILES),
    list(st = "386", status = "known")
  )
  # Undetermined by the search, settled by the profile table.
  expect_identical(
    pymlst$resolve_clamlst_call(
      "386;1421;17",
      "adk=1,atpA=1,ddl=1,gdh=1,gyd=1,pstS=,purK=1",
      EFM_PROFILES
    ),
    list(st = "1421", status = "inferred")
  )
  # Without the profile table the call stays what the search made of it.
  expect_identical(
    pymlst$resolve_clamlst_call(
      "386;1421;17",
      "adk=1,atpA=1,ddl=1,gdh=1,gyd=1,pstS=,purK=1"
    ),
    list(st = NA_character_, status = "partial")
  )
})

test_that("parse_typing_log resolves the ST it can and drops candidate lists", {
  log <- c(
    "Processing Strain: efm1",
    "Classical MLST ST: 386;1421;17",
    "Classical MLST alleles: adk=1,atpA=1,ddl=1,gdh=1,gyd=1,pstS=,purK=1",
    "Strain elapsed: 3",
    "Strain finished: 08:58:03",
    "Done!"
  )

  with_profiles <- pymlst$parse_typing_log(log, "efm1", EFM_PROFILES)
  expect_identical(with_profiles$st, "1421")
  expect_identical(with_profiles$cla_status, "inferred")

  without <- pymlst$parse_typing_log(log, "efm1")
  expect_identical(without$st, NA_character_)
  expect_identical(without$cla_status, "partial")
})

test_that("store_clamlst_results persists an ST only for an unambiguous call", {
  db <- file.path(local_tempdir(), "db.sqlite")
  con <- DBI::dbConnect(RSQLite::SQLite(), db)
  DBI::dbExecute(con, "CREATE TABLE mlst_type (name TEXT)")
  DBI::dbDisconnect(con)

  results <- data.frame(
    strain = c("clean", "gapA_missing"),
    st = c("11", AMBIGUOUS_STS),
    alleles = c(
      "gapA=3,infB=3,mdh=1,pgi=1,phoE=1,rpoB=1,tonB=4",
      "gapA=,infB=3,mdh=1,pgi=1,phoE=1,rpoB=1,tonB=4"
    ),
    stringsAsFactors = FALSE
  )
  expect_true(pymlst$store_clamlst_results(db, results))

  con <- DBI::dbConnect(RSQLite::SQLite(), db)
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  stored <- DBI::dbGetQuery(
    con,
    "SELECT DISTINCT isolate, st, status FROM classical_mlst ORDER BY isolate"
  )

  expect_identical(stored$isolate, c("clean", "gapA_missing"))
  expect_identical(stored$st, c("11", NA_character_))
  expect_identical(stored$status, c("known", "partial"))

  # The uncalled locus is dropped, the six that worked are still recorded.
  n_loci <- DBI::dbGetQuery(
    con,
    "SELECT COUNT(*) AS n FROM classical_mlst WHERE isolate = 'gapA_missing'"
  )
  expect_identical(n_loci$n, 6L)
  expect_false(any(grepl(";", stored$st, fixed = TRUE), na.rm = TRUE))
})

test_that("storing one isolate at a time matches storing them in one batch", {
  # Results are banked isolate by isolate while the run is live and swept again
  # at the end, so a strain can be written more than once - it must land as if
  # it had been written once.
  results <- data.frame(
    strain = c("s1", "s2"),
    st = c("11", "42"),
    alleles = rep("gapA=3,infB=3,mdh=1,pgi=1,phoE=1,rpoB=1,tonB=4", 2),
    cla_status = c("known", "known"),
    stringsAsFactors = FALSE
  )

  dir <- local_tempdir()
  fresh_db <- function(name) {
    db <- file.path(dir, name)
    con <- DBI::dbConnect(RSQLite::SQLite(), db)
    DBI::dbExecute(con, "CREATE TABLE mlst_type (name TEXT)")
    DBI::dbDisconnect(con)
    db
  }
  rows <- function(db) {
    con <- DBI::dbConnect(RSQLite::SQLite(), db)
    on.exit(DBI::dbDisconnect(con), add = TRUE)
    DBI::dbGetQuery(
      con,
      "SELECT isolate, gene, allele, st, status FROM classical_mlst
       ORDER BY isolate, gene"
    )
  }

  batch <- fresh_db("batch.sqlite")
  pymlst$store_clamlst_results(batch, results)

  incremental <- fresh_db("incremental.sqlite")
  for (i in seq_len(nrow(results))) {
    pymlst$store_clamlst_results(incremental, results[i, , drop = FALSE])
  }
  # The closing sweep runs over strains that were already written.
  for (i in seq_len(nrow(results))) {
    pymlst$store_clamlst_results(incremental, results[i, , drop = FALSE])
  }

  expect_identical(rows(incremental), rows(batch))
  expect_identical(nrow(rows(incremental)), 14L)
})
