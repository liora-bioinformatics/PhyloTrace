# dev/mock_database.R
#
# Generates an artificial PhyloTrace database without running typing: a
# cgMLST allele table with lineage structure, classical MLST calls and typing
# provenance, plus optional scenarios (mixed schemes, failed calls, ...) that
# are hard to reproduce from real genomes. The result opens in the app like any
# other database and its tables can be edited further by hand.
#
# Only the pyMLST core tables are written here. Everything PhyloTrace itself
# writes goes through the app's own writers (sync_metadata_table(),
# store_clamlst_results(), PROVENANCE_DDL, hash_database()), so a mock database
# has exactly the shape a typed one has.
#
# Usage (from the project root):
#   Rscript -e 'box::use(dev/mock_database); mock_database$mock_database("~/mock.db")'
#   Rscript -e 'box::use(dev/mock_database)
#     mock_database$mock_database("~/mock.db", scenarios = c("mixed_schemes", "failed"))'
# For realistic locations and dates afterwards: Rscript dev/enrich_metadata.R ~/mock.db

box::use(
  DBI[dbAppendTable, dbDisconnect, dbExecute, dbReadTable, dbWriteTable],
  stats[rexp, runif, setNames],
)
box::use(
  app / logic / database_functions[sync_metadata_table],
  app / logic / db_connect[connect],
  app / logic / provenance[PROVENANCE_DDL, scheme_provenance],
  app / logic / pymlst[hash_database, store_clamlst_results],
)

#' Mock Database Scenarios
#'
#' @description Every scenario `mock_database()` can apply, with what it does.
#'   The first group each claims its own slice of isolates, so any combination
#'   can be applied at once; `skipped` and `no_classical` replace classical MLST
#'   for the whole database and override the rest.
#'
#' @export
MOCK_SCENARIOS <- c(
  mixed_schemes = "Some isolates typed against a second classical scheme",
  mixed_releases = "Some isolates typed against an older profile-table release",
  mixed_thresholds = "Some isolates searched at other identity/coverage thresholds",
  mixed_pymlst = "Some isolates called with another pyMLST version",
  ambiguous = "Some isolates with a multi-hit locus (e.g. '3;7')",
  calls_without_alleles = "Some isolates with a call in provenance but no stored alleles",
  failed = "Some isolates whose classical MLST step failed",
  untyped = "Some isolates without any classical MLST record",
  provenance_mismatch = "Some isolates whose provenance names another release",
  skipped = "No scheme available: classical MLST skipped for every isolate",
  no_classical = "No classical MLST tables at all (database typed before it existed)"
)

# Share of isolates each slice scenario claims.
SLICE_SHARE <- c(
  mixed_schemes = 0.10,
  mixed_releases = 0.30,
  mixed_thresholds = 0.10,
  mixed_pymlst = 0.08,
  ambiguous = 0.05,
  calls_without_alleles = 0.05,
  failed = 0.05,
  untyped = 0.05,
  provenance_mismatch = 0.05
)

PRIMARY <- list(
  scheme = "Testus spp. (MLST)",
  repository = "pubmlst",
  loci = c("adk", "fumC", "gyrB", "icd", "mdh", "purA", "recA"),
  releases = c("2026-08-14", "2026-09-30")
)

SECONDARY <- list(
  scheme = "Testus spp. (MLST (Pasteur))",
  repository = "pasteur",
  loci = c("dinB", "icdA", "pabB", "polB", "putP", "trpA", "trpB", "uidA"),
  releases = "2026-09-12"
)

PYMLST_VERSION <- "2.2.2"
CLA_THRESHOLD <- 0.9

COUNTRIES <- c("Germany", "France", "Italy", "Spain", "Netherlands", "Poland")

# Random nucleotide sequence of `n` bases.
random_sequence <- function(n) {
  paste(sample(c("A", "C", "G", "T"), n, replace = TRUE), collapse = "")
}

# `sequence` with `k` random point substitutions.
mutate_sequence <- function(sequence, k = 1L) {
  bases <- strsplit(sequence, "", fixed = TRUE)[[1]]
  at <- sample(length(bases), k)
  bases[at] <- vapply(
    bases[at],
    function(b) sample(setdiff(c("A", "C", "G", "T"), b), 1),
    character(1)
  )
  paste(bases, collapse = "")
}

# Lineage of every isolate, sizes skewed the way real collections are.
assign_lineages <- function(n_isolates, n_lineages) {
  weights <- rexp(n_lineages)^2
  sample(n_lineages, n_isolates, replace = TRUE, prob = weights / sum(weights))
}

# The cgMLST allele table: reference alleles under "ref", lineage profiles
# derived from them and isolates derived from their lineage, with the odd
# missing locus pyMLST leaves out of `mlst` entirely.
cg_alleles <- function(isolates, lineage, n_loci) {
  loci <- sprintf("TST_%05d", seq_len(n_loci))
  ref <- vapply(
    loci,
    function(l) random_sequence(3L * sample(100:300, 1)),
    character(1)
  )
  bases <- lapply(seq_len(max(lineage)), function(i) {
    vapply(
      ref,
      function(s) if (runif(1) < 0.4) mutate_sequence(s, sample(1:3, 1)) else s,
      character(1)
    )
  })
  rows <- lapply(seq_along(isolates), function(i) {
    seqs <- vapply(
      bases[[lineage[i]]],
      function(s) if (runif(1) < 0.03) mutate_sequence(s) else s,
      character(1)
    )
    kept <- runif(n_loci) > 0.005
    data.frame(souche = isolates[i], gene = loci[kept], sequence = seqs[kept])
  })
  rbind(data.frame(souche = "ref", gene = loci, sequence = unname(ref)), do.call(rbind, rows))
}

# Writes the pyMLST core tables (`mlst_type`, `mlst`, `sequences`,
# `alembic_version`) and the scheme tables PhyloTrace adds on download.
write_core <- function(con, calls, species) {
  dbExecute(
    con,
    "CREATE TABLE alembic_version (
       version_num VARCHAR(32) NOT NULL,
       CONSTRAINT alembic_version_pkc PRIMARY KEY (version_num))"
  )
  dbExecute(con, "INSERT INTO alembic_version VALUES ('a793f8f3fd83')")
  dbExecute(
    con,
    "CREATE TABLE mlst_type (
       name VARCHAR(4) NOT NULL, source VARCHAR, species VARCHAR, version VARCHAR,
       PRIMARY KEY (name))"
  )
  dbExecute(
    con,
    "INSERT INTO mlst_type VALUES ('wg', 'mock', ?, '1.0')",
    params = list(species)
  )
  dbExecute(
    con,
    "CREATE TABLE sequences (
       id INTEGER NOT NULL, sequence TEXT, PRIMARY KEY (id), UNIQUE (sequence))"
  )
  dbExecute(
    con,
    "CREATE TABLE mlst (
       id INTEGER NOT NULL, souche TEXT, gene TEXT, seqid INTEGER,
       PRIMARY KEY (id), FOREIGN KEY(seqid) REFERENCES sequences (id))"
  )

  alleles <- unique(calls$sequence)
  dbAppendTable(con, "sequences", data.frame(id = seq_along(alleles), sequence = alleles))
  dbAppendTable(
    con,
    "mlst",
    data.frame(
      id = seq_len(nrow(calls)),
      souche = calls$souche,
      gene = calls$gene,
      seqid = match(calls$sequence, alleles)
    )
  )
  for (sql in c(
    "CREATE INDEX ix_gene ON mlst (gene)",
    "CREATE INDEX ix_seqid ON mlst (seqid)",
    "CREATE INDEX ix_souche ON mlst (souche)",
    "CREATE INDEX ix_souche_gene_seqid ON mlst (gene, souche, seqid)"
  )) {
    dbExecute(con, sql)
  }

  ref <- calls[calls$souche == "ref", ]
  dbWriteTable(
    con,
    "targets",
    data.frame(
      Locus = ref$gene,
      Gene = "",
      Start = as.character(seq(1L, by = 1000L, length.out = nrow(ref))),
      Length = as.character(nchar(ref$sequence)),
      Product = "hypothetical protein (mock)",
      Alleles = paste0(ref$gene, ".fasta")
    )
  )
  parts <- strsplit(species, " ", fixed = TRUE)[[1]]
  dbWriteTable(
    con,
    "scheme_overview",
    data.frame(
      key = c(
        "Name", "Database", "Version", "Genus", "Species", "Locus Count",
        "Complex Type Distance"
      ),
      value = c(
        paste(species, "cgMLST (mock)"), "PhyloTrace mock database", "1.0",
        parts[1], paste(parts[-1], collapse = " "), as.character(nrow(ref)), "10"
      )
    )
  )
}

# Fills a few metadata fields so filters and plots have something to show.
fill_metadata <- function(con, isolates) {
  meta <- dbReadTable(con, "metadata")
  at <- match(meta$isolate, isolates)
  dates <- as.Date("2023-01-01") + sample(0:1000, length(isolates), replace = TRUE)
  meta$sample_collection_date <- as.character(dates[at])
  meta$geo_loc_name_country <- sample(COUNTRIES, nrow(meta), replace = TRUE)
  meta$purpose_of_sampling <- sample(
    c("Surveillance", "Outbreak investigation", "Diagnostic"),
    nrow(meta),
    replace = TRUE
  )
  dbWriteTable(con, "metadata", meta, overwrite = TRUE)
}

# One classical ST profile per lineage and scheme, numbered like a repository.
st_profiles <- function(n_lineages, loci) {
  lapply(seq_len(n_lineages), function(i) {
    list(
      st = as.character(sample(1:2000, 1)),
      alleles = setNames(as.character(sample(1:60, length(loci), replace = TRUE)), loci)
    )
  })
}

# "gene=allele,..." string the typing log carries.
allele_spec <- function(alleles) {
  paste(names(alleles), alleles, sep = "=", collapse = ",")
}

# Gives each slice scenario its own isolates, in the order listed.
allocate_slices <- function(isolates, scenarios) {
  slice <- setNames(rep(NA_character_, length(isolates)), isolates)
  pool <- sample(isolates)
  for (sc in intersect(names(SLICE_SHARE), scenarios)) {
    n <- min(max(1L, round(SLICE_SHARE[[sc]] * length(isolates))), length(pool))
    if (!n) {
      break
    }
    slice[pool[seq_len(n)]] <- sc
    pool <- pool[-seq_len(n)]
  }
  slice
}

# Per-isolate plan of the classical MLST outcome: status, ST, alleles, and the
# scheme release and settings it was called with.
classical_plan <- function(isolates, lineage, slice) {
  n_lineages <- max(lineage)
  profiles <- list(
    primary = st_profiles(n_lineages, PRIMARY$loci),
    secondary = st_profiles(n_lineages, SECONDARY$loci)
  )
  plan <- data.frame(
    isolate = isolates,
    scenario = unname(slice),
    status = "known",
    st = NA_character_,
    alleles = NA_character_,
    scheme = PRIMARY$scheme,
    repository = PRIMARY$repository,
    version = PRIMARY$releases[2],
    identity = CLA_THRESHOLD,
    coverage = CLA_THRESHOLD,
    pymlst = PYMLST_VERSION,
    stored = TRUE,
    in_prov = TRUE,
    prov_version = NA_character_
  )

  for (i in seq_along(isolates)) {
    sc <- plan$scenario[i]
    secondary <- identical(sc, "mixed_schemes")
    profile <- profiles[[if (secondary) "secondary" else "primary"]][[lineage[i]]]
    alleles <- profile$alleles
    roll <- runif(1)
    # A small baseline of unresolved calls, as any real collection has.
    status <- if (roll < 0.04) "novel" else if (roll < 0.07) "partial" else "known"
    if (identical(sc, "ambiguous")) {
      status <- "ambiguous"
    }
    # Every other isolate of an older release keeps an unresolved call, so the
    # "outdated unresolved calls" check has something to find.
    if (identical(sc, "mixed_releases") && i %% 2 == 0) {
      status <- "novel"
    }
    locus <- sample(length(alleles), 1)
    alleles[locus] <- switch(
      status,
      novel = if (runif(1) < 0.5) "new" else as.character(sample(61:90, 1)),
      partial = "",
      ambiguous = paste(alleles[locus], sample(61:90, 1), sep = ";"),
      alleles[locus]
    )
    plan$status[i] <- status
    plan$st[i] <- if (status == "known") profile$st else NA_character_
    plan$alleles[i] <- allele_spec(alleles)
    if (secondary) {
      plan$scheme[i] <- SECONDARY$scheme
      plan$repository[i] <- SECONDARY$repository
      plan$version[i] <- SECONDARY$releases[1]
    }
  }

  is <- function(sc) plan$scenario %in% sc
  plan$version[is("mixed_releases")] <- PRIMARY$releases[1]
  plan$identity[is("mixed_thresholds")] <- 0.8
  plan$coverage[is("mixed_thresholds")] <- 0.85
  plan$pymlst[is("mixed_pymlst")] <- "2.1.6"
  plan$stored[is(c("calls_without_alleles", "failed", "untyped"))] <- FALSE
  plan$status[is("calls_without_alleles")] <- "ambiguous"
  plan$status[is("failed")] <- "failed"
  plan$in_prov[is("untyped")] <- FALSE
  plan$prov_version <- plan$version
  plan$prov_version[is("provenance_mismatch")] <- "2026-07-01"
  plan
}

# Stores the planned calls through the app's own writer, one call per scheme
# release and setting so each group carries its own provenance.
write_classical <- function(path, plan) {
  stored <- plan[plan$stored, , drop = FALSE]
  refs <- reference_sequences(stored$alleles)
  groups <- split(
    stored,
    paste(stored$scheme, stored$version, stored$identity, stored$coverage, stored$pymlst)
  )
  for (g in groups) {
    store_clamlst_results(
      path,
      data.frame(
        strain = g$isolate,
        st = g$st,
        alleles = g$alleles,
        cla_status = g$status
      ),
      identity = g$identity[1],
      coverage = g$coverage[1],
      repository = g$repository[1],
      scheme = g$scheme[1],
      scheme_version = g$version[1],
      pymlst_version = g$pymlst[1],
      refs = refs
    )
  }
  # Calls land stamped with the release they were made against, as a real run
  # on that day would have stamped them.
  con <- connect(path)
  on.exit(dbDisconnect(con))
  dbExecute(
    con,
    "UPDATE classical_mlst SET called_at = scheme_version || ' 10:00:00'"
  )
}

# Reference sequence of every registered allele the calls use; novel and
# multi-hit alleles have none, as in a real reference.
reference_sequences <- function(specs) {
  pairs <- unlist(strsplit(specs, ",", fixed = TRUE))
  pairs <- unique(pairs[grepl("=[0-9]+$", pairs)])
  keys <- sub("=", "\t", pairs, fixed = TRUE)
  list(
    sequences = setNames(
      vapply(keys, function(k) random_sequence(sample(450:520, 1)), character(1)),
      keys
    ),
    alembic = "a793f8f3fd83"
  )
}

# One provenance row per typed isolate, in the app's own table definition.
write_provenance <- function(path, plan, classical) {
  rows <- plan[plan$in_prov, , drop = FALSE]
  cg <- scheme_provenance(path)
  prov <- data.frame(
    isolate = rows$isolate,
    run_id = "mock_database",
    typed_at = paste(rows$version, "10:00:00"),
    phylotrace_version = "mock",
    cg_scheme_database = cg$cg_scheme_database,
    cg_scheme_version = cg$cg_scheme_version,
    cg_species = cg$cg_species,
    cg_locus_count = cg$cg_locus_count,
    pymlst_version = rows$pymlst
  )
  if (identical(classical, "skipped")) {
    prov$cla_status <- "skipped"
  } else if (identical(classical, "full")) {
    prov$cla_status <- rows$status
    prov$cla_scheme <- rows$scheme
    prov$cla_scheme_version <- rows$prov_version
    prov$cla_repository <- rows$repository
    prov$cla_identity <- rows$identity
    prov$cla_coverage <- rows$coverage
  }
  con <- connect(path)
  on.exit(dbDisconnect(con))
  dbExecute(con, PROVENANCE_DDL)
  dbAppendTable(con, "typing_provenance", prov)
}

#' Generate a Mock PhyloTrace Database
#'
#' @description Writes an artificial database with cgMLST allele calls, metadata,
#'   classical MLST calls and typing provenance, optionally with scenarios that
#'   real typing rarely produces (see `MOCK_SCENARIOS`).
#'
#' @param path Character path of the `.db` file to write.
#' @param n_isolates Integer number of isolates.
#' @param scenarios Character vector of `MOCK_SCENARIOS` names.
#' @param n_loci Integer number of cgMLST loci.
#' @param species Character scheme species.
#' @param seed Integer random seed; the same seed gives the same database. The
#'   caller's random number stream is left untouched.
#' @param overwrite Logical; replace an existing file at `path`.
#'
#' @return Invisibly, the per-isolate plan: scenario, classical status, ST and
#'   the scheme release each isolate was given.
#' @export
mock_database <- function(
  path,
  n_isolates = 60L,
  scenarios = character(0),
  n_loci = 100L,
  species = "Testus organismus",
  seed = 1L,
  overwrite = FALSE
) {
  unknown <- setdiff(scenarios, names(MOCK_SCENARIOS))
  if (length(unknown)) {
    stop("Unknown scenario(s): ", paste(unknown, collapse = ", "))
  }
  if (file.exists(path)) {
    if (!isTRUE(overwrite)) {
      stop("File exists: ", path, " (pass overwrite = TRUE to replace it)")
    }
    unlink(paste0(path, c("", "-wal", "-shm")))
  }
  if (n_isolates < 2) {
    stop("n_isolates must be at least 2")
  }

  if (exists(".Random.seed", envir = globalenv())) {
    previous <- get(".Random.seed", envir = globalenv())
    on.exit(assign(".Random.seed", previous, envir = globalenv()), add = TRUE)
  }
  set.seed(seed)

  isolates <- sprintf("MOCK_%04d", seq_len(n_isolates))
  lineage <- assign_lineages(n_isolates, max(3L, n_isolates %/% 8L))

  con <- connect(path, create = TRUE)
  write_core(con, cg_alleles(isolates, lineage, n_loci), species)
  dbDisconnect(con)
  hash_database(path)
  sync_metadata_table(path)
  con <- connect(path)
  fill_metadata(con, isolates)
  dbDisconnect(con)

  classical <- if ("no_classical" %in% scenarios) {
    "none"
  } else if ("skipped" %in% scenarios) {
    "skipped"
  } else {
    "full"
  }
  slice <- if (identical(classical, "full")) {
    allocate_slices(isolates, scenarios)
  } else {
    setNames(rep(NA_character_, n_isolates), isolates)
  }
  plan <- classical_plan(isolates, lineage, slice)
  if (identical(classical, "full")) {
    write_classical(path, plan)
  }
  write_provenance(path, plan, classical)

  message(sprintf(
    "Mock database written to %s: %d isolates, %d loci, scenarios: %s",
    path,
    n_isolates,
    n_loci,
    if (length(scenarios)) paste(scenarios, collapse = ", ") else "none"
  ))
  invisible(plan)
}
