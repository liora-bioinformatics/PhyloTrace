# dev/mock_database.R
#
# Generates an artificial PhyloTrace database without running typing: a
# cgMLST allele table with lineage structure, assembly hashes, classical MLST
# calls and typing provenance, plus optional scenarios (mixed schemes, failed
# calls, incomplete profiles, duplicate assemblies, ...) that are hard to
# reproduce from real genomes. The result opens in the app like any
# other database and its tables can be edited further by hand.
#
# Only the pyMLST core tables are written here. Everything PhyloTrace itself
# writes goes through the app's own writers or table definitions
# (sync_metadata_table(), store_clamlst_results(), PROVENANCE_DDL,
# GENOME_HASHES_DDL, hash_database()), so a mock database has exactly the shape
# a typed one has. AMR screens are written as AMRFinderPlus/abritamr output
# files and stored through store_amr_results(), the path a typing run takes.
#
# Usage (from the project root):
#   Rscript -e 'box::use(dev/mock_database); mock_database$mock_database("~/mock.db")'
#   Rscript -e 'box::use(dev/mock_database)
#     mock_database$mock_database("~/mock.db", scenarios = c("mixed_schemes", "failed"))'
# For realistic locations and dates afterwards: Rscript dev/enrich_metadata.R ~/mock.db

box::use(
  DBI[dbAppendTable, dbDisconnect, dbExecute, dbReadTable, dbWriteTable],
  stats[rexp, rpois, runif, setNames],
  utils[write.table],
)
box::use(
  app / logic / amr[store_amr_results],
  app / logic / app_meta[APP_VERSION],
  app / logic / database_functions[sync_metadata_table],
  app / logic / db_connect[connect],
  app / logic / genome_hash[GENOME_HASHES_DDL, GENOME_HASH_ALGORITHM],
  app / logic / provenance[PROVENANCE_DDL, scheme_provenance],
  app / logic / pymlst[hash_database, store_clamlst_results],
)

#' Mock Database Scenarios
#'
#' @description Every scenario `mock_database()` can apply, with what it does.
#'   All but the last five each claim their own slice of isolates, so any
#'   combination can be applied at once; the isolate-record ones
#'   (`RECORD_SCENARIOS`) slice the isolates a second time, independently of
#'   the rest. `orphan_records` adds records of
#'   isolates that are not in the database; `skipped` and `no_classical`
#'   replace classical MLST for the whole database, `amr_skipped` and `no_amr`
#'   AMR screening, and each overrides the slices of what it replaces.
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
  low_completeness = "Some isolates with 6-15% of the cgMLST loci missing",
  duplicate_assemblies = "Some isolates typed from an assembly identical to another's",
  mixed_cg_thresholds = "Some isolates whose alleles were called at other thresholds",
  scheme_version_change = "Some isolates typed against an older cgMLST scheme version",
  unusual_assemblies = "Some isolates with an inflated or truncated, fragmented assembly",
  amr_mixed_db = "Some isolates screened against an older AMRFinderPlus database",
  amr_failed = "Some isolates whose AMR screen failed",
  amr_unscreened = "Some isolates without any AMR screening record",
  amr_no_point_mutations = "Some isolates screened without point mutations",
  amr_results_lost = "Some isolates with fewer AMR elements stored than reported",
  assembly_changed = "Some isolates whose assembly record was replaced after typing",
  long_read_assemblies = "Some isolates with complete long-read assemblies (1-8 contigs)",
  no_assembly_record = "Some isolates without an assembly record (typed before hashing)",
  imported = "Some isolates imported from another database, with its own run and version",
  orphan_records = "Assembly and provenance records of isolates no longer in the database",
  skipped = "No scheme available: classical MLST skipped for every isolate",
  no_classical = "No classical MLST tables at all (database typed before it existed)",
  amr_skipped = "AMR screening switched off for every isolate",
  no_amr = "No AMR tables at all (database typed before screening existed)"
)

# Share of isolates each slice scenario claims.
SLICE_SHARE <- c(
  mixed_schemes = 0.08,
  mixed_releases = 0.18,
  mixed_thresholds = 0.08,
  mixed_pymlst = 0.06,
  ambiguous = 0.04,
  calls_without_alleles = 0.04,
  failed = 0.04,
  untyped = 0.04,
  provenance_mismatch = 0.04,
  low_completeness = 0.04,
  duplicate_assemblies = 0.04,
  mixed_cg_thresholds = 0.06,
  scheme_version_change = 0.06,
  unusual_assemblies = 0.04,
  amr_mixed_db = 0.03,
  amr_failed = 0.03,
  amr_unscreened = 0.03,
  amr_no_point_mutations = 0.03,
  amr_results_lost = 0.03,
  assembly_changed = 0.03,
  long_read_assemblies = 0.06,
  no_assembly_record = 0.04,
  imported = 0.08
)

# Slice scenarios that concern AMR screening.
AMR_SCENARIOS <- c(
  "amr_mixed_db",
  "amr_failed",
  "amr_unscreened",
  "amr_no_point_mutations",
  "amr_results_lost"
)

# Element catalogue of the mock screens: AMRFinderPlus's own vocabulary, as it
# writes it (block capitals, `class` only where it gives one).
AMR_CATALOGUE <- data.frame(
  gene = c(
    "blaTEM-1", "blaCTX-M-15", "aph(3'')-Ib", "tet(A)", "sul2", "dfrA14",
    "qnrS1", "floR", "mph(A)", "fosA",
    "gyrA_S83L", "parC_S80I", "pmrB_R256G",
    "fimH", "iutA", "iucA", "ybtS", "clbB",
    "arsB", "merA", "qacEdelta1", "clpK", "silE"
  ),
  type = c(rep("AMR", 13), rep("VIRULENCE", 5), rep("STRESS", 5)),
  subtype = c(
    rep("AMR", 10), rep("POINT", 3), rep("VIRULENCE", 5),
    "METAL", "METAL", "BIOCIDE", "HEAT", "METAL"
  ),
  class = c(
    "BETA-LACTAM", "BETA-LACTAM", "AMINOGLYCOSIDE", "TETRACYCLINE", "SULFONAMIDE",
    "TRIMETHOPRIM", "QUINOLONE", "PHENICOL", "MACROLIDE", "FOSFOMYCIN",
    "QUINOLONE", "QUINOLONE", "COLISTIN",
    rep(NA, 5),
    "ARSENIC", "MERCURY", "QUATERNARY AMMONIUM", NA, "SILVER"
  ),
  subclass = c(
    "BETA-LACTAM", "CEPHALOSPORIN", "STREPTOMYCIN", "TETRACYCLINE", "SULFONAMIDE",
    "TRIMETHOPRIM", "QUINOLONE", "CHLORAMPHENICOL/FLORFENICOL", "AZITHROMYCIN",
    "FOSFOMYCIN", "QUINOLONE", "QUINOLONE", "COLISTIN",
    rep(NA, 5),
    "ARSENITE", "MERCURY", "QUATERNARY AMMONIUM", NA, "SILVER"
  ),
  name = c(
    "class A broad-spectrum beta-lactamase TEM-1",
    "class A extended-spectrum beta-lactamase CTX-M-15",
    "aminoglycoside O-phosphotransferase APH(3'')-Ib",
    "tetracycline efflux MFS transporter Tet(A)",
    "sulfonamide-resistant dihydropteroate synthase Sul2",
    "trimethoprim-resistant dihydrofolate reductase DfrA14",
    "quinolone resistance pentapeptide repeat protein QnrS1",
    "chloramphenicol/florfenicol efflux MFS transporter FloR",
    "Mph(A) family macrolide 2'-phosphotransferase",
    "fosfomycin resistance glutathione transferase FosA",
    "DNA gyrase subunit A GyrA", "DNA topoisomerase IV subunit A ParC",
    "two-component system sensor histidine kinase PmrB",
    "type 1 fimbria D-mannose specific adhesin FimH",
    "ferric aerobactin receptor IutA", "aerobactin synthase IucA",
    "yersiniabactin biosynthesis salicylate synthase YbtS",
    "colibactin polyketide synthase ClbB",
    "arsenical efflux pump membrane protein ArsB",
    "mercury(II) reductase MerA", "quaternary ammonium compound efflux SMR transporter QacEdelta1",
    "heat resistance chaperone ClpK", "silver-binding protein SilE"
  ),
  stringsAsFactors = FALSE
)

AMR_ORGANISM <- "Testus_organismus"
AMR_DB_VERSION <- "2024-07-22.1"
AMR_OLD_DB_VERSION <- "2023-11-15.1"
AMR_TOOLS <- c(abritamr = "1.0.19", amrfinder = "3.12.8")

# Slice scenarios that concern cgMLST or the isolate records, and so still
# apply when classical MLST is replaced for the whole database.
CG_SCENARIOS <- c(
  "low_completeness",
  "duplicate_assemblies",
  "mixed_cg_thresholds",
  "scheme_version_change",
  "unusual_assemblies",
  "assembly_changed",
  "long_read_assemblies",
  "no_assembly_record",
  "imported"
)

# Slice scenarios that concern the isolate records - origin and assembly
# record. They touch nothing the other scenarios do, so they slice the isolates
# a second time on their own, sparing those whose assembly or provenance
# another scenario already rewrites.
RECORD_SCENARIOS <- c("assembly_changed", "long_read_assemblies", "no_assembly_record", "imported")
RECORD_SPARED <- c("untyped", "duplicate_assemblies", "unusual_assemblies")

# Where `imported` isolates came from: the peer's source label, the run that
# typed them there and the PhyloTrace version it ran.
PEER <- list(
  source = "partner-lab",
  run_id = "typing_20250611_090000_partner.log",
  typed_at = "2025-06-11 09:00:00",
  version = "1.5.0"
)

# Isolates whose records outlived them under `orphan_records`.
ORPHANS <- c("MOCK_GONE_1", "MOCK_GONE_2")

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
CG_IDENTITY <- 0.95
CG_COVERAGE <- 0.9
SCHEME_VERSION <- "1.0"
GENOME_LENGTH <- 3.1e6

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
# missing locus pyMLST leaves out of `mlst` entirely. A `low_completeness`
# isolate misses many more; a `duplicate_assemblies` one repeats its `twin`'s
# profile exactly, as typing the same assembly twice does.
cg_alleles <- function(isolates, lineage, n_loci, slice, twin) {
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
    # An exact share, so even a small scheme always loses some loci.
    if (identical(slice[[i]], "low_completeness")) {
      kept[sample(n_loci, max(1L, round(n_loci * runif(1, 0.06, 0.15))))] <- FALSE
    }
    data.frame(souche = isolates[i], gene = loci[kept], sequence = seqs[kept])
  })
  for (i in which(!is.na(twin))) {
    rows[[i]] <- rows[[twin[i]]]
    rows[[i]]$souche <- isolates[i]
  }
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
    "INSERT INTO mlst_type VALUES ('wg', 'mock', ?, ?)",
    params = list(species, SCHEME_VERSION)
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
        paste(species, "cgMLST (mock)"), "PhyloTrace mock database", SCHEME_VERSION,
        parts[1], paste(parts[-1], collapse = " "), as.character(nrow(ref)), "10"
      )
    )
  )
}

# Fills a few metadata fields so filters and plots have something to show, and
# stamps `imported` isolates with the peer's source label as a merge does.
fill_metadata <- function(con, isolates, record) {
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
  meta$source[meta$isolate %in% isolates[record %in% "imported"]] <- PEER$source
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

# One isolate's AMR screen as AMRFinderPlus would report it: the lineage's
# elements (each occasionally lost, the odd one gained) with a call method,
# identity and coverage, placed on contigs.
amr_elements <- function(lineage_set, point_mutations) {
  keep <- lineage_set[runif(length(lineage_set)) > 0.05]
  extra <- which(runif(nrow(AMR_CATALOGUE)) < 0.02)
  el <- AMR_CATALOGUE[sort(unique(c(keep, extra))), , drop = FALSE]
  if (!point_mutations) {
    el <- el[el$subtype != "POINT", , drop = FALSE]
  }
  n <- nrow(el)
  if (!n) {
    return(el)
  }
  roll <- runif(n)
  el$method <- ifelse(
    el$subtype == "POINT",
    "POINTX",
    as.character(cut(
      roll,
      c(0, 0.5, 0.7, 0.9, 0.94, 0.97, 1),
      c("EXACTX", "ALLELEX", "BLASTX", "PARTIALX", "PARTIAL_CONTIG_ENDX", "INTERNAL_STOP")
    ))
  )
  exact <- el$method %in% c("EXACTX", "ALLELEX")
  partial <- grepl("PARTIAL", el$method)
  el$identity <- ifelse(exact, 100, round(runif(n, 95, 99.9), 2))
  el$coverage <- ifelse(partial, round(runif(n, 40, 95), 2), 100)
  el$contig <- sprintf("contig_%d", sample(1:40, n, replace = TRUE))
  el$start <- sample(1000:250000, n)
  el$stop <- el$start + sample(600:2400, n)
  el$strand <- sample(c("+", "-"), n, replace = TRUE)
  el$accession <- sprintf("WP_%09d.1", sample(1e8:9e8, n))
  el
}

# abritamr's single-sample summary file: one column per class, the genes of a
# class comma-joined in its one row.
write_summary <- function(path, isolate, groups, genes) {
  if (!length(genes)) {
    return(invisible(NULL))
  }
  joined <- tapply(genes, groups, paste, collapse = ",")
  row <- as.data.frame(as.list(c(Isolate = isolate, joined)), check.names = FALSE)
  write.table(row, path, sep = "\t", quote = FALSE, row.names = FALSE)
}

# Screens every isolate the scenario has screened and stores each through the
# app's own writer; returns what provenance records about each screen.
write_amr <- function(path, isolates, lineage, slice, mode) {
  n <- length(isolates)
  screens <- data.frame(
    isolate = isolates,
    amr_status = if (identical(mode, "skipped")) "skipped" else NA_character_,
    amr_organism = NA_character_,
    amr_point_mutations = NA_integer_,
    amr_elements = NA_integer_,
    amr_abritamr_version = NA_character_,
    amr_amrfinder_version = NA_character_,
    amr_amrfinder_db_version = NA_character_,
    stringsAsFactors = FALSE
  )
  if (!identical(mode, "full")) {
    return(screens)
  }
  sets <- lapply(seq_len(max(lineage)), function(i) {
    p <- ifelse(AMR_CATALOGUE$gene == "fosA", 0.95, 0.35)
    which(runif(nrow(AMR_CATALOGUE)) < p)
  })
  scratch <- tempfile("mock_amr_")
  on.exit(unlink(scratch, recursive = TRUE), add = TRUE)
  is <- function(sc) slice %in% sc

  for (i in seq_len(n)) {
    if (is("amr_unscreened")[i]) {
      next
    }
    if (is("amr_failed")[i]) {
      screens$amr_status[i] <- "failed"
      next
    }
    pm <- !is("amr_no_point_mutations")[i]
    db <- if (is("amr_mixed_db")[i]) AMR_OLD_DB_VERSION else AMR_DB_VERSION
    el <- amr_elements(sets[[lineage[i]]], pm)
    dir <- file.path(scratch, isolates[i])
    dir.create(dir, recursive = TRUE)
    out <- data.frame(
      `Contig id` = el$contig,
      Start = el$start,
      Stop = el$stop,
      Strand = el$strand,
      `Gene symbol` = el$gene,
      `Sequence name` = el$name,
      `Element type` = el$type,
      `Element subtype` = el$subtype,
      Class = el$class,
      Subclass = el$subclass,
      Method = el$method,
      `% Coverage of reference sequence` = el$coverage,
      `% Identity to reference sequence` = el$identity,
      `Accession of closest sequence` = el$accession,
      `Name of closest sequence` = el$name,
      check.names = FALSE,
      stringsAsFactors = FALSE
    )
    write.table(
      out,
      file.path(dir, "amrfinder.out"),
      sep = "\t",
      quote = FALSE,
      row.names = FALSE,
      na = ""
    )
    resistance <- el$type == "AMR"
    partial <- grepl("PARTIAL", el$method)
    write_summary(
      file.path(dir, "summary_matches.txt"),
      isolates[i],
      el$class[resistance & !partial],
      el$gene[resistance & !partial]
    )
    write_summary(
      file.path(dir, "summary_partials.txt"),
      isolates[i],
      el$class[resistance & partial],
      el$gene[resistance & partial]
    )
    write_summary(
      file.path(dir, "summary_virulence.txt"),
      isolates[i],
      ifelse(is.na(el$class[!resistance]), el$subtype[!resistance], el$class[!resistance]),
      el$gene[!resistance]
    )
    store_amr_results(
      path,
      isolates[i],
      dir,
      abritamr_version = AMR_TOOLS[["abritamr"]],
      amrfinder_version = AMR_TOOLS[["amrfinder"]],
      amrfinder_db_version = db,
      organism = if (pm) AMR_ORGANISM else NA_character_,
      point_mutations = pm
    )
    screens$amr_status[i] <- "done"
    screens$amr_organism[i] <- if (pm) AMR_ORGANISM else NA_character_
    screens$amr_point_mutations[i] <- as.integer(pm)
    # A lost screen reported more elements than reached the database.
    screens$amr_elements[i] <- nrow(el) + if (is("amr_results_lost")[i]) 2L else 0L
    screens$amr_abritamr_version[i] <- AMR_TOOLS[["abritamr"]]
    screens$amr_amrfinder_version[i] <- AMR_TOOLS[["amrfinder"]]
    screens$amr_amrfinder_db_version[i] <- db
  }
  screens
}

# The isolate each `duplicate_assemblies` isolate copies, NA for the rest.
assign_twins <- function(isolates, slice) {
  dups <- which(slice %in% "duplicate_assemblies")
  others <- which(!(slice %in% "duplicate_assemblies"))
  twin <- rep(NA_integer_, length(isolates))
  twin[dups] <- others[sample(length(others), length(dups), replace = TRUE)]
  twin
}

# One assembly hash row per isolate, in the app's own table definition: a
# typical length and contig count, a duplicate sharing its twin's assembly,
# `unusual_assemblies` alternately inflated (contamination) or truncated and
# `long_read_assemblies` closed into a handful of contigs. `no_assembly_record`
# isolates get no row; the returned table still holds theirs.
write_hashes <- function(path, isolates, slice, twin, record) {
  n <- length(isolates)
  hashes <- data.frame(
    isolate = isolates,
    genome_digest = vapply(
      seq_len(n),
      function(i) paste(sample(c(0:9, letters[1:6]), 32, replace = TRUE), collapse = ""),
      character(1)
    ),
    algorithm = GENOME_HASH_ALGORITHM,
    n_contigs = as.integer(round(runif(n, 30, 90))),
    total_length = as.integer(round(GENOME_LENGTH * runif(n, 0.98, 1.02))),
    hashed_at = "2026-09-30 10:00:00"
  )
  odd <- which(slice %in% "unusual_assemblies")
  inflated <- odd[seq_along(odd) %% 2 == 1]
  hashes$total_length[inflated] <- as.integer(hashes$total_length[inflated] * 1.5)
  hashes$total_length[setdiff(odd, inflated)] <- as.integer(
    hashes$total_length[setdiff(odd, inflated)] * 0.7
  )
  hashes$n_contigs[odd] <- hashes$n_contigs[odd] * 6L
  closed <- which(record %in% "long_read_assemblies")
  hashes$n_contigs[closed] <- 1L + (seq_along(closed) - 1L) %% 8L
  copies <- which(!is.na(twin))
  hashes[copies, c("genome_digest", "n_contigs", "total_length")] <-
    hashes[twin[copies], c("genome_digest", "n_contigs", "total_length")]
  hashes$file_bytes <- as.integer(hashes$total_length * 1.02)

  con <- connect(path)
  on.exit(dbDisconnect(con))
  dbExecute(con, GENOME_HASHES_DDL)
  dbAppendTable(con, "genome_hashes", hashes[!(record %in% "no_assembly_record"), ])
  hashes
}

# One provenance row per typed isolate, in the app's own table definition,
# with the allele-calling figures a run records next to the classical step.
# Isolates of one scheme release were typed in one run on its release day.
write_provenance <- function(path, plan, classical, hashes, loci, n_loci, screens, record) {
  rows <- plan[plan$in_prov, , drop = FALSE]
  rec <- record[match(rows$isolate, plan$isolate)]
  cg <- scheme_provenance(path)
  prov <- data.frame(
    isolate = rows$isolate,
    run_id = sprintf("typing_%s_100000_mock.log", gsub("-", "", rows$version)),
    typed_at = paste(rows$version, "10:00:00"),
    phylotrace_version = APP_VERSION,
    cg_scheme_database = cg$cg_scheme_database,
    cg_scheme_version = cg$cg_scheme_version,
    cg_species = cg$cg_species,
    cg_locus_count = cg$cg_locus_count,
    pymlst_version = rows$pymlst,
    blat_version = "35",
    mafft_version = "v7.525"
  )
  at <- match(rows$isolate, hashes$isolate)
  # Typed before hashing: no assembly figures were recorded either.
  hashed <- !(rec %in% "no_assembly_record")
  prov$genome_digest <- ifelse(hashed, hashes$genome_digest[at], NA)
  prov$algorithm <- ifelse(hashed, hashes$algorithm[at], NA)
  prov$file_sha256 <- NA_character_
  prov$n_contigs <- ifelse(hashed, hashes$n_contigs[at], NA)
  prov$total_length <- ifelse(hashed, hashes$total_length[at], NA)
  # The assembly typed was another than the one on record now.
  changed <- rec %in% "assembly_changed"
  prov$genome_digest[changed] <- sprintf("replaced%024d", seq_len(sum(changed)))
  found <- unname(loci[rows$isolate])
  prov$cg_loci_found <- found
  prov$cg_completeness <- round(100 * found / n_loci, 1)
  prov$cg_identity <- ifelse(rows$scenario %in% "mixed_cg_thresholds", 0.9, CG_IDENTITY)
  prov$cg_coverage <- ifelse(rows$scenario %in% "mixed_cg_thresholds", 0.85, CG_COVERAGE)
  prov$cg_scheme_version[rows$scenario %in% "scheme_version_change"] <- "0.9"
  prov$cg_alleles_added <- rpois(nrow(rows), 2)
  prov$cg_partial_genes <- rpois(nrow(rows), 3)
  prov$cg_filled_genes <- pmin(prov$cg_partial_genes, rpois(nrow(rows), 2))
  prov$cg_removed_genes <- prov$cg_partial_genes - prov$cg_filled_genes
  prov$elapsed_seconds <- round(runif(nrow(rows), 25, 70), 1)
  peer <- rec %in% "imported"
  prov$run_id[peer] <- PEER$run_id
  prov$typed_at[peer] <- PEER$typed_at
  prov$phylotrace_version[peer] <- PEER$version
  at_screen <- match(rows$isolate, screens$isolate)
  for (col in c(
    "amr_status", "amr_organism", "amr_point_mutations", "amr_elements",
    "amr_abritamr_version", "amr_amrfinder_version", "amr_amrfinder_db_version"
  )) {
    prov[[col]] <- screens[[col]][at_screen]
  }
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

# Assembly and provenance rows of isolates that are not in the database, as
# when the file is edited outside PhyloTrace - the app's own removal never
# leaves any behind.
write_orphans <- function(path) {
  con <- connect(path)
  on.exit(dbDisconnect(con))
  n <- length(ORPHANS)
  digests <- sprintf("orphan%026d", seq_len(n))
  dbAppendTable(
    con,
    "genome_hashes",
    data.frame(
      isolate = ORPHANS,
      genome_digest = digests,
      algorithm = GENOME_HASH_ALGORITHM,
      n_contigs = 50L,
      total_length = as.integer(GENOME_LENGTH),
      hashed_at = "2025-01-15 10:00:00"
    )
  )
  dbAppendTable(
    con,
    "typing_provenance",
    data.frame(
      isolate = ORPHANS,
      run_id = "typing_20250115_100000_mock.log",
      typed_at = "2025-01-15 10:00:00",
      genome_digest = digests,
      algorithm = GENOME_HASH_ALGORITHM
    )
  )
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
#' @return Invisibly, the per-isolate plan: scenario, isolate-record scenario
#'   (`record`), classical status, ST and the scheme release each isolate was
#'   given.
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
  classical <- if ("no_classical" %in% scenarios) {
    "none"
  } else if ("skipped" %in% scenarios) {
    "skipped"
  } else {
    "full"
  }
  amr <- if ("no_amr" %in% scenarios) {
    "none"
  } else if ("amr_skipped" %in% scenarios) {
    "skipped"
  } else {
    "full"
  }
  sliced <- scenarios
  if (!identical(classical, "full")) {
    sliced <- intersect(sliced, c(CG_SCENARIOS, AMR_SCENARIOS))
  }
  if (!identical(amr, "full")) {
    sliced <- setdiff(sliced, AMR_SCENARIOS)
  }
  slice <- allocate_slices(isolates, setdiff(sliced, RECORD_SCENARIOS))
  record <- rep(NA_character_, n_isolates)
  if (any(RECORD_SCENARIOS %in% sliced)) {
    open <- !(slice %in% RECORD_SPARED)
    record[open] <- allocate_slices(isolates[open], intersect(sliced, RECORD_SCENARIOS))
  }
  twin <- assign_twins(isolates, slice)

  calls <- cg_alleles(isolates, lineage, n_loci, slice, twin)
  con <- connect(path, create = TRUE)
  write_core(con, calls, species)
  dbDisconnect(con)
  hash_database(path)
  sync_metadata_table(path)
  con <- connect(path)
  fill_metadata(con, isolates, record)
  dbDisconnect(con)
  hashes <- write_hashes(path, isolates, slice, twin, record)

  plan <- classical_plan(isolates, lineage, slice)
  if (identical(classical, "full")) {
    write_classical(path, plan)
  }
  loci <- table(calls$souche[calls$souche != "ref"])
  screens <- write_amr(path, isolates, lineage, slice, amr)
  write_provenance(path, plan, classical, hashes, loci, n_loci, screens, record)
  if ("orphan_records" %in% scenarios) {
    write_orphans(path)
  }

  message(sprintf(
    "Mock database written to %s: %d isolates, %d loci, scenarios: %s",
    path,
    n_isolates,
    n_loci,
    if (length(scenarios)) paste(scenarios, collapse = ", ") else "none"
  ))
  plan$record <- record
  invisible(plan)
}
