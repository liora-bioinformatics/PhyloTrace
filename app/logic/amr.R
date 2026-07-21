# app/logic/amr.R
#
# Antimicrobial resistance (AMR) screening, riding along with the cgMLST /
# classical MLST typing run. Each freshly typed assembly is screened with
# abritamr (a curation wrapper around NCBI AMRFinderPlus, installed in the app's
# own conda env) and the results are stored in the mother database, keyed on the
# same `souche` (strain name = FASTA basename) used by `mlst` / `classical_mlst`
# / `metadata`.
#
# AMRFinderPlus always runs with `--plus`, so it reports acquired AMR genes,
# virulence factors and stress/metal/biocide elements. Passing `--organism`
# (via abritamr's `--species`) additionally calls resistance point mutations,
# but only for a fixed set of supported species. When the loaded scheme's
# species is not supported we fall back to running without it: everything except
# point mutations is still detected.
#
# Two tables are written (both created lazily, mirroring `classical_mlst`):
#   * amr_results  - one row per detected element (parsed from amrfinder.out),
#                    the granular source of truth.
#   * amr_summary  - abritamr's curated per-isolate drug-class rollup, stored
#                    tidy/long (souche, section, drug_class, genes) so it pivots
#                    trivially to the classic matrix without dynamic columns.
# Run-level provenance (tool versions, AMRFinder DB version, whether point
# mutations were enabled) is denormalised onto every amr_results row.

box::use(
  RSQLite[SQLite],
  DBI[
    dbConnect,
    dbDisconnect,
    dbExecute,
    dbBegin,
    dbCommit,
    dbRollback,
  ],
  utils[read.delim],
)

# abritamr's `--species` accepted values (from `abritamr run --help`). Some are
# genus-level (Campylobacter, Escherichia, Salmonella): a "Genus species" name
# that has no exact match is retried against the genus alone.
SUPPORTED_AMR_SPECIES <- c(
  "Acinetobacter_baumannii",
  "Burkholderia_cepacia",
  "Burkholderia_pseudomallei",
  "Burkholderia_mallei",
  "Campylobacter",
  "Citrobacter_freundii",
  "Clostridioides_difficile",
  "Corynebacterium_diphtheriae",
  "Enterobacter_asburiae",
  "Enterobacter_cloacae",
  "Enterococcus_faecalis",
  "Enterococcus_faecium",
  "Escherichia",
  "Klebsiella_oxytoca",
  "Klebsiella_pneumoniae",
  "Neisseria_gonorrhoeae",
  "Neisseria_meningitidis",
  "Pseudomonas_aeruginosa",
  "Salmonella",
  "Serratia_marcescens",
  "Staphylococcus_aureus",
  "Staphylococcus_pseudintermedius",
  "Streptococcus_agalactiae",
  "Streptococcus_pneumoniae",
  "Streptococcus_pyogenes",
  "Vibrio_cholerae",
  "Vibrio_vulfinicus",
  "Vibrio_parahaemolyticus"
)

### Map a mother-DB species string to an abritamr `--species` token
# `db_species` is the free-text species recorded for the loaded scheme (e.g.
# "Pseudomonas aeruginosa"). Returns the matching supported token so point
# mutations can be called, or NA when the species is unknown / unsupported (the
# caller then runs abritamr without `--species`, i.e. the no-point-mutation
# fallback).
#' @export
amr_species <- function(db_species) {
  if (
    is.null(db_species) ||
      length(db_species) != 1 ||
      is.na(db_species) ||
      !nzchar(trimws(db_species))
  ) {
    return(NA_character_)
  }
  words <- strsplit(trimws(db_species), "\\s+")[[1]]
  # Try "Genus_species" first, then the genus on its own (for the genus-level
  # entries in the supported set).
  candidates <- c(
    if (length(words) >= 2) paste(words[1], words[2], sep = "_"),
    words[1]
  )
  hit <- candidates[candidates %in% SUPPORTED_AMR_SPECIES]
  if (length(hit)) hit[1] else NA_character_
}

### Scrape AMR run provenance from the typing log
# The typing shell script emits the tool / database versions once (before the
# genome loop) as `AMR ... version:` sentinel lines; parsed here into a list,
# each NA when its line is absent. Mirrors `parse_clamlst_meta()`.
#' @export
parse_amr_meta <- function(log_lines) {
  log_text <- paste(log_lines, collapse = "\n")
  grab <- function(pattern) {
    match <- regmatches(log_text, regexec(pattern, log_text))[[1]]
    if (length(match) > 1) {
      value <- trimws(match[2])
      if (nzchar(value)) value else NA_character_
    } else {
      NA_character_
    }
  }
  list(
    abritamr_version = grab("AMR abritamr version:[ \t]*([^\n]+)"),
    amrfinder_version = grab("AMR finder version:[ \t]*([^\n]+)"),
    amrfinder_db_version = grab("AMR database version:[ \t]*([^\n]+)")
  )
}

# Read a value-keyed column from a data frame by exact (spaced) header name,
# tolerating its absence across AMRFinderPlus versions.
.col <- function(df, name, default = NA) {
  if (name %in% names(df)) df[[name]] else rep(default, max(1L, nrow(df)))
}

### Parse an amrfinder.out TSV into the amr_results column shape
# Returns a data frame with one row per detected element (empty when the file is
# missing or holds only its header). Column names are the long AMRFinderPlus
# headers; they are looked up by name so column order / extra columns do not
# matter.
#' @export
parse_amrfinder_out <- function(path) {
  empty <- data.frame(
    gene_symbol = character(0),
    sequence_name = character(0),
    element_type = character(0),
    element_subtype = character(0),
    class = character(0),
    subclass = character(0),
    method = character(0),
    pct_coverage = numeric(0),
    pct_identity = numeric(0),
    contig = character(0),
    start = integer(0),
    stop = integer(0),
    strand = character(0),
    ref_accession = character(0),
    ref_name = character(0),
    stringsAsFactors = FALSE
  )
  if (is.null(path) || !file.exists(path)) {
    return(empty)
  }
  df <- tryCatch(
    read.delim(
      path,
      sep = "\t",
      header = TRUE,
      quote = "",
      check.names = FALSE,
      stringsAsFactors = FALSE,
      na.strings = c("NA", "")
    ),
    error = function(e) NULL
  )
  if (is.null(df) || !nrow(df)) {
    return(empty)
  }
  data.frame(
    gene_symbol = as.character(.col(df, "Gene symbol")),
    sequence_name = as.character(.col(df, "Sequence name")),
    element_type = as.character(.col(df, "Element type")),
    element_subtype = as.character(.col(df, "Element subtype")),
    class = as.character(.col(df, "Class")),
    subclass = as.character(.col(df, "Subclass")),
    method = as.character(.col(df, "Method")),
    pct_coverage = suppressWarnings(as.numeric(
      .col(df, "% Coverage of reference sequence")
    )),
    pct_identity = suppressWarnings(as.numeric(
      .col(df, "% Identity to reference sequence")
    )),
    contig = as.character(.col(df, "Contig id")),
    start = suppressWarnings(as.integer(.col(df, "Start"))),
    stop = suppressWarnings(as.integer(.col(df, "Stop"))),
    strand = as.character(.col(df, "Strand")),
    ref_accession = as.character(.col(df, "Accession of closest sequence")),
    ref_name = as.character(.col(df, "Name of closest sequence")),
    stringsAsFactors = FALSE
  )
}

# Melt one abritamr summary_*.txt (single-sample: a header row of drug-class /
# group names led by "Isolate", then one data row of comma-joined gene lists).
# The "Isolate" cell holds abritamr's own label (the output prefix path), so it
# is dropped - the caller supplies the real souche. Returns (drug_class, genes)
# for every non-empty cell.
.melt_summary <- function(path) {
  empty <- data.frame(
    drug_class = character(0),
    genes = character(0),
    stringsAsFactors = FALSE
  )
  if (is.null(path) || !file.exists(path)) {
    return(empty)
  }
  df <- tryCatch(
    read.delim(
      path,
      sep = "\t",
      header = TRUE,
      quote = "",
      check.names = FALSE,
      stringsAsFactors = FALSE
    ),
    error = function(e) NULL
  )
  if (is.null(df) || !nrow(df)) {
    return(empty)
  }
  cols <- setdiff(names(df), "Isolate")
  out <- lapply(cols, function(cl) {
    val <- trimws(as.character(df[[cl]][1]))
    if (is.na(val) || !nzchar(val)) {
      return(NULL)
    }
    # abritamr packs every gene it found for a drug class into one comma-joined
    # cell (e.g. "mexE,mexX*"). Split it so each gene becomes its own row - the
    # amr_summary table stays tidy (one gene per row), keeping its `*`/`^` quality
    # flags intact.
    genes <- trimws(strsplit(val, ",", fixed = TRUE)[[1]])
    genes <- genes[nzchar(genes)]
    if (!length(genes)) {
      return(NULL)
    }
    data.frame(drug_class = cl, genes = genes, stringsAsFactors = FALSE)
  })
  out <- do.call(rbind, out)
  if (is.null(out)) empty else out
}

### Parse abritamr's three summary files into the tidy amr_summary shape
# Returns a data frame(section, drug_class, genes) with section in
# matches / partials / virulence; empty when nothing was reported.
#' @export
parse_abritamr_summary <- function(dir) {
  files <- c(
    matches = "summary_matches.txt",
    partials = "summary_partials.txt",
    virulence = "summary_virulence.txt"
  )
  parts <- lapply(names(files), function(section) {
    melted <- .melt_summary(file.path(dir, files[[section]]))
    if (!nrow(melted)) {
      return(NULL)
    }
    cbind(section = section, melted, stringsAsFactors = FALSE)
  })
  parts <- do.call(rbind, parts)
  if (is.null(parts)) {
    data.frame(
      section = character(0),
      drug_class = character(0),
      genes = character(0),
      stringsAsFactors = FALSE
    )
  } else {
    parts
  }
}

### Persist one strain's AMR screen into the mother database
# Reads `amr_dir/amrfinder.out` (granular hits) and `amr_dir/summary_*.txt`
# (curated rollup) and writes them to `amr_results` / `amr_summary`, both keyed
# on `souche = strain`. A strain's existing rows are replaced so re-screening
# never duplicates them. When amrfinder.out is absent the screen did not run for
# this strain, so nothing is touched (existing rows are preserved). A strain that
# was screened but had no hits ends up with no rows - matching how a genome with
# no called classical loci is stored. Best-effort: any failure rolls back and
# returns FALSE without affecting cgMLST / classical MLST results.
#' @export
store_amr_results <- function(
  db_path,
  strain,
  amr_dir,
  abritamr_version = NA_character_,
  amrfinder_version = NA_character_,
  amrfinder_db_version = NA_character_,
  organism = NA_character_,
  point_mutations = FALSE,
  identity = NA_real_
) {
  if (
    is.null(db_path) ||
      length(db_path) != 1 ||
      is.na(db_path) ||
      !file.exists(db_path) ||
      is.null(strain) ||
      length(strain) != 1 ||
      is.na(strain) ||
      !nzchar(strain) ||
      is.null(amr_dir) ||
      is.na(amr_dir) ||
      !nzchar(amr_dir)
  ) {
    return(invisible(FALSE))
  }

  # amrfinder.out is the proof the screen actually ran; without it, leave any
  # existing rows for this strain untouched.
  amrfinder_out <- file.path(amr_dir, "amrfinder.out")
  if (!file.exists(amrfinder_out)) {
    return(invisible(FALSE))
  }

  hits <- parse_amrfinder_out(amrfinder_out)
  summary <- parse_abritamr_summary(amr_dir)
  pm <- if (isTRUE(point_mutations)) 1L else 0L

  con <- dbConnect(SQLite(), db_path, busy_timeout = 5000)
  on.exit(dbDisconnect(con))

  dbExecute(
    con,
    "CREATE TABLE IF NOT EXISTS amr_results (
       id INTEGER PRIMARY KEY AUTOINCREMENT,
       souche TEXT,
       gene_symbol TEXT,
       sequence_name TEXT,
       element_type TEXT,
       element_subtype TEXT,
       class TEXT,
       subclass TEXT,
       method TEXT,
       pct_coverage REAL,
       pct_identity REAL,
       contig TEXT,
       start INTEGER,
       stop INTEGER,
       strand TEXT,
       ref_accession TEXT,
       ref_name TEXT,
       organism TEXT,
       point_mutations INTEGER,
       identity_threshold REAL,
       abritamr_version TEXT,
       amrfinder_version TEXT,
       amrfinder_db_version TEXT,
       called_at TEXT
     )"
  )
  dbExecute(
    con,
    "CREATE TABLE IF NOT EXISTS amr_summary (
       id INTEGER PRIMARY KEY AUTOINCREMENT,
       souche TEXT,
       section TEXT,
       drug_class TEXT,
       genes TEXT,
       called_at TEXT
     )"
  )

  now <- as.character(Sys.time())
  dbBegin(con)
  ok <- tryCatch(
    {
      # Refresh this strain's rows so re-screening never duplicates them.
      dbExecute(
        con,
        "DELETE FROM amr_results WHERE souche = ?",
        params = list(strain)
      )
      dbExecute(
        con,
        "DELETE FROM amr_summary WHERE souche = ?",
        params = list(strain)
      )

      for (i in seq_len(nrow(hits))) {
        dbExecute(
          con,
          "INSERT INTO amr_results
             (souche, gene_symbol, sequence_name, element_type, element_subtype,
              class, subclass, method, pct_coverage, pct_identity, contig,
              start, stop, strand, ref_accession, ref_name, organism,
              point_mutations, identity_threshold, abritamr_version,
              amrfinder_version, amrfinder_db_version, called_at)
           VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)",
          params = list(
            strain,
            hits$gene_symbol[i],
            hits$sequence_name[i],
            hits$element_type[i],
            hits$element_subtype[i],
            hits$class[i],
            hits$subclass[i],
            hits$method[i],
            hits$pct_coverage[i],
            hits$pct_identity[i],
            hits$contig[i],
            hits$start[i],
            hits$stop[i],
            hits$strand[i],
            hits$ref_accession[i],
            hits$ref_name[i],
            organism,
            pm,
            identity,
            abritamr_version,
            amrfinder_version,
            amrfinder_db_version,
            now
          )
        )
      }

      for (i in seq_len(nrow(summary))) {
        dbExecute(
          con,
          "INSERT INTO amr_summary (souche, section, drug_class, genes, called_at)
           VALUES (?, ?, ?, ?, ?)",
          params = list(
            strain,
            summary$section[i],
            summary$drug_class[i],
            summary$genes[i],
            now
          )
        )
      }
      TRUE
    },
    error = function(e) FALSE
  )
  if (isTRUE(ok)) dbCommit(con) else dbRollback(con)

  invisible(ok)
}
