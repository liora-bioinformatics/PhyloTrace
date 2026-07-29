# app/logic/amr.R
#
# AMR screening that runs with cgMLST / classical MLST typing.
# Each typed assembly is screened with abritamr (NCBI AMRFinderPlus wrapper)
# and results are stored in the database under the same isolate key
# (strain name = FASTA basename) used by classical_mlst and metadata.
#
# AMRFinderPlus is always invoked with --plus (acquired genes, virulence,
# stress/metal/biocide). --organism (via abritamr --species) adds resistance
# point mutations for supported species only; otherwise gene detection alone
# is performed.
#
# Tables (created lazily, same pattern as classical_mlst):
#   amr_results  – one row per element from amrfinder.out
#   amr_summary  – abritamr drug-class rollup, tidy
#                  (isolate, section, drug_class, genes)
# Provenance (tool/DB versions, point-mutation flag) is stored on every
# amr_results row.

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
box::use(
  app / logic / logging[log_event],
)

# Values accepted by abritamr --species.
# Genus-only entries allow fallback when a full binomial has no exact match.
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

# Map free-text scheme species to an abritamr --species token.
# Returns the token when supported, otherwise NA (caller omits --species).
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
  # Try Genus_species first, then genus alone.
  candidates <- c(
    if (length(words) >= 2) paste(words[1], words[2], sep = "_"),
    words[1]
  )
  hit <- candidates[candidates %in% SUPPORTED_AMR_SPECIES]
  if (length(hit)) hit[1] else NA_character_
}

# Pull abritamr / AMRFinder / DB versions from typing-log sentinel lines.
# Missing lines become NA. Mirrors parse_clamlst_meta().
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

# Safe column lookup by exact header name; returns default when column is absent.
.col <- function(df, name, default = NA) {
  if (name %in% names(df)) df[[name]] else rep(default, max(1L, nrow(df)))
}

# Parse amrfinder.out into the amr_results shape (one row per element).
# Empty data.frame when file is missing or header-only. Columns matched by name.
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

# Melt a single-sample abritamr summary_*.txt into (drug_class, genes) rows.
# Header is drug-class names (led by "Isolate"); data row holds comma-joined
# gene lists. Isolate cell is dropped; caller supplies the real isolate.
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
    # One gene per row; quality flags (*, ^) are kept.
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

# Combine the three abritamr summary files into tidy (section, drug_class, genes).
# section ∈ {matches, partials, virulence}. Empty when nothing reported.
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

# Write one strain's AMR hits and summary into the database.
# Replaces any prior rows for the isolate. If amrfinder.out is missing the
# screen never ran and existing rows are left unchanged. A clean screen with
# zero hits stores zero rows. Failure rolls back and returns FALSE; other
# typing results are unaffected.
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

  # No amrfinder.out → screen did not run; leave prior rows alone.
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
       isolate TEXT,
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
       isolate TEXT,
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
      dbExecute(
        con,
        "DELETE FROM amr_results WHERE isolate = ?",
        params = list(strain)
      )
      dbExecute(
        con,
        "DELETE FROM amr_summary WHERE isolate = ?",
        params = list(strain)
      )

      for (i in seq_len(nrow(hits))) {
        dbExecute(
          con,
          "INSERT INTO amr_results
             (isolate, gene_symbol, sequence_name, element_type, element_subtype,
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
          "INSERT INTO amr_summary (isolate, section, drug_class, genes, called_at)
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
  if (isTRUE(ok)) {
    dbCommit(con)
  } else {
    dbRollback(con)
  }

  log_event(
    "DB",
    "amr_results/amr_summary",
    sprintf(
      "isolate=%s | %d result(s), %d summary row(s) %s",
      strain,
      nrow(hits),
      nrow(summary),
      if (isTRUE(ok)) "written" else "failed (rolled back)"
    )
  )

  invisible(ok)
}
