# app/logic/provenance.R
#
# One row per isolate recording how it was typed: the assembly it came from, the
# scheme and thresholds each analysis ran against, the tool versions behind
# them, and what allele calling made of it.
#
# The same facts are scattered across `genome_hashes`, `classical_mlst`,
# `amr_results` and the run log, several of them repeated on every allele row
# and none of them keyed together. This table is the one place a finished
# isolate can be traced back to the software and reference data that produced
# it, and it is written per isolate as the run goes, alongside the results
# themselves.
#
# The scheme columns are deliberately denormalized: they are a snapshot of what
# was true when the isolate was typed, so a later scheme refresh or complex-type
# recomputation cannot rewrite the history of a call, and an exported subset of
# isolates carries its own context with it.

box::use(
  DBI[dbDisconnect, dbExecute, dbGetQuery, dbListTables],
)
box::use(
  app / logic / db_connect[connect],
  app / logic / logging[log_event],
)

#' Typing Provenance Table Definition
#'
#' @description DDL of the isolate-keyed provenance table, created on first use.
#'
#' @return Character string containing the `CREATE TABLE` statement.
#' @export
PROVENANCE_DDL <- "CREATE TABLE IF NOT EXISTS typing_provenance (
       isolate TEXT PRIMARY KEY,
       run_id TEXT,
       typed_at TEXT,
       elapsed_seconds REAL,
       phylotrace_version TEXT,
       genome_digest TEXT,
       file_sha256 TEXT,
       algorithm TEXT,
       n_contigs INTEGER,
       total_length INTEGER,
       file_bytes INTEGER,
       cg_scheme_database TEXT,
       cg_scheme_version TEXT,
       cg_seed_genome TEXT,
       cg_genus TEXT,
       cg_species TEXT,
       cg_locus_count INTEGER,
       cg_complex_type_distance INTEGER,
       cg_complex_type_count INTEGER,
       cg_identity REAL,
       cg_coverage REAL,
       cg_loci_found INTEGER,
       cg_alleles_added INTEGER,
       cg_partial_genes INTEGER,
       cg_filled_genes INTEGER,
       cg_removed_genes INTEGER,
       cg_completeness REAL,
       cla_scheme TEXT,
       cla_scheme_version TEXT,
       cla_alembic_version TEXT,
       cla_repository TEXT,
       cla_identity REAL,
       cla_coverage REAL,
       amr_organism TEXT,
       amr_point_mutations INTEGER,
       amr_elements INTEGER,
       amr_abritamr_version TEXT,
       amr_amrfinder_version TEXT,
       amr_amrfinder_db_version TEXT,
       pymlst_version TEXT,
       blat_version TEXT,
       mafft_version TEXT
     )"

# Column names the table accepts, parsed straight out of its own definition so
# the two can never drift apart. Anything else handed to store_provenance() is a
# caller mistake and is dropped rather than pasted into SQL.
PROVENANCE_COLUMNS <- local({
  body <- sub("^[^(]*\\(", "", PROVENANCE_DDL)
  lines <- trimws(strsplit(body, ",", fixed = TRUE)[[1]])
  sub("[[:space:]].*$", "", lines[nzchar(lines)])
})

# `typed_at` is when the isolate was first recorded; later passes fill in fields
# that were not known yet but must not restate it.
IMMUTABLE_COLUMNS <- c("isolate", "typed_at")

#' Read a Value from the Scheme Overview
#'
#' @description Looks a key up in the long-format `scheme_overview` table.
#'
#' @param overview Data frame with `key` and `value` columns.
#' @param key Character string. Key to look up.
#' @param numeric Logical. Parse the value as a number, dropping the thousands
#'   separators cgmlst.org formats its counts with.
#'
#' @return The value, or `NA`.
#' @export
overview_value <- function(overview, key, numeric = FALSE) {
  empty <- if (numeric) NA_real_ else NA_character_
  if (
    !is.data.frame(overview) ||
      !nrow(overview) ||
      !all(c("key", "value") %in% names(overview))
  ) {
    return(empty)
  }
  hit <- which(trimws(as.character(overview$key)) == key)
  if (!length(hit)) {
    return(empty)
  }
  value <- trimws(as.character(overview$value[hit[1]]))
  if (!nzchar(value)) {
    return(empty)
  }
  if (numeric) {
    suppressWarnings(as.numeric(gsub("[^0-9.]", "", value)))
  } else {
    value
  }
}

#' Read a Database's cgMLST Scheme Provenance
#'
#' @description Collects the scheme-level context every isolate of a database is
#'   typed against, ready to be stamped onto its provenance rows.
#'
#' @param db_path Character path to the SQLite database file.
#'
#' @return Named list of `cg_*` fields; entries are `NA` when the database
#'   carries no scheme overview.
#' @export
scheme_provenance <- function(db_path) {
  empty <- list(
    cg_scheme_database = NA_character_,
    cg_scheme_version = NA_character_,
    cg_seed_genome = NA_character_,
    cg_genus = NA_character_,
    cg_species = NA_character_,
    cg_locus_count = NA_real_,
    cg_complex_type_distance = NA_real_,
    cg_complex_type_count = NA_real_
  )
  if (
    is.null(db_path) ||
      length(db_path) != 1 ||
      is.na(db_path) ||
      !file.exists(db_path)
  ) {
    return(empty)
  }

  con <- tryCatch(connect(db_path), error = function(e) NULL)
  if (is.null(con)) {
    return(empty)
  }
  on.exit(dbDisconnect(con))

  if (!("scheme_overview" %in% dbListTables(con))) {
    return(empty)
  }
  overview <- tryCatch(
    dbGetQuery(con, "SELECT key, value FROM scheme_overview"),
    error = function(e) NULL
  )
  if (is.null(overview) || !nrow(overview)) {
    return(empty)
  }

  list(
    cg_scheme_database = overview_value(overview, "Database"),
    cg_scheme_version = overview_value(overview, "Version"),
    cg_seed_genome = overview_value(overview, "Seed Genome"),
    cg_genus = overview_value(overview, "Genus"),
    cg_species = overview_value(overview, "Species"),
    cg_locus_count = overview_value(overview, "Locus Count", numeric = TRUE),
    cg_complex_type_distance = overview_value(
      overview,
      "Complex Type Distance",
      numeric = TRUE
    ),
    cg_complex_type_count = overview_value(
      overview,
      "Complex Type Count",
      numeric = TRUE
    )
  )
}

#' Record How an Isolate Was Typed
#'
#' @description Writes or extends one isolate's provenance row.
#'
#' @details
#' Called once per isolate for every step that reports in, so the row grows as
#' the run does: the assembly and scheme context land with allele calling, the
#' `cla_*` and `amr_*` blocks as those steps finish. Only the fields supplied are
#' touched - a later pass never blanks what an earlier one recorded - and
#' `typed_at` keeps the moment the isolate was first written.
#'
#' @param db_path Character path to the SQLite database file.
#' @param isolate Character string. Isolate the row belongs to.
#' @param fields Named list of column values; unknown names are ignored, `NULL`
#'   and zero-length entries are skipped.
#'
#' @return Logical success, invisibly.
#' @export
store_provenance <- function(db_path, isolate, fields) {
  if (
    is.null(db_path) ||
      length(db_path) != 1 ||
      is.na(db_path) ||
      !file.exists(db_path) ||
      is.null(isolate) ||
      length(isolate) != 1 ||
      is.na(isolate) ||
      !nzchar(isolate)
  ) {
    return(invisible(FALSE))
  }

  fields <- fields[!vapply(fields, function(x) is.null(x) || !length(x), logical(1))]
  fields <- fields[names(fields) %in% setdiff(PROVENANCE_COLUMNS, IMMUTABLE_COLUMNS)]
  # NA carries no information here: a step that has not run yet must not
  # overwrite what an earlier pass recorded.
  fields <- fields[!vapply(fields, function(x) is.na(x[1]), logical(1))]

  columns <- c("isolate", "typed_at", names(fields))
  values <- c(list(isolate, as.character(Sys.time())), unname(lapply(fields, function(x) x[1])))

  con <- tryCatch(connect(db_path), error = function(e) NULL)
  if (is.null(con)) {
    return(invisible(FALSE))
  }
  on.exit(dbDisconnect(con))

  ok <- tryCatch(
    {
      dbExecute(con, PROVENANCE_DDL)
      updates <- setdiff(columns, IMMUTABLE_COLUMNS)
      dbExecute(
        con,
        sprintf(
          "INSERT INTO typing_provenance (%s) VALUES (%s)
           ON CONFLICT(isolate) DO UPDATE SET %s",
          paste(columns, collapse = ", "),
          paste(rep("?", length(columns)), collapse = ", "),
          if (length(updates)) {
            paste(sprintf("%s = excluded.%s", updates, updates), collapse = ", ")
          } else {
            "isolate = excluded.isolate"
          }
        ),
        params = values
      )
      TRUE
    },
    error = function(e) {
      log_event("DB", "typing_provenance", sprintf("%s: %s", isolate, conditionMessage(e)))
      FALSE
    }
  )

  invisible(isTRUE(ok))
}
