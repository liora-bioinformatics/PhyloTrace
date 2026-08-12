# app/logic/db_export.R
#
# Exports a user-selected subset of the loaded database to a new SQLite `.db`
# file compatible with peer instances.
#
# To protect sensitive or confidential data, the target file is generated fresh
# from schema DDL rather than via copy-and-prune operations. Unselected isolates
# and unchosen metadata columns are never written to disk.
#
# Database schema definitions (DDL) are replayed directly from the source
# `sqlite_master` table instead of using `CREATE TABLE ... AS SELECT`. This
# preserves essential constraints, primary keys, foreign key definitions, and
# indexing structures required for downstream typing.

box::use(
  DBI[
    dbDisconnect,
    dbExecute,
    dbGetQuery,
    dbListFields,
    dbListTables,
    dbWriteTable,
    dbBegin,
    dbCommit,
    dbRollback
  ],
)

box::use(
  app / logic / db_connect[connect],
  app / logic / db_compat[REF_SOUCHE, attach_ro, connect_ro],
  app / logic / logging[log_event],
)

# Returns fallback value if target is NULL.
`%||%` <- function(a, b) if (is.null(a)) b else a

# Core scheme tables copied verbatim when present. Carrying `alembic_version`
# maintains pyMLST schema tracking, while `targets` and `scheme_overview`
# support summary views in client applications.
SCHEME_TABLES <- c(
  "mlst_type",
  "alembic_version",
  "targets",
  "scheme_overview"
)

# Optional analysis result tables linked by isolate ID.
# These tables do not contain allele foreign keys, allowing straightforward
# filtering during export operations.
CLASSICAL_TABLES <- c("classical_mlst")
AMR_TABLES <- c("amr_results", "amr_summary")

# Genome provenance: assembly checksums, so receiving systems can verify genome
# integrity, and the per-isolate record of the software and reference data that
# produced each call.
GENOME_TABLES <- c("genome_hashes", "typing_provenance")

# User-defined custom metadata variables.
# Variable selection is managed individually per field to permit targeted sharing.
#' @export
CUSTOM_TABLES <- c("phylotrace_custom_fields", "phylotrace_custom_values")

# Resolves requested analysis result tables that exist in the source database.
.result_tables <- function(src_tables, include_classical, include_amr) {
  wanted <- c(
    if (isTRUE(include_classical)) CLASSICAL_TABLES,
    if (isTRUE(include_amr)) AMR_TABLES,
    GENOME_TABLES
  )
  intersect(wanted, src_tables)
}

#' Query Available Analysis Result Tables
#'
#' Inspects the source database to determine which optional analysis tables are present.
#'
#' @param db_path Character path to the SQLite database file.
#' @return A list with logical flags `classical` and `amr` indicating table availability.
#' @export
available_result_tables <- function(db_path) {
  out <- list(classical = FALSE, amr = FALSE)
  if (
    is.null(db_path) ||
      length(db_path) != 1 ||
      is.na(db_path) ||
      !file.exists(db_path)
  ) {
    return(out)
  }
  con <- tryCatch(connect_ro(db_path), error = function(e) NULL)
  if (is.null(con)) {
    return(out)
  }
  on.exit(dbDisconnect(con))
  tbls <- dbListTables(con)
  out$classical <- all(CLASSICAL_TABLES %in% tbls)
  out$amr <- all(AMR_TABLES %in% tbls)
  out
}

#' Retrieve Custom Metadata Fields
#'
#' Retrieves ordered names of custom metadata fields defined in the database.
#'
#' @param db_path Character path to the SQLite database file.
#' @return Character vector of custom field names in display order.
#' @export
exportable_custom_fields <- function(db_path) {
  if (
    is.null(db_path) ||
      length(db_path) != 1 ||
      is.na(db_path) ||
      !file.exists(db_path)
  ) {
    return(character(0))
  }
  con <- tryCatch(connect_ro(db_path), error = function(e) NULL)
  if (is.null(con)) {
    return(character(0))
  }
  on.exit(dbDisconnect(con))

  if (!all(CUSTOM_TABLES %in% dbListTables(con))) {
    return(character(0))
  }
  dbGetQuery(
    con,
    "SELECT name FROM phylotrace_custom_fields ORDER BY COALESCE(position, id)"
  )$name
}

# Filters requested custom fields against those present in the source database.
.custom_out_fields <- function(con, src_tables, custom_fields) {
  if (!length(custom_fields) || !all(CUSTOM_TABLES %in% src_tables)) {
    return(character(0))
  }
  have <- dbGetQuery(
    con,
    "SELECT name FROM phylotrace_custom_fields ORDER BY COALESCE(position, id)"
  )$name
  intersect(have, custom_fields)
}

# Immutable metadata columns exported by default regardless of user selection.
#' @export
METADATA_FIXED_COLS <- c("isolate", "organism")

# Escapes double quotes in SQLite identifiers for safe query construction.
.quote_ident <- function(x) paste0('"', gsub('"', '""', x), '"')

# Default progress callback placeholder.
.noop_progress <- function(frac, msg) invisible(NULL)

# Reconstructs table or index DDL directly from sqlite_master to preserve schema constraints.
.source_ddl <- function(con, type, names) {
  if (!length(names)) {
    return(character(0))
  }
  res <- dbGetQuery(
    con,
    sprintf(
      "SELECT name, sql FROM sqlite_master
        WHERE type = ? AND sql IS NOT NULL
          AND name IN (%s)",
      paste(rep("?", length(names)), collapse = ", ")
    ),
    params = c(list(type), as.list(names))
  )
  stats::setNames(res$sql, res$name)
}

#' Summarize Planned Export Contents
#'
#' Calculates isolate, allele, locus counts, and column selections for an export job without writing to disk.
#'
#' @param src_path Character path to the source database file.
#' @param isolates Character vector of isolate IDs selected for export.
#' @param metadata_cols Character vector of metadata columns selected for inclusion.
#' @param include_metadata Logical indicating whether metadata should be exported.
#' @param custom_fields Character vector of custom fields selected for inclusion.
#' @return A list containing summary metrics and selected field metadata.
#' @export
export_preview <- function(
  src_path,
  isolates,
  metadata_cols = character(0),
  include_metadata = TRUE,
  custom_fields = character(0)
) {
  con <- connect_ro(src_path)
  on.exit(dbDisconnect(con), add = TRUE)

  # Exclude reference strain key from isolate filter list
  isolates <- setdiff(unique(isolates), REF_SOUCHE)

  if (!length(isolates)) {
    return(list(
      n_isolates = 0L,
      n_loci = 0L,
      n_alleles = 0L,
      n_calls = 0L,
      ref_calls = 0L,
      ref_alleles = 0L,
      columns = character(0),
      custom_fields = character(0),
      est_bytes = 0
    ))
  }

  .write_selection(con, isolates)

  # Compute allele and locus metrics, including the required reference strain
  counts <- dbGetQuery(
    con,
    "SELECT COUNT(*) AS calls,
            COUNT(DISTINCT gene) AS loci,
            COUNT(DISTINCT seqid) AS alleles
       FROM mlst
      WHERE souche = ? OR souche IN (SELECT isolate FROM sel)",
    params = list(REF_SOUCHE)
  )

  # The reference strain's own contribution, reported separately so a `.db`
  # export doesn't inflate the isolate-facing tiles.
  ref_counts <- dbGetQuery(
    con,
    "SELECT COUNT(*) AS calls, COUNT(DISTINCT seqid) AS alleles
       FROM mlst WHERE souche = ?",
    params = list(REF_SOUCHE)
  )

  list(
    n_isolates = length(isolates),
    n_loci = as.integer(counts$loci),
    n_alleles = as.integer(counts$alleles),
    n_calls = as.integer(counts$calls),
    ref_calls = as.integer(ref_counts$calls),
    ref_alleles = as.integer(ref_counts$alleles),
    columns = if (include_metadata) {
      .metadata_out_cols(con, metadata_cols)
    } else {
      character(0)
    },
    custom_fields = .custom_out_fields(con, dbListTables(con), custom_fields)
  )
}

# Writes target isolate selections to a temporary SQLite table to optimize query evaluation.
.write_selection <- function(con, isolates) {
  dbWriteTable(
    con,
    "sel",
    data.frame(isolate = isolates, stringsAsFactors = FALSE),
    temporary = TRUE,
    overwrite = TRUE
  )
}

# Determines metadata output columns in source ordering, keeping fixed columns.
.metadata_out_cols <- function(con, metadata_cols) {
  if (!"metadata" %in% dbListTables(con)) {
    return(character(0))
  }
  have <- dbListFields(con, "metadata")
  keep <- union(intersect(METADATA_FIXED_COLS, have), metadata_cols)
  have[have %in% keep]
}

#' Export Database Subset
#'
#' Generates a new SQLite database file containing selected isolates, metadata, custom fields, and analysis tables.
#'
#' @param src_path Character path to the source SQLite database file.
#' @param dest_path Character path for the output SQLite database file.
#' @param isolates Character vector of isolate IDs to export.
#' @param metadata_cols Character vector of metadata columns to include.
#' @param include_metadata Logical indicating whether to include metadata.
#' @param include_classical Logical indicating whether to include classical MLST data.
#' @param include_amr Logical indicating whether to include AMR screening results.
#' @param custom_fields Character vector of custom field names to include.
#' @param progress Optional callback function for reporting progress.
#' @return A list detailing exported records, field selections, and output file size.
#' @export
export_database <- function(
  src_path,
  dest_path,
  isolates,
  metadata_cols = character(0),
  include_metadata = TRUE,
  include_classical = FALSE,
  include_amr = FALSE,
  custom_fields = character(0),
  progress = NULL
) {
  progress <- progress %||% .noop_progress

  isolates <- setdiff(unique(isolates), REF_SOUCHE)
  if (!length(isolates)) {
    stop("Select at least one isolate to export.")
  }

  src <- connect_ro(src_path)
  on.exit(dbDisconnect(src), add = TRUE)

  src_tables <- dbListTables(src)
  known <- dbGetQuery(src, "SELECT DISTINCT souche FROM mlst")$souche
  unknown <- setdiff(isolates, known)
  if (length(unknown)) {
    stop(
      "Unknown isolate(s): ",
      paste(utils::head(unknown, 5), collapse = ", ")
    )
  }

  meta_cols <- if (include_metadata) {
    .metadata_out_cols(src, metadata_cols)
  } else {
    character(0)
  }
  write_meta <- length(meta_cols) > 0L

  # Resolve optional result tables present in source
  result_tables <- .result_tables(src_tables, include_classical, include_amr)

  # Resolve custom metadata fields to export
  out_custom <- .custom_out_fields(src, src_tables, custom_fields)
  custom_tables <- if (length(out_custom)) CUSTOM_TABLES else character(0)

  # Build target database in a temporary partial file to ensure atomic file generation
  part <- paste0(dest_path, ".part")
  if (file.exists(part)) {
    unlink(part)
  }

  con <- connect(part)
  ok <- FALSE
  on.exit(
    {
      if (DBI::dbIsValid(con)) {
        dbDisconnect(con)
      }
      if (!ok && file.exists(part)) unlink(part)
    },
    add = TRUE
  )

  progress(0.05, "Preparing destination …")

  attach_ro(con, src_path, "src")

  copy_tables <- c(
    intersect(SCHEME_TABLES, src_tables),
    "mlst",
    "sequences",
    intersect("hashes", src_tables),
    result_tables,
    custom_tables
  )

  table_ddl <- .source_ddl(src, "table", copy_tables)
  missing_ddl <- setdiff(copy_tables, names(table_ddl))
  if (length(missing_ddl)) {
    stop(
      "Source database has no DDL for: ",
      paste(missing_ddl, collapse = ", ")
    )
  }

  index_ddl <- .source_ddl(
    src,
    "index",
    dbGetQuery(
      src,
      "SELECT name FROM sqlite_master
        WHERE type = 'index' AND sql IS NOT NULL AND tbl_name = 'mlst'"
    )$name
  )

  # Create tables prior to index build to optimize bulk insertion performance
  for (nm in copy_tables) {
    dbExecute(con, table_ddl[[nm]])
  }
  if (write_meta) {
    dbExecute(
      con,
      sprintf(
        "CREATE TABLE metadata (%s)",
        paste(paste(.quote_ident(meta_cols), "TEXT"), collapse = ", ")
      )
    )
  }

  dbBegin(con)
  tryCatch(
    {
      .write_selection(con, isolates)

      progress(0.15, "Copying scheme tables …")
      for (nm in intersect(SCHEME_TABLES, src_tables)) {
        dbExecute(con, sprintf("INSERT INTO %1$s SELECT * FROM src.%1$s", nm))
      }

      progress(0.35, "Copying allele calls …")
      # Always carry scheme reference strain to preserve scheme metrics
      dbExecute(
        con,
        "INSERT INTO mlst SELECT * FROM src.mlst
          WHERE souche = ? OR souche IN (SELECT isolate FROM sel)",
        params = list(REF_SOUCHE)
      )

      progress(0.6, "Copying sequences …")
      # Maintain original sequence IDs to ensure consistent cross-referencing
      dbExecute(
        con,
        "INSERT INTO sequences SELECT * FROM src.sequences
          WHERE id IN (SELECT DISTINCT seqid FROM mlst)"
      )
      if ("hashes" %in% src_tables) {
        dbExecute(
          con,
          "INSERT INTO hashes SELECT * FROM src.hashes
            WHERE id IN (SELECT DISTINCT seqid FROM mlst)"
        )
      }

      if (write_meta) {
        progress(0.8, "Copying metadata …")
        cols <- paste(.quote_ident(meta_cols), collapse = ", ")
        dbExecute(
          con,
          sprintf(
            "INSERT INTO metadata (%s) SELECT %s FROM src.metadata
              WHERE isolate IN (SELECT isolate FROM sel)",
            cols,
            cols
          )
        )
      }

      # Filter and write optional analysis results for selected isolates
      if (length(result_tables)) {
        progress(0.85, "Copying analysis results …")
        for (nm in result_tables) {
          dbExecute(
            con,
            sprintf(
              "INSERT INTO %1$s SELECT * FROM src.%1$s
                WHERE isolate IN (SELECT isolate FROM sel)",
              nm
            )
          )
        }
      }

      # Transfer custom field definitions and corresponding isolate values
      if (length(out_custom)) {
        progress(0.87, "Copying custom variables …")
        dbExecute(
          con,
          sprintf(
            "INSERT INTO phylotrace_custom_fields
               SELECT * FROM src.phylotrace_custom_fields WHERE name IN (%s)",
            paste(rep("?", length(out_custom)), collapse = ", ")
          ),
          params = as.list(out_custom)
        )
        dbExecute(
          con,
          "INSERT INTO phylotrace_custom_values
             SELECT * FROM src.phylotrace_custom_values
            WHERE isolate IN (SELECT isolate FROM sel)
              AND field_id IN (SELECT id FROM phylotrace_custom_fields)"
        )
      }

      dbExecute(con, "DROP TABLE sel")
      dbCommit(con)
    },
    error = function(e) {
      dbRollback(con)
      stop(e)
    }
  )

  # Construct table indexes after record insertion
  progress(0.9, "Building indexes …")
  for (sql in index_ddl) {
    dbExecute(con, sql)
  }

  result <- dbGetQuery(
    con,
    "SELECT (SELECT COUNT(DISTINCT souche) FROM mlst WHERE souche != ?) AS isolates,
            (SELECT COUNT(*) FROM sequences) AS alleles,
            (SELECT COUNT(*) FROM mlst) AS calls",
    params = list(REF_SOUCHE)
  )

  dbExecute(con, "DETACH DATABASE src")
  # Close target connection prior to final file rename operation
  dbDisconnect(con)
  ok <- TRUE

  if (!file.rename(part, dest_path)) {
    # Fallback copy if moving across distinct file system mounts
    if (!file.copy(part, dest_path, overwrite = TRUE)) {
      unlink(part)
      stop("Could not write to ", dest_path)
    }
    unlink(part)
  }

  progress(1, "Done")

  log_event(
    "DB",
    "export",
    sprintf(
      "%s | %d isolate(s), %d allele(s), %d call(s)",
      dest_path,
      as.integer(result$isolates),
      as.integer(result$alleles),
      as.integer(result$calls)
    )
  )

  list(
    path = dest_path,
    n_isolates = as.integer(result$isolates),
    n_alleles = as.integer(result$alleles),
    n_calls = as.integer(result$calls),
    metadata_cols = meta_cols,
    custom_fields = out_custom,
    bytes = file.size(dest_path)
  )
}
