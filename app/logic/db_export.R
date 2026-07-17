# app/logic/db_export.R
#
# Write a user-selected subset of the loaded database to a new `.db` file that
# a peer lab can import (or that pyMLST's `wgMLST` can still open directly).
#
# The destination is *built fresh* rather than copied-and-pruned: unselected
# isolates and withheld metadata columns are never written to the output file at
# any point, not even transiently. That matters — the whole point of choosing
# columns is that some of them are confidential.
#
# The table DDL is replayed verbatim from the source's `sqlite_master` rather
# than reconstructed with `CREATE TABLE … AS SELECT`, which would silently drop
# `mlst`'s primary key, its `seqid -> sequences.id` foreign key, and the four
# `ix_*` indexes, leaving a file pyMLST cannot type against.

box::use(
  RSQLite[SQLite],
  DBI[
    dbConnect,
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
  app / logic / db_compat[REF_SOUCHE, attach_ro, connect_ro],
)

`%||%` <- function(a, b) if (is.null(a)) b else a

# Copied verbatim when present. `alembic_version` is carried so pyMLST still
# recognises the schema revision; `targets` and `scheme_overview` back the
# Loci Info and Scheme Info panels.
SCHEME_TABLES <- c(
  "mlst_type",
  "alembic_version",
  "targets",
  "scheme_overview"
)

# Always present in the output, never user-selectable.
#' @export
METADATA_FIXED_COLS <- c("isolate", "organism")

.quote_ident <- function(x) paste0('"', gsub('"', '""', x), '"')

.noop_progress <- function(frac, msg) invisible(NULL)

# Table/index DDL exactly as the source declares it, so PK/FK/index survive.
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

#' Summarise what an export would contain, without writing anything.
#' @export
export_preview <- function(
  src_path,
  isolates,
  metadata_cols = character(0),
  include_metadata = TRUE
) {
  con <- connect_ro(src_path)
  on.exit(dbDisconnect(con), add = TRUE)

  isolates <- setdiff(unique(isolates), REF_SOUCHE)

  if (!length(isolates)) {
    return(list(
      n_isolates = 0L,
      n_loci = 0L,
      n_alleles = 0L,
      n_calls = 0L,
      columns = character(0),
      est_bytes = 0
    ))
  }

  .write_selection(con, isolates)

  counts <- dbGetQuery(
    con,
    "SELECT COUNT(*) AS calls,
            COUNT(DISTINCT gene) AS loci,
            COUNT(DISTINCT seqid) AS alleles
       FROM mlst
      WHERE souche = ? OR souche IN (SELECT souche FROM sel)",
    params = list(REF_SOUCHE)
  )

  list(
    n_isolates = length(isolates),
    n_loci = as.integer(counts$loci),
    n_alleles = as.integer(counts$alleles),
    n_calls = as.integer(counts$calls),
    columns = if (include_metadata) {
      .metadata_out_cols(con, metadata_cols)
    } else {
      character(0)
    }
  )
}

# A temp table beats an IN (?, ?, …) list: no SQLITE_MAX_VARIABLE_NUMBER limit
# and the same plan.
.write_selection <- function(con, isolates) {
  dbWriteTable(
    con,
    "sel",
    data.frame(souche = isolates, stringsAsFactors = FALSE),
    temporary = TRUE,
    overwrite = TRUE
  )
}

# The exported columns, in the *source's* column order. `isolate` and `organism`
# are always carried whether or not the user selected them.
#
# Keeping the source order (rather than hoisting the two fixed columns to the
# front) means a full export is a structural clone of the source, not merely a
# value-for-value one — a peer diffing the two files sees the same table shape.
.metadata_out_cols <- function(con, metadata_cols) {
  if (!"metadata" %in% dbListTables(con)) {
    return(character(0))
  }
  have <- dbListFields(con, "metadata")
  keep <- union(intersect(METADATA_FIXED_COLS, have), metadata_cols)
  have[have %in% keep]
}

#' Build `dest_path` from `src_path`, carrying only `isolates` (plus the
#' scheme's `ref` souche) and only `metadata_cols` of the metadata table.
#'
#' Returns a list of what was written. The file appears at `dest_path` only on
#' success — the build happens in a sibling `.part` file that is renamed at the
#' end, so a crash never leaves a half-written database where a valid one is
#' expected.
#' @export
export_database <- function(
  src_path,
  dest_path,
  isolates,
  metadata_cols = character(0),
  include_metadata = TRUE,
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

  part <- paste0(dest_path, ".part")
  if (file.exists(part)) {
    unlink(part)
  }

  con <- dbConnect(SQLite(), part)
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
    intersect("hashes", src_tables)
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

  # Tables first, indexes last: inserting into an unindexed table is much
  # cheaper than maintaining four indexes per row.
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
      # The scheme reference is carried regardless of isolate selection: without
      # it the file has no scheme, and scheme_size() / the compat gate break.
      dbExecute(
        con,
        "INSERT INTO mlst SELECT * FROM src.mlst
          WHERE souche = ? OR souche IN (SELECT souche FROM sel)",
        params = list(REF_SOUCHE)
      )

      progress(0.6, "Copying sequences …")
      # Original seqids are kept — import remaps by (gene, hash) anyway, and
      # renumbering would only invite off-by-one bugs.
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
              WHERE isolate IN (SELECT souche FROM sel)",
            cols,
            cols
          )
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
  # Close before renaming so no journal/WAL sidecar outlives the .part file.
  dbDisconnect(con)
  ok <- TRUE

  if (!file.rename(part, dest_path)) {
    # Different filesystem: fall back to a copy.
    if (!file.copy(part, dest_path, overwrite = TRUE)) {
      unlink(part)
      stop("Could not write to ", dest_path)
    }
    unlink(part)
  }

  progress(1, "Done")

  list(
    path = dest_path,
    n_isolates = as.integer(result$isolates),
    n_alleles = as.integer(result$alleles),
    n_calls = as.integer(result$calls),
    metadata_cols = meta_cols,
    bytes = file.size(dest_path)
  )
}
