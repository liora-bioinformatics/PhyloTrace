# app/logic/db_import.R
#
# Merges a peer PhyloTrace database into the primary loaded database.
#
# The merge algorithm relies on the unique structural properties of the schema:
# while `seqid` values are database-specific, `(gene, sha256(sequence))` pairs
# globally identify alleles across systems. (In a valid PhyloTrace database,
# each `seqid` maps uniquely to one `gene`, and the `hashes` table stores
# `sha256(sequences.sequence)`, forming a bijective mapping with `seqid`.)
#
# Consequently, external `seqid` values are not trusted directly. Every incoming
# allele is mapped to a local identifier by content hash, allocating new local
# sequence IDs only when an allele is not present in the primary database.
#
# To ensure atomic execution and data safety, operations are never performed
# directly on the live database file. Merges execute against a temporary copy
# in the same target directory inside a single transaction. The temporary file
# replaces the live file using an atomic rename operation upon successful commit.
# The original database state is preserved as a timestamped backup (`.bak-*`).

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
    dbRollback,
    dbIsValid
  ],
  openssl[sha256],
)

box::use(
  app / logic / custom_fields[CUSTOM_SCHEMA_DDL],
  app /
    logic /
    db_compat[
      REF_SOUCHE,
      attach_ro,
      connect_ro,
      check_import_compatibility
    ],
  app / logic / pymlst[hash_database],
  app / logic / database_functions[make_metadata_table, load_db_species],
  app / logic / db_sources[SOURCE_COL, db_uuid, register_source],
  app / logic / logging[log_event],
)

# Returns fallback value if target is NULL.
`%||%` <- function(a, b) if (is.null(a)) b else a

# Default progress callback placeholder.
.noop_progress <- function(frac, msg) invisible(NULL)

# Escapes double quotes in SQLite identifiers for safe query construction.
.quote_ident <- function(x) paste0('"', gsub('"', '""', x), '"')

# Converts empty or NULL aggregate query results (e.g., SUM/MAX) safely to integer.
.as_count <- function(x) {
  if (!length(x) || is.na(x[[1]])) 0L else as.integer(x[[1]])
}

# Metadata fields excluded from manual user mapping:
# - `isolate`: Internal join key.
# - `organism`: Inferred from local species settings.
# - `called_at`: Provenance timestamp managed during merge.
# - `source`: Database origin provenance identifier.
#' @export
METADATA_RESERVED <- c("isolate", "organism", "called_at", SOURCE_COL)

# Custom field configuration tables.
CUSTOM_TABLES <- c("phylotrace_custom_fields", "phylotrace_custom_values")

# Temporary indexes applied during import processing and removed prior to finalize.
.TRANSIENT_INDEXES <- c(
  pt_import_ix_sequences_id = "CREATE INDEX IF NOT EXISTS pt_import_ix_sequences_id ON sequences(id)",
  pt_import_ix_hashes_id = "CREATE INDEX IF NOT EXISTS pt_import_ix_hashes_id ON hashes(id)"
)

# Canonical query for fetching allele profile digests per isolate.
.PROFILE_SQL <- "
  SELECT isolate, group_concat(gh, char(10) ORDER BY gh) AS profile FROM (
    SELECT DISTINCT m.souche AS isolate, m.gene || char(9) || h.hash AS gh
      FROM %1$s.mlst m JOIN %1$s.hashes h ON h.id = m.seqid
     WHERE m.souche <> ?
  ) GROUP BY isolate"

# ---------------------------------------------------------------------------
# Hash availability
# ---------------------------------------------------------------------------

# Verifies whether all sequences in the connection have corresponding hash entries.
.hashes_complete <- function(con) {
  if (!"hashes" %in% dbListTables(con)) {
    return(FALSE)
  }
  dbGetQuery(
    con,
    "SELECT COUNT(*) AS n FROM sequences s
       LEFT JOIN hashes h ON h.id = s.id
      WHERE h.id IS NULL"
  )$n ==
    0L
}

#' Ensure Complete Sequence Hash Coverage
#'
#' Verifies sequence hash coverage in the source database, generating missing hashes
#' in a temporary file if necessary without modifying the original source.
#'
#' @param path Character path to source SQLite database file.
#' @return A list containing file `path` and boolean `temp` flag.
#' @export
prepare_source <- function(path) {
  con <- connect_ro(path)
  complete <- tryCatch(.hashes_complete(con), error = function(e) FALSE)
  dbDisconnect(con)

  if (complete) {
    return(list(path = path, temp = FALSE))
  }

  tmp <- tempfile("phylotrace_import_", fileext = ".db")
  if (!file.copy(path, tmp)) {
    stop("Could not stage the external database for hashing.")
  }
  hash_database(tmp)
  list(path = tmp, temp = TRUE)
}

#' Release Prepared Source Resources
#'
#' Removes temporary database files created during source preparation.
#'
#' @param prep Result list returned by `prepare_source()`.
#' @export
release_source <- function(prep) {
  if (!is.null(prep) && isTRUE(prep$temp) && file.exists(prep$path)) {
    unlink(prep$path)
  }
  invisible(NULL)
}

# ---------------------------------------------------------------------------
# Isolate identity
# ---------------------------------------------------------------------------

# Computes SHA-256 profile digests for all isolates in the specified schema.
.profiles <- function(con, schema) {
  df <- dbGetQuery(
    con,
    sprintf(.PROFILE_SQL, schema),
    params = list(REF_SOUCHE)
  )
  if (!nrow(df)) {
    return(stats::setNames(character(0), character(0)))
  }
  stats::setNames(as.character(sha256(df$profile)), df$isolate)
}

#' Generate Isolate Profile Hashes
#'
#' Calculates cryptographic profile hashes derived from combined allele calls for each isolate.
#'
#' @param db_path Character path to SQLite database file.
#' @return Named character vector of profile hashes keyed by isolate identifier.
#' @export
isolate_profile_hashes <- function(db_path) {
  prep <- prepare_source(db_path)
  on.exit(release_source(prep), add = TRUE)

  con <- connect_ro(prep$path)
  on.exit(dbDisconnect(con), add = TRUE)

  .profiles(con, "main")
}

#' Classify Isolate Collisions
#'
#' Evaluates external isolates against the target database to identify new entries,
#' identical duplicates, and naming collisions.
#'
#' @param local_path Character path to local SQLite database file.
#' @param ext_path Character path to external SQLite database file.
#' @return A data frame containing isolate identifiers and collision classifications (`new`, `identical_duplicate`, `name_clash`).
#' @export
classify_isolate_collisions <- function(local_path, ext_path) {
  local <- isolate_profile_hashes(local_path)
  ext <- isolate_profile_hashes(ext_path)

  if (!length(ext)) {
    return(data.frame(
      isolate = character(0),
      status = character(0),
      stringsAsFactors = FALSE
    ))
  }

  status <- vapply(
    names(ext),
    function(nm) {
      if (!nm %in% names(local)) {
        "new"
      } else if (identical(unname(ext[[nm]]), unname(local[[nm]]))) {
        "identical_duplicate"
      } else {
        "name_clash"
      }
    },
    character(1),
    USE.NAMES = FALSE
  )

  data.frame(
    isolate = names(ext),
    status = status,
    stringsAsFactors = FALSE
  )
}

#' Generate Default Resolution Mapping
#'
#' Creates an initial import resolution table, selecting novel isolates for import and skipping duplicates or clashes by default.
#'
#' @param classification Data frame output from `classify_isolate_collisions()`.
#' @return Data frame detailing planned resolution actions (`add`, `skip`).
#' @export
default_resolutions <- function(classification) {
  data.frame(
    ext_isolate = classification$isolate,
    action = ifelse(classification$status == "new", "add", "skip"),
    final_isolate = classification$isolate,
    stringsAsFactors = FALSE
  )
}

#' Generate Unique Rename Candidate
#'
#' Suggests a non-conflicting isolate identifier by appending numeric suffix patterns.
#'
#' @param name Character base isolate identifier.
#' @param taken Character vector of existing identifiers.
#' @return Character string representing a unique candidate identifier.
#' @export
suggest_rename <- function(name, taken) {
  candidate <- paste0(name, "_imp")
  i <- 1L
  while (candidate %in% taken) {
    i <- i + 1L
    candidate <- paste0(name, "_imp", i)
  }
  candidate
}

# Validates resolution table actions, target identifier constraints, and collision rules.
.validate_resolutions <- function(resolutions, local_isolates, ext_isolates) {
  need <- c("ext_isolate", "action", "final_isolate")
  if (!all(need %in% names(resolutions))) {
    stop("`resolutions` needs columns: ", paste(need, collapse = ", "))
  }

  bad_action <- setdiff(
    resolutions$action,
    c("add", "skip", "overwrite", "rename")
  )
  if (length(bad_action)) {
    stop("Unknown action(s): ", paste(bad_action, collapse = ", "))
  }

  if (REF_SOUCHE %in% resolutions$ext_isolate) {
    stop(
      "The scheme reference '",
      REF_SOUCHE,
      "' is not an importable isolate."
    )
  }

  unknown <- setdiff(resolutions$ext_isolate, ext_isolates)
  if (length(unknown)) {
    stop(
      "Not present in the external database: ",
      paste(utils::head(unknown, 5), collapse = ", ")
    )
  }

  accepted <- resolutions[resolutions$action != "skip", , drop = FALSE]
  if (!nrow(accepted)) {
    stop("Nothing selected to import.")
  }

  if (anyDuplicated(accepted$final_isolate)) {
    dup <- unique(accepted$final_isolate[duplicated(accepted$final_isolate)])
    stop("Duplicate target name(s): ", paste(dup, collapse = ", "))
  }

  if (any(!nzchar(accepted$final_isolate))) {
    stop("Every imported isolate needs a non-empty name.")
  }

  fresh <- accepted$action %in% c("add", "rename")
  clash <- accepted$final_isolate[fresh] %in% local_isolates
  if (any(clash)) {
    stop(
      "Target name(s) already in the database: ",
      paste(
        utils::head(accepted$final_isolate[fresh][clash], 5),
        collapse = ", "
      )
    )
  }

  ow <- accepted$action == "overwrite"
  missing <- !accepted$final_isolate[ow] %in% local_isolates
  if (any(missing)) {
    stop(
      "Cannot overwrite isolate(s) that do not exist locally: ",
      paste(
        utils::head(accepted$final_isolate[ow][missing], 5),
        collapse = ", "
      )
    )
  }

  accepted
}

# ---------------------------------------------------------------------------
# Preview
# ---------------------------------------------------------------------------

# Extracts metadata column definitions from an attached SQLite schema.
.metadata_cols <- function(con, schema) {
  tbls <- dbGetQuery(
    con,
    sprintf("SELECT name FROM %s.sqlite_master WHERE type = 'table'", schema)
  )$name
  if (!"metadata" %in% tbls) {
    return(character(0))
  }
  names(dbGetQuery(
    con,
    sprintf("SELECT * FROM %s.metadata LIMIT 0", schema)
  ))
}

#' Generate Import Preview Summary
#'
#' Computes summary metrics for a proposed import configuration without committing changes to disk.
#'
#' @param local_path Character path to target local database.
#' @param ext_path Character path to source external database.
#' @param resolutions Data frame specifying import actions per isolate.
#' @param classification Pre-calculated classification results to optimize performance.
#' @return A list of summary metrics, including allele and metadata import impacts.
#' @export
import_preview <- function(
  local_path,
  ext_path,
  resolutions = NULL,
  classification = NULL
) {
  classification <- classification %||%
    classify_isolate_collisions(local_path, ext_path)
  resolutions <- resolutions %||% default_resolutions(classification)
  accepted <- resolutions[resolutions$action != "skip", , drop = FALSE]

  prep_local <- prepare_source(local_path)
  prep_ext <- prepare_source(ext_path)
  on.exit(
    {
      release_source(prep_local)
      release_source(prep_ext)
    },
    add = TRUE
  )

  con <- dbConnect(SQLite(), ":memory:")
  on.exit(dbDisconnect(con), add = TRUE)
  attach_ro(con, prep_local$path, "loc")
  attach_ro(con, prep_ext$path, "ext")

  n_new_alleles <- 0L
  n_shared_alleles <- 0L
  if (nrow(accepted)) {
    dbWriteTable(
      con,
      "acc",
      data.frame(isolate = accepted$ext_isolate, stringsAsFactors = FALSE),
      temporary = TRUE,
      overwrite = TRUE
    )
    counts <- dbGetQuery(
      con,
      "WITH used AS (
         SELECT DISTINCT em.gene AS gene, eh.hash AS hash
           FROM ext.mlst em
           JOIN ext.hashes eh ON eh.id = em.seqid
           JOIN acc a ON a.isolate = em.souche
       ),
       local_alleles AS (
         SELECT DISTINCT lm.gene AS gene, lh.hash AS hash
           FROM loc.mlst lm JOIN loc.hashes lh ON lh.id = lm.seqid
       )
       SELECT
         SUM(CASE WHEN la.hash IS NULL THEN 1 ELSE 0 END) AS novel,
         SUM(CASE WHEN la.hash IS NULL THEN 0 ELSE 1 END) AS shared
       FROM used u
       LEFT JOIN local_alleles la ON la.gene = u.gene AND la.hash = u.hash"
    )
    n_new_alleles <- .as_count(counts$novel)
    n_shared_alleles <- .as_count(counts$shared)
  }

  local_cols <- .metadata_cols(con, "loc")
  ext_cols <- .metadata_cols(con, "ext")
  custom <- .custom_split(.custom_defs(con, "loc"), .custom_defs(con, "ext"))

  list(
    n_new_isolates = sum(classification$status == "new"),
    n_identical_dupes = sum(classification$status == "identical_duplicate"),
    n_name_clashes = sum(classification$status == "name_clash"),
    n_accepted = nrow(accepted),
    n_new_alleles = n_new_alleles,
    n_shared_alleles = n_shared_alleles,
    metadata_shared = setdiff(
      intersect(local_cols, ext_cols),
      METADATA_RESERVED
    ),
    metadata_only_ext = setdiff(
      setdiff(ext_cols, local_cols),
      METADATA_RESERVED
    ),
    metadata_only_local = setdiff(
      setdiff(local_cols, ext_cols),
      METADATA_RESERVED
    ),
    custom_importable = custom$importable,
    custom_shared = custom$shared,
    custom_only_ext = custom$only_ext,
    custom_conflicts = custom$conflicts
  )
}

# ---------------------------------------------------------------------------
# Metadata reconciliation
# ---------------------------------------------------------------------------

# Reconciles and writes isolate metadata during import processing.
.write_metadata <- function(
  con,
  accepted,
  selected_cols,
  organism,
  ext_tables,
  source_label
) {
  ext_meta <- if ("metadata" %in% ext_tables) {
    dbGetQuery(con, "SELECT * FROM ext.metadata")
  } else {
    NULL
  }

  selected <- intersect(
    setdiff(selected_cols, METADATA_RESERVED),
    names(ext_meta) %||% character(0)
  )

  # `make_metadata_table()` skips creation when the local database has no
  # isolates yet (a freshly downloaded scheme), so the table may not exist.
  if (!"metadata" %in% dbListTables(con)) {
    dbExecute(
      con,
      sprintf(
        "CREATE TABLE metadata (%s)",
        paste(
          paste(.quote_ident(c(METADATA_RESERVED, selected)), "TEXT"),
          collapse = ", "
        )
      )
    )
  }

  # Both the reserved columns and every
  # newly selected external field must exist before the append.
  for (col in setdiff(
    c(METADATA_RESERVED, selected),
    dbListFields(con, "metadata")
  )) {
    dbExecute(
      con,
      sprintf("ALTER TABLE metadata ADD COLUMN %s TEXT", .quote_ident(col))
    )
  }

  idx <- match(accepted$ext_isolate, ext_meta$isolate)

  # Prefer the peer's own `called_at` (the isolate's original typing time) when
  # it carried one; fall back to the merge time for isolates the peer never
  # stamped. `called_at` is reserved only against user *mapping* — the peer's
  # existing value is still honoured here.
  peer_called_at <- if (
    !is.null(ext_meta) && "called_at" %in% names(ext_meta)
  ) {
    as.character(ext_meta$called_at[idx])
  } else {
    rep(NA_character_, nrow(accepted))
  }
  import_time <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  called_at <- ifelse(
    !is.na(peer_called_at) & nzchar(peer_called_at),
    peer_called_at,
    import_time
  )

  # Every row written here arrived by merge, including the ones replacing an
  # overwritten local isolate (whose metadata row was deleted just above) - the
  # replacement is the peer's isolate now, so it carries the peer's label.
  rows <- data.frame(
    isolate = accepted$final_isolate,
    organism = organism %||% NA_character_,
    called_at = called_at,
    stringsAsFactors = FALSE
  )
  rows[[SOURCE_COL]] <- source_label

  if (length(selected)) {
    for (col in selected) {
      rows[[col]] <- as.character(ext_meta[[col]][idx])
    }
  }

  dbWriteTable(con, "metadata", rows, append = TRUE)
  invisible(nrow(rows))
}

# ---------------------------------------------------------------------------
# Custom variables
# ---------------------------------------------------------------------------

# Fetches custom field definitions from specified attached database schema.
.custom_defs <- function(con, schema) {
  tbls <- dbGetQuery(
    con,
    sprintf("SELECT name FROM %s.sqlite_master WHERE type = 'table'", schema)
  )$name
  if (!all(CUSTOM_TABLES %in% tbls)) {
    return(data.frame(
      name = character(0),
      type = character(0),
      stringsAsFactors = FALSE
    ))
  }
  dbGetQuery(
    con,
    sprintf(
      "SELECT name, type FROM %s.phylotrace_custom_fields
        ORDER BY COALESCE(position, id)",
      schema
    )
  )
}

# Evaluates custom field compatibility between local and external schemas.
.custom_split <- function(local_defs, ext_defs) {
  empty_conflicts <- data.frame(
    name = character(0),
    local_type = character(0),
    ext_type = character(0),
    stringsAsFactors = FALSE
  )

  if (!nrow(ext_defs)) {
    return(list(
      importable = character(0),
      shared = character(0),
      only_ext = character(0),
      conflicts = empty_conflicts
    ))
  }

  idx <- match(tolower(ext_defs$name), tolower(local_defs$name))
  known <- !is.na(idx)
  same_type <- known & local_defs$type[idx] == ext_defs$type
  conflict <- known & !same_type

  list(
    importable = ext_defs$name[!conflict],
    shared = ext_defs$name[same_type],
    only_ext = ext_defs$name[!known],
    conflicts = if (any(conflict)) {
      data.frame(
        name = ext_defs$name[conflict],
        local_type = local_defs$type[idx[conflict]],
        ext_type = ext_defs$type[conflict],
        stringsAsFactors = FALSE
      )
    } else {
      empty_conflicts
    }
  )
}

#' Inspect Custom Field Import Eligibility
#'
#' Identifies external custom metadata fields compatible with the local schema and highlights datatype conflicts.
#'
#' @param local_path Character path to local SQLite database.
#' @param ext_path Character path to external SQLite database.
#' @return A list categorizing fields as `importable`, `shared`, `only_ext`, and `conflicts`.
#' @export
importable_custom_fields <- function(local_path, ext_path) {
  con <- dbConnect(SQLite(), ":memory:")
  on.exit(dbDisconnect(con), add = TRUE)
  attach_ro(con, local_path, "loc")
  attach_ro(con, ext_path, "ext")

  .custom_split(.custom_defs(con, "loc"), .custom_defs(con, "ext"))
}

# Merges selected custom fields and updates record values within transaction.
.write_custom <- function(con, selected_fields, ext_tables) {
  if (!length(selected_fields) || !all(CUSTOM_TABLES %in% ext_tables)) {
    return(0L)
  }

  for (sql in CUSTOM_SCHEMA_DDL) {
    dbExecute(con, sql)
  }

  ext_defs <- dbGetQuery(
    con,
    "SELECT id, name, type, description, levels FROM ext.phylotrace_custom_fields
      ORDER BY COALESCE(position, id)"
  )
  ext_defs <- ext_defs[ext_defs$name %in% selected_fields, , drop = FALSE]
  if (!nrow(ext_defs)) {
    return(0L)
  }

  local_defs <- dbGetQuery(
    con,
    "SELECT id, name, type FROM main.phylotrace_custom_fields"
  )
  split <- .custom_split(local_defs, ext_defs[, c("name", "type")])

  stamp <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  ext_ids <- integer(0)
  local_ids <- integer(0)

  for (i in seq_len(nrow(ext_defs))) {
    nm <- ext_defs$name[[i]]
    if (!nm %in% split$importable) {
      next
    }

    hit <- local_defs[tolower(local_defs$name) == tolower(nm), , drop = FALSE]
    local_id <- if (nrow(hit)) {
      as.integer(hit$id[[1]])
    } else {
      position <- dbGetQuery(
        con,
        "SELECT COALESCE(MAX(COALESCE(position, id)), 0) + 1 AS pos
           FROM main.phylotrace_custom_fields"
      )$pos[[1]]
      dbExecute(
        con,
        "INSERT INTO main.phylotrace_custom_fields
           (name, type, description, levels, position, created, modified)
         VALUES (?, ?, ?, ?, ?, ?, ?)",
        params = list(
          nm,
          ext_defs$type[[i]],
          ext_defs$description[[i]],
          ext_defs$levels[[i]],
          as.integer(position),
          stamp,
          stamp
        )
      )
      new_id <- as.integer(
        dbGetQuery(con, "SELECT last_insert_rowid() AS id")$id[[1]]
      )
      local_defs <- rbind(
        local_defs,
        data.frame(
          id = new_id,
          name = nm,
          type = ext_defs$type[[i]],
          stringsAsFactors = FALSE
        )
      )
      new_id
    }

    ext_ids <- c(ext_ids, as.integer(ext_defs$id[[i]]))
    local_ids <- c(local_ids, local_id)
  }

  if (!length(ext_ids)) {
    return(0L)
  }

  dbWriteTable(
    con,
    "custom_map",
    data.frame(ext_id = ext_ids, local_id = local_ids),
    temporary = TRUE,
    overwrite = TRUE
  )

  dbExecute(
    con,
    "DELETE FROM main.phylotrace_custom_values
      WHERE isolate IN (
        SELECT final_isolate FROM import_isolates WHERE action = 'overwrite'
      )"
  )

  n <- dbExecute(
    con,
    "INSERT OR REPLACE INTO main.phylotrace_custom_values
       (field_id, isolate, value)
       SELECT cm.local_id, s.final_isolate, ev.value
         FROM ext.phylotrace_custom_values ev
         JOIN custom_map cm ON cm.ext_id = ev.field_id
         JOIN import_isolates s ON s.ext_isolate = ev.isolate"
  )
  dbExecute(con, "DROP TABLE custom_map")

  as.integer(n)
}

# ---------------------------------------------------------------------------
# Analysis-result tables (classical_mlst / amr_*)
# ---------------------------------------------------------------------------

# Merges individual isolate-keyed analysis tables from external connection.
.merge_result_table <- function(con, nm) {
  if (!nm %in% dbListTables(con)) {
    ddl <- dbGetQuery(
      con,
      "SELECT sql FROM ext.sqlite_master WHERE type = 'table' AND name = ?",
      params = list(nm)
    )$sql
    if (!length(ddl) || is.na(ddl[1])) {
      return(0L)
    }
    dbExecute(con, ddl[1])
  }

  ext_cols <- names(dbGetQuery(
    con,
    sprintf("SELECT * FROM ext.%s LIMIT 0", nm)
  ))
  cols <- setdiff(intersect(dbListFields(con, nm), ext_cols), "id")
  if (!("isolate" %in% cols)) {
    return(0L)
  }

  # Replace overwritten isolates' existing rows so re-typed isolates never
  # accumulate stale result rows.
  dbExecute(
    con,
    sprintf(
      "DELETE FROM main.%s WHERE isolate IN
         (SELECT final_isolate FROM import_isolates WHERE action = 'overwrite')",
      nm
    )
  )

  quoted <- vapply(cols, .quote_ident, character(1))
  select_exprs <- vapply(
    cols,
    function(c) {
      if (identical(c, "isolate")) {
        "s.final_isolate"
      } else {
        paste0("em.", .quote_ident(c))
      }
    },
    character(1)
  )

  dbExecute(
    con,
    sprintf(
      "INSERT OR REPLACE INTO main.%s (%s)
         SELECT %s
           FROM ext.%s em
           JOIN import_isolates s ON s.ext_isolate = em.isolate",
      nm,
      paste(quoted, collapse = ", "),
      paste(select_exprs, collapse = ", "),
      nm
    )
  )
}

# ---------------------------------------------------------------------------
# Backups
# ---------------------------------------------------------------------------

#' List Database Backup Files
#'
#' Scans directory for existing timestamped backup copies of the specified database file.
#'
#' @param db_path Character path to SQLite database.
#' @return Character vector of backup file paths ordered by modification time descending.
#' @export
list_backups <- function(db_path) {
  if (is.null(db_path) || length(db_path) != 1 || is.na(db_path)) {
    return(character(0))
  }
  prefix <- paste0(basename(db_path), ".bak-")
  files <- list.files(dirname(db_path), full.names = TRUE)
  files <- files[startsWith(basename(files), prefix)]
  files[order(file.mtime(files), decreasing = TRUE)]
}

#' Restore Database Backup
#'
#' Replaces the active database file atomically with a designated backup file, capturing a safety backup prior to swap.
#'
#' @param db_path Character path to active database file.
#' @param backup_path Character path to source backup file.
#' @return Character path of restored database.
#' @export
restore_backup <- function(db_path, backup_path) {
  if (!file.exists(backup_path)) {
    stop("Backup no longer exists: ", backup_path)
  }

  if (file.exists(db_path)) {
    file.rename(db_path, .new_backup_path(db_path))
  }

  staging <- paste0(db_path, ".restore")
  if (!file.copy(backup_path, staging, overwrite = TRUE)) {
    stop("Could not stage the backup for restore.")
  }
  if (!file.rename(staging, db_path)) {
    unlink(staging)
    stop("Could not restore the backup.")
  }
  log_event("DB", "restore-backup", sprintf("from %s", backup_path))
  invisible(db_path)
}

# Constructs unique, non-colliding timestamped backup path string.
.new_backup_path <- function(db_path) {
  base <- paste0(db_path, ".bak-", format(Sys.time(), "%Y%m%d-%H%M%S"))
  candidate <- base
  i <- 1L
  while (file.exists(candidate)) {
    i <- i + 1L
    candidate <- paste0(base, "-", i)
  }
  candidate
}

# ---------------------------------------------------------------------------
# The merge
# ---------------------------------------------------------------------------

#' Merge External Database
#'
#' Executes full transactional merge of external database records into local database copy.
#'
#' @param local_path Character path to target local SQLite database.
#' @param ext_path Character path to external SQLite database.
#' @param resolutions Data frame specifying action mappings per isolate.
#' @param metadata_cols Character vector of metadata columns to import.
#' @param include_classical Logical flag to include classical MLST results.
#' @param include_amr Logical flag to include AMR analysis tables.
#' @param custom_fields Character vector of custom metadata variables to include.
#' @param backup Logical indicating whether to retain timestamped original database backup.
#' @param progress Optional progress reporting callback function.
#' @param source_file Display label for external database source provenance.
#' @return List summarizing total records added, modified, and created during merge.
#' @export
merge_databases <- function(
  local_path,
  ext_path,
  resolutions,
  metadata_cols = character(0),
  include_classical = FALSE,
  include_amr = FALSE,
  custom_fields = character(0),
  backup = TRUE,
  progress = NULL,
  source_file = NULL
) {
  progress <- progress %||% .noop_progress

  progress(0.02, "Checking compatibility …")
  compat <- check_import_compatibility(local_path, ext_path)
  if (isTRUE(attr(compat, "blocked"))) {
    failed <- compat$check[compat$status == "fail"]
    stop(
      "The external database is not compatible: ",
      paste(failed, collapse = ", ")
    )
  }

  local_isolates <- attr(compat, "local")$isolates
  ext_isolates <- attr(compat, "ext")$isolates
  accepted <- .validate_resolutions(resolutions, local_isolates, ext_isolates)

  progress(0.08, "Staging external database …")
  prep_ext <- prepare_source(ext_path)
  on.exit(release_source(prep_ext), add = TRUE)

  work <- paste0(local_path, ".import-work")
  if (file.exists(work)) {
    unlink(work)
  }

  committed <- FALSE
  on.exit(
    if (!committed && file.exists(work)) unlink(work),
    add = TRUE
  )

  progress(0.12, "Copying database …")
  if (!file.copy(local_path, work)) {
    stop("Could not create a working copy of the database.")
  }

  make_metadata_table(work)
  hash_database(work)

  organism <- load_db_species(local_path)

  con <- dbConnect(SQLite(), work, busy_timeout = 5000)
  on.exit(if (dbIsValid(con)) dbDisconnect(con), add = TRUE)

  attach_ro(con, prep_ext$path, "ext")
  ext_tables <- dbGetQuery(
    con,
    "SELECT name FROM ext.sqlite_master WHERE type = 'table'"
  )$name

  dbExecute(
    con,
    "DELETE FROM main.hashes WHERE id NOT IN (SELECT id FROM main.sequences)"
  )

  for (sql in .TRANSIENT_INDEXES) {
    dbExecute(con, sql)
  }

  progress(0.25, "Matching alleles by sequence hash …")

  dbWriteTable(
    con,
    "import_isolates",
    accepted[, c("ext_isolate", "final_isolate", "action")],
    temporary = TRUE,
    overwrite = TRUE
  )

  dbExecute(
    con,
    "CREATE TEMP TABLE local_alleles AS
       SELECT DISTINCT m.gene AS gene, h.hash AS hash, m.seqid AS local_seqid
         FROM main.mlst m JOIN main.hashes h ON h.id = m.seqid"
  )
  dbExecute(
    con,
    "CREATE TEMP TABLE ext_used AS
       SELECT DISTINCT em.gene AS gene, eh.hash AS hash, em.seqid AS ext_seqid
         FROM ext.mlst em
         JOIN ext.hashes eh ON eh.id = em.seqid
         JOIN import_isolates s ON s.ext_isolate = em.souche"
  )
  dbExecute(
    con,
    "CREATE TEMP TABLE seqid_map AS
       SELECT e.ext_seqid AS ext_seqid, e.gene AS gene, e.hash AS hash,
              l.local_seqid AS local_seqid
         FROM ext_used e
         LEFT JOIN local_alleles l ON l.gene = e.gene AND l.hash = e.hash"
  )
  dbExecute(con, "CREATE INDEX temp.ix_seqid_map ON seqid_map(ext_seqid)")

  # Re-link existing sequence entries left orphaned by prior deletion operations
  dbExecute(
    con,
    "UPDATE seqid_map
        SET local_seqid = (
              SELECT MIN(h.id) FROM main.hashes h
               WHERE h.hash = seqid_map.hash
                 AND h.id NOT IN (SELECT seqid FROM main.mlst)
            )
      WHERE local_seqid IS NULL
        AND EXISTS (
              SELECT 1 FROM main.hashes h
               WHERE h.hash = seqid_map.hash
                 AND h.id NOT IN (SELECT seqid FROM main.mlst)
            )"
  )

  n_new_alleles <- 0L
  n_calls <- 0L
  result_rows <- 0L
  custom_rows <- 0L

  result_tables <- intersect(
    c(
      if (isTRUE(include_classical)) "classical_mlst",
      if (isTRUE(include_amr)) c("amr_results", "amr_summary"),
      "genome_hashes"
    ),
    ext_tables
  )

  dbBegin(con)
  tryCatch(
    {
      progress(0.45, "Adding novel alleles …")

      dbExecute(
        con,
        "CREATE TEMP TABLE new_alleles AS
           SELECT ext_seqid, gene, hash,
                  (SELECT COALESCE(MAX(id), 0) FROM main.sequences)
                    + ROW_NUMBER() OVER (ORDER BY ext_seqid) AS new_id
             FROM seqid_map
            WHERE local_seqid IS NULL"
      )
      n_new_alleles <- dbGetQuery(
        con,
        "SELECT COUNT(*) AS n FROM new_alleles"
      )$n

      dbExecute(
        con,
        "INSERT INTO main.sequences (id, sequence)
           SELECT n.new_id, s.sequence
             FROM new_alleles n JOIN ext.sequences s ON s.id = n.ext_seqid"
      )
      dbExecute(
        con,
        "INSERT INTO main.hashes (id, hash) SELECT new_id, hash FROM new_alleles"
      )
      dbExecute(
        con,
        "UPDATE seqid_map
            SET local_seqid = (
              SELECT new_id FROM new_alleles n WHERE n.ext_seqid = seqid_map.ext_seqid
            )
          WHERE local_seqid IS NULL"
      )

      progress(0.6, "Replacing overwritten isolates …")
      dbExecute(
        con,
        "DELETE FROM main.mlst
          WHERE souche IN (
            SELECT final_isolate FROM import_isolates WHERE action = 'overwrite'
          )"
      )
      if ("metadata" %in% dbListTables(con)) {
        dbExecute(
          con,
          "DELETE FROM main.metadata
            WHERE isolate IN (
              SELECT final_isolate FROM import_isolates WHERE action = 'overwrite'
            )"
        )
      }

      progress(0.7, "Writing allele calls …")
      n_calls <- dbExecute(
        con,
        "INSERT INTO main.mlst (souche, gene, seqid)
           SELECT s.final_isolate, em.gene, sm.local_seqid
             FROM ext.mlst em
             JOIN import_isolates s ON s.ext_isolate = em.souche
             JOIN seqid_map sm ON sm.ext_seqid = em.seqid"
      )

      expected <- dbGetQuery(
        con,
        "SELECT COUNT(*) AS n FROM ext.mlst em
           JOIN import_isolates s ON s.ext_isolate = em.souche"
      )$n
      if (!identical(as.integer(n_calls), as.integer(expected))) {
        stop(sprintf(
          "external database is inconsistent: %d of %d allele calls reference a missing sequence",
          expected - n_calls,
          expected
        ))
      }

      progress(0.85, "Merging metadata …")
      source_label <- register_source(
        con,
        uuid = db_uuid(con, "ext"),
        file_name = source_file %||% ext_path
      )
      .write_metadata(
        con,
        accepted,
        metadata_cols,
        organism,
        ext_tables,
        source_label
      )

      if (length(custom_fields)) {
        progress(0.87, "Merging custom variables …")
        custom_rows <- .write_custom(con, custom_fields, ext_tables)
      }

      if (length(result_tables)) {
        progress(0.88, "Merging analysis results …")
        for (nm in result_tables) {
          result_rows <- result_rows +
            as.integer(.merge_result_table(con, nm))
        }
      }

      dbCommit(con)
    },
    error = function(e) {
      try(dbRollback(con), silent = TRUE)
      stop("Merge failed and was rolled back: ", conditionMessage(e))
    }
  )

  progress(0.92, "Finalising …")

  for (nm in names(.TRANSIENT_INDEXES)) {
    dbExecute(con, sprintf("DROP INDEX IF EXISTS %s", nm))
  }
  dbExecute(con, "DETACH DATABASE ext")
  dbDisconnect(con)

  backup_path <- .swap_in(local_path, work, backup)
  committed <- TRUE

  progress(1, "Done")

  log_event(
    "DB",
    "merge",
    sprintf(
      "from %s (source '%s') | %d added, %d overwritten, %d renamed, %d new allele(s)",
      source_file %||% ext_path,
      source_label,
      sum(accepted$action == "add"),
      sum(accepted$action == "overwrite"),
      sum(accepted$action == "rename"),
      as.integer(n_new_alleles)
    )
  )

  list(
    result_rows = as.integer(result_rows),
    custom_rows = as.integer(custom_rows),
    added = sum(accepted$action == "add"),
    overwritten = sum(accepted$action == "overwrite"),
    renamed = sum(accepted$action == "rename"),
    skipped = sum(resolutions$action == "skip"),
    new_alleles = as.integer(n_new_alleles),
    calls = as.integer(n_calls),
    backup_path = backup_path,
    source = source_label
  )
}

# Replaces active database file with working copy atomically, rolling back if rename steps fail.
.swap_in <- function(local_path, work, backup) {
  if (!backup) {
    if (!file.rename(work, local_path)) {
      stop("Merge succeeded but the database could not be replaced.")
    }
    return(NA_character_)
  }

  backup_path <- .new_backup_path(local_path)
  if (!file.rename(local_path, backup_path)) {
    stop("Merge succeeded but the backup could not be written.")
  }
  if (!file.rename(work, local_path)) {
    file.rename(backup_path, local_path)
    stop("Merge succeeded but the database could not be replaced; rolled back.")
  }
  backup_path
}
