# app/logic/db_import.R
#
# Merges a peer PhyloTrace database into the primary loaded database.
#
# `seqid` values are database-specific, while the sequence itself identifies an
# allele across systems. pyMLST stores every distinct sequence once
# (`sequences` is UNIQUE(sequence)), whatever locus it was called for, so two
# loci can share one `seqid`; the locus lives on the `mlst` row.
#
# Consequently, external `seqid` values are not trusted directly. Every incoming
# sequence is matched to a local `sequences.id` by its content, regardless of
# locus, and a new local id (with a freshly computed sha256 in `hashes`) is
# allocated only for a sequence the primary database does not hold yet.
#
# To ensure atomic execution and data safety, operations are never performed
# directly on the live database file. Merges execute against a temporary copy
# in the same target directory inside a single transaction. The temporary file
# replaces the live file using an atomic rename operation upon successful commit.
# The original database state is preserved as a timestamped backup (`.bak-*`).

box::use(
  DBI,
  RSQLite[SQLite],
  openssl[sha256],
  rlang[`%||%`],
)

box::use(
  app / logic / custom_fields[CUSTOM_SCHEMA_DDL],
  app / logic / amr[AMR_RESULTS_DDL, AMR_SUMMARY_DDL],
  app / logic / database_functions[ISOLATE_KEYED_TABLES, load_db_species, sync_metadata_table],
  app / logic / db_compat[REF_SOUCHE, attach_ro, check_import_compatibility, connect_ro],
  app / logic / db_connect[connect],
  app / logic / db_sources[SOURCE_COL, db_uuid, register_source],
  app / logic / genome_hash[GENOME_HASHES_DDL],
  app / logic / logging[log_event],
  app / logic / mlst_scheme[write_mlst_scheme],
  app / logic / provenance[PROVENANCE_DDL],
  app / logic / pymlst[CLASSICAL_MLST_DDL, hash_database],
)

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
  if (!"hashes" %in% DBI$dbListTables(con)) {
    return(FALSE)
  }
  DBI$dbGetQuery(
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
  DBI$dbDisconnect(con)

  if (complete) {
    return(list(path = path, temp = FALSE))
  }

  tmp <- tempfile("phylotrace_import_", fileext = ".db")
  if (!file.copy(path, tmp)) {
    unlink(tmp)
    stop("Could not stage the database for hashing.")
  }
  hashed <- tryCatch(hash_database(tmp), error = function(e) e)
  if (inherits(hashed, "error")) {
    unlink(tmp)
    stop(conditionMessage(hashed))
  }
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
  df <- DBI$dbGetQuery(
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
  on.exit(DBI$dbDisconnect(con), add = TRUE)

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
  tbls <- DBI$dbGetQuery(
    con,
    sprintf("SELECT name FROM %s.sqlite_master WHERE type = 'table'", schema)
  )$name
  if (!"metadata" %in% tbls) {
    return(character(0))
  }
  names(DBI$dbGetQuery(
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

  con <- DBI$dbConnect(SQLite(), ":memory:")
  on.exit(DBI$dbDisconnect(con), add = TRUE)
  attach_ro(con, prep_local$path, "loc")
  attach_ro(con, prep_ext$path, "ext")

  n_new_alleles <- 0L
  n_shared_alleles <- 0L
  if (nrow(accepted)) {
    DBI$dbWriteTable(
      con,
      "acc",
      data.frame(isolate = accepted$ext_isolate, stringsAsFactors = FALSE),
      temporary = TRUE,
      overwrite = TRUE
    )
    # Counted per distinct sequence, the unit the merge allocates: a sequence
    # the local database holds under any locus is not novel.
    counts <- DBI$dbGetQuery(
      con,
      "WITH used AS (
         SELECT DISTINCT eh.hash AS hash
           FROM ext.mlst em
           JOIN ext.hashes eh ON eh.id = em.seqid
           JOIN acc a ON a.isolate = em.souche
       )
       SELECT
         SUM(CASE WHEN la.hash IS NULL THEN 1 ELSE 0 END) AS novel,
         SUM(CASE WHEN la.hash IS NULL THEN 0 ELSE 1 END) AS shared
       FROM used u
       LEFT JOIN (SELECT DISTINCT hash FROM loc.hashes) la ON la.hash = u.hash"
    )
    n_new_alleles <- .as_count(counts$novel)
    n_shared_alleles <- .as_count(counts$shared)
  }

  local_cols <- .metadata_cols(con, "loc")
  ext_cols <- .metadata_cols(con, "ext")
  custom <- .custom_split(.custom_defs(con, "loc"), .custom_defs(con, "ext"))

  # Column names compare case-insensitively, as SQLite does.
  reserved <- tolower(METADATA_RESERVED)
  ext_free <- ext_cols[!tolower(ext_cols) %in% reserved]
  local_free <- local_cols[!tolower(local_cols) %in% reserved]

  list(
    n_new_isolates = sum(classification$status == "new"),
    n_identical_dupes = sum(classification$status == "identical_duplicate"),
    n_name_clashes = sum(classification$status == "name_clash"),
    n_accepted = nrow(accepted),
    n_new_alleles = n_new_alleles,
    n_shared_alleles = n_shared_alleles,
    metadata_shared = ext_free[tolower(ext_free) %in% tolower(local_free)],
    metadata_only_ext = ext_free[!tolower(ext_free) %in% tolower(local_free)],
    metadata_only_local = local_free[!tolower(local_free) %in% tolower(ext_free)],
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
    DBI$dbGetQuery(con, "SELECT * FROM ext.metadata")
  } else {
    NULL
  }

  selected <- intersect(selected_cols, names(ext_meta) %||% character(0))
  selected <- selected[!tolower(selected) %in% tolower(METADATA_RESERVED)]

  # `sync_metadata_table()` skips creation when the local database has no
  # isolates yet (a freshly downloaded scheme), so the table may not exist.
  if (!"metadata" %in% .main_tables(con)) {
    DBI$dbExecute(
      con,
      sprintf(
        "CREATE TABLE main.metadata (%s)",
        paste(
          paste(.quote_ident(c(METADATA_RESERVED, selected)), "TEXT"),
          collapse = ", "
        )
      )
    )
  }

  # SQLite column names are case-insensitive, so an external "Country" lands
  # in a local "country" rather than being added a second time. Both the
  # reserved columns and every newly selected field must exist before the
  # append.
  local_fields <- names(DBI$dbGetQuery(con, "SELECT * FROM main.metadata LIMIT 0"))
  target <- local_fields[match(tolower(selected), tolower(local_fields))]
  target[is.na(target)] <- selected[is.na(target)]
  missing_reserved <- METADATA_RESERVED[!tolower(METADATA_RESERVED) %in% tolower(local_fields)]
  for (col in c(missing_reserved, setdiff(target, local_fields))) {
    DBI$dbExecute(
      con,
      sprintf("ALTER TABLE main.metadata ADD COLUMN %s TEXT", .quote_ident(col))
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

  for (i in seq_along(selected)) {
    rows[[target[i]]] <- as.character(ext_meta[[selected[i]]][idx])
  }

  DBI$dbWriteTable(con, "metadata", rows, append = TRUE)
  invisible(nrow(rows))
}

# ---------------------------------------------------------------------------
# Custom variables
# ---------------------------------------------------------------------------

# Fetches custom field definitions from specified attached database schema.
.custom_defs <- function(con, schema) {
  tbls <- DBI$dbGetQuery(
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
  DBI$dbGetQuery(
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
  con <- DBI$dbConnect(SQLite(), ":memory:")
  on.exit(DBI$dbDisconnect(con), add = TRUE)
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
    DBI$dbExecute(con, sql)
  }

  ext_defs <- DBI$dbGetQuery(
    con,
    "SELECT id, name, type, description, levels FROM ext.phylotrace_custom_fields
      ORDER BY COALESCE(position, id)"
  )
  ext_defs <- ext_defs[ext_defs$name %in% selected_fields, , drop = FALSE]
  if (!nrow(ext_defs)) {
    return(0L)
  }

  local_defs <- DBI$dbGetQuery(
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
      position <- DBI$dbGetQuery(
        con,
        "SELECT COALESCE(MAX(COALESCE(position, id)), 0) + 1 AS pos
           FROM main.phylotrace_custom_fields"
      )$pos[[1]]
      DBI$dbExecute(
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
        DBI$dbGetQuery(con, "SELECT last_insert_rowid() AS id")$id[[1]]
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

  DBI$dbWriteTable(
    con,
    "custom_map",
    data.frame(ext_id = ext_ids, local_id = local_ids),
    temporary = TRUE,
    overwrite = TRUE
  )

  DBI$dbExecute(
    con,
    "DELETE FROM main.phylotrace_custom_values
      WHERE isolate IN (
        SELECT final_isolate FROM import_isolates WHERE action = 'overwrite'
      )"
  )

  n <- DBI$dbExecute(
    con,
    "INSERT OR REPLACE INTO main.phylotrace_custom_values
       (field_id, isolate, value)
       SELECT cm.local_id, s.final_isolate, ev.value
         FROM ext.phylotrace_custom_values ev
         JOIN custom_map cm ON cm.ext_id = ev.field_id
         JOIN import_isolates s ON s.ext_isolate = ev.isolate"
  )
  DBI$dbExecute(con, "DROP TABLE custom_map")

  as.integer(n)
}

# ---------------------------------------------------------------------------
# Analysis-result tables (classical_mlst / amr_*)
# ---------------------------------------------------------------------------

# The app's own definitions of the result tables, used when the local database
# has none yet: the external file's DDL is never executed.
.RESULT_DDL <- c(
  classical_mlst = sub("EXISTS ", "EXISTS main.", CLASSICAL_MLST_DDL, fixed = TRUE),
  amr_results = sub("EXISTS ", "EXISTS main.", AMR_RESULTS_DDL, fixed = TRUE),
  amr_summary = sub("EXISTS ", "EXISTS main.", AMR_SUMMARY_DDL, fixed = TRUE),
  genome_hashes = sub("EXISTS ", "EXISTS main.", GENOME_HASHES_DDL, fixed = TRUE),
  typing_provenance = sub("EXISTS ", "EXISTS main.", PROVENANCE_DDL, fixed = TRUE)
)

# Names of the tables in the working copy itself; the attached external
# database and temp tables are left out.
.main_tables <- function(con) {
  DBI$dbGetQuery(con, "SELECT name FROM main.sqlite_master WHERE type = 'table'")$name
}

# Merges individual isolate-keyed analysis tables from external connection.
# Rows of overwritten isolates were already removed by the caller.
.merge_result_table <- function(con, nm) {
  if (!nm %in% .main_tables(con)) {
    DBI$dbExecute(con, .RESULT_DDL[[nm]])
  }

  ext_cols <- names(DBI$dbGetQuery(
    con,
    sprintf("SELECT * FROM ext.%s LIMIT 0", nm)
  ))
  local_cols <- names(DBI$dbGetQuery(
    con,
    sprintf("SELECT * FROM main.%s LIMIT 0", nm)
  ))
  cols <- setdiff(intersect(local_cols, ext_cols), "id")
  if (!("isolate" %in% cols)) {
    return(0L)
  }

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

  DBI$dbExecute(
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

  # Stage first, so the live file is only moved once the copy is complete.
  staging <- paste0(db_path, ".restore")
  if (!file.copy(backup_path, staging, overwrite = TRUE)) {
    unlink(staging)
    stop("Could not stage the backup for restore.")
  }

  safety <- NA_character_
  if (file.exists(db_path)) {
    safety <- .new_backup_path(db_path)
    if (!file.rename(db_path, safety)) {
      unlink(staging)
      stop("Could not back up the current database, so nothing was restored.")
    }
  }
  if (!file.rename(staging, db_path)) {
    unlink(staging)
    if (!is.na(safety)) {
      file.rename(safety, db_path)
    }
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
#' @param adopt_mlst_scheme Logical; replace the local classical MLST scheme
#'   with the external one. Only allowed when `check_import_compatibility()`
#'   offers it (no local scheme, or a newer copy of the same scheme).
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
  source_file = NULL,
  adopt_mlst_scheme = FALSE
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
  adopt_mlst_scheme <- isTRUE(adopt_mlst_scheme)
  if (adopt_mlst_scheme && !isTRUE(attr(compat, "mlst_scheme")$adopt_offer)) {
    stop(
      "The external database's classical MLST scheme cannot be adopted: ",
      attr(compat, "mlst_scheme")$detail %||% "it has none"
    )
  }

  local_isolates <- attr(compat, "local")$isolates
  ext_isolates <- attr(compat, "ext")$isolates
  accepted <- .validate_resolutions(resolutions, local_isolates, ext_isolates)

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

  sync_metadata_table(work)
  hash_database(work)

  organism <- load_db_species(local_path)

  con <- connect(work)
  on.exit(if (DBI$dbIsValid(con)) DBI$dbDisconnect(con), add = TRUE)

  # The external `hashes` table is never consulted: sequences are matched by
  # content and the hash of every new one is computed here.
  attach_ro(con, ext_path, "ext")
  ext_tables <- DBI$dbGetQuery(
    con,
    "SELECT name FROM ext.sqlite_master WHERE type = 'table'"
  )$name

  DBI$dbExecute(
    con,
    "DELETE FROM main.hashes WHERE id NOT IN (SELECT id FROM main.sequences)"
  )

  for (sql in .TRANSIENT_INDEXES) {
    DBI$dbExecute(con, sql)
  }

  progress(0.25, "Matching alleles by sequence …")

  DBI$dbWriteTable(
    con,
    "import_isolates",
    accepted[, c("ext_isolate", "final_isolate", "action")],
    temporary = TRUE,
    overwrite = TRUE
  )

  new_seq <- .map_sequences(con)
  n_new_alleles <- nrow(new_seq)
  scheme <- if (adopt_mlst_scheme) .read_ext_mlst_scheme(con) else NULL

  n_calls <- 0L
  result_rows <- 0L
  custom_rows <- 0L

  result_tables <- intersect(
    c(
      if (isTRUE(include_classical)) "classical_mlst",
      if (isTRUE(include_amr)) c("amr_results", "amr_summary"),
      "genome_hashes",
      "typing_provenance"
    ),
    ext_tables
  )

  DBI$dbBegin(con)
  tryCatch(
    {
      progress(0.45, "Adding novel alleles …")
      DBI$dbExecute(
        con,
        "INSERT INTO main.sequences (id, sequence) SELECT id, sequence FROM new_seq"
      )
      DBI$dbExecute(
        con,
        "INSERT INTO main.hashes (id, hash) SELECT id, hash FROM new_seq"
      )
      DBI$dbExecute(
        con,
        "UPDATE ext_seq
            SET local_seqid = (SELECT m.id FROM new_map m WHERE m.ext_seqid = ext_seq.ext_seqid)
          WHERE local_seqid IS NULL"
      )

      progress(0.6, "Replacing overwritten isolates …")
      # Everything recorded about a replaced isolate goes, whatever is imported
      # in its place: results the external file does not carry, or that were
      # left out, would otherwise describe the old genome under the new one.
      main_tables <- .main_tables(con)
      for (nm in c("metadata", intersect(ISOLATE_KEYED_TABLES, main_tables))) {
        if (!nm %in% main_tables) {
          next
        }
        DBI$dbExecute(
          con,
          sprintf(
            "DELETE FROM main.%s WHERE isolate IN (
               SELECT final_isolate FROM import_isolates WHERE action = 'overwrite'
             )",
            nm
          )
        )
      }
      DBI$dbExecute(
        con,
        "DELETE FROM main.mlst
          WHERE souche IN (
            SELECT final_isolate FROM import_isolates WHERE action = 'overwrite'
          )"
      )

      progress(0.7, "Writing allele calls …")
      n_calls <- DBI$dbExecute(
        con,
        "INSERT INTO main.mlst (souche, gene, seqid)
           SELECT s.final_isolate, em.gene, es.local_seqid
             FROM ext.mlst em
             JOIN import_isolates s ON s.ext_isolate = em.souche
             JOIN ext_seq es ON es.ext_seqid = em.seqid
            WHERE es.local_seqid IS NOT NULL"
      )

      expected <- DBI$dbGetQuery(
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
        .blank_provenance(
          con,
          classical = "classical_mlst" %in% result_tables,
          amr = "amr_results" %in% result_tables
        )
      }

      if (!is.null(scheme)) {
        progress(0.9, "Adopting the classical MLST scheme …")
        .adopt_mlst_scheme(con, scheme)
      }

      DBI$dbCommit(con)
    },
    error = function(e) {
      try(DBI$dbRollback(con), silent = TRUE)
      stop("Merge failed and was rolled back: ", conditionMessage(e))
    }
  )

  progress(0.92, "Finalising …")

  for (nm in names(.TRANSIENT_INDEXES)) {
    DBI$dbExecute(con, sprintf("DROP INDEX IF EXISTS %s", nm))
  }
  DBI$dbExecute(con, "DETACH DATABASE ext")
  DBI$dbDisconnect(con)

  backup_path <- .swap_in(local_path, work, backup)
  committed <- TRUE

  progress(1, "Done")

  log_event(
    "DB",
    "merge",
    sprintf(
      "from %s (source '%s') | %d added, %d overwritten, %d renamed, %d new allele(s)%s",
      source_file %||% ext_path,
      source_label,
      sum(accepted$action == "add"),
      sum(accepted$action == "overwrite"),
      sum(accepted$action == "rename"),
      as.integer(n_new_alleles),
      if (!is.null(scheme)) ", classical MLST scheme adopted" else ""
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
    mlst_scheme_adopted = !is.null(scheme),
    backup_path = backup_path,
    source = source_label
  )
}

# Maps every external sequence the accepted isolates use onto a local
# `sequences.id` by content, whatever locus holds it locally, and prepares the
# rows for sequences that are new: one new id per distinct sequence, since
# pyMLST's `sequences` is UNIQUE(sequence). Leaves temp tables `ext_seq`
# (ext_seqid -> local_seqid), `new_seq` (id, sequence, hash) and `new_map`
# (ext_seqid -> new id) behind and returns the new sequences.
.map_sequences <- function(con) {
  DBI$dbExecute(
    con,
    "CREATE TEMP TABLE ext_seq AS
       SELECT DISTINCT em.seqid AS ext_seqid, CAST(NULL AS INTEGER) AS local_seqid
         FROM ext.mlst em
         JOIN import_isolates s ON s.ext_isolate = em.souche"
  )
  DBI$dbExecute(con, "CREATE INDEX temp.ix_ext_seq ON ext_seq(ext_seqid)")
  DBI$dbExecute(
    con,
    "UPDATE ext_seq
        SET local_seqid = (
              SELECT MIN(s.id)
                FROM ext.sequences es
                JOIN main.sequences s ON s.sequence = es.sequence
               WHERE es.id = ext_seq.ext_seqid
            )"
  )

  novel <- DBI$dbGetQuery(
    con,
    "SELECT e.ext_seqid AS ext_seqid, es.sequence AS sequence
       FROM ext_seq e
       JOIN ext.sequences es ON es.id = e.ext_seqid
      WHERE e.local_seqid IS NULL AND es.sequence IS NOT NULL
      ORDER BY e.ext_seqid"
  )
  novel$hash <- if (nrow(novel)) as.character(sha256(novel$sequence)) else character(0)

  first <- !duplicated(novel$hash)
  max_id <- .as_count(DBI$dbGetQuery(con, "SELECT MAX(id) AS n FROM main.sequences")$n)
  new_seq <- data.frame(
    id = max_id + seq_len(sum(first)),
    sequence = novel$sequence[first],
    hash = novel$hash[first],
    stringsAsFactors = FALSE
  )
  new_map <- data.frame(
    ext_seqid = novel$ext_seqid,
    id = new_seq$id[match(novel$hash, new_seq$hash)]
  )
  DBI$dbWriteTable(con, "new_seq", new_seq, temporary = TRUE, overwrite = TRUE)
  DBI$dbWriteTable(con, "new_map", new_map, temporary = TRUE, overwrite = TRUE)
  new_seq
}

# Clears the provenance claims about results that were not carried over, so a
# merged isolate never states a classical MLST or AMR status it has no rows for.
.blank_provenance <- function(con, classical, amr) {
  if (!"typing_provenance" %in% .main_tables(con)) {
    return(invisible(0L))
  }
  cols <- names(DBI$dbGetQuery(con, "SELECT * FROM main.typing_provenance LIMIT 0"))
  drop <- c(
    if (!classical) grep("^cla_", cols, value = TRUE),
    if (!amr) grep("^amr_", cols, value = TRUE)
  )
  if (!length(drop)) {
    return(invisible(0L))
  }
  invisible(DBI$dbExecute(
    con,
    sprintf(
      "UPDATE main.typing_provenance SET %s
        WHERE isolate IN (SELECT final_isolate FROM import_isolates)",
      paste(paste(.quote_ident(drop), "= NULL"), collapse = ", ")
    )
  ))
}

# Reads the external classical MLST scheme in the shape write_mlst_scheme()
# takes. The counts and the import time are derived again when it is written.
.read_ext_mlst_scheme <- function(con) {
  kv <- DBI$dbGetQuery(con, "SELECT key, value FROM ext.cla_scheme_info")
  kv <- kv[!kv$key %in% c("imported_at", "loci", "profiles", "alleles"), , drop = FALSE]
  info <- as.list(kv$value)
  names(info) <- kv$key
  list(
    scheme = list(
      loci = DBI$dbGetQuery(con, "SELECT locus FROM ext.cla_scheme_loci ORDER BY position")$locus,
      profiles = DBI$dbGetQuery(con, "SELECT st, locus, allele FROM ext.cla_scheme_profiles"),
      alleles = DBI$dbGetQuery(con, "SELECT locus, allele, sequence FROM ext.cla_scheme_alleles")
    ),
    info = info
  )
}

# Replaces the working copy's classical MLST scheme inside the merge
# transaction.
.adopt_mlst_scheme <- function(con, adopted) {
  write_mlst_scheme(con, adopted$scheme, adopted$info)
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
