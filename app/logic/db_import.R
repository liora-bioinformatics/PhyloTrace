# app/logic/db_import.R
#
# Merge a peer PhyloTrace database into the loaded one.
#
# The whole design rests on one property of the schema: `seqid` is meaningful
# only inside the database that assigned it, but `(gene, sha256(sequence))`
# identifies an allele anywhere. (In a PhyloTrace database each `seqid` belongs
# to exactly one `gene`, and the `hashes` table stores exactly
# `sha256(sequences.sequence)`, so the pair is a bijection with `seqid`.) The
# merge therefore never trusts an incoming `seqid`: it maps every external
# allele onto a local one by content, allocating fresh local ids only for
# alleles the local database has never seen.
#
# Nothing is written to the live database. The merge runs against a copy in the
# same directory, inside a single transaction, and the copy is swapped in with
# an atomic rename only after it has committed cleanly. The displaced original
# is retained as a timestamped `.bak-*` file for rollback.

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
  app / logic / logging[log_event],
)

`%||%` <- function(a, b) if (is.null(a)) b else a

.noop_progress <- function(frac, msg) invisible(NULL)

.quote_ident <- function(x) paste0('"', gsub('"', '""', x), '"')

# SQLite's SUM()/MAX() return NA over an empty set.
.as_count <- function(x) {
  if (!length(x) || is.na(x[[1]])) 0L else as.integer(x[[1]])
}

# Metadata columns the user may never map: `isolate` is the join key,
# `organism` is dictated by the local mlst_type.species, and `called_at` is a
# provenance timestamp handled automatically by `.write_metadata()` (the peer's
# own value when it has one, else the merge time) rather than user-mapped.
#' @export
METADATA_RESERVED <- c("isolate", "organism", "called_at")

# The user-defined custom variables (see app/logic/custom_fields.R).
CUSTOM_TABLES <- c("phylotrace_custom_fields", "phylotrace_custom_values")

# Indexes created transiently on the working copy to speed the merge, then
# dropped before the swap so the live database's schema is left exactly as it
# was. Prefixed so we can never drop one of pyMLST's own indexes.
.TRANSIENT_INDEXES <- c(
  pt_import_ix_sequences_id = "CREATE INDEX IF NOT EXISTS pt_import_ix_sequences_id ON sequences(id)",
  pt_import_ix_hashes_id = "CREATE INDEX IF NOT EXISTS pt_import_ix_hashes_id ON hashes(id)"
)

# Every non-ref isolate's allele profile, as one "gene\thash" line per locus,
# ordered so the concatenation is canonical. DISTINCT guards the join against a
# `hashes` table that carries more than one row for a seqid.
.PROFILE_SQL <- "
  SELECT isolate, group_concat(gh, char(10) ORDER BY gh) AS profile FROM (
    SELECT DISTINCT m.souche AS isolate, m.gene || char(9) || h.hash AS gh
      FROM %1$s.mlst m JOIN %1$s.hashes h ON h.id = m.seqid
     WHERE m.souche <> ?
  ) GROUP BY isolate"

# ---------------------------------------------------------------------------
# Hash availability
# ---------------------------------------------------------------------------

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

#' Guarantee a database has a complete `hashes` table without touching the
#' caller's file.
#'
#' Returns `list(path=, temp=)`. When the source already carries a complete
#' `hashes` table (PhyloTrace exports always do) the original path is returned
#' and nothing is copied — the common case costs nothing. Otherwise the file is
#' copied to a scratch location and hashed there.
#'
#' Call `release_source()` on the result when done.
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

#' A content hash of each isolate's full allele profile, keyed by isolate.
#'
#' Two isolates with the same name in different databases are the *same*
#' isolate only if their profile hashes agree.
#' @export
isolate_profile_hashes <- function(db_path) {
  prep <- prepare_source(db_path)
  on.exit(release_source(prep), add = TRUE)

  con <- connect_ro(prep$path)
  on.exit(dbDisconnect(con), add = TRUE)

  .profiles(con, "main")
}

#' Classify every isolate in the external database against the local one.
#'
#' - `new`: the local database has never seen this isolate name.
#' - `identical_duplicate`: same name, byte-identical allele profile. Nothing to
#'   gain by importing it; defaults to skip.
#' - `name_clash`: same name, *different* profile. The user must decide.
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

#' A safe starting point for the resolution table: import what is new, skip
#' everything that would need a decision.
#' @export
default_resolutions <- function(classification) {
  data.frame(
    ext_isolate = classification$isolate,
    action = ifelse(classification$status == "new", "add", "skip"),
    final_isolate = classification$isolate,
    stringsAsFactors = FALSE
  )
}

#' First free `<name>_imp`, `<name>_imp2`, … not present in `taken`.
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

  # `add`/`rename` must land on a free name; `overwrite` must land on a taken one.
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
      paste(utils::head(accepted$final_isolate[ow][missing], 5), collapse = ", ")
    )
  }

  accepted
}

# ---------------------------------------------------------------------------
# Preview
# ---------------------------------------------------------------------------

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

#' What would this import do? Counts only — nothing is written, and neither
#' database is copied when both already carry their `hashes` table.
#'
#' Classifying collisions means hashing every isolate's full allele profile on
#' both sides, which costs seconds on a real database. Callers that already hold
#' a `classify_isolate_collisions()` result — the Import panel recomputes the
#' preview on every keystroke — should pass it in rather than pay for it again.
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
    # SUM() over an empty set yields NA, not 0.
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

# Runs on the working copy's connection, inside the merge transaction. The local
# `metadata` table gains a column for every selected external-only field; rows
# for imported isolates carry the selected values, NA for local-only fields, and
# the *local* organism regardless of what the peer recorded.
.write_metadata <- function(
  con,
  accepted,
  selected_cols,
  organism,
  ext_tables
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

  # Both the reserved columns (an older table may predate `called_at`) and every
  # newly selected external field must exist before the append.
  for (col in setdiff(c(METADATA_RESERVED, selected), dbListFields(con, "metadata"))) {
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
  peer_called_at <- if (!is.null(ext_meta) && "called_at" %in% names(ext_meta)) {
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

  rows <- data.frame(
    isolate = accepted$final_isolate,
    organism = organism %||% NA_character_,
    called_at = called_at,
    stringsAsFactors = FALSE
  )

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

# The custom-variable definitions of one attached schema ("loc" / "ext"), or an
# empty frame when that side has no custom tables.
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

# Split the external definitions into what can be merged and what cannot.
#
# A variable is matched by name (case-insensitively, the same rule
# `validate_custom_name()` uses). Same name *and* same type merges into the local
# variable; same name but a different type is a conflict and is never imported —
# values are canonicalised for the type they were entered under, so adopting them
# under another type would silently corrupt them.
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

#' Which of an external database's custom variables can be merged into the local
#' one, and which clash.
#'
#' Returns the `.custom_split()` list: `importable` / `shared` / `only_ext`
#' names plus a `conflicts` frame of `name` / `local_type` / `ext_type`. The
#' Import panel offers `importable` in its picker and reports `conflicts` as the
#' reason the rest are missing. Nothing is written.
#' @export
importable_custom_fields <- function(local_path, ext_path) {
  con <- dbConnect(SQLite(), ":memory:")
  on.exit(dbDisconnect(con), add = TRUE)
  attach_ro(con, local_path, "loc")
  attach_ro(con, ext_path, "ext")

  .custom_split(.custom_defs(con, "loc"), .custom_defs(con, "ext"))
}

# Merge the selected custom variables into the working copy, inside the merge
# transaction. Definitions are matched by name (new ones are created, conflicting
# ones skipped — see `.custom_split()`); values follow the `import_isolates`
# rename map, so a renamed isolate carries its values under its new name.
# Returns the number of values written.
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
      # Merging into an existing variable: the local description and levels are
      # the ones this database's users maintain, so the peer's do not overwrite
      # them. Only the values travel.
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

  # Overwritten isolates start from a clean slate, mirroring the metadata and
  # result-table handling: a re-imported isolate must not keep stale values.
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

# Copy one isolate-keyed result table from the attached `ext` database into
# `main`, remapping `isolate` via the `import_isolates` rename map. These tables
# key solely on `isolate` (no `seqid` / allele foreign keys), so no allele remap
# is involved. Runs on the working copy's connection inside the merge
# transaction. Creates the table in `main` from the external DDL when the local
# database never ran that analysis; replaces overwritten isolates' rows first;
# the autoincrement `id` is dropped so SQLite assigns fresh, collision-free ids.
# Returns the number of rows inserted.
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

  # Columns common to both sides, minus the autoincrement id. `isolate` comes from
  # the rename map; the column intersection guards against schema drift.
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

  # OR REPLACE guards `genome_hashes`, whose key is `isolate` itself rather than
  # an autoincrement id. The DELETE above already clears the overwrite path, so
  # this only matters when a caller supplies a `final_isolate` that collides with
  # an isolate the local database already holds: without it the PK conflict
  # would abort the entire merge, where every other result table simply absorbs
  # the row. For the id-keyed tables it is a no-op - `id` is not among `cols`,
  # so no unique constraint is left for a conflict to fire on.
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

#' Timestamped rollback points for `db_path`, newest first.
#' @export
list_backups <- function(db_path) {
  if (is.null(db_path) || length(db_path) != 1 || is.na(db_path)) {
    return(character(0))
  }
  # Prefix matching on the basename, not a regex: a database filename may
  # legitimately contain regex metacharacters.
  prefix <- paste0(basename(db_path), ".bak-")
  files <- list.files(dirname(db_path), full.names = TRUE)
  files <- files[startsWith(basename(files), prefix)]
  files[order(file.mtime(files), decreasing = TRUE)]
}

#' Put `backup_path` back at `db_path`, atomically. The database being replaced
#' is itself backed up first, so a mistaken restore is also reversible.
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
  invisible(db_path)
}

# Second-granularity timestamps collide when a merge and a restore happen in the
# same second — and a colliding name would rename the live database *over* the
# very backup being restored from. Disambiguate with a counter.
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

#' Merge `ext_path` into `local_path`.
#'
#' `resolutions` is a data.frame of `ext_isolate` / `action` / `final_isolate`,
#' where action is one of `add`, `skip`, `overwrite`, `rename` (see
#' `default_resolutions()`). `metadata_cols` names the external metadata columns
#' to adopt; `isolate` and `organism` are handled automatically.
#'
#' The live database is never mutated in place — see the file header.
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
  progress = NULL
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

  # The working copy lives beside the original so the final rename is atomic
  # (a rename across filesystems is not).
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

  # Both must hold before the merge: metadata present and migrated, hashes
  # complete. Each opens and closes its own connection.
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

  # Repair any orphan `hashes` rows before allocating ids. Databases written by
  # older builds of remove_isolates() pruned `sequences` without pruning
  # `hashes`; a stale row whose id we then reused would give one seqid two hash
  # rows. Harmless when there is nothing to prune. The working copy is
  # disposable, so this runs outside the transaction.
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

  # `seqid` is DB-local; `(gene, hash)` is not. Everything below joins on the
  # latter.
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

  # `local_alleles` only carries alleles a strain still references (it joins
  # `mlst`), so an incoming allele whose sequence lives locally as an *orphan* -
  # left behind by remove_isolates(keep_alleles = TRUE), whose seqid no `mlst`
  # row points at anymore - matches nothing above and would be re-inserted as a
  # second `sequences`/`hashes` row sharing the orphan's hash. Reuse the orphan's
  # seqid instead: content is identical (equal hash), so the row is correct, no
  # duplicate hash accumulates, and the orphan becomes live again - which is the
  # whole point of keeping it. An orphan has no gene (that lived in `mlst`), so
  # this match is by hash alone, and it is scoped to orphans so a hash still
  # referenced under another gene is left to the normal new-allele path.
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

  # Isolate-keyed analysis-result tables to carry (only those the peer has).
  result_tables <- intersect(
    c(
      if (isTRUE(include_classical)) "classical_mlst",
      if (isTRUE(include_amr)) c("amr_results", "amr_summary"),
      # Assembly digests are provenance rather than an analysis result, so they
      # are not behind a toggle: they always come along when the peer has them
      # (see GENOME_TABLES in db_export.R).
      "genome_hashes"
    ),
    ext_tables
  )

  dbBegin(con)
  tryCatch(
    {
      progress(0.45, "Adding novel alleles …")

      # MAX(id) is read once, before any insert, so the row numbers extend the
      # existing id space without gaps or collisions.
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
      # `mlst.id` is a rowid alias, so omitting it lets SQLite assign fresh ids
      # that cannot collide with the local ones. `ref` never appears in
      # import_isolates, so the scheme reference is never imported as an isolate.
      n_calls <- dbExecute(
        con,
        "INSERT INTO main.mlst (souche, gene, seqid)
           SELECT s.final_isolate, em.gene, sm.local_seqid
             FROM ext.mlst em
             JOIN import_isolates s ON s.ext_isolate = em.souche
             JOIN seqid_map sm ON sm.ext_seqid = em.seqid"
      )

      # Every incoming call must have found a seqid mapping. A shortfall means
      # the external `mlst` references a seqid its own `sequences` table does
      # not hold, and the inner joins above would have dropped those calls
      # silently — importing an isolate with holes in its profile.
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
      .write_metadata(con, accepted, metadata_cols, organism, ext_tables)

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

  # Leave the schema exactly as we found it.
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
      "from %s | %d added, %d overwritten, %d renamed, %d new allele(s)",
      ext_path,
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
    backup_path = backup_path
  )
}

# Replace `local_path` with `work`. Both are in the same directory, so both
# renames are atomic. If the second fails we put the original back rather than
# leaving the user with no database.
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
