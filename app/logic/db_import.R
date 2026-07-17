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
)

`%||%` <- function(a, b) if (is.null(a)) b else a

.noop_progress <- function(frac, msg) invisible(NULL)

.quote_ident <- function(x) paste0('"', gsub('"', '""', x), '"')

# SQLite's SUM()/MAX() return NA over an empty set.
.as_count <- function(x) {
  if (!length(x) || is.na(x[[1]])) 0L else as.integer(x[[1]])
}

# Metadata columns the user may never map: `isolate` is the join key and
# `organism` is dictated by the local mlst_type.species.
#' @export
METADATA_RESERVED <- c("isolate", "organism")

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
  SELECT souche, group_concat(gh, char(10) ORDER BY gh) AS profile FROM (
    SELECT DISTINCT m.souche AS souche, m.gene || char(9) || h.hash AS gh
      FROM %1$s.mlst m JOIN %1$s.hashes h ON h.id = m.seqid
     WHERE m.souche <> ?
  ) GROUP BY souche"

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
  stats::setNames(as.character(sha256(df$profile)), df$souche)
}

#' A content hash of each isolate's full allele profile, keyed by souche.
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
    ext_souche = classification$isolate,
    action = ifelse(classification$status == "new", "add", "skip"),
    final_souche = classification$isolate,
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
  need <- c("ext_souche", "action", "final_souche")
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

  if (REF_SOUCHE %in% resolutions$ext_souche) {
    stop(
      "The scheme reference '",
      REF_SOUCHE,
      "' is not an importable isolate."
    )
  }

  unknown <- setdiff(resolutions$ext_souche, ext_isolates)
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

  if (anyDuplicated(accepted$final_souche)) {
    dup <- unique(accepted$final_souche[duplicated(accepted$final_souche)])
    stop("Duplicate target name(s): ", paste(dup, collapse = ", "))
  }

  if (any(!nzchar(accepted$final_souche))) {
    stop("Every imported isolate needs a non-empty name.")
  }

  # `add`/`rename` must land on a free name; `overwrite` must land on a taken one.
  fresh <- accepted$action %in% c("add", "rename")
  clash <- accepted$final_souche[fresh] %in% local_isolates
  if (any(clash)) {
    stop(
      "Target name(s) already in the database: ",
      paste(
        utils::head(accepted$final_souche[fresh][clash], 5),
        collapse = ", "
      )
    )
  }

  ow <- accepted$action == "overwrite"
  missing <- !accepted$final_souche[ow] %in% local_isolates
  if (any(missing)) {
    stop(
      "Cannot overwrite isolate(s) that do not exist locally: ",
      paste(utils::head(accepted$final_souche[ow][missing], 5), collapse = ", ")
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
      data.frame(souche = accepted$ext_souche, stringsAsFactors = FALSE),
      temporary = TRUE,
      overwrite = TRUE
    )
    counts <- dbGetQuery(
      con,
      "WITH used AS (
         SELECT DISTINCT em.gene AS gene, eh.hash AS hash
           FROM ext.mlst em
           JOIN ext.hashes eh ON eh.id = em.seqid
           JOIN acc a ON a.souche = em.souche
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
    )
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

  for (col in setdiff(selected, dbListFields(con, "metadata"))) {
    dbExecute(
      con,
      sprintf("ALTER TABLE metadata ADD COLUMN %s TEXT", .quote_ident(col))
    )
  }

  rows <- data.frame(
    isolate = accepted$final_souche,
    organism = organism %||% NA_character_,
    stringsAsFactors = FALSE
  )

  if (length(selected)) {
    idx <- match(accepted$ext_souche, ext_meta$isolate)
    for (col in selected) {
      rows[[col]] <- as.character(ext_meta[[col]][idx])
    }
  }

  dbWriteTable(con, "metadata", rows, append = TRUE)
  invisible(nrow(rows))
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
#' `resolutions` is a data.frame of `ext_souche` / `action` / `final_souche`,
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
    "import_souches",
    accepted[, c("ext_souche", "final_souche", "action")],
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
         JOIN import_souches s ON s.ext_souche = em.souche"
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

  n_new_alleles <- 0L
  n_calls <- 0L

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
            SELECT final_souche FROM import_souches WHERE action = 'overwrite'
          )"
      )
      if ("metadata" %in% dbListTables(con)) {
        dbExecute(
          con,
          "DELETE FROM main.metadata
            WHERE isolate IN (
              SELECT final_souche FROM import_souches WHERE action = 'overwrite'
            )"
        )
      }

      progress(0.7, "Writing allele calls …")
      # `mlst.id` is a rowid alias, so omitting it lets SQLite assign fresh ids
      # that cannot collide with the local ones. `ref` never appears in
      # import_souches, so the scheme reference is never imported as an isolate.
      n_calls <- dbExecute(
        con,
        "INSERT INTO main.mlst (souche, gene, seqid)
           SELECT s.final_souche, em.gene, sm.local_seqid
             FROM ext.mlst em
             JOIN import_souches s ON s.ext_souche = em.souche
             JOIN seqid_map sm ON sm.ext_seqid = em.seqid"
      )

      # Every incoming call must have found a seqid mapping. A shortfall means
      # the external `mlst` references a seqid its own `sequences` table does
      # not hold, and the inner joins above would have dropped those calls
      # silently — importing an isolate with holes in its profile.
      expected <- dbGetQuery(
        con,
        "SELECT COUNT(*) AS n FROM ext.mlst em
           JOIN import_souches s ON s.ext_souche = em.souche"
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

  list(
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
