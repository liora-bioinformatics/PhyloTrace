# app/logic/db_staging.R
#
# Staging area for imported typing results.
#
# A profile table from a peer carries allele identifiers rather than raw sequences.
# These isolates can participate in distance matrices (e.g., Hamming distance),
# but they are not added directly to the main typing database tables (mlst, sequences,
# hashes, metadata). Staging them separately preserves the seqid space, keeps
# hash_database() functioning correctly, and ensures cleanly clonable exports.
#
# Linkability model:
#   * hash: Standard portable identity; usable as-is.
#   * index: Matches the local database's `sequences.id` space. Linkability is verified
#     by checking whether (gene, integer) maps to a valid local pair.
#   * +sequences: FASTA sequences are hashed directly to establish portable identifiers.

box::use(
  DBI[
    dbDisconnect,
    dbExecute,
    dbGetQuery,
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
  app / logic / db_connect[connect],
  app / logic / db_compat[REF_SOUCHE, connect_ro],
  app / logic / profile_io[norm_locus],
  app / logic / logging[log_event],
)

`%||%` <- function(a, b) if (is.null(a)) b else a

# Minimum proportion of integer profile calls that must resolve locally to consider
# the profile linked to this database lineage.
LINK_RATE_OK <- 0.95

# Minimum share of matching alleles required before warning about potential
# hashing convention mismatches (e.g., strand differences or trimming).
SHARED_RATE_WARN <- 0.5

STAGING_TABLES <- c(
  "imported_sets",
  "imported_profiles",
  "imported_sequences",
  "imported_metadata"
)

#' Ensure Staging Tables
#'
#' Creates staging tables (`imported_sets`, `imported_profiles`,
#' `imported_sequences`, and `imported_metadata`) if they do not exist.
#'
#' @param con Active DBI database connection.
#' @export
ensure_staging_tables <- function(con) {
  have <- dbListTables(con)

  if (!"imported_sets" %in% have) {
    dbExecute(
      con,
      "CREATE TABLE imported_sets (
         set_id INTEGER PRIMARY KEY,
         name TEXT NOT NULL UNIQUE,
         source_file TEXT,
         imported_at TEXT,
         value_kind TEXT,
         n_isolates INTEGER,
         n_loci INTEGER,
         shared_allele_rate REAL,
         has_sequences INTEGER DEFAULT 0)"
    )
  }
  if (!"imported_profiles" %in% have) {
    dbExecute(
      con,
      "CREATE TABLE imported_profiles (
         set_id INTEGER NOT NULL,
         isolate TEXT NOT NULL,
         gene TEXT NOT NULL,
         hash TEXT NOT NULL,
         PRIMARY KEY (set_id, isolate, gene))"
    )
  }
  if (!"imported_sequences" %in% have) {
    dbExecute(
      con,
      "CREATE TABLE imported_sequences (
         hash TEXT PRIMARY KEY,
         sequence TEXT NOT NULL)"
    )
  }
  if (!"imported_metadata" %in% have) {
    dbExecute(
      con,
      "CREATE TABLE imported_metadata (
         set_id INTEGER NOT NULL,
         isolate TEXT NOT NULL,
         field TEXT NOT NULL,
         value TEXT,
         PRIMARY KEY (set_id, isolate, field))"
    )
  }
  invisible(TRUE)
}

# ---------------------------------------------------------------------------
# Local Allele Map
# ---------------------------------------------------------------------------

#' Retrieve Local Allele Map
#'
#' Fetches all known alleles in the target database as a data frame of `(gene, hash, seqid)`.
#'
#' @param db_path Path to SQLite database file.
#' @return Data frame mapping gene names to sequence IDs and hashes.
#' @export
local_allele_map <- function(db_path) {
  con <- connect_ro(db_path)
  on.exit(dbDisconnect(con), add = TRUE)

  if (!"hashes" %in% dbListTables(con)) {
    return(data.frame(
      gene = character(0),
      hash = character(0),
      seqid = integer(0),
      stringsAsFactors = FALSE
    ))
  }

  map <- dbGetQuery(
    con,
    "SELECT DISTINCT m.gene AS gene, h.hash AS hash, m.seqid AS seqid
       FROM mlst m JOIN hashes h ON h.id = m.seqid"
  )
  map$gene <- norm_locus(map$gene)
  map
}

# ---------------------------------------------------------------------------
# Profile Resolution Logic
# ---------------------------------------------------------------------------

#' Resolve Parsed Profile
#'
#' Maps a parsed profile to portable allele identities `(isolate, gene, hash)`.
#' Validates input using supplied sequence data, hash checks, or local index lookups.
#'
#' @param db_path Path to SQLite database file.
#' @param parsed Parsed profile structure from `parse_profile_file()`.
#' @param sequences Optional data frame containing `(gene, allele, sequence)` from FASTA input.
#' @return List with resolved long profile, linkability flag, check results, and summary metrics.
#' @export
resolve_profile <- function(db_path, parsed, sequences = NULL) {
  map <- local_allele_map(db_path)
  long <- parsed$long
  kind <- parsed$value_kind

  checks <- .row(
    "Loci",
    if (length(parsed$loci_seen)) "pass" else "fail",
    sprintf(
      "%d of this scheme's loci present%s",
      length(parsed$loci_seen),
      if (length(parsed$loci_unknown)) {
        sprintf("; %d unknown column(s) ignored", length(parsed$loci_unknown))
      } else {
        ""
      }
    )
  )

  if (kind %in% c("mixed", "empty")) {
    return(.unlinkable(
      checks,
      "Allele values",
      "fail",
      "Could not tell what the cells are: they are neither all integers nor all hashes."
    ))
  }

  # --- Hashes derived from sequences ----------------------------------------
  if (!is.null(sequences) && nrow(sequences)) {
    seqmap <- sequences
    seqmap$gene <- norm_locus(seqmap$gene)
    seqmap$hash <- as.character(sha256(seqmap$sequence))

    key <- paste(long$gene, long$value)
    seqkey <- paste(seqmap$gene, seqmap$allele)
    idx <- match(key, seqkey)

    resolved <- !is.na(idx)
    rate <- mean(resolved)

    if (rate < LINK_RATE_OK) {
      return(.unlinkable(
        checks,
        "Sequences",
        "fail",
        sprintf(
          "The sequence file resolves only %.0f%% of the allele calls. Is it the matching allele set?",
          100 * rate
        )
      ))
    }

    long$hash <- seqmap$hash[idx]
    long <- long[!is.na(long$hash), , drop = FALSE]

    checks <- rbind(
      checks,
      .row(
        "Allele values",
        "pass",
        sprintf("%s, resolved from the supplied sequences", kind)
      ),
      .row(
        "Sequences",
        if (rate == 1) "pass" else "warn",
        sprintf("%.1f%% of calls resolved", 100 * rate)
      )
    )
    return(.finish(db_path, long, kind, map, checks, sequences = seqmap))
  }

  # --- Direct Hash Profiles -------------------------------------------------
  if (identical(kind, "hash")) {
    long$hash <- tolower(long$value)
    checks <- rbind(
      checks,
      .row("Allele values", "pass", "sha256 hashes — portable allele identity")
    )
    return(.finish(db_path, long, kind, map, checks))
  }

  if (identical(kind, "hash_other")) {
    return(.unlinkable(
      checks,
      "Allele values",
      "fail",
      paste(
        "These look like hashes, but not sha256 (e.g. crc32 or md5). We cannot",
        "recompute them. Re-export with sha256, or supply the allele sequences."
      )
    ))
  }

  # --- Integer Profile Resolution -------------------------------------------
  key <- paste(long$gene, long$value)
  mapkey <- paste(map$gene, map$seqid)
  idx <- match(key, mapkey)
  rate <- mean(!is.na(idx))

  if (rate < LINK_RATE_OK) {
    return(.unlinkable(
      checks,
      "Allele values",
      "fail",
      sprintf(
        paste(
          "Only %.0f%% of these integers are allele ids of this database. They are",
          "per-locus allele numbers from another tool, which say nothing about which",
          "sequence they mean. Re-export as hashes (chewBBACA: --hash-profiles sha256)",
          "or supply the allele sequences."
        ),
        100 * rate
      )
    ))
  }

  long$hash <- map$hash[idx]
  dropped <- sum(is.na(long$hash))
  long <- long[!is.na(long$hash), , drop = FALSE]

  checks <- rbind(
    checks,
    .row(
      "Allele values",
      if (dropped == 0L) "pass" else "warn",
      if (dropped == 0L) {
        "Integer allele ids of this database"
      } else {
        sprintf(
          "Integer allele ids of this database; %d call(s) reference alleles no longer stored here and are treated as missing",
          dropped
        )
      }
    )
  )
  .finish(db_path, long, kind, map, checks)
}

# Shared tail: measure how much of the set we already know, which is the signal
# that catches a peer whose hashing convention differs from ours.
.finish <- function(db_path, long, kind, map, checks, sequences = NULL) {
  known <- paste(long$gene, long$hash) %in% paste(map$gene, map$hash)
  shared <- if (length(known)) mean(known) else 0

  checks <- rbind(
    checks,
    .row(
      "Shared alleles",
      if (shared >= SHARED_RATE_WARN) "pass" else "warn",
      if (shared >= SHARED_RATE_WARN) {
        sprintf(
          "%.1f%% of the calls are alleles this database already holds",
          100 * shared
        )
      } else {
        sprintf(
          paste(
            "Only %.1f%% of the calls are alleles this database holds. If these came from",
            "another tool, it may hash sequences differently (strand or trimming), in which",
            "case the distances would be meaningless."
          ),
          100 * shared
        )
      }
    )
  )

  isolates <- unique(long$isolate)
  checks <- rbind(
    checks,
    .row(
      "Isolates",
      if (length(isolates)) "pass" else "fail",
      sprintf("%d isolate(s), %d allele call(s)", length(isolates), nrow(long))
    )
  )

  out <- list(
    long = long[, c("isolate", "gene", "hash")],
    linkable = !any(checks$status == "fail"),
    checks = checks,
    value_kind = kind,
    shared_allele_rate = shared,
    isolates = isolates,
    sequences = sequences
  )
  attr(out$checks, "blocked") <- any(checks$status == "fail")
  out
}

.unlinkable <- function(checks, check, status, detail) {
  checks <- rbind(checks, .row(check, status, detail))
  attr(checks, "blocked") <- TRUE
  list(
    long = NULL,
    linkable = FALSE,
    checks = checks,
    value_kind = NA_character_,
    shared_allele_rate = NA_real_,
    isolates = character(0),
    sequences = NULL
  )
}

.row <- function(check, status, detail) {
  data.frame(
    check = check,
    status = status,
    detail = detail,
    stringsAsFactors = FALSE
  )
}

# ---------------------------------------------------------------------------
# Database Operations: Read / Write Staged Sets
# ---------------------------------------------------------------------------

#' List Reserved Isolate Names
#'
#' Retrieves names of isolates currently present in the database or staged sets to prevent collisions.
#'
#' @param db_path Path to SQLite database file.
#' @return Character vector of used isolate names.
#' @export
taken_isolate_names <- function(db_path) {
  con <- connect_ro(db_path)
  on.exit(dbDisconnect(con), add = TRUE)

  local <- dbGetQuery(con, "SELECT DISTINCT souche FROM mlst")$souche
  staged <- if ("imported_profiles" %in% dbListTables(con)) {
    dbGetQuery(con, "SELECT DISTINCT isolate FROM imported_profiles")$isolate
  } else {
    character(0)
  }

  unique(c(setdiff(local, REF_SOUCHE), staged))
}

#' Stage Resolved Profile Set
#'
#' Writes a resolved profile, optional novel sequences, and metadata into staging tables.
#'
#' @param db_path Path to SQLite database file.
#' @param name Human-readable string identifier for the imported set.
#' @param resolved Profile resolution output structure from `resolve_profile()`.
#' @param metadata Optional metadata data frame with an `isolate` column.
#' @param renames Optional named vector mapping old isolate names to new ones.
#' @param source_file Name of the original source file.
#' @return Assigned integer `set_id`.
#' @export
stage_profile_set <- function(
  db_path,
  name,
  resolved,
  metadata = NULL,
  renames = NULL,
  source_file = NA_character_
) {
  if (!isTRUE(resolved$linkable) || is.null(resolved$long)) {
    stop("This profile cannot be linked to the database's alleles.")
  }
  name <- trimws(name)
  if (!nzchar(name)) {
    stop("Give the imported set a name.")
  }

  long <- resolved$long
  if (!is.null(renames) && length(renames)) {
    hit <- match(long$isolate, names(renames))
    long$isolate[!is.na(hit)] <- unname(renames[hit[!is.na(hit)]])
  }

  con <- connect(db_path)
  on.exit(if (dbIsValid(con)) dbDisconnect(con), add = TRUE)

  ensure_staging_tables(con)

  if (
    nrow(dbGetQuery(
      con,
      "SELECT 1 FROM imported_sets WHERE name = ?",
      params = list(name)
    ))
  ) {
    stop("An imported set named '", name, "' already exists.")
  }

  taken <- taken_isolate_names(db_path)
  clash <- intersect(unique(long$isolate), taken)
  if (length(clash)) {
    stop(
      "These isolate names are already in use: ",
      paste(utils::head(clash, 5), collapse = ", ")
    )
  }

  dbBegin(con)
  tryCatch(
    {
      dbExecute(
        con,
        "INSERT INTO imported_sets
           (name, source_file, imported_at, value_kind, n_isolates, n_loci,
            shared_allele_rate, has_sequences)
         VALUES (?, ?, ?, ?, ?, ?, ?, ?)",
        params = list(
          name,
          source_file %||% NA_character_,
          format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
          resolved$value_kind,
          length(unique(long$isolate)),
          length(unique(long$gene)),
          resolved$shared_allele_rate,
          as.integer(!is.null(resolved$sequences))
        )
      )
      set_id <- dbGetQuery(con, "SELECT last_insert_rowid() AS id")$id

      long$set_id <- set_id
      dbWriteTable(
        con,
        "imported_profiles",
        long[, c("set_id", "isolate", "gene", "hash")],
        append = TRUE
      )

      if (!is.null(resolved$sequences)) {
        novel <- dbGetQuery(
          con,
          "SELECT DISTINCT p.hash FROM imported_profiles p
            WHERE p.set_id = ?
              AND p.hash NOT IN (SELECT hash FROM hashes)
              AND p.hash NOT IN (SELECT hash FROM imported_sequences)",
          params = list(set_id)
        )$hash

        add <- resolved$sequences[
          match(novel, resolved$sequences$hash),
          c("hash", "sequence"),
          drop = FALSE
        ]
        add <- add[!is.na(add$hash), , drop = FALSE]
        if (nrow(add)) {
          dbWriteTable(con, "imported_sequences", add, append = TRUE)
        }
      }

      if (
        !is.null(metadata) && nrow(metadata) && "isolate" %in% names(metadata)
      ) {
        md <- metadata
        if (!is.null(renames) && length(renames)) {
          hit <- match(md$isolate, names(renames))
          md$isolate[!is.na(hit)] <- unname(renames[hit[!is.na(hit)]])
        }
        md <- md[md$isolate %in% unique(long$isolate), , drop = FALSE]

        fields <- setdiff(names(md), "isolate")
        if (length(fields) && nrow(md)) {
          tall <- data.frame(
            set_id = set_id,
            isolate = rep(md$isolate, times = length(fields)),
            field = rep(fields, each = nrow(md)),
            value = unlist(lapply(md[fields], as.character), use.names = FALSE),
            stringsAsFactors = FALSE
          )
          tall <- tall[!is.na(tall$value) & nzchar(tall$value), , drop = FALSE]
          if (nrow(tall)) {
            dbWriteTable(con, "imported_metadata", tall, append = TRUE)
          }
        }
      }

      dbCommit(con)
      log_event(
        "DB",
        "staged-import",
        sprintf("'%s' (set_id=%s)", name, set_id)
      )
      set_id
    },
    error = function(e) {
      try(dbRollback(con), silent = TRUE)
      stop("Could not stage the imported set: ", conditionMessage(e))
    }
  )
}

#' List Imported Sets
#'
#' Retrieves metadata for all staged sets currently stored in the database.
#'
#' @param db_path Path to SQLite database file.
#' @return Data frame listing staged profile sets ordered by import timestamp descending.
#' @export
list_imported_sets <- function(db_path) {
  empty <- data.frame(
    set_id = integer(0),
    name = character(0),
    source_file = character(0),
    imported_at = character(0),
    value_kind = character(0),
    n_isolates = integer(0),
    n_loci = integer(0),
    shared_allele_rate = numeric(0),
    has_sequences = integer(0),
    stringsAsFactors = FALSE
  )

  if (
    is.null(db_path) ||
      length(db_path) != 1 ||
      is.na(db_path) ||
      !file.exists(db_path)
  ) {
    return(empty)
  }

  con <- tryCatch(connect_ro(db_path), error = function(e) NULL)
  if (is.null(con)) {
    return(empty)
  }
  on.exit(dbDisconnect(con), add = TRUE)

  if (!"imported_sets" %in% dbListTables(con)) {
    return(empty)
  }
  dbGetQuery(con, "SELECT * FROM imported_sets ORDER BY imported_at DESC")
}

#' Delete Imported Set
#'
#' Removes a staged set and cleans up any unreferenced imported sequences.
#'
#' @param db_path Path to SQLite database file.
#' @param set_id Target integer identifier of the set to remove.
#' @export
delete_imported_set <- function(db_path, set_id) {
  con <- connect(db_path)
  on.exit(dbDisconnect(con), add = TRUE)

  if (!"imported_sets" %in% dbListTables(con)) {
    return(invisible(FALSE))
  }

  dbBegin(con)
  tryCatch(
    {
      for (tbl in c("imported_profiles", "imported_metadata")) {
        dbExecute(
          con,
          sprintf("DELETE FROM %s WHERE set_id = ?", tbl),
          params = list(set_id)
        )
      }
      dbExecute(
        con,
        "DELETE FROM imported_sets WHERE set_id = ?",
        params = list(set_id)
      )
      # Sequences are shared across sets; drop only the now-unreferenced ones.
      dbExecute(
        con,
        "DELETE FROM imported_sequences
          WHERE hash NOT IN (SELECT hash FROM imported_profiles)"
      )
      dbCommit(con)
    },
    error = function(e) {
      try(dbRollback(con), silent = TRUE)
      stop(e)
    }
  )
  log_event("DB", "delete-imported-set", sprintf("set_id=%s", set_id))
  invisible(TRUE)
}

#' Get Imported Long Profile
#'
#' Extracts allele calls in long format `(isolate, gene, hash)` for specified staged sets.
#'
#' @param db_path Path to SQLite database file.
#' @param set_ids Vector of target integer set identifiers.
#' @return Data frame of allele mappings.
#' @export
imported_profile_long <- function(db_path, set_ids) {
  empty <- data.frame(
    isolate = character(0),
    gene = character(0),
    hash = character(0),
    stringsAsFactors = FALSE
  )
  if (!length(set_ids)) {
    return(empty)
  }

  con <- connect_ro(db_path)
  on.exit(dbDisconnect(con), add = TRUE)

  if (!"imported_profiles" %in% dbListTables(con)) {
    return(empty)
  }

  dbGetQuery(
    con,
    sprintf(
      "SELECT isolate, gene, hash FROM imported_profiles WHERE set_id IN (%s)",
      paste(rep("?", length(set_ids)), collapse = ", ")
    ),
    params = as.list(as.integer(set_ids))
  )
}

#' Pivot Staged Metadata to Wide Format
#'
#' Fetches staged metadata for specified set IDs and formats it into a wide frame compatible with local metadata representations.
#'
#' @param db_path Path to SQLite database file.
#' @param set_ids Vector of target integer set identifiers.
#' @return Wide data frame containing metadata fields per isolate, or `NULL` if empty.
#' @export
imported_metadata_wide <- function(db_path, set_ids) {
  if (!length(set_ids)) {
    return(NULL)
  }

  con <- connect_ro(db_path)
  on.exit(dbDisconnect(con), add = TRUE)

  tables <- dbListTables(con)
  if (!"imported_profiles" %in% tables) {
    return(NULL)
  }

  ph <- paste(rep("?", length(set_ids)), collapse = ", ")
  params <- as.list(as.integer(set_ids))

  who <- dbGetQuery(
    con,
    sprintf(
      "SELECT DISTINCT p.isolate AS isolate, s.name AS source
         FROM imported_profiles p JOIN imported_sets s ON s.set_id = p.set_id
        WHERE p.set_id IN (%s)",
      ph
    ),
    params = params
  )
  if (!nrow(who)) {
    return(NULL)
  }

  md <- if ("imported_metadata" %in% tables) {
    dbGetQuery(
      con,
      sprintf(
        "SELECT isolate, field, value FROM imported_metadata WHERE set_id IN (%s)",
        ph
      ),
      params = params
    )
  } else {
    data.frame(
      isolate = character(0),
      field = character(0),
      value = character(0),
      stringsAsFactors = FALSE
    )
  }

  out <- who
  for (f in unique(md$field)) {
    rows <- md[md$field == f, , drop = FALSE]
    out[[f]] <- rows$value[match(out$isolate, rows$isolate)]
  }
  out
}
