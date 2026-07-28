# app/logic/db_staging.R
#
# Staging area for imported typing results.
#
# A profile table from a peer carries allele *identifiers*, not sequences. Those
# isolates can therefore contribute to a distance matrix — Hamming compares
# identity, not sequence — but they can never become first-class members of the
# typing database, which needs the real DNA behind every allele call.
#
# So they live in their own `imported_*` tables and are read only by the
# distance path (Tree / MST). `mlst`, `sequences`, `hashes` and `metadata` are
# never touched: the `seqid` space stays intact, `hash_database()` keeps working,
# the `.db` export stays a clean clone, and a `.db` merge is unaffected.
#
# THE LINKABILITY PROBLEM
#
# An identifier is only useful if we can anchor it to a real allele:
#
#   hash        already the portable identity. Usable as-is.
#   index       the integer is this database's `sequences.id` — a GLOBAL id, not
#               a per-locus allele number. So we can simply ASK the database
#               whether every (gene, integer) is a real (gene, seqid) pair here.
#               Our own exports answer yes. A foreign per-locus profile answers
#               no almost immediately (locus PA5568 has alleles {3868, 2}; a
#               SeqSphere "3" is not one of them).
#   +sequences  any identifier becomes linkable, because we hash the supplied
#               DNA ourselves and get the portable identity back.
#
# We never invent an identity we cannot justify: an unlinkable profile is
# refused, not silently turned into a meaningless distance.

box::use(
  RSQLite[SQLite],
  DBI[
    dbConnect,
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
  app / logic / db_compat[REF_SOUCHE, connect_ro],
  app / logic / profile_io[norm_locus],
  app / logic / logging[log_event],
)

`%||%` <- function(a, b) if (is.null(a)) b else a

# An integer profile whose calls resolve at or above this rate came from this
# database's lineage; the shortfall is alleles pruned from `sequences` since the
# export (remove_isolates() drops unreferenced ones). Below it, the integers are
# from another tool's numbering space and mean nothing here.
LINK_RATE_OK <- 0.95

# Below this share of already-known alleles, a hash profile almost certainly came
# from a tool that hashes sequences differently than we do (different strand or
# trimming), and the distances would be nonsense.
SHARED_RATE_WARN <- 0.5

STAGING_TABLES <- c(
  "imported_sets",
  "imported_profiles",
  "imported_sequences",
  "imported_metadata"
)

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
    # Long form: a peer's metadata columns are arbitrary, and this avoids
    # ALTER TABLE churn on every import.
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
# The local allele universe
# ---------------------------------------------------------------------------

#' Every allele this database knows: `(gene, hash, seqid)`. `seqid` is the code
#' the distance path already uses, so an imported allele that matches one of
#' these can slot straight into the existing profile matrix.
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
# Resolving a parsed profile to allele identities
# ---------------------------------------------------------------------------

#' Turn a parsed profile (from `parse_profile_file()`) into `(isolate, gene,
#' hash)` — the portable identity — or explain why it cannot be done.
#'
#' `sequences` is an optional data.frame of `(gene, allele, sequence)` from an
#' accompanying FASTA; it is what makes a foreign profile linkable.
#'
#' Returns a list with `long` (NULL when unlinkable), `linkable`, `checks`
#' (a pass/warn/fail data.frame in the same shape the Import panel already
#' renders), and the rates behind them.
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

  # --- hash from the supplied sequences ------------------------------------
  # Any identifier becomes portable once we can hash the DNA ourselves.
  if (!is.null(sequences) && nrow(sequences)) {
    seqmap <- sequences
    seqmap$gene <- norm_locus(seqmap$gene)
    seqmap$hash <- as.character(sha256(seqmap$sequence))

    key <- paste(long$gene, long$value)
    seqkey <- paste(seqmap$gene, seqmap$allele)
    # Our own FASTA is keyed by hash, so a hash profile matches on the allele id
    # directly; a foreign FASTA is keyed by that tool's allele number.
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
      .row("Allele values", "pass", sprintf("%s, resolved from the supplied sequences", kind)),
      .row(
        "Sequences",
        if (rate == 1) "pass" else "warn",
        sprintf("%.1f%% of calls resolved", 100 * rate)
      )
    )
    return(.finish(db_path, long, kind, map, checks, sequences = seqmap))
  }

  # --- hash profile, no sequences ------------------------------------------
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

  # --- integer profile, no sequences ---------------------------------------
  # Ask the database whether these integers are its own seqids.
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
        sprintf("%.1f%% of the calls are alleles this database already holds", 100 * shared)
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
# Writing / reading staged sets
# ---------------------------------------------------------------------------

#' Existing isolate names the import must not collide with: the typing
#' database's souches plus every already-staged set's isolates.
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

#' Persist a resolved profile as a named set.
#'
#' `renames` maps original isolate name -> final name (for collisions).
#' `metadata` is an optional wide data.frame with an `isolate` column.
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

  con <- dbConnect(SQLite(), db_path, busy_timeout = 5000)
  on.exit(if (dbIsValid(con)) dbDisconnect(con), add = TRUE)

  ensure_staging_tables(con)

  # Name first: re-importing the same file would otherwise fail on the isolate
  # collision, which tells the user far less about what they actually did.
  if (nrow(dbGetQuery(
    con,
    "SELECT 1 FROM imported_sets WHERE name = ?",
    params = list(name)
  ))) {
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

      # Only the alleles we do not already hold: the rest are retrievable from
      # `sequences` via the hash, so storing them again would just bloat the file.
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

      if (!is.null(metadata) && nrow(metadata) && "isolate" %in% names(metadata)) {
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

  if (is.null(db_path) || length(db_path) != 1 || is.na(db_path) || !file.exists(db_path)) {
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

#' @export
delete_imported_set <- function(db_path, set_id) {
  con <- dbConnect(SQLite(), db_path, busy_timeout = 5000)
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

#' Allele calls of the given sets, as `(isolate, gene, hash)`. This is what the
#' distance path consumes.
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

#' Staged metadata pivoted back to a wide frame, with the `isolate` column plus
#' a `source` column naming the set. Shaped so it can be row-bound onto
#' `make_metadata_table()`'s output.
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
