# app/logic/db_sources.R
#
# Where the isolates in a database came from: typed here, or brought in by a
# merge - and from which peer database.
#
# Two pieces work together:
#
#   * `metadata.source`, one standard metadata column holding a human-readable
#     label ("local", "partner-lab-2026"). Denormalised on purpose. Metadata
#     travels: it is exported, read by peers, and re-imported. A foreign key
#     into a registry table the peer does not have would not survive that trip,
#     while a plain label stays readable everywhere - including in a CSV a user
#     opens outside the app.
#
#   * `phylotrace_sources`, the local registry that hands those labels out and
#     keeps them stable across repeat imports of the same peer.
#
# Labels are collision-aware. A peer is identified by its `phylotrace_meta.uuid`
# - the identity PhyloTrace already stamps into every database it creates - and
# only by its file name when it carries no uuid. So importing the same peer
# twice, even after it was renamed on disk, reuses its one label; two *different*
# databases that happen to share a file name get "partner-lab" and
# "partner-lab (2)". Once handed out a label is never rewritten: metadata rows
# already carry it, and rewriting would mean touching every one of them.

box::use(
  DBI[dbExecute, dbGetQuery, dbListTables],
)

# The metadata column carrying the label. Reserved (see METADATA_RESERVED in
# db_import.R): a peer's own `source` values describe *its* history, so they are
# never mapped onto ours - the merge stamps its own label instead.
#' @export
SOURCE_COL <- "source"

# Label for isolates this database typed itself. Reserved against external
# labels too, so a peer database that happens to be called "local.db" cannot
# claim it.
#' @export
SOURCE_LOCAL <- "local"

SOURCES_TABLE <- "phylotrace_sources"

SOURCES_DDL <- sprintf(
  "CREATE TABLE IF NOT EXISTS %s (
     source_key TEXT PRIMARY KEY,
     label TEXT NOT NULL UNIQUE,
     db_uuid TEXT,
     file_name TEXT,
     first_imported TEXT,
     last_imported TEXT
   )",
  SOURCES_TABLE
)

#' Create the registry table if this database has never recorded a source.
#' @export
ensure_sources_table <- function(con) {
  dbExecute(con, SOURCES_DDL)
  invisible(NULL)
}

#' The `phylotrace_meta.uuid` of an attached (or the main) database, or NA when
#' the database predates that table / was not written by PhyloTrace.
#' @export
db_uuid <- function(con, schema = "main") {
  tbls <- tryCatch(
    dbGetQuery(
      con,
      sprintf("SELECT name FROM %s.sqlite_master WHERE type = 'table'", schema)
    )$name,
    error = function(e) character(0)
  )
  if (!"phylotrace_meta" %in% tbls) {
    return(NA_character_)
  }
  val <- tryCatch(
    dbGetQuery(
      con,
      sprintf("SELECT value FROM %s.phylotrace_meta WHERE key = 'uuid'", schema)
    )$value,
    error = function(e) character(0)
  )
  if (!length(val) || is.na(val[[1]]) || !nzchar(val[[1]])) {
    return(NA_character_)
  }
  as.character(val[[1]])
}

# What makes two imports "the same peer". The uuid when there is one (survives a
# rename on disk); the file name otherwise, lowercased so a case-only difference
# does not mint a second label for one file.
.source_key <- function(uuid, file_name) {
  if (length(uuid) && !is.na(uuid) && nzchar(uuid)) {
    return(paste0("uuid:", uuid))
  }
  paste0("file:", tolower(.base_name(file_name)))
}

.base_name <- function(file_name) {
  if (!length(file_name) || is.na(file_name[[1]])) {
    return("")
  }
  sub("\\.db$", "", basename(as.character(file_name[[1]])), ignore.case = TRUE)
}

#' The label a database file would get, given the labels already handed out.
#'
#' Collisions are resolved with a numeric suffix rather than by mangling the
#' name, so the common case reads as the plain file name. Comparison is
#' case-insensitive: "Partner" and "partner" would be one label to a reader.
#' `SOURCE_LOCAL` always counts as taken.
#' @export
unique_source_label <- function(file_name, taken = character(0)) {
  base <- trimws(gsub("\\s+", " ", .base_name(file_name)))
  if (!nzchar(base)) {
    base <- "external database"
  }
  taken <- tolower(c(taken, SOURCE_LOCAL))

  candidate <- base
  i <- 1L
  while (tolower(candidate) %in% taken) {
    i <- i + 1L
    candidate <- sprintf("%s (%d)", base, i)
  }
  candidate
}

# Every label a new one must not collide with: the registry, plus whatever
# `metadata.source` already carries. The second half matters when the registry
# and the metadata disagree - a database restored from an export, say, where the
# rows kept their labels but the registry did not travel with them. Without it a
# second, unrelated peer could be handed a label rows already use.
.labels_in_use <- function(con) {
  labels <- dbGetQuery(con, sprintf("SELECT label FROM %s", SOURCES_TABLE))$label

  tbls <- tryCatch(dbListTables(con), error = function(e) character(0))
  if ("metadata" %in% tbls) {
    in_rows <- tryCatch(
      dbGetQuery(
        con,
        sprintf(
          "SELECT DISTINCT %s AS label FROM metadata WHERE %s IS NOT NULL",
          SOURCE_COL,
          SOURCE_COL
        )
      )$label,
      # No `source` column yet: nothing in use.
      error = function(e) character(0)
    )
    labels <- c(labels, in_rows)
  }
  unique(labels[!is.na(labels) & nzchar(labels)])
}

#' Label for one incoming peer database, registering it on first sight.
#'
#' Idempotent per peer: the second import of the same database returns the label
#' the first one created and only refreshes `last_imported`.
#'
#' @param con Connection to the database being written into.
#' @param uuid The peer's `phylotrace_meta.uuid`, or NA.
#' @param file_name The peer's file name *as the user chose it* - not the staged
#'   temp copy a merge may be reading from, whose name means nothing to anyone.
#' @return The label to write into `metadata.source`.
#' @export
register_source <- function(con, uuid = NA_character_, file_name = NA_character_) {
  ensure_sources_table(con)

  key <- .source_key(uuid, file_name)
  now <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")

  hit <- dbGetQuery(
    con,
    sprintf("SELECT label FROM %s WHERE source_key = ?", SOURCES_TABLE),
    params = list(key)
  )
  if (nrow(hit)) {
    dbExecute(
      con,
      sprintf("UPDATE %s SET last_imported = ?, file_name = ? WHERE source_key = ?", SOURCES_TABLE),
      params = list(now, .base_name(file_name), key)
    )
    return(hit$label[[1]])
  }

  label <- unique_source_label(file_name, .labels_in_use(con))
  dbExecute(
    con,
    sprintf(
      "INSERT INTO %s (source_key, label, db_uuid, file_name, first_imported, last_imported)
         VALUES (?, ?, ?, ?, ?, ?)",
      SOURCES_TABLE
    ),
    params = list(
      key,
      label,
      if (length(uuid) && !is.na(uuid) && nzchar(uuid)) uuid else NA_character_,
      .base_name(file_name),
      now,
      now
    )
  )
  label
}

#' Every source this database has recorded, newest import first. Empty frame
#' when nothing was ever imported.
#' @export
list_sources <- function(con) {
  if (!SOURCES_TABLE %in% dbListTables(con)) {
    return(data.frame(
      label = character(0),
      db_uuid = character(0),
      file_name = character(0),
      first_imported = character(0),
      last_imported = character(0),
      stringsAsFactors = FALSE
    ))
  }
  dbGetQuery(
    con,
    sprintf(
      "SELECT label, db_uuid, file_name, first_imported, last_imported
         FROM %s ORDER BY last_imported DESC",
      SOURCES_TABLE
    )
  )
}
