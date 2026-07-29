# app/logic/db_sources.R
#
# Tracks provenance for isolate origins: locally typed versus peer database imports.
#
# Provenance tracking operates via two complementary mechanisms:
#
#   * `metadata.source`: A standard metadata column storing a human-readable
#     provenance string (e.g., "local", "partner-lab-2026"). Intentional
#     denormalization ensures portability during data export/import workflows across
#     external tools and CSV exports without relying on database-specific
#     foreign key relationships.
#
#   * `phylotrace_sources`: A local registry table that issues standardized labels
#     and maintains key stability across repeated imports from the same peer.
#
# Source labeling uses collision-aware logic based on unique peer identifiers:
# - Primary tracking utilizes `phylotrace_meta.uuid` assigned during database creation.
# - Fallback tracking uses normalized filenames if UUID metadata is absent.
#
# Consequently, repeated imports from the same peer (even across file renames)
# retain the existing source label. Distinct databases with identical filenames
# receive disambiguated labels (e.g., "partner-lab", "partner-lab (2)"). Assigned
# labels remain static to maintain historical consistency across existing rows.

box::use(
  DBI[dbExecute, dbGetQuery, dbListListTables],
)

# Metadata field reserved for tracking provenance source labels.
#' @export
SOURCE_COL <- "source"

# Reserved identifier for isolates generated directly within the local database.
#' @export
SOURCE_LOCAL <- "local"

# Local source registry table identifier.
SOURCES_TABLE <- "phylotrace_sources"

# Schema definition for the local source registry table.
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

#' Ensure Sources Registry Table
#'
#' Initializes the `phylotrace_sources` table in the database if it does not yet exist.
#'
#' @param con Active DBI database connection.
#' @export
ensure_sources_table <- function(con) {
  dbExecute(con, SOURCES_DDL)
  invisible(NULL)
}

#' Get Database UUID
#'
#' Retrieves the unique identifier (`phylotrace_meta.uuid`) from the specified database schema.
#'
#' @param con Active DBI database connection.
#' @param schema Character name of target attached schema. Defaults to `"main"`.
#' @return Character string containing the database UUID, or `NA_character_` if absent.
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

# Constructs a stable registry key based on UUID or normalized lower-case filename.
.source_key <- function(uuid, file_name) {
  if (length(uuid) && !is.na(uuid) && nzchar(uuid)) {
    return(paste0("uuid:", uuid))
  }
  paste0("file:", tolower(.base_name(file_name)))
}

# Extracts stripped base file name omitting extension.
.base_name <- function(file_name) {
  if (!length(file_name) || is.na(file_name[[1]])) {
    return("")
  }
  sub("\\.db$", "", basename(as.character(file_name[[1]])), ignore.case = TRUE)
}

#' Generate Unique Source Label
#'
#' Constructs a collision-free source label by appending numeric suffixes when encountering duplicates.
#'
#' @param file_name Character string indicating file source name.
#' @param taken Character vector of labels currently in use.
#' @return Character string containing unique, disambiguated source label.
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

# Collects all active source labels from both registry and existing metadata entries.
.labels_in_use <- function(con) {
  labels <- dbGetQuery(
    con,
    sprintf("SELECT label FROM %s", SOURCES_TABLE)
  )$label

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
      error = function(e) character(0)
    )
    labels <- c(labels, in_rows)
  }
  unique(labels[!is.na(labels) & nzchar(labels)])
}

#' Register Database Source
#'
#' Registers an incoming external database peer or retrieves its existing source label.
#'
#' @param con Active DBI database connection.
#' @param uuid Optional UUID string of peer database.
#' @param file_name Original filename of peer database.
#' @return Character string containing the unique assigned source label.
#' @export
register_source <- function(
  con,
  uuid = NA_character_,
  file_name = NA_character_
) {
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
      sprintf(
        "UPDATE %s SET last_imported = ?, file_name = ? WHERE source_key = ?",
        SOURCES_TABLE
      ),
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

#' List Registered Sources
#'
#' Returns all source database records present in the local database registry.
#'
#' @param con Active DBI database connection.
#' @return Data frame listing registered source labels and import timestamps ordered by `last_imported` descending.
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
