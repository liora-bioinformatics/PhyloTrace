# app/logic/analysis_store.R
#
# Persistence layer for Analysis Dashboard containers and visualization plots.
# Stores saved analyses, plot input configurations, and thumbnails within the
# SQLite database (`phylotrace_` prefixed tables).

box::use(
  DBI[
    dbConnect,
    dbDisconnect,
    dbExecute,
    dbGetQuery,
    dbListTables,
    dbListFields,
  ],
  RSQLite[SQLite],
  openssl[rand_bytes],
)
box::use(
  app / logic / logging[log_event],
)

SCHEMA_VERSION <- "1"

# Helper: returns formatted local timestamp for record creation/modification columns.
.now <- function() format(Sys.time(), "%Y-%m-%d %H:%M:%S")

# Helper: validates that a database path argument is non-empty and exists.
.usable <- function(db_path) {
  !is.null(db_path) &&
    length(db_path) == 1 &&
    !is.na(db_path) &&
    nzchar(db_path) &&
    file.exists(db_path)
}

# Helper: returns empty data frame structure for analyses queries.
.empty_analyses <- function() {
  data.frame(
    id = integer(0),
    name = character(0),
    created = character(0),
    modified = character(0),
    stringsAsFactors = FALSE
  )
}

# Helper: returns empty data frame structure for plots queries.
.empty_plots <- function() {
  data.frame(
    id = integer(0),
    analysis_id = integer(0),
    name = character(0),
    plot_type = character(0),
    thumb_b64 = character(0),
    created = character(0),
    modified = character(0),
    stringsAsFactors = FALSE
  )
}

# Helper: generates a random 32-character hex UUID string.
.new_uuid <- function() {
  paste(as.character(rand_bytes(16)), collapse = "")
}

# Ensures core tables and migration columns exist in the target database connection.
.ensure_tables <- function(con) {
  dbExecute(
    con,
    "CREATE TABLE IF NOT EXISTS phylotrace_meta (
       key TEXT PRIMARY KEY,
       value TEXT
     )"
  )
  dbExecute(
    con,
    "CREATE TABLE IF NOT EXISTS phylotrace_analyses (
       id INTEGER PRIMARY KEY AUTOINCREMENT,
       name TEXT NOT NULL,
       description TEXT,
       created TEXT NOT NULL,
       modified TEXT NOT NULL,
       isolate_selection TEXT,
       isolate_universe TEXT
     )"
  )

  have_cols <- dbListFields(con, "phylotrace_analyses")
  for (col in setdiff(
    c("description", "isolate_selection", "isolate_universe"),
    have_cols
  )) {
    dbExecute(
      con,
      sprintf("ALTER TABLE phylotrace_analyses ADD COLUMN %s TEXT", col)
    )
  }

  dbExecute(
    con,
    "CREATE TABLE IF NOT EXISTS phylotrace_plots (
       id INTEGER PRIMARY KEY AUTOINCREMENT,
       analysis_id INTEGER NOT NULL,
       name TEXT NOT NULL,
       plot_type TEXT NOT NULL,
       inputs_json TEXT,
       thumb_b64 TEXT,
       created TEXT NOT NULL,
       modified TEXT NOT NULL
     )"
  )
  invisible(NULL)
}

# Helper: fetches the integer primary key of the last inserted row.
.last_id <- function(con) {
  as.integer(dbGetQuery(con, "SELECT last_insert_rowid() AS id")$id[[1]])
}

#' Ensure Database Schema and Versioning Metadata
#'
#' Initializes required `phylotrace_` database tables and populates schema
#' versioning and database UUID entries if missing.
#'
#' @param db_path Character path to the SQLite database file.
#' @return Character UUID string for the database, or `NULL` if invalid path.
#' @export
ensure_schema <- function(db_path) {
  if (!.usable(db_path)) {
    return(NULL)
  }

  con <- dbConnect(SQLite(), db_path)
  on.exit(dbDisconnect(con))

  .ensure_tables(con)

  have <- dbGetQuery(con, "SELECT key, value FROM phylotrace_meta")

  if (!"schema_version" %in% have$key) {
    dbExecute(
      con,
      "INSERT INTO phylotrace_meta (key, value) VALUES ('schema_version', ?)",
      params = list(SCHEMA_VERSION)
    )
  }

  uuid <- have$value[have$key == "uuid"]
  if (!length(uuid)) {
    uuid <- .new_uuid()
    dbExecute(
      con,
      "INSERT INTO phylotrace_meta (key, value) VALUES ('uuid', ?)",
      params = list(uuid)
    )
  }

  uuid
}

#' List All Saved Analyses
#'
#' Retrieves summary metadata for all saved analyses ordered by creation index.
#'
#' @param db_path Character path to the SQLite database file.
#' @return Data frame of available analyses, or an empty schema structure if
#'   unusable.
#' @export
list_analyses <- function(db_path) {
  if (!.usable(db_path)) {
    return(.empty_analyses())
  }

  con <- dbConnect(SQLite(), db_path)
  on.exit(dbDisconnect(con))

  if (!"phylotrace_analyses" %in% dbListTables(con)) {
    return(.empty_analyses())
  }

  dbGetQuery(
    con,
    "SELECT id, name, created, modified
       FROM phylotrace_analyses
      ORDER BY id ASC"
  )
}

#' Check for Isolate Selection Differences
#'
#' Compares two isolate selection vectors, ignoring element order.
#'
#' @param a Character vector of isolate names or `NULL`.
#' @param b Character vector of isolate names or `NULL`.
#' @return Logical `TRUE` if selections differ, `FALSE` otherwise.
#' @export
selection_differs <- function(a, b) {
  if (is.null(a) && is.null(b)) {
    return(FALSE)
  }
  if (is.null(a) || is.null(b)) {
    return(TRUE)
  }
  !setequal(a, b)
}

#' List Isolate Names from Database
#'
#' Queries distinct non-reference isolate identifiers from the MLST table.
#'
#' @param db_path Character path to the SQLite database file.
#' @return Character vector of sorted isolate names.
#' @export
list_isolates <- function(db_path) {
  if (!.usable(db_path)) {
    return(character(0))
  }

  con <- dbConnect(SQLite(), db_path)
  on.exit(dbDisconnect(con))

  if (!"mlst" %in% dbListTables(con)) {
    return(character(0))
  }

  res <- dbGetQuery(
    con,
    "SELECT DISTINCT souche FROM mlst WHERE souche != 'ref' ORDER BY souche"
  )
  as.character(res$souche)
}

#' Retrieve Analysis Record by ID
#'
#' @param db_path Character path to the SQLite database file.
#' @param id Integer analysis identifier.
#' @return Named list containing analysis properties, or `NULL` if not found.
#' @export
get_analysis <- function(db_path, id) {
  if (!.usable(db_path)) {
    return(NULL)
  }

  con <- dbConnect(SQLite(), db_path)
  on.exit(dbDisconnect(con))

  if (!"phylotrace_analyses" %in% dbListTables(con)) {
    return(NULL)
  }

  res <- dbGetQuery(
    con,
    "SELECT id, name, description, created, modified, isolate_selection,
            isolate_universe
       FROM phylotrace_analyses
      WHERE id = ?",
    params = list(as.integer(id))
  )

  if (!nrow(res)) {
    return(NULL)
  }
  as.list(res[1, ])
}

#' Update Static Isolate Selection for an Analysis
#'
#' @param db_path Character path to the SQLite database file.
#' @param id Integer analysis identifier.
#' @param selection_json Character JSON array of isolate names, or `NULL` to reset.
#' @return Invisible `NULL`.
#' @export
set_analysis_selection <- function(db_path, id, selection_json) {
  con <- dbConnect(SQLite(), db_path)
  on.exit(dbDisconnect(con))

  .ensure_tables(con)
  dbExecute(
    con,
    "UPDATE phylotrace_analyses
        SET isolate_selection = ?, modified = ?
      WHERE id = ?",
    params = list(
      if (is.null(selection_json)) NA_character_ else selection_json,
      .now(),
      as.integer(id)
    )
  )
  log_event("DB", "analysis", sprintf("id=%s selection updated", id))
  invisible(NULL)
}

#' Insert a New Analysis Container
#'
#' Creates a new analysis entry with optional static selection constraints and
#' universe tracking.
#'
#' @param db_path Character path to the SQLite database file.
#' @param name Display name for the analysis.
#' @param description Optional detailed text description.
#' @param isolate_selection Optional JSON array of isolate names.
#' @param isolate_universe Optional JSON array recording active isolates at creation time.
#' @return Integer ID of the newly created analysis.
#' @export
add_analysis <- function(
  db_path,
  name,
  description = NULL,
  isolate_selection = NULL,
  isolate_universe = NULL
) {
  con <- dbConnect(SQLite(), db_path)
  on.exit(dbDisconnect(con))

  .ensure_tables(con)
  now <- .now()
  dbExecute(
    con,
    "INSERT INTO phylotrace_analyses
       (name, description, created, modified, isolate_selection,
        isolate_universe)
       VALUES (?, ?, ?, ?, ?, ?)",
    params = list(
      name,
      if (is.null(description)) NA_character_ else description,
      now,
      now,
      if (is.null(isolate_selection)) NA_character_ else isolate_selection,
      if (is.null(isolate_universe)) NA_character_ else isolate_universe
    )
  )
  new_id <- .last_id(con)
  log_event("DB", "analysis", sprintf("created '%s' (id=%s)", name, new_id))
  new_id
}

#' Rename an Analysis
#'
#' @param db_path Character path to the SQLite database file.
#' @param id Integer analysis identifier.
#' @param name New display name.
#' @return Invisible `NULL`.
#' @export
rename_analysis <- function(db_path, id, name) {
  con <- dbConnect(SQLite(), db_path)
  on.exit(dbDisconnect(con))

  .ensure_tables(con)
  dbExecute(
    con,
    "UPDATE phylotrace_analyses SET name = ?, modified = ? WHERE id = ?",
    params = list(name, .now(), as.integer(id))
  )
  log_event("DB", "analysis", sprintf("id=%s renamed to '%s'", id, name))
  invisible(NULL)
}

#' Update Analysis Container Settings
#'
#' Replaces name, description, static selection, and recorded isolate universe.
#'
#' @param db_path Character path to the SQLite database file.
#' @param id Integer analysis identifier.
#' @param name Display name.
#' @param description Optional detailed text description.
#' @param isolate_selection Optional JSON array of isolate names.
#' @param isolate_universe Optional JSON array recording active isolates.
#' @return Invisible `NULL`.
#' @export
update_analysis_settings <- function(
  db_path,
  id,
  name,
  description = NULL,
  isolate_selection = NULL,
  isolate_universe = NULL
) {
  con <- dbConnect(SQLite(), db_path)
  on.exit(dbDisconnect(con))

  .ensure_tables(con)
  dbExecute(
    con,
    "UPDATE phylotrace_analyses
        SET name = ?, description = ?, isolate_selection = ?,
            isolate_universe = ?, modified = ?
      WHERE id = ?",
    params = list(
      name,
      if (is.null(description)) NA_character_ else description,
      if (is.null(isolate_selection)) NA_character_ else isolate_selection,
      if (is.null(isolate_universe)) NA_character_ else isolate_universe,
      .now(),
      as.integer(id)
    )
  )
  log_event("DB", "analysis", sprintf("id=%s settings updated", id))
  invisible(NULL)
}

#' Delete an Analysis and Associated Plots
#'
#' Cascades deletion to remove all child plot records associated with the target
#' analysis ID.
#'
#' @param db_path Character path to the SQLite database file.
#' @param id Integer analysis identifier.
#' @return Invisible `NULL`.
#' @export
delete_analysis <- function(db_path, id) {
  con <- dbConnect(SQLite(), db_path)
  on.exit(dbDisconnect(con))

  .ensure_tables(con)
  id <- as.integer(id)
  dbExecute(
    con,
    "DELETE FROM phylotrace_plots WHERE analysis_id = ?",
    params = list(id)
  )
  dbExecute(
    con,
    "DELETE FROM phylotrace_analyses WHERE id = ?",
    params = list(id)
  )
  log_event("DB", "analysis", sprintf("id=%s deleted (+ its plots)", id))
  invisible(NULL)
}

#' List Saved Plots for an Analysis
#'
#' Fetches plot metadata and thumbnail strings, excluding heavy input JSON bodies.
#'
#' @param db_path Character path to the SQLite database file.
#' @param analysis_id Integer analysis identifier.
#' @return Data frame of plot records.
#' @export
list_plots <- function(db_path, analysis_id) {
  if (!.usable(db_path)) {
    return(.empty_plots())
  }

  con <- dbConnect(SQLite(), db_path)
  on.exit(dbDisconnect(con))

  if (!"phylotrace_plots" %in% dbListTables(con)) {
    return(.empty_plots())
  }

  dbGetQuery(
    con,
    "SELECT id, analysis_id, name, plot_type, thumb_b64, created, modified
       FROM phylotrace_plots
      WHERE analysis_id = ?
      ORDER BY id ASC",
    params = list(as.integer(analysis_id))
  )
}

#' Retrieve Complete Plot Entry by ID
#'
#' @param db_path Character path to the SQLite database file.
#' @param id Integer plot identifier.
#' @return Named list containing complete plot properties, including `inputs_json`.
#' @export
get_plot <- function(db_path, id) {
  if (!.usable(db_path)) {
    return(NULL)
  }

  con <- dbConnect(SQLite(), db_path)
  on.exit(dbDisconnect(con))

  if (!"phylotrace_plots" %in% dbListTables(con)) {
    return(NULL)
  }

  res <- dbGetQuery(
    con,
    "SELECT id, analysis_id, name, plot_type, inputs_json, thumb_b64,
            created, modified
       FROM phylotrace_plots
      WHERE id = ?",
    params = list(as.integer(id))
  )

  if (!nrow(res)) {
    return(NULL)
  }
  as.list(res[1, ])
}

#' Create or Overwrite a Saved Plot
#'
#' Inserts a new plot or updates an existing plot entry. Preserves original creation
#' timestamp during updates.
#'
#' @param db_path Character path to the SQLite database file.
#' @param analysis_id Integer analysis identifier.
#' @param plot_id Integer plot identifier (or `NULL` to create new).
#' @param name Display name for the plot.
#' @param plot_type Character code defining visualizer type.
#' @param inputs_json JSON string of full visualization input parameters.
#' @param thumb_b64 Base64 encoded PNG preview string.
#' @return Integer plot identifier.
#' @export
upsert_plot <- function(
  db_path,
  analysis_id,
  plot_id,
  name,
  plot_type,
  inputs_json,
  thumb_b64
) {
  con <- dbConnect(SQLite(), db_path)
  on.exit(dbDisconnect(con))

  .ensure_tables(con)
  now <- .now()

  if (is.null(plot_id)) {
    dbExecute(
      con,
      "INSERT INTO phylotrace_plots
         (analysis_id, name, plot_type, inputs_json, thumb_b64,
          created, modified)
         VALUES (?, ?, ?, ?, ?, ?, ?)",
      params = list(
        as.integer(analysis_id),
        name,
        plot_type,
        inputs_json,
        thumb_b64,
        now,
        now
      )
    )
    new_id <- .last_id(con)
    log_event(
      "DB",
      "plot",
      sprintf("created '%s' (id=%s, analysis=%s)", name, new_id, analysis_id)
    )
    return(new_id)
  }

  dbExecute(
    con,
    "UPDATE phylotrace_plots
        SET name = ?, plot_type = ?, inputs_json = ?, thumb_b64 = ?,
            modified = ?
      WHERE id = ?",
    params = list(
      name,
      plot_type,
      inputs_json,
      thumb_b64,
      now,
      as.integer(plot_id)
    )
  )
  log_event("DB", "plot", sprintf("id=%s updated", plot_id))
  as.integer(plot_id)
}

# Helper: generates incremental unique copy display names to prevent naming collisions.
.copy_name <- function(base, existing) {
  root <- sub(" \\(copy(?: [0-9]+)?\\)$", "", base)
  candidate <- paste0(root, " (copy)")
  n <- 1L
  while (candidate %in% existing) {
    n <- n + 1L
    candidate <- sprintf("%s (copy %d)", root, n)
  }
  candidate
}

#' Duplicate an Existing Plot Entry
#'
#' Copies configuration parameters and thumbnail into a new plot entry with an
#' automatically appended suffix name.
#'
#' @param db_path Character path to the SQLite database file.
#' @param id Integer source plot identifier.
#' @return Integer ID of the newly created duplicate plot, or `NULL` if source missing.
#' @export
duplicate_plot <- function(db_path, id) {
  con <- dbConnect(SQLite(), db_path)
  on.exit(dbDisconnect(con))

  .ensure_tables(con)
  src <- dbGetQuery(
    con,
    "SELECT analysis_id, name, plot_type, inputs_json, thumb_b64
       FROM phylotrace_plots
      WHERE id = ?",
    params = list(as.integer(id))
  )
  if (!nrow(src)) {
    return(NULL)
  }

  analysis_id <- src$analysis_id[[1]]
  existing <- dbGetQuery(
    con,
    "SELECT name FROM phylotrace_plots WHERE analysis_id = ?",
    params = list(analysis_id)
  )$name

  now <- .now()
  dbExecute(
    con,
    "INSERT INTO phylotrace_plots
       (analysis_id, name, plot_type, inputs_json, thumb_b64,
        created, modified)
       VALUES (?, ?, ?, ?, ?, ?, ?)",
    params = list(
      analysis_id,
      .copy_name(src$name[[1]], existing),
      src$plot_type[[1]],
      src$inputs_json[[1]],
      src$thumb_b64[[1]],
      now,
      now
    )
  )
  new_id <- .last_id(con)
  log_event("DB", "plot", sprintf("duplicated id=%s -> id=%s", id, new_id))
  new_id
}

#' Rename a Plot Record
#'
#' @param db_path Character path to the SQLite database file.
#' @param id Integer plot identifier.
#' @param name New display name for the plot.
#' @return Invisible `NULL`.
#' @export
rename_plot <- function(db_path, id, name) {
  con <- dbConnect(SQLite(), db_path)
  on.exit(dbDisconnect(con))

  .ensure_tables(con)
  dbExecute(
    con,
    "UPDATE phylotrace_plots SET name = ?, modified = ? WHERE id = ?",
    params = list(name, .now(), as.integer(id))
  )
  log_event("DB", "plot", sprintf("id=%s renamed to '%s'", id, name))
  invisible(NULL)
}

#' Delete a Plot Record
#'
#' @param db_path Character path to the SQLite database file.
#' @param id Integer plot identifier.
#' @return Invisible `NULL`.
#' @export
delete_plot <- function(db_path, id) {
  con <- dbConnect(SQLite(), db_path)
  on.exit(dbDisconnect(con))

  .ensure_tables(con)
  dbExecute(
    con,
    "DELETE FROM phylotrace_plots WHERE id = ?",
    params = list(as.integer(id))
  )
  log_event("DB", "plot", sprintf("id=%s deleted", id))
  invisible(NULL)
}
