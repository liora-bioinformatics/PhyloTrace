# app/logic/analysis_store.R
#
# Persistence for the Analysis Dashboard.
#
# A dashboard "Analysis" is a named container of saved "Plots"; each Plot holds
# the full input snapshot needed to reproduce a visualization plus a base64 PNG
# thumbnail shown in the dashboard box. Everything lives inside the loaded
# PhyloTrace `.db` itself (tables prefixed `phylotrace_`), so Analyses are
# automatically scoped to their database and travel with the file when it is
# moved or shared.
#
# All access follows the app's standard idiom: open a private connection, close
# it on exit, and use parameterised statements. Callers pass the `db_path` that
# the rest of the app threads down from the landing page.

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

# ISO-ish local timestamp used for created/modified columns.
.now <- function() format(Sys.time(), "%Y-%m-%d %H:%M:%S")

# A usable single-file path pointing at an existing database.
.usable <- function(db_path) {
  !is.null(db_path) &&
    length(db_path) == 1 &&
    !is.na(db_path) &&
    nzchar(db_path) &&
    file.exists(db_path)
}

.empty_analyses <- function() {
  data.frame(
    id = integer(0),
    name = character(0),
    created = character(0),
    modified = character(0),
    stringsAsFactors = FALSE
  )
}

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

# 32-char hex identity string, stamped once per database.
.new_uuid <- function() {
  paste(as.character(rand_bytes(16)), collapse = "")
}

# CREATE TABLE IF NOT EXISTS for the three tables. Cheap and idempotent, so
# every write path calls it — a write can never fail on a missing table.
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
  # Migrations: add columns introduced after the table first shipped (SQLite
  # appends them; NULL = unset / no static selection).
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

.last_id <- function(con) {
  as.integer(dbGetQuery(con, "SELECT last_insert_rowid() AS id")$id[[1]])
}

#' Create the schema (if absent) and stamp identity/version rows.
#'
#' Safe to call on every database load. Returns the database's stable UUID, or
#' NULL when `db_path` is not a usable database file.
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

#' All Analyses in the database, oldest first. Empty data.frame when the
#' database is unusable or the schema does not exist yet.
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

#' Whether two isolate selections describe a different set.
#'
#' NULL means "all isolates" (no restriction), so it is only equal to another
#' NULL. Order is irrelevant. Shared by the dashboard (retrospective-change
#' confirmation, drift hints) and the Visualization module (reopen warning).
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

#' The isolate names currently in the database, sorted.
#'
#' Read-only on purpose: `make_metadata_table()` also yields the isolate list
#' but creates/backfills the `metadata` table as a side effect, which is far too
#' heavy for the drift checks the dashboard runs on every render. Returns
#' `character(0)` when the database is unusable or has no `mlst` table.
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

#' A single Analysis row, or NULL when not found.
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

#' Set (or clear) an Analysis's static isolate selection. `selection_json` is a
#' JSON array of isolate names to fix for every plot in the Analysis, or NULL to
#' clear it (plots then use their own per-plot selection).
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

#' Insert a new Analysis; returns its integer id. `description` and
#' `isolate_selection` (a JSON array, or NULL for "all isolates") are optional
#' and captured during the dashboard's setup wizard. `isolate_universe` is the
#' concrete isolate list the database held when the Analysis was configured —
#' recorded even when nothing is restricted, so later isolate additions to the
#' database can be detected (see list_isolates()).
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

#' Update an Analysis's full settings (name, description, static isolate
#' selection, recorded isolate universe) from the dashboard's setup wizard.
#' NULL clears the field.
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

#' Delete an Analysis and every Plot it contains.
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

#' Plots in an Analysis, oldest first. Excludes the (potentially large)
#' `inputs_json`; use `get_plot()` to fetch a single plot's snapshot.
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

#' A single Plot row including `inputs_json`, or NULL when not found.
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

#' Insert a new Plot (when `plot_id` is NULL) or overwrite an existing one.
#' Returns the plot's integer id.
#'
#' On overwrite, `created` is preserved and only `modified` advances.
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

# Name for a copy of `base`, avoiding anything already in `existing`.
# "MST plot" -> "MST plot (copy)" -> "MST plot (copy 2)" -> ...
# An existing copy suffix is stripped first, so duplicating "MST plot (copy)"
# yields "MST plot (copy 2)" rather than "MST plot (copy) (copy)".
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

#' Duplicate a Plot within its Analysis, carrying over its full settings
#' snapshot and thumbnail under a non-clashing "(copy)" name. Returns the new
#' plot's id, or NULL when the source doesn't exist.
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

#' Update just a Plot's display name (used by the box inline rename).
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
