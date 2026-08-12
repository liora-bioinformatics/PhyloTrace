# app/logic/custom_fields.R
#
# Management and storage for user-defined custom fields and isolate values within
# a PhyloTrace database. Persists user schema metadata and long-format custom values
# inside `phylotrace_custom_fields` and `phylotrace_custom_values` SQLite tables.

box::use(
  DBI[
    dbBegin,
    dbCommit,
    dbDisconnect,
    dbExecute,
    dbGetQuery,
    dbListTables,
    dbRollback
  ],
  jsonlite[fromJSON, toJSON],
)

box::use(
  app / logic / db_connect[connect],
  app / logic / database_functions[metadata_columns],
  app /
    logic /
    field_labels[AMR_COL_PREFIX, CUSTOM_COL_PREFIX, MLST_COL_PREFIX],
  app / logic / logging[log_event],
)

#' Mapping of Allowed Custom Variable Types
#'
#' Named character vector mapping type slug keys to display labels shown in the UI.
#' @export
CUSTOM_TYPES <- c(
  text = "Text",
  integer = "Whole number",
  numeric = "Decimal number",
  date = "Date",
  boolean = "Yes / No",
  category = "Category"
)

#' Custom Field Types Requiring Numeric Inputs
#' @export
NUMERIC_TYPES <- c("integer", "numeric")

#' Custom Field Types Using Selection Lists
#' @export
LISTED_TYPES <- c("boolean", "category")

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

`%||%` <- function(a, b) if (is.null(a)) b else a

# Helper: returns empty data frame structure matching custom fields query output.
.empty_fields <- function() {
  data.frame(
    id = integer(0),
    name = character(0),
    type = character(0),
    description = character(0),
    levels = character(0),
    position = integer(0),
    created = character(0),
    modified = character(0),
    n_filled = integer(0),
    stringsAsFactors = FALSE
  )
}

#' DDL Statements for Custom Field Schema
#'
#' Character vector of idempotent SQL statements used to construct custom field tables.
#' Exposes schema DDL for direct use during complex multi-step database transactions.
#' @export
CUSTOM_SCHEMA_DDL <- c(
  "CREATE TABLE IF NOT EXISTS phylotrace_custom_fields (
     id INTEGER PRIMARY KEY AUTOINCREMENT,
     name TEXT NOT NULL UNIQUE,
     type TEXT NOT NULL,
     description TEXT,
     levels TEXT,
     position INTEGER,
     created TEXT NOT NULL,
     modified TEXT NOT NULL
   )",
  "CREATE TABLE IF NOT EXISTS phylotrace_custom_values (
     field_id INTEGER NOT NULL,
     isolate TEXT NOT NULL,
     value TEXT,
     PRIMARY KEY (field_id, isolate)
   )"
)

# Ensures schema tables exist in the target database connection.
.ensure_tables <- function(con) {
  for (sql in CUSTOM_SCHEMA_DDL) {
    dbExecute(con, sql)
  }
  invisible(NULL)
}

# Helper: fetches the integer primary key of the last inserted row.
.last_id <- function(con) {
  as.integer(dbGetQuery(con, "SELECT last_insert_rowid() AS id")$id[[1]])
}

#' Ensure Custom Field Schema Exists
#'
#' Idempotently creates custom field schema tables within the target SQLite file.
#'
#' @param db_path Character path to the SQLite database file.
#' @return Invisible logical `TRUE` if successful, or `FALSE` if database path invalid.
#' @export
ensure_custom_schema <- function(db_path) {
  if (!.usable(db_path)) {
    return(invisible(FALSE))
  }

  con <- connect(db_path)
  on.exit(dbDisconnect(con))

  .ensure_tables(con)
  invisible(TRUE)
}

#' Format Name with Custom Column Prefix
#'
#' @param name Base name of the custom variable field.
#' @return Character column name prefixed for display/query consistency.
#' @export
custom_col <- function(name) paste0(CUSTOM_COL_PREFIX, name)

#' Decode JSON Categorical Field Levels
#'
#' Unpacks stored JSON level strings into a character vector for categorical variables.
#'
#' @param levels_json JSON character scalar representing categorical options.
#' @return Character vector of distinct levels, or `character(0)` for non-categories.
#' @export
field_levels <- function(levels_json) {
  if (
    is.null(levels_json) ||
      length(levels_json) != 1 ||
      is.na(levels_json) ||
      !nzchar(levels_json)
  ) {
    return(character(0))
  }
  out <- tryCatch(
    as.character(fromJSON(levels_json)),
    error = function(e) character(0)
  )
  out[!is.na(out) & nzchar(out)]
}

#' Encode Categorical Levels for Storage
#'
#' Cleans, sorts, and serializes custom level strings into a JSON array scalar.
#'
#' @param levels Character vector of user-defined categorical values.
#' @return Formatted JSON array character scalar, or `NULL` if empty.
#' @export
encode_levels <- function(levels) {
  levels <- trimws(as.character(levels %||% character(0)))
  levels <- unique(levels[!is.na(levels) & nzchar(levels)])
  if (!length(levels)) {
    return(NULL)
  }
  levels <- levels[order(tolower(levels))]
  as.character(toJSON(levels, auto_unbox = FALSE))
}

#' List All Defined Custom Fields
#'
#' Retrieves field definition metadata and non-empty entry counts for all custom variables.
#'
#' @param db_path Character path to the SQLite database file.
#' @return Data frame listing custom variable schema definitions and `n_filled` counts.
#' @export
list_custom_fields <- function(db_path) {
  if (!.usable(db_path)) {
    return(.empty_fields())
  }

  con <- connect(db_path)
  on.exit(dbDisconnect(con))

  if (!"phylotrace_custom_fields" %in% dbListTables(con)) {
    return(.empty_fields())
  }

  fields <- dbGetQuery(
    con,
    "SELECT id, name, type, description, levels, position, created, modified
       FROM phylotrace_custom_fields
      ORDER BY COALESCE(position, id), id"
  )
  if (!nrow(fields)) {
    return(.empty_fields())
  }

  counts <- if ("phylotrace_custom_values" %in% dbListTables(con)) {
    dbGetQuery(
      con,
      "SELECT field_id, COUNT(*) AS n FROM phylotrace_custom_values
        WHERE value IS NOT NULL AND value != ''
        GROUP BY field_id"
    )
  } else {
    data.frame(field_id = integer(0), n = integer(0))
  }

  n <- counts$n[match(fields$id, counts$field_id)]
  n[is.na(n)] <- 0L
  fields$n_filled <- as.integer(n)

  fields
}

#' Validate Proposed Custom Field Name
#'
#' Checks candidate field names for syntax validity, reserved prefixes, and collisions
#' with standard metadata columns or existing custom variables.
#'
#' @param db_path Character path to the SQLite database file.
#' @param name Candidate string for the field name.
#' @param id Optional integer ID of an existing variable being renamed.
#' @return Character error message if validation fails, or `NULL` if valid.
#' @export
validate_custom_name <- function(db_path, name, id = NULL) {
  name <- trimws(as.character(name %||% ""))

  if (!nzchar(name)) {
    return("Enter a name.")
  }
  if (!grepl("^[A-Za-z][A-Za-z0-9_-]*$", name)) {
    return(paste(
      "Use letters, digits, '_' and '-' only, starting with a letter.",
      "This keeps the name safe to query and to export as a column."
    ))
  }
  for (prefix in c(MLST_COL_PREFIX, AMR_COL_PREFIX, CUSTOM_COL_PREFIX)) {
    if (startsWith(tolower(name), prefix)) {
      return(sprintf(
        "'%s' is reserved for derived columns — choose a name that does not start with it.",
        prefix
      ))
    }
  }
  if (tolower(name) %in% tolower(metadata_columns(db_path))) {
    return("A metadata field of that name already exists.")
  }

  existing <- list_custom_fields(db_path)
  clash <- existing[tolower(existing$name) == tolower(name), , drop = FALSE]
  if (!is.null(id)) {
    clash <- clash[clash$id != id, , drop = FALSE]
  }
  if (nrow(clash)) {
    return("A custom variable of that name already exists.")
  }

  NULL
}

#' Create a Custom Variable Field
#'
#' Validates and inserts a new custom variable metadata record into the database.
#'
#' @param db_path Character path to the SQLite database file.
#' @param name Unique display name string.
#' @param type Field data type key matching `CUSTOM_TYPES`.
#' @param description Optional detailed text description.
#' @param levels Optional character vector of allowed values for categorical fields.
#' @return Integer ID of the created custom field.
#' @export
create_custom_field <- function(
  db_path,
  name,
  type,
  description = NULL,
  levels = NULL
) {
  if (!.usable(db_path)) {
    stop("No database loaded.")
  }

  name <- trimws(as.character(name %||% ""))
  reason <- validate_custom_name(db_path, name)
  if (!is.null(reason)) {
    stop(reason)
  }

  if (!isTRUE(type %in% names(CUSTOM_TYPES))) {
    stop("Choose a variable type.")
  }

  levels_json <- encode_levels(levels)
  if (identical(type, "category") && is.null(levels_json)) {
    stop("A category variable needs at least one allowed value.")
  }
  if (!identical(type, "category")) {
    levels_json <- NULL
  }

  description <- trimws(as.character(description %||% ""))
  if (!nzchar(description)) {
    description <- NA_character_
  }

  con <- connect(db_path)
  on.exit(dbDisconnect(con))
  .ensure_tables(con)

  position <- dbGetQuery(
    con,
    "SELECT COALESCE(MAX(COALESCE(position, id)), 0) + 1 AS pos
       FROM phylotrace_custom_fields"
  )$pos[[1]]

  stamp <- .now()
  dbExecute(
    con,
    "INSERT INTO phylotrace_custom_fields
       (name, type, description, levels, position, created, modified)
     VALUES (?, ?, ?, ?, ?, ?, ?)",
    params = list(
      name,
      type,
      description,
      levels_json %||% NA_character_,
      as.integer(position),
      stamp,
      stamp
    )
  )

  log_event(
    "DB",
    "custom-field",
    sprintf("created '%s' (type=%s)", name, type)
  )

  .last_id(con)
}

#' Update Custom Variable Metadata
#'
#' Modifies the display name, description, or categorical options of an existing custom field.
#'
#' @param db_path Character path to the SQLite database file.
#' @param id Integer identifier of the field to modify.
#' @param name Optional new field name string.
#' @param description Optional new description string.
#' @param levels Optional character vector of updated categorical values.
#' @return Invisible logical `TRUE` if records updated, or `FALSE` if no changes specified.
#' @export
update_custom_field <- function(
  db_path,
  id,
  name = NULL,
  description = NULL,
  levels = NULL
) {
  if (!.usable(db_path)) {
    stop("No database loaded.")
  }

  fields <- list_custom_fields(db_path)
  current <- fields[fields$id == id, , drop = FALSE]
  if (!nrow(current)) {
    stop("That custom variable no longer exists.")
  }

  sets <- character(0)
  params <- list()

  if (!is.null(name)) {
    name <- trimws(as.character(name))
    reason <- validate_custom_name(db_path, name, id = id)
    if (!is.null(reason)) {
      stop(reason)
    }
    sets <- c(sets, "name = ?")
    params <- c(params, list(name))
  }

  if (!is.null(description)) {
    description <- trimws(as.character(description))
    sets <- c(sets, "description = ?")
    params <- c(
      params,
      list(
        if (nzchar(description)) {
          description
        } else {
          NA_character_
        }
      )
    )
  }

  if (!is.null(levels) && identical(current$type[[1]], "category")) {
    levels_json <- encode_levels(levels)
    if (is.null(levels_json)) {
      stop("A category variable needs at least one allowed value.")
    }
    sets <- c(sets, "levels = ?")
    params <- c(params, list(levels_json))
  }

  if (!length(sets)) {
    return(invisible(FALSE))
  }

  con <- connect(db_path)
  on.exit(dbDisconnect(con))

  dbExecute(
    con,
    sprintf(
      "UPDATE phylotrace_custom_fields SET %s, modified = ? WHERE id = ?",
      paste(sets, collapse = ", ")
    ),
    params = c(params, list(.now(), as.integer(id)))
  )

  log_event("DB", "custom-field", sprintf("updated id=%s", id))

  invisible(TRUE)
}

#' Delete Custom Variable Fields
#'
#' Deletes specified custom fields and all associated isolate value entries in a single transaction.
#'
#' @param db_path Character path to the SQLite database file.
#' @param id Integer vector of custom field identifiers to remove.
#' @return Invisible logical `TRUE` if process completed, or `FALSE` if database invalid.
#' @export
delete_custom_field <- function(db_path, id) {
  if (!.usable(db_path) || !length(id)) {
    return(invisible(FALSE))
  }

  con <- connect(db_path)
  on.exit(dbDisconnect(con))

  if (!"phylotrace_custom_fields" %in% dbListTables(con)) {
    return(invisible(FALSE))
  }

  ids <- as.integer(id)
  placeholders <- paste(rep("?", length(ids)), collapse = ", ")

  dbBegin(con)
  tryCatch(
    {
      dbExecute(
        con,
        sprintf(
          "DELETE FROM phylotrace_custom_values WHERE field_id IN (%s)",
          placeholders
        ),
        params = as.list(ids)
      )
      dbExecute(
        con,
        sprintf(
          "DELETE FROM phylotrace_custom_fields WHERE id IN (%s)",
          placeholders
        ),
        params = as.list(ids)
      )
      dbCommit(con)
    },
    error = function(e) {
      dbRollback(con)
      stop(e)
    }
  )

  log_event(
    "DB",
    "custom-field",
    sprintf("deleted id(s)=%s (+ stored values)", paste(ids, collapse = ","))
  )

  invisible(TRUE)
}

# Helper: normalizes inputs (JSON, list, or scalar) into a simple character vector of levels.
.as_levels <- function(x) {
  if (is.null(x) || !length(x)) {
    return(character(0))
  }
  if (length(x) > 1) {
    return(as.character(x))
  }
  decoded <- field_levels(x)
  if (length(decoded)) decoded else as.character(x)
}

# String match constants for standard boolean inputs.
.BOOL_TRUE <- c("yes", "y", "true", "t", "1")
.BOOL_FALSE <- c("no", "n", "false", "f", "0")

#' Coerce and Validate Custom Field Inputs
#'
#' Canonicalizes input values based on target custom variable types and level definitions.
#'
#' @param value Raw input scalar value.
#' @param type Target field variable type slug.
#' @param levels Optional JSON or character vector defining allowed levels.
#' @return Named list with components `ok` (logical), `value` (canonical string or `NA`),
#'   and `reason` (validation error message if failed).
#' @export
coerce_custom_value <- function(value, type, levels = NULL) {
  ok <- function(v) list(ok = TRUE, value = v, reason = "")
  bad <- function(r) list(ok = FALSE, value = NA_character_, reason = r)

  if (is.null(value) || length(value) != 1 || is.na(value)) {
    return(ok(NA_character_))
  }
  v <- trimws(as.character(value))
  if (!nzchar(v)) {
    return(ok(NA_character_))
  }

  switch(
    type,
    text = ok(v),
    integer = {
      if (!grepl("^[+-]?[0-9]+$", v)) {
        bad("Whole numbers only, e.g. 42.")
      } else {
        ok(as.character(as.integer(v)))
      }
    },
    numeric = {
      num <- suppressWarnings(as.numeric(v))
      if (is.na(num)) {
        bad("Numbers only, e.g. 12.5.")
      } else {
        ok(format(num, scientific = FALSE, trim = TRUE))
      }
    },
    date = {
      parsed <- suppressWarnings(as.Date(v, format = "%Y-%m-%d"))
      if (is.na(parsed)) {
        bad("Dates must be written as YYYY-MM-DD.")
      } else {
        ok(format(parsed, "%Y-%m-%d"))
      }
    },
    boolean = {
      low <- tolower(v)
      if (low %in% .BOOL_TRUE) {
        ok("yes")
      } else if (low %in% .BOOL_FALSE) {
        ok("no")
      } else {
        bad("Enter yes or no.")
      }
    },
    category = {
      allowed <- .as_levels(levels)
      hit <- allowed[tolower(allowed) == tolower(v)]
      if (!length(hit)) {
        bad(paste0(
          "Allowed values: ",
          paste(allowed, collapse = ", "),
          "."
        ))
      } else {
        ok(hit[[1]])
      }
    },
    bad("Unknown variable type.")
  )
}

# Helper: casts canonical text database values into appropriate R vector types.
.cast_values <- function(v, type) {
  if (type %in% NUMERIC_TYPES) suppressWarnings(as.numeric(v)) else v
}

#' Load Pivot Wide Custom Isolate Values
#'
#' Fetches custom variable values and pivots long storage format into a wide data frame
#' keyed by isolate.
#'
#' @param db_path Character path to the SQLite database file.
#' @param fields Optional character vector restricting loaded custom variable names.
#' @return Wide data frame with an `isolate` column and typed `custom_<name>` columns,
#'   or `NULL` if no variables exist.
#' @export
load_custom_values <- function(db_path, fields = NULL) {
  defs <- list_custom_fields(db_path)
  if (!nrow(defs)) {
    return(NULL)
  }
  if (!is.null(fields)) {
    defs <- defs[defs$name %in% fields, , drop = FALSE]
    if (!nrow(defs)) {
      return(NULL)
    }
  }

  con <- connect(db_path)
  on.exit(dbDisconnect(con))

  values <- if ("phylotrace_custom_values" %in% dbListTables(con)) {
    dbGetQuery(
      con,
      "SELECT field_id, isolate, value FROM phylotrace_custom_values"
    )
  } else {
    data.frame(
      field_id = integer(0),
      isolate = character(0),
      value = character(0),
      stringsAsFactors = FALSE
    )
  }
  values <- values[values$field_id %in% defs$id, , drop = FALSE]

  isolates <- unique(values$isolate)
  out <- data.frame(isolate = isolates, stringsAsFactors = FALSE)

  for (i in seq_len(nrow(defs))) {
    sub <- values[values$field_id == defs$id[[i]], , drop = FALSE]
    col <- rep(NA_character_, length(isolates))
    col[match(sub$isolate, isolates)] <- sub$value
    out[[custom_col(defs$name[[i]])]] <- .cast_values(col, defs$type[[i]])
  }

  out
}

#' Merge Custom Variable Columns into Metadata Frame
#'
#' Appends custom field values as columns to an existing isolate-keyed metadata frame.
#'
#' @param meta Data frame containing metadata and an `isolate` identifier column.
#' @param db_path Character path to the SQLite database file.
#' @param fields Optional character vector restricting merged field names.
#' @return Input data frame with added `custom_<name>` columns and a populated
#'   `"custom_cols"` attribute listing added column names.
#' @export
append_custom <- function(meta, db_path, fields = NULL) {
  if (
    !is.data.frame(meta) || !nrow(meta) || isFALSE("isolate" %in% names(meta))
  ) {
    return(meta)
  }

  appended <- character(0)
  custom <- load_custom_values(db_path, fields)
  if (!is.null(custom)) {
    add <- setdiff(names(custom), c("isolate", names(meta)))
    if (length(add)) {
      idx <- match(meta$isolate, custom$isolate)
      for (col in add) {
        meta[[col]] <- custom[[col]][idx]
      }
      appended <- add
    }
  }

  attr(meta, "custom_cols") <- appended
  meta
}

#' Save Custom Field Isolate Values
#'
#' Writes a batch of edited custom field values back to long database storage within a single transaction.
#' Clears rows with NA or empty string values.
#'
#' @param db_path Character path to the SQLite database file.
#' @param edits Data frame containing `field_id`, `isolate`, and canonical `value` columns.
#' @return Invisible integer count of total input edit rows processed.
#' @export
save_custom_values <- function(db_path, edits) {
  if (!.usable(db_path) || !is.data.frame(edits) || !nrow(edits)) {
    return(invisible(0L))
  }

  con <- connect(db_path)
  on.exit(dbDisconnect(con))
  .ensure_tables(con)

  value <- as.character(edits$value)
  clear <- is.na(value) | !nzchar(value)

  dbBegin(con)
  tryCatch(
    {
      if (any(clear)) {
        dbExecute(
          con,
          "DELETE FROM phylotrace_custom_values
            WHERE field_id = ? AND isolate = ?",
          params = list(
            as.integer(edits$field_id[clear]),
            as.character(edits$isolate[clear])
          )
        )
      }
      if (any(!clear)) {
        dbExecute(
          con,
          "INSERT OR REPLACE INTO phylotrace_custom_values
             (field_id, isolate, value) VALUES (?, ?, ?)",
          params = list(
            as.integer(edits$field_id[!clear]),
            as.character(edits$isolate[!clear]),
            value[!clear]
          )
        )
      }
      dbCommit(con)
    },
    error = function(e) {
      dbRollback(con)
      stop(e)
    }
  )

  log_event(
    "DB",
    "phylotrace_custom_values",
    sprintf(
      "%d value(s) set, %d cleared",
      sum(!clear),
      sum(clear)
    )
  )

  invisible(nrow(edits))
}
