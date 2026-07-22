# app/logic/custom_fields.R
#
# User-defined custom variables ("custom fields") for the isolates of a
# PhyloTrace database.
#
# The `metadata` table is a fixed, GenEpiO-aligned field set. Labs routinely
# need one more field it does not have — a ward, a sampling site, an outbreak
# id, a CT value — so this module lets the user define their own, typed fields
# and fill them in per isolate.
#
# Two tables, both living inside the loaded `.db` (prefixed `phylotrace_`, like
# the Analysis Dashboard's store), so custom variables are automatically scoped
# to their database and travel with the file:
#
#   phylotrace_custom_fields   one row per variable  (name, type, description)
#   phylotrace_custom_values   one row per filled cell (field_id, souche, value)
#
# The values are stored long rather than as one column per variable: renaming a
# variable is then a single UPDATE with no DDL, and pivoting long -> wide at
# display time is the pattern `load_classical_mlst()` and `load_amr()` already
# use. Every value is TEXT in a canonical form (ISO date, "yes"/"no", the number
# as written) — consistent with every other table in the file. Type integrity is
# enforced on write by `coerce_custom_value()`, not by column affinity.
#
# All access follows the app's standard idiom: open a private connection, close
# it on exit, and use parameterised statements.

box::use(
  DBI[
    dbBegin,
    dbCommit,
    dbConnect,
    dbDisconnect,
    dbExecute,
    dbGetQuery,
    dbListTables,
    dbRollback
  ],
  jsonlite[fromJSON, toJSON],
  RSQLite[SQLite],
)

box::use(
  app / logic / database_functions[metadata_columns],
  app / logic / field_labels[AMR_COL_PREFIX, CUSTOM_COL_PREFIX, MLST_COL_PREFIX],
)

#' The variable types a user can choose from, as `slug = "Human label"`.
#'
#' The slug is what `phylotrace_custom_fields.type` stores and what
#' `coerce_custom_value()` switches on; the label is what every selector shows.
#' @export
CUSTOM_TYPES <- c(
  text = "Text",
  integer = "Whole number",
  numeric = "Decimal number",
  date = "Date",
  boolean = "Yes / No",
  category = "Category"
)

# Types whose cells take a `<input type="number">` in an editable DT.
#' @export
NUMERIC_TYPES <- c("integer", "numeric")

# Types whose values are drawn from a fixed list (offered as a <datalist>).
#' @export
LISTED_TYPES <- c("boolean", "category")

# ISO-ish local timestamp, matching analysis_store.R.
.now <- function() format(Sys.time(), "%Y-%m-%d %H:%M:%S")

# A usable single-file path pointing at an existing database.
.usable <- function(db_path) {
  !is.null(db_path) &&
    length(db_path) == 1 &&
    !is.na(db_path) &&
    nzchar(db_path) &&
    file.exists(db_path)
}

`%||%` <- function(a, b) if (is.null(a)) b else a

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

#' The two tables' DDL, as idempotent statements.
#'
#' Exported because the importer has to create them on a connection it already
#' holds, mid-transaction, and cannot open a second one to the same file.
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
     souche TEXT NOT NULL,
     value TEXT,
     PRIMARY KEY (field_id, souche)
   )"
)

# Cheap and idempotent, so every write path calls it — a write can never fail on
# a missing table.
.ensure_tables <- function(con) {
  for (sql in CUSTOM_SCHEMA_DDL) {
    dbExecute(con, sql)
  }
  invisible(NULL)
}

.last_id <- function(con) {
  as.integer(dbGetQuery(con, "SELECT last_insert_rowid() AS id")$id[[1]])
}

#' Create the two custom-variable tables if they do not exist yet.
#'
#' Safe to call on every database load; returns TRUE when the schema is in
#' place, FALSE when `db_path` is not a usable database file.
#' @export
ensure_custom_schema <- function(db_path) {
  if (!.usable(db_path)) {
    return(invisible(FALSE))
  }

  con <- dbConnect(SQLite(), db_path)
  on.exit(dbDisconnect(con))

  .ensure_tables(con)
  invisible(TRUE)
}

#' The metadata column name a custom variable is surfaced as (`custom_<name>`).
#' @export
custom_col <- function(name) paste0(CUSTOM_COL_PREFIX, name)

#' The allowed values of a `category` variable, decoded from its stored JSON.
#' Returns `character(0)` for every other type (and for malformed JSON).
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

#' Encode a level vector for storage. NULL for anything empty, so a non-category
#' variable stores SQL NULL rather than an empty JSON array.
#' @export
encode_levels <- function(levels) {
  levels <- trimws(as.character(levels %||% character(0)))
  levels <- unique(levels[!is.na(levels) & nzchar(levels)])
  if (!length(levels)) {
    return(NULL)
  }
  as.character(toJSON(levels, auto_unbox = FALSE))
}

#' Every custom variable defined in the database, in display order.
#'
#' Returns a data frame with one row per variable — `id`, `name`, `type`,
#' `description`, `levels` (raw JSON), `position`, `created`, `modified` — plus
#' `n_filled`, the number of isolates that carry a value for it. Empty (but
#' correctly shaped) when the database is unusable or has no custom tables, so
#' callers can always index the result.
#' @export
list_custom_fields <- function(db_path) {
  if (!.usable(db_path)) {
    return(.empty_fields())
  }

  con <- dbConnect(SQLite(), db_path)
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

#' Why `name` cannot be used for a custom variable, or NULL when it can.
#'
#' Exported so the New-Variable dialog can validate as the user types. `id` is
#' the variable being renamed, whose own name is not a collision with itself.
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

#' Define a new custom variable. Returns its new id.
#'
#' Stops with a user-facing message when the name is unusable (see
#' `validate_custom_name()`), the type is unknown, or a `category` variable is
#' given no levels — callers show that message as-is.
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

  con <- dbConnect(SQLite(), db_path)
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

  .last_id(con)
}

#' Rename a variable or change its description / levels.
#'
#' The *type* is deliberately not editable: stored values are canonicalised for
#' the type they were entered under, so switching it would silently invalidate
#' them. Arguments left NULL are kept as they are.
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
    params <- c(params, list(if (nzchar(description)) {
      description
    } else {
      NA_character_
    }))
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

  con <- dbConnect(SQLite(), db_path)
  on.exit(dbDisconnect(con))

  dbExecute(
    con,
    sprintf(
      "UPDATE phylotrace_custom_fields SET %s, modified = ? WHERE id = ?",
      paste(sets, collapse = ", ")
    ),
    params = c(params, list(.now(), as.integer(id)))
  )

  invisible(TRUE)
}

#' Delete a variable and every value stored for it, in one transaction.
#' @export
delete_custom_field <- function(db_path, id) {
  if (!.usable(db_path) || !length(id)) {
    return(invisible(FALSE))
  }

  con <- dbConnect(SQLite(), db_path)
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

  invisible(TRUE)
}

# Levels as a plain character vector, from either representation: the stored
# JSON scalar, an already-decoded vector, or a single bare level.
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

# Canonical "yes"/"no" for the strings a user might plausibly type.
.BOOL_TRUE <- c("yes", "y", "true", "t", "1")
.BOOL_FALSE <- c("no", "n", "false", "f", "0")

#' Validate and canonicalise one entered value against its variable's type.
#'
#' Returns `list(ok, value, reason)`. `ok = TRUE` with an `NA_character_` value
#' means "cleared" — an empty cell is always allowed and deletes the stored row.
#' `ok = FALSE` carries a `reason` short enough to show in a notification.
#'
#' This is the single authority on what may be stored: the DT input types are
#' only a hint (a user can paste anything into a text cell), so every write path
#' goes through here.
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

#' Cast a column of canonical stored values to the R type its variable implies.
#'
#' `integer` and `numeric` both become R numeric so a DataTable sorts them as
#' numbers rather than as strings; every other type stays character (dates as
#' ISO strings, exactly like `metadata.sample_collection_date`).
.cast_values <- function(v, type) {
  if (type %in% NUMERIC_TYPES) suppressWarnings(as.numeric(v)) else v
}

#' Per-isolate custom-variable values, wide.
#'
#' Pivots `phylotrace_custom_values` to one row per isolate: an `isolate` column
#' plus one `custom_<name>` column per defined variable, typed by its definition
#' (see `.cast_values()`). Variables keep their display order, and a variable
#' with no values yet still yields an (all-empty) column so it can be picked.
#'
#' `fields` optionally restricts the result to those variable *names*. Returns
#' NULL when the database has no custom variables at all.
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

  con <- dbConnect(SQLite(), db_path)
  on.exit(dbDisconnect(con))

  values <- if ("phylotrace_custom_values" %in% dbListTables(con)) {
    dbGetQuery(
      con,
      "SELECT field_id, souche, value FROM phylotrace_custom_values"
    )
  } else {
    data.frame(
      field_id = integer(0),
      souche = character(0),
      value = character(0),
      stringsAsFactors = FALSE
    )
  }
  values <- values[values$field_id %in% defs$id, , drop = FALSE]

  isolates <- unique(values$souche)
  out <- data.frame(isolate = isolates, stringsAsFactors = FALSE)

  for (i in seq_len(nrow(defs))) {
    sub <- values[values$field_id == defs$id[[i]], , drop = FALSE]
    col <- rep(NA_character_, length(isolates))
    col[match(sub$souche, isolates)] <- sub$value
    out[[custom_col(defs$name[[i]])]] <- .cast_values(col, defs$type[[i]])
  }

  out
}

#' Merge the custom-variable columns into a metadata frame keyed on `isolate`,
#' recording the added names in the result's `"custom_cols"` attribute.
#'
#' The custom-variable analogue of `append_classical_mlst()` / `append_amr()`:
#' reusable across views, columns that would shadow an existing metadata field
#' are skipped, and `meta` is returned unchanged (empty `"custom_cols"`) when it
#' is not a non-empty isolate-keyed frame or the database has no custom
#' variables. `fields` optionally restricts the merge to those variable names.
#'
#' Unlike the derived MLST/AMR columns these are *user data*, not results — but
#' they are still merged for display only. `phylotrace_custom_values` is the one
#' place they are written, from the Custom Variables panel.
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

#' Write edited cells back.
#'
#' `edits` is a data frame of `field_id` / `souche` / `value`, where `value` is
#' already canonical (i.e. it came out of `coerce_custom_value()`). An NA or
#' empty value deletes the row rather than storing a blank, so `n_filled` and
#' the export filters stay meaningful. The whole batch is one transaction.
#' @export
save_custom_values <- function(db_path, edits) {
  if (!.usable(db_path) || !is.data.frame(edits) || !nrow(edits)) {
    return(invisible(0L))
  }

  con <- dbConnect(SQLite(), db_path)
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
            WHERE field_id = ? AND souche = ?",
          params = list(
            as.integer(edits$field_id[clear]),
            as.character(edits$souche[clear])
          )
        )
      }
      if (any(!clear)) {
        # INSERT OR REPLACE, not an UPSERT: the primary key is the whole
        # identity of a row here, so replacing it is exactly the intent.
        dbExecute(
          con,
          "INSERT OR REPLACE INTO phylotrace_custom_values
             (field_id, souche, value) VALUES (?, ?, ?)",
          params = list(
            as.integer(edits$field_id[!clear]),
            as.character(edits$souche[!clear]),
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

  invisible(nrow(edits))
}
