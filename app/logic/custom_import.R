# app/logic/custom_import.R
#
# Bulk import of an isolate-keyed table (CSV / TSV / Excel) into the isolate
# metadata of a PhyloTrace database: reading the file, assessing it against the
# database (isolate overlap, name clashes, which types each column can take),
# and applying the user's plan in a single transaction. New columns become
# custom variables; a column may instead fill or overwrite an existing custom
# variable or an editable standard metadata field.

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
  openxlsx[getSheetNames, read.xlsx],
  rlang[`%||%`],
  stats[setNames],
  tools[file_ext],
  utils[head, read.table],
)

box::use(
  app / logic / custom_fields,
  app / logic / database_functions[metadata_columns],
  app / logic / db_connect[connect],
  app / logic / db_sources[SOURCE_COL],
  app / logic / field_labels[
    AMR_COL_PREFIX,
    CUSTOM_COL_PREFIX,
    field_labels_for,
    MLST_COL_PREFIX
  ],
  app / logic / field_types[FIXED_FIELD_TYPES],
  app / logic / logging[log_event],
  app / logic / pymlst[existing_strains],
)

#' File Extensions Accepted for Import
#' @export
IMPORT_EXTENSIONS <- c("csv", "tsv", "tab", "txt", "xlsx", "xlsm")

#' Standard Metadata Columns the User May Never Edit
#'
#' `isolate` is the isolate identity (shared with mlst.souche), `organism` is
#' fixed by mlst_type.species for the whole database, `called_at` is the
#' app-stamped time the isolate was typed, and `source` is provenance the app
#' writes when the isolate arrives. Located by name, not position — an imported
#' peer database can carry extra metadata columns in any order.
#' @export
READONLY_METADATA_COLS <- c("isolate", "organism", "called_at", SOURCE_COL)

#' Cell Contents Read as a Missing Value
#'
#' Compared case-insensitively after trimming. A missing cell is never stored
#' and never clears a value already in the database.
#' @export
MISSING_TOKENS <- c("", "na", "n/a", "nan", "null", "#n/a", "-", ".")

#' Most Distinct Values a Column May Have to Be Offered as a Category
#' @export
CATEGORY_MAX_LEVELS <- 100L

# Order in which types are offered, most specific first.
.TYPE_ORDER <- c("boolean", "integer", "numeric", "date", "category", "text")

# Word forms that make a column read as yes/no rather than as 0/1 numbers.
.BOOL_WORDS <- c("yes", "no", "y", "n", "true", "false", "t", "f")

# Strict YYYY-MM-DD: as.Date() alone accepts trailing junk ("2024-01-01x").
.DATE_RE <- "^[0-9]{4}-[0-9]{1,2}-[0-9]{1,2}$"

# Plain decimal notation: as.numeric() also takes "Inf" and hex ("0x1A").
.NUMBER_RE <- "^[+-]?([0-9]+[.]?[0-9]*|[.][0-9]+)([eE][+-]?[0-9]+)?$"

# A leading zero ("007", postal code "01067") is part of an identifier, which a
# number type would silently strip.
.LEADING_ZERO_RE <- "^[+-]?0[0-9]"

# Text form of one spreadsheet column: dates as YYYY-MM-DD, numbers written out
# in full (never "1e+05", which no numeric type would accept back).
.cell_text <- function(x) {
  if (inherits(x, "Date")) {
    return(ifelse(is.na(x), NA_character_, format(x, "%Y-%m-%d")))
  }
  if (inherits(x, "POSIXt")) {
    return(ifelse(is.na(x), NA_character_, format(x, "%Y-%m-%d %H:%M:%S")))
  }
  if (is.numeric(x)) {
    return(vapply(
      x,
      function(v) {
        if (is.na(v)) NA_character_ else format(v, scientific = FALSE, trim = TRUE, digits = 15)
      },
      character(1)
    ))
  }
  as.character(x)
}

# Picks the delimiter that splits the header line into the most fields.
.sniff_sep <- function(path) {
  first <- readLines(path, n = 1L, warn = FALSE, encoding = "UTF-8")
  if (!length(first)) {
    stop("The file is empty.")
  }
  counts <- vapply(
    c("\t", ";", ","),
    function(s) lengths(regmatches(first, gregexpr(s, first, fixed = TRUE))),
    integer(1)
  )
  if (all(counts == 0L)) "\t" else names(counts)[which.max(counts)]
}

# Blank or duplicated headers get a positional stand-in so every column can be
# addressed by name.
.fix_headers <- function(headers) {
  headers <- trimws(gsub("^﻿", "", headers %||% character(0)))
  blank <- is.na(headers) | !nzchar(headers)
  headers[blank] <- paste0("column_", which(blank))
  make.unique(headers, sep = "_")
}

#' Read a Tabular File for Import
#'
#' Reads CSV / TSV (delimiter sniffed from the header line: tab, `;` or `,`) or
#' Excel workbooks into an all-character data frame. Excel dates come back as
#' YYYY-MM-DD and numbers in full, so they type-check like typed-in text.
#'
#' @param path Path to the file.
#' @param sheet Optional sheet name for workbooks; the first sheet by default.
#' @return List with `data` (character data frame, headers made unique),
#'   `sheets` (sheet names, `character(0)` for delimited files), `sheet` (the
#'   sheet read, or `NA`) and `renamed` (original headers that were blank or
#'   duplicated, named by their stand-in).
#' @export
read_import_table <- function(path, sheet = NULL) {
  if (!file.exists(path)) {
    stop("The file no longer exists.")
  }
  ext <- tolower(file_ext(path))
  if (!ext %in% IMPORT_EXTENSIONS) {
    stop(sprintf(
      "Unsupported file type '.%s'. Use one of: %s.",
      ext,
      paste0(".", IMPORT_EXTENSIONS, collapse = ", ")
    ))
  }

  sheets <- character(0)
  if (ext %in% c("xlsx", "xlsm")) {
    sheets <- getSheetNames(path)
    sheet <- sheet %||% sheets[[1]]
    if (!sheet %in% sheets) {
      stop(sprintf("Sheet '%s' not found in the workbook.", sheet))
    }
    df <- read.xlsx(
      path,
      sheet = sheet,
      colNames = TRUE,
      check.names = FALSE,
      sep.names = " ",
      detectDates = TRUE,
      skipEmptyRows = TRUE,
      skipEmptyCols = TRUE,
      na.strings = character(0)
    )
    if (is.null(df)) {
      df <- data.frame()
    }
    df[] <- lapply(df, .cell_text)
  } else {
    df <- read.table(
      path,
      sep = .sniff_sep(path),
      header = TRUE,
      quote = "\"",
      comment.char = "",
      check.names = FALSE,
      colClasses = "character",
      na.strings = character(0),
      fileEncoding = "UTF-8-BOM",
      blank.lines.skip = TRUE,
      strip.white = TRUE
    )
    sheet <- NA_character_
  }

  original <- names(df)
  names(df) <- .fix_headers(original)
  changed <- names(df) != trimws(gsub("^﻿", "", original))
  renamed <- setNames(original[changed], names(df)[changed])

  if (!ncol(df)) {
    stop("No columns could be read from the file.")
  }
  df <- as.data.frame(df, stringsAsFactors = FALSE)
  rownames(df) <- NULL

  list(data = df, sheets = sheets, sheet = sheet, renamed = renamed)
}

#' Detect Missing Cells
#'
#' @param x Character vector.
#' @return Logical vector, `TRUE` where the cell is blank or a missing token.
#' @export
is_missing_value <- function(x) {
  is.na(x) | tolower(trimws(x)) %in% MISSING_TOKENS
}

#' Choose the Isolate Key Column
#'
#' A column headed "isolate" (any case) wins. Otherwise, given the table and the
#' database's isolates, the column holding the most of them (left-most on a
#' tie); failing that, the left-most column.
#'
#' @param headers Column names of the uploaded table.
#' @param df Optional uploaded table, to count matches per column.
#' @param db_isolates Optional isolate names of the database.
#' @return The key column's name, with attribute `source`: `"header"`,
#'   `"matches"` or `"first"` - how it was chosen.
#' @export
key_column <- function(headers, df = NULL, db_isolates = NULL) {
  pick <- function(key, source) structure(key, source = source)
  hit <- headers[tolower(trimws(headers)) == "isolate"]
  if (length(hit)) {
    return(pick(hit[[1]], "header"))
  }
  if (!is.null(df) && length(db_isolates)) {
    n <- vapply(headers, function(h) sum(trimws(df[[h]]) %in% db_isolates), integer(1))
    if (max(n) > 0L) {
      return(pick(headers[[which.max(n)]], "matches"))
    }
  }
  pick(headers[[1]], "first")
}

#' Turn a Header into a Valid Variable Name Suggestion
#'
#' Replaces anything outside `[A-Za-z0-9_-]` with `_`, makes sure the name
#' starts with a letter and does not start with a reserved derived-column prefix.
#'
#' @param header Character vector of column headers.
#' @return Character vector of candidate names (not checked for clashes).
#' @export
suggest_name <- function(header) {
  x <- gsub("[^A-Za-z0-9_-]+", "_", trimws(header))
  x <- gsub("_+", "_", x)
  x <- gsub("^[_-]+|[_-]+$", "", x)
  x[!nzchar(x)] <- "variable"
  x <- ifelse(grepl("^[A-Za-z]", x), x, paste0("v_", x))
  reserved <- startsWith(tolower(x), MLST_COL_PREFIX) |
    startsWith(tolower(x), AMR_COL_PREFIX) |
    startsWith(tolower(x), CUSTOM_COL_PREFIX)
  x[reserved] <- paste0("imported_", x[reserved])
  x
}

#' Prepare Uploaded Rows for Matching
#'
#' Trims every cell, turns missing tokens into `NA`, drops rows without a key,
#' collapses rows repeated verbatim, and sets aside isolates listed more than
#' once with differing values — they are excluded rather than guessed at.
#'
#' @param df Character data frame as returned by `read_import_table()`.
#' @param key Name of the key column.
#' @return List with `rows` (one row per distinct isolate, key column renamed to
#'   `isolate`), `n_rows`, `n_blank_keys`, `blank_preview` (what the first few
#'   key-less rows hold, e.g. a footnote), `n_repeated` (verbatim repeats
#'   collapsed) and `conflicting` (isolates excluded for contradicting rows).
#' @export
prepare_rows <- function(df, key) {
  n_rows <- nrow(df)
  df[] <- lapply(df, function(col) {
    col <- trimws(as.character(col))
    col[is_missing_value(col)] <- NA_character_
    col
  })

  ids <- df[[key]]
  blank <- is.na(ids)
  blank_preview <- vapply(
    which(blank),
    function(i) {
      cells <- unlist(df[i, ], use.names = FALSE)
      paste(head(cells[!is.na(cells)], 2L), collapse = " | ")
    },
    character(1)
  )
  blank_preview <- head(blank_preview[nzchar(blank_preview)], 3L)
  df <- df[!blank, , drop = FALSE]

  values <- df[setdiff(names(df), key)]
  values <- data.frame(isolate = df[[key]], values, check.names = FALSE, stringsAsFactors = FALSE)

  repeated <- duplicated(values)
  values <- values[!repeated, , drop = FALSE]
  conflicting <- unique(values$isolate[duplicated(values$isolate)])
  values <- values[!values$isolate %in% conflicting, , drop = FALSE]
  rownames(values) <- NULL

  list(
    rows = values,
    n_rows = n_rows,
    n_blank_keys = sum(blank),
    blank_preview = blank_preview,
    n_repeated = sum(repeated),
    conflicting = sort(conflicting)
  )
}

# Distinct values with case-only variants folded into the first spelling seen,
# the way a category's levels have to be (it matches values case-insensitively).
.levels_of <- function(v) {
  v <- v[!is.na(v)]
  v[!duplicated(tolower(v))]
}

#' Types a Column's Values Can Be Stored As
#'
#' A type is feasible only if *every* non-missing value converts to it without
#' loss, using the same `coerce_custom_value()` the editing grids use. Text is
#' always feasible; a category needs at most `CATEGORY_MAX_LEVELS` distinct values.
#'
#' @param v Character vector of non-missing values (may be empty).
#' @return Character vector of feasible type slugs, most specific first.
#' @export
feasible_types <- function(v) {
  v <- v[!is.na(v)]
  fits <- function(type, levels = NULL) {
    all(vapply(
      v,
      function(x) {
        # as.integer() warns past the 32-bit range; the NA it returns is the answer.
        res <- suppressWarnings(custom_fields$coerce_custom_value(x, type, levels))
        isTRUE(res$ok) && !is.na(res$value)
      },
      logical(1)
    ))
  }

  out <- "text"
  if (!length(v)) {
    return(.TYPE_ORDER)
  }
  if (length(.levels_of(v)) <= CATEGORY_MAX_LEVELS) {
    out <- c(out, "category")
  }
  if (all(grepl(.DATE_RE, v)) && fits("date")) {
    out <- c(out, "date")
  }
  if (all(grepl(.NUMBER_RE, v)) && !any(grepl(.LEADING_ZERO_RE, v))) {
    if (fits("numeric")) {
      out <- c(out, "numeric")
    }
    if (fits("integer")) {
      out <- c(out, "integer")
    }
  }
  if (fits("boolean")) {
    out <- c(out, "boolean")
  }
  .TYPE_ORDER[.TYPE_ORDER %in% out]
}

#' Suggest the Most Fitting Type for a Column
#'
#' Yes/no words beat numbers (a 0/1 column stays a whole number), numbers beat
#' dates, and a column of few distinct repeating values reads as a category.
#'
#' @param v Character vector of non-missing values.
#' @param feasible Output of `feasible_types(v)`.
#' @return A single type slug.
#' @export
guess_type <- function(v, feasible = feasible_types(v)) {
  v <- v[!is.na(v)]
  if (!length(v)) {
    return("text")
  }
  if ("boolean" %in% feasible && any(tolower(v) %in% .BOOL_WORDS)) {
    return("boolean")
  }
  for (type in c("integer", "numeric", "date")) {
    if (type %in% feasible) {
      return(type)
    }
  }
  n_levels <- length(.levels_of(v))
  if ("category" %in% feasible && n_levels <= 12L && n_levels <= length(v) / 2) {
    return("category")
  }
  "text"
}

# Suffixes repeats ("ward", "Ward" -> "ward", "Ward_2"): names are compared
# case-insensitively everywhere in the store.
.unique_names <- function(x) {
  low <- tolower(x)
  for (i in which(duplicated(low))) {
    k <- 2L
    while (tolower(paste0(x[[i]], "_", k)) %in% low) {
      k <- k + 1L
    }
    x[[i]] <- paste0(x[[i]], "_", k)
    low[[i]] <- tolower(x[[i]])
  }
  x
}

# Where a variable name already lives in the database: a custom variable, an
# editable standard metadata field, a locked one, or nowhere. Standard fields
# are found only by their exact column name. A name that merely reads like one
# ("Country" for geo_loc_name_country) is "label": it is neither merged into
# that field nor accepted as a new name, since it would look identical to it.
.name_target <- function(name, custom_names, meta_cols) {
  low <- tolower(name)
  hit <- custom_names[tolower(custom_names) == low]
  if (length(hit)) {
    return(list(kind = "custom", name = hit[[1]]))
  }
  hit <- meta_cols[tolower(meta_cols) == low]
  if (length(hit)) {
    kind <- if (hit[[1]] %in% READONLY_METADATA_COLS) "readonly" else "metadata"
    return(list(kind = kind, name = hit[[1]]))
  }
  hit <- meta_cols[
    custom_fields$field_key(field_labels_for(meta_cols)) == custom_fields$field_key(name)
  ]
  if (length(hit)) {
    return(list(kind = "label", name = hit[[1]]))
  }
  list(kind = "none", name = NA_character_)
}

#' Type and Levels of an Existing Merge Target
#'
#' @param kind `"custom"` or `"metadata"`.
#' @param name Target variable / column name.
#' @param defs Output of `custom_fields$list_custom_fields()`.
#' @return List with `type` and `levels` (JSON, categories only).
#' @export
target_type <- function(kind, name, defs) {
  if (identical(kind, "custom")) {
    row <- defs[defs$name == name, , drop = FALSE]
    return(list(type = row$type[[1]], levels = row$levels[[1]]))
  }
  type <- unname(FIXED_FIELD_TYPES[name])
  list(type = if (is.na(type) || identical(type, "datetime")) "text" else type, levels = NA)
}

# Count of values that would fail the target's type, or NA when the column can
# not be merged into it at all.
.merge_failures <- function(v, kind, name, defs) {
  if (!kind %in% c("custom", "metadata")) {
    return(NA_integer_)
  }
  tt <- target_type(kind, name, defs)
  sum(!vapply(
    v,
    function(x) {
      res <- custom_fields$coerce_custom_value(x, tt$type, tt$levels)
      isTRUE(res$ok) && !is.na(res$value)
    },
    logical(1)
  ))
}

#' Assess an Uploaded Table Against the Database
#'
#' Matches the upload's isolates against the database's, and profiles every
#' non-key column over the matched rows only (only those can be imported):
#' how many values it carries, which types they all fit, a suggested type and
#' name, and whether that name already exists in the database.
#'
#' @param db_path Character path to the SQLite database file.
#' @param df Character data frame from `read_import_table()`.
#' @param key Optional key column name; `key_column()` by default.
#' @return List: `key`, `key_source` (`"user"` when `key` was given, otherwise
#'   see `key_column()`), `prepared` (see `prepare_rows()`), `db_isolates`,
#'   `matched`, `unmatched`, `case_only` (unmatched isolates that differ from a
#'   database isolate only in letter case), `n_db_untouched`, and `columns`, a
#'   data frame with one row per column (`header`, `n_values`, `n_missing`,
#'   `n_distinct`, `feasible` (comma-joined), `guess`, `suggested`, `clash`,
#'   `clash_target`, `merge_failures`, `sample`).
#' @export
assess_import <- function(db_path, df, key = NULL) {
  db_isolates <- existing_strains(db_path)
  key_source <- "user"
  if (is.null(key)) {
    key <- key_column(names(df), df, db_isolates)
    key_source <- attr(key, "source")
    key <- as.character(key)
  }
  prepared <- prepare_rows(df, key)
  rows <- prepared$rows

  matched <- intersect(rows$isolate, db_isolates)
  unmatched <- setdiff(rows$isolate, db_isolates)
  case_only <- unmatched[tolower(unmatched) %in% tolower(db_isolates)]
  rows <- rows[rows$isolate %in% matched, , drop = FALSE]

  defs <- custom_fields$list_custom_fields(db_path)
  meta_cols <- metadata_columns(db_path)

  headers <- setdiff(names(rows), "isolate")
  suggested <- .unique_names(suggest_name(headers))

  columns <- do.call(rbind, lapply(seq_along(headers), function(i) {
    v <- rows[[headers[[i]]]]
    present <- v[!is.na(v)]
    feasible <- feasible_types(present)
    target <- .name_target(suggested[[i]], defs$name, meta_cols)
    data.frame(
      header = headers[[i]],
      n_values = length(present),
      n_missing = sum(is.na(v)),
      n_distinct = length(.levels_of(present)),
      feasible = paste(feasible, collapse = ","),
      guess = guess_type(present, feasible),
      suggested = suggested[[i]],
      clash = target$kind,
      clash_target = target$name,
      merge_failures = .merge_failures(present, target$kind, target$name, defs),
      sample = paste(head(unique(present), 3L), collapse = ", "),
      stringsAsFactors = FALSE
    )
  }))
  if (is.null(columns)) {
    columns <- data.frame(
      header = character(0),
      n_values = integer(0),
      n_missing = integer(0),
      n_distinct = integer(0),
      feasible = character(0),
      guess = character(0),
      suggested = character(0),
      clash = character(0),
      clash_target = character(0),
      merge_failures = integer(0),
      sample = character(0),
      stringsAsFactors = FALSE
    )
  }

  list(
    key = key,
    key_source = key_source,
    prepared = prepared,
    rows = rows,
    db_isolates = db_isolates,
    matched = matched,
    unmatched = unmatched,
    case_only = case_only,
    n_db_untouched = length(setdiff(db_isolates, matched)),
    columns = columns
  )
}

#' Validate an Import Plan
#'
#' Checks every planned column against the current database. New variables must
#' have a valid, unique name and a type every value fits; merges must target an
#' existing custom variable or editable metadata field whose type every value
#' fits. Nothing here writes.
#'
#' @param db_path Character path to the SQLite database file.
#' @param rows Matched rows (`assess_import()$rows`).
#' @param plan Data frame, one row per column to import: `header`, `action`
#'   (`"new"`, `"fill"` or `"overwrite"`), `name` (new variable name, or the
#'   merge target) and `type` (new variables only).
#' @return Character vector of problems, named by header; empty if the plan is
#'   sound.
#' @export
validate_plan <- function(db_path, rows, plan) {
  problems <- character(0)
  add <- function(header, msg) {
    problems[[header]] <<- msg
  }
  if (!nrow(plan)) {
    return(problems)
  }

  defs <- custom_fields$list_custom_fields(db_path)
  meta_cols <- metadata_columns(db_path)
  new_names <- tolower(plan$name[plan$action == "new"])
  merged <- tolower(plan$name[plan$action %in% c("fill", "overwrite")])

  for (i in seq_len(nrow(plan))) {
    p <- plan[i, , drop = FALSE]
    if (!p$header %in% names(rows)) {
      add(p$header, "Column not found in the uploaded table.")
      next
    }
    v <- rows[[p$header]]
    v <- v[!is.na(v)]

    if (identical(p$action, "new")) {
      reason <- custom_fields$validate_custom_name(db_path, p$name)
      if (!is.null(reason)) {
        add(p$header, reason)
        next
      }
      if (sum(new_names == tolower(p$name)) > 1L) {
        add(p$header, "Two imported columns would get the same name.")
        next
      }
      if (!isTRUE(p$type %in% feasible_types(v))) {
        add(p$header, "Not every value fits that type.")
        next
      }
      if (identical(p$type, "category") && !length(v)) {
        add(p$header, "A category needs at least one value to take its levels from.")
      }
    } else if (p$action %in% c("fill", "overwrite")) {
      target <- .name_target(p$name, defs$name, meta_cols)
      if (!target$kind %in% c("custom", "metadata")) {
        add(p$header, "Only custom variables and editable metadata fields can be merged into.")
        next
      }
      if (sum(merged == tolower(p$name)) > 1L) {
        add(p$header, sprintf("Another column also writes into \u201c%s\u201d; skip one.", p$name))
        next
      }
      failures <- .merge_failures(v, target$kind, target$name, defs)
      if (failures > 0L) {
        add(p$header, sprintf(
          "%d value(s) do not fit the existing %s type.",
          failures,
          tolower(custom_fields$CUSTOM_TYPES[[target_type(target$kind, target$name, defs)$type]])
        ))
      }
    } else {
      add(p$header, "Unknown action.")
    }
  }
  problems
}

# Canonical stored form of each value (NA for missing), aborting on any value
# that does not convert - the plan was validated, so this is a last guard.
.canonical <- function(v, type, levels) {
  out <- rep(NA_character_, length(v))
  for (k in which(!is.na(v))) {
    res <- custom_fields$coerce_custom_value(v[[k]], type, levels)
    if (!isTRUE(res$ok) || is.na(res$value)) {
      stop(sprintf("Value '%s' does not fit type '%s'.", v[[k]], type))
    }
    out[[k]] <- res$value
  }
  out
}

#' Preview What a Merge Would Change
#'
#' Compares the upload's values for one merge target with what the database
#' holds now, per matched isolate with a value.
#'
#' @param db_path Character path to the SQLite database file.
#' @param rows Matched rows.
#' @param header Upload column.
#' @param kind `"custom"` or `"metadata"`.
#' @param name Target name.
#' @return List of counts: `fill` (database empty), `same` (identical value) and
#'   `differ` (database holds a different value).
#' @export
merge_preview <- function(db_path, rows, header, kind, name) {
  defs <- custom_fields$list_custom_fields(db_path)
  tt <- target_type(kind, name, defs)
  new <- .canonical(rows[[header]], tt$type, tt$levels)
  keep <- !is.na(new)
  current <- .current_values(db_path, kind, name, defs, rows$isolate[keep])
  empty <- is.na(current) | !nzchar(current)
  list(
    fill = sum(empty),
    same = sum(!empty & current == new[keep]),
    differ = sum(!empty & current != new[keep])
  )
}

# The values the database holds now for `isolates` in one target.
.current_values <- function(db_path, kind, name, defs, isolates, con = NULL) {
  if (is.null(con)) {
    con <- connect(db_path)
    on.exit(dbDisconnect(con))
  }
  if (identical(kind, "custom")) {
    id <- defs$id[defs$name == name][[1]]
    stored <- dbGetQuery(
      con,
      "SELECT isolate, value FROM phylotrace_custom_values WHERE field_id = ?",
      params = list(as.integer(id))
    )
  } else {
    stored <- dbGetQuery(con, sprintf('SELECT isolate, "%s" AS value FROM metadata', name))
  }
  as.character(stored$value[match(isolates, stored$isolate)])
}

#' Apply an Import Plan
#'
#' Validates `plan` against the current database, then writes it in one
#' transaction: creates each new custom variable with its values, and fills or
#' overwrites merge targets. Missing upload cells are never written, so an
#' import can add or replace values but never clear one. Any failure rolls the
#' whole import back.
#'
#' @param db_path Character path to the SQLite database file.
#' @param rows Matched rows (`assess_import()$rows`).
#' @param plan See `validate_plan()`.
#' @return List of counts: `created` (variables), `values` (cells written to new
#'   variables), `filled` and `overwritten` (cells of existing targets), and
#'   `metadata` (`TRUE` if the metadata table was written).
#' @export
apply_import <- function(db_path, rows, plan) {
  if (!nrow(plan)) {
    stop("Nothing selected to import.")
  }
  problems <- validate_plan(db_path, rows, plan)
  if (length(problems)) {
    stop(paste0(names(problems), ": ", problems, collapse = "\n"))
  }

  isolates <- intersect(rows$isolate, existing_strains(db_path))
  rows <- rows[rows$isolate %in% isolates, , drop = FALSE]
  defs <- custom_fields$list_custom_fields(db_path)
  meta_cols <- metadata_columns(db_path)

  con <- connect(db_path)
  on.exit(dbDisconnect(con))

  counts <- list(created = 0L, values = 0L, filled = 0L, overwritten = 0L, metadata = FALSE)
  stamp <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")

  dbBegin(con)
  tryCatch(
    {
      for (sql in custom_fields$CUSTOM_SCHEMA_DDL) {
        dbExecute(con, sql)
      }
      meta_isolates <- if ("metadata" %in% dbListTables(con)) {
        dbGetQuery(con, "SELECT isolate FROM metadata")$isolate
      } else {
        character(0)
      }

      for (i in seq_len(nrow(plan))) {
        p <- plan[i, , drop = FALSE]
        v <- rows[[p$header]]

        if (identical(p$action, "new")) {
          levels_json <- if (identical(p$type, "category")) {
            custom_fields$encode_levels(.levels_of(v))
          } else {
            NA_character_
          }
          stored <- .canonical(v, p$type, levels_json)
          position <- dbGetQuery(
            con,
            "SELECT COALESCE(MAX(COALESCE(position, id)), 0) + 1 AS pos
               FROM phylotrace_custom_fields"
          )$pos[[1]]
          dbExecute(
            con,
            "INSERT INTO phylotrace_custom_fields
               (name, type, description, levels, position, created, modified)
             VALUES (?, ?, ?, ?, ?, ?, ?)",
            params = list(
              p$name,
              p$type,
              sprintf("Imported from column '%s'.", p$header),
              levels_json,
              as.integer(position),
              stamp,
              stamp
            )
          )
          id <- as.integer(dbGetQuery(con, "SELECT last_insert_rowid() AS id")$id[[1]])
          keep <- !is.na(stored)
          if (any(keep)) {
            dbExecute(
              con,
              "INSERT INTO phylotrace_custom_values (field_id, isolate, value) VALUES (?, ?, ?)",
              params = list(rep(id, sum(keep)), rows$isolate[keep], stored[keep])
            )
          }
          counts$created <- counts$created + 1L
          counts$values <- counts$values + sum(keep)
          next
        }

        target <- .name_target(p$name, defs$name, meta_cols)
        tt <- target_type(target$kind, target$name, defs)
        stored <- .canonical(v, tt$type, tt$levels)
        keep <- !is.na(stored)
        if (identical(target$kind, "metadata")) {
          keep <- keep & rows$isolate %in% meta_isolates
        }
        if (!any(keep)) {
          next
        }
        ids <- rows$isolate[keep]
        new <- stored[keep]
        current <- .current_values(db_path, target$kind, target$name, defs, ids, con = con)
        empty <- is.na(current) | !nzchar(current)
        write <- if (identical(p$action, "fill")) empty else (empty | current != new)

        if (any(write)) {
          if (identical(target$kind, "custom")) {
            id <- defs$id[defs$name == target$name][[1]]
            dbExecute(
              con,
              "INSERT OR REPLACE INTO phylotrace_custom_values
                 (field_id, isolate, value) VALUES (?, ?, ?)",
              params = list(rep(as.integer(id), sum(write)), ids[write], new[write])
            )
            dbExecute(
              con,
              "UPDATE phylotrace_custom_fields SET modified = ? WHERE id = ?",
              params = list(stamp, as.integer(id))
            )
          } else {
            dbExecute(
              con,
              sprintf('UPDATE metadata SET "%s" = ? WHERE isolate = ?', target$name),
              params = list(new[write], ids[write])
            )
            counts$metadata <- TRUE
          }
        }
        counts$filled <- counts$filled + sum(write & empty)
        counts$overwritten <- counts$overwritten + sum(write & !empty)
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
    "custom-import",
    sprintf(
      "%d variable(s) created with %d value(s); %d cell(s) filled, %d overwritten",
      counts$created,
      counts$values,
      counts$filled,
      counts$overwritten
    )
  )

  counts
}
