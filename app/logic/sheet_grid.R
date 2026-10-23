# app/logic/sheet_grid.R
#
# Server half of the spreadsheet layer (app/js/dt-sheet.js) shared by the
# editable isolate grids: validating and applying cell edit batches, the
# undo/redo history, the user-facing report, and the glue that wires a DT grid
# to the client-side layer.

box::use(
  htmltools[tagList, tags],
  jsonlite[toJSON],
  stringi[stri_replace_all_regex, stri_trans_nfc, stri_trim_both],
)

box::use(
  app /
    logic /
    custom_fields[coerce_custom_value, custom_col, CUSTOM_TYPES, listed_levels],
  app / logic / field_labels[field_labels_for],
)

# Undo steps kept per grid; the oldest are dropped beyond this.
HISTORY_LIMIT <- 100L

#' Longest Free-Text Cell Value
#'
#' Characters a free-text cell (metadata text, custom `text` variables) may
#' hold. Cells never wrap, so one long value widens its whole column, and every
#' value travels into plot labels and exports.
#' @export
MAX_CELL_CHARS <- 50L

# Rejected values are quoted back in notifications up to this many characters.
PREVIEW_CHARS <- 30L

# Cleans typed or pasted text before validation. Line and paragraph breaks,
# tabs and no-break spaces become one space (a quoted spreadsheet cell can
# carry a line break, and an unquoted TSV/CSV export would split the row on
# it). Other control characters and invisible format characters are dropped:
# zero-width space, word joiner, byte-order mark, soft hyphen and the
# bidirectional overrides that can make text display differently from what it
# holds. Zero-width (non-)joiners stay - some scripts and emoji need them.
# NFC normalisation makes a composed "ü" and "u" + combining diaeresis the
# same value, so they group together in filters and plots.
.clean_text <- function(x) {
  x <- stri_trans_nfc(x)
  x <- stri_replace_all_regex(
    x,
    "[\\t\\n\\r\\x{0B}\\x{0C}\\x{85}\\x{A0}\\x{2028}\\x{2029}]+",
    " "
  )
  x <- stri_replace_all_regex(
    x,
    "[\\p{Cc}\\x{AD}\\x{200B}\\x{2060}\\x{FEFF}\\x{202A}-\\x{202E}\\x{2066}-\\x{2069}]",
    ""
  )
  stri_trim_both(x)
}

# Shortens a rejected value for quoting in a notification.
.preview <- function(x) {
  long <- !is.na(x) & nchar(x) > PREVIEW_CHARS
  x[long] <- paste0(substr(x[long], 1L, PREVIEW_CHARS), "\u2026")
  x
}

# Text form of grid values, as an undo step stores them to restore later.
# Numbers are written out in full (never "1e+06") so a restored value reads
# back exactly as coerce_custom_value() would have stored it.
.as_text <- function(x) {
  if (!is.numeric(x)) {
    return(as.character(x))
  }
  vapply(
    x,
    function(v) {
      if (is.na(v)) {
        NA_character_
      } else {
        format(v, scientific = FALSE, trim = TRUE, digits = 15)
      }
    },
    character(1)
  )
}

.no_changes <- function() {
  data.frame(
    row = integer(0),
    column = character(0),
    old = character(0),
    value = character(0),
    stringsAsFactors = FALSE
  )
}

#' Apply a Batch of Cell Edits
#'
#' Cleans every value (see `.clean_text()`), caps free text at
#' `MAX_CELL_CHARS`, validates every typed cell with `coerce_custom_value()`
#' and writes the accepted values into `df`, keeping each column's R type (a
#' numeric column stays numeric, so it still sorts as one). Later cells win
#' within a batch.
#'
#' @param df Data frame the grid shows.
#' @param edits Data frame with one row per cell: `row` (in `df`), `column`
#'   (name) and `value` (text as typed or pasted). Rows naming no row or column
#'   of `df` are ignored.
#' @param types Named character vector, column -> type slug to validate with.
#'   Columns not named are free text.
#' @param levels Named character vector, column -> levels JSON (categories).
#' @param na_cols Columns whose empty value is `NA`; every other column stores
#'   `""` for an empty cell.
#' @param trusted `TRUE` skips cleaning and validation, for restoring values
#'   the grid itself held before (undo/redo) - a legacy value that would no
#'   longer pass must still come back exactly.
#' @return A list: `data` (the updated frame), `changed` (one row per cell that
#'   actually changed: `row`, `column`, `old` and `value`, both as text) and
#'   `rejected` (`column`, `value`, `type`, `reason`; `type` is `NA` for a
#'   value rejected for its length).
#' @export
apply_edits <- function(
  df,
  edits,
  types = character(0),
  levels = character(0),
  na_cols = character(0),
  trusted = FALSE
) {
  edits <- edits[
    !is.na(edits$row) & edits$row <= nrow(df) & edits$column %in% names(df),
    ,
    drop = FALSE
  ]
  edits <- edits[
    !duplicated(edits[c("row", "column")], fromLast = TRUE),
    ,
    drop = FALSE
  ]

  value <- as.character(edits$value)
  type <- unname(types[edits$column])
  ok <- rep(TRUE, nrow(edits))
  reason <- rep("", nrow(edits))
  long <- rep(FALSE, nrow(edits))
  if (!trusted) {
    value <- .clean_text(value)
    # Typed values are bounded by their type (a category by its levels), so
    # only free text is capped.
    long <- (is.na(type) | type == "text") &
      !is.na(value) &
      nchar(value) > MAX_CELL_CHARS
    ok[long] <- FALSE
    reason[long] <- sprintf(
      "At most %d characters (this has %d).",
      MAX_CELL_CHARS,
      nchar(value[long])
    )
  }
  stored <- value
  if (!trusted) {
    for (k in which(ok & !is.na(type))) {
      res <- coerce_custom_value(
        value[[k]],
        type[[k]],
        unname(levels[edits$column[[k]]])
      )
      ok[[k]] <- isTRUE(res$ok)
      stored[[k]] <- as.character(res$value)
      reason[[k]] <- res$reason
    }
  }
  blank <- is.na(stored) | !nzchar(stored)
  stored[blank] <- ifelse(edits$column[blank] %in% na_cols, NA_character_, "")

  changed <- list()
  for (col in unique(edits$column[ok])) {
    i <- which(ok & edits$column == col)
    rows <- edits$row[i]
    new <- if (is.numeric(df[[col]])) {
      suppressWarnings(as.numeric(stored[i]))
    } else {
      stored[i]
    }
    old <- df[[col]][rows]
    # An empty custom cell is NA while an editor always reads back "", so
    # compare real values: a click-in/click-out is not a change.
    diff <- !((is.na(old) & is.na(new)) |
      (!is.na(old) & !is.na(new) & old == new))
    if (!any(diff)) {
      next
    }
    df[[col]][rows[diff]] <- new[diff]
    changed[[col]] <- data.frame(
      row = rows[diff],
      column = col,
      old = .as_text(old[diff]),
      value = stored[i][diff],
      stringsAsFactors = FALSE
    )
  }

  list(
    data = df,
    changed = if (length(changed)) {
      do.call(rbind, unname(changed))
    } else {
      .no_changes()
    },
    rejected = data.frame(
      column = edits$column[!ok],
      value = value[!ok],
      type = ifelse(long, NA_character_, type)[!ok],
      reason = reason[!ok],
      stringsAsFactors = FALSE
    )
  )
}

#' Decode a Spreadsheet Edit Batch
#'
#' Turns the columnar payload app/js/dt-sheet.js sends (`isolate`, 0-based
#' `col`, `value`) into the `edits` frame `apply_edits()` takes. Rows are found
#' by isolate, not position, so a batch lands on the right rows however the
#' table is sorted or filtered.
#'
#' @param batch The Shiny input value.
#' @param df Data frame the grid shows, with an `isolate` column.
#' @return Data frame with `row`, `column` and `value`.
#' @export
decode_batch <- function(batch, df) {
  data.frame(
    row = match(as.character(unlist(batch$isolate)), df$isolate),
    column = names(df)[as.integer(unlist(batch$col)) + 1L],
    value = as.character(unlist(batch$value)),
    stringsAsFactors = FALSE
  )
}

#' Merge Edited Cells into a Dirty Buffer
#'
#' One row per cell: an entry for a cell that is already pending replaces it
#' rather than queuing a second write. A `pristine` entry - one that puts the
#' cell back to its saved value, as an undo usually does - removes the cell
#' from the buffer instead, so the grid stops counting it as unsaved.
#'
#' @param buffer Current buffer, or `NULL`.
#' @param entries New entries, or `NULL`.
#' @param keys Columns identifying a cell.
#' @param pristine Logical per entry: `TRUE` when it matches the saved value.
#' @return The merged buffer, or `NULL` when nothing is left pending.
#' @export
merge_dirty <- function(buffer, entries, keys, pristine = NULL) {
  if (is.null(entries) || !nrow(entries)) {
    return(buffer)
  }
  if (is.null(pristine)) {
    pristine <- rep(FALSE, nrow(entries))
  }
  id <- function(df) do.call(paste, c(unname(as.list(df[keys])), sep = "\r"))
  kept <- if (is.null(buffer) || !nrow(buffer)) {
    NULL
  } else {
    buffer[!id(buffer) %in% id(entries), , drop = FALSE]
  }
  out <- rbind(kept, entries[!pristine, , drop = FALSE])
  if (is.null(out) || !nrow(out)) NULL else out
}

#' Compare Edited Cells with Their Saved Values
#'
#' @param base Data frame as last read from the database (`isolate` column
#'   plus the grid's columns), or `NULL`.
#' @param isolate,column,value One entry per cell; `value` as text, the way
#'   `apply_edits()` reports it.
#' @return Logical per cell: `TRUE` when the value equals the saved one.
#' @export
matches_base <- function(base, isolate, column, value) {
  out <- rep(FALSE, length(isolate))
  if (is.null(base) || !nrow(base)) {
    return(out)
  }
  rows <- match(isolate, base$isolate)
  for (col in intersect(unique(column), names(base))) {
    k <- which(column == col & !is.na(rows))
    saved <- .as_text(base[[col]][rows[k]])
    out[k] <- (is.na(saved) & is.na(value[k])) |
      (!is.na(saved) & !is.na(value[k]) & saved == value[k])
  }
  out
}

#' Report on a Batch of Cell Edits
#'
#' The one notification a batch warrants: a single rejected value reads as one
#' sentence, a pasted block as a count with the first few offenders, plus how
#' many read-only cells were left alone.
#'
#' @param rejected `rejected` from `apply_edits()`.
#' @param skipped Number of read-only cells left unchanged.
#' @param max_shown Rejections listed individually.
#' @return Notification content, or `NULL` when everything went through.
#' @export
edit_report <- function(rejected, skipped = 0L, max_shown = 3L) {
  n <- nrow(rejected)
  if (!n && !skipped) {
    return(NULL)
  }
  skipped_note <- if (skipped) {
    sprintf(
      "%d read-only %s left unchanged.",
      skipped,
      if (skipped == 1) "cell was" else "cells were"
    )
  }
  if (!n) {
    return(skipped_note)
  }
  rejected$value <- .preview(rejected$value)
  if (n == 1) {
    single <- paste0(
      "\"",
      rejected$value,
      "\" ",
      # A missing type marks a value rejected for its length (see apply_edits()).
      if (is.na(rejected$type)) {
        "is too long. "
      } else {
        paste0("is not a valid ", tolower(CUSTOM_TYPES[[rejected$type]]), " value. ")
      },
      rejected$reason
    )
    return(if (skipped) tagList(single, tags$br(), skipped_note) else single)
  }
  shown <- rejected[seq_len(min(n, max_shown)), , drop = FALSE]
  tagList(
    tags$strong(sprintf("%d values were not accepted.", n)),
    lapply(seq_len(nrow(shown)), function(k) {
      tagList(
        tags$br(),
        sprintf(
          "%s: \"%s\" - %s",
          field_labels_for(shown$column[[k]]),
          shown$value[[k]],
          shown$reason[[k]]
        )
      )
    }),
    if (n > max_shown) {
      tagList(tags$br(), sprintf("... and %d more.", n - max_shown))
    },
    if (skipped) tagList(tags$br(), skipped_note)
  )
}

#' Create an Undo/Redo History
#'
#' Each step is a data frame of cells (`isolate`, `column`, `value`) holding
#' what to write back to undo or redo one change. Steps are keyed by isolate and
#' column name, so they still land correctly after a reload re-sorted the grid.
#'
#' @return A mutable history object.
#' @export
new_history <- function() {
  h <- new.env(parent = emptyenv())
  h$undo <- list()
  h$redo <- list()
  h
}

#' Record a Change in an Undo History
#'
#' @param h History from `new_history()`.
#' @param step Cells' values from before the change.
#' @param kind What made the change: `"edit"` (a new change, which also drops
#'   the redo steps), `"undo"` (its inverse becomes a redo step) or `"redo"`.
#' @export
record_step <- function(h, step, kind = "edit") {
  if (!nrow(step)) {
    return(invisible(h))
  }
  stack <- if (identical(kind, "undo")) "redo" else "undo"
  steps <- c(h[[stack]], list(step))
  h[[stack]] <- steps[seq.int(
    max(1L, length(steps) - HISTORY_LIMIT + 1L),
    length(steps)
  )]
  if (identical(kind, "edit")) {
    h$redo <- list()
  }
  invisible(h)
}

#' Take the Latest Undo or Redo Step
#'
#' @param h History from `new_history()`.
#' @param kind `"undo"` or `"redo"`.
#' @return The step, removed from the history, or `NULL` when there is none.
#' @export
take_step <- function(h, kind) {
  n <- length(h[[kind]])
  if (!n) {
    return(NULL)
  }
  step <- h[[kind]][[n]]
  h[[kind]] <- h[[kind]][-n]
  step
}

#' Clear an Undo History
#'
#' @param h History from `new_history()`.
#' @export
clear_history <- function(h) {
  h$undo <- list()
  h$redo <- list()
  invisible(h)
}

#' Dropdown Choices per Grid Column
#'
#' @param cols Column names of the grid, in order.
#' @param defs Custom variable definitions (`name`, `type`, `levels`).
#' @return Named list, 0-based column index -> allowed values, for every
#'   boolean/category column present.
#' @export
sheet_lists <- function(cols, defs) {
  out <- list()
  for (i in seq_len(nrow(defs))) {
    lv <- listed_levels(defs$type[[i]], defs$levels[[i]])
    idx <- match(custom_col(defs$name[[i]]), cols) - 1L
    if (length(lv) && !is.na(idx)) {
      out[[as.character(idx)]] <- lv
    }
  }
  out
}

#' JavaScript Attaching the Spreadsheet Layer
#'
#' The `initComplete` snippet handing a DT grid to `App.attachSheet()`
#' (app/js/dt-sheet.js). Expects `api` in scope and declares `sheet`.
#'
#' @param input_id Namespaced input receiving edit batches and undo/redo.
#' @param readonly 0-based column indices never written.
#' @param lists Dropdown choices from `sheet_lists()`.
#' @param active JS function telling whether the layer handles input now.
#' @return Character scalar of JavaScript.
#' @export
sheet_attach_js <- function(
  input_id,
  readonly,
  lists = list(),
  active = "function() { return true; }"
) {
  sprintf(
    "var sheet = App.attachSheet(api, {
       inputId: %s,
       readonly: %s,
       lists: %s,
       maxLength: %d,
       isActive: %s
     });",
    toJSON(input_id, auto_unbox = TRUE),
    # Never auto_unbox: one read-only column must still be an array.
    toJSON(as.integer(readonly)),
    if (length(lists)) toJSON(lists, auto_unbox = FALSE) else "{}",
    MAX_CELL_CHARS,
    active
  )
}

#' Gesture Hints
#'
#' A compact key / action list for a sidebar, so a grid's gestures are
#' discoverable without a manual.
#'
#' @param hints List of `list(keys, what)`: `keys` is a tag (or tag list) of
#'   `<kbd>`s, `what` the action text.
#' @param note Optional closing line.
#' @return A tag.
#' @export
key_hints <- function(hints, note = NULL) {
  tags$div(
    class = "sheet-hints",
    lapply(hints, function(h) {
      tagList(tags$span(class = "sheet-hint-keys", h$keys), tags$span(h$what))
    }),
    if (!is.null(note)) tags$div(class = "sheet-hints-note", note)
  )
}

#' Gesture Hints for a Spreadsheet Grid
#'
#' `key_hints()` for every gesture app/js/dt-sheet.js handles.
#'
#' @param note Optional closing line.
#' @return A tag.
#' @export
sheet_hints <- function(note = "Read-only (italic) cells are skipped.") {
  hint <- function(keys, what) list(keys = keys, what = what)
  k <- function(...) lapply(c(...), tags$kbd)
  key_hints(
    list(
      hint(k("Click", "Drag"), "Select (Shift extends)"),
      hint(k("Type", "Enter"), "Edit, or double-click"),
      hint(k("Enter", "Tab"), "Confirm and move on"),
      hint(k("Esc"), "Cancel the edit"),
      hint(k("Ctrl+C/X/V"), "Copy, cut, paste"),
      hint(k("Del"), "Clear the selection"),
      hint(k("Ctrl+Z/Y"), "Undo, redo"),
      hint(tags$span(class = "sheet-hint-handle"), "Drag the corner to fill")
    ),
    note
  )
}
