# app/logic/overview_ui.R
#
# UI building blocks shared by the Database overviews (cgMLST, MLST, AMR,
# Isolates): the card layouts, the loci tab, scrolling tables with a loading
# overlay and their row selection, horizontal bar charts and the provenance
# check list.

box::use(
  DT[DTOutput, JS, datatable],
  bslib[
    as_fill_carrier,
    as_fill_item,
    as_fillable_container,
    card,
    card_body,
    card_header,
    tooltip,
  ],
  htmltools[HTML, htmlEscape],
  shiny,
  shinyWidgets[virtualSelectInput],
  shinyjs[disabled, runjs],
)
box::use(
  app / logic / functions[color_sequence, table_loading],
)

# Clears a table's loading overlay once DataTables has drawn it.
DRAW_SIGNAL <- JS(
  "function(settings) {",
  "  $(this.api().table().node())",
  "    .closest('.loci-table-body').addClass('is-loaded');",
  "}"
)

#' Checks Card Hint
#'
#' @description What the Checks card of every overview shows.
#'
#' @export
CHECKS_HINT <- paste(
  "Consistency checks over every isolate, most severe first. Red makes results",
  "incomparable, yellow needs attention, blue is for information and green",
  "confirms consistency. The number counts the isolates concerned."
)

# Cuts a text cell to one line, the full value on hover. Values arrive already
# escaped (DT's `escape`), so only quotes need escaping for the title attribute.
TRUNCATE_CELL <- JS(
  "function(data, type) {",
  "  if (type !== 'display' || data === null || data === undefined) return data;",
  "  var text = String(data);",
  "  return '<div class=\"overview-truncate\" title=\"' + text.replace(/\"/g, '&quot;') +",
  "    '\">' + text + '</div>';",
  "}"
)

# Viewports' worth of rows Scroller draws at a time. Its viewport is the
# window height (see `.loci-table-body` in main.scss), more than any table card
# shows, so three leave ample rows above and below the visible ones.
SCROLL_BUFFER <- 3L

# Icon shown with each check tone.
TONE_ICONS <- c(
  danger = "circle-xmark",
  warning = "triangle-exclamation",
  info = "circle-info",
  success = "circle-check"
)

#' Format a Count
#'
#' @param x Numeric vector.
#'
#' @return Character vector with thousands separators.
#' @export
fmt <- function(x) format(x, big.mark = ",", trim = TRUE, scientific = FALSE)

#' Card Header Hint
#'
#' @description A question mark that explains a card on hover (and on keyboard
#'   focus), set at the right end of the card header.
#'
#' @param title Character card title, named for screen readers.
#' @param hint Optional character text shown on hover.
#'
#' @return The hint, or `NULL` when there is none.
#' @export
hint_icon <- function(title, hint = NULL) {
  if (is.null(hint)) {
    return(NULL)
  }
  tooltip(
    shiny$span(
      class = "overview-hint",
      tabindex = "0",
      `aria-label` = paste("About", title),
      shiny$icon("circle-question")
    ),
    hint,
    placement = "bottom"
  )
}

#' Count with Its Noun
#'
#' @param n Numeric count.
#' @param noun Character singular noun; the plural adds an "s".
#'
#' @return Character such as `"1 isolate"` or `"1,204 isolates"`.
#' @export
count_label <- function(n, noun = "isolate") {
  paste(fmt(n), ifelse(n == 1, noun, paste0(noun, "s")))
}

#' Fillable Overview Card
#'
#' @param header Character title, or a `card_header()`.
#' @param ... Card body contents.
#' @param body_class Optional extra class of the card body.
#' @param hint Optional character text shown on hovering a question mark next
#'   to a character title.
#'
#' @return A fill item wrapping a full-screen-capable `bslib::card()`.
#' @export
fill_card <- function(header, ..., body_class = NULL, hint = NULL) {
  as_fill_item(card(
    fill = TRUE,
    full_screen = TRUE,
    if (is.character(header)) card_header(header, hint_icon(header, hint)) else header,
    card_body(class = body_class, ...)
  ))
}

#' Main/Aside Overview Layout
#'
#' @description A wide main column next to a fixed-width aside, each a stack of
#'   `fill_card()`s.
#'
#' @param main List of cards for the main column.
#' @param aside List of cards for the aside.
#'
#' @return A fill carrier holding both columns.
#' @export
split_layout <- function(main, aside) {
  as_fill_carrier(shiny$div(
    class = "scheme-info-layout",
    as_fill_carrier(shiny$div(class = "scheme-info-main", main)),
    as_fill_carrier(shiny$div(class = "scheme-aside overview-aside", aside))
  ))
}

#' Card Header with a CSV Export Button
#'
#' @param title Character card title.
#' @param button_id Namespaced id of the download button.
#' @param controls Optional UI placed between the title and the button.
#' @param hint Optional character text shown on hovering a question mark next
#'   to the title.
#'
#' @return A `bslib::card_header()`.
#' @export
table_header <- function(title, button_id, controls = NULL, hint = NULL) {
  card_header(
    class = "loci-info-header",
    title,
    shiny$div(
      class = "overview-header-end",
      if (!is.null(controls)) shiny$div(class = "overview-header-controls", controls),
      shiny$downloadButton(
        button_id,
        "Export CSV",
        icon = shiny$icon("file-csv"),
        class = "btn-sm loci-export-btn"
      ),
      hint_icon(title, hint)
    )
  )
}

#' Table Card
#'
#' @description A full-height card whose DataTable fills it edge to edge and
#'   sits behind a loading overlay until its first draw.
#'
#' @param ns Namespace function of the module.
#' @param id Output id of the table (unnamespaced).
#' @param title Character card title.
#' @param export_id Output id of the CSV download (unnamespaced).
#' @param label Character loading message.
#' @param controls Optional UI shown in the card header, before the button.
#' @param hint Optional character text shown on hovering a question mark next
#'   to the title.
#'
#' @return A `fill_card()`.
#' @export
table_card <- function(ns, id, title, export_id, label, controls = NULL, hint = NULL) {
  as_fill_item(card(
    fill = TRUE,
    full_screen = TRUE,
    table_header(title, ns(export_id), controls, hint),
    card_body(
      id = ns(paste0(id, "_body")),
      class = "loci-table-body",
      table_loading(label),
      DTOutput(ns(id), fill = TRUE)
    )
  ))
}

#' Show Table Loading Overlays Again
#'
#' @description Put back the overlays of `table_card()`s while a newly opened
#'   database's tables draw.
#'
#' @param ns Namespace function of the module.
#' @param ids Character vector of table output ids (unnamespaced).
#' @export
reset_table_overlays <- function(ns, ids) {
  runjs(sprintf(
    "$('%s').removeClass('is-loaded');",
    paste0("#", ns(paste0(ids, "_body")), collapse = ", ")
  ))
}

#' Loci Info Tab
#'
#' @description The loci table next to the allele frequency chart, the allele
#'   picker, a line of allele details and the sequence viewer. Output and input
#'   ids are fixed: `allele_bars`, `allele_select`, `allele_info`,
#'   `allele_sequence`, `copy_seq`, `copy_idx`, `download_locus` and
#'   `export_loci`.
#'
#' @param ns Namespace function of the module.
#' @param table_id Output id of the loci table (unnamespaced).
#' @param index_label Character label of the `copy_idx` button, naming what it
#'   copies.
#' @param index_icon Character Font Awesome icon of that button.
#' @param table_hint Optional character hint of the loci table.
#'
#' @return A fill carrier.
#' @export
loci_layout <- function(
  ns,
  table_id,
  index_label = "Allele",
  index_icon = "hashtag",
  table_hint = NULL
) {
  as_fill_carrier(shiny$div(
    class = "loci-info-layout",
    as_fill_carrier(shiny$div(
      class = "loci-info-main",
      table_card(
        ns,
        table_id,
        "Loci Table",
        "export_loci",
        "Loading loci table ...",
        hint = table_hint
      )
    )),
    as_fillable_container(shiny$div(
      class = "loci-controls",
      shiny$uiOutput(ns("allele_bars")),
      # The picker, hash line, buttons and sequence all describe the picked allele,
      # so they sit in one group; only the frequency bars above stand apart.
      as_fill_carrier(shiny$div(
        class = "loci-allele-group",
        # A popup over the whole window rather than a menu under the toggle, as
        # every virtualSelectInput in the app: it can never run off the screen,
        # and its virtual list renders only the visible options of a long locus.
        virtualSelectInput(
          ns("allele_select"),
          label = "Select locus from table",
          choices = character(0),
          search = TRUE,
          searchPlaceholderText = "Search alleles ...",
          optionsCount = 10,
          hideClearButton = TRUE,
          dropboxWrapper = "body",
          showDropboxAsPopup = TRUE,
          popupDropboxBreakpoint = "10000px",
          width = "100%"
        ),
        shiny$uiOutput(ns("allele_info")),
        as_fill_carrier(shiny$div(
          class = "loci-allele-panel",
          shiny$div(
            class = "loci-allele-header",
            shiny$div(
              class = "loci-allele-actions",
              # Enabled by the module once a locus with alleles is picked.
              disabled(shiny$actionButton(
                ns("copy_seq"),
                "Sequence",
                icon = shiny$icon("copy"),
                class = "btn-sm"
              )),
              disabled(shiny$actionButton(
                ns("copy_idx"),
                index_label,
                icon = shiny$icon(index_icon),
                class = "btn-sm"
              )),
              disabled(shiny$downloadButton(
                ns("download_locus"),
                "Locus",
                icon = shiny$icon("download"),
                class = "btn-sm"
              ))
            )
          ),
          as_fill_item(
            shiny$div(class = "sequence", shiny$uiOutput(ns("allele_sequence")))
          )
        ))
      ))
    ))
  ))
}

#' One-Cell Message Table
#'
#' @description Stands in for a table that has nothing to show, and clears the
#'   loading overlay like a real one.
#'
#' @param text Character (HTML) message.
#'
#' @return A DT widget.
#' @export
message_table <- function(text) {
  datatable(
    data.frame(" " = text, check.names = FALSE),
    rownames = FALSE,
    colnames = "",
    escape = FALSE,
    selection = "none",
    options = list(
      dom = "t",
      ordering = FALSE,
      paging = FALSE,
      initComplete = DRAW_SIGNAL
    )
  )
}

# Converts plain double columns holding only whole numbers (positions, counts
# summed as doubles) to integer. DT gives a double column a stepless range
# slider that shows two decimals, an integer one a slider stepping by one.
# Classed doubles (dates) and values beyond the integer range stay as they are.
whole_as_integer <- function(df) {
  whole <- vapply(df, function(x) {
    is.double(x) && !is.object(x) &&
      all(is.na(x) | (x == round(x) & abs(x) <= .Machine$integer.max))
  }, logical(1))
  df[whole] <- lapply(df[whole], as.integer)
  df
}

#' Scrolling Data Table
#'
#' @description Rows are drawn as the table scrolls (DataTables' Scroller), so
#'   only the rows around the visible ones exist in the page, however many
#'   `df` has. Every row is one line high, as Scroller requires: text cells
#'   are cut short, their full value shown on hover. Column headers stay on
#'   one line; the table scrolls sideways instead.
#'
#' @param df Data frame to show. Plain numeric columns holding only whole
#'   numbers are shown as integers.
#' @param selection DT selection setting.
#' @param filter DT column filter position.
#'
#' @return A DT widget that fills its `table_card()` and scrolls internally.
#' @export
data_table <- function(df, selection = "none", filter = "top") {
  df <- whole_as_integer(df)
  text_cols <- which(vapply(df, function(x) is.character(x) || is.factor(x), logical(1)))
  datatable(
    df,
    rownames = FALSE,
    filter = filter,
    selection = selection,
    class = "stripe row-border order-column overview-table",
    extensions = "Scroller",
    options = list(
      dom = "ti",
      deferRender = TRUE,
      scroller = list(displayBuffer = SCROLL_BUFFER),
      scrollX = TRUE,
      scrollY = "1px",
      scrollCollapse = TRUE,
      columnDefs = list(
        list(className = "dt-left", targets = "_all"),
        list(render = TRUNCATE_CELL, targets = unname(text_cols) - 1L)
      ),
      initComplete = DRAW_SIGNAL
    )
  )
}

#' Selected Row of a Data Table
#'
#' @description Follows the single-row selection of a `data_table()`. The row
#'   is kept in a `reactiveVal`, which ignores a value identical to the one it
#'   holds, so the panels built on it re-render only when another row is
#'   selected or its data changes, not each time DataTables reports the same
#'   selection again. An empty selection, which DataTables reports whenever a
#'   table is rebuilt, keeps the last row.
#'
#' @param input Module `input`.
#' @param table_id Output id of the table (unnamespaced).
#' @param rows Function (e.g. a reactive) returning the data frame the table
#'   shows, in the order it was given to `data_table()`.
#'
#' @return A reactive returning the selected one-row data frame; it stops with
#'   `req()` until a row is selected and once the row no longer exists.
#' @export
selected_row <- function(input, table_id, rows) {
  current <- shiny$reactiveVal(NULL)
  input_id <- paste0(table_id, "_rows_selected")

  # Store the selected row. `rows()` is read only once a row is selected, so
  # a table that was never shown loads nothing.
  shiny$observe({
    row <- input[[input_id]]
    shiny$req(length(row) == 1)
    df <- rows()
    current(if (row <= nrow(df)) df[row, , drop = FALSE])
  })

  # The stored row, stopping while there is none.
  shiny$reactive(shiny$req(current()))
}

#' Key/Value Table
#'
#' @param keys Character vector of row labels.
#' @param values Character vector of HTML values; `NA` rows are dropped.
#'
#' @return A DT widget without headers, as the scheme metadata is shown.
#' @export
key_value_table <- function(keys, values) {
  keep <- !is.na(values)
  rows <- data.frame(
    key = sprintf("<strong>%s</strong>", htmlEscape(keys[keep])),
    value = values[keep],
    stringsAsFactors = FALSE
  )
  datatable(
    rows,
    class = "stripe row-border order-column",
    colnames = rep("", ncol(rows)),
    rownames = FALSE,
    escape = FALSE,
    selection = "none",
    options = list(dom = "t", ordering = FALSE, paging = FALSE)
  )
}

#' Side Panel Gate
#'
#' @description Placeholder inside a side panel that hides the panel while it has
#' nothing to show. Rendered by [side_gate()]; a rendered rule is replayed when the
#' app rebuilds its panels, which a one-shot shinyjs toggle is not.
#'
#' @param ns Module namespace function.
#' @param id Character id of the side panel the gate sits in.
#'
#' @return A hidden output holding the gate's style rule.
#' @export
side_gate_ui <- function(ns, id) {
  shiny$uiOutput(ns(paste0(id, "_gate")), style = "display: none;")
}

#' Side Panel Gate Output
#'
#' @param output Shiny output object of the module.
#' @param session Shiny session of the module.
#' @param id Character id of the side panel, as given to [side_gate_ui()].
#' @param show Reactive logical; the panel is hidden while it is not `TRUE`.
#'
#' @return Called for its side effect of defining the gate's output.
#' @export
side_gate <- function(output, session, id, show) {
  output[[paste0(id, "_gate")]] <- shiny$renderUI({
    if (!isTRUE(show())) {
      shiny$tags$style(sprintf("#%s { display: none !important; }", session$ns(id)))
    }
  })
  # The wrapper is itself hidden, which would otherwise suspend the output.
  shiny$outputOptions(output, paste0(id, "_gate"), suspendWhenHidden = FALSE)
}

#' Empty-State Hint
#'
#' @param text Character (HTML) message.
#'
#' @return A muted, centred placeholder.
#' @export
empty_hint <- function(text) {
  shiny$div(
    class = "db-empty-hint overview-empty",
    shiny$icon("circle-info"),
    shiny$p(HTML(text))
  )
}

#' Section Caption
#'
#' @param text Character caption.
#'
#' @return An uppercase caption above a block inside a card.
#' @export
section_title <- function(text) shiny$div(class = "overview-section-title", text)

#' Captioned Block Stack
#'
#' @param ... `section()`s.
#'
#' @return A plain block stack; unlike the card body it adds no gap of its own,
#'   so the sections sit close together.
#' @export
sections <- function(...) shiny$div(class = "overview-sections", ...)

#' Captioned Block
#'
#' @param title Character caption.
#' @param ... Content below the caption, e.g. `bar_rows()`.
#'
#' @return A caption with its content.
#' @export
section <- function(title, ...) {
  shiny$div(class = "overview-section", section_title(title), ...)
}

#' Grid of Stat Tiles
#'
#' @param ... `stat_tile()`s.
#'
#' @return A two-column tile grid.
#' @export
stat_grid <- function(...) shiny$div(class = "overview-stats", ...)

#' Horizontal Bar Chart
#'
#' @description One row per count: label, a bar scaled to the largest count,
#'   the count and its share of `total`. Hovering a row shows its exact figures.
#'
#' @param labels Character row labels.
#' @param counts Numeric counts.
#' @param total Numeric denominator of the shares.
#' @param tones Optional bar tones (`primary`, `success`, `info`, `warning`,
#'   `danger`, `secondary`), one per row.
#' @param notes Optional text replacing the share column where a share means
#'   nothing.
#' @param unit Character noun the hover text counts in.
#'
#' @return A `div` of bar rows.
#' @export
bar_rows <- function(labels, counts, total, tones = NULL, notes = NULL, unit = "isolates") {
  peak <- max(c(counts, 1))
  shiny$div(
    class = "overview-bars",
    lapply(seq_along(counts), function(i) {
      share <- if (total > 0) 100 * counts[i] / total else 0
      note <- if (is.null(notes)) sprintf("%.1f%%", share) else notes[i]
      shiny$div(
        class = "overview-bar",
        title = if (is.null(notes)) {
          sprintf(
            "%s: %s of %s %s (%.1f%%)",
            labels[i],
            fmt(counts[i]),
            fmt(total),
            unit,
            share
          )
        } else {
          sprintf("%s: %s %s", labels[i], fmt(counts[i]), note)
        },
        shiny$span(class = "overview-bar_label", labels[i]),
        shiny$div(
          class = "overview-bar_track",
          shiny$div(
            class = paste("overview-bar_fill", if (!is.null(tones)) paste0("is-", tones[i])),
            style = sprintf("width: %.1f%%;", 100 * counts[i] / peak)
          )
        ),
        shiny$span(class = "overview-bar_value", fmt(counts[i])),
        shiny$span(class = "overview-bar_share", note)
      )
    })
  )
}

#' Chip List
#'
#' @param labels Character labels; nothing is shown when there are none.
#' @param tone Character chip tone, `primary` or `secondary`.
#'
#' @return A wrapping row of chips, each label shown in full.
#' @export
chips <- function(labels, tone = "primary") {
  shiny$div(
    class = "overview-chips",
    lapply(labels, function(label) {
      shiny$span(class = paste("overview-chip", paste0("is-", tone)), label)
    })
  )
}

#' Fold a Long Count List into Its Top Rows
#'
#' @param labels Character labels, most frequent first.
#' @param counts Numeric counts, parallel to `labels`.
#' @param top Integer number of rows kept as they are.
#' @param other Character `sprintf()` format for the folded row, given the
#'   number of folded entries.
#'
#' @return A list with `labels`, `counts` and `tones` (`primary`, with the
#'   folded row `secondary`).
#' @export
fold_top <- function(labels, counts, top, other) {
  n <- length(counts)
  keep <- seq_len(min(top, n))
  out <- list(
    labels = labels[keep],
    counts = counts[keep],
    tones = rep("primary", length(keep))
  )
  if (n > top) {
    out$labels <- c(out$labels, sprintf(other, n - top))
    out$counts <- c(out$counts, sum(counts[-keep]))
    out$tones <- c(out$tones, "secondary")
  }
  out
}

#' Provenance Check List
#'
#' @param checks Data frame from `check_list()$table()`.
#'
#' @return One card per check: its tone as icon and edge colour, title, the
#'   number of isolates concerned and a sentence of detail.
#' @export
checks_ui <- function(checks) {
  shiny$div(
    class = "overview-checks",
    lapply(seq_len(nrow(checks)), function(i) {
      shiny$div(
        class = paste0("overview-check is-", checks$tone[i]),
        shiny$icon(TONE_ICONS[[checks$tone[i]]]),
        shiny$div(
          class = "overview-check_body",
          shiny$div(
            class = "overview-check_title",
            shiny$span(checks$check[i]),
            if (!is.na(checks$n[i])) {
              shiny$span(class = "overview-check_count", fmt(checks$n[i]))
            }
          ),
          shiny$p(class = "overview-check_detail", checks$detail[i])
        )
      )
    })
  )
}

#' Colored Sequence View
#'
#' @description The allele it shows is named by the picker above it, so the
#'   sequence stands alone.
#'
#' @param sequence Character nucleotide sequence.
#'
#' @return HTML of the colored sequence.
#' @export
sequence_view <- function(sequence) HTML(color_sequence(sequence))

#' Allele Details Line
#'
#' @param label Character label, e.g. `"SHA-256"`.
#' @param value Character value; nothing is shown when it is `NA` or `NULL`.
#' @param keep Integer characters kept at each end of a longer value, which is
#'   cut in the middle to fit one line; the full value shows on hover.
#'
#' @return A small labelled line.
#' @export
allele_info <- function(label, value, keep = 12L) {
  if (is.null(value) || is.na(value)) {
    return(NULL)
  }
  n <- nchar(value)
  shown <- if (n > 2L * keep + 1L) {
    paste0(substr(value, 1L, keep), "\u2026", substr(value, n - keep + 1L, n))
  } else {
    value
  }
  shiny$div(
    class = "overview-allele-info",
    title = value,
    shiny$span(class = "overview-allele-info_label", label),
    shiny$span(class = "overview-allele-info_value", shown)
  )
}
