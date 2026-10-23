# app/logic/overview_ui.R
#
# UI building blocks shared by the Database overviews (cgMLST, MLST): the card
# layouts, the loci tab, scrolling tables with a loading overlay, horizontal bar
# charts and the provenance check list.

box::use(
  DT[DTOutput, JS, datatable],
  bslib[
    as_fill_carrier,
    as_fill_item,
    as_fillable_container,
    card,
    card_body,
    card_header,
  ],
  htmltools[HTML, htmlEscape],
  shiny,
  shinyWidgets[pickerInput, pickerOptions],
  shinyjs[runjs],
)
box::use(
  app / logic / functions[color_sequence],
)

# Clears a table's loading overlay once DataTables has drawn it.
DRAW_SIGNAL <- JS(
  "function(settings) {",
  "  $(this.api().table().node())",
  "    .closest('.loci-table-body').addClass('is-loaded');",
  "}"
)

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

#' Fillable Overview Card
#'
#' @param header Character title, or a `card_header()`.
#' @param ... Card body contents.
#' @param body_class Optional extra class of the card body.
#'
#' @return A fill item wrapping a full-screen-capable `bslib::card()`.
#' @export
fill_card <- function(header, ..., body_class = NULL) {
  as_fill_item(card(
    fill = TRUE,
    full_screen = TRUE,
    if (is.character(header)) card_header(header) else header,
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
#'
#' @return A `bslib::card_header()`.
#' @export
table_header <- function(title, button_id, controls = NULL) {
  card_header(
    class = "loci-info-header",
    title,
    if (!is.null(controls)) shiny$div(class = "overview-header-controls", controls),
    shiny$downloadButton(
      button_id,
      "Export CSV",
      icon = shiny$icon("file-csv"),
      class = "btn-sm loci-export-btn"
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
#'
#' @return A `fill_card()`.
#' @export
table_card <- function(ns, id, title, export_id, label, controls = NULL) {
  as_fill_item(card(
    fill = TRUE,
    full_screen = TRUE,
    table_header(title, ns(export_id), controls),
    card_body(
      id = ns(paste0(id, "_body")),
      class = "loci-table-body",
      shiny$div(
        class = "loci-table-loading",
        shiny$div(
          class = "loci-loading-inner",
          shiny$div(class = "loci-spinner"),
          shiny$p(label, class = "loci-loading_text")
        )
      ),
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
#' @description The loci table next to the allele picker, the allele frequency
#'   chart and the sequence viewer. Output and input ids are fixed:
#'   `allele_select`, `allele_bars`, `allele_sequence`, `copy_seq`, `copy_idx`,
#'   `download_locus` and `export_loci`.
#'
#' @param ns Namespace function of the module.
#' @param table_id Output id of the loci table (unnamespaced).
#'
#' @return A fill carrier.
#' @export
loci_layout <- function(ns, table_id) {
  as_fill_carrier(shiny$div(
    class = "loci-info-layout",
    as_fill_carrier(shiny$div(
      class = "loci-info-main",
      table_card(ns, table_id, "Loci Table", "export_loci", "Loading loci table ...")
    )),
    as_fillable_container(shiny$div(
      class = "loci-controls",
      pickerInput(
        ns("allele_select"),
        label = "Select locus from table",
        choices = character(0),
        options = pickerOptions(
          liveSearch = TRUE,
          size = 10,
          liveSearchPlaceholder = "Search alleles ...",
          container = "body"
        )
      ),
      shiny$uiOutput(ns("allele_bars")),
      as_fill_carrier(shiny$div(
        class = "loci-allele-panel",
        shiny$div(
          class = "loci-allele-header",
          shiny$div(
            class = "loci-allele-actions",
            shiny$actionButton(
              ns("copy_seq"),
              "Sequence",
              icon = shiny$icon("copy"),
              class = "btn-sm"
            ),
            shiny$actionButton(
              ns("copy_idx"),
              "Allele",
              icon = shiny$icon("hashtag"),
              class = "btn-sm"
            ),
            shiny$downloadButton(
              ns("download_locus"),
              "Locus",
              icon = shiny$icon("download"),
              class = "btn-sm"
            )
          )
        ),
        as_fill_item(
          shiny$div(class = "sequence", shiny$uiOutput(ns("allele_sequence")))
        )
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

#' Scrolling Data Table
#'
#' @param df Data frame to show.
#' @param selection DT selection setting.
#' @param filter DT column filter position.
#'
#' @return A DT widget that fills its `table_card()` and scrolls internally.
#' @export
data_table <- function(df, selection = "none", filter = "top") {
  datatable(
    df,
    rownames = FALSE,
    filter = filter,
    selection = selection,
    class = "stripe row-border order-column",
    options = list(
      dom = "ti",
      paging = FALSE,
      scrollX = TRUE,
      scrollY = "1px",
      scrollCollapse = TRUE,
      columnDefs = list(list(className = "dt-left", targets = "_all")),
      initComplete = DRAW_SIGNAL
    )
  )
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

#' Footnote
#'
#' @param ... Text pieces, pasted with spaces.
#'
#' @return A muted note below a chart.
#' @export
overview_note <- function(...) shiny$p(class = "overview-note", paste(...))

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

#' Colored FASTA View
#'
#' @param header Character FASTA header (without `>`).
#' @param sequence Character nucleotide sequence.
#'
#' @return HTML of the header line and the colored sequence.
#' @export
sequence_view <- function(header, sequence) {
  HTML(paste0(
    "<div class=\"fasta-header\">&gt;",
    htmlEscape(header),
    "</div>",
    color_sequence(sequence)
  ))
}
