# app/view/database_browser.R

box::use(
  shiny[
    NS,
    moduleServer,
    observeEvent,
    reactive,
    reactiveVal,
    isolate,
    div,
    h5,
    h2,
    reactiveValues,
    showNotification,
    actionButton,
    icon,
    req,
    uiOutput,
    renderUI,
    modalDialog,
    modalButton,
    showModal,
    removeModal,
    tagList,
    radioButtons
  ],
  bslib[as_fill_carrier, layout_sidebar, sidebar],
  shinyjs[disabled, disable, enable, addClass, removeClass],
  shinyWidgets[virtualSelectInput],
  waiter[Waiter, spin_flower, useWaiter, autoWaiter],
  htmltools[tags],
  DT[
    DTOutput,
    renderDT,
    datatable,
    editData,
    dataTableProxy,
    replaceData
  ],
)

box::use(
  app /
    logic /
    custom_fields[
      CUSTOM_TYPES,
      NUMERIC_TYPES,
      append_custom,
      coerce_custom_value,
      custom_col,
      list_custom_fields,
      save_custom_values
    ],
  app /
    logic /
    database_functions[
      save_metadata_table,
      remove_isolates,
      append_classical_mlst,
      append_amr_matrix
    ],
  app / logic / db_events,
  app / logic / db_store,
  app / logic / field_labels[field_labels_for, grouped_field_choices],
  app / logic / field_types[date_fields],
  app / logic / db_sources[SOURCE_COL, SOURCE_LOCAL],
)

# Synthetic picker-choice values for the AMR drug-class / virulence-stress
# groups (see amr_gene_groups() below): one entry per group, not per gene, so
# the picker shows/hides a whole hierarchical header group at once. Never a
# real column name - resolved back to the group's actual gene columns in the
# col_picker observer.
AMR_GROUP_TOKEN_PREFIX <- ".amr_group:"

# Columns the user may never edit: `isolate` is the isolate identity (shared
# with mlst.souche), `organism` is fixed by mlst_type.species for the whole
# database, `called_at` is the app-stamped time the isolate was taken on, and
# `source` is provenance the app writes when the isolate arrives (typed here, or
# the label of the database a merge brought it in from).
# Located by name, not position — an imported peer database can carry extra
# metadata columns in any order.
READONLY_COLS <- c("isolate", "organism", "called_at", SOURCE_COL)

# 0-based DT column indices for `cols` within `all_cols`; NAs dropped.
.dt_idx <- function(all_cols, cols) {
  idx <- match(cols, all_cols)
  as.integer(idx[!is.na(idx)] - 1L)
}

# The same columns, in the base DT's `editable` list wants for its
# numeric/date/area fields — which is *not* the base columnDefs targets use.
# DT::makeEditableField() ends with `z - is.null(rn)`: with rownames = FALSE
# there is no rownames column, so it shifts whatever it is given one column to
# the left before handing it to the JS. Passing 0-based indices therefore opens
# the date picker on the column *before* the date one. Hand it 1-based indices
# so the shift lands on the intended column.
#
# `disable$columns` goes through no such adjustment and stays 0-based, which is
# why READONLY_COLS is indexed with .dt_idx() directly.
.dt_editable_idx <- function(all_cols, cols) .dt_idx(all_cols, cols) + 1L

# A two-row DT header (see DT's `container` argument): row one names each
# category over a colspan of the columns in it, row two names the columns
# themselves. `groups` and `labels` are both named by column name; a column
# absent from `groups` is ungrouped and spans both rows instead.
#
# The rowspan matters for the frozen first column. DataTables maps each column
# to a single header cell, and for an ungrouped column it picks the row-one
# cell - so splitting `isolate` into an empty row-one cell plus a labelled
# row-two one hands FixedColumns the *empty* cell to pin, leaving the visible
# "Isolate" label to scroll away with the body. One rowspan cell is the column's
# only header cell, so the label is what gets pinned.
#
# Columns of one category are assumed contiguous, which holds because
# metadata_base() appends them a category at a time.
.grouped_header_sketch <- function(cols, labels, groups) {
  top <- list()
  leaf <- list()
  i <- 1L
  n <- length(cols)
  group_of <- function(col) {
    if (col %in% names(groups)) unname(groups[col]) else NA_character_
  }

  # DataTables tags the one cell it maps to each column with `.sorting`, which
  # is what the app's header typography keys off. For a group spanning several
  # columns that is the leaf cell, but a *single*-column group's row-one cell
  # also identifies its column uniquely, and DataTables picks that one - so the
  # leaf under it is left bare and would render unstyled next to its siblings.
  # Carrying the styling on a class of our own makes every gene name look the
  # same however DataTables resolved it.
  add_leaf <- function(col) {
    leaf[[length(leaf) + 1L]] <<- tags$th(
      class = "dt-hdr-leaf",
      unname(labels[col])
    )
  }

  while (i <= n) {
    grp <- group_of(cols[i])
    if (is.na(grp)) {
      top[[length(top) + 1L]] <- tags$th(
        rowspan = 2,
        class = "dt-hdr-ungrouped",
        unname(labels[cols[i]])
      )
      i <- i + 1L
      next
    }
    j <- i
    while (j <= n && identical(group_of(cols[j]), grp)) {
      j <- j + 1L
    }
    run <- cols[i:(j - 1L)]
    top[[length(top) + 1L]] <- tags$th(
      colspan = length(run),
      class = "dt-hdr-group",
      grp
    )
    for (col_j in run) {
      add_leaf(col_j)
    }
    i <- j
  }

  tags$table(
    tags$thead(
      do.call(tags$tr, top),
      do.call(tags$tr, leaf)
    )
  )
}

#' @export
ui <- function(id) {
  ns <- NS(id)

  as_fill_carrier(
    div(
      id = ns("module-container"),
      layout_sidebar(
        padding = 0,
        border = FALSE,
        sidebar = sidebar(
          position = "right",
          width = 300,
          open = TRUE,
          fillable = TRUE,
          as_fill_carrier(
            div(
              class = "sidebar-control",
              div(
                class = "sidebar-control-group",
                div(class = "control-group-label", "Column Selection"),
                uiOutput(ns("col_picker_ui"))
              ),
              div(
                class = "sidebar-control-group",
                div(class = "control-group-label", "Edit"),
                disabled(
                  actionButton(
                    ns("discard"),
                    "Discard",
                    icon = icon("rotate-left")
                  )
                ),
                disabled(
                  actionButton(
                    ns("save"),
                    "Save Changes",
                    class = "btn-success",
                    icon = icon("floppy-disk")
                  )
                )
              ),
              div(
                class = "sidebar-control-group",
                div(class = "control-group-label", "Remove isolates"),
                uiOutput(ns("remove_picker_ui")),
                disabled(
                  actionButton(
                    ns("remove_btn"),
                    "Remove",
                    class = "btn-danger",
                    icon = icon("trash")
                  )
                )
              )
            )
          )
        ),
        as_fill_carrier(
          div(
            # allow-free-text: every editable cell here is either a metadata
            # value or a custom-variable value, and both are display text
            # (@sec-data-naming), not an identifier - unlike the Custom
            # Variables Values grid, this one was missing the opt-out from the
            # global charset filter (app/js/index.js), so a place name typed
            # here silently lost its spaces and punctuation on entry.
            class = "db-page_body edit-table allow-free-text",
            autoWaiter(ns("metadata_table")),
            uiOutput(ns("table_ui"), fill = TRUE)
          )
        )
      )
    )
  )
}

# Re-bases the grid's edited snapshot onto the metadata table as it stands now.
#
# `current` decides which isolates exist and in what column order, so isolates
# added since the grid was filled keep their fresh row and isolates removed
# since simply are not there. `edited` decides the values, but only for the
# isolates present in both and only for columns both sides have.
.reconcile_metadata <- function(edited, current) {
  if (is.null(current) || !nrow(current)) {
    return(edited)
  }
  if (is.null(edited) || !nrow(edited)) {
    return(current)
  }

  keep <- intersect(edited$isolate, current$isolate)
  if (!length(keep)) {
    return(current)
  }

  into <- match(keep, current$isolate)
  from <- match(keep, edited$isolate)
  for (col in setdiff(intersect(names(edited), names(current)), "isolate")) {
    current[[col]][into] <- edited[[col]][from]
  }
  current
}

#' @export
server <- function(
  id,
  db_path = shiny::reactive(NULL),
  session_reset = shiny::reactive(0L),
  db_rev = db_events$new_bus(),
  # Wired to whatever db_path/db_rev this call actually received - see
  # database.R's server() for why the default can't just be
  # `db_store$new_store()`.
  store = db_store$new_store(db_path = db_path, db_rev = db_rev)
) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Two dirty buffers, one per write destination: metadata_dirty for
    # save_metadata_table() (isolate/column/value, one row per edited cell),
    # custom_dirty for save_custom_values() (see the cell-edit observer).
    State <- reactiveValues(
      data = NULL,
      pending = FALSE,
      metadata_dirty = NULL,
      custom_dirty = NULL
    )

    # Removing many isolates re-hashes what's left and can block the R
    # session for a while; cover the whole tab so the UI reads as busy
    # rather than frozen, same pattern as the Import panel.
    remove_waiter <- Waiter$new(
      id = ns("module-container"),
      html = div(
        class = "spinner-custom",
        spin_flower(),
        h5("Removing isolates ...")
      )
    )

    # The only thing that invalidates metadata_base's cache after the initial
    # load. Everything that wants this grid re-read goes through here rather
    # than becoming a dependency of metadata_base itself, because every route
    # has to pass the same test first: are there unsaved edits on screen?
    #
    # Bumped by an explicit data reload, by save and discard, and - via the
    # revision observer below - by another module's write, but in that last case
    # only when nothing is pending. db_rev is deliberately not a dependency of
    # metadata_base() itself: a re-read behind the user's back wipes edits they
    # have not committed.
    reload_token <- reactiveVal(0L)

    metadata_base <- reactive({
      reload_token()
      path <- db_path()
      if (is.null(path) || is.na(path)) {
        return(NULL)
      }
      # isolate(): metadata_base must recompute only on reload_token, exactly
      # as before the store existed - reading store$metadata() reactively would
      # pull in its dependency on db_rev and defeat the whole point of routing
      # every refresh through reload_token (see its definition above).
      df <- isolate(store$metadata())
      if (is.null(df) || !nrow(df)) {
        return(NULL)
      }

      # Append the classical-MLST columns for display only (see
      # append_classical_mlst): never persisted - the Save handler strips them
      # again before writing.
      df <- append_classical_mlst(df, path)
      appended_mlst <- attr(df, "mlst_cols", exact = TRUE)

      df[is.na(df)] <- ""

      # The AMR gene-matrix columns (see append_amr_matrix) and the
      # user-defined custom variables, also display only. Both appended *after*
      # the NA replacement above: assigning "" into a numeric custom column
      # would coerce it to character and cost it its numeric sort order, and
      # assigning "" into a gene column would coerce its call-state factor into
      # character - losing both the NA that means "gene not found" and DT's
      # automatic level dropdown for factor columns (see load_amr_matrix()).
      df <- append_amr_matrix(df, path)
      appended_amr <- attr(df, "amr_cols", exact = TRUE)
      appended_amr_groups <- attr(df, "amr_gene_groups", exact = TRUE)
      appended_amr_labels <- attr(df, "amr_gene_labels", exact = TRUE)

      df <- append_custom(df, path)
      appended_custom <- attr(df, "custom_cols", exact = TRUE)

      # Re-set last so they survive the NA replacement above.
      attr(df, "mlst_cols") <- appended_mlst
      attr(df, "amr_cols") <- appended_amr
      attr(df, "amr_gene_groups") <- appended_amr_groups
      attr(df, "amr_gene_labels") <- appended_amr_labels
      attr(df, "custom_cols") <- appended_custom
      df
    })

    # Isolates already present the first time this database is shown this
    # session. Anything in the table beyond this set was added since (typed,
    # then brought in by a reload) and gets a subtle row highlight in renderDT.
    # Keyed on db_path so the baseline is taken once per database and survives
    # the reload that introduces the new isolates (a reload keeps db_path); a
    # different database re-captures. The baseline is read straight off the
    # already-loaded df, so probing "what's new" costs no extra query.
    #
    # Plain session variables rather than reactives: nothing may depend on them.
    # They are written in exactly two places - the capture in session_added()
    # below, and the removal handler, which prunes what it deleted so that
    # re-adding the same isolate this session still counts as an addition. Both
    # writes happen before the read they affect, so there is no ordering hazard.
    baseline_key <- NULL
    baseline_isolates <- character(0)

    # All picker choice values (raw column names plus AMR group tokens) the
    # column picker has ever offered, tracked outside the reactive graph for
    # the same reason as baseline_isolates above: col_picker_ui reruns on
    # every metadata_base() refresh and needs to tell a genuinely new field -
    # which should get the normal start-visible/start-hidden default - apart
    # from one the user deliberately hid. Written and read only there.
    col_picker_seen <- character(0)

    session_added <- reactive({
      df <- metadata_base()
      path <- db_path()
      if (is.null(df) || !nrow(df) || is.null(path) || is.na(path)) {
        return(character(0))
      }
      if (!identical(baseline_key, path)) {
        # First view of this database this session: everything present now is
        # the baseline, so nothing is new yet.
        baseline_key <<- path
        baseline_isolates <<- df$isolate
        return(character(0))
      }
      setdiff(df$isolate, baseline_isolates)
    })

    # The same set, split by how each isolate arrived: typing stamps
    # `metadata.source` with SOURCE_LOCAL, a merge stamps the peer's label (see
    # app/logic/db_sources.R). Isolates whose row predates provenance tracking
    # carry NA and count as typed — whatever they are, they are not something
    # this session imported. `sources` names the peer databases involved, for
    # the legend's tooltip.
    session_added_split <- reactive({
      new <- session_added()
      df <- metadata_base()
      empty <- list(
        typed = character(0),
        imported = character(0),
        sources = character(0)
      )
      if (!length(new) || is.null(df)) {
        return(empty)
      }

      src <- if (SOURCE_COL %in% names(df)) {
        as.character(df[[SOURCE_COL]])[match(new, df$isolate)]
      } else {
        rep(NA_character_, length(new))
      }
      is_import <- !is.na(src) & nzchar(src) & src != SOURCE_LOCAL

      list(
        typed = new[!is_import],
        imported = new[is_import],
        sources = sort(unique(src[is_import]))
      )
    })

    # Names of the appended classical-MLST columns (empty when the database has
    # no classical typing). The single source of truth for which columns are
    # read-only, grouped under "Classical MLST", and stripped before save.
    mlst_cols <- reactive({
      df <- metadata_base()
      if (is.null(df)) {
        return(character(0))
      }
      cols <- attr(df, "mlst_cols", exact = TRUE)
      if (is.null(cols)) character(0) else cols
    })

    # Names of the appended AMR-screening columns (empty when the database has no
    # AMR screening). Like mlst_cols: read-only, grouped under "AMR screening",
    # and stripped before save.
    amr_cols <- reactive({
      df <- metadata_base()
      if (is.null(df)) {
        return(character(0))
      }
      cols <- attr(df, "amr_cols", exact = TRUE)
      if (is.null(cols)) character(0) else cols
    })

    # The drug-class / virulence-stress group each AMR gene column belongs to
    # (named by column, in column order). Drives the AMR part of the
    # hierarchical DT header and lets the column picker show/hide a whole drug
    # class at once instead of gene by gene.
    amr_gene_groups <- reactive({
      df <- metadata_base()
      if (is.null(df)) {
        return(character(0))
      }
      groups <- attr(df, "amr_gene_groups", exact = TRUE)
      if (is.null(groups)) character(0) else groups
    })

    # The bare (canonical) gene symbol each AMR gene column holds, for the
    # hierarchical header's leaf row. Same shape as amr_gene_groups().
    amr_gene_labels <- reactive({
      df <- metadata_base()
      if (is.null(df)) {
        return(character(0))
      }
      labels <- attr(df, "amr_gene_labels", exact = TRUE)
      if (is.null(labels)) character(0) else labels
    })

    # Names of the appended custom-variable columns (empty when the database has
    # none defined). Editable here as well as in the Custom Variables panel;
    # both route through coerce_custom_value() / save_custom_values(), so
    # phylotrace_custom_values still has exactly one validated write path.
    custom_cols <- reactive({
      df <- metadata_base()
      if (is.null(df)) {
        return(character(0))
      }
      cols <- attr(df, "custom_cols", exact = TRUE)
      if (is.null(cols)) character(0) else cols
    })

    # The category every column sits under in the two-row header, named by
    # column and in column order. The same four categories the field picker
    # groups by, so header and picker read as one scheme - except that AMR is
    # split by drug class / virulence-stress group rather than lumped under one
    # "AMR screening" span, which would say nothing a gene name doesn't.
    #
    # `isolate` is deliberately left out: it is the frozen first column, and
    # leaving it ungrouped keeps the FixedColumns clone simple (see
    # .grouped_header_sketch()).
    header_groups <- reactive({
      df <- metadata_base()
      if (is.null(df)) {
        return(character(0))
      }
      cols <- names(df)
      genes <- amr_gene_groups()
      mlst <- mlst_cols()
      custom <- custom_cols()
      base <- setdiff(cols, c("isolate", names(genes), mlst, custom))

      groups <- c(
        stats::setNames(rep("Sample metadata", length(base)), base),
        stats::setNames(rep("Classical MLST", length(mlst)), mlst),
        genes,
        stats::setNames(rep("Custom variables", length(custom)), custom)
      )
      # Column order, so the sketch's contiguous-run detection sees the runs
      # the frame actually has.
      groups[intersect(cols, names(groups))]
    })

    # The label every column carries in the header's second row: the usual
    # field label, except for the AMR gene columns, whose synthetic `amr_g<n>`
    # names carry no meaning - they show the bare gene symbol instead.
    header_labels <- reactive({
      df <- metadata_base()
      if (is.null(df)) {
        return(character(0))
      }
      cols <- names(df)
      labels <- stats::setNames(field_labels_for(cols), cols)
      genes <- amr_gene_labels()
      if (length(genes)) {
        labels[names(genes)] <- unname(genes)
      }
      labels
    })

    # The custom-variable definitions (id / name / type / levels) behind those
    # columns, keyed by the prefixed column name they appear under. This is what
    # makes the type in `phylotrace_custom_fields` the authority here too: which
    # editor a cell gets and how its value is validated both come from `type`,
    # never from guessing at the column's contents.
    custom_defs <- reactive({
      cols <- custom_cols()
      defs <- list_custom_fields(db_path())
      if (!is.data.frame(defs) || !nrow(defs)) {
        # Still carry `column`, so every consumer can index on it without
        # first re-checking whether any variables are defined.
        return(data.frame(
          id = integer(0),
          name = character(0),
          type = character(0),
          levels = character(0),
          column = character(0),
          stringsAsFactors = FALSE
        ))
      }
      defs$column <- custom_col(defs$name)
      defs[defs$column %in% cols, , drop = FALSE]
    })

    # Custom columns of a given type, as column names. Used to hand DT the
    # right native editor per column.
    custom_cols_of_type <- function(wanted) {
      defs <- custom_defs()
      defs$column[defs$type %in% wanted]
    }

    # Every date-typed column on screen: the fixed schema's collection date plus
    # any user-defined date variable, both answered by the shared type map (see
    # app/logic/field_types.R). Drives the native date editor and the `col-date`
    # width floor, so a custom date column behaves exactly like the built-in
    # one. Plain `"date"` only — `called_at` is a timestamp, and a date picker
    # on it would drop its time of day.
    date_cols <- reactive({
      df <- metadata_base()
      date_fields(db_path(), names(df), types = "date")
    })

    observeEvent(metadata_base(), {
      State$data <- metadata_base()
      State$metadata_dirty <- NULL
      State$custom_dirty <- NULL
      State$pending <- FALSE
    })

    # Columns the user can toggle: everything except the always-visible
    # `isolate`. Choice *values* are the raw column names (stable across
    # imports); the labels are the prettified ones.
    optional_cols <- reactive({
      df <- metadata_base()
      if (is.null(df)) character(0) else setdiff(names(df), "isolate")
    })

    output$col_picker_ui <- renderUI({
      # This output reruns on every metadata_base() refresh - including a
      # Discard, which bumps reload_token() precisely to catch this grid up
      # with the database (see the discard observer) - and virtualSelectInput
      # has no incremental "choices changed" path through renderUI: each
      # rerun re-sends the widget's full markup, which the browser treats as
      # a fresh element and re-initializes to whatever `selected=` says here.
      # Anchoring on the field's *current* picker value rather than always
      # falling back to the fixed default is what keeps a Discard from also
      # wiping out the user's field visibility choices. isolate(): reading
      # input$col_picker must not itself become a dependency of this output -
      # col_picker has updateOn = "close" so it usually only changes when the
      # dropdown closes anyway, but making the read reactive would still wire
      # this renderUI to rerun (and rebuild the whole widget, closing it) on
      # every value it sends - exactly the bug this caused on remove_picker,
      # which has no updateOn and so updates live on every click.
      prev_selected <- isolate(input$col_picker)

      cols <- optional_cols()
      mlst <- mlst_cols()
      amr <- amr_cols()
      amr_groups <- amr_gene_groups()
      gene_cols <- names(amr_groups)
      custom <- custom_cols()

      # Group the derived MLST columns and the custom variables under their own
      # headings so the user can pick them out as categories. Individual gene
      # columns are never picker entries of their own: one entry per drug-class
      # / virulence-stress group is added below instead, so toggling it
      # shows/hides every gene column under that group at once (see the
      # col_picker observer, which resolves the group token back to columns).
      choices <- grouped_field_choices(
        setdiff(cols, gene_cols),
        mlst,
        character(0),
        custom
      )

      if (length(amr_groups)) {
        group_labels <- unique(unname(amr_groups))
        if (!is.list(choices)) {
          choices <- list("Sample metadata" = choices)
        }
        choices[["AMR screening"]] <- stats::setNames(
          paste0(AMR_GROUP_TOKEN_PREFIX, group_labels),
          group_labels
        )
        # grouped_field_choices() has no AMR entry to place any more (every AMR
        # column is a gene, and those are withheld above), so the group added
        # here lands last. Restore the categories' intended reading order.
        choices <- choices[intersect(
          c(
            "Sample metadata",
            "Classical MLST",
            "AMR screening",
            "Custom variables"
          ),
          names(choices)
        )]
      }

      # virtualSelectInput rather than pickerInput: in multiple mode it puts a
      # checkbox on every optgroup header (disableOptionGroupCheckbox defaults
      # to FALSE), which is the only way to (de)select a whole category at once
      # - bootstrap-select has no equivalent. Same nested-list `choices`, and
      # the input value is still a plain character vector of column names.
      virtualSelectInput(
        ns("col_picker"),
        label = NULL,
        choices = choices,
        # Derived MLST / AMR columns start hidden - the user opts into the
        # category to show them. Custom variables, being the user's own data,
        # start visible. Keep this consistent with the DT's initial column
        # visibility. That default only applies the first time this renders
        # (prev_selected is NULL then) and to any field appearing for the
        # first time on a later rerun (a new custom variable, say); every
        # other field keeps whatever the user had picked, dropping only
        # choices that stopped existing (see prev_selected above).
        selected = {
          all_values <- unlist(choices, use.names = FALSE)
          default_visible <- setdiff(cols, c(mlst, amr))
          sel <- if (is.null(prev_selected)) {
            default_visible
          } else {
            kept <- intersect(prev_selected, all_values)
            new_fields <- setdiff(all_values, col_picker_seen)
            union(kept, intersect(new_fields, default_visible))
          }
          col_picker_seen <<- all_values
          sel
        },
        multiple = TRUE,
        search = TRUE,
        searchPlaceholderText = "Search fields ...",
        placeholder = "Show fields …",
        optionsCount = 8,
        noOfDisplayValues = 2,
        # This tab pane is a bslib fill container (overflow: hidden), which
        # clips the dropdown once it grows taller than the remaining space.
        # Render it into <body> instead so it escapes that clip.
        dropboxWrapper = "body",
        # virtual-select's own "show as popup" mode (normally mobile-only,
        # gated behind popupDropboxBreakpoint) centers the dropbox over the
        # whole viewport with a backdrop instead of anchoring it under the
        # toggle - forced on unconditionally here by setting the breakpoint
        # above any real screen width, so a 300px-wide sidebar control never
        # has to open a box far wider than itself in place.
        showDropboxAsPopup = TRUE,
        popupDropboxBreakpoint = "10000px",
        # One column-visibility pass per dropdown session rather than one per
        # click: toggling a whole group otherwise redraws the table per option.
        updateOn = "close",
        width = "100%"
      )
    })

    output$remove_picker_ui <- renderUI({
      # Reruns on every metadata_base() refresh, including a Discard - see
      # the matching comment on col_picker_ui above for why that means this
      # widget gets fully re-initialized in the browser, and why `selected=`
      # has to be seeded from the picker's own current value rather than a
      # fixed default. isolate(): remove_picker has no updateOn = "close", so
      # input$remove_picker updates live on every click - reading it
      # reactively here would rerun (and so rebuild and close) this widget on
      # every single click, making multi-select impossible.
      prev_selected <- isolate(input$remove_picker)

      df <- metadata_base()
      choices <- if (!is.null(df)) df$isolate else character(0)
      # virtualSelectInput rather than pickerInput, same as col_picker above:
      # isolate ids are long and this list can be large, and the popup mode
      # (see col_picker_ui) reads far better centered over the page than
      # pinned under a 300px-wide sidebar control.
      virtualSelectInput(
        ns("remove_picker"),
        label = NULL,
        choices = choices,
        selected = intersect(prev_selected, choices),
        multiple = TRUE,
        search = TRUE,
        searchPlaceholderText = "Search isolates ...",
        placeholder = "Select isolates to remove …",
        noOfDisplayValues = 2,
        dropboxWrapper = "body",
        showDropboxAsPopup = TRUE,
        popupDropboxBreakpoint = "10000px",
        width = "100%"
      )
    })

    observeEvent(
      session_reset(),
      {
        State$data <- NULL
        State$pending <- FALSE
        reload_token(reload_token() + 1L)
      },
      ignoreInit = TRUE
    )

    # Someone else wrote what this grid is showing: custom variable definitions
    # or values (the Custom Variables panel), the isolate pool (typing, a
    # removal), or the metadata itself (a merge or a restore).
    #
    # Skipped while edits are pending. This grid is the one place in the app
    # holding uncommitted user input, so an automatic re-read is not a refresh -
    # it is data loss. It goes stale instead, and catches up on the next save,
    # discard or explicit "Reload Database"; main.R's reload notification warns
    # about the pending edits before offering that button.
    observeEvent(
      db_events$revision(db_rev, "custom_fields", "isolates", "metadata"),
      {
        if (!isTRUE(State$pending)) {
          reload_token(reload_token() + 1L)
        }
      },
      ignoreInit = TRUE
    )

    output$table_ui <- renderUI({
      df <- metadata_base()

      if (is.null(df)) {
        return(div(
          class = "db-empty-hint",
          "No entries in this database yet.",
          tags$br(),
          "Add isolates by typing them in the ",
          tags$strong("Add Isolates"),
          " module."
        ))
      }

      DTOutput(ns("metadata_table"), fill = TRUE)
    })

    output$metadata_table <- renderDT({
      df <- metadata_base()
      req(df)

      # Isolates that entered this database during the current session, split by
      # provenance: typed ones get the .session-new-row class, merged-in ones
      # .session-import-row (both styled subtly in main.scss). Serialised to JS
      # array literals for the rowCallback below.
      added <- session_added_split()
      new_isolates <- added$typed
      imported_isolates <- added$imported
      has_new <- length(new_isolates) > 0
      has_imported <- length(imported_isolates) > 0

      cols <- names(df)
      # Default-sorted on below via options$order: newest isolate first, so a
      # session's new rows (highlighted via .session-new-row) surface at the
      # top without needing to scroll or track a "just added" state.
      called_at_idx <- .dt_idx(cols, "called_at")
      # MLST / AMR columns are derived from typing (classical_mlst / amr_summary)
      # and are never editable here. Custom variables are: their edits are
      # validated and routed to phylotrace_custom_values on save. The derived
      # ones additionally start hidden, until the user opts into their category.
      derived <- c(mlst_cols(), amr_cols())
      readonly_idx <- .dt_idx(cols, c(READONLY_COLS, derived))
      hidden_idx <- .dt_idx(cols, derived)
      # Two bases on purpose — see .dt_editable_idx(). The className targets
      # below are columnDefs (0-based); the editable list is not.
      date_idx <- .dt_idx(cols, date_cols())
      edit_date_idx <- .dt_editable_idx(cols, date_cols())
      edit_numeric_idx <- .dt_editable_idx(
        cols,
        custom_cols_of_type(NUMERIC_TYPES)
      )

      # AMR gene columns hold a call-state factor, NA where the gene was not
      # found (see load_amr_matrix()). Render the state as a mark rather than
      # its word, so a wide grid of genes stays scannable, and title each one so
      # the distinction is discoverable without a legend. The raw level is
      # returned for every non-display request, which is what keeps sorting and
      # the column's level dropdown filtering on the state itself.
      gene_idx <- .dt_idx(cols, names(amr_gene_groups()))
      has_amr <- length(gene_idx) > 0

      # Legend(s) explaining marks this table draws without a visible key of
      # their own: the AMR call marks above and the .session-new-row highlight
      # (rowCallback below). Built once, up front, as a plain HTML string -
      # appended to DataTables' own info text by infoCallback (see dt_args)
      # rather than injected as a DOM node, because DataTables replaces the
      # info div's content wholesale on every draw (paging, filtering, and the
      # column-visibility.dt redraw the picker triggers), which would wipe out
      # anything appended only once via initComplete.
      legend_html <- ""
      if (has_amr || has_new || has_imported) {
        parts <- character(0)
        if (has_amr) {
          parts <- c(parts, paste0(
            '<span class="amr-legend">',
            '<span class="amr-call amr-call-match" title="Match — reported without a quality flag (an exact or allele match for an AMR gene family; point mutations and virulence/stress genes are never flagged)">&#10003;</span> match&ensp;',
            '<span class="amr-call amr-call-inexact" title="Inexact — flagged * by abritamr: a BLAST hit close to, but not identical to, a known reference allele">&#10003;*</span> inexact&ensp;',
            '<span class="amr-call amr-call-partial" title="Partial — from abritamr&#39;s partials set (e.g. an internal stop codon): the gene is likely truncated or non-functional">~</span> partial',
            '</span>'
          ))
        }
        # Two separate entries rather than one "added this session": the two
        # tints mean different things (typed here vs merged in from a peer), and
        # a single count would leave the second tint unexplained. The import
        # entry names its source databases on hover — the same labels the Source
        # column carries, so the legend and the column agree.
        if (has_new) {
          parts <- c(parts, paste0(
            '<span class="session-new-legend">',
            '<span class="session-new-swatch"></span>',
            if (length(new_isolates) == 1) "1 isolate" else paste0(length(new_isolates), " isolates"),
            " typed this session",
            '</span>'
          ))
        }
        if (has_imported) {
          parts <- c(parts, paste0(
            '<span class="session-new-legend" title="',
            htmltools::htmlEscape(
              paste0("From: ", paste(added$sources, collapse = ", ")),
              attribute = TRUE
            ),
            '">',
            '<span class="session-import-swatch"></span>',
            if (length(imported_isolates) == 1) "1 isolate" else paste0(length(imported_isolates), " isolates"),
            " imported this session",
            '</span>'
          ))
        }
        legend_html <- paste0(
          '<span class="dt-legend-group">',
          paste(parts, collapse = ""),
          '</span>'
        )
      }

      column_defs <- list(
        list(className = "dt-left", targets = "_all"),
        list(className = "col-readonly", targets = readonly_idx)
      )
      if (length(gene_idx)) {
        column_defs <- c(
          column_defs,
          list(list(
            targets = gene_idx,
            className = "amr-gene-cell",
            render = DT::JS(
              "function(data, type) {
                if (type !== 'display') return data;
                if (data === 'Match') {
                  return '<span class=\"amr-call amr-call-match\" title=\"Match — reported without a quality flag (an exact or allele match for an AMR gene family; point mutations and virulence/stress genes are never flagged)\">&#10003;</span>';
                }
                if (data === 'Inexact') {
                  return '<span class=\"amr-call amr-call-inexact\" title=\"Inexact — flagged * by abritamr: a BLAST hit close to, but not identical to, a known reference allele\">&#10003;*</span>';
                }
                if (data === 'Partial') {
                  return '<span class=\"amr-call amr-call-partial\" title=\"Partial — from abritamr&#39;s partials set (e.g. an internal stop codon): the gene is likely truncated or non-functional\">~</span>';
                }
                return '';
              }"
            )
          ))
        )
      }
      # Give every date column a wider floor than the rest (see col-date in
      # main.scss): a native date input is wider than the 6rem general floor, so
      # without this DT grows the column the moment the editor opens and reflows
      # everything to its right. Guarded on length() because DT's columnDefs
      # treat an empty `targets` as "all columns".
      if (length(date_idx)) {
        column_defs <- c(
          column_defs,
          list(list(className = "col-date", targets = date_idx))
        )
      }
      # Start the MLST columns hidden; the picker toggles them. This keeps the
      # initial DT state consistent with the picker (which starts them
      # deselected).
      if (length(hidden_idx)) {
        column_defs <- c(
          column_defs,
          list(list(visible = FALSE, targets = hidden_idx))
        )
      }

      # The two-row category header. Skipped in the degenerate case where no
      # column has a category at all, which leaves DT's default single-row
      # header (driven by `colnames` below).
      groups <- header_groups()
      sketch <- if (length(groups)) {
        .grouped_header_sketch(cols, header_labels(), groups)
      }

      # DT's `container` builds its own default header when the argument is
      # missing entirely - passing an explicit NULL is not the same thing and
      # breaks rendering, so it is only added to the call when there is a
      # sketch to use (do.call, rather than passing container = sketch below).
      dt_args <- list(
        df,
        rownames = FALSE,
        colnames = field_labels_for(cols),
        filter = "top",
        # Drop the default "display" class's zebra striping (keep borders /
        # hover / sortable) so the cells aren't tinted per row.
        class = "row-border order-column",
        editable = list(
          target = "cell",
          disable = list(columns = readonly_idx),
          # Native inputs per column type, the same way database_custom.R does
          # it: DT builds a <input type="date"> / <input type="number"> itself
          # and its own blur/Escape teardown keeps working. (This replaces a
          # MutationObserver that retyped DT's text input after the fact —
          # which had to re-parse and re-focus the input, and only ever knew
          # about the one hardcoded collection-date column.)
          numeric = edit_numeric_idx,
          date = edit_date_idx
        ),
        selection = "none",
        extensions = "FixedColumns",
        options = list(
          dom = "ti",
          paging = FALSE,
          scrollX = TRUE,
          scrollY = "1px",
          scrollCollapse = TRUE,
          fixedColumns = list(leftColumns = 1),
          columnDefs = column_defs,
          # Tag rows for isolates added this session (data[0] is the isolate,
          # since rownames = FALSE) so main.scss can tint them.
          rowCallback = DT::JS(sprintf(
            "function(row, data) {
               var iso = String(data[0]);
               var newSet = %s;
               var importedSet = %s;
               if (newSet.indexOf(iso) !== -1) {
                 row.classList.add('session-new-row');
               } else if (importedSet.indexOf(iso) !== -1) {
                 row.classList.add('session-import-row');
               }
             }",
            jsonlite::toJSON(new_isolates),
            jsonlite::toJSON(imported_isolates)
          )),
          initComplete = DT::JS(
            "function(settings) {
              var api = this.api();
              var tableNode = api.table().node();

              $(tableNode).on('keyup', 'input', function(e) {
                if (e.key === 'Enter') this.blur();
              });

              // A native date input only accepts YYYY-MM-DD, so a stored value
              // in any other shape (legacy rows, an import) lands in the editor
              // blank — and blurring that blank would commit it, wiping the
              // date. Recover it from the cell's own data, which DT has not
              // touched yet, and offer it in the form the input can hold.
              $(tableNode).on('focusin', 'input[type=date]', function() {
                // Native date inputs have no format option; the displayed
                // component order/separator follows the browser locale of the
                // element's `lang`. ja renders yyyy/mm/dd, matching the rest
                // of the app's date display.
                this.setAttribute('lang', 'ja');
                if (this.value) return;
                var td = $(this).closest('td');
                if (!td.length) return;
                var raw = api.cell(td[0]).data();
                raw = (raw === null || raw === undefined) ? '' : String(raw).trim();
                if (!raw) return;
                var d = new Date(raw);
                if (!isNaN(d.getTime())) this.value = d.toISOString().split('T')[0];
              });

              // Re-measure once the picker has finished changing columns.
              //
              // DataTables fires column-visibility.dt once *per column*, not
              // once per change, so toggling a whole category announces 33 of
              // them. Running the pass below on each one is quadratic in the
              // worst way: columns.adjust() re-measures every column against a
              // table that (paging = FALSE) has every row in the DOM, and each
              // adjust in turn fires column-sizing.dt, which is what
              // FixedColumns rebuilds its pinned column from - walking every
              // cell. Toggling the AMR category that way blocked the main
              // thread for ~45 seconds and had Chrome offering \"Page
              // Unresponsive\".
              //
              // The whole burst is synchronous, so a zero-delay timer coalesces
              // it into the single pass this always meant to be: the first
              // event schedules, the rest are absorbed, and the measure runs
              // once with every column already in its final state.
              var visibilityRedraw = null;
              api.on('column-visibility.dt', function() {
                if (visibilityRedraw) return;
                visibilityRedraw = setTimeout(function() {
                  visibilityRedraw = null;
                  api.columns.adjust().draw(false);
                }, 0);
              });

              // Flex-align DataTables' own \"Showing x to y of z entries\" info
              // div (dom: 'ti') against whatever legend infoCallback below adds
              // to it, so info text stays left and the legend(s) sit on the
              // right. Added once here rather than per-draw: DataTables
              // replaces the info div's *content* on every draw (see
              // infoCallback), but never touches its class list, so this
              // survives redraws (paging, filtering, column visibility) fine.
              $(tableNode)
                .closest('.dataTables_wrapper')
                .children('.dataTables_info')
                .addClass('session-legend-row');
            }"
          ),
          # DataTables rebuilds the info div's *content* from scratch on every
          # draw (paging, filtering, and - critically - the columns.adjust()
          # redraw the column-visibility.dt handler above triggers when the
          # picker toggles a column). Appending the legend as a DOM child in
          # initComplete only survives the first draw; infoCallback is called
          # on every draw and its return value becomes the div's new content,
          # so the legend has to be rebuilt here each time to survive column
          # toggling. The legend markup itself is static (built once, in R,
          # as legend_html above), not templated with each draw's own
          # start/end/total shown in `pre`.
          infoCallback = DT::JS(sprintf(
            "function(settings, start, end, max, total, pre) {
              return pre + %s;
            }",
            jsonlite::toJSON(legend_html, auto_unbox = TRUE)
          ))
        )
      )
      if (!is.null(sketch)) {
        dt_args$container <- sketch
      }
      if (length(called_at_idx)) {
        dt_args$options$order <- list(list(called_at_idx, "desc"))
      }
      do.call(datatable, dt_args)
    })

    proxy <- dataTableProxy("metadata_table", session = session)

    # Apply cell edit to in-memory state and push to table without full re-render.
    #
    # Two destinations behind one table: a metadata column is edited in place in
    # State$data and buffered in State$metadata_dirty for save_metadata_table(),
    # while a custom-variable column is validated against its declared type
    # first and buffered in State$custom_dirty for save_custom_values(). Both
    # dirty buffers are tidy one-row-per-cell diffs, isolate/column/value, so a
    # save writes exactly the cells the user touched rather than a whole
    # snapshot - see save_metadata_table()'s docs for why that distinction is
    # what keeps two sessions editing the same database from clobbering each
    # other. Mirrors the same flow in database_custom.R, which is why the
    # rejection and no-op handling below read the same.
    #
    # replaceData() is skipped for a genuine no-op: DT's own blur handler fires
    # cell_edit for a no-op click on an empty cell too (JS null vs. the
    # editor's blur value "" never compare equal), so calling it unconditionally
    # sent a round trip on virtually every click into a not-yet-filled cell. If
    # the user had already clicked into a *different* cell to start editing it
    # by the time that round trip's replaceData() landed, its table-wide
    # redraw tore that cell's still-open editor out from under them — so the
    # first click on it appeared to do nothing and a second one was needed.
    # See the same fix in database_custom.R's values_table_cell_edit observer.
    #
    # But for an *accepted* edit, replaceData() always runs, even when the
    # value R stored is byte-identical to what the client already shows: DT's
    # blur handler updates its cell's data client-side (cell().data()) without
    # ever calling draw(), so the table's per-row sort/filter caches are never
    # rebuilt for that edit. They stay built from the pre-edit (blank) data
    # until something forces a real draw. Skipping replaceData() here used to
    # be exactly that shortcut - and its absence meant the *next* column-header
    # sort click was the first thing to force a draw, at which point DataTables
    # rebuilt those stale caches and blanked every cell edited since the table
    # last rendered, even though the save to the database already had the
    # right values (a full reload always showed them correctly, since that
    # goes through a real render). See conversation 2026-07-28 for the repro.
    observeEvent(input$metadata_table_cell_edit, {
      req(is.data.frame(State$data))
      info <- input$metadata_table_cell_edit
      col <- names(State$data)[info$col + 1L]

      if (!col %in% custom_cols()) {
        old_value <- State$data[[col]][[info$row]]
        State$data <- editData(State$data, info, rownames = FALSE)
        new_value <- State$data[[col]][[info$row]]

        unchanged <- if (is.na(old_value) || is.na(new_value)) {
          is.na(old_value) && is.na(new_value)
        } else {
          old_value == new_value
        }
        if (isTRUE(unchanged)) {
          return()
        }

        entry <- data.frame(
          isolate = State$data$isolate[[info$row]],
          column = col,
          value = new_value,
          stringsAsFactors = FALSE
        )
        # One row per cell: re-editing a cell before saving replaces its
        # pending value rather than queuing a second write for it.
        keep <- if (is.null(State$metadata_dirty)) {
          NULL
        } else {
          State$metadata_dirty[
            !(State$metadata_dirty$isolate == entry$isolate &
              State$metadata_dirty$column == entry$column),
            ,
            drop = FALSE
          ]
        }
        State$metadata_dirty <- rbind(keep, entry)

        State$pending <- TRUE
        replaceData(proxy, State$data, resetPaging = FALSE, rownames = FALSE)
        return()
      }

      defs <- custom_defs()
      def <- defs[defs$column == col, , drop = FALSE]
      req(nrow(def) == 1)

      checked <- coerce_custom_value(
        info$value,
        def$type[[1]],
        def$levels[[1]]
      )

      if (!isTRUE(checked$ok)) {
        replaceData(proxy, State$data, resetPaging = FALSE, rownames = FALSE)
        showNotification(
          paste0(
            "\"",
            info$value,
            "\" is not a valid ",
            tolower(CUSTOM_TYPES[[def$type[[1]]]]),
            " value. ",
            checked$reason
          ),
          type = "warning",
          duration = 6
        )
        return()
      }

      stored <- checked$value
      # Keep the column's R type so a numeric variable still sorts as a number.
      new_value <- if (def$type[[1]] %in% NUMERIC_TYPES) {
        suppressWarnings(as.numeric(stored))
      } else {
        stored
      }

      # DT fires cell_edit whenever the input's value differs from the cell's
      # raw JS data, and an empty custom cell is NA (`null` in JS) while the
      # editor always reads back "" on blur — so a click-in/click-out with no
      # typing would otherwise mark the table dirty. Compare the real values.
      old_value <- State$data[[col]][[info$row]]
      unchanged <- if (is.na(old_value) || is.na(new_value)) {
        is.na(old_value) && is.na(new_value)
      } else {
        old_value == new_value
      }
      if (isTRUE(unchanged)) {
        return()
      }

      State$data[[col]][info$row] <- new_value

      entry <- data.frame(
        field_id = def$id[[1]],
        isolate = State$data$isolate[[info$row]],
        value = stored,
        stringsAsFactors = FALSE
      )
      # One row per cell: re-editing a cell replaces its pending value.
      keep <- if (is.null(State$custom_dirty)) {
        NULL
      } else {
        State$custom_dirty[
          !(State$custom_dirty$field_id == entry$field_id &
            State$custom_dirty$isolate == entry$isolate),
          ,
          drop = FALSE
        ]
      }
      State$custom_dirty <- rbind(keep, entry)
      State$pending <- TRUE
      replaceData(proxy, State$data, resetPaging = FALSE, rownames = FALSE)
    })

    # Enable/disable both action buttons together based on pending edits
    observeEvent(State$pending, {
      if (isTRUE(State$pending)) {
        enable("save")
        addClass("save", "btn-attention")
        enable("discard")
      } else {
        disable("save")
        removeClass("save", "btn-attention")
        disable("discard")
      }
    })

    observeEvent(input$save, {
      # observeEvent() handlers run isolated, so this read of the shared store
      # does not add a reactive dependency here. Used below only to decide
      # which pending edits still name a real isolate and to report how the
      # isolate list moved while this grid's edits were pending - the writes
      # themselves no longer need a fresh full table (see save_metadata_table()).
      current <- store$metadata()

      # An isolate removed elsewhere while its edit was still pending is
      # dropped rather than sent to save_metadata_table() (which would ignore
      # it anyway), so it can be reported below the same way a dropped custom
      # edit already is.
      dirty_meta <- State$metadata_dirty
      n_meta <- 0L
      if (is.data.frame(dirty_meta) && nrow(dirty_meta)) {
        live <- db_events$reconcile_names(dirty_meta$isolate, current$isolate)
        dirty_meta <- dirty_meta[
          !(dirty_meta$isolate %in% live$dropped),
          ,
          drop = FALSE
        ]
        n_meta <- nrow(dirty_meta)
        if (n_meta) {
          save_metadata_table(db_path(), dirty_meta)
        }
      }
      State$metadata_dirty <- NULL

      wrote_custom <- is.data.frame(State$custom_dirty) &&
        nrow(State$custom_dirty)
      if (wrote_custom) {
        # A custom value keyed to an isolate that no longer exists would be an
        # orphan row no view can reach, so drop those the same way.
        dirty <- State$custom_dirty
        live <- db_events$reconcile_names(dirty$isolate, current$isolate)
        save_custom_values(db_path(), dirty[!(dirty$isolate %in% live$dropped), ])
        State$custom_dirty <- NULL
      }

      State$pending <- FALSE

      # Announce before reporting: only the domains actually written to.
      # Clearing State$pending first means this grid's own observer above now
      # accepts the re-read it has been skipping, so a save also catches the
      # grid up on whatever it ignored meanwhile.
      domains <- c(if (n_meta) "metadata", if (wrote_custom) "custom_fields")
      if (length(domains)) {
        do.call(db_events$bump, c(list(db_rev), as.list(domains)))
      }

      # The isolate list may have moved elsewhere while these edits were
      # pending; report it the same way a reconciled write used to require,
      # even though the write itself no longer needs the reconciled table -
      # .reconcile_metadata() is reused here purely to compute that delta.
      strip <- c(mlst_cols(), amr_cols(), custom_cols())
      edited <- State$data[, setdiff(names(State$data), strip), drop = FALSE]
      to_save <- .reconcile_metadata(edited, current)
      dropped <- setdiff(edited$isolate, to_save$isolate)
      appeared <- setdiff(to_save$isolate, edited$isolate)
      showNotification(
        if (length(dropped) || length(appeared)) {
          tagList(
            tags$strong("Database changes saved."),
            tags$br(),
            sprintf(
              paste(
                "The isolate list moved while you were editing:",
                "%d added, %d removed elsewhere. Your edits were applied to",
                "the isolates that are still present."
              ),
              length(appeared),
              length(dropped)
            )
          )
        } else {
          "Database changes saved."
        },
        type = "message",
        duration = if (length(dropped) || length(appeared)) 10 else 3
      )
    })

    # Discard: re-fetch from DB and push back to the table without re-render.
    # Bumps reload_token first because metadata_base() may be holding a read
    # from before a change this grid skipped while edits were pending —
    # discarding has to land on the database as it is now, not as it was.
    observeEvent(input$discard, {
      State$metadata_dirty <- NULL
      State$custom_dirty <- NULL
      State$pending <- FALSE
      reload_token(isolate(reload_token()) + 1L)
      fresh <- metadata_base()
      State$data <- fresh
      replaceData(proxy, fresh, resetPaging = FALSE, rownames = FALSE)
    })

    observeEvent(
      input$remove_picker,
      {
        if (length(input$remove_picker) > 0) {
          enable("remove_btn")
        } else {
          disable("remove_btn")
        }
      },
      ignoreNULL = FALSE
    )

    observeEvent(input$remove_btn, {
      isolates <- input$remove_picker
      req(length(isolates) > 0)
      showModal(modalDialog(
        title = "Remove isolates",
        div(
          class = "custom-modal",
          # Removing reloads the grid from the database (see confirm_remove),
          # which - unlike a save or an explicit discard - wipes any edit still
          # sitting unsaved in it. Warn here rather than after the fact, the
          # same way main.R warns before "Reload Database".
          if (isTRUE(State$pending)) {
            div(
              class = "alert alert-warning p-2 mb-2 small",
              icon("triangle-exclamation"),
              " ",
              tags$strong("Unsaved edits will be lost."),
              " Removing isolates reloads this table from the database;",
              " save or discard your changes first if you want to keep them."
            )
          },
          if (length(isolates) == 1) {
            div(
              div('Permanently remove'),
              div(class = "dest-label is-empty", isolates),
              div('from the database? This cannot be undone.')
            )
          } else {
            div(
              style = "margin-bottom: 1rem;",
              paste0(
                "Permanently remove ",
                length(isolates),
                " isolates from the database? This cannot be undone."
              )
            )
          },
          div("Delete also cgMLST allele sequences?"),
          radioButtons(
            ns("keep_alleles"),
            label = NULL,
            choices = c(
              "Keep allele sequences" = TRUE,
              "Delete allele sequences" = FALSE
            ),
            selected = TRUE
          ),
          tags$small(
            class = "dest-label is-empty",
            paste(
              "Optionally keep removed isolate(s)' allele sequences so newly added or imported",
              "isolates re-use the same allele identity. Deleting will",
              "remove the alleles no longer referenced by any remaining isolate."
            )
          )
        ),
        footer = tagList(
          modalButton("Cancel"),
          actionButton(ns("confirm_remove"), "Remove", class = "btn-danger")
        ),
        easyClose = TRUE
      ))
    })

    observeEvent(input$confirm_remove, {
      isolates <- input$remove_picker
      keep_alleles <- isTRUE(input$keep_alleles)
      removeModal()
      remove_waiter$show()
      on.exit(remove_waiter$hide())
      remove_isolates(db_path(), isolates, keep_alleles = keep_alleles)
      # Drop them from the "was already here" baseline too, so bringing the same
      # isolate back later this session - re-typed or imported - still reads as
      # an addition. Without this the set-diff in session_added() returns to
      # exactly the baseline and the row is highlighted as nothing at all.
      baseline_isolates <<- setdiff(baseline_isolates, isolates)
      reload_token(reload_token() + 1L)
      # The widest-reaching write in the app: remove_isolates() deletes from
      # mlst, metadata and every isolate-keyed table (classical MLST, AMR,
      # custom values, genome hashes), and with keep_alleles = FALSE from
      # sequences and hashes as well. Everything except staged sets and saved
      # Analyses is affected, and those two only hold *names* of isolates, which
      # their own readers reconcile.
      db_events$bump(
        db_rev,
        "isolates",
        "metadata",
        "custom_fields",
        "amr",
        "schema"
      )
      showNotification(
        paste0(length(isolates), " isolate(s) removed from the database."),
        type = "message",
        duration = 3
      )
    })

    # Column visibility. `input$col_picker` carries raw column names for every
    # ordinary field, but an AMR group is picked as a single synthetic token
    # (AMR_GROUP_TOKEN_PREFIX + drug class - see col_picker_ui): resolve those
    # back to the real gene columns they stand for before looking up DT
    # indices against the frame's actual column order.
    observeEvent(
      input$col_picker,
      {
        df <- metadata_base()
        req(is.data.frame(df))

        selected <- input$col_picker
        if (!is.character(selected)) {
          selected <- character(0)
        }
        is_group_token <- startsWith(selected, AMR_GROUP_TOKEN_PREFIX)
        selected_groups <- sub(
          paste0("^", AMR_GROUP_TOKEN_PREFIX),
          "",
          selected[is_group_token]
        )

        groups <- amr_gene_groups()
        gene_cols <- names(groups)
        shown_genes <- gene_cols[groups %in% selected_groups]

        optional <- setdiff(optional_cols(), gene_cols)
        selected_cols <- selected[!is_group_token]

        show_idx <- .dt_idx(
          names(df),
          c(intersect(optional, selected_cols), shown_genes)
        )
        hide_idx <- .dt_idx(
          names(df),
          c(setdiff(optional, selected_cols), setdiff(gene_cols, shown_genes))
        )

        # Deliberately not showCols()/hideCols(): each ends in its own
        # columns.adjust(), and that re-measure is the expensive part of a
        # visibility change on a table this wide with every row in the DOM
        # (paging = FALSE). Sending both sets together lets the client make the
        # DOM changes first and measure once — see app/js/dt-column-visibility.js.
        session$sendCustomMessage(
          "pt-dt-column-visibility",
          list(
            id = session$ns("metadata_table"),
            show = show_idx,
            hide = hide_idx
          )
        )
      },
      ignoreNULL = FALSE,
      ignoreInit = TRUE
    )

    list(pending = reactive(State$pending))
  })
}
