box::use(
  shiny[isolate, observe, reactive, reactiveVal, testServer],
  testthat[
    expect_equal,
    expect_false,
    expect_identical,
    expect_setequal,
    expect_true,
    test_that
  ],
  stats[setNames],
  withr[local_tempdir],
)
box::use(
  app / logic / amr_plot,
  app / logic / field_profile[field_profiles],
  app / logic / tree_plot,
  app / view / visualization_tree,
)

impl <- attr(visualization_tree, "namespace")

# Four isolates is the smallest tree the distance matrix will build (>= 3).
alleles <- function() {
  list(
    ref = ref_alleles(),
    A = c(g1 = seqv("A1"), g2 = seqv("A2"), g3 = seqv("A3")),
    B = c(g1 = seqv("B1"), g2 = seqv("A2"), g3 = seqv("A3")),
    C = c(g1 = seqv("C1"), g2 = seqv("C2"), g3 = seqv("A3")),
    D = c(g1 = seqv("D1"), g2 = seqv("C2"), g3 = seqv("D3"))
  )
}

fixture_db <- function(dir) {
  db <- file.path(dir, "local.db")
  build_db(db, alleles(), metadata = meta_df(c("A", "B", "C", "D")))
  db
}

# testServer starts every input NULL, and the ggtree build dereferences most of
# them (a NULL size reaches unit() and errors), so stand the whole control
# panel up at its coded defaults.
set_tree_inputs <- function(session) {
  session$setInputs(
    nj_root_isolate = "Automatic",
    nj_layout = "rectangular",
    nj_color = "#000000",
    nj_bg = "#ffffff",
    nj_tiplab_show = TRUE,
    nj_tiplab = "isolate",
    nj_tiplab_size = 4,
    nj_tiplab_color = "#000000",
    nj_mapping_show = FALSE,
    nj_show_branch_label = FALSE,
    nj_branch_size = 4,
    nj_branch_color = "#000000",
    nj_tippoint_show = FALSE,
    nj_tippoint_alpha = 0.5,
    nj_tippoint_size = 4,
    nj_tippoint_color = "#3A4657",
    nj_tippoint_shape = 16,
    nj_tipcolor_mapping_show = FALSE,
    nj_tipshape_mapping_show = FALSE,
    nj_nodelabel_show = FALSE,
    nj_clade_scale = "#D0F221",
    nj_heatmap_show = FALSE,
    nj_rootedge_show = FALSE,
    nj_treescale_show = FALSE,
    nj_axis_show = TRUE,
    nj_aspect_ratio = 0.6
  )
}

# --- The fitted-control mirrors ----------------------------------------------

test_that("a fitted value reaches the plot without waiting for the input", {
  testServer(
    visualization_tree$server,
    args = list(plot_type = reactiveVal("Tree")),
    {
      set_tree_inputs(session)
      session$flushReact()

      # What the fit does at Generate. The input still holds 4 — in the browser
      # the new value has only just been sent.
      set_fitted("nj_tiplab_size", 2.3)
      session$flushReact()

      expect_equal(tree_opts()$tiplab_size, 2.3)
      expect_equal(isolate(input$nj_tiplab_size), 4)
    }
  )
})

test_that("the browser echoing a fitted value back does not redraw", {
  testServer(
    visualization_tree$server,
    args = list(plot_type = reactiveVal("Tree")),
    {
      set_tree_inputs(session)
      session$flushReact()

      draws <- 0L
      observe({
        tree_opts()
        draws <<- draws + 1L
      })
      session$flushReact()
      expect_identical(draws, 1L)

      # The fit: one redraw, with the right values.
      set_fitted("nj_tiplab_size", 2.3)
      session$flushReact()
      expect_identical(draws, 2L)

      # The echo of the updateSliderInput that went with it. This is the second
      # and third render a Generate used to log.
      session$setInputs(nj_tiplab_size = 2.3)
      session$flushReact()
      expect_identical(draws, 2L)

      # A user drag is a real change and must still redraw.
      session$setInputs(nj_tiplab_size = 7)
      session$flushReact()
      expect_identical(draws, 3L)
      expect_equal(tree_opts()$tiplab_size, 7)
    }
  )
})

test_that("adding a mapping layer redraws once, and only when it changes", {
  # The old per-aesthetic pickers were rewritten on every Generate whether or
  # not their switch was on, and each rewrite echoed back from the browser a
  # flush later — a second draw for a mapping the plot was not drawing. Layers
  # are written only by a user action, so there is no server-sent value left to
  # echo; this test pins that property to the new shape.
  meta <- data.frame(
    isolate = sprintf("ISO-%02d", 1:8),
    ward = sprintf("W-%02d", 1:8),
    purpose = rep(c("outbreak", "surveillance"), 4),
    stringsAsFactors = FALSE
  )

  testServer(
    visualization_tree$server,
    args = list(
      viz_metadata = reactive(meta),
      field_profiles = reactive(field_profiles(meta)),
      plot_type = reactiveVal("Tree")
    ),
    {
      set_tree_inputs(session)
      session$flushReact()

      draws <- 0L
      observe({
        tree_opts()
        draws <<- draws + 1L
      })
      session$flushReact()
      draws <- 0L

      session$setInputs(nj_layer_add = "purpose")
      session$flushReact()
      expect_identical(draws, 1L)
      expect_identical(length(tree_opts()$layers), 1L)
      expect_equal(tree_opts()$layers[[1]]$field, "purpose")

      # Re-picking the same variable is not a second mapping, and so not a
      # second draw.
      session$setInputs(nj_layer_add = "purpose")
      session$flushReact()
      expect_identical(draws, 1L)

      # A column with a different value per isolate groups nothing, so it has
      # no aesthetic to take and adds no layer — and still must not redraw.
      session$setInputs(nj_layer_add = "ward")
      session$flushReact()
      expect_identical(draws, 1L)
      expect_identical(length(tree_opts()$layers), 1L)
    }
  )
})

test_that("a mapping layer can be removed again", {
  meta <- data.frame(
    isolate = sprintf("ISO-%02d", 1:8),
    purpose = rep(c("outbreak", "surveillance"), 4),
    stringsAsFactors = FALSE
  )

  testServer(
    visualization_tree$server,
    args = list(
      viz_metadata = reactive(meta),
      field_profiles = reactive(field_profiles(meta)),
      plot_type = reactiveVal("Tree")
    ),
    {
      set_tree_inputs(session)
      session$flushReact()

      session$setInputs(nj_layer_add = "purpose")
      session$flushReact()
      id <- tree_opts()$layers[[1]]$id

      session$setInputs(nj_layer_delete = id)
      session$flushReact()
      expect_identical(length(tree_opts()$layers), 0L)
    }
  )
})

test_that("Generate resolving a select costs no extra draw", {
  # The reported case, as the console reported it: one Generate logged
  # "tree rebuild: tiplab" and "tree rebuild: tiles" *after* it had already
  # drawn. Both came from updates the server sends to the browser, which reach
  # input$ only on the echo — a flush after the tree was drawn from the stale
  # value. Resolving them into the mirrors instead puts them in the same draw
  # as the tree they belong to.
  dir <- local_tempdir()
  db <- fixture_db(dir)
  generate <- reactiveVal(0L)
  meta <- data.frame(isolate = c("A", "B", "C", "D"), stringsAsFactors = FALSE)

  testServer(
    visualization_tree$server,
    args = list(
      db_path = reactive(db),
      viz_metadata = reactive(meta),
      generate = generate,
      plot_type = reactiveVal("Tree")
    ),
    {
      set_tree_inputs(session)
      # The label source still on the placeholder the UI ships with, which is
      # not a column of this database — what Generate has to resolve.
      session$setInputs(nj_tiplab = "Species")
      session$flushReact()

      draws <- 0L
      observe({
        tree_plot_built()
        draws <<- draws + 1L
      })
      session$flushReact()
      draws <- 0L

      generate(1L)
      session$flushReact()

      expect_identical(draws, 1L)
      expect_equal(tree_opts()$tiplab, "isolate")
      # Nothing is mapped, so there is nothing for the mappings to contribute.
      expect_identical(length(tree_opts()$layers), 0L)
    }
  )
})

test_that("the label source carries the database's own fields before Generate", {
  # The reported bug: every variable picker listed the placeholder names
  # viz_helpers declares them with — "Isolation Date", "Host", "Country" — which
  # are columns of no real database, and only a Generate replaced them.
  meta <- data.frame(
    isolate = sprintf("ISO-%02d", 1:8),
    organism = rep("P. aeruginosa", 8),
    purpose = rep(c("outbreak", "surveillance"), 4),
    stringsAsFactors = FALSE
  )

  testServer(
    visualization_tree$server,
    args = list(
      viz_metadata = reactive(meta),
      field_profiles = reactive(field_profiles(meta)),
      plot_type = reactiveVal("Tree")
    ),
    {
      set_tree_inputs(session)
      session$flushReact()

      # No generate() at all.
      expect_equal(isolate(fitted$nj_tiplab), "isolate")
    }
  )
})

test_that("a mapping is offered every column, and picks the aesthetic itself", {
  # The behaviour the rewrite is for: the shape picker used to *hide* any
  # column with more than six levels, so a user looking for `country` found
  # nothing and no reason why. Now every column is profiled and offered, and
  # the engine — not the user — decides what a column can drive.
  meta <- data.frame(
    isolate = sprintf("ISO-%02d", 1:40),
    organism = rep("P. aeruginosa", 40),
    purpose = rep(c("outbreak", "surveillance", "screening", "referral"), 10),
    country = sprintf("C%02d", seq_len(40) %% 20),
    stringsAsFactors = FALSE
  )

  testServer(
    visualization_tree$server,
    args = list(
      viz_metadata = reactive(meta),
      field_profiles = reactive(field_profiles(meta)),
      plot_type = reactiveVal("Tree")
    ),
    {
      set_tree_inputs(session)
      session$flushReact()

      # Four levels fits the six-shape ceiling, so it takes the scarce
      # aesthetic and leaves the colours free.
      session$setInputs(nj_layer_add = "purpose")
      session$flushReact()
      expect_equal(tree_opts()$layers[[1]]$aesthetic, "tippoint_shape")

      # Twenty levels cannot be shapes and would be noise as text colour, so it
      # goes to a strip with a generated palette — no ColorBrewer overflow.
      session$setInputs(nj_layer_add = "country")
      session$flushReact()
      expect_equal(tree_opts()$layers[[2]]$aesthetic, "tile")
      expect_equal(tree_opts()$layers[[2]]$palette, "viridis")

      # A mapping onto the tip points is meaningless while they are hidden, and
      # that switch lives in another tab — so it comes on with the mapping.
      expect_true(isolate(fitted$nj_tippoint_show))

      # And goes off again when the mapping that needed it is deleted: the
      # points were the mapping's doing, so leaving them behind put dots on the
      # tree that nothing was mapped to.
      session$setInputs(nj_layer_delete = "L1")
      session$flushReact()
      expect_length(tree_opts()$layers, 1L)
      expect_false(isolate(fitted$nj_tippoint_show))
    }
  )
})

test_that("a mapping keeps its tip points drawn even if the switch says off", {
  # The sidebar locks "Show tip points" on while a mapping is drawn on them,
  # but the lock lives in the browser. The plot enforces the same rule itself,
  # so a switch that reads FALSE — a restored Analysis writing a saved "off"
  # into the mirror after the mapping is already there — cannot leave the tree
  # with a legend for marks it never drew.
  meta <- data.frame(
    isolate = sprintf("ISO-%02d", 1:12),
    purpose = rep(c("outbreak", "surveillance", "screening"), 4),
    stringsAsFactors = FALSE
  )

  testServer(
    visualization_tree$server,
    args = list(
      viz_metadata = reactive(meta),
      field_profiles = reactive(field_profiles(meta)),
      plot_type = reactiveVal("Tree")
    ),
    {
      set_tree_inputs(session)
      session$setInputs(nj_layer_add = "purpose")
      session$flushReact()
      expect_equal(tree_opts()$layers[[1]]$aesthetic, "tippoint_shape")

      session$setInputs(nj_tippoint_show = FALSE)
      session$flushReact()
      expect_false(isolate(fitted$nj_tippoint_show))
      expect_true(tree_opts()$tippoint_show)
    }
  )
})

test_that("tip points the user switched on survive their mapping", {
  # The other half of the same rule: only the points the mapping turned on are
  # turned back off. A user who opened Elements and asked for tip points keeps
  # them when a tip-point mapping comes and goes.
  meta <- data.frame(
    isolate = sprintf("ISO-%02d", 1:12),
    purpose = rep(c("outbreak", "surveillance", "screening"), 4),
    stringsAsFactors = FALSE
  )

  testServer(
    visualization_tree$server,
    args = list(
      viz_metadata = reactive(meta),
      field_profiles = reactive(field_profiles(meta)),
      plot_type = reactiveVal("Tree")
    ),
    {
      set_tree_inputs(session)
      session$setInputs(nj_tippoint_show = TRUE)
      session$flushReact()

      session$setInputs(nj_layer_add = "purpose")
      session$flushReact()
      expect_equal(tree_opts()$layers[[1]]$aesthetic, "tippoint_shape")

      session$setInputs(nj_layer_delete = "L1")
      session$flushReact()
      expect_length(tree_opts()$layers, 0L)
      expect_true(isolate(fitted$nj_tippoint_show))
    }
  )
})

test_that("the tree's own width is fixed, not taken from the browser", {
  # renderPlot re-executes whenever the output's pixel size changes, so a canvas
  # sized from session$clientData redraws the tree — a second of work for a few
  # hundred tips — on every width the browser reports, which is what drew one
  # Generate three times. The width the plot, the label reserve and the export
  # all work from is a constant instead. (A resize cannot be simulated here:
  # testServer's session refuses writes to clientData.)
  testServer(
    visualization_tree$server,
    args = list(plot_type = reactiveVal("Tree")),
    {
      set_tree_inputs(session)
      session$flushReact()
      expect_equal(plot_width_in(), impl$TREE_PANEL_IN)
      expect_equal(tree_opts()$width_in, impl$TREE_PANEL_IN)
    }
  )
})

test_that("the canvas grows for what is drawn beside the tree", {
  # The reported failure: an annotation or a legend was paid for out of the
  # tree's own width, so switching a heatmap on turned the tree into a hairline.
  # The tree's budget is fixed; the canvas is what moves.
  meta <- data.frame(
    isolate = sprintf("ISO-%02d", 1:8),
    purpose = rep(c("outbreak", "surveillance"), 4),
    amr_gly = rep(c("aac", ""), 4),
    stringsAsFactors = FALSE
  )
  attr(meta, "amr_cols") <- "amr_gly"

  testServer(
    visualization_tree$server,
    args = list(
      viz_metadata = reactive(meta),
      field_profiles = reactive(field_profiles(meta, amr_cols = "amr_gly")),
      plot_type = reactiveVal("Tree")
    ),
    {
      set_tree_inputs(session)
      session$flushReact()
      bare <- plot_canvas()
      expect_equal(bare$canvas_in, impl$TREE_PANEL_IN)

      # A mapping adds a legend, and a tile strip beside the tips.
      session$setInputs(nj_layer_add = "purpose")
      session$flushReact()
      mapped <- plot_canvas()
      expect_gt(mapped$canvas_in, bare$canvas_in)
      # The tree itself keeps its width, and the height keeps its row pitch.
      expect_equal(tree_opts()$width_in, impl$TREE_PANEL_IN)
      expect_equal(mapped$height_in, bare$height_in)
    }
  )
})

test_that("the canvas cannot grow without limit", {
  meta <- data.frame(
    isolate = sprintf("ISO-%02d", 1:8),
    v = rep(c("an extremely long category label", "another very long one"), 4),
    stringsAsFactors = FALSE
  )

  testServer(
    visualization_tree$server,
    args = list(
      viz_metadata = reactive(meta),
      field_profiles = reactive(field_profiles(meta)),
      plot_type = reactiveVal("Tree")
    ),
    {
      set_tree_inputs(session)
      session$setInputs(nj_layer_add = "v")
      session$flushReact()
      expect_lte(
        plot_canvas()$canvas_in,
        impl$TREE_PANEL_IN * impl$CANVAS_MAX_FACTOR
      )
    }
  )
})

test_that("Generate draws the tree exactly once, already fitted", {
  dir <- local_tempdir()
  db <- fixture_db(dir)
  generate <- reactiveVal(0L)
  selected <- reactiveVal(c("A", "B", "C", "D"))
  meta <- data.frame(isolate = c("A", "B", "C", "D"), stringsAsFactors = FALSE)

  testServer(
    visualization_tree$server,
    args = list(
      db_path = reactive(db),
      viz_metadata = reactive(meta),
      selected_isolates = selected,
      generate = generate,
      plot_type = reactiveVal("Tree")
    ),
    {
      set_tree_inputs(session)
      session$flushReact()

      # The actual ggtree build — what the "Rendering … tree_plot" lines count.
      draws <- 0L
      observe({
        tree_plot_built()
        draws <<- draws + 1L
      })
      session$flushReact()
      # Nothing before Generate. The left sidebar is a form the user submits, so
      # a freshly opened tab shows its "press Generate" prompt rather than a tree
      # nobody asked for — and does not spend a second computing one.
      expect_identical(draws, 0L)
      expect_false(generated())

      # Generate over an isolate set: a tree and a fit, so exactly one draw, with
      # the fitted values already in it. This is where three used to be logged,
      # one per control echoing back.
      selected(c("A", "B", "C"))
      generate(1L)
      session$flushReact()

      expect_identical(draws, 1L)
      expect_true(generated())
      expect_identical(length(tree_obj()$tip.label), 3L)

      # Three tips on the fixed canvas: a squat plot with large labels.
      opts <- tree_opts()
      expect_equal(opts$tiplab_size, isolate(fitted$nj_tiplab_size))
      expect_true(opts$tiplab_size > 4)
      expect_equal(isolate(fitted$nj_aspect_ratio), 0.5)

      # And every echo the browser will now send back changes nothing.
      session$setInputs(
        nj_tiplab_size = isolate(fitted$nj_tiplab_size),
        nj_aspect_ratio = isolate(fitted$nj_aspect_ratio),
        nj_tippoint_size = isolate(fitted$nj_tippoint_size),
        nj_branch_size = isolate(fitted$nj_branch_size)
      )
      session$flushReact()
      expect_identical(draws, 1L)
    }
  )
})

# --- The AMR heatmap controls ------------------------------------------------

# Two resistance genes (one under each of two drug classes) and one virulence
# gene, across 8 isolates. gene_catalog()/amr_matrix() read amr_results (and
# amr_summary for the rollup class) straight off the database now, so the
# heatmap tests need a real one rather than metadata attributes.
amr_fixture_db <- function(dir) {
  path <- file.path(dir, "amr.db")
  con <- DBI::dbConnect(RSQLite::SQLite(), path)
  on.exit(DBI::dbDisconnect(con))
  DBI::dbExecute(
    con,
    "CREATE TABLE amr_results (
       id INTEGER PRIMARY KEY AUTOINCREMENT,
       isolate TEXT, gene_symbol TEXT, element_type TEXT, element_subtype TEXT,
       class TEXT, subclass TEXT, method TEXT, pct_identity REAL,
       pct_coverage REAL, called_at TEXT)"
  )
  DBI::dbExecute(
    con,
    "CREATE TABLE amr_summary (
       id INTEGER PRIMARY KEY AUTOINCREMENT,
       isolate TEXT, section TEXT, drug_class TEXT, genes TEXT, called_at TEXT)"
  )

  # `class` and `drug_class` deliberately disagree, the way the real tables do:
  # AMRFinderPlus writes a broad heading in caps, abritamr re-files the same hit
  # under a finer curated one. The vocabulary switch is the difference between
  # them, so a fixture where they matched could not tell them apart.
  isolates <- sprintf("ISO-%02d", 1:8)
  genes <- list(
    list(
      gene = "blaOXA-2", element = "AMR",
      class = "BETA-LACTAM", drug_class = "Carbapenemase",
      section = "matches", isolates = isolates[c(TRUE, FALSE)]
    ),
    list(
      gene = "aac(6')-Ib3", element = "AMR",
      class = "AMINOGLYCOSIDE", drug_class = "Aminoglycoside (AAC)",
      section = "matches", isolates = isolates[1:4]
    ),
    list(
      gene = "exoU", element = "VIRULENCE",
      class = "VIRULENCE", drug_class = "Secretion system",
      section = "virulence", isolates = isolates[c(TRUE, FALSE)]
    )
  )
  for (g in genes) {
    for (iso in g$isolates) {
      DBI::dbExecute(
        con,
        "INSERT INTO amr_results
           (isolate, gene_symbol, element_type, class, method, called_at)
         VALUES (?, ?, ?, ?, ?, ?)",
        list(iso, g$gene, g$element, g$class, "EXACTX", "2026-01-01")
      )
      DBI::dbExecute(
        con,
        "INSERT INTO amr_summary (isolate, section, drug_class, genes, called_at)
         VALUES (?, ?, ?, ?, ?)",
        list(iso, g$section, g$drug_class, g$gene, "2026-01-01")
      )
    }
  }
  path
}

# What one heatmap card's own gene picker reports. Its input id is per panel,
# so the name has to be built rather than written.
set_genes <- function(session, id, genes) {
  do.call(
    session$setInputs,
    setNames(list(genes), paste0("nj_heatmap_cols_", id))
  )
  session$flushReact()
}

amr_args <- function(db) {
  meta <- data.frame(
    isolate = sprintf("ISO-%02d", 1:8),
    purpose = rep(c("outbreak", "surveillance"), 4),
    stringsAsFactors = FALSE
  )
  list(
    db_path = reactive(db),
    viz_metadata = reactive(meta),
    field_profiles = reactive(field_profiles(meta)),
    plot_type = reactiveVal("Tree")
  )
}

test_that("adding a heatmap draws every gene of its element type", {
  dir <- local_tempdir()
  db <- amr_fixture_db(dir)
  testServer(visualization_tree$server, args = amr_args(db), {
    set_tree_inputs(session)
    session$flushReact()
    expect_identical(length(tree_opts()$heatmaps), 0L)

    session$setInputs(nj_heatmap_add = "AMR")
    session$flushReact()

    hs <- tree_opts()$heatmaps
    expect_identical(length(hs), 1L)
    expect_identical(hs[[1]]$level, "gene")
    expect_identical(hs[[1]]$element, "AMR")
    # Default settings mean every gene of the element type — and only that
    # type: the virulence gene belongs to a panel of its own.
    expect_setequal(hs[[1]]$cols, c("blaOXA-2", "aac(6')-Ib3"))
  })
})

test_that("the virulence element draws its own genes and nothing else", {
  dir <- local_tempdir()
  db <- amr_fixture_db(dir)
  testServer(visualization_tree$server, args = amr_args(db), {
    set_tree_inputs(session)
    session$setInputs(nj_heatmap_add = "VIRULENCE")
    session$flushReact()

    hs <- tree_opts()$heatmaps
    expect_identical(length(hs), 1L)
    expect_identical(hs[[1]]$element, "VIRULENCE")
    expect_identical(hs[[1]]$cols, "exoU")
    expect_identical(hs[[1]]$title, "Virulence genes")
  })
})

test_that("choosing genes narrows a heatmap, and costs exactly one redraw", {
  dir <- local_tempdir()
  db <- amr_fixture_db(dir)
  testServer(visualization_tree$server, args = amr_args(db), {
    set_tree_inputs(session)
    session$setInputs(nj_heatmap_add = "AMR")
    session$flushReact()
    id <- tree_opts()$heatmaps[[1]]$id

    draws <- 0L
    observe({
      tree_opts()
      draws <<- draws + 1L
    })
    session$flushReact()
    draws <- 0L

    # The picker lives in the card, so what the browser reports first is the
    # selection the card was drawn from — not a change, and not a redraw.
    set_genes(session, id, c("blaOXA-2", "aac(6')-Ib3"))
    expect_identical(draws, 0L)

    set_genes(session, id, "blaOXA-2")
    expect_identical(draws, 1L)
    hs <- tree_opts()$heatmaps
    expect_identical(hs[[1]]$cols, "blaOXA-2")
    expect_identical(hs[[1]]$labels, "blaOXA-2")
  })
})

test_that("a heatmap starts on the AMR plot's own colours and unclustered", {
  dir <- local_tempdir()
  db <- amr_fixture_db(dir)
  testServer(visualization_tree$server, args = amr_args(db), {
    set_tree_inputs(session)
    session$setInputs(nj_heatmap_add = "AMR")
    session$flushReact()

    h <- tree_opts()$heatmaps[[1]]
    expect_false(isTRUE(h$cluster))
    expect_identical(h$cluster_distance, amr_plot$AMR_CLUSTER_DISTANCE_DEFAULT)
    expect_identical(h$cluster_method, amr_plot$AMR_CLUSTER_METHOD_DEFAULT)
    expect_identical(
      c(h$color_absent, h$color_partial, h$color_strong, h$color_present),
      unname(tree_plot$AMR_CONFIDENCE_COLORS)
    )
  })
})

test_that("the colour modal sets one panel's colours, not its genes", {
  dir <- local_tempdir()
  db <- amr_fixture_db(dir)
  testServer(visualization_tree$server, args = amr_args(db), {
    set_tree_inputs(session)
    session$setInputs(nj_heatmap_add = "AMR")
    session$flushReact()
    id <- tree_opts()$heatmaps[[1]]$id
    before <- tree_opts()$heatmaps[[1]]$cols

    session$setInputs(nj_heatmap_colors = id)
    session$setInputs(
      nj_heatmap_present = "#112233",
      nj_heatmap_strong = "#445566",
      nj_heatmap_partial = "#778899",
      nj_heatmap_absent = "#AABBCC"
    )
    session$setInputs(nj_heatmap_apply = 1)
    session$flushReact()

    h <- tree_opts()$heatmaps[[1]]
    expect_identical(h$color_present, "#112233")
    expect_identical(h$color_absent, "#AABBCC")
    # The modal declares no gene picker, so applying it leaves the panel's
    # columns exactly where the gene picker left them.
    expect_identical(h$cols, before)
  })
})

test_that("colour is per panel where clustering and labels are shared", {
  # The split the sidebar makes: two panels, one recoloured, and only that one
  # changes \u2014 while a shared switch moves both.
  dir <- local_tempdir()
  db <- amr_fixture_db(dir)
  testServer(visualization_tree$server, args = amr_args(db), {
    set_tree_inputs(session)
    session$setInputs(nj_heatmap_add = "AMR")
    session$flushReact()
    session$setInputs(nj_heatmap_add = "VIRULENCE")
    session$flushReact()
    hs <- tree_opts()$heatmaps
    skip_if_not(length(hs) == 2L, "fixture has only one element type")

    session$setInputs(nj_heatmap_colors = hs[[1]]$id)
    session$setInputs(nj_heatmap_present = "#112233")
    session$setInputs(nj_heatmap_apply = 1)
    session$flushReact()

    after <- tree_opts()$heatmaps
    expect_identical(after[[1]]$color_present, "#112233")
    expect_false(identical(after[[2]]$color_present, "#112233"))

    # And a shared control reaches both.
    session$setInputs(nj_heatmap_cluster = TRUE)
    session$flushReact()
    expect_true(all(vapply(
      tree_opts()$heatmaps, function(h) isTRUE(h$cluster), logical(1)
    )))
  })
})

test_that("the shared switches ride into every panel", {
  dir <- local_tempdir()
  db <- amr_fixture_db(dir)
  testServer(visualization_tree$server, args = amr_args(db), {
    set_tree_inputs(session)
    session$setInputs(nj_heatmap_add = "AMR")
    session$flushReact()

    # Sidebar inputs now, so there is no modal to open and no Apply to press:
    # setting one applies it to the block.
    session$setInputs(
      nj_heatmap_cluster = TRUE,
      nj_heatmap_dend = 15,
      nj_heatmap_gene_names = FALSE,
      nj_heatmap_class_names = FALSE,
      nj_heatmap_strip = FALSE
    )
    session$flushReact()

    h <- tree_opts()$heatmaps[[1]]
    expect_identical(h$dend_depth, 15)
    expect_false(h$show_gene_names)
    expect_false(h$show_class_names)
    expect_false(h$show_class_strip)
  })
})

test_that("a panel added later adopts the shared settings already set", {
  # The reason the shared observer reads the panel list as well as the inputs:
  # a matrix must join the block arranged like the ones beside it, not on the
  # defaults it was born with.
  dir <- local_tempdir()
  db <- amr_fixture_db(dir)
  testServer(visualization_tree$server, args = amr_args(db), {
    set_tree_inputs(session)
    session$setInputs(nj_heatmap_cluster = TRUE, nj_heatmap_gene_names = FALSE)
    session$flushReact()
    session$setInputs(nj_heatmap_add = "AMR")
    session$flushReact()

    h <- tree_opts()$heatmaps[[1]]
    expect_true(h$cluster)
    expect_false(h$show_gene_names)
  })
})

test_that("the element-type label is off by default and switchable to either end", {
  dir <- local_tempdir()
  db <- amr_fixture_db(dir)
  testServer(visualization_tree$server, args = amr_args(db), {
    set_tree_inputs(session)
    session$setInputs(nj_heatmap_add = "AMR")
    session$flushReact()
    expect_false(isTRUE(tree_opts()$heatmaps[[1]]$show_element_type))

    session$setInputs(nj_heatmap_element = TRUE)
    session$flushReact()
    expect_true(tree_opts()$heatmaps[[1]]$show_element_type)
    expect_identical(
      tree_opts()$heatmaps[[1]]$element_pos,
      tree_plot$ELEMENT_POS_DEFAULT
    )

    session$setInputs(nj_heatmap_element_pos = "bottom")
    session$flushReact()
    expect_identical(tree_opts()$heatmaps[[1]]$element_pos, "bottom")
  })
})

test_that("the strip's colour scale is the reader's to pick", {
  # A new panel starts on the shared default so a class matches the AMR tab;
  # the colour modal's scale picker rides into that one panel.
  dir <- local_tempdir()
  db <- amr_fixture_db(dir)
  testServer(visualization_tree$server, args = amr_args(db), {
    set_tree_inputs(session)
    session$setInputs(nj_heatmap_add = "AMR")
    session$flushReact()
    id <- tree_opts()$heatmaps[[1]]$id
    expect_identical(
      tree_opts()$heatmaps[[1]]$strip_scale,
      tree_plot$CLASS_STRIP_SCALE
    )

    session$setInputs(nj_heatmap_colors = id)
    session$setInputs(nj_heatmap_strip_scale = "Dark2")
    session$setInputs(nj_heatmap_apply = 1)
    session$flushReact()

    expect_identical(tree_opts()$heatmaps[[1]]$strip_scale, "Dark2")
  })
})

test_that("the colour mode picks which of the two colour controls is live", {
  # Both sets of values survive the switch \u2014 the modal keeps whichever half
  # was hidden, so flipping back does not lose the swatches.
  dir <- local_tempdir()
  db <- amr_fixture_db(dir)
  testServer(visualization_tree$server, args = amr_args(db), {
    set_tree_inputs(session)
    session$setInputs(nj_heatmap_add = "AMR")
    session$flushReact()
    id <- tree_opts()$heatmaps[[1]]$id
    expect_identical(tree_opts()$heatmaps[[1]]$color_mode, "tiers")

    session$setInputs(nj_heatmap_colors = id)
    session$setInputs(
      nj_heatmap_present = "#112233",
      nj_heatmap_color_mode = "scale",
      nj_heatmap_heat_scale = "Blues"
    )
    session$setInputs(nj_heatmap_apply = 1)
    session$flushReact()

    h <- tree_opts()$heatmaps[[1]]
    expect_identical(h$color_mode, "scale")
    expect_identical(h$heat_scale, "Blues")
    # The swatch the reader set before switching away is still on the record.
    expect_identical(h$color_present, "#112233")
  })
})

test_that("switching classification re-files the genes it keeps", {
  # The two vocabularies are two readings of the same hits, so changing one
  # rewrites the panel's classes (and its column order, which is class order)
  # without touching which genes it draws.
  dir <- local_tempdir()
  db <- amr_fixture_db(dir)
  testServer(visualization_tree$server, args = amr_args(db), {
    set_tree_inputs(session)
    session$setInputs(nj_heatmap_add = "AMR")
    session$flushReact()
    before <- tree_opts()$heatmaps[[1]]
    expect_identical(before$vocabulary, amr_plot$AMR_CLASS_VOCABULARY_DEFAULT)

    session$setInputs(nj_heatmap_vocabulary = "amrfinder")
    session$flushReact()

    after <- tree_opts()$heatmaps[[1]]
    expect_identical(after$vocabulary, "amrfinder")
    expect_setequal(after$cols, before$cols)
    # Same genes, re-filed: abritamr calls blaOXA-2 a carbapenemase, where
    # AMRFinderPlus files it under the broad beta-lactam heading.
    expect_identical(
      before$classes[match("blaOXA-2", before$cols)],
      "Carbapenemase"
    )
    expect_identical(
      after$classes[match("blaOXA-2", after$cols)],
      "Beta-lactam"
    )
  })
})

test_that("clearing the gene picker keeps every gene", {
  # The picker's placeholder says an empty selection means "all genes", not an
  # empty matrix — same contract the old always-visible picker made.
  dir <- local_tempdir()
  db <- amr_fixture_db(dir)
  testServer(visualization_tree$server, args = amr_args(db), {
    set_tree_inputs(session)
    session$setInputs(nj_heatmap_add = "AMR")
    session$flushReact()
    id <- tree_opts()$heatmaps[[1]]$id

    set_genes(session, id, "blaOXA-2")
    expect_identical(tree_opts()$heatmaps[[1]]$cols, "blaOXA-2")

    set_genes(session, id, character(0))
    expect_setequal(tree_opts()$heatmaps[[1]]$cols, c("blaOXA-2", "aac(6')-Ib3"))
  })
})

test_that("each heatmap card's gene picker moves only its own panel", {
  # One observer per card, and each closes over its own panel id — a loop
  # variable shared between them would have made every pick land on the last
  # card drawn.
  dir <- local_tempdir()
  db <- amr_fixture_db(dir)
  testServer(visualization_tree$server, args = amr_args(db), {
    set_tree_inputs(session)
    session$setInputs(nj_heatmap_add = "AMR")
    session$flushReact()
    session$setInputs(nj_heatmap_add = "VIRULENCE")
    session$flushReact()
    ids <- vapply(tree_opts()$heatmaps, function(h) h$id, character(1))

    set_genes(session, ids[[1]], "blaOXA-2")
    hs <- tree_opts()$heatmaps
    expect_identical(hs[[1]]$cols, "blaOXA-2")
    expect_identical(hs[[2]]$cols, "exoU")
  })
})

test_that("heatmap panels draw in the order they were added", {
  dir <- local_tempdir()
  db <- amr_fixture_db(dir)
  testServer(visualization_tree$server, args = amr_args(db), {
    set_tree_inputs(session)
    session$setInputs(nj_heatmap_add = "VIRULENCE")
    session$flushReact()
    session$setInputs(nj_heatmap_add = "AMR")
    session$flushReact()

    hs <- tree_opts()$heatmaps
    expect_identical(length(hs), 2L)
    # Add order, not a fixed element order — the same rule mapping layers draw
    # by.
    expect_identical(
      vapply(hs, function(h) h$element, character(1)),
      c("VIRULENCE", "AMR")
    )
  })
})

test_that("removing a heatmap drops its panel", {
  dir <- local_tempdir()
  db <- amr_fixture_db(dir)
  testServer(visualization_tree$server, args = amr_args(db), {
    set_tree_inputs(session)
    session$setInputs(nj_heatmap_add = "AMR")
    session$flushReact()
    id <- tree_opts()$heatmaps[[1]]$id
    expect_identical(length(tree_opts()$heatmaps), 1L)

    session$setInputs(nj_heatmap_delete = id)
    session$flushReact()
    expect_identical(length(tree_opts()$heatmaps), 0L)
  })
})

test_that("a database with no AMR results offers no heatmap", {
  meta <- data.frame(
    isolate = sprintf("ISO-%02d", 1:8),
    purpose = rep(c("outbreak", "surveillance"), 4),
    stringsAsFactors = FALSE
  )
  testServer(
    visualization_tree$server,
    args = list(
      viz_metadata = reactive(meta),
      field_profiles = reactive(field_profiles(meta)),
      plot_type = reactiveVal("Tree")
    ),
    {
      set_tree_inputs(session)
      # A stale or forced value still cannot add a panel with nothing behind it.
      session$setInputs(nj_heatmap_add = "AMR")
      session$flushReact()
      expect_identical(length(tree_opts()$heatmaps), 0L)
    }
  )
})
