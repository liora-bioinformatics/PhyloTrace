box::use(
  shiny[isolate, observe, reactive, reactiveVal, testServer],
  testthat[
    expect_equal,
    expect_false,
    expect_identical,
    expect_true,
    test_that
  ],
  withr[local_tempdir],
)
box::use(
  app / logic / field_profile[field_profiles],
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
# them (a NULL legend size reaches unit() and errors), so stand the whole
# control panel up at its coded defaults.
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
    nj_aspect_ratio = 0.6,
    nj_zoom = 0.95,
    nj_h = -0.05,
    nj_v = 0,
    nj_legend_orientation = "vertical",
    nj_legend_size = 10
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

      # A column that groups nothing has no aesthetic to take, so it adds no
      # layer — and still must not redraw.
      session$setInputs(nj_layer_add = "organism")
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

amr_meta <- function() {
  meta <- data.frame(
    isolate = sprintf("ISO-%02d", 1:8),
    purpose = rep(c("outbreak", "surveillance"), 4),
    `amr_Beta-lactam` = rep(c("blaOXA", ""), 4),
    amr_Aminoglycoside = rep(c("aac(6')", "", "", ""), 2),
    amr_profile = rep(c("Beta-lactam", ""), 4),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
  attr(meta, "amr_cols") <- c(
    "amr_Beta-lactam",
    "amr_Aminoglycoside",
    "amr_profile"
  )
  attr(meta, "amr_class_sections") <- c(
    `amr_Beta-lactam` = "Resistance",
    amr_Aminoglycoside = "Resistance",
    amr_profile = "Resistance"
  )
  meta
}

amr_args <- function(meta) {
  list(
    viz_metadata = reactive(meta),
    field_profiles = reactive(field_profiles(
      meta,
      amr_cols = attr(meta, "amr_cols")
    )),
    plot_type = reactiveVal("Tree")
  )
}

test_that("switching the class heatmap on draws every column by default", {
  meta <- amr_meta()
  testServer(visualization_tree$server, args = amr_args(meta), {
    set_tree_inputs(session)
    session$flushReact()
    expect_identical(length(tree_opts()$heatmaps), 0L)

    session$setInputs(nj_heatmap_class = TRUE)
    session$flushReact()

    hs <- tree_opts()$heatmaps
    expect_identical(length(hs), 1L)
    expect_identical(hs[[1]]$level, "class")
    # An empty picker means "all of them", not an empty matrix — and the
    # profile column is a summary of the others, so it is never one of them.
    expect_setequal(hs[[1]]$cols, c("amr_Beta-lactam", "amr_Aminoglycoside"))
  })
})

test_that("a chosen subset survives, and costs exactly one redraw", {
  # The reported fault: choosing a subset re-rendered the controls, which
  # re-created the picker empty and discarded the selection — so the tree drew
  # twice and the choice was gone. The controls are static now.
  meta <- amr_meta()
  testServer(visualization_tree$server, args = amr_args(meta), {
    set_tree_inputs(session)
    session$setInputs(nj_heatmap_class = TRUE)
    session$flushReact()

    draws <- 0L
    observe({
      tree_opts()
      draws <<- draws + 1L
    })
    session$flushReact()
    draws <- 0L

    session$setInputs(nj_heatcols_class = "amr_Beta-lactam")
    session$flushReact()

    expect_identical(draws, 1L)
    expect_identical(tree_opts()$heatmaps[[1]]$cols, "amr_Beta-lactam")
    # And it is still what the input holds — nothing reset it.
    expect_identical(isolate(input$nj_heatcols_class), "amr_Beta-lactam")
  })
})

test_that("both heatmap panels can be shown at once, classes first", {
  meta <- amr_meta()
  testServer(visualization_tree$server, args = amr_args(meta), {
    set_tree_inputs(session)
    session$setInputs(nj_heatmap_class = TRUE, nj_heatmap_gene = TRUE)
    session$flushReact()

    hs <- tree_opts()$heatmaps
    # No gene matrix in this fixture, so only the class panel has anything to
    # draw — a level with no columns contributes no panel rather than an empty
    # one.
    expect_true(length(hs) >= 1L)
    expect_identical(hs[[1]]$level, "class")
    if (length(hs) == 2L) {
      expect_identical(hs[[2]]$level, "gene")
    }
  })
})

test_that("switching a heatmap off removes its panel", {
  meta <- amr_meta()
  testServer(visualization_tree$server, args = amr_args(meta), {
    set_tree_inputs(session)
    session$setInputs(nj_heatmap_class = TRUE)
    session$flushReact()
    expect_identical(length(tree_opts()$heatmaps), 1L)

    session$setInputs(nj_heatmap_class = FALSE)
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
  testServer(visualization_tree$server, args = amr_args(meta), {
    set_tree_inputs(session)
    session$setInputs(nj_heatmap_class = TRUE)
    session$flushReact()
    expect_identical(length(tree_opts()$heatmaps), 0L)
  })
})
