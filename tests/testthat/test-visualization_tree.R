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
    nj_ladder = TRUE,
    nj_color = "#000000",
    nj_bg = "#ffffff",
    nj_tiplab_show = TRUE,
    nj_tiplab = "isolate",
    nj_tiplab_size = 4,
    nj_tiplab_alpha = 1,
    nj_tiplab_fontface = "plain",
    nj_tiplab_angle = 0,
    nj_align = TRUE,
    nj_tiplab_color = "#000000",
    nj_tiplab_fill = "#84D9A0",
    nj_mapping_show = FALSE,
    nj_show_branch_label = FALSE,
    nj_branch_label = "Allelic Distance",
    nj_branch_size = 4,
    nj_branchlabel_cutoff = 10,
    nj_branch_color = "#000000",
    nj_branch_label_color = "#FFB7B7",
    nj_tippoint_show = FALSE,
    nj_tippoint_alpha = 0.5,
    nj_tippoint_size = 4,
    nj_tippoint_color = "#3A4657",
    nj_tippoint_shape = 16,
    nj_tipcolor_mapping_show = FALSE,
    nj_tipshape_mapping_show = FALSE,
    nj_nodepoint_show = FALSE,
    nj_nodepoint_alpha = 1,
    nj_nodepoint_color = "#3A4657",
    nj_nodepoint_shape = 16,
    nj_nodepoint_size = 2.5,
    nj_nodelabel_show = FALSE,
    nj_clade_scale = "#D0F221",
    nj_clade_type = "roundrect",
    nj_heatmap_show = FALSE,
    nj_rootedge_show = TRUE,
    nj_treescale_show = TRUE,
    nj_title = "",
    nj_subtitle = "",
    nj_title_color = "#000000",
    nj_title_size = 30,
    nj_subtitle_size = 30,
    nj_aspect_ratio = 0.6,
    nj_zoom = 0.95,
    nj_h = -0.05,
    nj_v = 0,
    nj_legend_orientation = "vertical",
    nj_legend_x = 0.9,
    nj_legend_y = 0.2,
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

test_that("a control the plot is not drawing cannot cause a rebuild", {
  testServer(
    visualization_tree$server,
    args = list(plot_type = reactiveVal("Tree")),
    {
      set_tree_inputs(session)
      session$flushReact()
      before <- tree_opts()

      # Generate repopulates these pickers from the loaded metadata whether or
      # not their switch is on (populate_metadata_selects / filter_scale_choices),
      # and each rewrite echoes back from the browser. With every switch off,
      # none of it is being drawn, so none of it may count as a change.
      session$setInputs(
        nj_color_mapping = "organism",
        nj_tiplab_scale = "magma",
        nj_tipcolor_mapping = "organism",
        nj_tippoint_scale = "magma",
        nj_tipshape_mapping = "organism",
        nj_branch_label = "organism"
      )
      session$flushReact()
      expect_identical(tree_opts(), before)

      # Switched on, the same variable is suddenly load-bearing.
      session$setInputs(nj_mapping_show = TRUE)
      session$flushReact()
      expect_false(identical(tree_opts(), before))
      expect_equal(tree_opts()$color_mapping, "organism")
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
      # Five hidden tile strips cannot contribute a draw between them.
      expect_identical(length(tree_opts()$tiles), 0L)
    }
  )
})

test_that("the canvas is fixed, not taken from the browser", {
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
      expect_equal(plot_width_in(), impl$PLOT_WIDTH_PX / impl$PLOT_RES)
      expect_equal(tree_opts()$width_in, impl$PLOT_WIDTH_PX / impl$PLOT_RES)
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
      expect_identical(draws, 1L)
      expect_true(generated())

      # A Generate over a different isolate set: a new tree and a new fit, so a
      # redraw is owed — exactly one, with the fitted values already in it.
      # This is where three used to be logged, one per control echoing back.
      selected(c("A", "B", "C"))
      generate(1L)
      session$flushReact()

      expect_identical(draws, 2L)
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
        nj_nodepoint_size = isolate(fitted$nj_nodepoint_size),
        nj_branch_size = isolate(fitted$nj_branch_size)
      )
      session$flushReact()
      expect_identical(draws, 2L)
    }
  )
})
