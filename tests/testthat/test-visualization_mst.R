box::use(
  shiny[reactive, reactiveVal, testServer],
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
  app / logic / mst_plot[mst_node_sizes],
  app / view / visualization_mst,
)

impl <- attr(visualization_mst, "namespace")

# Five isolates, two of them with identical profiles — so the MST has a merged
# node, which is the case every per-node decision in this engine turns on.
alleles <- function() {
  list(
    ref = ref_alleles(),
    A = c(g1 = seqv("A1"), g2 = seqv("A2"), g3 = seqv("A3")),
    B = c(g1 = seqv("B1"), g2 = seqv("A2"), g3 = seqv("A3")),
    C = c(g1 = seqv("C1"), g2 = seqv("C2"), g3 = seqv("A3")),
    D = c(g1 = seqv("D1"), g2 = seqv("C2"), g3 = seqv("D3")),
    E = c(g1 = seqv("D1"), g2 = seqv("C2"), g3 = seqv("D3"))
  )
}

isolates <- c("A", "B", "C", "D", "E")

meta <- function() {
  meta_df(
    isolates,
    extra = list(
      host = c("Human", "Human", "Bovine", "Bovine", "Ovine"),
      purpose = c("Outbreak", "Outbreak", "Routine", "Routine", "Routine"),
      # Unique per isolate, so it cannot group them — the profile says so and
      # the mapping engine refuses it.
      ward = c("A", "B", "C", "D", "E")
    )
  )
}

# `complex_type` NULL leaves the scheme silent about its cluster distance, which
# is the other branch of the threshold default.
fixture_db <- function(dir, complex_type = 7L) {
  db <- file.path(dir, "local.db")
  build_db(db, alleles(), metadata = meta())
  if (!is.null(complex_type)) {
    con <- DBI::dbConnect(RSQLite::SQLite(), db)
    on.exit(DBI::dbDisconnect(con))
    DBI::dbExecute(
      con,
      "INSERT INTO scheme_overview (key, value) VALUES ('Complex Type Distance', ?)",
      params = list(as.character(complex_type))
    )
  }
  db
}

# testServer starts every input NULL, and mst_frames() dereferences most of
# them, so stand the whole control panel up at its coded defaults.
set_mst_inputs <- function(session, ...) {
  defaults <- list(
    mst_show_label = TRUE,
    mst_show_edge_label = TRUE,
    mst_node_label_fontsize = 13,
    mst_edge_font_size = 20,
    mst_text_color = "#000000",
    mst_color_node = "#B2FACA",
    mst_color_edge = "#000000",
    mst_edge_font_color = "#000000",
    mst_background_color = "#ffffff",
    mst_background_transparent = TRUE,
    mst_scale_nodes = TRUE,
    mst_shadow = TRUE,
    mst_length_mode = "log",
    mst_edge_length_scale = 15,
    mst_shorten_long = TRUE,
    mst_cap_mult = 7,
    mst_rotation = 0,
    mst_collapse_threshold = 0,
    mst_show_clusters = TRUE,
    mst_cluster_col_scale = "viridis",
    mst_cluster_width = 20,
    mst_cluster_opacity = 35,
    mst_cluster_label_size = 18,
    mst_cluster_label_tint = FALSE,
    mst_show_legend = TRUE,
    mst_legend_ori = "left",
    mst_show_scale_caption = TRUE
  )
  do.call(session$setInputs, utils::modifyList(defaults, list(...)))
}

args_for <- function(db, generate) {
  list(
    db_path = reactive(db),
    viz_metadata = reactive(meta()),
    selected_isolates = reactive(isolates),
    generate = generate,
    plot_type = reactive("MST")
  )
}

# --- The cluster threshold ----------------------------------------------------

test_that("the threshold defaults to the scheme's complex-type distance", {
  dir <- local_tempdir()
  db <- fixture_db(dir, complex_type = 7L)
  testServer(visualization_mst$server, args = args_for(db, reactiveVal(0L)), {
    set_mst_inputs(session)
    expect_identical(scheme_threshold(), 7L)
    # The mirror is what the render reads, so that is where the default has to
    # land — not only in the visible control.
    expect_identical(fitted$mst_cluster_threshold, 7L)
  })
})

test_that("a scheme with no complex-type distance keeps the placeholder", {
  dir <- local_tempdir()
  db <- fixture_db(dir, complex_type = NULL)
  testServer(visualization_mst$server, args = args_for(db, reactiveVal(0L)), {
    set_mst_inputs(session)
    expect_identical(scheme_threshold(), NULL)
    expect_identical(
      fitted$mst_cluster_threshold,
      impl$THRESHOLD_PLACEHOLDER
    )
  })
})

test_that("a threshold the user set is never overwritten by the scheme's", {
  dir <- local_tempdir()
  db <- fixture_db(dir, complex_type = 7L)
  testServer(visualization_mst$server, args = args_for(db, reactiveVal(0L)), {
    set_mst_inputs(session, mst_cluster_threshold = 3)
    apply_scheme_threshold()
    expect_equal(fitted$mst_cluster_threshold, 3)
    # Reset is the one route that is allowed to take it back.
    apply_scheme_threshold(force_default = TRUE)
    expect_identical(fitted$mst_cluster_threshold, 7L)
  })
})

# --- Generate and the fit -----------------------------------------------------

test_that("Generate computes the MST and fits the controls to it", {
  dir <- local_tempdir()
  db <- fixture_db(dir)
  generate <- reactiveVal(0L)
  testServer(visualization_mst$server, args = args_for(db, generate), {
    set_mst_inputs(session)
    generate(1L)
    session$flushReact()

    expect_true(generated())
    # D and E share a profile, so five isolates make four nodes.
    expect_equal(igraph::vcount(mst_obj()), 4)
    expect_true(fitted$mst_show_label)
    # Two handles, because the size means smallest and largest.
    expect_identical(length(fitted$mst_node_size), 2L)

    fr <- frames()
    expect_identical(nrow(fr$nodes), 4L)
    expect_identical(nrow(fr$edges), 3L)
    # One of those four stands for two isolates.
    expect_identical(max(mst_node_sizes(fr$nodes$id)), 2L)
  })
})

test_that("a Generate for the other engine leaves this one's graph alone", {
  dir <- local_tempdir()
  db <- fixture_db(dir)
  generate <- reactiveVal(0L)
  plot_type <- reactiveVal("MST")
  args <- args_for(db, generate)
  args$plot_type <- plot_type
  testServer(visualization_mst$server, args = args, {
    set_mst_inputs(session)
    generate(1L)
    session$flushReact()
    before <- mst_obj()

    plot_type("Tree")
    generate(2L)
    session$flushReact()
    expect_identical(mst_obj(), before)
  })
})

# --- Variable mapping ---------------------------------------------------------

test_that("a mapped variable takes the fill; a second is refused", {
  dir <- local_tempdir()
  db <- fixture_db(dir)
  generate <- reactiveVal(0L)
  testServer(visualization_mst$server, args = args_for(db, generate), {
    set_mst_inputs(session)
    generate(1L)
    session$flushReact()

    session$setInputs(mst_layer_add = "host")
    expect_identical(length(mst_layers()), 1L)
    expect_identical(mst_layers()[[1]]$aesthetic, "node_fill")
    # The fill is what shows a merged node's whole distribution.
    expect_true(frames()$custom)

    # Node fill is the MST's only channel, so a second variable is refused
    # rather than replacing or stacking onto the first.
    session$setInputs(mst_layer_add = "purpose")
    expect_identical(length(mst_layers()), 1L)
    expect_identical(mst_layers()[[1]]$field, "host")

    # Adding the same variable twice is a no-op rather than a duplicate layer.
    session$setInputs(mst_layer_add = "host")
    expect_identical(length(mst_layers()), 1L)

    # A column that names every isolate uniquely cannot group them, so it is
    # refused rather than mapped to nothing.
    session$setInputs(mst_layer_add = "ward")
    expect_identical(length(mst_layers()), 1L)
  })
})

test_that("deleting the mapped layer leaves none", {
  dir <- local_tempdir()
  db <- fixture_db(dir)
  testServer(visualization_mst$server, args = args_for(db, reactiveVal(0L)), {
    set_mst_inputs(session)
    session$setInputs(mst_layer_add = "host")
    id <- mst_layers()[[1]]$id
    session$setInputs(mst_layer_delete = id)
    expect_identical(mst_layers(), list())
  })
})

test_that("the mapping list is emptied by a reset", {
  dir <- local_tempdir()
  db <- fixture_db(dir)
  testServer(visualization_mst$server, args = args_for(db, reactiveVal(0L)), {
    set_mst_inputs(session)
    session$setInputs(mst_layer_add = "host")
    expect_identical(length(mst_layers()), 1L)
    session$setInputs(reset_settings = 1)
    expect_identical(mst_layers(), list())
  })
})

# --- The node size slider -----------------------------------------------------

test_that("the size slider has two handles only while nodes are scaled", {
  dir <- local_tempdir()
  db <- fixture_db(dir)
  testServer(visualization_mst$server, args = args_for(db, reactiveVal(0L)), {
    set_mst_inputs(session, mst_scale_nodes = TRUE)
    expect_true(grepl('data-type="double"', output$mst_node_size_ui$html))
    session$setInputs(mst_scale_nodes = FALSE)
    expect_false(grepl('data-type="double"', output$mst_node_size_ui$html))
  })
})

# --- What forces a rebuild ----------------------------------------------------

test_that("a colour change is a data push, not a rebuilt network", {
  dir <- local_tempdir()
  db <- fixture_db(dir)
  generate <- reactiveVal(0L)
  testServer(visualization_mst$server, args = args_for(db, generate), {
    set_mst_inputs(session)
    generate(1L)
    session$flushReact()
    before <- shell()

    session$setInputs(mst_color_edge = "#FF0000")
    session$flushReact()
    # Same shell: the widget is left standing and the new edge table is pushed
    # into it.
    expect_identical(shell(), before)
    expect_identical(unique(drawn()$edges$color), "#FF0000")
  })
})

test_that("allelic distances can be switched back off once shown", {
  dir <- local_tempdir()
  db <- fixture_db(dir)
  generate <- reactiveVal(0L)
  testServer(visualization_mst$server, args = args_for(db, generate), {
    set_mst_inputs(session, mst_show_edge_label = TRUE)
    generate(1L)
    session$flushReact()
    expect_false(identical(unique(drawn()$edges$label), ""))
    labelled <- drawn()$edges$id

    session$setInputs(mst_show_edge_label = FALSE)
    session$flushReact()
    # A single space, not "" — vis.js only overwrites a label on a truthy
    # incoming value (see mst_frames()'s comment), so "" would be silently
    # ignored and the last real label would stay on screen forever.
    expect_identical(unique(drawn()$edges$label), " ")
    # Same ids, so the push updates the branches already on the canvas. Without
    # them vis.js appended a second, unlabelled copy of every branch on top of
    # the labelled ones — which stayed, and stayed readable.
    expect_identical(drawn()$edges$id, labelled)
  })
})

test_that("the widget is rebuilt on a changed value, not a changed input", {
  dir <- local_tempdir()
  db <- fixture_db(dir)
  generate <- reactiveVal(0L)
  testServer(visualization_mst$server, args = args_for(db, generate), {
    set_mst_inputs(session)
    generate(1L)
    session$flushReact()
    built <- shell_built()
    expect_false(is.null(built))

    # Re-reporting a value it already holds — which is what every echo of an
    # update*Input() message is — must not republish and so must not rebuild.
    session$setInputs(mst_shadow = TRUE, mst_legend_ori = "left")
    session$flushReact()
    expect_identical(shell_built(), built)

    session$setInputs(mst_legend_ori = "right")
    session$flushReact()
    expect_false(identical(shell_built(), built))
  })
})

test_that("collapsing folds the tree and says how far", {
  dir <- local_tempdir()
  db <- fixture_db(dir)
  generate <- reactiveVal(0L)
  testServer(visualization_mst$server, args = args_for(db, generate), {
    set_mst_inputs(session)
    generate(1L)
    session$flushReact()
    nodes <- nrow(frames()$nodes)
    before <- shell()

    session$setInputs(mst_collapse_threshold = 1)
    session$flushReact()
    expect_lt(nrow(frames()$nodes), nodes)
    # A node the threshold folded away has to actually leave the canvas, and the
    # incremental path only ever updates or adds by id — it never removes one.
    expect_false(identical(shell(), before))
  })
})

test_that("rotation turns the drawing and rebuilds it", {
  dir <- local_tempdir()
  db <- fixture_db(dir)
  generate <- reactiveVal(0L)
  testServer(visualization_mst$server, args = args_for(db, generate), {
    set_mst_inputs(session)
    generate(1L)
    session$flushReact()
    before <- frames()$coords$x

    session$setInputs(mst_rotation = 90)
    session$flushReact()
    expect_false(isTRUE(all.equal(frames()$coords$x, before)))
  })
})

test_that("the branch font is a data push too", {
  dir <- local_tempdir()
  db <- fixture_db(dir)
  generate <- reactiveVal(0L)
  testServer(visualization_mst$server, args = args_for(db, generate), {
    set_mst_inputs(session)
    generate(1L)
    session$flushReact()
    before <- shell()

    session$setInputs(mst_edge_font_size = 26, mst_edge_font_color = "#FF0000")
    session$flushReact()
    # It used to reach neither path: not the shell, which does not depend on it,
    # and not the data, which did not carry it — so the control did nothing.
    expect_identical(shell(), before)
    expect_identical(unique(drawn()$edges$font.size), 26)
    expect_identical(unique(drawn()$edges$font.color), "#FF0000")
  })
})

test_that("the size slider reaches the radii while duplicates scale them", {
  dir <- local_tempdir()
  db <- fixture_db(dir)
  generate <- reactiveVal(0L)
  testServer(visualization_mst$server, args = args_for(db, generate), {
    set_mst_inputs(session, mst_scale_nodes = TRUE)
    generate(1L)
    session$flushReact()

    session$setInputs(mst_node_size = c(8, 44))
    session$flushReact()
    expect_equal(range(frames()$nodes$size), c(8, 44))
  })
})

test_that("the label switch survives everything but a fresh Generate", {
  dir <- local_tempdir()
  db <- fixture_db(dir)
  generate <- reactiveVal(0L)
  testServer(visualization_mst$server, args = args_for(db, generate), {
    set_mst_inputs(session)
    generate(1L)
    session$flushReact()

    session$setInputs(mst_show_label = FALSE)
    session$flushReact()
    expect_false(fitted$mst_show_label)

    # A refit used to ride along with the label source and take this with it.
    session$setInputs(mst_node_label = "isolate")
    session$flushReact()
    expect_false(fitted$mst_show_label)
  })
})

test_that("the label source is a full metadata picker, defaulting to Isolate", {
  dir <- local_tempdir()
  db <- fixture_db(dir)
  testServer(visualization_mst$server, args = args_for(db, reactiveVal(0L)), {
    set_mst_inputs(session)
    session$flushReact()
    expect_identical(fitted$mst_node_label, "isolate")
    # Every metadata column is offered, Isolate included — the same set
    # mst_layer_add draws from, plus Isolate itself.
    expect_true(all(c("isolate", "host", "purpose") %in% profiles()$field))

    session$setInputs(mst_node_label = "host")
    session$flushReact()
    expect_identical(fitted$mst_node_label, "host")
  })
})

test_that("Isolate is a selectable label source, not disabled for being unique", {
  dir <- local_tempdir()
  db <- fixture_db(dir)
  testServer(visualization_mst$server, args = args_for(db, reactiveVal(0L)), {
    set_mst_inputs(session)
    session$flushReact()
    # field_profiles() marks isolate non-groupable (it is unique per row),
    # which is correct for the *mapping* picker but would grey Isolate out of
    # the label picker too if the same profile reached it unmodified.
    prof <- profiles()
    expect_false(prof$groupable[prof$field == "isolate"])
    expect_true(label_profile()$groupable[prof$field == "isolate"])
  })
})

test_that("a cluster region's look rebuilds the network", {
  dir <- local_tempdir()
  db <- fixture_db(dir)
  generate <- reactiveVal(0L)
  testServer(visualization_mst$server, args = args_for(db, generate), {
    set_mst_inputs(session, mst_show_clusters = TRUE)
    generate(1L)
    session$flushReact()
    before <- shell()

    # The region is painted by a beforeDrawing hook, which is baked into the
    # widget — the proxy cannot reach it, so every one of these has to rebuild.
    for (change in list(
      list(mst_cluster_width = 40),
      list(mst_cluster_opacity = 90),
      list(mst_cluster_label_size = 0),
      list(mst_cluster_label_tint = TRUE)
    )) {
      previous <- shell()
      do.call(session$setInputs, change)
      session$flushReact()
      expect_false(identical(shell(), previous))
    }
    expect_false(identical(shell(), before))
  })
})

test_that("a legacy layer on a channel that is gone lands back on the fill", {
  dir <- local_tempdir()
  db <- fixture_db(dir)
  testServer(visualization_mst$server, args = args_for(db, reactiveVal(0L)), {
    set_mst_inputs(session)
    restore(list(
      .layers = list(list(
        id = "L1",
        field = "host",
        title = "Host",
        aesthetic = "node_border",
        palette = "viridis",
        n_levels = 2L,
        continuous = FALSE,
        auto = FALSE
      ))
    ))
    session$flushReact()
    expect_identical(length(mst_layers()), 1L)
    expect_identical(mst_layers()[[1]]$aesthetic, "node_fill")
  })
})

test_that("a change to the geometry rebuilds the network", {
  dir <- local_tempdir()
  db <- fixture_db(dir)
  generate <- reactiveVal(0L)
  testServer(visualization_mst$server, args = args_for(db, generate), {
    set_mst_inputs(session)
    generate(1L)
    session$flushReact()
    before <- shell()
    extent <- diff(range(frames()$coords$x))

    session$setInputs(mst_edge_length_scale = 30)
    session$flushReact()
    expect_false(identical(shell(), before))
    expect_equal(diff(range(frames()$coords$x)), extent * 2)
  })
})

# --- Saved analyses -----------------------------------------------------------

test_that("a snapshot carries the mapping layers as well as the controls", {
  dir <- local_tempdir()
  db <- fixture_db(dir)
  testServer(visualization_mst$server, args = args_for(db, reactiveVal(0L)), {
    set_mst_inputs(session)
    session$setInputs(mst_layer_add = "host")
    snap <- snapshot()
    expect_true("mst_show_label" %in% names(snap))
    expect_identical(length(snap$.layers), 1L)
    expect_identical(snap$.layers[[1]]$field, "host")
  })
})

test_that("a saved analysis restores its layers, mirrors included", {
  dir <- local_tempdir()
  db <- fixture_db(dir)
  testServer(visualization_mst$server, args = args_for(db, reactiveVal(0L)), {
    set_mst_inputs(session)
    session$setInputs(mst_layer_add = "host")
    snap <- snapshot()
    session$setInputs(reset_settings = 1)
    expect_identical(mst_layers(), list())

    restore(snap)
    session$flushReact()
    expect_identical(length(mst_layers()), 1L)
    expect_identical(mst_layers()[[1]]$field, "host")
    # The mirror, not just the control: a restored value the browser does not
    # echo back would otherwise never reach the render.
    expect_identical(fitted$mst_node_label, "isolate")
    expect_equal(fitted$mst_edge_length_scale, snap[["mst_edge_length_scale"]])
  })
})

test_that("a saved analysis restores a non-default label source", {
  dir <- local_tempdir()
  db <- fixture_db(dir)
  testServer(visualization_mst$server, args = args_for(db, reactiveVal(0L)), {
    set_mst_inputs(session)
    session$setInputs(mst_node_label = "host")
    session$flushReact()
    snap <- snapshot()
    expect_identical(snap$mst_node_label, "host")

    # reset_settings' own revert to Isolate happens behind shinyjs::delay(),
    # which needs a browser to fire and so never runs inside testServer — mimic
    # it directly instead, the way the module's own delayed callback would.
    populate_metadata_selects(force_default = TRUE)
    expect_identical(fitted$mst_node_label, "isolate")

    restore(snap)
    session$flushReact()
    expect_identical(fitted$mst_node_label, "host")
  })
})

test_that("a pre-rewrite snapshot's pie mapping becomes a fill layer", {
  dir <- local_tempdir()
  db <- fixture_db(dir)
  testServer(visualization_mst$server, args = args_for(db, reactiveVal(0L)), {
    set_mst_inputs(session)
    restore(list(
      mst_color_var = TRUE,
      mst_col_var = "host",
      mst_col_scale = "Viridis",
      # And "Scale allelic distance" off, which is now a named transform.
      mst_scale_edges = FALSE
    ))
    session$flushReact()
    expect_identical(length(mst_layers()), 1L)
    expect_identical(mst_layers()[[1]]$field, "host")
    expect_identical(mst_layers()[[1]]$aesthetic, "node_fill")
    expect_identical(fitted$mst_length_mode, "uniform")
  })
})

test_that("a snapshot with no mapping at all restores no layers", {
  dir <- local_tempdir()
  db <- fixture_db(dir)
  testServer(visualization_mst$server, args = args_for(db, reactiveVal(0L)), {
    set_mst_inputs(session)
    restore(list(mst_color_var = FALSE, mst_show_label = FALSE))
    expect_identical(mst_layers(), list())
  })
})
