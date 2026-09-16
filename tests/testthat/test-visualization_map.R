box::use(
  leaflet[leaflet],
  shiny[NS, reactive, testServer],
  testthat[
    expect_false,
    expect_identical,
    expect_match,
    expect_setequal,
    expect_true,
    test_that
  ],
)
box::use(
  app / logic / viz_helpers[control_ids],
  app / view / visualization_map,
)

impl <- attr(visualization_map, "namespace")

meta_fixture <- function() {
  data.frame(
    isolate = paste0("ISO-", 1:6),
    specimen = c("Urine", "Blood", "Urine", NA, "Blood", "Sputum"),
    sample_collection_date = c(
      "2026-01-14",
      "2026-01-18",
      "2026-02-03",
      "2026-02-10",
      "2026-03-01",
      "2026-03-04"
    ),
    geo_loc_name_country = rep(c("Germany", "France"), 3),
    stringsAsFactors = FALSE
  )
}

coords_fixture <- function() {
  meta <- meta_fixture()
  meta$place <- meta$geo_loc_name_country
  meta$longitude <- ifelse(meta$place == "Germany", 10.4, 2.2)
  meta$latitude <- ifelse(meta$place == "Germany", 51.1, 46.2)
  meta
}

# Every string a widget's calls carry, flattened for a membership check.
widget_text <- function(m) paste(unlist(m$x$calls), collapse = " ")

# --- Reset catalogue ----------------------------------------------------------

test_that("every control the sidebar renders is in the reset catalogue", {
  ids <- rendered_control_ids(
    impl$map_controls(NS("x")),
    drop = c(
      # Buttons, not settings.
      "reset_settings",
      "map_reset",
      "map_play",
      "map_step_start_prev",
      "map_step_start_next",
      "map_step_end_prev",
      "map_step_end_next",
      # Clears itself the moment it is picked from.
      "map_layer_add",
      # Fitted to the generated coordinates, not declared.
      "map_daterange",
      # Outputs and wrappers the mode script and shinyjs address.
      "map_anim_label",
      "wrap_map_tiles",
      "wrap_map_legend_digits"
    )
  )
  expect_catalogued(ids, impl$map_control_defaults())
})

test_that("every catalogued control is filed under exactly one family", {
  families <- control_ids(impl$MAP_CONTROLS)
  expect_identical(anyDuplicated(families), 0L)
  expect_setequal(families, names(impl$map_control_defaults()))
})

test_that("the removed controls are gone from the panel", {
  html <- as.character(impl$map_controls(NS("x")))
  for (id in c(
    "map_spiderfy",
    "map_na_color",
    "map_region_label_nonzero",
    "map_col_var",
    "map_chart_var",
    "map_chart_scale",
    "map_zoom_to_bounds",
    "map_coverage",
    "map_weight",
    "map_opacity",
    "map_region_reverse",
    "map_stroke_color",
    "map_chart_cluster",
    "map_cluster_zoom_level"
  )) {
    expect_false(grepl(sprintf('id="x-%s"', id), html, fixed = TRUE))
  }
})

# --- Basemaps -----------------------------------------------------------------

test_that("no basemap needing an API key is offered", {
  # CARTO stamps "API KEY REQUIRED" across tiles requested without one.
  expect_false(any(grepl("Carto", c(names(impl$map_providers), impl$map_providers))))
  expect_true(all(impl$mode_tile_defaults %in% impl$map_providers))
})

test_that("choropleth labels come from a keyless tile layer", {
  o <- list(
    mode = "Choropleth",
    scalebar = FALSE,
    minimap = FALSE,
    graticule = FALSE,
    show_controls = FALSE,
    show_time_label = FALSE,
    legend = FALSE
  )
  m <- impl$build_map(NULL, o)
  expect_false(grepl("carto", widget_text(m), ignore.case = TRUE))
})

# --- Heatmap ------------------------------------------------------------------

test_that("heat intensity is not divided down by the zoom level", {
  # leaflet.heat scales every point by 2^(maxZoom - zoom); left at the map's
  # own maxZoom, a country holding hundreds of isolates drew as faint as none.
  o <- list(
    heat_radius = 25,
    heat_max = 3,
    heat_scale = "viridis",
    region_fixed_scale = TRUE,
    legend = FALSE
  )
  m <- impl$build_heatmap(leaflet(), coords_fixture(), o)
  last <- m$x$calls[[length(m$x$calls)]]
  expect_identical(last$method, "addHeatmap")
  expect_identical(last$args[[4]]$maxZoom, 0)
})

# --- Mapping layers -----------------------------------------------------------

test_that("a missing mapped value is drawn grey on a factor scale", {
  layer <- list(palette = "Set1", continuous = FALSE)
  pal <- impl$layer_palette(layer, c("a", "b", NA, ""))
  cols <- pal$pal(pal$convert(c("a", NA, "")))
  expect_identical(cols[2:3], rep(impl$NA_COLOR, 2))
  expect_false(cols[[1]] == impl$NA_COLOR)
})

test_that("an ungrouped date layer gets a numeric scale over its days", {
  layer <- list(palette = "viridis", continuous = TRUE, transform = "as_date")
  pal <- impl$layer_palette(layer, c("2026-01-01", "junk", "2026-03-01"))
  expect_identical(pal$type, "Date")
  expect_identical(pal$pal(pal$convert("junk")), impl$NA_COLOR)
})

test_that("an extended gradient (leaflet doesn't know natively) still colours", {
  # leaflet::colorNumeric()/colorFactor() only special-case "viridis", "magma",
  # "inferno" and "plasma"; a name like "turbo" fell through to being treated
  # as one literal, unparseable colour -- every value rendered black, with an
  # empty legend.
  layer <- list(palette = "turbo", continuous = TRUE)
  pal <- impl$layer_palette(layer, c(1, 5, 10))
  cols <- pal$pal(pal$convert(c(1, 10)))
  expect_false(identical(cols[[1]], cols[[2]]))
  expect_true(all(grepl("^#[0-9A-Fa-f]{6,8}$", cols)))
})

test_that("a qualitative palette with more levels than colours doesn't warn", {
  # colorFactor() only calls brewer.pal() (and warns "n too large...") the
  # first time its *returned* palette function is invoked, not when
  # colorFactor() itself is built -- so the actual colouring call, not just
  # construction, has to be free of the warning.
  layer <- list(palette = "Set3", continuous = FALSE)
  levels <- paste0("level_", seq_len(20))
  pal <- impl$layer_palette(layer, levels)
  expect_no_warning(cols <- pal$pal(pal$convert(levels)))
  expect_identical(length(unique(cols)), 20L)
})

test_that("hex_rgb/hex_alpha round-trip an opacity-enabled colour picker's value", {
  expect_identical(impl$hex_rgb("#2c7fb8"), "#2c7fb8")
  expect_identical(impl$hex_alpha("#2c7fb8"), 1)
  expect_identical(impl$hex_rgb("#33333300"), "#333333")
  expect_identical(impl$hex_alpha("#33333300"), 0)
  expect_identical(impl$hex_alpha("#2c7fb880"), 128 / 255)
})

test_that("markers take their colour from the mapping layer", {
  o <- list(
    cluster = FALSE,
    cluster_radius = 80,
    popup_fields = NULL,
    hover_field = NULL,
    permanent = FALSE,
    label_size = 12,
    marker_color = "#2c7fb8",
    radius = 10,
    region_fixed_scale = FALSE,
    legend = FALSE,
    layer = list(field = "specimen", palette = "Set1", continuous = FALSE)
  )
  m <- impl$build_markers(leaflet(), coords_fixture(), o)
  text <- widget_text(m)
  expect_false(grepl("#2c7fb8", text, fixed = TRUE))
  expect_true(grepl(impl$NA_COLOR, text, fixed = TRUE))
})

test_that("marker opacity is applied to the marker pane, not stacked per circle", {
  o <- list(
    cluster = FALSE,
    cluster_radius = 80,
    popup_fields = NULL,
    hover_field = NULL,
    permanent = FALSE,
    label_size = 12,
    marker_color = "#2c7fb880",
    radius = 10,
    region_fixed_scale = FALSE,
    legend = FALSE
  )
  m <- impl$build_markers(leaflet(), coords_fixture(), o)
  methods <- vapply(m$x$calls, `[[`, "", "method")
  expect_true("createMapPane" %in% methods)
  last <- m$x$calls[[max(which(methods == "addCircleMarkers"))]]
  # Path options (stroke/color/weight/opacity/fillColor/fillOpacity/pane)
  # travel as one bundled list among addCircleMarkers()'s positional args.
  path_opts <- Filter(function(a) is.list(a) && "fillColor" %in% names(a), last$args)[[1]]
  expect_true(path_opts$stroke)
  expect_identical(path_opts$weight, 1)
  expect_identical(path_opts$opacity, 1)
  expect_identical(path_opts$color, "#000000")
  expect_identical(path_opts$fillColor, "#2c7fb8")
  expect_identical(path_opts$fillOpacity, 1)
  expect_identical(path_opts$pane, impl$MARKER_PANE)
  hooks <- vapply(m$jsHooks$render, `[[`, "", "code")
  expect_true(any(grepl("pane.style.opacity = 0.502", hooks, fixed = TRUE)))
})

test_that("only regions with at least one isolate get a permanent label", {
  o <- list(
    mode = "Choropleth",
    region_transform = "Raw",
    region_fixed_scale = FALSE,
    region_scale = "Oranges",
    region_opacity = 0.7,
    region_border = "#000000",
    region_permanent = TRUE,
    legend = FALSE
  )
  m <- impl$build_choropleth(leaflet(), coords_fixture(), o)
  call <- m$x$calls[[length(m$x$calls)]]
  expect_identical(call$method, "addPolygons")
  lopts <- call$args[[8]]
  permanent <- vapply(lopts, function(x) x$permanent, logical(1))
  expect_true(any(permanent))
  expect_true(any(!permanent))
})

test_that("charts give isolates missing the value a grey slice", {
  o <- list(
    chart_type = "pie",
    chart_size = 40,
    chart_opacity = 1,
    chart_cluster = FALSE,
    region_fixed_scale = FALSE,
    legend = FALSE,
    layer = list(field = "specimen", palette = "Set1", continuous = FALSE)
  )
  m <- impl$build_charts(leaflet(), coords_fixture(), o)
  text <- widget_text(m)
  expect_match(text, "Missing", fixed = TRUE)
  expect_match(text, impl$NA_COLOR, fixed = TRUE)
})

test_that("chart colours match the same field's marker colours, category for category", {
  layer <- list(field = "specimen", palette = "Set1", continuous = FALSE)
  o <- list(
    chart_type = "pie",
    chart_size = 40,
    chart_opacity = 1,
    chart_cluster = FALSE,
    region_fixed_scale = FALSE,
    legend = FALSE,
    layer = layer
  )
  coords <- coords_fixture()
  expected <- impl$layer_palette(layer, coords$specimen)
  m <- impl$build_charts(leaflet(), coords, o)
  call <- m$x$calls[[which(vapply(m$x$calls, `[[`, "", "method") == "addMinicharts")]]
  cols <- call$args[[4]]
  labels <- call$args[[7]]$labels
  named <- labels != "Missing"
  expect_identical(cols[named], expected$pal(labels[named]))
})

test_that("charts fold into Other only once a category cap is set", {
  o <- list(
    chart_type = "pie",
    chart_size = 40,
    chart_opacity = 1,
    chart_cluster = FALSE,
    region_fixed_scale = FALSE,
    legend = FALSE,
    layer = list(field = "specimen", palette = "Set1", continuous = FALSE)
  )
  chart_call <- function(m) {
    m$x$calls[[which(vapply(m$x$calls, `[[`, "", "method") == "addMinicharts")]]
  }
  unfolded <- chart_call(impl$build_charts(leaflet(), coords_fixture(), o))$args[[7]]$labels
  expect_false("Other" %in% unfolded)
  expect_true(all(c("Blood", "Sputum", "Urine") %in% unfolded))

  o$chart_max_categories <- 2
  call <- chart_call(impl$build_charts(leaflet(), coords_fixture(), o))
  folded <- call$args[[7]]$labels
  expect_true("Other" %in% folded)
  expect_identical(sum(folded %in% c("Blood", "Sputum", "Urine")), 2L)
  expect_identical(call$args[[4]][folded == "Other"], impl$OTHER_COLOR)
})

test_that("charts draw the shared titled legend, not minicharts' own", {
  o <- list(
    chart_type = "pie",
    chart_size = 40,
    chart_opacity = 1,
    chart_cluster = FALSE,
    region_fixed_scale = FALSE,
    legend = TRUE,
    legend_pos = "topleft",
    legend_title = "",
    layer = list(
      field = "specimen",
      palette = "Set1",
      continuous = FALSE,
      title = "Specimen source"
    )
  )
  m <- impl$build_charts(leaflet(), coords_fixture(), o)
  expect_true("addLegend" %in% vapply(m$x$calls, `[[`, "", "method"))
  expect_match(widget_text(m), "Specimen source", fixed = TRUE)
})

test_that("picking a variable adds one layer and a reset removes it", {
  meta <- meta_fixture()
  testServer(
    visualization_map$server,
    args = list(viz_metadata = reactive(meta)),
    {
      session$setInputs(map_layer_add = "specimen")
      expect_identical(length(map_layers()), 1L)
      expect_identical(map_layers()[[1]]$field, "specimen")
      expect_true(nzchar(map_layers()[[1]]$palette))

      session$setInputs(reset_settings_confirm = 1)
      expect_identical(length(map_layers()), 0L)
    }
  )
})

test_that("a plot saved before the map had layers restores its mapping", {
  meta <- meta_fixture()
  testServer(
    visualization_map$server,
    args = list(viz_metadata = reactive(meta)),
    {
      restore(list(
        map_mode = "Markers",
        map_color_var = TRUE,
        map_col_var = "specimen",
        map_col_scale = "Dark2"
      ))
      expect_identical(length(map_layers()), 1L)
      expect_identical(map_layers()[[1]]$palette, "Dark2")
    }
  )
})
