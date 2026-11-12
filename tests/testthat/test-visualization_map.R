box::use(
  leaflet[leaflet],
  sf[st_intersects, st_point, st_sfc],
  shiny[NS, reactive, testServer],
  testthat[
    expect_false,
    expect_identical,
    expect_match,
    expect_null,
    expect_setequal,
    expect_true,
    test_that
  ],
  withr[local_tempdir],
)
box::use(
  app / logic / geocode_cache[write_geocodes],
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
    region_scale = "Oranges",
    region_opacity = 0.7,
    region_border = "#000000",
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

# --- filter_coords --------------------------------------------------------------

test_that("filter_coords filters by the chosen date variable, by period overlap", {
  coords <- data.frame(
    isolate = c("a", "b", "c", "d"),
    custom_year = c("2020", "2021", NA, "2022"),
    stringsAsFactors = FALSE
  )
  bounds <- as.Date(c("2020-01-01", "2022-12-31"))
  o <- list(
    daterange = as.Date(c("2021-03-01", "2021-03-31")),
    date_field = "custom_year",
    date_bounds = bounds
  )
  expect_identical(impl$filter_coords(coords, o)$isolate, "b")

  # The whole span is no filter: the undated isolate stays.
  o$daterange <- bounds
  expect_identical(impl$filter_coords(coords, o)$isolate, coords$isolate)

  # Nothing to filter by (no variable holds dates).
  o$daterange <- as.Date(c("2021-03-01", "2021-03-31"))
  o$date_field <- NULL
  expect_identical(nrow(impl$filter_coords(coords, o)), 4L)
})

# --- Geography ------------------------------------------------------------------

test_that("place strings lead with the postcode and drop it for the fallback", {
  meta <- data.frame(
    geo_loc_name_postal_code = c("10115", NA, "75001"),
    geo_loc_name_city = c("Berlin", "Lyon", NA),
    geo_loc_name_state_province = c("Berlin", NA, NA),
    geo_loc_name_country = c("Germany", "France", "France"),
    stringsAsFactors = FALSE
  )
  expect_identical(
    impl$build_place_strings(meta),
    c("10115 Berlin, Berlin, Germany", "Lyon, France", "75001, France")
  )
  expect_identical(
    impl$build_place_strings(meta, postcode = FALSE),
    c("Berlin, Berlin, Germany", "Lyon, France", "France")
  )
})

test_that("the choropleth counts any recognised spelling of a country", {
  o <- list(
    mode = "Choropleth",
    region_transform = "Raw",
    region_fixed_scale = FALSE,
    region_scale = "Oranges",
    region_opacity = 0.7,
    region_border = "#000000",
    region_permanent = FALSE,
    legend = FALSE
  )
  coords <- coords_fixture()
  coords$geo_loc_name_country <- rep(c("Deutschland", "FR"), 3)
  m <- impl$build_choropleth(leaflet(), coords, o)
  text <- widget_text(m)
  expect_match(text, "Germany: 3 isolates", fixed = TRUE)
  expect_match(text, "France: 3 isolates", fixed = TRUE)
})

test_that("country_fit() flags swapped and out-of-country coordinates", {
  fit <- impl$country_fit(
    lat = c(52.52, 13.40, -33.9, 52.52),
    lon = c(13.40, 52.52, 18.4, 13.40),
    code = c("DEU", "DEU", "DEU", NA)
  )
  expect_identical(fit$swapped, c(FALSE, TRUE, FALSE, FALSE))
  expect_identical(fit$outside, c(FALSE, FALSE, TRUE, FALSE))
})

test_that("build_map_coords() keeps every recognised country as a region", {
  meta <- data.frame(
    isolate = c("A", "B", "C", "D"),
    geo_loc_name_country = c("Germany", "DE", "Atlantis", NA),
    # Every row carries coordinates or nothing to geocode: no network here.
    geo_loc_coordinates = c("52.52, 13.40", "13.40, 52.52", "0, 0", "garbage"),
    sample_collection_date = c("2026-01-01", "2026-01-02", NA, NA),
    stringsAsFactors = FALSE
  )
  built <- impl$build_map_coords(meta)
  expect_identical(built$coords$isolate, c("A", "B", "C"))
  expect_identical(built$regions$isolate, c("A", "B"))
  expect_identical(built$regions$country_code, c("DEU", "DEU"))
  s <- built$status
  expect_identical(s$n_unknown_country, 1L)
  expect_identical(s$n_bad_coords, 1L)
  expect_identical(s$n_swapped, 1L)
  # State-level rows are covered by their own test below.
  probs <- s$problems[s$problems$kind != "admin1_uncounted", , drop = FALSE]
  expect_identical(
    probs[, c("kind", "isolate", "value")],
    data.frame(
      kind = c("unknown_country", "bad_coords", "swapped"),
      isolate = c("C", "D", "B"),
      value = c("Atlantis", "garbage", "13.40, 52.52"),
      stringsAsFactors = FALSE
    )
  )
})

test_that("advisories stay short however many isolates are affected", {
  n <- 300L
  s <- impl$empty_geocode_status()
  s$problems <- impl$geo_problems(
    "bad_coords", sprintf("isolate_%03d", seq_len(n)), "Coordinates", "N/A?"
  )
  html <- as.character(impl$geo_advisories(s, "show_all"))
  expect_identical(lengths(regmatches(html, gregexpr("geo-tip-item", html))), 5L)
  expect_match(html, "and 295 more", fixed = TRUE)
  expect_match(html, "Show all 300 affected isolates", fixed = TRUE)
  table <- as.character(impl$geo_problem_table(s$problems))
  expect_identical(lengths(regmatches(table, gregexpr("<tr>", table))), n + 1L)
})

test_that("no advisories without problems", {
  expect_null(impl$geo_advisories(impl$empty_geocode_status(), "show_all"))
})

test_that("build_map() draws the choropleth from regions, not geocoded points", {
  o <- list(
    mode = "Choropleth",
    tiles = "OpenStreetMap",
    scalebar = FALSE,
    minimap = FALSE,
    graticule = FALSE,
    show_controls = FALSE,
    region_transform = "Raw",
    region_fixed_scale = FALSE,
    region_scale = "Oranges",
    region_opacity = 0.7,
    region_border = "#000000",
    region_permanent = FALSE,
    legend = FALSE
  )
  regions <- meta_fixture()
  regions$country_code <- ifelse(regions$geo_loc_name_country == "Germany", "DEU", "FRA")
  m <- impl$build_map(NULL, o, regions = regions)
  expect_match(widget_text(m), "Germany: 3 isolates", fixed = TRUE)
})

test_that("a choropleth with all isolates in one country still draws a legend scale", {
  expect_identical(impl$count_domain(c(NA, 94, NA)), c(0, 94))
  expect_identical(impl$count_domain(c(NA, 3, 10)), c(3, 10))
  o <- list(
    mode = "Choropleth",
    region_transform = "Raw",
    region_fixed_scale = FALSE,
    region_scale = "Oranges",
    region_opacity = 0.7,
    region_border = "#000000",
    region_permanent = FALSE,
    legend = TRUE,
    legend_pos = "topleft"
  )
  rows <- data.frame(isolate = paste0("i", 1:5), geo_loc_name_country = "Switzerland")
  m <- impl$build_choropleth(leaflet(), rows, o)
  legend <- Filter(function(x) x$method == "addLegend", m$x$calls)[[1]]$args[[1]]
  expect_identical(legend$type, "numeric")
  expect_identical(as.character(legend$labels[c(1, length(legend$labels))]), c("0", "5"))
})

test_that("isolates sharing one spot are grouped even with clustering off", {
  o <- list(
    cluster = FALSE,
    cluster_radius = 0,
    popup_fields = NULL,
    hover_field = NULL,
    permanent = FALSE,
    label_size = 12,
    marker_color = "#2c7fb8",
    radius = 10,
    region_fixed_scale = FALSE,
    legend = FALSE
  )
  m <- impl$build_markers(leaflet(), coords_fixture(), o)
  call <- Filter(function(x) x$method == "addCircleMarkers", m$x$calls)[[1]]
  opts <- Filter(function(a) is.list(a) && "maxClusterRadius" %in% names(a), call$args)[[1]]
  expect_identical(opts$maxClusterRadius, impl$COINCIDENT_PX)
  expect_true(opts$spiderfyOnMaxZoom)
})

test_that("pending geocodes count only places missing from the database cache", {
  db <- file.path(local_tempdir(), "t.db")
  build_db(db, default_local(), metadata = meta_df(c("A", "B")))
  meta <- data.frame(
    isolate = c("A", "B", "C"),
    geo_loc_name_city = c("Berlin", "Munich", "Paris"),
    geo_loc_name_country = c("Germany", "Germany", "France"),
    geo_loc_coordinates = c(NA, NA, "48.857, 2.352")
  )
  expect_identical(impl$geocode_pending_count(meta, db), 2L)
  write_geocodes(db, data.frame(place = "Berlin, Germany", latitude = 52.52, longitude = 13.4))
  expect_identical(impl$geocode_pending_count(meta, db), 1L)
})

test_that("isolates placed at city level get an info line naming them", {
  s <- impl$empty_geocode_status()
  s$problems <- impl$geo_problems(
    "postcode_fallback", c("iso_1", "iso_2"), "Postal code",
    c("99999, placed at Berlin, Germany", "00000, placed at Munich, Germany")
  )
  html <- as.character(impl$geo_advisories(s, "show_all"))
  expect_match(html, "2 isolate(s) placed at city level", fixed = TRUE)
  expect_match(html, "circle-info", fixed = TRUE)
  expect_match(html, "99999, placed at Berlin, Germany", fixed = TRUE)
  table <- as.character(impl$geo_problem_table(s$problems))
  expect_match(table, "Postcode not found, placed at city level", fixed = TRUE)
})

test_that("the geocoding estimate follows the measured pace", {
  first <- as.character(impl$geocode_waiter_html(52))
  expect_match(first, "Geocoding 52 locations", fixed = TRUE)
  expect_match(first, "data-total=\"78\"", fixed = TRUE)

  # 10 places took 30 s: 3 s each, so 42 left take 126 s, 156 s overall.
  later <- as.character(impl$geocode_waiter_html(52, 10L, 30))
  expect_match(later, "Geocoding location 11 of 52", fixed = TRUE)
  expect_match(later, "data-total=\"126\"", fixed = TRUE)
  expect_match(later, "Remaining time 2:06 / 2:36", fixed = TRUE)

  expect_match(as.character(impl$geocode_waiter_html(0)), "Preparing map", fixed = TRUE)
})

test_that("a fitted view keeps a margin around the outermost isolates", {
  coords <- data.frame(longitude = c(-120, 170), latitude = c(-40, 60))
  call <- impl$frame_coords(leaflet(), coords)$x$fitBounds
  expect_identical(call[[5]]$padding, c(impl$FIT_PADDING_PX, impl$FIT_PADDING_PX))
})

# --- State/province choropleth ---------------------------------------------------

# Every string each addPolygons() call carries, one entry per call, in order,
# leaving out the Show filter's mask.
polygon_texts <- function(m) {
  calls <- Filter(function(x) x$method == "addPolygons", m$x$calls)
  texts <- vapply(calls, function(x) paste(unlist(x$args), collapse = " "), "")
  texts[!grepl(impl$FOCUS_MASK_PANE, texts, fixed = TRUE)]
}

# Choropleth options at one region level, as map_opts() bundles them.
choro_opts <- function(level) {
  list(
    mode = "Choropleth",
    tiles = "OpenStreetMap",
    scalebar = FALSE,
    minimap = FALSE,
    graticule = FALSE,
    show_controls = FALSE,
    show_time_label = FALSE,
    region_level = level,
    region_transform = "Raw",
    region_fixed_scale = FALSE,
    region_scale = "Oranges",
    region_opacity = 0.7,
    region_border = "#000000",
    region_permanent = FALSE,
    legend = FALSE,
    legend_pos = "topleft"
  )
}

# The bundled admin-1 code of a German state, by its display name.
german_state <- function(name) {
  admin1 <- impl$get_admin1()
  admin1$adm1_code[admin1$adm0_a3 == "DEU" & admin1$name == name]
}

test_that("points are assigned to the state/province they fall in", {
  admin1 <- impl$get_admin1()
  codes <- impl$admin1_of(
    # Munich; 11 km off Barcelona's coast; 130 km out in the North Sea; no
    # point; Munich again (tested once, assigned to both).
    lat = c(48.14, 41.30, 54.5, NA, 48.14),
    lon = c(11.58, 2.30, 4.5, NA, 11.58)
  )
  expect_identical(
    admin1$name[match(codes, admin1$adm1_code)],
    c("Bavaria", "Barcelona", NA, NA, "Bavaria")
  )
})

test_that("build_map_coords() assigns states and reports the isolates it cannot count", {
  db <- file.path(local_tempdir(), "t.db")
  build_db(db, default_local(), metadata = meta_df(c("A", "B", "C")))
  # Served from the database's geocode cache, so no network is needed.
  write_geocodes(db, data.frame(place = "Germany", latitude = 51.0, longitude = 10.4))
  meta <- data.frame(
    isolate = c("A", "B", "C"),
    geo_loc_name_country = "Germany",
    geo_loc_coordinates = c("48.14, 11.58", NA, "54.5, 4.5"),
    stringsAsFactors = FALSE
  )
  built <- impl$build_map_coords(meta, db)
  expect_identical(built$coords$isolate, c("A", "B", "C"))
  expect_identical(built$coords$region_admin1, c(german_state("Bavaria"), NA, NA))
  # A country-only place is mapped at the country's centre but not counted in
  # whichever state that centre lies in.
  probs <- built$status$problems
  probs <- probs[probs$kind == "admin1_uncounted", , drop = FALSE]
  expect_identical(probs$isolate, c("B", "C"))
  expect_identical(probs$field, c("Country", "Coordinates"))
  expect_match(probs$value[1], "Germany (country only)", fixed = TRUE)
  expect_match(probs$value[2], "no state/province within 25 km", fixed = TRUE)
  expect_identical(built$status$n_admin1_uncounted, 2L)
  expect_identical(unique(built$shapes$admin1$adm0_a3), "DEU")
  # Filed for the Show filter under the country each point lies in: the
  # country-only centre is in Germany, the North Sea point in no country.
  expect_identical(built$coords$map_country, c("DEU", "DEU", NA))
})

test_that("state-level notes only show while the choropleth counts by state", {
  s <- impl$empty_geocode_status()
  s$problems <- rbind(
    impl$geo_problems("admin1_uncounted", "a", "Country", "Germany (country only)"),
    impl$geo_problems("bad_coords", "b", "Coordinates", "x")
  )
  expect_identical(impl$status_for_view(s, "Markers", "admin1")$problems$kind, "bad_coords")
  expect_identical(impl$status_for_view(s, "Choropleth", "country")$problems$kind, "bad_coords")
  shown <- impl$status_for_view(s, "Choropleth", "admin1")
  expect_identical(shown$problems$kind, c("admin1_uncounted", "bad_coords"))
  html <- as.character(impl$geo_advisories(shown, "show_all"))
  expect_match(html, "1 isolate(s) not counted by state/province", fixed = TRUE)
  expect_match(
    as.character(impl$geo_problem_table(shown$problems)),
    "Not counted by state/province",
    fixed = TRUE
  )
})

test_that("states are counted over every region of each country with isolates", {
  rows <- data.frame(
    isolate = paste0("i", 1:4),
    region_admin1 = c(german_state("Bavaria"), german_state("Bavaria"), german_state("Berlin"), NA)
  )
  shapes <- list(admin1 = impl$admin1_shapes(rows$region_admin1))
  expect_identical(nrow(shapes$admin1), 16L)
  expect_identical(unique(shapes$admin1$adm0_a3), "DEU")
  expect_null(impl$admin1_shapes(NA_character_))

  # Counts from the shown rows, the fixed scale's from all of them; zero-count
  # states stay drawn, uncounted (NA, drawn grey).
  units <- impl$choropleth_units("admin1", rows[2:3, ], rows, shapes)
  at <- match(c("Bavaria", "Berlin"), shapes$admin1$name)
  expect_identical(units$n[at], c(1L, 1L))
  expect_identical(units$full_n[at], c(2L, 1L))
  expect_identical(sum(is.na(units$n)), 14L)
  expect_identical(units$label[at[1]], "Bavaria, Germany")
})

test_that("build_map() draws the state level from the shown points, scaled over all", {
  coords <- data.frame(
    isolate = c("a", "b", "c"),
    sample_collection_date = c("2026-01-01", "2026-01-02", "2026-03-01"),
    latitude = 48.14,
    longitude = 11.58,
    region_admin1 = german_state("Bavaria"),
    stringsAsFactors = FALSE
  )
  # Every isolate in one state: the zero-width domain case.
  o <- choro_opts("admin1")
  o$legend <- TRUE
  o$region_fixed_scale <- TRUE
  o$date_field <- "sample_collection_date"
  o$date_bounds <- as.Date(c("2026-01-01", "2026-03-01"))
  o$daterange <- as.Date(c("2026-01-01", "2026-01-31"))
  regions <- coords
  regions$country_code <- "DEU"
  m <- impl$build_map(
    impl$filter_coords(coords, o),
    o,
    full_coords = coords,
    regions = regions,
    shapes = list(admin1 = impl$admin1_shapes(coords$region_admin1))
  )
  methods <- vapply(m$x$calls, `[[`, "", "method")
  expect_true(all(c("addPolygons", "addLegend") %in% methods))
  expect_match(widget_text(m), "Bavaria, Germany: 2 isolates", fixed = TRUE)
  # Every other country is drawn whole underneath, uncounted.
  polys <- polygon_texts(m)
  expect_identical(length(polys), 2L)
  expect_match(polys[[1]], "France", fixed = TRUE)
  expect_false(grepl("Germany", polys[[1]], fixed = TRUE))
  expect_false(grepl("France", polys[[2]], fixed = TRUE))
  legend <- m$x$calls[[which(methods == "addLegend")]]$args[[1]]
  expect_identical(as.character(legend$labels[c(1, length(legend$labels))]), c("0", "3"))

  # Nothing counted in the window at all: the scale still has a width.
  expect_identical(impl$count_domain(c(NA_integer_, NA_integer_)), c(0, 1))
})

test_that("a plot saved before the state level existed restores counting by country", {
  meta <- meta_fixture()
  sent <- new.env()
  testServer(
    visualization_map$server,
    args = list(viz_metadata = reactive(meta)),
    {
      root <- .subset2(session, "parent")
      root$sendInputMessage <- function(id, message) {
        assign(id, message, envir = sent)
      }
      restore(list(map_mode = "Choropleth", map_region_scale = "Blues"))
      expect_identical(sent$map_region_level$selected, "country")
      restore(list(map_mode = "Choropleth", map_region_level = "admin1"))
      expect_identical(sent$map_region_level$selected, "admin1")
    }
  )
})

# --- Show filter ------------------------------------------------------------------

test_that("the Show filter offers every country with its isolate count", {
  choices <- impl$focus_choices(c(DEU = 2L, FRA = 1L, JPN = 4L))
  expect_true(inherits(choices, "vs_choices"))
  groups <- choices$choices
  continents <- vapply(groups, `[[`, "", "label")
  europe <- groups[[match("Europe", continents)]]
  africa <- groups[[match("Africa", continents)]]
  # Countries without isolates are offered too, counted 0.
  offered <- unlist(lapply(groups, function(g) g$options$value))
  expect_identical(length(offered), nrow(impl$get_world()))
  expect_true(all(c("DZA", "TCD") %in% africa$options$value))
  expect_identical(africa$customData$count, 0L)
  expect_identical(europe$customData$count, 3L)
  at <- match(c("France", "Germany", "Spain"), europe$options$label)
  expect_identical(europe$options$value[at], c("FRA", "DEU", "ESP"))
  counts <- vapply(europe$options$customData[at], `[[`, 0L, "count")
  expect_identical(counts, c(1L, 2L, 0L))
  expect_false(is.unsorted(europe$options$label))
})

test_that("each isolate is counted once, under its point's country first", {
  coords <- data.frame(isolate = c("a", "b"), map_country = c("FRA", NA))
  regions <- data.frame(isolate = c("a", "c", "d"), country_code = c("DEU", "DEU", "ESP"))
  expect_identical(impl$focus_counts(coords, regions), c(DEU = 1L, ESP = 1L, FRA = 1L))
  expect_identical(impl$focus_counts(NULL, regions), c(DEU = 2L, ESP = 1L))
  expect_identical(impl$focus_counts(coords, NULL), c(FRA = 1L))
})

test_that("the Show filter keeps the rows of the selected countries", {
  rows <- data.frame(
    isolate = c("a", "b", "c"),
    map_country = c("DEU", "FRA", NA),
    country_code = c("DEU", "DEU", "FRA")
  )
  expect_identical(impl$focus_rows(rows, character(0))$isolate, c("a", "b", "c"))
  expect_identical(impl$focus_rows(rows, NULL)$isolate, c("a", "b", "c"))
  expect_identical(impl$focus_rows(rows, "FRA")$isolate, "b")
  expect_identical(impl$focus_rows(rows, "FRA", "country_code")$isolate, "c")
})

test_that("a Show selection limits what every choropleth level draws", {
  regions <- meta_fixture()
  regions$country_code <- ifelse(regions$geo_loc_name_country == "Germany", "DEU", "FRA")
  o <- choro_opts("country")
  o$focus <- "FRA"
  polys <- polygon_texts(impl$build_map(NULL, o, regions = regions))
  expect_identical(length(polys), 1L)
  expect_match(polys[[1]], "France: 3 isolates", fixed = TRUE)
  expect_false(grepl("Germany", polys[[1]], fixed = TRUE))

  # State level: Germany's states, and only France as an uncounted backdrop.
  coords <- data.frame(
    isolate = "a",
    latitude = 48.14,
    longitude = 11.58,
    region_admin1 = german_state("Bavaria"),
    map_country = "DEU"
  )
  o <- choro_opts("admin1")
  o$focus <- c("DEU", "FRA")
  shapes <- list(admin1 = impl$admin1_shapes(coords$region_admin1))
  polys <- polygon_texts(impl$build_map(coords, o, full_coords = coords, shapes = shapes))
  expect_identical(length(polys), 2L)
  expect_match(polys[[1]], "France", fixed = TRUE)
  expect_false(grepl("Spain", polys[[1]], fixed = TRUE))
  expect_match(polys[[2]], "Bavaria, Germany: 1 isolates", fixed = TRUE)

  # Selecting only France leaves no state polygons, just France's outline.
  o$focus <- "FRA"
  polys <- polygon_texts(impl$build_map(coords, o, full_coords = coords, shapes = shapes))
  expect_identical(length(polys), 1L)
  expect_false(grepl("Bavaria", polys[[1]], fixed = TRUE))
})

test_that("a choropleth still draws its regions for an empty date window", {
  o <- choro_opts("admin1")
  coords <- data.frame(
    isolate = "a",
    latitude = 48.14,
    longitude = 11.58,
    region_admin1 = german_state("Bavaria")
  )
  shapes <- list(admin1 = impl$admin1_shapes(coords$region_admin1))
  polys <- polygon_texts(impl$build_map(coords[0, ], o, full_coords = coords, shapes = shapes))
  expect_identical(length(polys), 2L)
  expect_match(polys[[2]], "Bavaria, Germany: 0 isolates", fixed = TRUE)
})

test_that("a plot saved before the Show filter restores showing the world", {
  meta <- meta_fixture()
  sent <- new.env()
  testServer(
    visualization_map$server,
    args = list(viz_metadata = reactive(meta)),
    {
      root <- .subset2(session, "parent")
      root$sendInputMessage <- function(id, message) {
        assign(id, message, envir = sent)
      }
      restore(list(map_mode = "Markers"))
      expect_identical(sent$map_focus$value, character(0))
      restore(list(map_mode = "Markers", map_focus = list("DEU", "FRA")))
      expect_identical(sent$map_focus$value, c("DEU", "FRA"))
    }
  )
})

test_that("a Show selection masks every country outside it", {
  expect_null(impl$focus_mask(character(0)))
  expect_null(impl$focus_mask("XXX"))
  # Every country selected is the world: no mask.
  expect_null(impl$focus_mask(impl$get_world()$adm0_a3))
  # The window reaches past the coast but not across a land border, and an
  # unselected enclave stays hidden.
  inside <- function(focus, lon, lat) {
    pts <- st_sfc(lapply(seq_along(lon), function(i) st_point(c(lon[i], lat[i]))))
    lengths(st_intersects(pts, impl$focus_window(focus))) > 0
  }
  # Johannesburg; 10 km off Durban; Maseru (Lesotho); Gaborone (Botswana).
  lon <- c(28.05, 31.15, 27.48, 25.91)
  lat <- c(-26.20, -29.86, -29.31, -24.65)
  expect_identical(inside("ZAF", lon, lat), c(TRUE, TRUE, FALSE, FALSE))
  expect_identical(inside(c("ZAF", "LSO"), lon, lat), c(TRUE, TRUE, TRUE, FALSE))

  o <- choro_opts("country")
  o$mode <- "Markers"
  o$focus <- "DEU"
  # The frame reaches past +-180, which sf warns about on a long/lat layer.
  expect_no_warning(m <- impl$build_map(NULL, o))
  expect_identical(length(polygon_texts(m)), 0L)
  expect_identical(sum(vapply(m$x$calls, `[[`, "", "method") == "addPolygons"), 1L)
  expect_match(widget_text(m), impl$FOCUS_MASK_PANE, fixed = TRUE)
  expect_match(widget_text(m), impl$FOCUS_MASK_COLOR, fixed = TRUE)
  # Its own padded renderer, redrawn while the map pans or flies, so no tiles
  # show past it mid-gesture.
  expect_match(widget_text(m), impl$FOCUS_MASK_RENDERER, fixed = TRUE)
  hooks <- vapply(m$jsHooks$render, `[[`, "", "code")
  expect_true(impl$FOCUS_MASK_FOLLOW_JS %in% hooks)
  o$focus <- NULL
  expect_false(grepl(impl$FOCUS_MASK_PANE, widget_text(impl$build_map(NULL, o)), fixed = TRUE))
})

test_that("re-clustering charts on zoom keeps the one legend", {
  # Each zoom redraws the charts through a proxy; drawing the legend there too
  # stacked a new copy per zoom.
  meta <- meta_fixture()
  sent <- new.env()
  sent$methods <- character(0)
  testServer(
    visualization_map$server,
    args = list(viz_metadata = reactive(meta)),
    {
      root <- .subset2(session, "parent")
      root$sendCustomMessage <- function(type, message) {
        if (identical(type, "leaflet-calls")) {
          sent$methods <- c(sent$methods, vapply(message$calls, `[[`, "", "method"))
        }
      }
      session$setInputs(
        map_layer_add = "specimen",
        map_mode = "Charts",
        map_chart_cluster_radius = 100,
        map_legend = TRUE
      )
      map_coords(coords_fixture())
      session$elapse(300)
      session$setInputs(map_zoom = 5)
      session$elapse(300)
      session$setInputs(map_zoom = 6)
      session$elapse(300)
      expect_identical(sum(sent$methods == "addMinicharts"), 2L)
      expect_false("addLegend" %in% sent$methods)
    }
  )
})
