# app/view/visualization_map.R
#
# Geographic (leaflet) visualization submodule. Owns its own right-hand control
# sidebar, the map output and all map-specific reactive state. Mounted by
# app/view/visualization.R inside a navset_hidden panel; the shared Generate
# button, plot type, session reset and per-isolate metadata are forwarded in as
# reactives — the same contract the MST and Tree engines use. Isolates are
# placed by geo_loc_coordinates where readable (app/logic/geo.R), otherwise by
# their postcode + city + state/province + country, geocoded once per distinct
# place via OSM/Nominatim on Generate. Countries resolve locally against the
# Natural Earth vocabulary, so the country choropleth needs no geocoding at all;
# the state/province choropleth counts each isolate in the bundled Natural Earth
# admin-1 region its point falls in (dev/build_admin1.R), assigned on Generate.
#
# Four map modes share one builder (`build_map`): Markers (styled points),
# Choropleth (countries or states/provinces shaded by isolate count),
# Heatmap (point density) and Charts (a minichart per location). Rendering is a
# single renderLeaflet reacting to the geocoded coords and a debounced bundle of
# all control inputs (`map_opts`); the same builder backs the HTML export. The
# map itself stays hidden (behind the "press Generate" prompt) until the first
# Generate, so the base tiles and the coordinates always appear together.

box::use(
  bslib,
  curl[nslookup],
  htmltools[HTML],
  htmlwidgets[JS, onRender, saveWidget],
  httr[timeout, with_config],
  leaflet,
  leaflet.extras[addFullscreenControl, addHeatmap],
  leaflet.minicharts[addMinicharts, clearMinicharts],
  rlang[`%||%`],
  shiny,
  shinyWidgets[
    pickerInput,
    radioGroupButtons,
    updatePickerInput,
    updateRadioGroupButtons,
    updateVirtualSelect,
    virtualSelectInput,
  ],
  shinyjs[runjs, toggle, toggleClass, toggleState, useShinyjs],
  stats[aggregate, setNames],
  tidygeocoder[geocode],
  utils[head, tail],
  waiter[Waiter, spin_flower, useWaiter],
)
box::use(
  app /
    logic /
    date_bins[
      bin_date_values,
      date_extent,
      date_precision,
      first_dated_field,
      in_date_range,
      is_binned,
      parse_dates,
      timeline_fields
    ],
  app / logic / db_events,
  app / logic / field_labels[field_label],
  app /
    logic /
    field_profile[
      field_profiles_of = field_profiles,
      profile_for,
      scale_categories_for,
    ],
  app / logic / functions[render_info],
  app / logic / geo[country_code, format_coordinates, parse_coordinates],
  app / logic / geocode_cache[clear_geocodes, read_geocodes, write_geocodes],
  app /
    logic /
    mapping_engine[
      aesthetic_block_reason,
      assign_mapping_layer,
      granularity_profile,
      is_date_profile,
      max_layers,
      rebalance_layers,
      set_layer_granularity,
    ],
  app / logic / viz_helpers,
  app /
    logic /
    viz_layers[
      drop_layer,
      find_layer,
      layer_cards,
      layer_defaults,
      layer_has_field,
      layer_id_source,
      normalize_layers,
    ],
)

# Basemap providers offered in the Basemap select. Only ones that serve tiles
# without an API key: CARTO's basemaps now stamp "API KEY REQUIRED" across every
# tile requested without one, so they are not offered.
map_providers <- c(
  "OpenStreetMap" = "OpenStreetMap",
  "OSM Humanitarian" = "OpenStreetMap.HOT",
  "OSM (German style)" = "OpenStreetMap.DE",
  "OpenTopoMap" = "OpenTopoMap",
  "Esri Satellite" = "Esri.WorldImagery",
  "Esri Topographic" = "Esri.WorldTopoMap",
  "Esri Streets" = "Esri.WorldStreetMap",
  "Esri NatGeo" = "Esri.NatGeoWorldMap",
  "Esri Gray Canvas" = "Esri.WorldGrayCanvas"
)

# Basemap auto-selected when the user switches into each map mode (a sensible
# starting point per mode; the user can still change it afterward).
mode_tile_defaults <- c(
  Markers = "OpenStreetMap",
  Choropleth = "Esri.WorldGrayCanvas",
  Heatmap = "Esri.WorldGrayCanvas",
  Charts = "Esri.WorldGrayCanvas"
)

# Choropleth basemap handling: a busy — or even muted — basemap underneath a
# color fill still reads as "textured" wherever the fill isn't fully opaque,
# so Choropleth mode renders no base tile layer at all (the "Base map" picker
# is hidden for this mode; see choro_hide in map_controls()). Place names are
# still useful for context, though, so Esri's labels-only reference layer for
# its gray canvas is placed in its own pane stacked above the polygon overlay.
# It is not in leaflet.providers, hence a URL rather than a provider name.
choropleth_labels_url <- paste0(
  "https://server.arcgisonline.com/ArcGIS/rest/services/Canvas/",
  "World_Light_Gray_Reference/MapServer/tile/{z}/{y}/{x}"
)
choropleth_labels_attribution <- "Tiles &copy; Esri &mdash; Esri, DeLorme, NAVTEQ"

# --- Variable mapping and control defaults -----------------------------------

# The medium this engine maps onto, in app/logic/mapping_engine.R's terms: one
# colour channel that Markers draw as the point fill and Charts as the slices.
MEDIUM <- "map"

# Canonical shape of one mapping layer, shared with the other engines' snapshot
# and restore paths.
LAYER_DEFAULTS <- layer_defaults(MEDIUM)

# Isolates whose mapped value is missing are always drawn in this grey, in every
# mode that draws a variable.
NA_COLOR <- "#808080"

# Charts mode's fold-into-"Other" bucket (build_charts()) gets this fixed
# light grey, distinct from NA_COLOR so a chart with both a real "Missing"
# slice and an "Other" slice can still tell them apart.
OTHER_COLOR <- "#BFBFBF"

# Popup and hover pickers take at most this many fields: past that a popup is
# taller than the map it opens over.
MAX_LABEL_FIELDS <- 20L

POPUP_DEFAULT <- c("isolate", "place", "sample_collection_date")
HOVER_DEFAULT <- "isolate"

# Every control the sidebar renders, filed under the family whose update path
# restores it (see control_families() and the reset reference at the top of
# viz_helpers.R). Absent on purpose: `map_layer_add` clears itself the moment
# it is picked from, and `map_daterange` is fitted to the loaded data rather
# than declared, so it is refitted after the catalogue rather than reset by it.
MAP_CONTROLS <- viz_helpers$control_families(
  switches = c(
    "map_scalebar",
    "map_minimap",
    "map_graticule",
    "map_show_controls",
    "map_legend",
    "map_permanent",
    "map_show_time_label",
    "map_region_fixed_scale",
    "map_region_permanent"
  ),
  pickers = c(
    "map_mode",
    "map_tiles",
    "map_legend_pos",
    "map_region_scale",
    "map_heat_scale",
    "map_chart_type"
  ),
  virtual_selects = c("map_focus", "map_popup", "map_hover_field"),
  sliders = c(
    "map_cluster_radius",
    "map_radius",
    "map_legend_opacity",
    "map_label_size",
    "map_region_opacity",
    "map_heat_radius",
    "map_heat_max",
    "map_chart_size",
    "map_chart_opacity",
    "map_chart_cluster_radius",
    "map_chart_max_categories"
  ),
  numerics = "map_legend_digits",
  texts = "map_legend_title",
  radio_groups = c("map_interval", "map_region_level", "map_region_transform"),
  colors = c("map_marker_color", "map_region_border")
)

# Every catalogued control's coded default. The widgets below are declared with
# these same values.
map_control_defaults <- function() {
  list(
    map_mode = "Markers",
    # Nothing ticked: the whole world.
    map_focus = character(0),
    map_tiles = mode_tile_defaults[["Markers"]],
    map_scalebar = TRUE,
    map_minimap = TRUE,
    map_graticule = FALSE,
    map_show_controls = TRUE,
    map_cluster_radius = 0,
    map_radius = 10,
    map_marker_color = "#2C7FB8C7",
    map_legend = TRUE,
    map_legend_pos = "topleft",
    map_legend_title = "",
    map_legend_opacity = 0.85,
    map_legend_digits = 2,
    map_popup = POPUP_DEFAULT,
    map_hover_field = HOVER_DEFAULT,
    map_permanent = FALSE,
    map_label_size = 12,
    map_interval = "Day",
    map_show_time_label = TRUE,
    map_region_fixed_scale = TRUE,
    map_region_level = "country",
    map_region_transform = "Raw",
    map_region_scale = "Oranges",
    map_region_opacity = 0.7,
    map_region_border = "#000000",
    map_region_permanent = FALSE,
    map_heat_radius = 25,
    # Refitted to the busiest place after the catalogue is applied.
    map_heat_max = 10,
    map_heat_scale = "Oranges",
    map_chart_type = "pie",
    map_chart_size = 40,
    map_chart_opacity = 0.7,
    map_chart_cluster_radius = 100,
    map_chart_max_categories = 0
  )
}

# Margin, in pixels, kept between the outermost isolates and the map's edge
# when the view is fitted to them, so edge markers and clusters stay whole.
FIT_PADDING_PX <- 50

# Natural Earth country and admin-1 polygons, loaded once per process and
# cached for the choropleth.
.world_env <- new.env(parent = emptyenv())
# sf and rnaturalearth are imported here rather than at the top so they load on
# the first map generation, not at app startup.
get_world <- function() {
  if (is.null(.world_env$sf)) {
    box::use(
      rnaturalearth[ne_countries],
    )
    .world_env$sf <- ne_countries(
      scale = 50,
      returnclass = "sf"
    )
  }
  .world_env$sf
}

# Levels the choropleth counts at, as offered in the Region tab. "country"
# joins each isolate's stated country to Natural Earth admin-0; "admin1" counts
# it in the state/province its point falls in. A finer level (admin-2) would be
# one more entry here, a column in build_map_coords() and a branch in
# choropleth_units().
REGION_LEVELS <- c("Country" = "country", "State / province" = "admin1")

# Natural Earth admin-1 regions (adm1_code, name, adm0_a3, country, geometry),
# bundled by dev/build_admin1.R. Static reference data, so the process-level
# cache is safe across session restarts. sf is imported here so it loads on the
# first Generate, not at app startup; it also registers the sf methods the
# stored object needs.
get_admin1 <- function() {
  if (is.null(.world_env$admin1)) {
    box::use(
      sf[st_crs],
    )
    shapes <- readRDS(box::file("../logic/data/admin1.rds"))
    stopifnot(identical(st_crs(shapes)$epsg, 4326L))
    .world_env$admin1 <- shapes
  }
  .world_env$admin1
}

# The admin-1 region (adm1_code) each point falls in. A point outside every
# region (just offshore, or in a hairline gap between simplified neighbours) is
# snapped to the nearest one within COUNTRY_TOLERANCE_M; beyond that it is NA.
# Distinct points are tested once, as isolates often share a geocoded place.
admin1_of <- function(lat, lon) {
  box::use(
    sf[st_as_sf, st_distance, st_intersects, st_nearest_feature],
  )
  out <- rep(NA_character_, length(lat))
  ok <- which(!is.na(lat) & !is.na(lon))
  if (!length(ok)) {
    return(out)
  }
  admin1 <- get_admin1()
  spot <- paste(lat[ok], lon[ok])
  first <- !duplicated(spot)
  pts <- st_as_sf(
    data.frame(lon = lon[ok][first], lat = lat[ok][first]),
    coords = c("lon", "lat"),
    crs = 4326
  )
  hit <- vapply(st_intersects(pts, admin1), function(i) i[1], integer(1))
  away <- which(is.na(hit))
  if (length(away)) {
    nearest <- st_nearest_feature(pts[away, ], admin1)
    dist <- as.numeric(st_distance(pts[away, ], admin1[nearest, ], by_element = TRUE))
    close <- dist <= COUNTRY_TOLERANCE_M
    hit[away[close]] <- nearest[close]
  }
  out[ok] <- admin1$adm1_code[hit][match(spot, spot[first])]
  out
}

# The admin-1 regions a state-level choropleth draws for these assigned codes:
# every region of each country holding at least one of them, so a country is
# shown whole, zero-count regions included. NULL when nothing was assigned.
admin1_shapes <- function(codes) {
  codes <- unique(codes[!is.na(codes)])
  if (!length(codes)) {
    return(NULL)
  }
  admin1 <- get_admin1()
  countries <- unique(admin1$adm0_a3[admin1$adm1_code %in% codes])
  admin1[admin1$adm0_a3 %in% countries, ]
}

# --- coordinate resolution ---------------------------------------------------

# Empty/no-op geocoding status, shared by every early-exit below so callers
# always get a status object back even when nothing was mappable.
empty_geocode_status <- function() {
  list(
    n_isolates = 0L,
    n_mapped = 0L,
    n_locations = 0L,
    n_cached = 0L,
    n_new = 0L,
    n_failed_places = 0L,
    failed_preview = "",
    # Every fresh lookup failed and Nominatim's host does not resolve.
    offline = FALSE,
    # The geocoder's own error, when it aborted rather than missing places.
    geocode_error = "",
    # Coordinates that could not be read, so the row fell back to geocoding.
    n_bad_coords = 0L,
    # Country spellings outside the vocabulary (no choropleth region).
    n_unknown_country = 0L,
    # Coordinates off their isolate's stated country (see country_fit()).
    n_outside = 0L,
    n_swapped = 0L,
    # Isolates placed at city level because their postcode was not found.
    n_postcode_fallback = 0L,
    # Isolates with a location but no admin-1 region (no point, a point naming
    # only the country, or a point at sea): not counted at state level.
    n_admin1_uncounted = 0L,
    # One row per affected isolate and problem, behind the advisories' tooltips
    # and the full list in the "Show all" dialog (see geo_problem_table()).
    problems = geo_problems()
  )
}

# Kinds of geographic data note, in the order the "Show all" table sorts them.
# Unmapped isolates have no advisory line of their own (the headline counts
# them) but are listed in the table too.
GEO_PROBLEM_KINDS <- c(
  "not_found", "postcode_fallback", "unknown_country", "bad_coords", "swapped", "outside",
  "admin1_uncounted"
)

# Problem rows for one kind: the isolate, the field and the value as entered.
geo_problems <- function(kind = character(0), isolate = character(0),
                         field = character(0), value = character(0)) {
  data.frame(
    kind = rep(kind, length.out = length(isolate)),
    isolate = as.character(isolate),
    field = rep(field, length.out = length(isolate)),
    value = as.character(value),
    stringsAsFactors = FALSE
  )
}

# How far (metres) a point may lie outside its country's 1:50m outline and
# still count as inside it: coastal and island sites routinely fall just off a
# generalised coastline.
COUNTRY_TOLERANCE_M <- 25000

# Longest a single geocoding request may take, in seconds. tidygeocoder sets no
# timeout of its own on single lookups, so a stalled connection would otherwise
# hold the (blocking) Generate indefinitely.
GEOCODE_TIMEOUT_S <- 15

# Up to this many failed place strings are named in the sidebar feedback;
# beyond that it just adds a "+N more" tail rather than growing unbounded.
max_failed_preview <- 5L

# Format the geocode failure summary shown in the sidebar: the distinct place
# strings Nominatim couldn't resolve, truncated to max_failed_preview.
format_failed_preview <- function(failed_places) {
  if (!length(failed_places)) {
    return("")
  }
  shown <- failed_places[seq_len(min(
    max_failed_preview,
    length(failed_places)
  ))]
  extra <- length(failed_places) - length(shown)
  txt <- paste(shown, collapse = "; ")
  if (extra > 0) {
    txt <- paste0(txt, sprintf(" (+%d more)", extra))
  }
  txt
}

# Advisory lines for the sidebar status: data problems a Generate found that
# do not stop the map but make it less true than it looks. Each names what was
# affected in its tooltip and how to fix it, and `details_id` adds a link to
# the full list. NULL when there is nothing to say.
geo_advisories <- function(s, details_id = NULL) {
  probs <- s$problems
  if (is.null(probs) || !nrow(probs)) {
    return(NULL)
  }
  # One line per kind; its tooltip explains the problem and names the first
  # few isolates with the value as entered, one per line. `info` lines report
  # something worth knowing that is not a data error.
  line <- function(kind, text, tip, info = FALSE) {
    rows <- probs[probs$kind == kind, , drop = FALSE]
    if (!nrow(rows)) {
      return(NULL)
    }
    shown <- head(rows, max_failed_preview)
    items <- lapply(seq_len(nrow(shown)), function(i) {
      shiny$div(
        class = "geo-tip-item",
        shiny$tags$b(shown$isolate[i]),
        shiny$tags$br(),
        if (nzchar(shown$value[i])) shown$value[i] else "(empty)"
      )
    })
    more <- nrow(rows) - nrow(shown)
    bslib$tooltip(
      shiny$div(
        class = if (info) "small text-muted mb-1" else "small text-warning mb-1",
        shiny$icon(if (info) "circle-info" else "triangle-exclamation"),
        sprintf(text, nrow(rows))
      ),
      shiny$div(tip),
      items,
      if (more > 0) shiny$div(class = "geo-tip-more", sprintf("and %d more", more)),
      options = list(customClass = "geo-advisory-tip")
    )
  }
  lines <- Filter(Negate(is.null), list(
    line(
      "postcode_fallback",
      " %d isolate(s) placed at city level",
      paste(
        "The geocoder did not know their postcode, so they were placed by",
        "city, state and country instead."
      ),
      info = TRUE
    ),
    line(
      "unknown_country",
      " %d isolate(s) name an unrecognised country",
      paste(
        "Not counted on the choropleth. Pick the country from the list in the",
        "Database browser or use its ISO code."
      )
    ),
    line(
      "bad_coords",
      " %d coordinate value(s) unreadable",
      paste(
        "Placed by their place names instead. Write latitude, longitude,",
        "e.g. 52.52, 13.40."
      )
    ),
    line(
      "swapped",
      " %d isolate(s) with latitude and longitude swapped",
      paste(
        "Their coordinates only fall inside the stated country with the two",
        "values exchanged."
      )
    ),
    line(
      "outside",
      " %d isolate(s) placed outside their country",
      sprintf(
        "Their coordinates lie more than %g km outside the stated country.",
        COUNTRY_TOLERANCE_M / 1000
      )
    ),
    line(
      "admin1_uncounted",
      " %d isolate(s) not counted by state/province",
      paste(
        "They have no point inside a state or province: their place could not",
        "be geocoded, names only the country, or lies at sea. Add coordinates",
        "or a city, postcode or state/province."
      )
    )
  ))
  n_isolates <- length(unique(probs$isolate))
  shiny$tagList(
    lines,
    if (!is.null(details_id)) {
      shiny$actionLink(
        details_id,
        sprintf("Show all %d affected isolate%s", n_isolates, if (n_isolates == 1) "" else "s"),
        class = "small geo-problems-link"
      )
    }
  )
}

# Short label of each problem kind, for the "Show all" dialog's table.
GEO_PROBLEM_LABELS <- c(
  not_found = "Place not found, not mapped",
  postcode_fallback = "Postcode not found, placed at city level",
  unknown_country = "Unrecognised country",
  bad_coords = "Unreadable coordinates",
  swapped = "Latitude and longitude swapped",
  outside = "Outside the stated country",
  admin1_uncounted = "Not counted by state/province"
)

# Every affected isolate as a table, sorted by problem kind then isolate.
geo_problem_table <- function(probs) {
  probs <- probs[order(match(probs$kind, GEO_PROBLEM_KINDS), probs$isolate), , drop = FALSE]
  shiny$div(
    class = "geo-problem-table",
    shiny$tags$table(
      class = "table table-sm table-striped",
      shiny$tags$thead(shiny$tags$tr(
        shiny$tags$th("Isolate"),
        shiny$tags$th("Problem"),
        shiny$tags$th("Field"),
        shiny$tags$th("Value as entered")
      )),
      shiny$tags$tbody(lapply(seq_len(nrow(probs)), function(i) {
        shiny$tags$tr(
          shiny$tags$td(probs$isolate[i]),
          shiny$tags$td(GEO_PROBLEM_LABELS[[probs$kind[i]]]),
          shiny$tags$td(probs$field[i]),
          shiny$tags$td(probs$value[i])
        )
      }))
    )
  )
}

# The status as the current view needs it. State-level rows only matter while
# the choropleth counts by state/province; elsewhere they would flag every
# isolate that names just its country, so they are left out of the advisories
# and the "Show all" dialog.
status_for_view <- function(s, mode, level) {
  if (is.null(s$problems) || (identical(mode, "Choropleth") && identical(level, "admin1"))) {
    return(s)
  }
  s$problems <- s$problems[s$problems$kind != "admin1_uncounted", , drop = FALSE]
  s
}

# Join postcode/city/state/country (most specific first) into one place string
# per isolate row, so Nominatim resolves each to the finest available point;
# the postcode leads its city ("10115 Berlin, Berlin, Germany"), as addresses
# write it. `postcode = FALSE` gives the coarser string build_map_coords()
# retries with. Shared with geocode_pending_count() (the pre-flight count shown
# in the waiter), so the two can never disagree about which strings get
# geocoded.
build_place_strings <- function(meta, postcode = TRUE) {
  col <- function(name) {
    v <- if (name %in% names(meta)) {
      meta[[name]]
    } else {
      rep(NA_character_, nrow(meta))
    }
    v <- trimws(as.character(v))
    v[is.na(v)] <- ""
    v
  }
  city <- col("geo_loc_name_city")
  if (postcode) {
    city <- trimws(paste(col("geo_loc_name_postal_code"), city))
  }
  state <- col("geo_loc_name_state_province")
  country <- col("geo_loc_name_country")
  vapply(
    seq_len(nrow(meta)),
    function(i) {
      parts <- c(city[i], state[i], country[i])
      paste(parts[nzchar(parts)], collapse = ", ")
    },
    character(1)
  )
}

# Which explicit coordinates fall more than COUNTRY_TOLERANCE_M outside the
# Natural Earth outline of the country their isolate names, and which of those
# would land inside with latitude and longitude swapped, the commonest entry
# slip. Tested one country at a time against its own polygon only. Purely
# advisory: a geometry failure reports nothing rather than blocking the map.
country_fit <- function(lat, lon, code) {
  box::use(
    sf[st_as_sf, st_is_within_distance],
  )
  n <- length(lat)
  out <- list(outside = rep(FALSE, n), swapped = rep(FALSE, n))
  world <- get_world()
  check <- which(!is.na(lat) & !is.na(lon) & code %in% world$adm0_a3)
  if (!length(check)) {
    return(out)
  }
  # Whether each point lies within reach of its country's polygon.
  near <- function(la, lo, cc) {
    hit <- rep(FALSE, length(la))
    for (country in unique(cc)) {
      sel <- which(cc == country)
      pts <- st_as_sf(
        data.frame(lon = lo[sel], lat = la[sel]),
        coords = c("lon", "lat"),
        crs = 4326
      )
      reach <- st_is_within_distance(
        pts,
        world[world$adm0_a3 == country, ],
        dist = COUNTRY_TOLERANCE_M
      )
      hit[sel] <- lengths(reach) > 0
    }
    hit
  }
  tryCatch(
    {
      away <- check[!near(lat[check], lon[check], code[check])]
      flippable <- away[abs(lon[away]) <= 90]
      swapped <- flippable[near(
        lon[flippable],
        lat[flippable],
        code[flippable]
      )]
      out$swapped[swapped] <- TRUE
      out$outside[setdiff(away, swapped)] <- TRUE
      out
    },
    error = function(e) out
  )
}

# --- geocode cache -----------------------------------------------------------

# Resolved places are kept in the loaded database (app/logic/geocode_cache.R),
# so they travel with it. A place string's coordinates do not change, so entries
# never expire. Only SUCCESSFUL lookups are stored: a place that failed (often a
# passing network issue) is retried on the next Generate. Reusing resolved
# coordinates is also what the OSM/Nominatim usage policy asks for.

# Resolve a set of distinct place strings to coordinates, cache-first: places
# already in the database's cache are served from it, and only the rest are sent
# to Nominatim. Newly resolved places are appended to the cache. Returns a list
# with `located` (one row per distinct place — place, latitude, longitude, with
# NA coords where a lookup failed), `n_cached` (distinct places served from the
# cache), `n_new` (distinct places freshly geocoded this call), `offline` and
# `error` (see empty_geocode_status()), for the geocode-status feedback. A
# geocoder failure leaves its places unresolved instead of aborting, so
# isolates with explicit coordinates and cached places still reach the map.
# `progress`, when given, is called before each lookup with the number done,
# the number to do and the seconds spent so far.
geocode_places_cached <- function(place_vec, db_path = NULL, progress = NULL) {
  places <- unique(place_vec[!is.na(place_vec) & nzchar(place_vec)])
  # rep() rather than a scalar NA: with no places (every isolate carries its
  # own coordinates) a scalar makes data.frame() fail on 0 vs 1 rows.
  located <- data.frame(
    place = places,
    latitude = rep(NA_real_, length(places)),
    longitude = rep(NA_real_, length(places)),
    stringsAsFactors = FALSE
  )
  if (!length(places)) {
    return(list(
      located = located,
      n_cached = 0L,
      n_new = 0L,
      offline = FALSE,
      error = ""
    ))
  }

  cache <- read_geocodes(db_path)
  hit <- match(located$place, cache$place)
  located$latitude <- cache$latitude[hit]
  located$longitude <- cache$longitude[hit]
  n_cached <- sum(!is.na(hit))

  todo <- located$place[is.na(hit)]
  n_new <- 0L
  offline <- FALSE
  error <- ""
  if (length(todo)) {
    fresh <- data.frame(
      place = todo,
      latitude = rep(NA_real_, length(todo)),
      longitude = rep(NA_real_, length(todo)),
      stringsAsFactors = FALSE
    )
    # One place per request, so a slow or stalled one costs at most
    # GEOCODE_TIMEOUT_S and progress can be reported between them. tidygeocoder
    # keeps Nominatim's one-request-per-second limit across calls.
    started <- Sys.time()
    checked_network <- FALSE
    for (i in seq_along(todo)) {
      if (is.function(progress)) {
        progress(i - 1L, length(todo), as.numeric(difftime(Sys.time(), started, units = "secs")))
      }
      res <- tryCatch(
        with_config(
          timeout(GEOCODE_TIMEOUT_S),
          geocode(
            fresh[i, "place", drop = FALSE],
            place,
            method = "osm",
            lat = "latitude",
            long = "longitude",
            quiet = TRUE,
            progress_bar = FALSE
          )
        ),
        error = function(e) {
          error <<- conditionMessage(e)
          NULL
        }
      )
      if (!is.null(res) && nrow(res)) {
        fresh$latitude[i] <- res$latitude[1]
        fresh$longitude[i] <- res$longitude[1]
      }
      # A failure is usually the data, but the first one checks whether the
      # service is reachable at all: offline, the remaining lookups would each
      # only wait for their timeout, so they are skipped and the sidebar says
      # "offline" instead of blaming every place string.
      if (is.na(fresh$latitude[i]) && !checked_network) {
        checked_network <- TRUE
        offline <- is.null(nslookup("nominatim.openstreetmap.org", error = FALSE))
        if (offline) {
          break
        }
      }
    }
    fill <- match(located$place, fresh$place)
    got <- !is.na(fill)
    located$latitude[got] <- fresh$latitude[fill[got]]
    located$longitude[got] <- fresh$longitude[fill[got]]

    # Persist only the newly resolved (successful) lookups.
    ok <- !is.na(fresh$latitude) & !is.na(fresh$longitude)
    n_new <- sum(ok)
    if (n_new > 0) {
      write_geocodes(db_path, fresh[ok, c("place", "latitude", "longitude"), drop = FALSE])
    }
  }
  list(
    located = located,
    n_cached = n_cached,
    n_new = n_new,
    offline = offline,
    error = error
  )
}

# How many DISTINCT, non-empty places from this metadata are NOT already in the
# cache — i.e. the ones that will actually be sent to Nominatim on the next
# Generate. This is what drives the wait, so it (not the full place count) is
# what the waiter countdown is seeded from. Computed up front, cheaply, before
# the blocking call.
geocode_pending_count <- function(meta, db_path = NULL) {
  if (is.null(meta) || !nrow(meta)) {
    return(0L)
  }
  # Rows with explicit coordinates never reach Nominatim (see build_map_coords),
  # so they don't count towards the wait.
  explicit <- parse_coordinates(
    if ("geo_loc_coordinates" %in% names(meta)) {
      meta$geo_loc_coordinates
    } else {
      rep(NA_character_, nrow(meta))
    }
  )
  has_explicit <- !is.na(explicit$latitude) & !is.na(explicit$longitude)
  places <- unique(build_place_strings(meta)[!has_explicit])
  places <- places[nzchar(places)]
  length(setdiff(places, read_geocodes(db_path)$place))
}

# Format a whole-second duration as M:SS, for the geocoding countdown. The
# total estimate is the distinct-place count: the public OSM/Nominatim endpoint
# is rate-limited to ~1 lookup per second (tidygeocoder waits out that min_time
# between requests), so wall-clock time tracks the count almost 1:1.
format_mmss <- function(secs) {
  secs <- max(as.integer(secs), 0L)
  sprintf("%d:%02d", secs %/% 60L, secs %% 60L)
}

# Seconds one Nominatim lookup is assumed to take before any has been timed:
# the service allows one request per second and answers in about 0.5-2 s.
GEOCODE_SECONDS_PER_PLACE <- 1.5

# Waiter contents shown while geocoding: the flower spinner, how many locations
# are being resolved, and a "Remaining time M:SS / M:SS" countdown (time left /
# expected total). The server re-sends this before every lookup with an
# estimate from the measured pace (`elapsed` seconds for `done` places); in
# between, the browser counts it down on its own (the `.viz-countdown` element,
# driven by the observer script installed once in ui()), into negative time if
# a lookup runs long. `n_places` is the number of UNCACHED places being fetched
# (see geocode_pending_count()); with none there is nothing to wait on, so the
# countdown is dropped and the message just covers the map build.
geocode_waiter_html <- function(n_places, done = 0L, elapsed = 0, label = "Geocoding") {
  total <- max(as.integer(n_places), 0L)
  message <- if (total == 0L) {
    "Preparing map…"
  } else if (done == 0L) {
    sprintf("%s %d location%s…", label, total, if (total == 1L) "" else "s")
  } else {
    sprintf("%s location %d of %d…", label, done + 1L, total)
  }
  per_place <- if (done > 0L) elapsed / done else GEOCODE_SECONDS_PER_PLACE
  left <- round(per_place * (total - done))
  overall <- round(elapsed + left)
  countdown <- if (total > 0L) {
    shiny$div(
      class = "viz-waiter-estimate viz-countdown",
      `data-total` = left,
      `data-overall` = overall,
      sprintf("Remaining time %s / %s", format_mmss(left), format_mmss(overall))
    )
  }
  shiny$tagList(
    shiny$div(
      class = "viz-spinner-dark",
      spin_flower(),
      shiny$div(style = "margin-top:1rem;", message),
      countdown
    )
  )
}

# Build a geocoded coordinate table from the isolate metadata. Postcode, city,
# state and country are joined into one place string (most specific first) so
# Nominatim resolves to the finest available point; the distinct places are
# resolved once (cache-first, see geocode_places_cached()) and merged back onto
# every isolate. All metadata columns are retained. Rows with no spatial
# fields, or that fail to geocode, are dropped from `coords`.
#
# Returns a list with `coords` (the metadata columns plus place, country_code,
# longitude, latitude, region_admin1 and map_country (see the Show filter's
# focus_rows()), ordered by collection date, or NULL
# when nothing is mappable), `regions` (every isolate naming a recognised
# country, with its country_code, whether or not it got a point: the country
# choropleth counts these, so a failed or offline geocode never drops an
# isolate from its country), `shapes` (per finer choropleth level, the polygons
# it draws: `admin1` from admin1_shapes(), NULL when no isolate was assigned)
# and `status` (see empty_geocode_status()'s fields). `db_path` is the database
# whose geocode cache is read and extended; NULL looks everything up.
# `progress(label, done, total, seconds)` is called before each fresh lookup.
build_map_coords <- function(meta, db_path = NULL, progress = NULL) {
  status <- empty_geocode_status()
  if (is.null(meta) || !nrow(meta)) {
    return(list(coords = NULL, regions = NULL, status = status))
  }
  col_or_na <- function(name) {
    if (name %in% names(meta)) {
      as.character(meta[[name]])
    } else {
      rep(NA_character_, nrow(meta))
    }
  }

  df <- meta
  df$place <- build_place_strings(meta)
  fallback <- build_place_strings(meta, postcode = FALSE)

  country <- trimws(col_or_na("geo_loc_name_country"))
  df$country_code <- country_code(country)
  unknown <- !is.na(country) & nzchar(country) & is.na(df$country_code)
  status$n_unknown_country <- sum(unknown)
  regions <- df[!is.na(df$country_code), , drop = FALSE]
  if (!nrow(regions)) {
    regions <- NULL
  }

  # Explicit per-isolate coordinates (geo_loc_coordinates) win over geocoding:
  # they are already point-precise, so those rows are used as-is and never sent
  # to Nominatim. An unreadable value falls back to the place string and is
  # reported, rather than silently ignored.
  raw_coords <- trimws(col_or_na("geo_loc_coordinates"))
  explicit <- parse_coordinates(raw_coords)
  df$latitude <- explicit$latitude
  df$longitude <- explicit$longitude
  bad <- nzchar(explicit$problem)
  status$n_bad_coords <- sum(bad)
  status$problems <- rbind(
    geo_problems("unknown_country", df$isolate[unknown], "Country", country[unknown]),
    geo_problems("bad_coords", df$isolate[bad], "Coordinates", raw_coords[bad])
  )
  df$raw_coords <- raw_coords

  # Mappable = has explicit coordinates, or has a place string to geocode.
  has_explicit <- !is.na(df$latitude) & !is.na(df$longitude)
  keep <- has_explicit | nzchar(df$place)
  df <- df[keep, , drop = FALSE]
  fallback <- fallback[keep]
  has_explicit <- has_explicit[keep]
  status$n_isolates <- nrow(df)
  if (!nrow(df)) {
    return(list(coords = NULL, regions = regions, status = status))
  }

  fit <- country_fit(df$latitude, df$longitude, df$country_code)
  status$n_outside <- sum(fit$outside)
  status$n_swapped <- sum(fit$swapped)
  status$problems <- rbind(
    status$problems,
    geo_problems("swapped", df$isolate[fit$swapped], "Coordinates", df$raw_coords[fit$swapped]),
    geo_problems(
      "outside",
      df$isolate[fit$outside],
      "Coordinates",
      sprintf("%s (%s)", df$raw_coords[fit$outside], country[keep][fit$outside])
    )
  )
  df$raw_coords <- NULL

  # Geocode only the rows lacking explicit coordinates (and carrying a place).
  need <- !has_explicit & nzchar(df$place)
  geo <- geocode_places_cached(
    df$place[need],
    db_path,
    if (is.function(progress)) function(...) progress("Geocoding", ...)
  )
  hit <- match(df$place, geo$located$place)
  fill <- need & !is.na(hit)
  df$latitude[fill] <- geo$located$latitude[hit[fill]]
  df$longitude[fill] <- geo$located$longitude[hit[fill]]
  n_cached <- geo$n_cached
  n_new <- geo$n_new
  offline <- geo$offline
  error <- geo$error

  # A postcode Nominatim does not know sinks the whole query, so places that
  # failed with one are retried without it, which places them at city level.
  retry <- need & is.na(df$latitude) & nzchar(fallback) & fallback != df$place
  if (any(retry) && !offline) {
    geo2 <- geocode_places_cached(
      fallback[retry],
      db_path,
      if (is.function(progress)) function(...) progress("Retrying without postcode", ...)
    )
    hit2 <- match(fallback, geo2$located$place)
    got <- retry & !is.na(hit2) & !is.na(geo2$located$latitude[hit2])
    df$latitude[got] <- geo2$located$latitude[hit2[got]]
    df$longitude[got] <- geo2$located$longitude[hit2[got]]
    status$n_postcode_fallback <- sum(got)
    status$problems <- rbind(
      status$problems,
      geo_problems(
        "postcode_fallback",
        df$isolate[got],
        "Postal code",
        sprintf("%s, placed at %s", df$geo_loc_name_postal_code[got], fallback[got])
      )
    )
    df$place[got] <- fallback[got]
    n_cached <- n_cached + geo2$n_cached
    n_new <- n_new + geo2$n_new
    offline <- geo2$offline
    if (!nzchar(error)) {
      error <- geo2$error
    }
  }

  mapped <- !is.na(df$longitude) & !is.na(df$latitude)
  failed_places <- unique(df$place[need & !mapped])
  status$problems <- rbind(
    status$problems,
    geo_problems("not_found", df$isolate[need & !mapped], "Place", df$place[need & !mapped])
  )
  status$n_mapped <- sum(mapped)
  # Distinct places actually resolved to coordinates (each backs one or more
  # mapped isolates), split into those served from the persistent cache vs.
  # freshly fetched from Nominatim this Generate.
  status$n_locations <- length(unique(df$place[need & mapped]))
  status$n_cached <- n_cached
  status$n_new <- n_new
  status$n_failed_places <- length(failed_places)
  status$failed_preview <- format_failed_preview(failed_places)
  status$offline <- offline
  status$geocode_error <- error

  # The state/province each point falls in, assigned here once per Generate
  # rather than per frame. A place naming only the country geocodes to the
  # country's centre, which would count every such isolate in one arbitrary
  # state, so only explicit coordinates and finer places are assigned.
  country_only <- !has_explicit & df$place == ifelse(is.na(country[keep]), "", country[keep])
  # Every point is located once; the Show filter below uses where it lies
  # whatever its place says.
  spot <- rep(NA_character_, nrow(df))
  spot[mapped] <- admin1_of(df$latitude[mapped], df$longitude[mapped])
  df$region_admin1 <- ifelse(country_only, NA_character_, spot)
  uncounted <- is.na(df$region_admin1)
  status$n_admin1_uncounted <- sum(uncounted)
  status$problems <- rbind(
    status$problems,
    geo_problems(
      "admin1_uncounted",
      df$isolate[uncounted],
      ifelse(
        has_explicit[uncounted],
        "Coordinates",
        ifelse(country_only[uncounted], "Country", "Place")
      ),
      ifelse(
        !mapped[uncounted],
        sprintf("%s (not geocoded)", df$place[uncounted]),
        ifelse(
          country_only[uncounted],
          sprintf("%s (country only)", df$place[uncounted]),
          sprintf(
            "%s (no state/province within %g km)",
            format_coordinates(df$latitude[uncounted], df$longitude[uncounted]),
            COUNTRY_TOLERANCE_M / 1000
          )
        )
      )
    )
  )
  shapes <- list(admin1 = admin1_shapes(df$region_admin1))

  # The country the Show filter files each isolate under: the one its point
  # lies in, so a selection never keeps a point drawn outside it. A point at
  # sea (often swapped coordinates) belongs to no country and shows only with
  # the whole world. A point in a unit the 1:50m countries lack (Gibraltar,
  # Guantanamo...) falls back to the country the isolate names.
  parent <- if (any(!is.na(spot))) {
    admin1 <- get_admin1()
    admin1$adm0_a3[match(spot, admin1$adm1_code)]
  } else {
    rep(NA_character_, nrow(df))
  }
  outside_50m <- !is.na(parent) & !parent %in% get_world()$adm0_a3
  df$map_country <- ifelse(outside_50m, df$country_code, parent)

  out <- df[mapped, , drop = FALSE]
  if (!nrow(out)) {
    return(list(coords = NULL, regions = regions, shapes = shapes, status = status))
  }
  if ("sample_collection_date" %in% names(out)) {
    out <- out[order(out$sample_collection_date), , drop = FALSE]
  }
  list(coords = out, regions = regions, shapes = shapes, status = status)
}

# Subset the coordinates to the selected range of the Time tab's date variable.
# A no-op once the range covers the full data span (the default after
# Generate), so there is no separate on/off switch — narrowing the slider is
# the filter.
filter_coords <- function(coords, o) {
  if (is.null(coords) || !nrow(coords)) {
    return(coords)
  }
  field <- o$date_field
  if (
    is.null(o$daterange) ||
      length(field) != 1L ||
      !nzchar(field) ||
      !field %in% names(coords)
  ) {
    return(coords)
  }
  rng <- as.Date(o$daterange)
  # A window over the whole span filters nothing, so isolates without a date
  # stay on the map until the window is actually narrowed.
  bounds <- o$date_bounds
  if (
    length(bounds) == 2L && rng[[1]] <= bounds[[1]] && rng[[2]] >= bounds[[2]]
  ) {
    return(coords)
  }
  keep <- in_date_range(coords[[field]], rng)
  coords[keep, , drop = FALSE]
}

# Subset rows to the countries the Show filter selects (`focus`, adm0_a3
# codes; empty is the whole world). `col` holds each row's country: the
# geocoded coords carry map_country, build_map_coords()'s `regions` only the
# stated country_code, which is what the country choropleth counts by.
focus_rows <- function(rows, focus, col = "map_country") {
  if (is.null(rows) || !length(focus) || !col %in% names(rows)) {
    return(rows)
  }
  rows[rows[[col]] %in% focus, , drop = FALSE]
}

# Map pane of the Show filter's mask and its stacking. Always above the base
# tiles (200). In Choropleth mode also above the polygons and their labels
# layer (450), whose labels would otherwise print over masked countries; in the
# other modes below the overlay pane (400) and the marker pane (410), so a
# point, heat spot or chart on a border is never cut in half.
FOCUS_MASK_PANE <- "focusMask"
FOCUS_MASK_Z <- c(choropleth = 455, other = 350)

# Fill of the mask, the map canvas's own #ddd (see .viz-map-canvas), so what
# lies outside the selection reads as no map at all.
FOCUS_MASK_COLOR <- "#dddddd"

# The mask gets its own SVG renderer. Leaflet draws vector layers into an SVG
# only 10% larger than the view and redraws it only once a pan or zoom ends, so
# mid-gesture the shrunken or shifted SVG left the tiles beyond it unmasked for
# a moment. A margin of one view on every side covers a whole-level zoom-out
# animation and any drag, and the hook below redraws the mask on every frame of
# a pan or fly, which a margin alone cannot cover (a fly can zoom out many
# levels). `_reset` and `_update` are Leaflet's own redraw steps for a
# renderer; its own guard skips them during a zoom animation, where the margin
# applies.
FOCUS_MASK_RENDERER <- sprintf("L.svg({padding: 1, pane: '%s'})", FOCUS_MASK_PANE)
FOCUS_MASK_FOLLOW_JS <- sprintf(
  "function(el) {
     var map = this;
     map.eachLayer(function(layer) {
       var r = layer._renderer;
       if (!r || r.options.pane !== '%s' || r._ptFollows) return;
       r._ptFollows = true;
       var zoom = map.getZoom();
       map.on('move', function() {
         if (map._animatingZoom) return;
         if (map.getZoom() !== zoom) {
           zoom = map.getZoom();
           r._reset();
         } else {
           r._update();
         }
       });
     });
   }",
  FOCUS_MASK_PANE
)

# How far (degrees, about 125 km) the Show filter's window reaches past the
# selected countries' coasts. The 1:50m outlines are generalised, so at
# regional zoom their coastline sits kilometres off the tiles' own; cut exactly
# there, the mask clipped coastal cities and their labels. Kept generous on
# purpose: offshore islands and the nearby sea read as part of the selection.
FOCUS_COAST_MARGIN_DEG <- 1.25

# The area the Show filter leaves visible: the selected countries grown by
# FOCUS_COAST_MARGIN_DEG, less every other country, so the margin reaches into
# the sea but never across a land border (an enclave such as Lesotho stays
# hidden unless selected too). Worked in plane degrees with the CRS dropped:
# GEOS is fast and exact enough at this scale, and the growth past +-180 that
# sf would flag on long/lat data is harmless. Rounded corners are coarse and
# the result simplified, as the mask is redrawn on every frame of a pan.
# Cached for the last selection only; it is static reference data.
focus_window <- function(focus) {
  key <- paste(sort(unique(focus)), collapse = ",")
  if (identical(.world_env$window_key, key)) {
    return(.world_env$window)
  }
  box::use(
    sf,
  )
  world <- get_world()
  outlines <- sf$st_set_crs(sf$st_geometry(world), NA)
  chosen <- world$adm0_a3 %in% focus
  window <- NULL
  if (any(chosen)) {
    window <- sf$st_buffer(
      sf$st_union(sf$st_make_valid(outlines[chosen])),
      FOCUS_COAST_MARGIN_DEG,
      nQuadSegs = 2
    )
    others <- outlines[!chosen]
    near <- others[lengths(sf$st_intersects(others, window)) > 0]
    if (length(near)) {
      window <- sf$st_difference(window, sf$st_union(sf$st_make_valid(near)))
    }
    window <- sf$st_simplify(window, dTolerance = 0.01)
    if (any(sf$st_geometry_type(window) == "GEOMETRYCOLLECTION")) {
      window <- sf$st_collection_extract(window, "POLYGON")
    }
    window <- sf$st_cast(window, "MULTIPOLYGON")
  }
  .world_env$window_key <- key
  .world_env$window <- window
  window
}

# The Show filter's mask: one opaque polygon spanning the world (and the copies
# either side of it) with every ring of focus_window() cut out. Leaflet fills
# paths even-odd, so the rings need no further geometry to become holes, and a
# hole inside the window (an unselected enclave) is filled again. NULL when
# nothing, or every country, is selected.
focus_mask <- function(focus) {
  if (!length(focus) || all(get_world()$adm0_a3 %in% focus)) {
    return(NULL)
  }
  window <- focus_window(focus)
  if (is.null(window)) {
    return(NULL)
  }
  box::use(
    sf[st_coordinates, st_polygon, st_sfc],
  )
  xy <- st_coordinates(window)
  ring_id <- do.call(paste, as.data.frame(xy[, grepl("^L", colnames(xy)), drop = FALSE]))
  rings <- split.data.frame(xy[, c("X", "Y")], ring_id)
  frame <- rbind(c(-540, -90), c(540, -90), c(540, 90), c(-540, 90), c(-540, -90))
  # No CRS on purpose: the frame reaches past +-180 to cover the world copies
  # either side, which sf flags as invalid long/lat when leaflet checks one.
  # Leaflet reads the coordinates as long/lat either way.
  st_sfc(st_polygon(c(list(frame), unname(rings))))
}

# Isolates per country for the Show filter's counts, named by adm0_a3 code:
# each isolate once, under the country its point lies in where it has a point
# (map_country; a point at sea counts nowhere), else under the country it names.
focus_counts <- function(coords, regions) {
  unplaced <- regions[!regions$isolate %in% coords$isolate, , drop = FALSE]
  codes <- c(coords$map_country, unplaced$country_code)
  counts <- table(codes[!is.na(codes)])
  setNames(as.integer(counts), names(counts))
}

# Choices of the Show filter: every Natural Earth country, grouped by
# continent, both sorted by name, each country and continent carrying its
# isolate count (from focus_counts()) for the list to print right-aligned (see
# app/js/virtual-select-count-label.js). Ticking a continent's group header
# selects all of its countries. Built in shinyWidgets' own processed form, as
# its plain named-list form has no room for the counts.
focus_choices <- function(counts = integer(0)) {
  world <- get_world()
  n <- unname(counts[world$adm0_a3])
  n[is.na(n)] <- 0L
  ord <- order(world$continent, world$name)
  groups <- split(ord, world$continent[ord])
  structure(
    list(
      type = "transpose_group",
      choices = unname(lapply(names(groups), function(continent) {
        rows <- groups[[continent]]
        list(
          label = continent,
          customData = list(count = sum(n[rows])),
          options = list(
            label = world$name[rows],
            value = world$adm0_a3[rows],
            customData = lapply(n[rows], function(k) list(count = k))
          )
        )
      }))
    ),
    class = c("list", "vs_choices")
  )
}

# Build a per-marker HTML popup from the selected metadata fields.
build_popup <- function(coords, fields) {
  fields <- intersect(fields, names(coords))
  if (!length(fields)) {
    return(NULL)
  }
  rows <- lapply(fields, function(f) {
    paste0("<b>", field_label(f), ":</b> ", coords[[f]])
  })
  Reduce(function(a, b) paste(a, b, sep = "<br>"), rows)
}

# Blank strings are missing values too: SQLite hands an unfilled cell back as
# either, and both have to land on the grey rather than one earning a colour.
blank_to_na <- function(vals) {
  vals <- as.character(vals)
  vals[!is.na(vals) & !nzchar(trimws(vals))] <- NA_character_
  vals
}

# The RGB portion of a colour string as "#RRGGBB", dropping any alpha byte an
# opacity-enabled colour picker (see viz_color()'s `opacity` argument) may have
# appended. Leaflet accepts an 8-digit hex directly, but the swatch's own alpha
# is instead pulled out and applied through fillOpacity/opacity (see
# build_markers()) so the two don't multiply into a darker-than-intended result.
hex_rgb <- function(color) {
  color <- as.character(color %||% "#000000")
  if (grepl("^#[0-9A-Fa-f]{8}$", color)) substr(color, 1, 7) else color
}

# The alpha channel of an 8-digit hex colour, in 0..1, or 1 for a plain 6-digit
# one. This is what lets a single colour picker double as that element's
# opacity control (see viz_color()'s `opacity` argument) instead of a separate
# slider.
hex_alpha <- function(color) {
  color <- as.character(color %||% "#000000")
  if (grepl("^#[0-9A-Fa-f]{8}$", color)) {
    strtoi(substr(color, 8, 9), base = 16L) / 255
  } else {
    1
  }
}

# A date column as days since the epoch, NA wherever a value does not parse.
date_days <- function(vals) {
  as.numeric(parse_dates(blank_to_na(vals)))
}

# The colour scale a mapping layer draws with, built over `vals`. A continuous
# layer gets a numeric ramp (a date one over its days, so the legend can print
# dates); everything else a factor scale. `convert` turns the values being drawn
# into the space the scale was built in, so a frame filtered from the same data
# is coloured consistently. Missing values are always NA_COLOR.
layer_palette <- function(layer, vals) {
  palette <- viz_helpers$resolve_gradient_palette(layer$palette %||% "viridis")
  is_date <- identical(layer$transform, "as_date")
  if (isTRUE(layer$continuous)) {
    convert <- if (is_date) {
      date_days
    } else {
      function(x) suppressWarnings(as.numeric(blank_to_na(x)))
    }
    num <- convert(vals)
    if (any(!is.na(num))) {
      return(list(
        pal = leaflet$colorNumeric(palette, domain = num, na.color = NA_COLOR),
        values = num,
        type = if (is_date) "Date" else "Numeric",
        convert = convert
      ))
    }
  }
  vals <- blank_to_na(vals)
  # A qualitative palette (e.g. Set1) with more levels than it has tabulated
  # colours needs expanding up front -- see resolve_qualitative_palette() --
  # since colorFactor() only calls brewer.pal() (and warns) the first time the
  # palette function it returns is actually invoked to colour real values.
  n <- length(unique(vals[!is.na(vals)]))
  list(
    pal = leaflet$colorFactor(
      viz_helpers$resolve_qualitative_palette(palette, n),
      domain = vals,
      na.color = NA_COLOR
    ),
    values = vals,
    type = "Factor",
    convert = blank_to_na
  )
}

# --- mode renderers ----------------------------------------------------------

# A date variable coarsened to the interval the user picked. Applied inside the
# two builders that read a mapped variable rather than upstream, so it happens
# exactly once however the builder was reached — binning an already-binned
# column would parse "2026-01" as a date and yield all-NA.
granular_vals <- function(vals, granularity) {
  if (!is_binned(granularity)) {
    return(vals)
  }
  as.character(bin_date_values(vals, granularity))
}

# The mapping layer a builder should draw, or NULL when none is set or its
# column is absent from these coordinates.
drawn_layer <- function(o, coords) {
  layer <- o$layer
  if (is.null(layer) || !isTRUE(layer$field %in% names(coords))) {
    return(NULL)
  }
  layer
}

# Screen distance, in pixels, under which markers count as stacked on one spot
# when clustering is off (see build_markers()).
COINCIDENT_PX <- 1

# Map pane the point markers are drawn into, so their opacity can be applied
# to the layer as a whole (see build_markers()).
MARKER_PANE <- "mapMarkers"

# Styled point markers. layerId = isolate keeps each marker individually
# addressable (e.g. for a future click-driven cross-filter).
build_markers <- function(m, coords, o, full_coords = NULL) {
  # Spiderfying is always on: without it, isolates sharing one geocoded place
  # stay a cluster at every zoom and can never be told apart. Coverage and
  # zoom-to-bounds-on-click are likewise always on -- both are one-way
  # conveniences (a hover outline, a click that zooms in) with no real reason to
  # disable, so they no longer cost the user a decision.
  # With clustering off, markers that would sit on the same pixel (typically
  # isolates sharing one city) are still grouped: stacked circles would show
  # only the topmost isolate on hover and click. Such a group spiderfies on the
  # first click, since its bounds are a single point.
  cluster_opts <- leaflet$markerClusterOptions(
    showCoverageOnHover = o$cluster,
    spiderfyOnMaxZoom = TRUE,
    zoomToBoundsOnClick = TRUE,
    maxClusterRadius = if (o$cluster) o$cluster_radius else COINCIDENT_PX
  )
  popup <- build_popup(coords, o$popup_fields)
  label <- if (length(o$hover_field)) {
    txt <- build_popup(coords, o$hover_field)
    if (!is.null(txt)) lapply(txt, HTML)
  } else {
    NULL
  }
  lopts <- leaflet$labelOptions(
    permanent = o$permanent,
    textsize = paste0(o$label_size, "px")
  )

  layer <- drawn_layer(o, coords)
  pal_info <- NULL
  fill <- hex_rgb(o$marker_color)
  if (!is.null(layer)) {
    # "Fix color scale to full date range" (o$region_fixed_scale): build the
    # palette + legend from the full, unfiltered values so a category keeps its
    # color and the legend keeps every key as the date range animates — rather
    # than rescaling to whichever subset is currently visible. The fill is still
    # applied to the visible coords (a subset of the domain). Off: domain and
    # fill both come from the visible subset.
    fixed <- isTRUE(o$region_fixed_scale) &&
      !is.null(full_coords) &&
      layer$field %in% names(full_coords)
    ref_vals <- granular_vals(
      if (fixed) full_coords[[layer$field]] else coords[[layer$field]],
      layer$granularity
    )
    pal_info <- layer_palette(layer, ref_vals)
    fill <- pal_info$pal(pal_info$convert(
      granular_vals(coords[[layer$field]], layer$granularity)
    ))
  }

  # Opacity (the Fill color picker's alpha, which also applies when a mapped
  # layer supplies the hue) is set on the marker pane as a whole, not on each
  # circle: isolates sharing a geocoded place draw one circle each, exactly on
  # top of one another, and ~40 stacked circles at 50% alpha composite to a
  # solid one, so a per-circle alpha only became visible near zero. Group
  # opacity renders such a stack like a single circle at the chosen alpha.
  m <- leaflet$addMapPane(m, MARKER_PANE, zIndex = 410)
  m <- leaflet$addCircleMarkers(
    m,
    data = coords,
    lng = ~longitude,
    lat = ~latitude,
    layerId = ~isolate,
    radius = o$radius,
    stroke = TRUE,
    color = "#000000",
    weight = 1,
    opacity = 1,
    fillColor = fill,
    fillOpacity = 1,
    popup = popup,
    label = label,
    labelOptions = lopts,
    options = leaflet$pathOptions(pane = MARKER_PANE),
    clusterOptions = cluster_opts
  )
  # createPane() adds a fresh pane element on every render, so every copy is
  # set rather than just the latest one.
  m <- onRender(
    m,
    sprintf(
      "function(el) {
         el.querySelectorAll('.leaflet-%s-pane').forEach(function(pane) {
           pane.style.opacity = %s;
         });
       }",
      MARKER_PANE,
      format(round(hex_alpha(o$marker_color), 3))
    )
  )

  if (!is.null(layer) && o$legend) {
    m <- leaflet$addLegend(
      m,
      position = o$legend_pos,
      pal = pal_info$pal,
      values = pal_info$values,
      title = if (nzchar(o$legend_title)) o$legend_title else layer$title,
      # Swatches stay fully opaque so their colors read correctly; the
      # legend's background transparency (what map_legend_opacity actually
      # controls) is applied to the whole box in build_map() below.
      opacity = 1,
      labFormat = if (identical(pal_info$type, "Date")) {
        leaflet$labelFormat(transform = function(x) {
          as.Date(x, origin = "1970-01-01")
        })
      } else {
        leaflet$labelFormat(digits = o$legend_digits)
      }
    )
  }
  m
}

# Isolates per region, aligned with `keys` (one per drawn polygon; NA for
# none). `assigned` holds each isolate's region key.
region_counts <- function(assigned, keys) {
  as.integer(table(as.character(assigned))[keys])
}

# Isolates per Natural Earth unit, aligned with `world`'s rows (NA for none).
# Joined on the unit code, so any spelling app/logic/geo.R recognises counts
# ("USA", "Deutschland", "DE"), not only Natural Earth's own name.
country_counts <- function(rows, world) {
  if (is.null(rows) || !nrow(rows)) {
    return(rep(NA_integer_, nrow(world)))
  }
  codes <- if ("country_code" %in% names(rows)) {
    rows$country_code
  } else {
    country_code(rows$geo_loc_name_country)
  }
  region_counts(codes, world$adm0_a3)
}

# What one choropleth level draws: the polygons, a display label for each, and
# the isolates counted in each from the shown rows (`n`) and from all of them
# (`full_n`, for the fixed colour scale; NULL without `full_rows`). Country
# level counts by the stated country over every Natural Earth unit; state level
# counts by region_admin1 over the regions build_map_coords() picked (`shapes`),
# with every other country drawn whole as an uncounted `backdrop`, so the map
# never shows holes where a country has no state-level isolates. `focus` (the
# Show filter's codes; empty is the world) limits both to those countries.
choropleth_units <- function(level, rows, full_rows = NULL, shapes = NULL, focus = NULL) {
  world <- get_world()
  if (length(focus)) {
    world <- world[world$adm0_a3 %in% focus, ]
  }
  if (identical(level, "admin1")) {
    polys <- shapes$admin1
    if (!is.null(polys) && length(focus)) {
      polys <- polys[polys$adm0_a3 %in% focus, ]
    }
    if (!is.null(polys) && !nrow(polys)) {
      polys <- NULL
    }
    return(list(
      polys = polys,
      label = if (!is.null(polys)) paste0(polys$name, ", ", polys$country),
      n = if (!is.null(polys)) region_counts(rows$region_admin1, polys$adm1_code),
      full_n = if (!is.null(polys) && !is.null(full_rows)) {
        region_counts(full_rows$region_admin1, polys$adm1_code)
      },
      backdrop = world[!world$adm0_a3 %in% polys$adm0_a3, ]
    ))
  }
  list(
    polys = world,
    label = world$name,
    n = country_counts(rows, world),
    full_n = if (!is.null(full_rows)) country_counts(full_rows, world)
  )
}

# Color-scale domain for region counts. A single counted value (every isolate
# in one region, or one count shared by all) would give a zero-width domain,
# which leaflet's numeric legend draws as an empty bar; the scale then starts at
# zero instead. With nothing counted at all (a date window holding only
# isolates no state-level region took) the scale is 0 to 1.
count_domain <- function(n) {
  if (all(is.na(n))) {
    return(c(0, 1))
  }
  r <- range(n, na.rm = TRUE)
  if (r[1] == r[2]) c(0, max(r[2], 1)) else r
}

# Choropleth: shade regions by isolate count, using the Region tab's palette
# (the mapping layers don't apply here — the mapped variable is always the
# isolate count). The level (o$region_level) decides the regions, see
# choropleth_units(); everything from the palette on is shared. At country
# level `coords` are build_map_coords()'s `regions`, at state level its
# `coords`, as build_map() passes them; `shapes` is its `shapes`.
build_choropleth <- function(m, coords, o, full_coords = NULL, shapes = NULL) {
  units <- choropleth_units(
    o$region_level %||% "country",
    coords,
    full_coords,
    shapes,
    o$focus
  )
  # Countries counted at no finer level go underneath, grey and labelled by
  # name only: a count there would read as zero isolates, though some may
  # name just the country.
  if (!is.null(units$backdrop) && nrow(units$backdrop)) {
    m <- leaflet$addPolygons(
      m,
      data = units$backdrop,
      fillColor = "#f0f0f0",
      fillOpacity = o$region_opacity,
      color = o$region_border,
      weight = 1,
      smoothFactor = 0.3,
      label = units$backdrop$name,
      highlightOptions = leaflet$highlightOptions(
        weight = 2,
        color = "#000000",
        bringToFront = TRUE
      )
    )
  }
  if (is.null(units$polys)) {
    return(m)
  }
  n <- units$n

  # Off (default): the domain is the currently-shown (possibly date-filtered)
  # counts, so the color scale rescales to whatever subset is visible — the
  # darkest region always looks "full" regardless of its absolute count. On:
  # the domain comes from the full, unfiltered counts instead, so a partial
  # range renders lighter until the count actually approaches the final
  # total — useful for watching the animation "fill in" over time.
  domain_n <- if (isTRUE(o$region_fixed_scale) && !is.null(units$full_n)) {
    units$full_n
  } else {
    n
  }

  vals <- if (identical(o$region_transform, "Log")) log1p(n) else n
  domain_range <- count_domain(domain_n)
  domain_vals <- if (identical(o$region_transform, "Log")) {
    log1p(domain_range)
  } else {
    domain_range
  }
  pal <- suppressWarnings(leaflet$colorNumeric(
    viz_helpers$resolve_gradient_palette(o$region_scale),
    domain = domain_vals,
    na.color = "#f0f0f0"
  ))
  fill <- pal(vals)
  fill[is.na(vals)] <- "#f0f0f0"

  # Every region is labelled, those without isolates included: "0 isolates"
  # is an answer, where no label at all reads as a region the map forgot.
  region_label <- paste0(
    units$label,
    ": ",
    ifelse(is.na(n), 0L, n),
    " isolates"
  )

  # "Always show labels" only pins the ones actually saying something: a
  # region with zero isolates permanently printing "0 isolates" all over an
  # otherwise empty map just adds noise. One labelOptions() per row (rather
  # than the single shared object every other addPolygons() argument takes)
  # is what lets `permanent` vary polygon by polygon here.
  has_isolates <- !is.na(n) & n > 0
  region_label_opts <- lapply(has_isolates, function(shown) {
    leaflet$labelOptions(permanent = isTRUE(o$region_permanent) && shown)
  })

  m <- leaflet$addPolygons(
    m,
    data = units$polys,
    fillColor = fill,
    fillOpacity = o$region_opacity,
    color = o$region_border,
    weight = 1,
    smoothFactor = 0.3,
    label = region_label,
    labelOptions = region_label_opts,
    highlightOptions = leaflet$highlightOptions(
      weight = 2,
      color = "#000000",
      fillOpacity = 0.9,
      bringToFront = TRUE
    )
  )

  if (o$legend && any(!is.na(domain_n))) {
    lpal <- suppressWarnings(leaflet$colorNumeric(
      viz_helpers$resolve_gradient_palette(o$region_scale),
      domain = domain_range,
      na.color = "#f0f0f0"
    ))
    m <- leaflet$addLegend(
      m,
      position = o$legend_pos,
      pal = lpal,
      values = domain_range,
      title = "Isolate count",
      opacity = 1,
      labFormat = leaflet$labelFormat(digits = 0)
    )
  }
  m
}

# Point-density heatmap (one unit of intensity per isolate). minOpacity is the
# only opacity-like knob leaflet.heat exposes; raising it makes the map read as
# less "solid" over dark basemaps.
build_heatmap <- function(m, coords, o, full_coords = NULL) {
  # Aggregate to one point per distinct place with intensity = isolate count,
  # rather than letting addHeatmap default every isolate to intensity 1 and
  # rely on many overlapping points at the same geocoded coordinate to sum up
  # a density. That made `max` effectively invisible: a handful of co-located
  # isolates already saturates the color scale for any max in a normal range.
  # Explicit counts make `max` a meaningful, explainable knob ("isolate count
  # needed to reach full color") — see build_map_coords()'s auto-fitted slider.
  agg <- aggregate(
    isolate ~ place + longitude + latitude,
    data = coords,
    FUN = length
  )
  names(agg)[names(agg) == "isolate"] <- "count"

  # Mirrors Choropleth's own on/off domain switch (region_fixed_scale). On
  # (default): the color scale stays pinned to the "Max intensity" slider,
  # itself auto-fit to the full, unfiltered dataset's busiest place at
  # Generate (see that updateSliderInput call) — so intensity stays
  # comparable across every frame of a date-range animation. Off: the scale
  # instead rescales every frame to the busiest place in the currently
  # visible (possibly date-filtered) subset, so a handful of isolates in an
  # early, sparse frame already saturates to full color.
  effective_max <- if (isTRUE(o$region_fixed_scale) || is.null(full_coords)) {
    o$heat_max
  } else {
    max(agg$count, 1)
  }

  # addHeatmap()'s `gradient` argument must be a palette *function* (or a
  # value colorNumeric() itself accepts) — passing a pre-built named list of
  # stops, as this used to do, makes leaflet.extras run colorNumeric() on
  # that list a second time internally, which has no method for "list" and
  # errors out. Handing it the colorNumeric() palette function directly lets
  # addHeatmap() do its own resampling. Colors below 0.2 are forced
  # transparent to preserve leaflet.heat's usual "fades out at low density"
  # look instead of solid-coloring even single-isolate points.
  gradient_pal <- leaflet$colorNumeric(
    viz_helpers$resolve_gradient_palette(o$heat_scale %||% "viridis"),
    domain = c(0, 1)
  )
  gradient <- function(x) ifelse(x < 0.2, "rgba(0,0,0,0)", gradient_pal(x))

  # blur and the minimum-opacity floor are fixed rather than user-tunable
  # (see the Density panel's comment) — blur scaled to radius keeps the same
  # relative softness at any radius; 0.3 matches the app's previous default.
  m <- addHeatmap(
    m,
    data = agg,
    lng = ~longitude,
    lat = ~latitude,
    intensity = ~count,
    radius = o$heat_radius,
    blur = o$heat_radius * 0.6,
    max = effective_max,
    minOpacity = 0.3,
    gradient = gradient
  )
  # leaflet.heat divides every point's intensity by 2^(maxZoom - zoom), capped
  # at 2^12, and maxZoom defaults to the map's own (18). At a country-level
  # zoom that turned a place holding 300 isolates into an intensity of ~0.07
  # against a max of 300, so the densest country drew as faintly as an empty
  # one. maxZoom = 0 makes that factor 1 at every zoom, which is what lets `max`
  # mean "isolates needed for full colour". addHeatmap() does not expose the
  # option, so it is set on the call it just emitted.
  last <- length(m$x$calls)
  if (last && identical(m$x$calls[[last]]$method, "addHeatmap")) {
    m$x$calls[[last]]$args[[4]]$maxZoom <- 0
  }

  # Mirrors Choropleth's legend: same gradient the heat layer itself uses
  # (0 to the effective max), so the color a place shows on the map reads
  # back to an isolate count the same way Choropleth's fill does.
  if (o$legend) {
    lpal <- leaflet$colorNumeric(
      viz_helpers$resolve_gradient_palette(o$heat_scale %||% "viridis"),
      domain = c(0, effective_max)
    )
    m <- leaflet$addLegend(
      m,
      position = o$legend_pos,
      pal = lpal,
      values = c(0, effective_max),
      title = if (nzchar(o$legend_title)) o$legend_title else "Isolate count",
      opacity = 1,
      labFormat = leaflet$labelFormat(digits = 0)
    )
  }
  m
}

# Assign each point to a cell of a square pixel grid at the given zoom, so that
# points closer than ~radius_px on screen share a cell and their charts merge
# into one. leaflet.markercluster can't be reused for this — it only groups
# L.Marker, and minicharts extend L.CircleMarker — so the binning is done here
# instead. Cells are computed in Web Mercator pixel space (the projection
# Leaflet itself uses), so the grouping tightens as the user zooms in and the
# charts split apart again, the same way marker clusters behave.
cluster_cell <- function(lng, lat, zoom, radius_px) {
  scale <- 256 * 2^zoom
  x <- (lng + 180) / 360 * scale
  siny <- pmin(pmax(sin(lat * pi / 180), -0.9999), 0.9999)
  y <- (0.5 - log((1 + siny) / (1 - siny)) / (4 * pi)) * scale
  paste(floor(x / radius_px), floor(y / radius_px))
}

# One minichart per location, showing the composition of the mapped variable
# in the mapping layer's palette. Every level gets its own slice by default, so
# the legend reads exactly like the one Markers draws for the same variable.
# o$chart_max_categories (0 = off) optionally keeps only that many of the most
# common levels and folds the rest into a single "Other" slice — for a variable
# whose long tail would otherwise slice every chart too thin to read. Isolates
# missing the value get their own grey "Missing" slice.
#
# With clustering on (and a known zoom), nearby locations are grouped by
# pixel-grid cell so overlapping charts combine into one aggregate chart that
# splits as the user zooms in — see cluster_cell(). Without a zoom (the initial
# render before the client reports one, or the static HTML export, which has no
# server to re-bin on zoom) it falls back to one chart per exact place.
build_charts <- function(m, coords, o, full_coords = NULL, zoom = NULL) {
  layer <- drawn_layer(o, coords)
  if (is.null(layer)) {
    return(m)
  }
  var <- layer$field

  # 0 (the default) folds nothing: every level keeps its own slice and its own
  # legend key, the way every other mode draws the same variable.
  max_categories <- o$chart_max_categories %||% 0

  # "Fix color scale to full date range" (o$region_fixed_scale): derive the
  # category set — which levels get their own slice vs. fold into "Other" — the
  # column order, the palette, and the legend from the full, unfiltered data so
  # they all stay stable as the date range animates. Off: recompute the top-N
  # folding and category set from just the visible subset, so a level's color
  # can shift and the legend gains/loses keys frame to frame.
  fixed <- isTRUE(o$region_fixed_scale) &&
    !is.null(full_coords) &&
    var %in% names(full_coords)
  ref_vals <- blank_to_na(granular_vals(
    if (fixed) full_coords[[var]] else coords[[var]],
    layer$granularity
  ))
  # The same value -> colour mapping Markers/Choropleth use (see
  # layer_palette()), built from the full, unfolded category domain rather
  # than just the levels that survive the top-N fold below: a palette scoped
  # to the folded subset would rank those levels among themselves, so the
  # same real-world category could come out a different colour depending on
  # which mode drew it. It also supplies the display order below, so colour
  # and order can't drift apart between modes.
  pal_info <- layer_palette(layer, ref_vals)
  ref_freq <- sort(table(ref_vals), decreasing = TRUE)
  folded <- max_categories > 0 && length(ref_freq) > max_categories
  top <- if (folded) {
    names(ref_freq)[seq_len(max_categories)]
  } else {
    names(ref_freq)
  }
  # Which levels survive the fold is a frequency question — keep the most
  # common — but the order they are *listed* in is the palette's own domain
  # order, the same order every other mode's legend reads in, so a category
  # keeps its place on the shared colour ramp whichever mode drew it. Slices
  # follow suit, since this one column order drives both.
  top <- top[order(pal_info$convert(top))]
  # Master column/legend order; "Other" (the fold bucket) and "Missing" last.
  missing_label <- "Missing"
  cats <- c(
    top,
    if (folded) "Other",
    if (anyNA(ref_vals)) missing_label
  )

  vals <- blank_to_na(granular_vals(coords[[var]], layer$granularity))
  if (folded) {
    vals <- ifelse(is.na(vals) | vals %in% top, vals, "Other")
  }
  vals[is.na(vals)] <- missing_label

  key <- if (isTRUE(o$chart_cluster) && !is.null(zoom)) {
    cluster_cell(
      coords$longitude,
      coords$latitude,
      zoom,
      o$chart_cluster_radius %||% 80
    )
  } else {
    coords$place
  }

  # Force the full master category set as columns via a fixed-level factor, so
  # levels absent from the current frame still get a (zero) column. That keeps
  # both the palette and the minichart legend covering every category — when
  # fixed, the complete full-range set, otherwise just the visible set.
  cd <- as.data.frame.matrix(table(key, factor(vals, levels = cats)))

  # Each chart sits at the centroid of the points in its group; with the
  # per-place key every point in a group shares one coordinate, so the
  # unclustered layout is identical to before.
  lng <- as.numeric(tapply(coords$longitude, key, mean)[rownames(cd)])
  lat <- as.numeric(tapply(coords$latitude, key, mean)[rownames(cd)])

  # "Other" (the fold bucket) and "Missing" aren't real domain levels, so they
  # take their own fixed greys; every other column takes its colour from the
  # shared palette built above.
  named <- !(names(cd) %in% c("Other", missing_label))
  colors <- rep(NA_COLOR, ncol(cd))
  colors[named] <- pal_info$pal(pal_info$convert(names(cd)[named]))
  if ("Other" %in% names(cd)) {
    colors[names(cd) == "Other"] <- OTHER_COLOR
  }

  m <- addMinicharts(
    m,
    lng = lng,
    lat = lat,
    type = o$chart_type,
    chartdata = cd,
    colorPalette = unname(colors),
    width = o$chart_size,
    opacity = o$chart_opacity,
    # minicharts draws a legend of its own, but it takes no title and ignores
    # the shared Legend tab's styling, so this mode's legend always read as a
    # different object from every other mode's. It is switched off in favour
    # of the same addLegend() the other modes use, below.
    legend = FALSE
  )

  # The one legend every mode draws — same title, same swatch/background
  # treatment, same position control — over the columns actually charted, in
  # the column order set above. Colours come from `colors`, so the keys, their
  # order and their colours are the Markers legend's for the same variable.
  if (isTRUE(o$legend)) {
    m <- leaflet$addLegend(
      m,
      position = o$legend_pos,
      colors = unname(colors),
      labels = names(cd),
      title = if (nzchar(o$legend_title)) o$legend_title else layer$title,
      opacity = 1
    )
  }
  m
}

# Human-readable "Date range" label — "YYYY-MM-DD to YYYY-MM-DD", or a
# single date if both ends match (e.g. Play's first frame). Shared by the
# on-map annotation below and the sidebar's map_anim_label, so both always
# read identically.
format_daterange_label <- function(rng) {
  rng <- as.Date(rng)
  if (rng[1] == rng[2]) {
    format(rng[1], "%Y-%m-%d")
  } else {
    paste(format(rng[1], "%Y-%m-%d"), "to", format(rng[2], "%Y-%m-%d"))
  }
}

# Assemble the full leaflet widget (no view/zoom — the caller sets that): base
# tiles, decorations, plugins, then the layer for the active mode. Shared by the
# live renderer and the HTML export. `regions` is build_map_coords()'s
# unfiltered country table; NULL makes the country choropleth count `coords`
# instead. `shapes` is its `shapes`, the polygons of the finer levels.
build_map <- function(
  coords,
  o,
  full_coords = NULL,
  zoom = NULL,
  regions = NULL,
  shapes = NULL
) {
  # The native zoom control has no `position` argument of its own -- it always
  # lands at Leaflet's hardcoded top-left, which is also where a "Top left"
  # legend/anything else the Legend tab picks would go. Turning it off here
  # and adding a custom one (below, alongside the fullscreen button) is what
  # actually lets that corner mean top-left, rather than being permanently
  # occupied by the zoom buttons regardless of what the position picker says.
  #
  # zoomSnap sets the granularity the map may rest at (0.25 = quarter-level
  # steps instead of integers), so mousewheel zoom can settle on fractional
  # levels; wheelPxPerZoomLevel raises the scroll distance per zoom level so
  # each wheel notch moves less. zoomDelta keeps the +/- buttons and keyboard
  # in step with the finer snap.
  m <- leaflet$leaflet(
    options = leaflet$leafletOptions(
      zoomControl = FALSE,
      zoomSnap = 0.25,
      zoomDelta = 0.25,
      wheelPxPerZoomLevel = 120
    )
  )

  # Choropleth mode has no base tile layer at all — the basemap picker is
  # hidden for this mode (it had no visible effect once a fill covers the
  # polygons) and a busy or even muted tile image underneath still reads as
  # "textured" wherever the fill isn't fully opaque. Only a labels-only tile
  # layer (see choropleth_labels_url above) is kept, in its own pane
  # stacked above the polygon overlay, so place names stay legible for
  # context against an otherwise blank background.
  choropleth <- identical(o$mode, "Choropleth")
  if (choropleth) {
    m <- leaflet$addMapPane(m, "choroplethLabels", zIndex = 450)
  } else {
    m <- if (identical(o$tiles, "OpenStreetMap")) {
      leaflet$addTiles(m)
    } else {
      leaflet$addProviderTiles(m, o$tiles)
    }
  }
  if (o$scalebar) {
    m <- leaflet$addScaleBar(m, position = "bottomleft")
  }
  if (o$minimap) {
    m <- leaflet$addMiniMap(m, toggleDisplay = TRUE, minimized = FALSE)
  }
  if (o$graticule) {
    m <- leaflet$addSimpleGraticule(m)
  }
  if (o$show_controls) {
    m <- addFullscreenControl(m, position = "bottomleft")
    # this = the map instance (leaflet's htmlwidgets binding returns it from
    # initialize(), which onRender() calls its hook against).
    m <- onRender(
      m,
      "function(el, x) { L.control.zoom({ position: 'bottomleft' }).addTo(this); }"
    )
  }

  # The country choropleth counts build_map_coords()'s `regions` (every isolate
  # naming a recognised country) rather than the geocoded points, under the same
  # time filter, so an isolate whose place failed to geocode still counts. The
  # state level needs a point, so it counts the (filtered) geocoded `coords`.
  # The Show filter blanks every country outside the selection, base tiles
  # and choropleth labels included, and applies to every mode alike before
  # anything is counted.
  mask <- focus_mask(o$focus)
  if (!is.null(mask)) {
    m <- leaflet$addMapPane(
      m,
      FOCUS_MASK_PANE,
      zIndex = FOCUS_MASK_Z[[if (choropleth) "choropleth" else "other"]]
    )
    m <- leaflet$addPolygons(
      m,
      data = mask,
      stroke = FALSE,
      fillColor = FOCUS_MASK_COLOR,
      fillOpacity = 1,
      smoothFactor = 0.3,
      options = leaflet$pathOptions(
        pane = FOCUS_MASK_PANE,
        interactive = FALSE,
        renderer = JS(FOCUS_MASK_RENDERER)
      )
    )
    m <- onRender(m, FOCUS_MASK_FOLLOW_JS)
  }
  coords <- focus_rows(coords, o$focus)
  full_coords <- focus_rows(full_coords, o$focus)
  regions <- focus_rows(regions, o$focus, "country_code")
  country_level <- identical(o$region_level %||% "country", "country")
  if (choropleth && country_level && !is.null(regions)) {
    full_coords <- regions
    coords <- filter_coords(regions, o)
  }

  # A choropleth still draws its regions, uncounted, for an empty date window.
  if (!choropleth && (is.null(coords) || !nrow(coords))) {
    return(m)
  }

  m <- switch(
    o$mode,
    Choropleth = build_choropleth(m, coords, o, full_coords, shapes),
    Heatmap = build_heatmap(m, coords, o, full_coords),
    Charts = build_charts(m, coords, o, full_coords, zoom),
    build_markers(m, coords, o, full_coords)
  )

  # Second half of the label sandwich set up above: added only now, after the
  # polygons, so it lands in the "choroplethLabels" pane stacked above them.
  if (choropleth) {
    m <- leaflet$addTiles(
      m,
      urlTemplate = choropleth_labels_url,
      attribution = choropleth_labels_attribution,
      options = leaflet$tileOptions(pane = "choroplethLabels")
    )
  }

  # On-map annotation of the currently displayed date range — separate from
  # the sidebar's map_anim_label text, this one is visible on the map itself
  # (and so also present in the HTML export), and reflects a manual slider
  # drag exactly the same way it reflects the Play animation, since both just
  # go through o$daterange.
  if (isTRUE(o$show_time_label) && length(o$daterange) == 2) {
    label_text <- format_daterange_label(o$daterange)
    m <- leaflet$addControl(
      m,
      html = as.character(shiny$tags$div(class = "map-date-label", label_text)),
      position = "topright",
      className = "map-date-label-control"
    )
  }

  # addLegend()'s own `opacity` argument only controls the color swatches, not
  # the legend box itself — Leaflet gives the box a fixed semi-opaque
  # background via its own CSS. map_legend_opacity is meant to control that
  # background, so it is applied directly to the rendered box post-render.
  #
  # Leaflet also never constrains the legend's own height, so a categorical
  # legend with many entries (e.g. one row per isolate) just grows past the
  # map's edge instead of wrapping or scrolling. `.leaflet-top`/`.leaflet-bottom`
  # are absolutely positioned with only `top` or `bottom` set (no explicit
  # height), so a CSS `max-height: 100%` on the legend can't resolve against
  # that auto-height ancestor — the legend is instead capped in JS so it
  # scrolls internally rather than overflowing.
  #
  # Getting the cap right took several tries. Two lessons baked into the code
  # below: (1) the measurement must run after layout — computing it once,
  # synchronously in onRender, read a stale/zero map height — so it's deferred
  # to requestAnimationFrame and re-run on every resize via a ResizeObserver;
  # and (2) it must never be derived from the legend's own measured edges,
  # which move as the cap is applied and so can chase themselves. The cap is
  # instead the band this column has free — the map's height less its margins,
  # less whatever the diagonally opposite corner occupies — minus the controls
  # stacked with the legend inside its own corner: Leaflet lays each corner's
  # controls out as one block growing away from the anchored edge, so a legend
  # that takes the whole band pushes its neighbours clean off the map.
  if (isTRUE(o$legend)) {
    m <- onRender(
      m,
      sprintf(
        "function(el, x) {
         var EDGE = 24;   // gap kept between the legend and the map's edges
         var GAP = 12;    // gap kept between the legend and another control
         function fit() {
           var l = el.querySelector('.info.legend');
           if (!l) return;
           l.style.background = 'rgba(255, 255, 255, %s)';
           var mapRect = el.getBoundingClientRect();
           var dateLabel = el.querySelector('.map-date-label-control');
           var isBottom = !!l.closest('.leaflet-bottom');
           var isRight = !!l.closest('.leaflet-right');
           // When the legend shares the top-right corner with the date label,
           // Leaflet's control add-order is unreliable, so pin the label above
           // the legend and open a gap; otherwise clear any stale margin.
           if (dateLabel && l.parentNode === dateLabel.parentNode) {
             l.parentNode.insertBefore(dateLabel, l);
             l.style.marginTop = GAP + 'px';
           } else {
             l.style.marginTop = '';
           }
           // The band this column leaves the legend: the map's height less an
           // edge margin, further limited by the corner container diagonally
           // opposite the legend's own. Those two corners are independently
           // anchored boxes with no mutual height reservation, so a top-left
           // legend growing down runs straight into the zoom/fullscreen/scale
           // controls anchored bottom-left unless this is measured live. On
           // the right column it is the corner the date label (top-right) and
           // minimap (bottom-right) already live in, so they are covered too.
           var topLimit = mapRect.top + EDGE;
           var bottomLimit = mapRect.bottom - EDGE;
           var opposite = el.querySelector(
             '.leaflet-' + (isBottom ? 'top' : 'bottom') +
             '.leaflet-' + (isRight ? 'right' : 'left')
           );
           if (opposite && opposite.children.length) {
             var oRect = opposite.getBoundingClientRect();
             if (isBottom) {
               topLimit = Math.max(topLimit, oRect.bottom + GAP);
             } else {
               bottomLimit = Math.min(bottomLimit, oRect.top - GAP);
             }
           }
           // Whatever shares the legend's own corner is stacked with it in a
           // single container, so it spends the same band: the zoom buttons
           // above a bottom-left legend are pushed up (and off the map) by
           // exactly the height the legend takes. Reserving their height here
           // is what keeps them on it.
           var stacked = 0;
           Array.prototype.forEach.call(l.parentNode.children, function (sib) {
             if (sib === l) return;
             var h = sib.getBoundingClientRect().height;
             if (h) stacked += h + GAP;
           });
           l.style.maxHeight =
             Math.max(bottomLimit - topLimit - stacked, 60) + 'px';
           l.style.overflowY = 'auto';
           l.style.overflowX = 'hidden';
         }
         requestAnimationFrame(fit);
         if (window.ResizeObserver) {
           new ResizeObserver(fit).observe(el);
         } else {
           window.addEventListener('resize', fit);
         }
       }",
        o$legend_opacity %||% 0.85
      )
    )
  }
  m
}

# Frame the map on a coordinate set. When the points span an area this is a
# plain fitBounds (or flyToBounds, for the animated Reset-view); but when every
# point shares one location — a single isolate, or several co-located ones —
# the bounds are zero-area and fitBounds falls back to computing its zoom from
# the container size, snapping to maxZoom. On the very first render that is a
# bug: the map container was only just un-hidden and hasn't been measured yet,
# so getBoundsZoom resolves against a zero-size container and produces a bogus
# center/zoom, leaving the tiles unloaded (a grey box). A direct setView on the
# shared point at a fixed regional zoom sidesteps getBoundsZoom entirely, so a
# one-isolate map frames correctly the first time.
single_point_zoom <- 6
frame_coords <- function(m, coords, fly = FALSE) {
  fit <- list(padding = c(FIT_PADDING_PX, FIT_PADDING_PX))
  lng_rng <- range(coords$longitude)
  lat_rng <- range(coords$latitude)
  if (lng_rng[1] == lng_rng[2] && lat_rng[1] == lat_rng[2]) {
    if (fly) {
      leaflet$flyTo(
        m,
        lng = lng_rng[1],
        lat = lat_rng[1],
        zoom = single_point_zoom
      )
    } else {
      leaflet$setView(
        m,
        lng = lng_rng[1],
        lat = lat_rng[1],
        zoom = single_point_zoom
      )
    }
  } else if (fly) {
    leaflet$flyToBounds(m, lng_rng[1], lat_rng[1], lng_rng[2], lat_rng[2], options = fit)
  } else {
    leaflet$fitBounds(m, lng_rng[1], lat_rng[1], lng_rng[2], lat_rng[2], options = fit)
  }
}

# --- control sidebar ---------------------------------------------------------

# Entries the popup and hover pickers offer that are not metadata columns: the
# isolate id, and for the popup the geocoded place string.
label_sentinels <- function(id) {
  if (identical(id, "map_popup")) {
    c(Isolate = "isolate", Location = "place")
  } else {
    c(Isolate = "isolate")
  }
}

# A slider whose value is a distance on screen: a marker cluster's radius, a
# heat point's spread, a chart cluster's cell. Leaflet measures these in
# pixels, and they are pixels on purpose — a fixed number of kilometres would
# stop clustering at a world view and blow up at street level, which is the
# opposite of what decluttering a map needs. What a pixel count does not say is
# how far that reaches on the ground, so the slider carries a line that does,
# for the current zoom, and dragging or hovering it outlines that reach on the
# map itself (see reach_script). `shape` is "circle" for a radius and "square"
# for a grid cell whose side is the value. `ring` names which layers on the map
# the outline is drawn around, one per element: "cluster" (markers and cluster
# icons), "chart" (minicharts, outlined by the grid cell they were binned in)
# or "heat" (heat points). With nothing of that kind in view, or NULL, a single
# outline is centred on the map.
reach_slider <- function(
  ns,
  id,
  label,
  min,
  max,
  value,
  step,
  shape = "circle",
  ring = NULL
) {
  attribs <- list(
    class = "custom-slider map-reach-slider",
    `data-map` = ns("map"),
    `data-shape` = shape
  )
  if (!is.null(ring)) {
    attribs$`data-ring` <- ring
  }
  do.call(
    shiny$div,
    c(
      attribs,
      list(
        shiny$sliderInput(
          ns(id),
          label,
          min = min,
          max = max,
          value = value,
          step = step,
          ticks = FALSE
        ),
        shiny$div(class = "map-reach-hint", "Shown in km once the map is drawn")
      )
    )
  )
}

# Browser side of reach_slider(), installed once per page however many map tabs
# are open. The ground distance is measured on the live map (Leaflet's own
# distance between two screen points at its centre), so it is exact for the
# projection and latitude on screen rather than an equator approximation.
reach_script <- shiny$tags$script(shiny$HTML(
  "(function(){
    if (window.__phylotraceMapReach) return;
    window.__phylotraceMapReach = true;
    function mapFor(wrap) {
      var id = wrap.getAttribute('data-map');
      if (!id || !window.HTMLWidgets) return null;
      var w = HTMLWidgets.find('#' + CSS.escape(id));
      var map = w && w.getMap ? w.getMap() : null;
      if (!map || !map.getContainer().offsetWidth) return null;
      if (!map.__reachBound) {
        map.__reachBound = true;
        map.on('zoomend moveend resize', refreshAll);
      }
      return map;
    }
    function value(wrap) {
      var input = wrap.querySelector('input.js-range-slider');
      return input ? parseFloat(input.value) || 0 : 0;
    }
    function round(x) {
      return x >= 10 ? Math.round(x).toLocaleString() : x.toFixed(1);
    }
    function refresh(wrap) {
      var hint = wrap.querySelector('.map-reach-hint');
      var map = mapFor(wrap);
      if (!hint || !map) return;
      var px = value(wrap);
      var size = map.getSize();
      var c = L.point(size.x / 2, size.y / 2);
      var metres = map.distance(
        map.containerPointToLatLng(c.subtract([px / 2, 0])),
        map.containerPointToLatLng(c.add([px / 2, 0]))
      );
      var what = wrap.getAttribute('data-shape') === 'square' ?
        ' across' : ' radius';
      hint.textContent = '\\u2248 ' + round(metres / 1000) + ' km (' +
        round(metres / 1609.344) + ' mi)' + what + ' at the current zoom';
    }
    function refreshAll() {
      document.querySelectorAll('.map-reach-slider').forEach(refresh);
    }
    function isMinichart(layer) {
      return !!(L.Minichart && layer instanceof L.Minichart);
    }
    // Lat/lngs of the layers the outline is drawn around, read from the map's
    // own layers (a heatmap is one canvas, and minicharts share one SVG, so
    // neither has a DOM element per point to measure).
    function anchorLatLngs(kind, map) {
      var out = [];
      map.eachLayer(function(layer) {
        if (kind === 'heat') {
          if (L.HeatLayer && layer instanceof L.HeatLayer) {
            (layer._latlngs || []).forEach(function(ll) { out.push(L.latLng(ll)); });
          }
        } else if (kind === 'chart') {
          if (isMinichart(layer)) out.push(layer.getLatLng());
        } else if (kind === 'cluster') {
          // Visible cluster icons and unclustered circle markers; markers
          // inside a cluster are not on the map, so are skipped by eachLayer.
          if (!isMinichart(layer) && (layer instanceof L.CircleMarker ||
              (layer instanceof L.Marker && layer._icon))) {
            out.push(layer.getLatLng());
          }
        }
      });
      return out;
    }
    // Container-pixel centres to outline, one per distinct spot in view. A
    // chart cell is outlined as the actual grid cell cluster_cell() bins it in
    // (world pixels at this zoom, floored to the cell size), since charts in
    // the same square merge whether or not their own centred squares overlap.
    function anchors(wrap, map, d) {
      var kind = wrap.getAttribute('data-ring');
      if (!kind) return [];
      var size = map.getSize();
      var zoom = map.getZoom();
      var seen = {};
      var out = [];
      anchorLatLngs(kind, map).forEach(function(ll) {
        var p;
        if (kind === 'chart') {
          var w = map.project(ll, zoom);
          var cell = L.point(
            (Math.floor(w.x / d) + 0.5) * d,
            (Math.floor(w.y / d) + 0.5) * d
          );
          p = map.latLngToContainerPoint(map.unproject(cell, zoom));
        } else {
          p = map.latLngToContainerPoint(ll);
        }
        if (p.x < -d || p.y < -d || p.x > size.x + d || p.y > size.y + d) return;
        var key = Math.round(p.x) + ',' + Math.round(p.y);
        if (seen[key]) return;
        seen[key] = true;
        out.push(p);
      });
      return out;
    }
    // The preview stays up for as long as its slider is hovered or dragged.
    // Redrawn on a short timer while shown, so it follows the slider value,
    // pans/zooms, and the map re-rendering its layers after a change.
    var active = null;
    var hovered = null;
    var dragging = false;
    var ticker = null;
    function clearBoxes() {
      document.querySelectorAll('.map-reach-preview').forEach(function(box) {
        box.remove();
      });
    }
    function draw() {
      if (!active) return;
      if (!document.body.contains(active)) { hide(); return; }
      var map = mapFor(active);
      if (!map) { clearBoxes(); return; }
      var el = map.getContainer();
      var square = active.getAttribute('data-shape') === 'square';
      var d = square ? value(active) : 2 * value(active);
      var size = map.getSize();
      var centers = anchors(active, map, d);
      if (!centers.length) centers = [L.point(size.x / 2, size.y / 2)];
      // Capped so a dense, zoomed-out map doesn't paint hundreds of outlines.
      centers = centers.slice(0, 300);
      var cls = 'map-reach-preview' + (square ? ' map-reach-preview--square' : '');
      var boxes = el.querySelectorAll(':scope > .map-reach-preview');
      centers.forEach(function(c, i) {
        var box = boxes[i];
        if (!box) {
          box = document.createElement('div');
          el.appendChild(box);
        }
        box.className = cls;
        box.style.width = d + 'px';
        box.style.height = d + 'px';
        box.style.left = c.x + 'px';
        box.style.top = c.y + 'px';
      });
      for (var j = centers.length; j < boxes.length; j++) boxes[j].remove();
    }
    function hide() {
      clearInterval(ticker);
      ticker = null;
      active = null;
      clearBoxes();
    }
    function show(wrap) {
      if (active !== wrap) {
        hide();
        active = wrap;
      }
      refresh(wrap);
      draw();
      if (!ticker) ticker = setInterval(draw, 150);
    }
    $(document).on('mouseenter', '.map-reach-slider', function() {
      hovered = this;
      show(this);
    });
    $(document).on('mouseleave', '.map-reach-slider', function() {
      if (hovered === this) hovered = null;
      if (!dragging && active === this) hide();
    });
    $(document).on('pointerdown', '.map-reach-slider', function() {
      dragging = true;
      show(this);
    });
    $(document).on('pointerup pointercancel', function() {
      if (!dragging) return;
      dragging = false;
      if (active && hovered !== active) hide();
    });
    $(document).on('change', '.map-reach-slider input.js-range-slider', function() {
      var wrap = this.closest('.map-reach-slider');
      refresh(wrap);
      if (active === wrap) draw();
    });
    $(document).on('shiny:value shown.bs.tab shown.bs.collapse', function() {
      setTimeout(refreshAll, 0);
    });
  })();"
))

# Tabbed control panel (mirrors mst_controls / the Tree controls). A mode select
# sits above the tabs; a small client-side script (below) shows only the tabs
# and individual rows relevant to the current map mode / marker style.
map_controls <- function(ns) {
  shiny$tagList(
    bslib$navset_tab(
      # Basemap + view -------------------------------------------------------
      bslib$nav_panel(
        "Basemap",
        value = "basemap",
        icon = shiny$icon("layer-group"),
        # Countries to show, every mode alike; filled on Generate with every
        # country and its isolate count (see focus_choices()). Nothing ticked
        # is the whole world. A popup like every multi-select in the app, so
        # edits are staged until Confirm (virtual-select-popup-confirm.js) and
        # "close" reports the confirmed selection once.
        virtualSelectInput(
          ns("map_focus"),
          "Show",
          choices = NULL,
          selected = character(0),
          multiple = TRUE,
          search = TRUE,
          # The group checkbox ticks only the countries the search matches.
          selectAllOnlyVisible = TRUE,
          searchPlaceholderText = "Search countries ...",
          labelRenderer = "ptCountLabel",
          placeholder = "World (all countries)",
          optionsCount = 8,
          noOfDisplayValues = 2,
          dropboxWrapper = "body",
          showDropboxAsPopup = TRUE,
          popupDropboxBreakpoint = "10000px",
          updateOn = "close",
          width = "100%"
        ),
        shiny$div(
          id = ns("wrap_map_tiles"),
          pickerInput(
            ns("map_tiles"),
            "Base map",
            choices = map_providers,
            selected = mode_tile_defaults[["Markers"]]
          )
        ),
        bslib$input_switch(ns("map_scalebar"), "Scale bar", TRUE),
        bslib$input_switch(ns("map_minimap"), "Minimap inset", TRUE),
        bslib$input_switch(ns("map_graticule"), "Lat/long grid", FALSE),
        bslib$input_switch(ns("map_show_controls"), "Zoom controls", TRUE)
      ),
      # Markers --------------------------------------------------------------
      bslib$nav_panel(
        "Markers",
        value = "markers",
        icon = shiny$icon("location-dot"),
        bslib$accordion(
          open = "Clustering",
          bslib$accordion_panel(
            "Clustering",
            icon = shiny$icon("object-group"),
            # A single slider now fully decides clustering: 0 means off, any
            # other value is the pixel radius nearby markers combine within.
            # This folds in what used to be a separate on/off switch (and,
            # before that, both a "Marker style: Cluster" option and a
            # separate "disable clustering past this zoom" slider) -- one
            # control instead of three that could disagree with each other.
            reach_slider(
              ns,
              "map_cluster_radius",
              "Cluster radius (0 = off)",
              min = 0,
              max = 200,
              value = 0,
              step = 10,
              ring = "cluster"
            )
          ),
          bslib$accordion_panel(
            "Marker Style",
            icon = shiny$icon("sliders"),
            shiny$div(
              class = "custom-slider",
              shiny$sliderInput(
                ns("map_radius"),
                "Marker size",
                min = 3,
                max = 18,
                value = 10,
                step = 1
              )
            ),
            # The picker's own alpha slider stands in for a separate opacity
            # control; it sets the whole marker layer's opacity (see
            # build_markers()). The border is a fixed thin black outline.
            viz_helpers$viz_color(
              ns,
              "map_marker_color",
              "Fill color",
              "#2C7FB8C7",
              opacity = TRUE
            )
          )
        )
      ),
      # Variable mapping -------------------------------------------------------
      bslib$nav_panel(
        "Mapping",
        value = "color",
        icon = shiny$icon("map-pin"),
        # One picker over the *variables*, the same arrangement as the other
        # engines: picking one adds a layer and app/logic/mapping_engine.R
        # decides its palette from the variable's own profile. Markers draw it
        # as the point fill, Charts as the slices. Missing values are grey.
        viz_helpers$field_select(ns, "map_layer_add", "Map a variable"),
        shiny$uiOutput(ns("map_layers_ui"))
      ),
      # Charts (minicharts) -----------------------------------------------------
      bslib$nav_panel(
        "Charts",
        value = "charts",
        icon = shiny$icon("chart-pie"),
        pickerInput(
          ns("map_chart_type"),
          "Chart type",
          choices = c("Pie" = "pie", "Bar" = "bar", "Polar area" = "polar-area")
        ),
        # One slice per level is the default (0 = all), so the chart legend
        # reads exactly like the one Markers draws for the same variable.
        # Raising this keeps only the N most common levels and folds the rest
        # into a single "Other" slice — for a variable whose long tail would
        # otherwise slice every chart too thin to read. See build_charts().
        shiny$div(
          class = "custom-slider",
          shiny$sliderInput(
            ns("map_chart_max_categories"),
            "Top categories (0 = all)",
            min = 0,
            max = 20,
            value = 0,
            step = 1,
            ticks = FALSE
          )
        ),
        # Combine charts whose locations sit within this cell size of each
        # other on screen into a single aggregate chart, splitting apart again
        # as the map is zoomed in (see build_charts()/cluster_cell()). The
        # minichart analog of Markers-mode clustering; 0 folds in what used to
        # be a separate on/off switch, the same way map_cluster_radius's own
        # 0 does for Markers.
        reach_slider(
          ns,
          "map_chart_cluster_radius",
          "Cluster cell size (0 = off)",
          min = 0,
          max = 200,
          value = 100,
          step = 10,
          shape = "square",
          ring = "chart"
        ),
        bslib$accordion(
          open = FALSE,
          bslib$accordion_panel(
            "Chart Style",
            icon = shiny$icon("sliders"),
            # The variable each chart splits by is the mapping layer (Mapping tab).
            shiny$div(
              class = "custom-slider",
              shiny$sliderInput(
                ns("map_chart_size"),
                "Chart size",
                min = 20,
                max = 120,
                value = 40,
                step = 5
              )
            ),
            shiny$div(
              class = "custom-slider",
              shiny$sliderInput(
                ns("map_chart_opacity"),
                "Opacity",
                min = 0.1,
                max = 1,
                value = 0.7,
                step = 0.05
              )
            )
          )
        )
      ),
      # Choropleth (Region) ----------------------------------------------------
      # Choropleth's only tab of its own, so it sits where Mapping does in the
      # modes that have one. The fill is always the isolate count, so the
      # palette lives here rather than on a mapping layer.
      bslib$nav_panel(
        "Region",
        value = "region",
        icon = shiny$icon("earth-europe"),
        # What a region is. State level counts each isolate in the admin-1
        # region its point falls in (see build_map_coords()).
        radioGroupButtons(
          ns("map_region_level"),
          "Regions",
          choices = REGION_LEVELS,
          selected = "country",
          justified = TRUE,
          size = "sm"
        ),
        radioGroupButtons(
          ns("map_region_transform"),
          "Count scale",
          choices = c("Raw", "Log"),
          selected = "Raw",
          justified = TRUE,
          size = "sm"
        ),
        # A count is never categorical or diverging, so only ordered palettes.
        viz_helpers$scale_select(
          ns,
          "map_region_scale",
          categories = c("Sequential", "Gradient"),
          selected = "Oranges"
        ),
        shiny$div(
          class = "custom-slider",
          shiny$sliderInput(
            ns("map_region_opacity"),
            "Fill opacity",
            min = 0.1,
            max = 1,
            value = 0.7,
            step = 0.05
          )
        ),
        viz_helpers$viz_color(
          ns,
          "map_region_border",
          "Border color",
          "#000000"
        ),
        bslib$input_switch(
          ns("map_region_permanent"),
          "Always show labels",
          FALSE
        )
      ),
      # Heatmap (Density) -------------------------------------------------------
      bslib$nav_panel(
        "Density",
        value = "density",
        icon = shiny$icon("fire"),
        # Pared down to the two knobs that actually change what the map
        # communicates: how far each isolate's heat spreads, and how many
        # co-located isolates it takes to hit full color. Blur and the
        # minimum-opacity floor were dropped — they only fine-tune the same
        # visual effect as Radius (softness/spread) and rarely need
        # per-plot tuning, so leaving them user-adjustable mostly just added
        # ways to produce a different-looking map from identical data.
        reach_slider(
          ns,
          "map_heat_radius",
          "Radius",
          min = 5,
          max = 50,
          value = 25,
          step = 1,
          ring = "heat"
        ),
        shiny$div(
          class = "custom-slider",
          shiny$sliderInput(
            ns("map_heat_max"),
            "Max intensity (isolates)",
            min = 1,
            max = 50,
            value = 10,
            step = 1
          )
        ),
        # Heat intensity is always a non-negative isolate density, never
        # categorical or meaningfully diverging, so this picker is restricted
        # to ordered palettes, like the Region tab's count scale.
        viz_helpers$scale_select(
          ns,
          "map_heat_scale",
          categories = c("Sequential", "Gradient"),
          selected = "Oranges"
        )
      ),
      # Legend -----------------------------------------------------------------
      bslib$nav_panel(
        "Legend",
        value = "legend",
        icon = shiny$icon("list"),
        bslib$input_switch(ns("map_legend"), "Show legend", TRUE),
        pickerInput(
          ns("map_legend_pos"),
          "Position",
          selected = "topleft",
          choices = c(
            "Bottom right" = "bottomright",
            "Bottom left" = "bottomleft",
            "Top right" = "topright",
            "Top left" = "topleft"
          )
        ),
        # allow-free-text: a legend title is display text, so it takes spaces
        # and punctuation, and opts out of the identifier charset restriction
        # that index.js applies to text inputs.
        shiny$div(
          class = "allow-free-text",
          shiny$textInput(
            ns("map_legend_title"),
            "Title",
            placeholder = "(variable name)"
          )
        ),
        shiny$div(
          class = "custom-slider",
          shiny$sliderInput(
            ns("map_legend_opacity"),
            "Opacity",
            min = 0.1,
            max = 1,
            # Default matches the shared $map-control-opacity in main.scss so
            # the legend's backing lines up with every other floating map
            # control out of the box; the user can still freely retune it.
            value = 0.85,
            step = 0.05
          )
        ),
        shiny$div(
          id = ns("wrap_map_legend_digits"),
          shiny$numericInput(
            ns("map_legend_digits"),
            "Number digits",
            value = 2,
            min = 0,
            max = 6
          )
        )
      ),
      # Popup + hover labels -------------------------------------------------
      bslib$nav_panel(
        "Labels",
        value = "labels",
        icon = shiny$icon("tag"),
        # The database's columns replace these two sentinel-only choice lists
        # as soon as it loads (see populate_metadata_selects()). update_on =
        # "close": the map only needs to redraw once the picker is done being
        # clicked through, not on every individual selection -- ticking
        # several boxes in quick succession otherwise raced the dropdown's own
        # open state and could close it mid-click.
        viz_helpers$field_select(
          ns,
          "map_popup",
          sprintf("Popup fields (up to %d)", MAX_LABEL_FIELDS),
          selected = POPUP_DEFAULT,
          extra = label_sentinels("map_popup"),
          placeholder = "Pick fields ...",
          multiple = TRUE,
          max_values = MAX_LABEL_FIELDS,
          update_on = "close"
        ),
        viz_helpers$field_select(
          ns,
          "map_hover_field",
          sprintf("Hover fields (up to %d)", MAX_LABEL_FIELDS),
          selected = HOVER_DEFAULT,
          extra = label_sentinels("map_hover_field"),
          placeholder = "Pick fields ...",
          multiple = TRUE,
          max_values = MAX_LABEL_FIELDS,
          update_on = "close"
        ),
        bslib$input_switch(ns("map_permanent"), "Always show labels", FALSE),
        shiny$div(
          class = "custom-slider",
          shiny$sliderInput(
            ns("map_label_size"),
            "Label size",
            min = 8,
            max = 20,
            value = 12,
            step = 1
          )
        )
      ),
      # Temporal filter --------------------------------------------------------
      bslib$nav_panel(
        "Time",
        value = "time",
        icon = shiny$icon("clock"),
        shiny$div(
          class = "custom-slider date-slider",
          # Covered while Play is running (see the anim_playing observer): the
          # per-tick updateSliderInput redraws the ion.rangeSlider — handles,
          # rotated tick labels and all — on every frame, which reads as a
          # flickering, half-broken control. The window it would show is already
          # narrated by the on-map date label and map_anim_label, so the slider
          # is hidden behind an opaque "Animating…" cover for the duration
          # rather than left twitching.
          id = ns("daterange_slider_wrap"),
          shiny$sliderInput(
            ns("map_daterange"),
            NULL,
            min = Sys.Date() - 30,
            max = Sys.Date(),
            value = c(Sys.Date() - 30, Sys.Date()),
            timeFormat = "%Y-%m-%d"
          ),
          shiny$div(
            class = "date-slider_cover",
            shiny$icon("circle-notch", class = "fa-spin"),
            shiny$span("Animating…")
          )
        ),
        radioGroupButtons(
          ns("map_interval"),
          "Interval",
          choices = c("Hour", "Day", "Week", "Month", "Year"),
          selected = "Day",
          justified = TRUE,
          size = "sm"
        ),
        shiny$div(
          class = "time-step-buttons",
          shiny$div(
            class = "btn-group time-step-group",
            shiny$actionButton(
              ns("map_step_start_prev"),
              NULL,
              icon = shiny$icon("angle-left"),
              class = "btn-step",
              title = "Move start date back one interval"
            ),
            shiny$actionButton(
              ns("map_step_start_next"),
              NULL,
              icon = shiny$icon("angle-right"),
              class = "btn-step",
              title = "Move start date forward one interval"
            )
          ),
          shiny$actionButton(
            ns("map_play"),
            "Play",
            icon = shiny$icon("play"),
            class = "btn-play"
          ),
          shiny$div(
            class = "btn-group time-step-group",
            shiny$actionButton(
              ns("map_step_end_prev"),
              NULL,
              icon = shiny$icon("angle-left"),
              class = "btn-step",
              title = "Move end date back one interval"
            ),
            shiny$actionButton(
              ns("map_step_end_next"),
              NULL,
              icon = shiny$icon("angle-right"),
              class = "btn-step",
              title = "Move end date forward one interval"
            )
          )
        ),
        shiny$tags$small(
          class = "text-muted",
          # Class rather than the namespaced id, so the spacing rule survives
          # one map instance per plot tab. It sits on the textOutput span
          # itself — the same element the old #...-map_anim_label rule matched.
          shiny$tagAppendAttributes(
            shiny$textOutput(ns("map_anim_label")),
            class = "map-anim-label"
          )
        ),
        # Which Date or Year variable the slider filters by, rendered
        # server-side so it is created carrying this data's choices.
        shiny$uiOutput(ns("date_field_ui")),
        bslib$input_switch(
          ns("map_show_time_label"),
          "Annotate date range on map",
          TRUE
        ),
        bslib$input_switch(
          ns("map_region_fixed_scale"),
          "Fix color scale to full date range",
          TRUE
        )
      )
    ),
    reach_script,
    shiny$div(
      class = "viz-mode-dropup",
      pickerInput(
        ns("map_mode"),
        "Map mode",
        choices = c("Markers", "Choropleth", "Heatmap", "Charts")
      )
    ),
    shiny$div(
      class = "reset-buttons",
      shiny$actionButton(
        ns("map_reset"),
        "Reset view",
        icon = shiny$icon("expand"),
        width = "100%"
      ),
      shiny$actionButton(
        ns("reset_settings"),
        "Reset settings",
        icon = shiny$icon("rotate-left"),
        width = "100%"
      )
    ),
    # Show only the control tabs — and, within them, the individual control
    # rows — relevant to the current map mode. Done in the browser (not via
    # server nav_show/nav_hide / toggle on many small ids) so it can
    # never throw "Node cannot be found" and interfere with Shiny's render
    # batch.
    shiny$tags$script(shiny$HTML(local({
      mode_id <- ns("map_mode")
      tiles_wrap_id <- ns("wrap_map_tiles")
      paste0(
        "(function(){",
        "var modeSel='#",
        mode_id,
        "';",
        "var tilesWrapId='",
        tiles_wrap_id,
        "';",
        # Mapping ('color') only where a variable is drawn; Region only for
        # Choropleth, which fills by isolate count instead.
        "var tabsByMode={",
        "Markers:['basemap','markers','color','legend','labels','time'],",
        "Choropleth:['basemap','region','legend','time'],",
        "Heatmap:['basemap','time','density','legend'],",
        "Charts:['basemap','color','time','charts','legend']};",
        "var allTabs=['basemap','markers','color','region','legend',",
        "'labels','time','density','charts'];",
        "function curMode(){var el=document.querySelector(modeSel);return el?el.value:'Markers';}",
        "function apply(){",
        "var modeEl=document.querySelector(modeSel);if(!modeEl)return;",
        "var wrap=modeEl.closest('.viz-nav-wrap');if(!wrap)return;",
        "var m=curMode();",
        "var vis=(tabsByMode[m]||allTabs).slice();",
        "allTabs.forEach(function(v){",
        "var link=wrap.querySelector('.nav-link[data-value='+JSON.stringify(v)+']');",
        "if(link){var li=link.closest('.nav-item');",
        "if(li){li.style.display=(vis.indexOf(v)>=0)?'':'none';}}",
        "});",
        "var active=wrap.querySelector('.nav-link.active');",
        "if(active&&vis.indexOf(active.getAttribute('data-value'))<0){",
        "var f=wrap.querySelector('.nav-link[data-value='+JSON.stringify(vis[0])+']');",
        "if(f){f.click();}",
        "}",
        "var tiles=document.getElementById(tilesWrapId);",
        "if(tiles)tiles.style.display=(m==='Choropleth')?'none':'';",
        "}",
        "$(document).on('change',modeSel,apply);",
        "var n=0,t=setInterval(function(){n++;",
        "if(document.querySelector(modeSel)){apply();}if(n>40){clearInterval(t);}},300);",
        "})();"
      )
    })))
  )
}

#' Map Status for the Setup Sidebar
#'
#' The last Generate's mapping summary and data advisories, mounted by
#' app/view/visualization_plot.R at the bottom of the plot tab's left sidebar.
#' Rendered by this module's `output$map_geocode_status_ui`.
#'
#' @param id The engine's namespaced id, the same one passed to `ui()`.
#' @return A `uiOutput`.
#' @export
setup_ui <- function(id) {
  shiny$uiOutput(shiny$NS(id, "map_geocode_status_ui"), class = "map-geocode-status")
}

# `options_ui` is accepted for one signature across all engines; this one has no
# distance-computation controls, so the tab never passes any.
#' @export
ui <- function(id, generate_id, options_ui = NULL) {
  ns <- shiny$NS(id)

  bslib$layout_sidebar(
    # See visualization_mst.R: `padding` replaces the old non-unique
    # `id = "plot-sidebar"`, which layout_sidebar has no formal for.
    padding = 0,
    border = FALSE,
    sidebar = bslib$sidebar(
      id = ns("controls_sidebar"),
      class = "viz-controls-sidebar",
      position = "right",
      width = 380,
      open = TRUE,
      fillable = TRUE,
      bslib$as_fill_carrier(
        shiny$div(
          id = ns("controls_wrap"),
          class = "viz-nav-wrap",
          map_controls(ns)
        )
      )
    ),
    useShinyjs(),
    useWaiter(),
    # Hide the "press Generate" prompt the instant Generate is clicked —
    # mirrors the MST/Tree engines' loading-overlay script — rather than
    # waiting for `generated()` to flip server-side, which for Map only
    # happens after geocoding finishes (the prompt would otherwise stay
    # visible underneath the geocoding waiter for the whole wait).
    shiny$tags$script(
      shiny$HTML(
        paste0(
          "(function(){",
          "var gen='",
          generate_id,
          "';var stage='",
          ns("plot_stage"),
          "';",
          "$(document).on('click','#'+gen.replace(/([:.])/g,'\\\\$1'),",
          "function(){",
          "var s=document.getElementById(stage);if(!s)return;",
          # Ignore Generate clicks while this engine's panel is hidden (the
          # sibling engine is active) — offsetParent is null when display:none.
          "if(s.offsetParent===null)return;",
          "var p=s.querySelector('.viz-plot-prompt');if(p)p.style.display='none';",
          "});",
          "})();"
        )
      )
    ),
    # Drives the geocoding waiter's live "Remaining time M:SS / M:SS" countdown.
    # Geocoding blocks the R session, so the tick can't be pushed from the
    # server; instead this runs once on page load and watches for the
    # `.viz-countdown` element the waiter injects (see geocode_waiter_html()),
    # counting its data-total seconds down in the browser. When geocoding takes
    # longer than estimated it keeps counting into negative time (-0:05), and
    # stops once the waiter, and with it the element, is torn down. One
    # observer covers every show.
    shiny$tags$script(
      shiny$HTML(
        "(function(){
           function fmt(s){
             s=Math.round(s);
             var sign=s<0?'-':'';
             s=Math.abs(s);
             var m=Math.floor(s/60),sec=s%60;
             return sign+m+':'+(sec<10?'0':'')+sec;
           }
           function start(el){
             if(el._countdownStarted)return;
             el._countdownStarted=true;
             var total=parseInt(el.getAttribute('data-total'),10)||0;
             var overall=parseInt(el.getAttribute('data-overall'),10)||total;
             var remaining=total;
             function render(){
               el.textContent='Remaining time '+fmt(remaining)+' / '+fmt(overall);
             }
             render();
             var timer=setInterval(function(){
               if(!document.body.contains(el)){clearInterval(timer);return;}
               remaining-=1;
               render();
             },1000);
           }
           function scan(n){
             if(n.nodeType!==1)return;
             if(n.classList&&n.classList.contains('viz-countdown')){start(n);}
             if(n.querySelectorAll){n.querySelectorAll('.viz-countdown').forEach(start);}
           }
           // One body-wide observer for the whole app: it matches on the
           // .viz-countdown class, so it already covers every map instance,
           // and without this guard each plot tab would add another.
           if(!window.__phylotraceCountdownRegistered){
             window.__phylotraceCountdownRegistered=true;
             new MutationObserver(function(muts){
               muts.forEach(function(m){m.addedNodes.forEach(scan);});
             }).observe(document.body,{childList:true,subtree:true});
           }
         })();"
      )
    ),
    bslib$card(
      full_screen = TRUE,
      class = "plot-card map-plot-card",
      bslib$card_body(shiny$uiOutput(ns("plot_area")))
    )
  )
}

#' @export
server <- function(
  id,
  db_path = shiny$reactive(NULL),
  db_rev = db_events$new_bus(),
  viz_metadata = shiny$reactive(NULL),
  # Per-column profile of the metadata: declared type, distinct-value count,
  # coverage and group, built once by the coordinator
  # (app/logic/field_profile.R). Field pickers read it so every engine
  # describes a variable the same way.
  field_profiles = shiny$reactive(NULL),
  # Accepted for a uniform `shared` bundle from the coordinator; the Map subsets
  # straight from the filtered viz_metadata(), so it needs no separate handling.
  selected_isolates = shiny$reactive(NULL),
  na_handling = shiny$reactive("ignore_na"),
  generate = shiny$reactive(0L),
  plot_type = shiny$reactive("MST"),
  # TRUE while this engine's panel is the visible one. Leaflet initialises at
  # zero size in a hidden container and never recomputes on its own, so the
  # coordinator drives this to trigger the resize nudge below. It used to be
  # inferred from plot_type(), which no longer changes now that each plot tab
  # fixes its type for life.
  visible = shiny$reactive(TRUE)
) {
  shiny$moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Whether a map has been generated for this engine. Drives the "press
    # Generate" prompt overlay (and keeps the map itself hidden until then, so
    # the base tiles and the first batch of coordinates always appear
    # together). Retained across plot-type switches (only session reset
    # clears it) — mirrors the MST/Tree engines.
    generated <- shiny$reactiveVal(FALSE)

    # Geocoded coordinates for the currently generated map. Held in a
    # reactiveVal (not eventReactive) so a Generate for another engine — which
    # also ticks the shared generate() — leaves this engine's result untouched.
    map_coords <- shiny$reactiveVal(NULL)

    # Every isolate naming a recognised country, geocoded or not (see
    # build_map_coords()'s `regions`): what Choropleth mode counts.
    map_regions <- shiny$reactiveVal(NULL)

    # The polygons each finer choropleth level draws (build_map_coords()'s
    # `shapes`), picked on Generate. Set together with map_coords(), whose
    # region_admin1 column they belong to, so the two never disagree.
    map_shapes <- shiny$reactiveVal(NULL)

    # The variable mapped onto the markers or charts. At most one (see MEDIUM),
    # but held as a list, because that is the shape every engine's mapping
    # state, snapshot and restore path share.
    map_layers <- shiny$reactiveVal(list())
    map_layer_seq <- shiny$reactiveVal(0L)

    # Whether the popup and hover pickers have been filled from a database yet
    # (see populate_metadata_selects()). Plain state, not reactive: nothing
    # should re-run because it flipped.
    labels_filled <- FALSE

    # Geocoding success/failure summary for the currently generated map (see
    # build_map_coords()'s status list) — drives output$map_geocode_status_ui
    # below, shown at the bottom of the plot tab's left sidebar (setup_ui()).
    map_geocode_status <- shiny$reactiveVal(NULL)

    # How many isolates the last Generate was able to place on the map, with a
    # warning + a few example place names when some couldn't be resolved, then
    # one advisory line per data problem found on the way (see geo_advisories()).
    # NULL (the "Not available" placeholder) until a Generate has produced a
    # status.
    # Bumped when the geocode cache is cleared, so the status re-reads its size.
    cache_rev <- shiny$reactiveVal(0L)

    output$map_geocode_status_ui <- shiny$renderUI({
      cache_rev()
      s <- status_for_view(map_geocode_status(), input$map_mode, input$map_region_level)
      n_cache <- nrow(read_geocodes(db_path()))
      clear_link <- if (n_cache > 0) {
        shiny$actionLink(
          session$ns("map_clear_cache"),
          sprintf("Clear geocode cache (%d place%s)", n_cache, if (n_cache == 1) "" else "s"),
          class = "small geo-cache-link"
        )
      }
      advisories <- if (!is.null(s)) geo_advisories(s, session$ns("map_geo_problems"))
      if (is.null(s) || s$n_isolates == 0) {
        return(shiny$tagList(
          shiny$div(class = "text-muted small", "No isolates mapped"),
          shiny$div(class = "text-muted small mb-2", "No location geocoded"),
          advisories,
          clear_link
        ))
      }
      # Distinct places resolved (each backs one or more of the mapped isolates)
      # — shown under the isolate summary so it's clear how many geocode lookups
      # the mapped isolates collapsed to, plus how many of those came from the
      # database's geocode cache vs. were freshly fetched this Generate
      # (see build_map_coords()'s status / geocode_places_cached()). The cache
      # breakdown is only appended when there's actually something cached to
      # report, so a first-ever run stays a plain count.
      cache_note <- if (s$n_cached > 0 && s$n_new > 0) {
        sprintf(" (%d cached, %d newly fetched)", s$n_cached, s$n_new)
      } else if (s$n_cached > 0) {
        " (all from cache)"
      } else {
        ""
      }
      locations_line <- shiny$div(
        class = "small text-muted mb-2",
        sprintf(
          "%d location%s geocoded%s",
          s$n_locations,
          if (s$n_locations == 1) "" else "s",
          cache_note
        )
      )
      headline <- if (s$n_failed_places == 0) {
        shiny$div(
          class = "mt-2 small text-success",
          shiny$icon("circle-check"),
          sprintf(" All %d isolates mapped", s$n_mapped)
        )
      } else {
        why <- if (isTRUE(s$offline)) {
          paste(
            "Nominatim (nominatim.openstreetmap.org) could not be reached, so",
            "no new places were geocoded. Isolates with coordinates or with",
            "places geocoded before are still shown. Not found:"
          )
        } else if (nzchar(s$geocode_error %||% "")) {
          paste0("The geocoder failed (", s$geocode_error, "). Not found:")
        } else {
          "Could not geocode:"
        }
        bslib$tooltip(
          shiny$div(
            class = "mt-2 small text-warning",
            shiny$icon("triangle-exclamation"),
            sprintf(
              " %d of %d isolates mapped%s",
              s$n_mapped,
              s$n_isolates,
              if (isTRUE(s$offline)) " (offline)" else ""
            )
          ),
          paste(why, s$failed_preview)
        )
      }
      shiny$tagList(headline, locations_line, advisories, clear_link)
    })

    # Ask before clearing the geocode cache: refilling it costs about a second
    # per place and needs an internet connection.
    shiny$observeEvent(input$map_clear_cache, {
      n_cache <- nrow(read_geocodes(db_path()))
      shiny$showModal(shiny$modalDialog(
        title = "Clear geocode cache?",
        easyClose = TRUE,
        shiny$p(sprintf(
          paste(
            "This database keeps the coordinates of %d place%s looked up for",
            "its isolates. Clearing them makes the next",
            "Generate look each place up again, which needs an internet",
            "connection and takes about a second per place. Use this when a",
            "place was put in the wrong spot. The current map does not change."
          ),
          n_cache,
          if (n_cache == 1) "" else "s"
        )),
        footer = shiny$tagList(
          shiny$modalButton("Cancel"),
          shiny$actionButton(
            session$ns("map_clear_cache_confirm"),
            "Clear cache",
            class = "btn-danger"
          )
        )
      ))
    })

    # Delete the cache once confirmed and refresh the count shown.
    shiny$observeEvent(input$map_clear_cache_confirm, {
      ok <- clear_geocodes(db_path())
      shiny$removeModal()
      cache_rev(cache_rev() + 1L)
      shiny$showNotification(
        if (ok) "Geocode cache cleared." else "The geocode cache could not be deleted.",
        type = if (ok) "message" else "error"
      )
    })

    # Every isolate behind the advisories, for when there are more than the
    # tooltips name.
    shiny$observeEvent(input$map_geo_problems, {
      shown <- status_for_view(map_geocode_status(), input$map_mode, input$map_region_level)
      probs <- shown$problems
      shiny$req(!is.null(probs), nrow(probs) > 0)
      shiny$showModal(shiny$div(
        class = "modal-wide",
        shiny$modalDialog(
          title = "Geographic data to check",
          easyClose = TRUE,
          shiny$p(
            class = "small text-muted",
            "Correct these in the Database browser. Values are shown as entered."
          ),
          geo_problem_table(probs),
          footer = shiny$modalButton("Close")
        )
      ))
    })

    # --- Time animation -------------------------------------------------------
    # The full extent "Date range" can be dragged across — i.e. the slider's
    # min/max, not its current value. Shiny doesn't expose a slider's own
    # min/max back through `input`, only its value, so this is tracked
    # server-side wherever the slider's min/max are (re)programmed: the coded
    # default below, matching the sliderInput() in the UI, and the Generate/
    # Reset-settings updates further down that refit it to the data's actual
    # date span. The step buttons clamp to this so stepping can't run past
    # the data; see step_daterange() below.
    daterange_bounds <- shiny$reactiveVal(c(Sys.Date() - 30, Sys.Date()))

    # Whether the "Play" animation is currently running. Drives the tick loop
    # below, the Play/Pause button label, and disabling the date-range/interval
    # controls (a manual drag mid-playback is indistinguishable server-side
    # from an animation tick, so the controls are simply locked while playing).
    anim_playing <- shiny$reactiveVal(FALSE)

    # Which bin the animation is currently on. This is the source of truth for
    # "what's the next frame" — NOT input$map_daterange. shiny$updateSliderInput()
    # only pushes a value to the client; it does not update input$map_daterange
    # until the browser echoes the new value back on a later flush. Reading the
    # slider back to decide the next bin raced that round trip: on the very
    # first Play click, the tick loop below would run in the same flush as the
    # slider-snap, see the *old* (already-at-the-end) value, conclude there was
    # nothing left to animate, and immediately stop — requiring a second click
    # once the first click's update had finally echoed back. Tracking the index
    # as plain server-side state sidesteps the round trip entirely.
    anim_idx <- shiny$reactiveVal(0L)

    # Frozen snapshot of the bin boundaries for the CURRENT playback run only
    # — deliberately a reactiveVal, not a live reactive off input$map_daterange.
    # The tick loop below updates map_daterange every frame; if the bins were
    # recomputed live from that same input, each tick would invalidate its own
    # bin set and re-trigger independently of the 750ms shiny$invalidateLater(),
    # racing/ignoring the intended pacing. Freezing it once at Play-start (from
    # whatever range is selected on the slider *then*) avoids that feedback
    # loop entirely, and is also what lets the user pre-narrow "Date range" to
    # animate just that window instead of the whole dataset.
    anim_bins <- shiny$reactiveVal(NULL)

    # Live preview of what the bins *would* be for the currently selected date
    # range + interval — used only to enable/disable the Play button, so it's
    # safe to recompute on every change (nothing here writes back to
    # map_daterange). seq() does not reliably land on the exact upper bound for
    # week/month/year steps, so it is appended if missing — otherwise the last
    # stretch of the selected range would never be revealed by the animation.
    # "Hour" collapses to day-granularity stepping (filter_coords() floors to
    # whole days, so an actual hourly step over date-only precision would
    # silently do nothing) — shared by the Play preview below and the step
    # buttons further down, so both agree on what one interval unit means.
    interval_unit <- function() {
      switch(
        input$map_interval %||% "Day",
        Hour = "day",
        Day = "day",
        Week = "week",
        Month = "month",
        Year = "year"
      )
    }

    anim_bins_preview <- shiny$reactive({
      rng <- input$map_daterange
      shiny$req(rng)
      lo <- as.Date(rng[1])
      hi <- as.Date(rng[2])
      shiny$req(lo <= hi)
      bins <- seq(lo, hi, by = interval_unit())
      if (!length(bins) || tail(bins, 1) < hi) {
        bins <- c(bins, hi)
      }
      unique(bins)
    })

    # Start/stop the animation. On start, freeze the bins for this run from
    # whatever is currently selected on "Date range", then snap to the first
    # bin so playback always begins from the same, predictable frame.
    shiny$observeEvent(input$map_play, {
      shiny$req(generated())
      if (isTRUE(anim_playing())) {
        anim_playing(FALSE)
        anim_idx(0L)
        return(invisible(NULL))
      }
      bins <- shiny$isolate(anim_bins_preview())
      shiny$req(length(bins) >= 2)
      anim_bins(bins)
      anim_idx(1L)
      shiny$updateSliderInput(
        session,
        "map_daterange",
        value = c(bins[1], bins[1])
      )
      anim_playing(TRUE)
    })

    # Reflect play/pause state in the button and lock the controls an
    # in-progress animation is driving.
    shiny$observeEvent(
      anim_playing(),
      {
        playing <- isTRUE(anim_playing())
        shiny$updateActionButton(
          session,
          "map_play",
          label = if (playing) "Pause" else "Play",
          icon = shiny$icon(if (playing) "pause" else "play")
        )
        # Lock every other Time control for the duration of playback: a manual
        # drag, interval switch or scale/label toggle mid-run either races the
        # tick loop that is driving map_daterange or rebins the very frames it
        # is walking. Play itself stays live so it can pause. The step buttons
        # are NOT touched here — they are owned solely by the enable/disable
        # observe further down (which already keys off anim_playing()), so no
        # control is written by two observers in the same flush.
        toggleState("map_daterange", condition = !playing)
        toggleState("map_interval", condition = !playing)
        toggleState("map_show_time_label", condition = !playing)
        toggleState("map_region_fixed_scale", condition = !playing)
        # Hide the flickering slider behind its cover (see the Time nav_panel).
        toggleClass(
          id = "daterange_slider_wrap",
          class = "date-slider--playing",
          condition = playing
        )
      },
      ignoreInit = TRUE
    )

    # Manual stepping (the two arrow-pairs) is deliberately independent of
    # Play's frozen bins/index — each click always reads "Date range" +
    # "Interval" fresh off the live inputs and nudges just ONE end of the
    # currently displayed window by one interval unit, rather than resuming
    # some earlier frozen sequence. A step-based approach that shared Play's
    # frozen state (an earlier version of this code did) goes stale the
    # moment the user drags the slider or changes Interval without going
    # through Play/Next first — exactly the "doesn't react to the currently
    # selected input" bug this replaces. Reading live inputs on every click
    # can't go stale.
    #
    # `which` is "start" (the left handle, i.e. rng[1]) or "end" (the right
    # handle, rng[2]). Each is clamped against the *other* handle — the
    # start can't be pushed past the end, or vice versa — as well as against
    # daterange_bounds(), the slider's own extent, so stepping can't run
    # past the data.
    step_daterange <- function(which, sign) {
      rng <- input$map_daterange
      shiny$req(rng)
      bounds <- shiny$isolate(daterange_bounds())
      shiny$req(bounds)
      lo <- as.Date(rng[1])
      hi <- as.Date(rng[2])
      by <- paste(sign, interval_unit())
      if (identical(which, "start")) {
        new_lo <- seq(lo, by = by, length.out = 2)[2]
        new_lo <- max(new_lo, bounds[1])
        new_lo <- min(new_lo, hi)
        shiny$updateSliderInput(session, "map_daterange", value = c(new_lo, hi))
      } else {
        new_hi <- seq(hi, by = by, length.out = 2)[2]
        new_hi <- min(new_hi, bounds[2])
        new_hi <- max(new_hi, lo)
        shiny$updateSliderInput(session, "map_daterange", value = c(lo, new_hi))
      }
    }

    shiny$observeEvent(input$map_step_start_prev, {
      shiny$req(generated())
      step_daterange("start", -1)
    })

    shiny$observeEvent(input$map_step_start_next, {
      shiny$req(generated())
      step_daterange("start", 1)
    })

    shiny$observeEvent(input$map_step_end_prev, {
      shiny$req(generated())
      step_daterange("end", -1)
    })

    shiny$observeEvent(input$map_step_end_next, {
      shiny$req(generated())
      step_daterange("end", 1)
    })

    # The tick loop: advance one bin every 750ms (comfortably above map_opts()'s
    # 250ms debounce, so every tick reliably produces a rendered frame) while
    # playing. Mirrors the guarded shiny$observe()+shiny$invalidateLater() polling pattern
    # already used in app/view/typing.R for the typing-progress loop. Stops
    # (does not reschedule) once the bins are exhausted; also goes dormant
    # while this plot's tab is in the background — a hidden tab is only
    # display:none, and Shiny auto-suspends hidden *outputs* but not a running
    # shiny$observe() loop — and resumes automatically once visible() next
    # reads back TRUE, since reading it here creates the reactive dependency.
    # Always reveals cumulatively from the frozen bins' first (selected) bound.
    shiny$observe({
      if (!isTRUE(anim_playing())) {
        return(NULL)
      }
      if (!isTRUE(visible())) {
        return(NULL)
      }
      bins <- anim_bins()
      shiny$req(bins)
      idx <- shiny$isolate(anim_idx())
      if (idx >= length(bins)) {
        anim_playing(FALSE)
        anim_idx(0L)
        return(NULL)
      }
      nxt_idx <- idx + 1L
      anim_idx(nxt_idx)
      shiny$updateSliderInput(
        session,
        "map_daterange",
        value = c(bins[1], bins[nxt_idx])
      )
      shiny$invalidateLater(750, session)
    })

    output$map_anim_label <- shiny$renderText({
      rng <- input$map_daterange
      shiny$req(rng)
      format_daterange_label(rng)
    })

    # Single owner of the Play button and the four step buttons' enabled state
    # (the anim_playing observer above deliberately does NOT touch these, so
    # they can never be written by two observers in one flush). Reads
    # anim_playing() so it re-evaluates on every play/pause transition too.
    #
    # A single date (or an interval with only one real boundary) has nothing to
    # animate through, so Play is disabled then — but NOT while an animation is
    # already running: its very first frame collapses "Date range" to a single
    # point (bins[1]..bins[1]), which would otherwise disable the button at the
    # exact moment the user needs it to read "Pause", so playing forces it on.
    # Each step button is disabled once nudging its own handle would run past
    # the data on that side, or past the *other* handle — reactive off the live
    # "Date range" value (not a frozen copy), so they stay in sync with
    # whatever's currently selected — and unconditionally while playing.
    shiny$observe({
      playing <- isTRUE(anim_playing())
      preview <- tryCatch(anim_bins_preview(), error = function(e) NULL)
      toggleState(
        "map_play",
        condition = playing || length(preview) >= 2
      )

      rng <- input$map_daterange
      bounds <- daterange_bounds()
      can_step <- !playing && !is.null(rng) && !is.null(bounds)
      lo <- if (can_step) as.Date(rng[1]) else NA
      hi <- if (can_step) as.Date(rng[2]) else NA

      toggleState(
        "map_step_start_prev",
        condition = can_step && lo > bounds[1]
      )
      toggleState(
        "map_step_start_next",
        condition = can_step && lo < hi
      )
      toggleState(
        "map_step_end_prev",
        condition = can_step && hi > lo
      )
      toggleState(
        "map_step_end_next",
        condition = can_step && hi < bounds[2]
      )
    })

    # Spinner shown while geocoding (a blocking network call). Targets the
    # always-visible stage wrapper, not the map div itself — the map stays
    # hidden until `generated` flips TRUE (see below), so a target scoped to
    # it would have zero size on the very first Generate. The html is a generic
    # placeholder here and swapped for the location-count/estimate version (see
    # geocode_waiter_html()) at show time in the Generate observer, once the
    # metadata — and therefore the distinct-place count — is known.
    waiter <- Waiter$new(
      id = ns("plot_stage"),
      html = geocode_waiter_html(0L),
      # A faint white veil (rather than fully transparent) so the overlay reads
      # as covering the plot area while geocoding, dimming any already-visible
      # basemap underneath on a re-Generate; the spinner/text are the dark
      # accent for contrast against it — see .viz-spinner-dark in main.scss
      # (shared with MST/Tree's own loading overlay).
      color = "rgba(255, 255, 255, 0.85)"
    )

    # All styling controls bundled together, debounced so dragging a slider
    # doesn't rebuild the map on every intermediate value.
    map_opts <- shiny$debounce(
      shiny$reactive({
        list(
          mode = input$map_mode %||% "Markers",
          focus = input$map_focus,
          # A basemap no longer offered (a saved plot on a CARTO one) draws
          # the default rather than a watermarked tile.
          tiles = if (isTRUE(input$map_tiles %in% map_providers)) {
            input$map_tiles
          } else {
            "OpenStreetMap"
          },
          scalebar = isTRUE(input$map_scalebar),
          minimap = isTRUE(input$map_minimap),
          graticule = isTRUE(input$map_graticule),
          show_controls = isTRUE(input$map_show_controls),
          radius = input$map_radius %||% 10,
          marker_color = input$map_marker_color %||% "#2C7FB8C7",
          layer = mapped_layer(),
          legend = isTRUE(input$map_legend),
          legend_pos = input$map_legend_pos %||% "topleft",
          legend_title = input$map_legend_title %||% "",
          legend_opacity = input$map_legend_opacity %||% 0.85,
          legend_digits = input$map_legend_digits %||% 2,
          popup_fields = input$map_popup,
          hover_field = input$map_hover_field,
          permanent = isTRUE(input$map_permanent),
          label_size = input$map_label_size %||% 12,
          daterange = input$map_daterange,
          date_field = time_field(),
          date_bounds = daterange_bounds(),
          show_time_label = isTRUE(input$map_show_time_label),
          cluster = (input$map_cluster_radius %||% 0) > 0,
          cluster_radius = input$map_cluster_radius %||% 0,
          region_level = input$map_region_level %||% "country",
          region_transform = input$map_region_transform %||% "Raw",
          region_fixed_scale = isTRUE(input$map_region_fixed_scale),
          region_scale = input$map_region_scale %||% "Oranges",
          region_opacity = input$map_region_opacity %||% 0.7,
          region_border = input$map_region_border %||% "#000000",
          region_permanent = isTRUE(input$map_region_permanent),
          heat_radius = input$map_heat_radius %||% 25,
          heat_max = input$map_heat_max %||% 10,
          heat_scale = input$map_heat_scale %||% "viridis",
          chart_type = input$map_chart_type %||% "pie",
          chart_size = input$map_chart_size %||% 40,
          chart_opacity = input$map_chart_opacity %||% 0.7,
          chart_cluster = (input$map_chart_cluster_radius %||% 100) > 0,
          chart_cluster_radius = input$map_chart_cluster_radius %||% 100,
          chart_max_categories = input$map_chart_max_categories %||% 0
        )
      }),
      250
    )

    # The plot output element is kept mounted so that each Generate re-renders
    # the *same* output; the "press Generate" prompt is an overlay toggled
    # separately (see the `generated` observer below). This block has no live
    # reactive reads, so it only runs once at the initial flush — the
    # leafletOutput node it creates is never torn down and rebuilt.
    #
    # Both the prompt's and the map wrapper's *initial* visibility are baked
    # in here via `shiny$isolate(generated())` rather than left to the `generated`
    # observer below: that observer's toggle() calls are reactive and
    # would race the very first client-side paint (the toggle message can
    # arrive before the DOM node it targets exists, and get silently
    # dropped — leaving the map wrapper visible by default). Embedding the
    # correct initial style directly in this renderUI's first output avoids
    # that race entirely, since it's delivered atomically with the node
    # itself; the observer only has to handle *later* show/hide transitions,
    # by which point the node definitely exists.
    output$plot_area <- shiny$renderUI({
      render_info("map_plot plot_area")
      prompt <- shiny$div(
        id = ns("viz_prompt"),
        class = "viz-plot-prompt",
        style = if (isTRUE(shiny$isolate(generated()))) {
          "display:none;"
        } else {
          NULL
        },
        shiny$icon("earth-europe", class = "viz-plot-icon"),
        shiny$p(
          "Configure the Map options, then press ",
          shiny$tags$strong("Generate Plot"),
          "."
        )
      )
      shiny$div(
        class = "map-plot-stage html-fill-container html-fill-item",
        id = ns("plot_stage"),
        prompt,
        shiny$div(
          id = ns("map_wrap"),
          style = if (isTRUE(shiny$isolate(generated()))) {
            "height:100%;"
          } else {
            "display:none; height:100%;"
          },
          # The class (not the namespaced id) is what main.scss hangs the
          # neutral #ddd backing off, so it keeps working for every plot tab's
          # own map instance. See the .viz-map-canvas rule for why it exists.
          shiny$tagAppendAttributes(
            leaflet$leafletOutput(ns("map"), height = "100%")
            # class = "viz-map-canvas"
          )
        )
      )
    })

    # Set TRUE by every Generate to make the renderer (re)frame the view on the
    # new data, and held TRUE across the several re-renders one Generate can
    # trigger — the generate observer also reprograms the date / interval / heat
    # controls, each of which invalidates the debounced map_opts() ~250ms later
    # and fires a second render pass. It's cleared only once the user moves the
    # map themselves (see the map_user_view observer below). Tracking a mere
    # "did the coords change" flag inside the render instead raced that second
    # pass: the coords were already recorded, so the pass fell back to
    # setView(current zoom/center) — but Leaflet reports a freshly fitted view
    # only asynchronously, so it read the *pre-Generate* view and snapped the map
    # back to the previous data's frame instead of the new isolates'.
    pending_fit <- shiny$reactiveVal(FALSE)

    # Whole-map render. Reacts to the coordinates and the debounced control
    # bundle; date-filtering and widget assembly are delegated to the helpers.
    # While hidden (not yet generated — see the `generated` toggle below),
    # Shiny suspends this output entirely, so no tiles are ever built or shown
    # before the user's first Generate.
    output$map <- leaflet$renderLeaflet({
      o <- map_opts()
      base <- map_coords()
      coords <- filter_coords(base, o)
      # What the view is framed on: build_map() applies the Show filter itself.
      shown <- focus_rows(base, o$focus)

      # Read the current view first so Charts-mode clustering can bin at the
      # live zoom (the client reports it after the first interaction); on the
      # very first render z is NULL and build_charts falls back to per-place,
      # which the debounced zoom observer below then re-clusters once fitBounds
      # settles a zoom.
      has_data <- !is.null(shown) && nrow(shown) > 0
      z <- shiny$isolate(input$map_zoom)
      ctr <- shiny$isolate(input$map_center)
      # Read (not depend on) the fit flag: clearing it must not itself trigger a
      # re-render, or every user pan — which clears it — would rebuild the map.
      refit <- shiny$isolate(pending_fit())

      m <- build_map(
        coords,
        o,
        full_coords = base,
        zoom = z,
        regions = map_regions(),
        shapes = map_shapes()
      )

      # Fit every mapped isolate after a Generate, and keep doing so on each
      # re-render until the user moves the map; otherwise keep their view
      # (zoom/center inputs are populated by Leaflet). Framed on all isolates
      # rather than the date window, so an animation does not jump around.
      if (has_data && (refit || is.null(z) || is.null(ctr))) {
        m <- frame_coords(m, shown)
      } else if (!is.null(z) && !is.null(ctr)) {
        m <- leaflet$setView(m, lng = ctr$lng, lat = ctr$lat, zoom = z)
      } else {
        m <- leaflet$setView(m, lng = 10, lat = 50, zoom = 4)
      }
      # Reports the user's own pan, zoom or click on the map (not programmatic
      # moves), which ends the refitting above. Hooked once per output element.
      onRender(
        m,
        "function(el) {
           if (el._userViewHooked) return;
           el._userViewHooked = true;
           var id = el.id + '_user_view';
           ['mousedown', 'wheel', 'touchstart', 'keydown', 'dblclick'].forEach(function(ev) {
             el.addEventListener(ev, function() {
               if (window.Shiny) Shiny.setInputValue(id, Date.now());
             }, {passive: true});
           });
         }"
      )
    })

    # Stop refitting once the user moves the map themselves. Clearing on the
    # first reported center instead raced the several re-renders one Generate
    # triggers: a pass could read a view reported before the fit landed and put
    # the map back on the previous data's frame.
    shiny$observeEvent(input$map_user_view, pending_fit(FALSE), ignoreInit = TRUE)

    # A new Show selection frames its isolates, as a Generate does.
    shiny$observeEvent(
      input$map_focus,
      pending_fit(TRUE),
      ignoreInit = TRUE,
      ignoreNULL = FALSE
    )

    # Charts-mode clustering: re-bin and redraw the minichart layer as the map
    # is zoomed, so overlapping charts merge and split like marker clusters. A
    # single scroll-zoom emits several zoom events and each redraw rebuilds the
    # D3 charts from scratch (clearMinicharts + addMinicharts), so the zoom is
    # debounced to coalesce them and avoid flicker. renderLeaflet already draws
    # the correct clustering for the current zoom on any *control* change (it
    # reads the live zoom), so this observer only needs to handle the zoom
    # itself changing. leafletProxy edits the existing map in place — no full
    # re-render — so it stays cheap even at this app's data volumes.
    map_zoom_d <- shiny$debounce(shiny$reactive(input$map_zoom), 250)
    shiny$observeEvent(
      map_zoom_d(),
      {
        o <- map_opts()
        z <- map_zoom_d()
        if (!identical(o$mode, "Charts") || !isTRUE(o$chart_cluster)) {
          return()
        }
        if (is.null(z)) {
          return()
        }
        base <- focus_rows(map_coords(), o$focus)
        coords <- filter_coords(base, o)
        if (is.null(coords) || !nrow(coords)) {
          return()
        }
        proxy <- leaflet$leafletProxy("map")
        clearMinicharts(proxy)
        # Pass the full (unfiltered) coords so a re-cluster on zoom keeps the
        # same fixed full-range category set and palette the full render uses.
        # The legend lists those same categories at every zoom, so the one the
        # full render drew stays: drawn again here, each zoom stacked another
        # copy, without the sizing and opacity that render applied to it.
        o$legend <- FALSE
        build_charts(proxy, coords, o, full_coords = base, zoom = z)
      },
      ignoreInit = TRUE
    )

    # Leaflet initialises at zero size while its panel/container is hidden and
    # never recomputes when shown, leaving a grey box. Dispatching a window
    # resize makes Leaflet (trackResize = TRUE) call invalidateSize(); do it
    # whenever the Map engine becomes visible or the map is un-hidden.
    nudge_resize <- function(delay = 250) {
      runjs(sprintf(
        "setTimeout(function(){window.dispatchEvent(new Event('resize'));}, %d);",
        delay
      ))
    }
    shiny$observeEvent(visible(), {
      if (isTRUE(visible())) {
        nudge_resize()
      }
    })

    # Auto-pick a sensible basemap when the mode changes.
    shiny$observeEvent(
      input$map_mode,
      {
        mode <- input$map_mode %||% "Markers"
        default_tile <- mode_tile_defaults[[mode]]
        if (!is.null(default_tile)) {
          updatePickerInput(session, "map_tiles", selected = default_tile)
        }
      },
      ignoreInit = TRUE
    )

    # --- Variable mapping layers --------------------------------------------

    # Anything except the isolate id is a candidate variable.
    mappable_fields <- shiny$reactive({
      meta <- viz_metadata()
      if (is.null(meta) || !length(names(meta))) {
        return(character())
      }
      setdiff(names(meta), "isolate")
    })

    # Every column's profile, for the variable pickers and the mapping engine.
    profiles <- shiny$reactive({
      meta <- viz_metadata()
      if (is.null(meta) || !length(names(meta))) {
        return(NULL)
      }
      field_profiles() %||%
        field_profiles_of(
          meta,
          mlst_cols = attr(meta, "mlst_cols"),
          amr_cols = attr(meta, "amr_cols"),
          custom_cols = attr(meta, "custom_cols")
        )
    })

    # Refill the variable, popup and hover pickers from the loaded database's
    # own columns. Until this runs they list only their placeholders.
    # `force_default` (Reset settings) puts popup and hover back on their
    # defaults; otherwise a selection still valid for these columns is kept, and
    # `popup` / `hover` (a restored plot) name one outright.
    populate_metadata_selects <- function(
      force_default = FALSE,
      popup = NULL,
      hover = NULL
    ) {
      prof <- profiles()
      fields <- mappable_fields()
      if (is.null(prof) || !nrow(prof) || !length(fields)) {
        return(invisible(NULL))
      }
      prof <- prof[prof$field %in% fields, , drop = FALSE]
      viz_helpers$update_field_select(session, "map_layer_add", prof)

      # The first fill always starts from the defaults: before it, the pickers
      # held only their sentinels, so the browser has already dropped the
      # default entries that are real columns (the collection date) from what
      # it reports.
      use_default <- force_default || !labels_filled
      labels_filled <<- TRUE

      # Popup and hover show a value rather than group by it, so a column
      # unique per isolate is a perfectly good choice and stays enabled.
      refill <- function(id, override, default) {
        offered <- c(unname(label_sentinels(id)), fields)
        wanted <- override %||%
          if (use_default) default else input[[id]] %||% default
        sel <- intersect(unlist(wanted), offered)
        if (!length(sel)) {
          sel <- intersect(default, offered)
        }
        viz_helpers$update_field_select(
          session,
          id,
          prof,
          selected = sel[seq_len(min(length(sel), MAX_LABEL_FIELDS))],
          extra = label_sentinels(id),
          disable_ungroupable = FALSE
        )
      }
      refill("map_popup", popup, POPUP_DEFAULT)
      refill("map_hover_field", hover, HOVER_DEFAULT)
    }

    # Fill the pickers as soon as a database is loaded rather than waiting for
    # a Generate, so a variable can be mapped before the first map is drawn.
    shiny$observeEvent(
      profiles(),
      populate_metadata_selects(),
      ignoreNULL = TRUE
    )

    next_layer_id <- layer_id_source(map_layer_seq)

    # Picking a variable adds it as the map's one mapping layer.
    shiny$observeEvent(input$map_layer_add, {
      field <- input$map_layer_add
      shiny$req(nzchar(field %||% ""))
      # Clear the picker straight away so the same variable can be re-picked
      # after a delete, and so the selection cannot re-fire on a later flush.
      updateVirtualSelect(
        inputId = "map_layer_add",
        session = session,
        selected = character(0)
      )

      if (!isTRUE(field %in% mappable_fields())) {
        return()
      }
      layers <- map_layers()
      if (layer_has_field(layers, field)) {
        return()
      }
      if (length(layers) >= max_layers(MEDIUM)) {
        shiny$showNotification(
          paste(
            "A map can show only one variable at a time. Remove the current",
            "mapping first."
          ),
          type = "warning"
        )
        return()
      }
      prof <- profile_for(profiles(), field)
      layer <- assign_mapping_layer(
        prof,
        layers,
        next_layer_id(),
        MEDIUM,
        viz_metadata()[[field]]
      )
      if (is.null(layer)) {
        shiny$showNotification(
          aesthetic_block_reason(prof, NULL, MEDIUM) %||%
            "That variable cannot be mapped.",
          type = "warning"
        )
        return()
      }
      map_layers(c(layers, list(layer)))
    })

    # One delegated handler per action rather than one observer per row: an
    # observeEvent created inside renderUI is re-registered on every render, so
    # the ids push their own value into a single input instead.
    shiny$observeEvent(input$map_layer_delete, {
      keep <- drop_layer(map_layers(), input$map_layer_delete)
      map_layers(rebalance_layers(keep, profiles(), MEDIUM, viz_metadata()))
    })

    # The mapping card under the variable picker.
    output$map_layers_ui <- shiny$renderUI({
      render_info("visualization_map map_layers_ui")
      layer_cards(
        ns,
        map_layers(),
        MEDIUM,
        "map_layer_edit",
        "map_layer_delete",
        empty_text = paste(
          "No mapping yet - markers keep their fill colour and charts need a",
          "variable to split by."
        )
      )
    })

    editing <- shiny$reactiveVal(NULL)

    # The edit dialog: the palette and, for a date, its grouping. There is no
    # "Show as" choice, because the medium has one channel.
    shiny$observeEvent(input$map_layer_edit, {
      l <- find_layer(map_layers(), input$map_layer_edit)
      shiny$req(!is.null(l))
      prof <- profile_for(profiles(), l$field)
      shiny$req(!is.null(prof))
      editing(l$id)

      values <- viz_metadata()[[l$field]]
      # The palette has to suit the variable as the chosen granularity leaves
      # it: binned to months it is a category, not a continuum.
      binned <- granularity_profile(prof, values, l$granularity)
      # Parsed, not raw: an ungrouped date reaches the scale as a continuum,
      # and out of SQLite it is a character column that no test for one can
      # recognise.
      shown <- if (is_date_profile(prof)) {
        bin_date_values(values, l$granularity)
      } else {
        values
      }
      cats <- scale_categories_for(
        if (isTRUE(binned$continuous)) shown else as.character(shown),
        viz_helpers$suitable_scale_categories(
          if (isTRUE(binned$continuous)) "Numeric" else "Factor",
          shown
        )
      )

      shiny$showModal(shiny$modalDialog(
        title = paste("Mapping:", l$title),
        size = "s",
        easyClose = TRUE,
        if (is_date_profile(prof)) {
          viz_helpers$granularity_select(
            ns,
            "map_layer_granularity",
            l$granularity,
            label = "Group dates by",
            values = values
          )
        },
        viz_helpers$scale_select(
          ns,
          "map_layer_palette",
          categories = cats,
          selected = l$palette
        ),
        footer = shiny$tagList(
          shiny$modalButton("Cancel"),
          shiny$actionButton(ns("map_layer_apply"), "Apply")
        )
      ))
    })

    # Apply the edit dialog to the layer it was opened for.
    shiny$observeEvent(input$map_layer_apply, {
      id <- editing()
      shiny$req(!is.null(id))
      layers <- lapply(map_layers(), function(l) {
        if (!identical(l$id, id)) {
          return(l)
        }
        l$palette <- input$map_layer_palette %||% l$palette
        l <- set_layer_granularity(
          l,
          input$map_layer_granularity,
          viz_metadata()[[l$field]]
        )
        # Pinned: rebalance_layers() rebuilds automatic layers from scratch and
        # would discard the palette just chosen.
        l$auto <- FALSE
        l
      })
      map_layers(rebalance_layers(layers, profiles(), MEDIUM, viz_metadata()))
      editing(NULL)
      shiny$removeModal()
    })

    # The mapping the markers and charts are drawn with, or NULL for none.
    mapped_layer <- function() {
      layers <- map_layers()
      if (length(layers)) layers[[1]] else NULL
    }

    # The legend's number digits only format a continuous, non-date scale on
    # the markers, so it is hidden rather than left as a dead control. The
    # Fill color row stays visible even once a mapping colours the markers by
    # its hue instead: its opacity slider is still live then (see
    # build_markers()), which the flat "Fill opacity" slider it replaced was.
    shiny$observe({
      layer <- mapped_layer()
      toggle(
        id = "wrap_map_legend_digits",
        condition = identical(input$map_mode, "Markers") &&
          isTRUE(layer$continuous) &&
          !identical(layer$transform, "as_date")
      )
    })

    # Date variables the Time tab can filter by: standard metadata first with
    # the collection date on top, then custom Date and Year variables.
    date_choices <- shiny$reactive(timeline_fields(profiles()))

    # The Time tab's date variable: the picked one while this data still has
    # it, otherwise the first holding any readable date.
    date_field <- shiny$reactive({
      fields <- date_choices()
      picked <- input$map_date_field
      if (isTRUE(picked %in% fields)) {
        return(picked)
      }
      first_dated_field(viz_metadata(), fields) %||%
        (if (length(fields)) fields[[1]])
    })

    # The date variable the slider is currently fitted to; NULL while it holds
    # no readable date, which switches the time filter off (filter_coords()).
    time_field <- shiny$reactiveVal(NULL)

    # Bumped to rebuild the date-variable picker; the value to rebuild with,
    # when not the current one, is parked in restore_date_field (consumed on
    # read). Rendered rather than updated, like the epi curve's own picker.
    date_field_rebuild <- shiny$reactiveVal(0L)
    restore_date_field <- shiny$reactiveVal(NULL)

    # The Time tab's date-variable picker.
    output$date_field_ui <- shiny$renderUI({
      render_info("visualization_map date_field_ui")
      date_field_rebuild()
      fields <- date_choices()
      pending <- shiny$isolate(restore_date_field())
      if (!is.null(pending)) {
        restore_date_field(NULL)
      }
      if (!length(fields)) {
        return(shiny$div(
          class = "timeline-field timeline-field-empty",
          "No date variable in this database, so the map cannot be filtered",
          "by time. Fill in collection dates under Database › Browse, or",
          "create a Date or Year variable under Custom Variables."
        ))
      }
      prof <- profiles()
      shiny$div(
        class = "timeline-field",
        viz_helpers$field_select(
          ns,
          "map_date_field",
          "Date variable",
          profiles = prof[match(fields, prof$field), , drop = FALSE],
          selected = if (isTRUE(pending %in% fields)) {
            pending
          } else {
            shiny$isolate(date_field())
          }
        )
      )
    })

    # Fit the date slider and the interval buttons to the chosen date variable:
    # the slider spans its dates (a bare year to 31 December), and the
    # intervals stop at its precision.
    fit_time_controls <- function(coords) {
      field <- shiny$isolate(date_field())
      values <- if (length(field) && field %in% names(coords)) coords[[field]]
      span <- date_extent(values)
      if (is.null(span)) {
        time_field(NULL)
        return(invisible(NULL))
      }
      time_field(field)
      daterange_bounds(span)
      shiny$updateSliderInput(
        session,
        "map_daterange",
        min = span[[1]],
        max = span[[2]],
        value = span
      )
      # "Hour" only makes sense when the values carry a time of day —
      # filter_coords() works in whole days, so an Hour animation over
      # date-only data would silently collapse back to Day. as.POSIXct's
      # format guessing throws (not just warns) when no one format fits every
      # value, so that reads as "no time component" rather than a crash.
      ts <- tryCatch(
        suppressWarnings(as.POSIXct(values, tz = "UTC")),
        error = function(e) as.POSIXct(character(0), tz = "UTC")
      )
      has_time <- any(!is.na(ts) & format(ts, "%H:%M:%S") != "00:00:00")
      interval_choices <- if (identical(date_precision(values), "year")) {
        "Year"
      } else if (has_time) {
        c("Hour", "Day", "Week", "Month", "Year")
      } else {
        c("Day", "Week", "Month", "Year")
      }
      current <- shiny$isolate(input$map_interval)
      updateRadioGroupButtons(
        session,
        "map_interval",
        choices = interval_choices,
        selected = if (isTRUE(current %in% interval_choices)) {
          current
        } else {
          interval_choices[[1]]
        }
      )
    }

    # A different date variable re-fits the slider to its span. Only on an
    # actual change: date_field() re-evaluates with every metadata refresh.
    shown_date_field <- NULL
    shiny$observeEvent(date_field(), {
      f <- date_field()
      changed <- !is.null(shown_date_field) && !identical(f, shown_date_field)
      shown_date_field <<- f
      coords <- map_coords()
      if (changed && !is.null(coords) && nrow(coords)) {
        anim_playing(FALSE)
        fit_time_controls(coords)
      }
    })

    # Refit the controls solved from the generated coordinates rather than
    # declared: the date range spans the data, and the heatmap's max intensity
    # is the busiest place's isolate count, so the slider spans a range where
    # moving it is visible instead of saturating everywhere.
    fit_data_controls <- function(coords) {
      if (is.null(coords) || !nrow(coords)) {
        return(invisible(NULL))
      }
      fit_time_controls(coords)
      max_per_place <- max(table(coords$place))
      shiny$updateSliderInput(
        session,
        "map_heat_max",
        max = max(max_per_place, 10),
        value = max_per_place
      )
    }

    # Countries the Show filter offers since the last Generate (NULL before
    # any; filled then rather than at startup, so the country table loads with
    # the map and the counts can be shown), and a restored selection waiting
    # for them: a plot restored before its first Generate has nothing to
    # select from yet. Plain state:
    # nothing should re-run because either changed.
    focus_offered <- NULL
    focus_pending <- NULL

    # Offer every country with this Generate's isolate counts, keeping the
    # current (or restored) selection.
    fill_focus_choices <- function(built) {
      choices <- focus_choices(focus_counts(built$coords, built$regions))
      focus_offered <<- unlist(lapply(choices$choices, function(g) g$options$value))
      wanted <- focus_pending %||% shiny$isolate(input$map_focus)
      focus_pending <<- NULL
      updateVirtualSelect(
        inputId = "map_focus",
        session = session,
        choices = choices,
        selected = as.character(intersect(unlist(wanted), focus_offered))
      )
    }

    # Geocode + populate the metadata-backed selects and the date slider, only
    # when Map is the active engine and Generate is clicked (mirrors the MST/Tree
    # guard). The heavy geocoding is covered by the waiter spinner.

    shiny$observeEvent(generate(), {
      if (!identical(plot_type(), "Map")) {
        return()
      }
      meta <- viz_metadata()
      shiny$req(meta)

      populate_metadata_selects(force_default = FALSE)

      # Label the spinner with how many distinct locations are about to be
      # fetched from Nominatim — the UNCACHED ones (see geocode_pending_count()),
      # which is the actual work and what the live remaining-time countdown is
      # seeded from. Computed here, before the blocking geocode call. When every
      # location is already cached the count is 0 and the waiter drops the
      # countdown (see geocode_waiter_html()).
      waiter$show()
      on.exit(waiter$hide(), add = TRUE)
      waiter$update(html = geocode_waiter_html(geocode_pending_count(meta, db_path())))

      # Re-sent before every lookup: the count done and an estimate from the
      # pace measured so far, so a slow geocoder lengthens it as it goes.
      progress <- function(label, done, total, elapsed) {
        waiter$update(html = geocode_waiter_html(total, done, elapsed, label))
      }
      built <- tryCatch(
        build_map_coords(meta, db_path(), progress),
        error = function(e) {
          shiny$showNotification(
            paste("Could not geocode isolate locations:", conditionMessage(e)),
            type = "error"
          )
          list(coords = NULL, regions = NULL, shapes = NULL, status = empty_geocode_status())
        }
      )
      coords <- built$coords
      map_geocode_status(built$status)

      if (is.null(coords) || !nrow(coords)) {
        # Recognised countries still draw a choropleth without any point.
        choropleth_note <- if (!is.null(built$regions)) {
          " Countries can still be shown in Choropleth mode."
        } else {
          ""
        }
        shiny$showNotification(
          paste0(
            if (isTRUE(built$status$offline)) {
              "Nominatim could not be reached, so no isolate place could be geocoded."
            } else {
              paste(
                "No mappable locations found. Fill in coordinates, or a city,",
                "postcode, state/province or country."
              )
            },
            choropleth_note
          ),
          type = "warning",
          duration = 8
        )
      } else {
        fit_data_controls(coords)
      }
      map_coords(coords)
      map_regions(built$regions)
      map_shapes(built$shapes)
      fill_focus_choices(built)
      generated(TRUE)
      # Frame the view on this Generate's data, refitting even if the previous
      # map was left zoomed in on an earlier (e.g. single-isolate) selection.
      pending_fit(TRUE)
      # The setup sidebar collapses on Generate (parent), changing the map's
      # width — recompute the Leaflet size once the markers are drawn.
      nudge_resize(350)
    })

    # Hide the prompt overlay (and reveal the map) once a map has been
    # generated; mirrors the MST/Tree engines' prompt handling, extended to
    # also gate the map's own visibility (see the renderLeaflet comment above).
    shiny$observeEvent(
      generated(),
      {
        toggle(id = "viz_prompt", condition = !isTRUE(generated()))
        toggle(id = "map_wrap", condition = isTRUE(generated()))
      },
      ignoreNULL = FALSE
    )

    # Reset-view button: refit to the (date-filtered) data with a fly animation.
    shiny$observeEvent(input$map_reset, {
      o <- map_opts()
      coords <- filter_coords(focus_rows(map_coords(), o$focus), o)
      shiny$req(!is.null(coords) && nrow(coords) > 0)
      frame_coords(leaflet$leafletProxy("map"), coords, fly = TRUE)
    })

    # ---- Export contract ----------------------------------------------------
    # A browser-drawn engine: the tab's sidebar asks for a raster and the client
    # answers through `capture_data`. HTML is the format the server writes
    # itself, rebuilding the current map with the shared builder and serialising
    # it as a self-contained interactive file.
    export <- list(
      kind = "widget",
      label = "map",
      ready = shiny$reactive(isTRUE(generated())),
      save = function(file, format, opts) {
        o <- map_opts()
        base <- map_coords()
        coords <- filter_coords(base, o)
        # Bin Charts-mode clustering at the current on-screen zoom so the export
        # is a faithful snapshot; the exported HTML is static (no server), so it
        # keeps that binning rather than re-clustering as its viewer zooms.
        m <- build_map(
          coords,
          o,
          full_coords = base,
          zoom = shiny$isolate(input$map_zoom),
          regions = map_regions(),
          shapes = map_shapes()
        )
        shown <- focus_rows(coords, o$focus)
        if (!is.null(shown) && nrow(shown) > 0) {
          m <- frame_coords(m, shown)
        }
        saveWidget(m, file, selfcontained = TRUE)
      },
      # html2canvas re-rasterises the DOM at a multiple, so the overlays that
      # carry the data — markers, labels, legend, choropleth polygons — are
      # redrawn crisply. The basemap tiles are fixed-resolution images and can
      # only be upscaled, which is why HTML is offered alongside. `targetPx`
      # rather than a raw scale is what makes the result reproducible: the
      # handler measures the map's actual on-screen width itself (the Map has
      # no fixed aspect and fills whatever box it is given, so this matters
      # more here than anywhere else) and works out the multiplier from that.
      # See effectiveScale() in app/view/visualization.R.
      capture = function(format, opts) {
        session$sendCustomMessage(
          "phylotrace_capture",
          list(
            selector = paste0("#", ns("map")),
            mode = "html2canvas",
            inputId = session$ns("export_capture"),
            targetPx = opts$target_px,
            format = format,
            background = "#ffffff"
          )
        )
      },
      capture_data = shiny$reactive(input$export_capture)
    )

    # Reset settings: restore every control in this engine's own sidebar to
    # its coded default, WITHOUT disturbing the already-geocoded plot (no
    # re-Generate needed), and drop the mapping, which is reactiveVal state
    # rather than a control.
    #
    # The catalogue (MAP_CONTROLS / map_control_defaults()) is what makes that
    # complete: this used to be reset(), which silently skips every
    # pickerInput, virtualSelectInput, colour swatch and radioGroupButtons in
    # this panel (see the reference at the top of viz_helpers.R). The
    # data-fitted controls are sent after it, unconditionally, so the fit is
    # the message that lands.
    reset_map_settings <- function() {
      anim_playing(FALSE)
      anim_idx(0L)
      # The seq goes back to zero with the layers, which is safe only because
      # no card survives to address an id that will be handed out again.
      map_layers(list())
      map_layer_seq(0L)

      viz_helpers$apply_controls(session, map_control_defaults(), MAP_CONTROLS)
      populate_metadata_selects(force_default = TRUE)
      # Back to the date variable the data opens on.
      restore_date_field(first_dated_field(
        shiny$isolate(viz_metadata()),
        shiny$isolate(date_choices())
      ))
      date_field_rebuild(shiny$isolate(date_field_rebuild()) + 1L)
      fit_data_controls(map_coords())
    }

    viz_helpers$on_confirmed_reset(
      input,
      session,
      reset_map_settings,
      "The mapped variable is removed with the rest."
    )

    # Keep the wrapper output reactive while hidden: the Map engine's panel is
    # display:none-hidden by navset_hidden whenever another engine is active,
    # and by default Shiny would suspend an output bound to a hidden element —
    # this ensures the prompt/map wrapper still renders on the very first
    # switch to the Map tab. output$map itself is deliberately left to the
    # default suspend-when-hidden behavior (see its own comment above).
    shiny$outputOptions(output, "plot_area", suspendWhenHidden = FALSE)

    # ---- Dashboard "Save Analysis" contract ---------------------------------
    # Snapshot the map_* controls plus the mapping layers, which are
    # reactiveVal state rather than inputs.
    snapshot <- shiny$reactive(c(
      viz_helpers$collect_input_snapshot(input, "map_"),
      list(.layers = map_layers())
    ))

    # Rebuild a mapping layer from a snapshot saved before the map had layers.
    # Markers kept their variable in map_color_var / map_col_var (grouping in
    # map_col_granularity, palette in map_col_scale), Charts in map_chart_var /
    # map_chart_granularity / map_chart_scale; the saved mode says which one
    # the plot was showing.
    migrate_legacy_mapping <- function(vals) {
      charts <- identical(vals$map_mode, "Charts")
      field <- if (charts) {
        vals$map_chart_var
      } else if (isTRUE(vals$map_color_var)) {
        vals$map_col_var
      }
      if (is.null(field) || !isTRUE(field %in% mappable_fields())) {
        return(NULL)
      }
      prof <- profile_for(profiles(), field)
      if (is.null(prof)) {
        return(NULL)
      }
      layer <- assign_mapping_layer(
        prof,
        list(),
        "L1",
        MEDIUM,
        viz_metadata()[[field]],
        granularity = if (charts) {
          vals$map_chart_granularity
        } else {
          vals$map_col_granularity
        }
      )
      if (is.null(layer)) {
        return(NULL)
      }
      palette <- if (charts) vals$map_chart_scale else vals$map_col_scale
      if (!is.null(palette)) {
        layer$palette <- palette
      }
      # Pinned: the saved palette and grouping are the user's choices.
      layer$auto <- FALSE
      list(layer)
    }

    restore <- function(vals) {
      if (is.null(vals)) {
        return(invisible(NULL))
      }
      # A basemap no longer offered (the CARTO ones) keeps the current one.
      if (
        !is.null(vals$map_tiles) && !isTRUE(vals$map_tiles %in% map_providers)
      ) {
        vals$map_tiles <- NULL
      }
      # Choropleth's palette used to be the shared map_col_scale, before it
      # moved to the Region tab (a saved map_region_reverse, from back when the
      # palette could be flipped, is simply dropped -- it isn't any more).
      if (identical(vals$map_mode, "Choropleth")) {
        vals$map_region_scale <- vals$map_region_scale %||% vals$map_col_scale
      }
      # A plot saved before the state level existed counted by country.
      vals$map_region_level <- vals$map_region_level %||% "country"
      # Saved before the Show filter, or with nothing ticked: the whole world.
      vals$map_focus <- unlist(vals$map_focus) %||% character(0)
      if (is.null(focus_offered)) {
        focus_pending <<- vals$map_focus
      }

      # Same catalogue a reset applies, holding saved values instead of the
      # coded ones.
      viz_helpers$apply_controls(session, vals, MAP_CONTROLS)
      # The popup and hover selections only stick once their choices exist, so
      # they are sent again together with the database's columns.
      populate_metadata_selects(
        popup = vals$map_popup,
        hover = vals$map_hover_field
      )

      # The mapping is written straight into its reactiveVal. A layer on a
      # column this database no longer has is dropped rather than drawn.
      layers <- normalize_layers(vals$.layers, LAYER_DEFAULTS, MEDIUM)
      if (!is.null(layers)) {
        fields <- mappable_fields()
        layers <- Filter(function(l) isTRUE(l$field %in% fields), layers)
      }
      if (!length(layers)) {
        layers <- migrate_legacy_mapping(vals)
      }
      if (!is.null(layers)) {
        map_layers(layers)
        map_layer_seq(length(layers))
      }

      # The Time tab's date variable: its picker is renderUI output.
      if (is.character(vals$map_date_field) && nzchar(vals$map_date_field)) {
        restore_date_field(vals$map_date_field)
        date_field_rebuild(shiny$isolate(date_field_rebuild()) + 1L)
      }

      # Date-range slider: restore as Dates.
      if (!is.null(vals$map_daterange)) {
        dr <- tryCatch(
          as.Date(unlist(vals$map_daterange)),
          error = function(e) NULL
        )
        if (!is.null(dr) && length(dr) == 2 && !any(is.na(dr))) {
          shiny$updateSliderInput(session, "map_daterange", value = dr)
        }
      }
    }

    # Thumbnail: capture the Leaflet container in the browser (html2canvas),
    # returned via input$thumb_data.
    request_thumb <- function() {
      session$sendCustomMessage(
        "phylotrace_capture",
        list(
          selector = paste0("#", ns("map")),
          mode = "html2canvas",
          inputId = session$ns("thumb_data")
        )
      )
    }

    list(
      snapshot = snapshot,
      restore = restore,
      save_thumb = NULL,
      request_thumb = request_thumb,
      thumb_data = shiny$reactive(input$thumb_data),
      export = export
    )
  })
}
