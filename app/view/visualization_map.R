# app/view/visualization_map.R
#
# Geographic (leaflet) visualization submodule. Owns its own right-hand control
# sidebar, the map output and all map-specific reactive state. Mounted by
# app/view/visualization.R inside a navset_hidden panel; the shared Generate
# button, plot type, session reset and per-isolate metadata are forwarded in as
# reactives — the same contract the MST and Tree engines use. Isolate
# coordinates are derived from the metadata's spatial fields
# (geo_loc_name_city + geo_loc_name_state_province + geo_loc_name_country),
# geocoded once per distinct place via OSM/Nominatim on Generate.
#
# Four map modes share one builder (`build_map`): Markers (styled points),
# Choropleth (countries shaded by isolate count, from Natural Earth polygons),
# Heatmap (point density) and Charts (a minichart per location). Rendering is a
# single renderLeaflet reacting to the geocoded coords and a debounced bundle of
# all control inputs (`map_opts`); the same builder backs the HTML export. The
# map itself stays hidden (behind the "press Generate" prompt) until the first
# Generate, so the base tiles and the coordinates always appear together.

box::use(
  shiny,
  bslib[
    as_fill_carrier,
    input_switch,
    layout_sidebar,
    nav_panel,
    navset_tab,
    sidebar,
    accordion,
    accordion_panel,
    card,
    card_body,
    tooltip,
  ],
  shinyWidgets[
    radioGroupButtons,
    updateRadioGroupButtons,
    pickerInput,
    updatePickerInput,
    updateVirtualSelect,
  ],
  leaflet[
    leaflet,
    leafletOptions,
    leafletOutput,
    renderLeaflet,
    leafletProxy,
    addTiles,
    addProviderTiles,
    tileOptions,
    addMapPane,
    addCircleMarkers,
    addPolygons,
    highlightOptions,
    addLegend,
    addScaleBar,
    addMiniMap,
    addControl,
    addSimpleGraticule,
    markerClusterOptions,
    labelOptions,
    labelFormat,
    pathOptions,
    colorFactor,
    colorNumeric,
    fitBounds,
    flyToBounds,
    flyTo,
    setView,
  ],
  htmlwidgets[onRender],
  leaflet.extras[addFullscreenControl, addHeatmap],
  leaflet.minicharts[addMinicharts, clearMinicharts],
  rlang[`%||%`],
  tidygeocoder[geocode],
  shinyjs[runjs],
  utils[read.csv, write.csv],
)
box::use(
  app /
    logic /
    viz_helpers[
      apply_controls,
      collect_input_snapshot,
      control_families,
      field_select,
      granularity_select,
      on_confirmed_reset,
      resolve_gradient_palette,
      resolve_qualitative_palette,
      scale_select,
      suitable_scale_categories,
      update_field_select,
      viz_color,
    ],
  app / logic / date_bins[bin_date_values, is_binned],
  app / logic / db_events,
  app / logic / functions[render_info],
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
  app / logic / paths[app_local_share_path],
  app / logic / field_labels[field_label],
  app /
    logic /
    field_profile[
      field_profiles_of = field_profiles,
      profile_for,
      scale_categories_for,
    ],
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
MAP_CONTROLS <- control_families(
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
  virtual_selects = c("map_popup", "map_hover_field"),
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
  radio_groups = c("map_interval", "map_region_transform"),
  colors = c("map_marker_color", "map_region_border")
)

# Every catalogued control's coded default. The widgets below are declared with
# these same values.
map_control_defaults <- function() {
  list(
    map_mode = "Markers",
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

# Natural Earth country polygons are fetched once and cached for the choropleth.
.world_env <- new.env(parent = emptyenv())
get_world <- function() {
  if (is.null(.world_env$sf)) {
    .world_env$sf <- rnaturalearth::ne_countries(
      scale = 50,
      returnclass = "sf"
    )
  }
  .world_env$sf
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
    failed_preview = ""
  )
}

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

# Join city/state/country (most specific first) into one place string per
# isolate row, so Nominatim resolves each to the finest available point. Shared
# by build_map_coords() (the geocoder itself) and geocode_pending_count() (the
# pre-flight count shown in the waiter), so the two can never disagree about
# exactly which strings get geocoded.
build_place_strings <- function(meta) {
  col <- function(name) {
    if (name %in% names(meta)) meta[[name]] else rep(NA_character_, nrow(meta))
  }
  city <- col("geo_loc_name_city")
  state <- col("geo_loc_name_state_province")
  country <- col("geo_loc_name_country")
  vapply(
    seq_len(nrow(meta)),
    function(i) {
      parts <- trimws(c(city[i], state[i], country[i]))
      parts <- parts[!is.na(parts) & nzchar(parts)]
      paste(parts, collapse = ", ")
    },
    character(1)
  )
}

# Parse the free-text `geo_loc_coordinates` field into numeric lat/long. Accepts
# "latitude, longitude" in decimal degrees (comma-, semicolon- or
# whitespace-separated); anything that isn't exactly two in-range numbers
# (|lat| <= 90, |lon| <= 180) yields NA, so a blank or malformed entry simply
# falls back to geocoding the place string. Vectorised: returns a data.frame
# with `latitude` and `longitude`, one row per input.
parse_coordinates <- function(x) {
  n <- length(x)
  out <- data.frame(
    latitude = rep(NA_real_, n),
    longitude = rep(NA_real_, n),
    stringsAsFactors = FALSE
  )
  if (!n) {
    return(out)
  }
  raw <- trimws(as.character(x))
  for (i in seq_len(n)) {
    s <- raw[i]
    if (is.na(s) || !nzchar(s)) {
      next
    }
    parts <- suppressWarnings(as.numeric(trimws(strsplit(s, "[,;[:space:]]+")[[
      1
    ]])))
    parts <- parts[!is.na(parts)]
    if (length(parts) != 2L) {
      next
    }
    if (abs(parts[1]) <= 90 && abs(parts[2]) <= 180) {
      out$latitude[i] <- parts[1]
      out$longitude[i] <- parts[2]
    }
  }
  out
}

# --- persistent geocode cache -----------------------------------------------

# Cross-session cache of resolved place → coordinates, stored as a CSV alongside
# the app's other local state (app_local_share_path, next to state.json /
# event_list.rds). A place string's coordinates never change, so entries never
# expire. Only SUCCESSFUL lookups are stored — a place that failed to resolve
# (often a transient network issue) is retried on the next Generate rather than
# remembered as unmappable. Reusing resolved coordinates like this is also what
# the OSM/Nominatim usage policy asks for (cache results; don't re-query them).
geocode_cache_path <- function() {
  file.path(app_local_share_path, "geocode_cache.csv")
}

# Load the cache as a data.frame (place, latitude, longitude), or an empty frame
# with those columns when there is no cache file yet. Defensive against a
# missing/older/corrupt file: anything unreadable or lacking the expected
# columns, and any row without usable coordinates, is treated as "not cached".
read_geocode_cache <- function(path = geocode_cache_path()) {
  empty <- data.frame(
    place = character(0),
    latitude = numeric(0),
    longitude = numeric(0),
    stringsAsFactors = FALSE
  )
  if (!file.exists(path)) {
    return(empty)
  }
  cached <- tryCatch(
    read.csv(
      path,
      stringsAsFactors = FALSE,
      colClasses = c(place = "character")
    ),
    error = function(e) empty
  )
  if (!all(c("place", "latitude", "longitude") %in% names(cached))) {
    return(empty)
  }
  cached <- cached[, c("place", "latitude", "longitude"), drop = FALSE]
  cached$latitude <- suppressWarnings(as.numeric(cached$latitude))
  cached$longitude <- suppressWarnings(as.numeric(cached$longitude))
  cached[
    !is.na(cached$place) &
      nzchar(cached$place) &
      !is.na(cached$latitude) &
      !is.na(cached$longitude),
    ,
    drop = FALSE
  ]
}

# Persist the cache, overwriting the file. Best-effort: a write failure (e.g. a
# read-only home dir) must never break map generation — the coordinates for
# this session are already in hand, only the cross-session speedup is lost — so
# it is wrapped and silently ignored.
write_geocode_cache <- function(df, path = geocode_cache_path()) {
  df <- df[!duplicated(df$place), , drop = FALSE]
  tryCatch(
    write.csv(df, path, row.names = FALSE),
    error = function(e) NULL
  )
}

# Resolve a set of distinct place strings to coordinates, cache-first: places
# already in the persistent cache are served from it, and only the rest are sent
# to Nominatim. Newly resolved places are appended to the cache. Returns a list
# with `located` (one row per distinct place — place, latitude, longitude, with
# NA coords where a lookup failed), `n_cached` (distinct places served from the
# cache) and `n_new` (distinct places freshly geocoded this call), for the
# geocode-status feedback.
geocode_places_cached <- function(
  place_vec,
  cache_path = geocode_cache_path()
) {
  places <- unique(place_vec[!is.na(place_vec) & nzchar(place_vec)])
  located <- data.frame(
    place = places,
    latitude = NA_real_,
    longitude = NA_real_,
    stringsAsFactors = FALSE
  )
  if (!length(places)) {
    return(list(located = located, n_cached = 0L, n_new = 0L))
  }

  cache <- read_geocode_cache(cache_path)
  hit <- match(located$place, cache$place)
  located$latitude <- cache$latitude[hit]
  located$longitude <- cache$longitude[hit]
  n_cached <- sum(!is.na(hit))

  todo <- located$place[is.na(hit)]
  n_new <- 0L
  if (length(todo)) {
    fresh <- geocode(
      data.frame(place = todo, stringsAsFactors = FALSE),
      place,
      method = "osm",
      lat = "latitude",
      long = "longitude"
    )
    fill <- match(located$place, fresh$place)
    got <- !is.na(fill)
    located$latitude[got] <- fresh$latitude[fill[got]]
    located$longitude[got] <- fresh$longitude[fill[got]]

    # Persist only the newly resolved (successful) lookups.
    ok <- !is.na(fresh$latitude) & !is.na(fresh$longitude)
    n_new <- sum(ok)
    if (n_new > 0) {
      write_geocode_cache(
        rbind(
          cache,
          fresh[ok, c("place", "latitude", "longitude"), drop = FALSE]
        ),
        cache_path
      )
    }
  }
  list(located = located, n_cached = n_cached, n_new = n_new)
}

# How many DISTINCT, non-empty places from this metadata are NOT already in the
# cache — i.e. the ones that will actually be sent to Nominatim on the next
# Generate. This is what drives the wait, so it (not the full place count) is
# what the waiter countdown is seeded from. Computed up front, cheaply, before
# the blocking call.
geocode_pending_count <- function(meta, cache_path = geocode_cache_path()) {
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
  cache <- read_geocode_cache(cache_path)
  length(setdiff(places, cache$place))
}

# Format a whole-second duration as M:SS, for the geocoding countdown. The
# total estimate is the distinct-place count: the public OSM/Nominatim endpoint
# is rate-limited to ~1 lookup per second (tidygeocoder waits out that min_time
# between requests), so wall-clock time tracks the count almost 1:1.
format_mmss <- function(secs) {
  secs <- max(as.integer(secs), 0L)
  sprintf("%d:%02d", secs %/% 60L, secs %% 60L)
}

# Waiter contents shown while geocoding: the flower spinner, how many locations
# are being resolved, and a live "Remaining time M:SS / M:SS" countdown. The
# geocode call blocks the R session, so the tick can't come from the server —
# the browser counts the estimate down on its own; the `.viz-countdown` element
# (carrying the total, in seconds, in data-total) is driven by the observer
# script installed once in ui(). `n_places` is the number of UNCACHED places
# actually being fetched (see geocode_pending_count()); when it's zero — every
# location already cached — there's nothing to wait on, so the countdown is
# dropped and the message just reflects the quick cache read + map build.
geocode_waiter_html <- function(n_places) {
  total <- max(as.integer(n_places), 0L)
  message <- if (total == 0L) {
    "Preparing map…"
  } else {
    sprintf(
      "Geocoding %d location%s…",
      total,
      if (total == 1L) "" else "s"
    )
  }
  countdown <- if (total > 0L) {
    shiny$div(
      class = "viz-waiter-estimate viz-countdown",
      `data-total` = total,
      sprintf(
        "Remaining time %s / %s",
        format_mmss(total),
        format_mmss(total)
      )
    )
  }
  shiny$tagList(
    shiny$div(
      class = "viz-spinner-dark",
      waiter::spin_flower(),
      shiny$div(style = "margin-top:1rem;", message),
      countdown
    )
  )
}

# Build a geocoded coordinate table from the isolate metadata. City, state and
# country are joined into one place string (most specific first) so Nominatim
# resolves to the finest available point; the distinct places are resolved once
# (cache-first — see geocode_places_cached()) and merged back onto every
# isolate. All metadata columns are retained. Rows with no spatial fields, or
# that fail to geocode, are dropped. Returns a list with `coords` (the metadata
# columns plus place, longitude and latitude, ordered by collection date, or
# NULL when nothing is mappable) and `status` (geocoding success + cache counts,
# for the sidebar feedback — see empty_geocode_status()'s fields).
build_map_coords <- function(meta) {
  if (is.null(meta) || !nrow(meta)) {
    return(list(coords = NULL, status = empty_geocode_status()))
  }

  df <- meta
  df$place <- build_place_strings(meta)

  # Explicit per-isolate coordinates (geo_loc_coordinates) win over geocoding:
  # they are already point-precise, so those rows are used as-is and never sent
  # to Nominatim.
  explicit <- parse_coordinates(
    if ("geo_loc_coordinates" %in% names(df)) {
      df$geo_loc_coordinates
    } else {
      rep(NA_character_, nrow(df))
    }
  )
  df$latitude <- explicit$latitude
  df$longitude <- explicit$longitude

  # Mappable = has explicit coordinates, or has a place string to geocode.
  has_explicit <- !is.na(df$latitude) & !is.na(df$longitude)
  df <- df[has_explicit | nzchar(df$place), , drop = FALSE]
  if (!nrow(df)) {
    return(list(coords = NULL, status = empty_geocode_status()))
  }
  has_explicit <- !is.na(df$latitude) & !is.na(df$longitude)

  # Geocode only the rows lacking explicit coordinates (and carrying a place).
  geo <- geocode_places_cached(df$place[!has_explicit & nzchar(df$place)])
  located <- geo$located
  failed_places <- located$place[
    is.na(located$latitude) | is.na(located$longitude)
  ]

  if (nrow(located)) {
    hit <- match(df$place, located$place)
    fill <- !has_explicit & !is.na(hit)
    df$latitude[fill] <- located$latitude[hit[fill]]
    df$longitude[fill] <- located$longitude[hit[fill]]
  }

  out <- df
  n_isolates <- nrow(out)
  out <- out[!is.na(out$longitude) & !is.na(out$latitude), , drop = FALSE]
  status <- list(
    n_isolates = n_isolates,
    n_mapped = nrow(out),
    # Distinct places actually resolved to coordinates (each backs one or more
    # mapped isolates), split into those served from the persistent cache vs.
    # freshly fetched from Nominatim this Generate.
    n_locations = nrow(located) - length(failed_places),
    n_cached = geo$n_cached,
    n_new = geo$n_new,
    n_failed_places = length(failed_places),
    failed_preview = format_failed_preview(failed_places)
  )
  if (!nrow(out)) {
    return(list(coords = NULL, status = status))
  }
  if ("sample_collection_date" %in% names(out)) {
    out <- out[order(out$sample_collection_date), , drop = FALSE]
  }
  list(coords = out, status = status)
}

# Subset the coordinates to the selected collection-date range. A no-op once
# the range covers the full data span (the default after Generate), so there
# is no separate on/off switch — narrowing the slider is the filter.
filter_coords <- function(coords, o) {
  if (is.null(coords) || !nrow(coords)) {
    return(coords)
  }
  if (is.null(o$daterange) || !"sample_collection_date" %in% names(coords)) {
    return(coords)
  }
  d <- suppressWarnings(as.Date(coords$sample_collection_date))
  rng <- as.Date(o$daterange)
  keep <- !is.na(d) & d >= rng[1] & d <= rng[2]
  coords[keep, , drop = FALSE]
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
# Parsed one value at a time: as.Date() on a vector throws outright when its
# first entry is not a date, rather than returning NA for it.
date_days <- function(vals) {
  vapply(
    blank_to_na(vals),
    function(v) {
      if (is.na(v)) {
        return(NA_real_)
      }
      as.numeric(tryCatch(as.Date(v), error = function(e) NA))
    },
    numeric(1),
    USE.NAMES = FALSE
  )
}

# The colour scale a mapping layer draws with, built over `vals`. A continuous
# layer gets a numeric ramp (a date one over its days, so the legend can print
# dates); everything else a factor scale. `convert` turns the values being drawn
# into the space the scale was built in, so a frame filtered from the same data
# is coloured consistently. Missing values are always NA_COLOR.
layer_palette <- function(layer, vals) {
  palette <- resolve_gradient_palette(layer$palette %||% "viridis")
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
        pal = colorNumeric(palette, domain = num, na.color = NA_COLOR),
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
    pal = colorFactor(
      resolve_qualitative_palette(palette, n),
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

# Map pane the point markers are drawn into, so their opacity can be applied
# to the layer as a whole (see build_markers()).
MARKER_PANE <- "mapMarkers"

# Styled point markers. layerId = isolate keeps each marker individually
# addressable (e.g. for a future click-driven cross-filter).
build_markers <- function(m, coords, o, full_coords = NULL) {
  cluster_opts <- if (o$cluster) {
    # Spiderfying is always on: without it, isolates sharing one geocoded
    # place stay a cluster at every zoom and can never be told apart. Coverage
    # and zoom-to-bounds-on-click are likewise always on -- both are one-way
    # conveniences (a hover outline, a click that zooms in) with no real reason
    # to disable, so they no longer cost the user a decision.
    markerClusterOptions(
      showCoverageOnHover = TRUE,
      spiderfyOnMaxZoom = TRUE,
      zoomToBoundsOnClick = TRUE,
      maxClusterRadius = o$cluster_radius
    )
  } else {
    NULL
  }
  popup <- build_popup(coords, o$popup_fields)
  label <- if (length(o$hover_field)) {
    txt <- build_popup(coords, o$hover_field)
    if (!is.null(txt)) lapply(txt, htmltools::HTML)
  } else {
    NULL
  }
  lopts <- labelOptions(
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
  m <- addMapPane(m, MARKER_PANE, zIndex = 410)
  m <- addCircleMarkers(
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
    options = pathOptions(pane = MARKER_PANE),
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
    m <- addLegend(
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
        labelFormat(transform = function(x) as.Date(x, origin = "1970-01-01"))
      } else {
        labelFormat(digits = o$legend_digits)
      }
    )
  }
  m
}

# Choropleth: shade Natural Earth countries by the number of isolates whose
# geo_loc_name_country matches, using the Region tab's palette (the mapping
# layers don't apply here — the mapped variable is always the isolate count).
build_choropleth <- function(m, coords, o, full_coords = NULL) {
  world <- get_world()
  counts <- table(coords$geo_loc_name_country)

  n <- as.integer(counts[world$name])
  by_admin <- as.integer(counts[world$admin])
  n[is.na(n)] <- by_admin[is.na(n)]
  world$n <- n

  # Off (default): the domain is the currently-shown (possibly date-filtered)
  # counts, so the color scale rescales to whatever subset is visible — the
  # darkest country always looks "full" regardless of its absolute count. On:
  # the domain comes from the full, unfiltered counts instead, so a partial
  # range renders lighter until the count actually approaches the final
  # total — useful for watching the animation "fill in" over time.
  domain_n <- if (isTRUE(o$region_fixed_scale) && !is.null(full_coords)) {
    full_counts <- table(full_coords$geo_loc_name_country)
    dn <- as.integer(full_counts[world$name])
    dn_by_admin <- as.integer(full_counts[world$admin])
    dn[is.na(dn)] <- dn_by_admin[is.na(dn)]
    dn
  } else {
    world$n
  }

  vals <- if (identical(o$region_transform, "Log")) log1p(world$n) else world$n
  domain_vals <- if (identical(o$region_transform, "Log")) {
    log1p(domain_n)
  } else {
    domain_n
  }
  pal <- suppressWarnings(colorNumeric(
    resolve_gradient_palette(o$region_scale),
    domain = range(domain_vals, na.rm = TRUE),
    na.color = "#f0f0f0"
  ))
  fill <- pal(vals)
  fill[is.na(vals)] <- "#f0f0f0"

  # Every country is labelled, those without isolates included: "0 isolates"
  # is an answer, where no label at all reads as a country the map forgot.
  region_label <- paste0(
    world$name,
    ": ",
    ifelse(is.na(world$n), 0L, world$n),
    " isolates"
  )

  # "Always show labels" only pins the ones actually saying something: a
  # country with zero isolates permanently printing "0 isolates" all over an
  # otherwise empty map just adds noise. One labelOptions() per row (rather
  # than the single shared object every other addPolygons() argument takes)
  # is what lets `permanent` vary polygon by polygon here.
  has_isolates <- !is.na(world$n) & world$n > 0
  region_label_opts <- lapply(has_isolates, function(shown) {
    labelOptions(permanent = isTRUE(o$region_permanent) && shown)
  })

  m <- addPolygons(
    m,
    data = world,
    fillColor = fill,
    fillOpacity = o$region_opacity,
    color = o$region_border,
    weight = 1,
    smoothFactor = 0.3,
    label = region_label,
    labelOptions = region_label_opts,
    highlightOptions = highlightOptions(
      weight = 2,
      color = "#000000",
      fillOpacity = 0.9,
      bringToFront = TRUE
    )
  )

  if (o$legend && any(!is.na(domain_n))) {
    lpal <- suppressWarnings(colorNumeric(
      resolve_gradient_palette(o$region_scale),
      domain = range(domain_n, na.rm = TRUE),
      na.color = "#f0f0f0"
    ))
    m <- addLegend(
      m,
      position = o$legend_pos,
      pal = lpal,
      values = domain_n[!is.na(domain_n)],
      title = "Isolate count",
      opacity = 1,
      labFormat = labelFormat(digits = 0)
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
  agg <- stats::aggregate(
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
  gradient_pal <- colorNumeric(
    resolve_gradient_palette(o$heat_scale %||% "viridis"),
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
    lpal <- colorNumeric(
      resolve_gradient_palette(o$heat_scale %||% "viridis"),
      domain = c(0, effective_max)
    )
    m <- addLegend(
      m,
      position = o$legend_pos,
      pal = lpal,
      values = c(0, effective_max),
      title = if (nzchar(o$legend_title)) o$legend_title else "Isolate count",
      opacity = 1,
      labFormat = labelFormat(digits = 0)
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
    m <- addLegend(
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
# live renderer and the HTML export.
build_map <- function(coords, o, full_coords = NULL, zoom = NULL) {
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
  m <- leaflet(
    options = leafletOptions(
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
    m <- addMapPane(m, "choroplethLabels", zIndex = 450)
  } else {
    m <- if (identical(o$tiles, "OpenStreetMap")) {
      addTiles(m)
    } else {
      addProviderTiles(m, o$tiles)
    }
  }
  if (o$scalebar) {
    m <- addScaleBar(m, position = "bottomleft")
  }
  if (o$minimap) {
    m <- addMiniMap(m, toggleDisplay = TRUE, minimized = FALSE)
  }
  if (o$graticule) {
    m <- addSimpleGraticule(m)
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

  if (is.null(coords) || !nrow(coords)) {
    return(m)
  }

  m <- switch(
    o$mode,
    Choropleth = build_choropleth(m, coords, o, full_coords),
    Heatmap = build_heatmap(m, coords, o, full_coords),
    Charts = build_charts(m, coords, o, full_coords, zoom),
    build_markers(m, coords, o, full_coords)
  )

  # Second half of the label sandwich set up above: added only now, after the
  # polygons, so it lands in the "choroplethLabels" pane stacked above them.
  if (choropleth) {
    m <- addTiles(
      m,
      urlTemplate = choropleth_labels_url,
      attribution = choropleth_labels_attribution,
      options = tileOptions(pane = "choroplethLabels")
    )
  }

  # On-map annotation of the currently displayed date range — separate from
  # the sidebar's map_anim_label text, this one is visible on the map itself
  # (and so also present in the HTML export), and reflects a manual slider
  # drag exactly the same way it reflects the Play animation, since both just
  # go through o$daterange.
  if (isTRUE(o$show_time_label) && length(o$daterange) == 2) {
    label_text <- format_daterange_label(o$daterange)
    m <- addControl(
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
  lng_rng <- range(coords$longitude)
  lat_rng <- range(coords$latitude)
  if (lng_rng[1] == lng_rng[2] && lat_rng[1] == lat_rng[2]) {
    if (fly) {
      flyTo(m, lng = lng_rng[1], lat = lat_rng[1], zoom = single_point_zoom)
    } else {
      setView(m, lng = lng_rng[1], lat = lat_rng[1], zoom = single_point_zoom)
    }
  } else if (fly) {
    flyToBounds(m, lng_rng[1], lat_rng[1], lng_rng[2], lat_rng[2])
  } else {
    fitBounds(m, lng_rng[1], lat_rng[1], lng_rng[2], lat_rng[2])
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
    navset_tab(
      # Basemap + view -------------------------------------------------------
      nav_panel(
        "Basemap",
        value = "basemap",
        icon = shiny$icon("layer-group"),
        shiny$div(
          id = ns("wrap_map_tiles"),
          pickerInput(
            ns("map_tiles"),
            "Base map",
            choices = map_providers,
            selected = mode_tile_defaults[["Markers"]]
          )
        ),
        input_switch(ns("map_scalebar"), "Scale bar", TRUE),
        input_switch(ns("map_minimap"), "Minimap inset", TRUE),
        input_switch(ns("map_graticule"), "Lat/long grid", FALSE),
        input_switch(ns("map_show_controls"), "Zoom controls", TRUE)
      ),
      # Markers --------------------------------------------------------------
      nav_panel(
        "Markers",
        value = "markers",
        icon = shiny$icon("location-dot"),
        accordion(
          open = "Clustering",
          accordion_panel(
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
          accordion_panel(
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
            viz_color(
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
      nav_panel(
        "Mapping",
        value = "color",
        icon = shiny$icon("map-pin"),
        # One picker over the *variables*, the same arrangement as the other
        # engines: picking one adds a layer and app/logic/mapping_engine.R
        # decides its palette from the variable's own profile. Markers draw it
        # as the point fill, Charts as the slices. Missing values are grey.
        field_select(ns, "map_layer_add", "Map a variable"),
        shiny$uiOutput(ns("map_layers_ui"))
      ),
      # Charts (minicharts) -----------------------------------------------------
      nav_panel(
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
        accordion(
          open = FALSE,
          accordion_panel(
            "Chart Style",
            icon = shiny$icon("sliders"),
            # The variable each chart splits by is the mapping layer (Mapping tab).
            shiny$div(
              class = "custom-slider",
              shiny$sliderInput(
                ns("map_chart_size"),
                "Chart size",
                min = 20,
                max = 80,
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
      nav_panel(
        "Region",
        value = "region",
        icon = shiny$icon("earth-europe"),
        radioGroupButtons(
          ns("map_region_transform"),
          "Count scale",
          choices = c("Raw", "Log"),
          selected = "Raw",
          justified = TRUE,
          size = "sm"
        ),
        # A count is never categorical or diverging, so only ordered palettes.
        scale_select(
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
        viz_color(ns, "map_region_border", "Border color", "#000000"),
        input_switch(
          ns("map_region_permanent"),
          "Always show labels",
          FALSE
        )
      ),
      # Heatmap (Density) -------------------------------------------------------
      nav_panel(
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
        scale_select(
          ns,
          "map_heat_scale",
          categories = c("Sequential", "Gradient"),
          selected = "Oranges"
        )
      ),
      # Legend -----------------------------------------------------------------
      nav_panel(
        "Legend",
        value = "legend",
        icon = shiny$icon("list"),
        input_switch(ns("map_legend"), "Show legend", TRUE),
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
        # and punctuation — it opts out of the identifier charset restriction in
        # app/js/index.js.
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
      nav_panel(
        "Labels",
        value = "labels",
        icon = shiny$icon("tag"),
        # The database's columns replace these two sentinel-only choice lists
        # as soon as it loads (see populate_metadata_selects()). update_on =
        # "close": the map only needs to redraw once the picker is done being
        # clicked through, not on every individual selection -- ticking
        # several boxes in quick succession otherwise raced the dropdown's own
        # open state and could close it mid-click.
        field_select(
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
        field_select(
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
        input_switch(ns("map_permanent"), "Always show labels", FALSE),
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
      nav_panel(
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
        input_switch(
          ns("map_show_time_label"),
          "Annotate date range on map",
          TRUE
        ),
        input_switch(
          ns("map_region_fixed_scale"),
          "Fix color scale to full date range",
          TRUE
        )
      )
    ),
    reach_script,
    # Geocoding feedback: how many isolates the last Generate was able to
    # place on the map, with a warning + a few example place names when some
    # couldn't be resolved. See output$map_geocode_status_ui in the server.
    shiny$uiOutput(ns("map_geocode_status_ui"), class = "map-geocode-status"),
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
    # server nav_show/nav_hide / shinyjs::toggle on many small ids) so it can
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
        "if(link){var li=link.closest('.nav-item');if(li){li.style.display=(vis.indexOf(v)>=0)?'':'none';}}",
        "});",
        "var active=wrap.querySelector('.nav-link.active');",
        "if(active&&vis.indexOf(active.getAttribute('data-value'))<0){",
        "var f=wrap.querySelector('.nav-link[data-value='+JSON.stringify(vis[0])+']');if(f){f.click();}",
        "}",
        "var tiles=document.getElementById(tilesWrapId);",
        "if(tiles)tiles.style.display=(m==='Choropleth')?'none':'';",
        "}",
        "$(document).on('change',modeSel,apply);",
        "var n=0,t=setInterval(function(){n++;if(document.querySelector(modeSel)){apply();}if(n>40){clearInterval(t);}},300);",
        "})();"
      )
    })))
  )
}

#' @export
# `options_ui` is accepted for one signature across all engines; this one has no
# distance-computation controls, so the tab never passes any.
ui <- function(id, generate_id, options_ui = NULL) {
  ns <- shiny$NS(id)

  layout_sidebar(
    # See visualization_mst.R: `padding` replaces the old non-unique
    # `id = "plot-sidebar"`, which layout_sidebar has no formal for.
    padding = 0,
    border = FALSE,
    sidebar = sidebar(
      id = ns("controls_sidebar"),
      class = "viz-controls-sidebar",
      position = "right",
      width = 380,
      open = TRUE,
      fillable = TRUE,
      as_fill_carrier(
        shiny$div(
          id = ns("controls_wrap"),
          class = "viz-nav-wrap",
          map_controls(ns)
        )
      )
    ),
    shinyjs::useShinyjs(),
    waiter::useWaiter(),
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
    # counting its data-total seconds down to zero in the browser. It stops at
    # 0:00 if the estimate runs out before geocoding actually returns (the
    # waiter is torn down the moment it does). One observer covers every show.
    shiny$tags$script(
      shiny$HTML(
        "(function(){
           function fmt(s){
             s=Math.max(0,Math.round(s));
             var m=Math.floor(s/60),sec=s%60;
             return m+':'+(sec<10?'0':'')+sec;
           }
           function start(el){
             if(el._countdownStarted)return;
             el._countdownStarted=true;
             var total=parseInt(el.getAttribute('data-total'),10)||0;
             var remaining=total;
             function render(){
               el.textContent='Remaining time '+fmt(remaining)+' / '+fmt(total);
             }
             render();
             var timer=setInterval(function(){
               remaining-=1;
               if(remaining<=0){remaining=0;render();clearInterval(timer);return;}
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
    card(
      full_screen = TRUE,
      class = "plot-card map-plot-card",
      card_body(shiny$uiOutput(ns("plot_area")))
    )
  )
}

#' @export
server <- function(
  id,
  db_path = shiny$reactive(NULL),
  db_rev = db_events$new_bus(),
  session_reset = shiny$reactive(0L),
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
    # below, shown under the "Map mode" picker in this engine's own sidebar.
    map_geocode_status <- shiny$reactiveVal(NULL)

    # How many isolates the last Generate was able to place on the map, with a
    # warning + a few example place names when some couldn't be resolved. NULL
    # (the "Not available" placeholder) until a Generate has produced a status.
    output$map_geocode_status_ui <- shiny$renderUI({
      not_available <- shiny$tagList(
        shiny$div(class = "text-muted small", "No isolates mapped"),
        shiny$div(class = "text-muted small mb-2", "No location geocoded")
      )
      s <- map_geocode_status()
      if (is.null(s) || s$n_isolates == 0) {
        return(not_available)
      }
      # Distinct places resolved (each backs one or more of the mapped isolates)
      # — shown under the isolate summary so it's clear how many geocode lookups
      # the mapped isolates collapsed to, plus how many of those came from the
      # persistent cross-session cache vs. were freshly fetched this Generate
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
      if (s$n_failed_places == 0) {
        shiny$tagList(
          shiny$div(
            class = "mt-2 small text-success",
            shiny$icon("circle-check"),
            sprintf(" All %d isolates mapped", s$n_mapped)
          ),
          locations_line
        )
      } else {
        shiny$tagList(
          tooltip(
            shiny$div(
              class = "mt-2 small text-warning",
              shiny$icon("triangle-exclamation"),
              sprintf(" %d of %d isolates mapped", s$n_mapped, s$n_isolates)
            ),
            paste("Could not geocode:", s$failed_preview)
          ),
          locations_line
        )
      }
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
      if (!length(bins) || utils::tail(bins, 1) < hi) {
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
        shinyjs::toggleState("map_daterange", condition = !playing)
        shinyjs::toggleState("map_interval", condition = !playing)
        shinyjs::toggleState("map_show_time_label", condition = !playing)
        shinyjs::toggleState("map_region_fixed_scale", condition = !playing)
        # Hide the flickering slider behind its cover (see the Time nav_panel).
        shinyjs::toggleClass(
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
      shinyjs::toggleState(
        "map_play",
        condition = playing || length(preview) >= 2
      )

      rng <- input$map_daterange
      bounds <- daterange_bounds()
      can_step <- !playing && !is.null(rng) && !is.null(bounds)
      lo <- if (can_step) as.Date(rng[1]) else NA
      hi <- if (can_step) as.Date(rng[2]) else NA

      shinyjs::toggleState(
        "map_step_start_prev",
        condition = can_step && lo > bounds[1]
      )
      shinyjs::toggleState(
        "map_step_start_next",
        condition = can_step && lo < hi
      )
      shinyjs::toggleState(
        "map_step_end_prev",
        condition = can_step && hi > lo
      )
      shinyjs::toggleState(
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
    waiter <- waiter::Waiter$new(
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
          show_time_label = isTRUE(input$map_show_time_label),
          cluster = (input$map_cluster_radius %||% 0) > 0,
          cluster_radius = input$map_cluster_radius %||% 0,
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
    # observer below: that observer's shinyjs::toggle() calls are reactive and
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
            leafletOutput(ns("map"), height = "100%")
            # class = "viz-map-canvas"
          )
        )
      )
    })

    # Set TRUE by every Generate to make the renderer (re)frame the view on the
    # new data, and held TRUE across the several re-renders one Generate can
    # trigger — the generate observer also reprograms the date / interval / heat
    # controls, each of which invalidates the debounced map_opts() ~250ms later
    # and fires a second render pass. It's cleared only once the client reports
    # the settled view back (see the map_center observer below). Tracking a mere
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
    output$map <- renderLeaflet({
      o <- map_opts()
      base <- map_coords()
      coords <- filter_coords(base, o)

      # Read the current view first so Charts-mode clustering can bin at the
      # live zoom (the client reports it after the first interaction); on the
      # very first render z is NULL and build_charts falls back to per-place,
      # which the debounced zoom observer below then re-clusters once fitBounds
      # settles a zoom.
      has_data <- !is.null(coords) && nrow(coords) > 0
      z <- shiny$isolate(input$map_zoom)
      ctr <- shiny$isolate(input$map_center)
      # Read (not depend on) the fit flag: clearing it must not itself trigger a
      # re-render, or every user pan — which clears it — would rebuild the map.
      refit <- shiny$isolate(pending_fit())

      m <- build_map(coords, o, full_coords = base, zoom = z)

      # Refit to the data on a fresh Generate; otherwise keep the user's current
      # view (zoom/center inputs are populated by Leaflet after interaction).
      if (has_data && (refit || is.null(z) || is.null(ctr))) {
        m <- frame_coords(m, coords)
      } else if (!is.null(z) && !is.null(ctr)) {
        m <- setView(m, lng = ctr$lng, lat = ctr$lat, zoom = z)
      } else {
        m <- setView(m, lng = 10, lat = 50, zoom = 4)
      }
      m
    })

    # The client reports a center after the fit lands (or after any user
    # pan/zoom); either way the view is now settled, so stop forcing a refit.
    # This keeps later restyles and animation frames on the user's view rather
    # than re-framing every time, and — because it fires once the fitted view
    # has actually been reported — leaves input$map_zoom/center consistent with
    # a cleared flag, so the race described above resolves to the fitted frame
    # whichever pass wins.
    shiny$observeEvent(
      input$map_center,
      pending_fit(FALSE),
      ignoreInit = TRUE
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
        base <- map_coords()
        coords <- filter_coords(base, o)
        if (is.null(coords) || !nrow(coords)) {
          return()
        }
        proxy <- leafletProxy("map")
        clearMinicharts(proxy)
        # Pass the full (unfiltered) coords so a re-cluster on zoom keeps the
        # same fixed full-range category set/palette/legend the full render uses.
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
      update_field_select(session, "map_layer_add", prof)

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
        update_field_select(
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
        suitable_scale_categories(
          if (isTRUE(binned$continuous)) "Numeric" else "Factor",
          shown
        )
      )

      shiny$showModal(shiny$modalDialog(
        title = paste("Mapping:", l$title),
        size = "s",
        easyClose = TRUE,
        if (is_date_profile(prof)) {
          granularity_select(
            ns,
            "map_layer_granularity",
            l$granularity,
            label = "Group dates by",
            values = values
          )
        },
        scale_select(
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
      shinyjs::toggle(
        id = "wrap_map_legend_digits",
        condition = identical(input$map_mode, "Markers") &&
          isTRUE(layer$continuous) &&
          !identical(layer$transform, "as_date")
      )
    })

    # Refit the controls solved from the generated coordinates rather than
    # declared: the date range spans the data, and the heatmap's max intensity
    # is the busiest place's isolate count, so the slider spans a range where
    # moving it is visible instead of saturating everywhere.
    fit_data_controls <- function(coords) {
      if (is.null(coords) || !nrow(coords)) {
        return(invisible(NULL))
      }
      dts <- suppressWarnings(as.Date(coords$sample_collection_date))
      if (any(!is.na(dts))) {
        lo <- min(dts, na.rm = TRUE)
        hi <- max(dts, na.rm = TRUE)
        daterange_bounds(c(lo, hi))
        shiny$updateSliderInput(
          session,
          "map_daterange",
          min = lo,
          max = hi,
          value = c(lo, hi)
        )
      }
      max_per_place <- max(table(coords$place))
      shiny$updateSliderInput(
        session,
        "map_heat_max",
        max = max(max_per_place, 10),
        value = max_per_place
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
      waiter$update(html = geocode_waiter_html(geocode_pending_count(meta)))

      built <- tryCatch(
        build_map_coords(meta),
        error = function(e) {
          shiny$showNotification(
            paste("Could not geocode isolate locations:", conditionMessage(e)),
            type = "error"
          )
          list(coords = NULL, status = empty_geocode_status())
        }
      )
      coords <- built$coords
      map_geocode_status(built$status)

      if (is.null(coords) || !nrow(coords)) {
        shiny$showNotification(
          "No mappable locations found in the metadata (country / state fields).",
          type = "warning"
        )
      } else {
        fit_data_controls(coords)
        # "Hour" only makes sense to offer when the data actually carries a
        # time-of-day component — filter_coords() floors to whole days, so an
        # Hour animation over date-only data would silently collapse back to
        # Day granularity and look broken rather than just under-precise.
        # `sample_collection_date` is free text (see field_types.R), so
        # as.POSIXct's format guessing can throw a hard error (not just a
        # warning) when it can't find one format that fits every value —
        # suppressWarnings() doesn't catch that. Fall back to "no time
        # component" rather than crashing this observer.
        ts <- tryCatch(
          suppressWarnings(as.POSIXct(
            coords$sample_collection_date,
            tz = "UTC"
          )),
          error = function(e) as.POSIXct(character(0), tz = "UTC")
        )
        has_time <- any(!is.na(ts) & format(ts, "%H:%M:%S") != "00:00:00")
        day_choices <- c("Day", "Week", "Month", "Year")
        interval_choices <- if (has_time) {
          c("Hour", day_choices)
        } else {
          day_choices
        }
        updateRadioGroupButtons(
          session,
          "map_interval",
          choices = interval_choices,
          selected = if (isTRUE(input$map_interval %in% interval_choices)) {
            input$map_interval
          } else {
            "Day"
          }
        )
      }
      map_coords(coords)
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
        shinyjs::toggle(id = "viz_prompt", condition = !isTRUE(generated()))
        shinyjs::toggle(id = "map_wrap", condition = isTRUE(generated()))
      },
      ignoreNULL = FALSE
    )

    # Reset-view button: refit to the (date-filtered) data with a fly animation.
    shiny$observeEvent(input$map_reset, {
      coords <- filter_coords(map_coords(), map_opts())
      shiny$req(!is.null(coords) && nrow(coords) > 0)
      frame_coords(leafletProxy("map"), coords, fly = TRUE)
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
          zoom = shiny$isolate(input$map_zoom)
        )
        if (!is.null(coords) && nrow(coords) > 0) {
          m <- frame_coords(m, coords)
        }
        htmlwidgets::saveWidget(m, file, selfcontained = TRUE)
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

    # On session reset (top-level app-reset), clear the markers and cached
    # coordinates so the stale map is torn down, and restore the sidebar
    # controls to their defaults — mirroring the local "Reset settings" button
    # (reset_map_settings(), defined below).
    shiny$observeEvent(
      session_reset(),
      {
        map_coords(NULL)
        map_geocode_status(NULL)
        generated(FALSE)
        anim_playing(FALSE)
        anim_idx(0L)
        reset_map_settings()
      },
      ignoreInit = TRUE
    )

    # Reset settings: restore every control in this engine's own sidebar to
    # its coded default, WITHOUT disturbing the already-geocoded plot (no
    # re-Generate needed), and drop the mapping, which is reactiveVal state
    # rather than a control. Shared by the "Reset settings" button and the
    # top-level app-reset (session_reset) path above.
    #
    # The catalogue (MAP_CONTROLS / map_control_defaults()) is what makes that
    # complete: this used to be shinyjs::reset(), which silently skips every
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

      apply_controls(session, map_control_defaults(), MAP_CONTROLS)
      populate_metadata_selects(force_default = TRUE)
      fit_data_controls(map_coords())
    }

    on_confirmed_reset(
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
      collect_input_snapshot(input, "map_"),
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

      # Same catalogue a reset applies, holding saved values instead of the
      # coded ones.
      apply_controls(session, vals, MAP_CONTROLS)
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
