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
    pickerOptions,
    updatePickerInput,
  ],
  leaflet[
    leaflet,
    leafletOptions,
    leafletOutput,
    renderLeaflet,
    leafletProxy,
    addTiles,
    addProviderTiles,
    providerTileOptions,
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
    colorFactor,
    colorNumeric,
    colorBin,
    colorQuantile,
    fitBounds,
    flyToBounds,
    flyTo,
    setView,
  ],
  leaflet.extras[addFullscreenControl, addHeatmap],
  leaflet.minicharts[addMinicharts, clearMinicharts],
  tidygeocoder[geocode],
  shinyjs[runjs],
  utils[URLencode, read.csv, write.csv],
  waiter[Waiter, spin_flower, useWaiter],
)
box::use(
  app /
    logic /
    viz_helpers[
      scale_select,
      color_scales,
      suitable_scale_categories,
      viz_color,
      reset_viz_colors,
      reset_viz_radio_buttons,
      collect_input_snapshot,
      apply_input_snapshot,
    ],
  app / logic / functions[render_info],
  app / logic / paths[app_local_share_path],
  app / logic / field_labels[field_label],
)

`%||%` <- function(a, b) if (is.null(a) || length(a) == 0) b else a

# Non-API-key basemap providers offered in the Basemap select.
map_providers <- c(
  "OpenStreetMap" = "OpenStreetMap",
  "OSM Humanitarian" = "OpenStreetMap.HOT",
  "OSM (German style)" = "OpenStreetMap.DE",
  "OpenTopoMap" = "OpenTopoMap",
  "Carto Light" = "CartoDB.Positron",
  "Carto Dark" = "CartoDB.DarkMatter",
  "Carto Voyager" = "CartoDB.Voyager",
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
  Heatmap = "CartoDB.Positron",
  Charts = "Esri.WorldGrayCanvas"
)

# Choropleth basemap handling: a busy — or even muted — basemap underneath a
# color fill still reads as "textured" wherever the fill isn't fully opaque,
# so Choropleth mode renders no base tile layer at all (the "Base map" picker
# is hidden for this mode; see choro_hide in map_controls()). Place names are
# still useful for context, though, so a single labels-only tile layer (via
# leaflet.providers' CartoDB "OnlyLabels" split) is placed in its own pane
# stacked above the polygon overlay.
choropleth_labels_provider <- "CartoDB.PositronOnlyLabels"

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

# Resolve the effective Factor/Numeric/Bin/Quantile type for a variable: "Auto"
# picks Numeric/Factor based on whether the values parse as numeric, and an
# explicitly numeric-family type still falls back to Factor when they don't
# (e.g. a date string). Shared by make_palette() (below) and the color-scale
# category filter (the map_col_scale dispatcher), so the dropdown's filtering
# and the actual renderer can never disagree about what a variable "is".
resolve_scale_type <- function(scale_type, vals) {
  num <- suppressWarnings(as.numeric(vals))
  is_num <- !all(is.na(num))
  type <- scale_type %||% "Auto"
  if (identical(type, "Auto")) {
    type <- if (is_num) "Numeric" else "Factor"
  }
  if (type %in% c("Numeric", "Bin", "Quantile") && !is_num) {
    type <- "Factor"
  }
  type
}

# Resolve a color palette + values for a variable, honouring the requested
# scale type. Numeric-only scales fall back to a factor scale when the variable
# is not numeric, and any palette-construction error degrades to a factor scale.
make_palette <- function(scale_type, palette, vals, reverse, na_color, bins) {
  reverse <- isTRUE(reverse)
  na_color <- na_color %||% "#808080"
  type <- resolve_scale_type(scale_type, vals)
  num <- suppressWarnings(as.numeric(vals))

  factor_pal <- function() {
    list(
      pal = colorFactor(
        palette,
        domain = vals,
        reverse = reverse,
        na.color = na_color
      ),
      values = vals,
      type = "Factor"
    )
  }
  n_bins <- as.integer(bins %||% 5)
  # suppressWarnings: a qualitative palette (e.g. Set1) with more levels than it
  # has colors warns via RColorBrewer but still interpolates a valid ramp.
  suppressWarnings(tryCatch(
    switch(
      type,
      Factor = factor_pal(),
      Numeric = list(
        pal = colorNumeric(
          palette,
          domain = num,
          reverse = reverse,
          na.color = na_color
        ),
        values = num,
        type = "Numeric"
      ),
      Bin = list(
        pal = colorBin(
          palette,
          domain = num,
          bins = n_bins,
          reverse = reverse,
          na.color = na_color
        ),
        values = num,
        type = "Bin"
      ),
      Quantile = list(
        pal = colorQuantile(
          palette,
          domain = num,
          n = n_bins,
          reverse = reverse,
          na.color = na_color
        ),
        values = num,
        type = "Quantile"
      )
    ),
    error = function(e) factor_pal()
  ))
}

# --- mode renderers ----------------------------------------------------------

# Styled point markers. layerId = isolate keeps each marker individually
# addressable (e.g. for a future click-driven cross-filter).
build_markers <- function(m, coords, o, full_coords = NULL) {
  cluster_opts <- if (o$cluster) {
    markerClusterOptions(
      showCoverageOnHover = o$coverage,
      spiderfyOnMaxZoom = o$spiderfy,
      zoomToBoundsOnClick = o$zoom_to_bounds,
      maxClusterRadius = o$cluster_radius,
      disableClusteringAtZoom = o$cluster_zoom_level
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

  use_var <- o$color_var &&
    !is.null(o$col_var) &&
    o$col_var %in% names(coords)
  pal_info <- NULL
  fill <- o$marker_color
  if (use_var) {
    # "Fix color scale to full date range" (o$region_fixed_scale): build the
    # palette + legend from the full, unfiltered values so a category keeps its
    # color and the legend keeps every key as the date range animates — rather
    # than rescaling to whichever subset is currently visible. The fill is still
    # applied to the visible coords (a subset of the domain). Off: domain and
    # fill both come from the visible subset, exactly as before.
    fixed <- isTRUE(o$region_fixed_scale) &&
      !is.null(full_coords) &&
      o$col_var %in% names(full_coords)
    ref_vals <- if (fixed) full_coords[[o$col_var]] else coords[[o$col_var]]
    pal_info <- make_palette(
      o$scale_type,
      o$col_scale,
      ref_vals,
      o$reverse,
      o$na_color,
      o$bins
    )
    # Mirror make_palette()'s own numeric coercion so the fill maps the visible
    # values through the same space the palette domain was built in.
    apply_vals <- coords[[o$col_var]]
    if (pal_info$type %in% c("Numeric", "Bin", "Quantile")) {
      apply_vals <- suppressWarnings(as.numeric(apply_vals))
    }
    fill <- pal_info$pal(apply_vals)
  }

  # weight = 0 draws no border, so a separate "show border" toggle is
  # redundant; derive it straight from the border-width slider.
  m <- addCircleMarkers(
    m,
    data = coords,
    lng = ~longitude,
    lat = ~latitude,
    layerId = ~isolate,
    radius = o$radius,
    stroke = o$weight > 0,
    color = o$stroke_color,
    weight = o$weight,
    opacity = 1,
    fillColor = fill,
    fillOpacity = o$opacity,
    popup = popup,
    label = label,
    labelOptions = lopts,
    clusterOptions = cluster_opts
  )

  if (use_var && o$legend) {
    m <- addLegend(
      m,
      position = o$legend_pos,
      pal = pal_info$pal,
      values = pal_info$values,
      title = if (nzchar(o$legend_title)) o$legend_title else o$col_var,
      # Swatches stay fully opaque so their colors read correctly; the
      # legend's background transparency (what map_legend_opacity actually
      # controls) is applied to the whole box in build_map() below.
      opacity = 1,
      labFormat = labelFormat(digits = o$legend_digits)
    )
  }
  m
}

# Choropleth: shade Natural Earth countries by the number of isolates whose
# geo_loc_name_country matches, using the Color tab's palette + reverse toggle
# (the color-by-variable controls don't apply here — the mapped variable is
# always the isolate count).
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
    o$col_scale,
    domain = range(domain_vals, na.rm = TRUE),
    na.color = "#f0f0f0",
    reverse = o$reverse
  ))
  fill <- pal(vals)
  fill[is.na(vals)] <- "#f0f0f0"

  # A leaflet label of NA (rather than "") skips binding a tooltip to that
  # polygon entirely (see leaflet.js addLayers()), which is the native way to
  # exclude specific rows from getting a hover/permanent label at all.
  region_label <- paste0(
    world$name,
    ": ",
    ifelse(is.na(world$n), 0L, world$n),
    " isolates"
  )
  if (isTRUE(o$region_label_nonzero)) {
    region_label[is.na(world$n) | world$n == 0] <- NA
  }

  m <- addPolygons(
    m,
    data = world,
    fillColor = fill,
    fillOpacity = o$region_opacity,
    color = o$region_border,
    weight = 1,
    smoothFactor = 0.3,
    label = region_label,
    labelOptions = labelOptions(permanent = o$region_permanent),
    highlightOptions = highlightOptions(
      weight = 2,
      color = "#000000",
      fillOpacity = 0.9,
      bringToFront = TRUE
    )
  )

  if (o$legend && any(!is.na(domain_n))) {
    lpal <- suppressWarnings(colorNumeric(
      o$col_scale,
      domain = range(domain_n, na.rm = TRUE),
      na.color = "#f0f0f0",
      reverse = o$reverse
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
  gradient_pal <- colorNumeric(o$heat_scale %||% "viridis", domain = c(0, 1))
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

  # Mirrors Choropleth's legend: same gradient the heat layer itself uses
  # (0 to the effective max), so the color a place shows on the map reads
  # back to an isolate count the same way Choropleth's fill does.
  if (o$legend) {
    lpal <- colorNumeric(
      o$heat_scale %||% "viridis",
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

# One minichart per location, showing the composition of a categorical
# variable, colored per o$chart_scale (a Qualitative palette — see the
# comment on that picker in map_controls()). Most brewer Qualitative palettes
# only have 8-12 distinct colors, so a variable with more than a handful of
# levels (e.g. a near-unique field like collection date) would produce
# unreadable slices and a legend that overflows the map — the least-frequent
# levels are folded into "Other" to keep every chart legible regardless of
# which variable is selected.
#
# With clustering on (and a known zoom), nearby locations are grouped by
# pixel-grid cell so overlapping charts combine into one aggregate chart that
# splits as the user zooms in — see cluster_cell(). Without a zoom (the initial
# render before the client reports one, or the static HTML export, which has no
# server to re-bin on zoom) it falls back to one chart per exact place.
build_charts <- function(m, coords, o, full_coords = NULL, zoom = NULL) {
  var <- o$chart_var
  if (is.null(var) || !var %in% names(coords)) {
    return(m)
  }

  max_categories <- 9

  # "Fix color scale to full date range" (o$region_fixed_scale): derive the
  # category set — which levels get their own slice vs. fold into "Other" — the
  # column order, the palette, and the legend from the full, unfiltered data so
  # they all stay stable as the date range animates. Off: recompute the top-N
  # folding and category set from just the visible subset, so a level's color
  # can shift and the legend gains/loses keys frame to frame.
  fixed <- isTRUE(o$region_fixed_scale) &&
    !is.null(full_coords) &&
    var %in% names(full_coords)
  ref_vals <- if (fixed) full_coords[[var]] else coords[[var]]
  ref_freq <- sort(table(ref_vals), decreasing = TRUE)
  folded <- length(ref_freq) > max_categories
  top <- if (folded) {
    names(ref_freq)[seq_len(max_categories)]
  } else {
    names(ref_freq)
  }
  # Master column/legend order; "Other" (the fold bucket) always last.
  cats <- if (folded) c(top, "Other") else top

  vals <- coords[[var]]
  if (folded) {
    vals <- ifelse(vals %in% top, vals, "Other")
  }

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
  # fixed, the complete full-range set; otherwise just the visible set, in
  # frequency order with "Other" last (matching the old explicit reorder).
  cd <- as.data.frame.matrix(table(key, factor(vals, levels = cats)))

  # Each chart sits at the centroid of the points in its group; with the
  # per-place key every point in a group shares one coordinate, so the
  # unclustered layout is identical to before.
  lng <- as.numeric(tapply(coords$longitude, key, mean)[rownames(cd)])
  lat <- as.numeric(tapply(coords$latitude, key, mean)[rownames(cd)])

  # One color per category column, drawn from the chosen Qualitative palette
  # (see scale_select(..., categories = "Qualitative") in map_controls()) via
  # the same colorFactor() leaflet already uses for Markers-mode category
  # coloring, so a brewer palette with fewer swatches than categories degrades
  # the same way there too (an interpolated ramp, not a hard error).
  chart_pal <- suppressWarnings(
    colorFactor(o$chart_scale %||% "Set1", domain = names(cd))
  )

  addMinicharts(
    m,
    lng = lng,
    lat = lat,
    type = o$chart_type,
    chartdata = cd,
    colorPalette = chart_pal(names(cd)),
    width = o$chart_size,
    opacity = o$chart_opacity,
    # addMinicharts() defaults to legend=TRUE at a hardcoded "topright" —
    # independent of (and ignoring) the shared Legend tab's own position
    # control, which is why it used to collide with the topright date-range
    # label regardless of what the user picked elsewhere. Wiring it to the
    # same o$legend/o$legend_pos as the other modes' legends fixes both.
    legend = isTRUE(o$legend),
    legendPosition = o$legend_pos %||% "topright"
  )
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
  # zoomControl is a native leaflet map option (toggling it removes the +/-
  # buttons entirely, rather than just hiding them behind CSS); the fullscreen
  # button lives in the same corner, so one switch controls both.
  #
  # zoomSnap sets the granularity the map may rest at (0.25 = quarter-level
  # steps instead of integers), so mousewheel zoom can settle on fractional
  # levels; wheelPxPerZoomLevel raises the scroll distance per zoom level so
  # each wheel notch moves less. zoomDelta keeps the +/- buttons and keyboard
  # in step with the finer snap.
  m <- leaflet(
    options = leafletOptions(
      zoomControl = o$show_controls,
      zoomSnap = 0.25,
      zoomDelta = 0.25,
      wheelPxPerZoomLevel = 120
    )
  )

  # Choropleth mode has no base tile layer at all — the basemap picker is
  # hidden for this mode (it had no visible effect once a fill covers the
  # polygons) and a busy or even muted tile image underneath still reads as
  # "textured" wherever the fill isn't fully opaque. Only a labels-only tile
  # layer (see choropleth_labels_provider above) is kept, in its own pane
  # stacked above the polygon overlay, so place names stay legible for
  # context against an otherwise blank background.
  labels_provider <- NULL
  if (identical(o$mode, "Choropleth")) {
    m <- addMapPane(m, "choroplethLabels", zIndex = 450)
    labels_provider <- choropleth_labels_provider
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
    m <- addFullscreenControl(m, position = "topleft")
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
  if (!is.null(labels_provider)) {
    m <- addProviderTiles(
      m,
      labels_provider,
      options = providerTileOptions(pane = "choroplethLabels")
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
  # and (2) the cap can't be modelled from control *heights*, because the
  # legend grows a different direction depending on which corner it's anchored
  # in (a bottom-anchored legend grows up into the top-right date label, which
  # a top-down height reservation doesn't account for). Instead the legend's
  # fixed anchored edge and each obstacle's real position are measured live
  # with getBoundingClientRect, and the cap is the gap between them.
  if (isTRUE(o$legend)) {
    m <- htmlwidgets::onRender(
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
           var minimap = el.querySelector('.leaflet-control-minimap');
           // Which corner the legend is anchored in decides which way it grows
           // and therefore which controls it can run into. `.leaflet-bottom`
           // controls grow upward from their bottom edge; `.leaflet-top` ones
           // grow downward from their top. `.leaflet-right` ones share the
           // right edge with the date label (always top-right) and the minimap
           // (always bottom-right).
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
           // The legend's anchored edge (its bottom if it grows up, its top if
           // it grows down) stays fixed regardless of its own height, so it can
           // be measured directly and the cap derived as the distance from that
           // edge to the nearest obstacle — the live-measured position of the
           // date label / minimap / map edge — rather than modelled from
           // heights (which got the bottom-anchored case wrong before).
           var lRect = l.getBoundingClientRect();
           var avail;
           if (isBottom) {
             var topLimit = mapRect.top + EDGE;
             if (dateLabel && isRight) {
               topLimit = Math.max(topLimit, dateLabel.getBoundingClientRect().bottom + GAP);
             }
             avail = lRect.bottom - topLimit;
           } else {
             var bottomLimit = mapRect.bottom - EDGE;
             if (minimap && isRight) {
               bottomLimit = Math.min(bottomLimit, minimap.getBoundingClientRect().top - GAP);
             }
             avail = bottomLimit - lRect.top;
           }
           l.style.maxHeight = Math.max(avail, 60) + 'px';
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
          open = "Marker Style",
          accordion_panel(
            "Clustering",
            icon = shiny$icon("object-group"),
            shiny$div(
              class = "custom-slider",
              shiny$sliderInput(
                ns("map_cluster_radius"),
                "Cluster radius (px)",
                min = 20,
                max = 200,
                value = 80,
                step = 10
              )
            ),
            input_switch(
              ns("map_zoom_to_bounds"),
              "Zoom to bounds on cluster click",
              TRUE
            ),
            input_switch(ns("map_spiderfy"), "Spiderfy at max zoom", TRUE),
            input_switch(ns("map_coverage"), "Show coverage on hover", TRUE),
            # Raw Leaflet zoom levels (0 = world, ~18 = building) aren't
            # self-explanatory on their own, so the full range is exposed as
            # a slider with the old named presets kept on as reference
            # labels underneath. 0 doubles as "None" — the zoom level the
            # clusters stay disbanded until is meaningless once clustering
            # itself is off, and nobody picks the world view as a real
            # disable-at threshold — so it folds in what used to be a
            # separate cluster on/off switch (and the "Marker style:
            # Cluster" option before that): this one control now fully
            # decides whether markers cluster at all, and — if so — the
            # zoom level they stay clustered until.
            shiny$div(
              id = ns("cluster_zoom_wrap"),
              class = "custom-slider cluster-zoom-slider",
              shiny$sliderInput(
                ns("map_cluster_zoom_level"),
                "Clustering",
                min = 0,
                max = 18,
                value = 7,
                step = 1,
                ticks = FALSE
              ),
              # A tick at every one of the 19 zoom levels the slider can
              # land on, not just the 5 named ones — those 5 (the same
              # values the labels below name) get the "major" class for a
              # taller, darker mark so they still read as the meaningful
              # stops.
              shiny$div(
                class = "cluster-zoom-slider_ticks",
                lapply(0:18, function(v) {
                  shiny$span(
                    class = if (v %in% c(0, 4, 7, 11, 15)) {
                      "cluster-zoom-slider_tick-major"
                    },
                    `data-pct` = v / 18 * 100
                  )
                })
              ),
              shiny$div(
                class = "cluster-zoom-slider_labels",
                shiny$span("None", `data-pct` = 0),
                shiny$span("Country", `data-pct` = 22.22),
                shiny$span("Region", `data-pct` = 38.89),
                shiny$span("City", `data-pct` = 61.11),
                shiny$span("Street", `data-pct` = 83.33)
              ),
              # ion.rangeSlider reserves extra horizontal space at each end of
              # the widget for the value tooltip bubble, so the visible track
              # (.irs-line) is narrower than — and inset from — the slider's
              # own box by an amount CSS can't predict. Reading the rendered
              # .irs-line back and placing each tick/label in pixels against
              # it is the only way to actually line them up with the zoom
              # levels they name; redone on resize and on any Bootstrap
              # accordion "shown" (the track is 0-width while its panel is
              # collapsed).
              shiny$tags$script(shiny$HTML(local({
                wrap_id <- ns("cluster_zoom_wrap")
                paste0(
                  "(function(){",
                  "var wrap=document.getElementById('",
                  wrap_id,
                  "');",
                  "if(!wrap)return;",
                  "var groups=[",
                  "wrap.querySelector('.cluster-zoom-slider_ticks'),",
                  "wrap.querySelector('.cluster-zoom-slider_labels')",
                  "];",
                  "function position(){",
                  "var line=wrap.querySelector('.irs-line');",
                  "if(!line||!line.offsetWidth)return false;",
                  "var lineRect=line.getBoundingClientRect();",
                  "groups.forEach(function(group){",
                  "var base=group.getBoundingClientRect();",
                  "group.querySelectorAll('[data-pct]').forEach(function(el){",
                  "var pct=parseFloat(el.getAttribute('data-pct'));",
                  "el.style.left=((lineRect.left-base.left)+lineRect.width*pct/100)+'px';",
                  "});",
                  "});",
                  "return true;",
                  "}",
                  "var tries=0;",
                  "var timer=setInterval(function(){",
                  "tries++;",
                  "if(position()||tries>40)clearInterval(timer);",
                  "},50);",
                  "window.addEventListener('resize',position);",
                  "if(window.jQuery)jQuery(document).on('shown.bs.collapse',position);",
                  "})();"
                )
              })))
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
            shiny$div(
              class = "custom-slider",
              shiny$sliderInput(
                ns("map_opacity"),
                "Fill opacity",
                min = 0.1,
                max = 1,
                value = 1,
                step = 0.05
              )
            ),
            viz_color(ns, "map_marker_color", "Fill color", "#2c7fb8"),
            shiny$div(
              class = "custom-slider",
              shiny$sliderInput(
                ns("map_weight"),
                "Border width",
                min = 0,
                max = 6,
                value = 0,
                step = 0.5
              )
            ),
            viz_color(ns, "map_stroke_color", "Border color", "#333333")
          )
        )
      ),
      # Variable coloring ------------------------------------------------------
      nav_panel(
        "Color",
        value = "color",
        icon = shiny$icon("palette"),
        accordion(
          open = "Variable Mapping",
          accordion_panel(
            "Variable Mapping",
            icon = shiny$icon("layer-group"),
            input_switch(ns("map_color_var"), "Color by variable", FALSE),
            pickerInput(ns("map_col_var"), "Variable", choices = NULL)
          ),
          accordion_panel(
            "Scale",
            icon = shiny$icon("sliders"),
            scale_select(ns, "map_col_scale"),
            shiny$div(
              id = ns("wrap_map_scale_type"),
              shiny$radioButtons(
                ns("map_scale_type"),
                "Scale type",
                choices = c("Auto", "Factor", "Numeric", "Bin", "Quantile"),
                selected = "Auto",
                inline = TRUE
              )
            ),
            shiny$div(
              id = ns("wrap_map_bins"),
              shiny$div(
                class = "custom-slider",
                shiny$sliderInput(
                  ns("map_bins"),
                  "Bins (numeric)",
                  min = 3,
                  max = 9,
                  value = 5,
                  step = 1
                )
              )
            ),
            input_switch(ns("map_reverse"), "Reverse palette", FALSE),
            shiny$div(
              id = ns("wrap_map_na_color"),
              viz_color(ns, "map_na_color", "Missing color", "#808080")
            )
          )
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
        pickerInput(
          ns("map_popup"),
          "Popup fields",
          choices = c(
            "Isolate" = "isolate",
            "Location" = "place",
            "Collection Date" = "sample_collection_date"
          ),
          selected = c("isolate", "place", "sample_collection_date"),
          multiple = TRUE,
          options = pickerOptions(
            actionsBox = TRUE,
            liveSearch = TRUE,
            selectedTextFormat = "count > 3",
            container = "body"
          )
        ),
        pickerInput(
          ns("map_hover_field"),
          "Hover field(s)",
          choices = NULL,
          multiple = TRUE,
          options = pickerOptions(
            actionsBox = TRUE,
            liveSearch = TRUE,
            selectedTextFormat = "count > 3",
            container = "body"
          )
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
      ),
      # Choropleth (Region) ----------------------------------------------------
      nav_panel(
        "Region",
        value = "region",
        icon = shiny$icon("earth-europe"),
        shiny$radioButtons(
          ns("map_region_transform"),
          "Count scale",
          choices = c("Raw", "Log"),
          inline = TRUE
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
        viz_color(ns, "map_region_border", "Border color", "#ffffff"),
        input_switch(
          ns("map_region_permanent"),
          "Always show labels",
          FALSE
        ),
        input_switch(
          ns("map_region_label_nonzero"),
          "Only label areas with isolates",
          TRUE
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
        shiny$div(
          class = "custom-slider",
          shiny$sliderInput(
            ns("map_heat_radius"),
            "Radius",
            min = 5,
            max = 50,
            value = 25,
            step = 1
          )
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
        # categorical or meaningfully diverging, so this picker is statically
        # restricted (unlike map_col_scale below, which depends on whatever
        # variable is currently mapped).
        scale_select(
          ns,
          "map_heat_scale",
          categories = c("Sequential", "Gradient")
        )
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
        pickerInput(ns("map_chart_var"), "Variable", choices = NULL),
        # Each chart shows the composition of a categorical variable (see
        # build_charts()'s max_categories folding below), so — like
        # map_heat_scale above — this is a static restriction rather than a
        # per-variable dynamic one: it's never numeric, so only Qualitative
        # palettes are ever suitable.
        scale_select(ns, "map_chart_scale", categories = "Qualitative"),
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
            value = 1,
            step = 0.05
          )
        ),
        # Combine charts whose locations sit within the cluster radius of each
        # other on screen into a single aggregate chart, splitting apart again
        # as the map is zoomed in (see build_charts()/cluster_cell()). The
        # minichart analog of Markers-mode clustering.
        input_switch(
          ns("map_chart_cluster"),
          "Cluster overlapping charts",
          TRUE
        ),
        shiny$div(
          class = "custom-slider",
          shiny$sliderInput(
            ns("map_chart_cluster_radius"),
            "Cluster radius (px)",
            min = 20,
            max = 200,
            value = 100,
            step = 10
          )
        )
      )
    ),
    shiny$div(
      class = "map-mode-dropup",
      pickerInput(
        ns("map_mode"),
        "Map mode",
        choices = c("Markers", "Choropleth", "Heatmap", "Charts")
      )
    ),
    # Geocoding feedback: how many isolates the last Generate was able to
    # place on the map, with a warning + a few example place names when some
    # couldn't be resolved. See output$map_geocode_status_ui in the server.
    shiny$uiOutput(ns("map_geocode_status_ui"), class = "map-geocode-status"),
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
      scale_type_name <- ns("map_scale_type")
      choro_hide <- ns(c(
        "wrap_map_tiles",
        "wrap_map_scale_type",
        "wrap_map_bins",
        "wrap_map_na_color"
      ))
      digits_id <- ns("wrap_map_legend_digits")
      js_arr <- function(x) {
        paste0("[", paste0("'", x, "'", collapse = ","), "]")
      }
      paste0(
        "(function(){",
        "var modeSel='#",
        mode_id,
        "';",
        "var scaleTypeName='",
        scale_type_name,
        "';",
        "var scaleTypeSel='input[name=\"'+scaleTypeName+'\"]';",
        "var digitsId='",
        digits_id,
        "';",
        "var tabsByMode={",
        "Markers:['basemap','markers','color','legend','labels','time','export'],",
        "Choropleth:['basemap','color','legend','time','region','export'],",
        "Heatmap:['basemap','time','density','legend','export'],",
        "Charts:['basemap','time','charts','legend','export']};",
        "var allTabs=['basemap','markers','color','legend','labels','time','region','density','charts','export'];",
        "var choroHide=",
        js_arr(choro_hide),
        ";",
        "function curMode(){var el=document.querySelector(modeSel);return el?el.value:'Markers';}",
        "function curScaleType(){var el=document.querySelector(scaleTypeSel+':checked');return el?el.value:'Auto';}",
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
        "choroHide.forEach(function(id){var el=document.getElementById(id);if(el)el.style.display=(m==='Choropleth')?'none':'';});",
        # Choropleth colors purely by isolate count — there's no per-point
        # "color by variable" to map — so the whole panel (not just its
        # inputs) is hidden rather than left as an empty, awkward-looking
        # shell. bslib's own accordion_panel() has no id/class param, but it
        # does set data-value to the panel's title, giving a stable selector
        # for the entire .accordion-item.
        "var vmPanel=wrap.querySelector('.accordion-item[data-value=\"Variable Mapping\"]');",
        "if(vmPanel){vmPanel.style.display=(m==='Choropleth')?'none':'';}",
        # With Variable Mapping hidden, Choropleth's Color tab is down to a
        # single "Scale" panel holding just the palette picker and reverse
        # switch — too little content to justify an accordion header/chevron.
        # Strip the header and force the body open so it reads as a plain
        # section instead of a collapsible one; other modes keep the normal
        # accordion since they still have two panels worth collapsing.
        "var scalePanel=wrap.querySelector('.accordion-item[data-value=\"Scale\"]');",
        "if(scalePanel){",
        "scalePanel.classList.toggle('viz-accordion-flat',m==='Choropleth');",
        "if(m==='Choropleth'){",
        "var sc=scalePanel.querySelector('.accordion-collapse');",
        "if(sc)sc.classList.add('show');",
        "var sb=scalePanel.querySelector('.accordion-button');",
        "if(sb){sb.classList.remove('collapsed');sb.setAttribute('aria-expanded','true');}",
        "}",
        "}",
        # Choropleth's legend (isolate count) is always numeric; within Markers
        # mode, the legend is only numeric when the resolved scale type isn't
        # a plain factor scale.
        "var digitsEl=document.getElementById(digitsId);",
        "if(digitsEl){digitsEl.style.display=(m==='Markers'&&curScaleType()==='Factor')?'none':'';}",
        "}",
        "$(document).on('change',modeSel+','+scaleTypeSel,apply);",
        "var n=0,t=setInterval(function(){n++;if(document.querySelector(modeSel)){apply();}if(n>40){clearInterval(t);}},300);",
        "})();"
      )
    })))
  )
}

#' @export
ui <- function(id, generate_id) {
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
  session_reset = shiny$reactive(0L),
  viz_metadata = shiny$reactive(NULL),
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

    # Geocoding success/failure summary for the currently generated map (see
    # build_map_coords()'s status list) — drives output$map_geocode_status_ui
    # below, shown under the "Map mode" picker in this engine's own sidebar.
    map_geocode_status <- shiny$reactiveVal(NULL)

    # How many isolates the last Generate was able to place on the map, with a
    # warning + a few example place names when some couldn't be resolved. NULL
    # (the "Not available" placeholder) until a Generate has produced a status.
    output$map_geocode_status_ui <- shiny$renderUI({
      not_available <- shiny$div(class = "text-muted small", "Not available")
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
        class = "small text-muted",
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
          tiles = input$map_tiles %||% "OpenStreetMap",
          scalebar = isTRUE(input$map_scalebar),
          minimap = isTRUE(input$map_minimap),
          graticule = isTRUE(input$map_graticule),
          show_controls = isTRUE(input$map_show_controls),
          radius = input$map_radius %||% 10,
          opacity = input$map_opacity %||% 1,
          marker_color = input$map_marker_color %||% "#2c7fb8",
          stroke_color = input$map_stroke_color %||% "#333333",
          weight = input$map_weight %||% 0,
          color_var = isTRUE(input$map_color_var),
          col_var = input$map_col_var,
          col_scale = input$map_col_scale %||% "viridis",
          scale_type = input$map_scale_type %||% "Auto",
          bins = input$map_bins %||% 5,
          reverse = isTRUE(input$map_reverse),
          na_color = input$map_na_color %||% "#808080",
          legend = isTRUE(input$map_legend),
          legend_pos = input$map_legend_pos %||% "bottomright",
          legend_title = input$map_legend_title %||% "",
          legend_opacity = input$map_legend_opacity %||% 0.85,
          legend_digits = input$map_legend_digits %||% 2,
          popup_fields = input$map_popup,
          hover_field = input$map_hover_field,
          permanent = isTRUE(input$map_permanent),
          label_size = input$map_label_size %||% 12,
          daterange = input$map_daterange,
          show_time_label = isTRUE(input$map_show_time_label),
          cluster = (input$map_cluster_zoom_level %||% 7) > 0,
          cluster_radius = input$map_cluster_radius %||% 80,
          zoom_to_bounds = isTRUE(input$map_zoom_to_bounds),
          spiderfy = isTRUE(input$map_spiderfy),
          coverage = isTRUE(input$map_coverage),
          cluster_zoom_level = {
            lvl <- input$map_cluster_zoom_level %||% 7
            if (lvl <= 0) NA_integer_ else as.integer(lvl)
          },
          region_transform = input$map_region_transform %||% "Raw",
          region_fixed_scale = isTRUE(input$map_region_fixed_scale),
          region_opacity = input$map_region_opacity %||% 0.7,
          region_border = input$map_region_border %||% "#ffffff",
          region_permanent = isTRUE(input$map_region_permanent),
          region_label_nonzero = isTRUE(input$map_region_label_nonzero),
          heat_radius = input$map_heat_radius %||% 25,
          heat_max = input$map_heat_max %||% 10,
          heat_scale = input$map_heat_scale %||% "viridis",
          chart_type = input$map_chart_type %||% "pie",
          chart_var = input$map_chart_var,
          chart_scale = input$map_chart_scale %||% "Set1",
          chart_size = input$map_chart_size %||% 40,
          chart_opacity = input$map_chart_opacity %||% 1,
          chart_cluster = isTRUE(input$map_chart_cluster),
          chart_cluster_radius = input$map_chart_cluster_radius %||% 100
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
            leafletOutput(ns("map"), height = "100%"),
            class = "viz-map-canvas"
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

    # Restrict map_col_scale's choices to whichever color_scales categories
    # actually suit the data currently driving it: isolate counts for
    # Choropleth (always numeric, never diverging — a qualitative or diverging
    # palette doesn't read well as a single ordered count), or the selected
    # Markers-mode variable resolved the same way make_palette() itself would
    # (via the shared resolve_scale_type()), so the dropdown and the renderer
    # can never disagree about what a variable "is". Heatmap/Charts never show
    # this selector at all (see the client-side tabsByMode script above), so
    # they fall through to the "nothing pickable" branch, which is harmless.
    shiny$observeEvent(
      list(
        input$map_mode,
        input$map_col_var,
        input$map_scale_type,
        map_coords()
      ),
      {
        mode <- input$map_mode %||% "Markers"
        coords <- map_coords()
        vals <- if (identical(mode, "Choropleth")) {
          shiny$req(coords)
          as.numeric(table(coords$geo_loc_name_country))
        } else if (
          identical(mode, "Markers") &&
            isTRUE(input$map_col_var %in% names(coords))
        ) {
          shiny$req(coords)
          coords[[input$map_col_var]]
        } else {
          character(0)
        }
        scale_type_in <- if (identical(mode, "Choropleth")) {
          "Numeric"
        } else {
          input$map_scale_type
        }
        resolved <- resolve_scale_type(scale_type_in, vals)
        cats <- if (!length(vals)) {
          names(color_scales)
        } else {
          suitable_scale_categories(resolved, vals)
        }
        sel <- if (
          isTRUE(
            input$map_col_scale %in%
              unlist(color_scales[cats], use.names = FALSE)
          )
        ) {
          input$map_col_scale
        } else {
          color_scales[[cats[1]]][1]
        }
        updatePickerInput(
          session,
          "map_col_scale",
          choices = color_scales[cats],
          selected = sel
        )
      },
      ignoreInit = TRUE
    )

    # Geocode + populate the metadata-backed selects and the date slider, only
    # when Map is the active engine and Generate is clicked (mirrors the MST/Tree
    # guard). The heavy geocoding is covered by the waiter spinner.
    # map_col_var/map_chart_var/map_popup/map_hover_field's *choices* are all
    # swapped out at Generate time for the loaded database's actual metadata
    # columns (see below) — the UI-declared choices (NULL, for the selects;
    # empty, for the pickers) are just placeholders shown before any data is
    # loaded. shinyjs::reset() only knows how to restore the selected *value*
    # it captured at page load (back when those placeholder choices were
    # still current, i.e. empty); it never restores `choices`, so after
    # Generate has swapped them out, that captured value usually isn't even
    # among the current options any more, leaving the control visibly blank
    # instead of at any real value. force_default = TRUE (Reset settings)
    # always jumps to the same default Generate would use for metadata it's
    # never seen a selection for; force_default = FALSE (Generate) keeps the
    # current selection when it's still valid, so re-Generating doesn't
    # clobber a deliberate user choice.
    populate_metadata_selects <- function(force_default = FALSE) {
      meta <- viz_metadata()
      if (is.null(meta) || !nrow(meta)) {
        return(invisible(NULL))
      }
      fields <- setdiff(names(meta), "isolate")
      if (!length(fields)) {
        return(invisible(NULL))
      }

      keep <- function(id, choices, default) {
        updatePickerInput(
          session,
          id,
          choices = choices,
          selected = if (!force_default && isTRUE(input[[id]] %in% choices)) {
            input[[id]]
          } else {
            default
          }
        )
      }
      keep("map_col_var", fields, fields[1])
      keep(
        "map_chart_var",
        fields,
        if ("specimen_source_id" %in% fields) {
          "specimen_source_id"
        } else {
          fields[1]
        }
      )

      # Popup fields: every metadata column (plus the synthetic geocoded
      # "place"), fetched programmatically the same way as the selects above.
      popup_ids <- unique(c("isolate", "place", fields))
      popup_choices <- stats::setNames(
        popup_ids,
        vapply(popup_ids, field_label, character(1))
      )
      default_popup <- intersect(
        c("isolate", "place", "sample_collection_date"),
        popup_ids
      )
      prev_popup <- if (force_default) {
        default_popup
      } else {
        intersect(input$map_popup %||% default_popup, popup_ids)
      }
      if (!length(prev_popup)) {
        prev_popup <- default_popup
      }
      updatePickerInput(
        session,
        "map_popup",
        choices = popup_choices,
        selected = prev_popup
      )

      # Hover field(s): same programmatically-fetched field set and labels as
      # the popup picker, but multi-select so several fields can combine into
      # one hover tooltip (see build_popup(), reused for the label content).
      hover_ids <- unique(c("isolate", fields))
      hover_choices <- stats::setNames(
        hover_ids,
        vapply(hover_ids, field_label, character(1))
      )
      prev_hover <- if (force_default) {
        "isolate"
      } else {
        intersect(input$map_hover_field %||% "isolate", hover_ids)
      }
      if (!length(prev_hover)) {
        prev_hover <- "isolate"
      }
      updatePickerInput(
        session,
        "map_hover_field",
        choices = hover_choices,
        selected = prev_hover
      )
    }

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
        # Fit the heatmap's "Max intensity" to the busiest place's isolate
        # count, so the slider spans a range where changing it is actually
        # visible instead of being saturated red everywhere.
        max_per_place <- max(table(coords$place))
        shiny$updateSliderInput(
          session,
          "map_heat_max",
          max = max(max_per_place, 10),
          value = max_per_place
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

    # HTML export: rebuild the current map with the shared builder and serialise
    # it as a self-contained interactive HTML file (downloadButton triggers the
    # browser download directly).
    output$map_html <- shiny$downloadHandler(
      filename = function() paste0(Sys.Date(), "_map.html"),
      content = function(file) {
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
      }
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
    # re-Generate needed). Local to this module (see the "Reset settings"
    # button above) — no confirmation modal, mirroring the directness of the
    # "Reset view" button.
    #
    # shinyjs::reset() alone is not enough here, for four reasons:
    #  - map_daterange and map_col_var/map_chart_var/map_popup/
    #    map_hover_field *are* plain <select>s / a sliderInput (a pickerInput
    #    is a <select> underneath) that shinyjs::reset() recognizes and
    #    restores — but only asynchronously (it round-trips through the
    #    browser to read back each resettable element's page-load value
    #    before calling the matching update*Input() on the server). A
    #    same-tick call right after shinyjs::reset() would send the correct,
    #    data-fitted values *first*, and shinyjs's own (stale, pre-Generate)
    #    restoration would land *after* it and overwrite it — e.g.
    #    map_daterange would flash the right dates, then silently revert to
    #    its literal as-coded HTML default (Sys.Date()-30 .. Sys.Date()),
    #    which both breaks the slider's date formatting client-side (it shows
    #    the raw millisecond value) and, worse, makes filter_coords() exclude
    #    all of the real data, so every marker vanishes until Generate is
    #    clicked again. Deferring past that round-trip (typically well under
    #    100ms locally) via shinyjs::delay() guarantees these run last and
    #    win — see reset_data_fitted_controls() below.
    #  - colorPickr (map_marker_color/map_stroke_color/map_region_border/
    #    map_na_color) is a custom JS-rendered widget shinyjs::reset() doesn't
    #    even recognize — see reset_viz_colors().
    #  - radioGroupButtons (map_interval) IS recognized by shinyjs::reset(),
    #    but it then calls shiny::updateRadioButtons(), which the widget's
    #    own JS binding silently ignores — see reset_viz_radio_buttons().
    #    map_cluster_zoom_level is a plain sliderInput now (like
    #    map_cluster_radius above), so the blanket reset already handles it.
    # All four are patched up explicitly below, right after the blanket reset.
    reset_data_fitted_controls <- function() {
      populate_metadata_selects(force_default = TRUE)

      coords <- map_coords()
      if (!is.null(coords) && nrow(coords) > 0) {
        dts <- suppressWarnings(as.Date(coords$sample_collection_date))
        if (any(!is.na(dts))) {
          daterange_bounds(range(dts, na.rm = TRUE))
          shiny$updateSliderInput(
            session,
            "map_daterange",
            min = min(dts, na.rm = TRUE),
            max = max(dts, na.rm = TRUE),
            value = range(dts, na.rm = TRUE)
          )
        }
      }
    }

    # Restore every sidebar control to its coded default. Shared by this
    # engine's own "Reset settings" button and the top-level app-reset
    # (session_reset) path above, so both routes return the controls
    # identically.
    reset_map_settings <- function() {
      anim_playing(FALSE)
      anim_idx(0L)
      shinyjs::reset(id = "controls_wrap")

      reset_viz_colors(
        session,
        map_marker_color = "#2c7fb8",
        map_stroke_color = "#333333",
        map_region_border = "#ffffff",
        map_na_color = "#808080"
      )
      reset_viz_radio_buttons(
        session,
        map_interval = "Day"
      )
      shinyjs::delay(400, reset_data_fitted_controls())
    }

    shiny$observeEvent(input$reset_settings, reset_map_settings())

    # Keep the wrapper output reactive while hidden: the Map engine's panel is
    # display:none-hidden by navset_hidden whenever another engine is active,
    # and by default Shiny would suspend an output bound to a hidden element —
    # this ensures the prompt/map wrapper still renders on the very first
    # switch to the Map tab. output$map itself is deliberately left to the
    # default suspend-when-hidden behavior (see its own comment above).
    shiny$outputOptions(output, "plot_area", suspendWhenHidden = FALSE)

    # ---- Dashboard "Save Analysis" contract ---------------------------------
    snapshot <- shiny$reactive(collect_input_snapshot(input, "map_"))

    restore <- function(vals) {
      apply_input_snapshot(
        session,
        vals,
        switches = c(
          "map_color_var",
          "map_coverage",
          "map_graticule",
          "map_legend",
          "map_minimap",
          "map_permanent",
          "map_reverse",
          "map_scalebar",
          "map_show_controls",
          "map_spiderfy",
          "map_chart_cluster",
          "map_region_fixed_scale",
          "map_region_label_nonzero",
          "map_region_permanent",
          "map_show_time_label",
          "map_zoom_to_bounds"
        ),
        selects = c(
          "map_mode",
          "map_tiles",
          "map_legend_pos",
          "map_chart_type",
          "map_col_scale",
          "map_chart_scale",
          "map_heat_scale"
        ),
        sliders = c(
          "map_radius",
          "map_opacity",
          "map_weight",
          "map_bins",
          "map_legend_opacity",
          "map_label_size",
          "map_cluster_radius",
          "map_cluster_zoom_level",
          "map_region_opacity",
          "map_heat_radius",
          "map_heat_max",
          "map_chart_size",
          "map_chart_opacity",
          "map_chart_cluster_radius"
        ),
        numerics = "map_legend_digits",
        texts = "map_legend_title",
        colors = c(
          "map_marker_color",
          "map_stroke_color",
          "map_na_color",
          "map_region_border"
        ),
        radio_groups = "map_interval"
      )

      # Base radioButtons (scale mode / region transform).
      if (!is.null(vals$map_scale_type)) {
        shiny$updateRadioButtons(
          session,
          "map_scale_type",
          selected = vals$map_scale_type
        )
      }
      if (!is.null(vals$map_region_transform)) {
        shiny$updateRadioButtons(
          session,
          "map_region_transform",
          selected = vals$map_region_transform
        )
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

      # Metadata-backed selects / pickers (mirror populate_metadata_selects()).
      meta <- viz_metadata()
      if (!is.null(meta) && nrow(meta)) {
        fields <- setdiff(names(meta), "isolate")
        if (length(fields)) {
          if (!is.null(vals$map_col_var)) {
            updatePickerInput(
              session,
              "map_col_var",
              choices = fields,
              selected = vals$map_col_var
            )
          }
          if (!is.null(vals$map_chart_var)) {
            updatePickerInput(
              session,
              "map_chart_var",
              choices = fields,
              selected = vals$map_chart_var
            )
          }
          popup_ids <- unique(c("isolate", "place", fields))
          popup_choices <- stats::setNames(
            popup_ids,
            vapply(popup_ids, field_label, character(1))
          )
          if (!is.null(vals$map_popup)) {
            updatePickerInput(
              session,
              "map_popup",
              choices = popup_choices,
              selected = intersect(unlist(vals$map_popup), popup_ids)
            )
          }
          hover_ids <- unique(c("isolate", fields))
          hover_choices <- stats::setNames(
            hover_ids,
            vapply(hover_ids, field_label, character(1))
          )
          if (!is.null(vals$map_hover_field)) {
            updatePickerInput(
              session,
              "map_hover_field",
              choices = hover_choices,
              selected = intersect(unlist(vals$map_hover_field), hover_ids)
            )
          }
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
      thumb_data = shiny$reactive(input$thumb_data)
    )
  })
}
