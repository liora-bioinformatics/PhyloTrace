# dev/build_admin1.R
#
# Builds the state/province (admin-1) polygons the Map's choropleth draws at
# "State / province" level, from Natural Earth's 1:10m admin-1 table (public
# domain), simplified so the bundled file stays a few MB.
#
# Output: app/logic/data/admin1.rds, an sf object in EPSG:4326, one row per
# Natural Earth admin-1 unit:
#   adm1_code   region key, unique per row (what isolates are assigned to)
#   name        display name: Natural Earth `name_en`, else `name`
#   adm0_a3     parent unit, the code the country choropleth joins on
#   country     parent display name: the app's canonical country name
#               (app/logic/data/countries.csv), else admin-1's own `admin`
#   iso_3166_2  ISO 3166-2 code, empty where Natural Earth has none
#
# Parent codes are checked against the 1:50m admin-0 units the country
# choropleth draws, and the result is printed. Findings (Natural Earth 5.1.1):
# - Every 1:50m admin-0 unit has admin-1 coverage.
# - Nine admin-1 parents are 1:10m-only units with no 1:50m polygon: the UK
#   bases on Cyprus (ESB, WSB), Guantanamo (USG), Gibraltar (GIB), Baikonur
#   (KAB), the US minor islands (UMI), Coral Sea Islands (CSI), the Spratlys
#   (PGA) and Clipperton (CLP). They keep their own code: folding them into
#   the unit their land sits in (Spain, Cuba, Kazakhstan...) would count an
#   isolate under the wrong sovereign. Labelled by admin-1's `admin`.
# - France's overseas departments (French Guiana, Reunion, ...) carry FRA, as
#   they do at admin-0, so they draw as part of France.
# - About 90 small-island regions lie off their parent's 1:50m outline only
#   because 1:50m drops those islands; they keep their parent code. Melilla,
#   Brando (Aland) and Redonda sit nearer a neighbour's outline for the same
#   reason and keep theirs too.
#
# Simplification uses rmapshaper (topology-preserving) when installed, and
# otherwise sf's s2 simplification, which simplifies each region on its own and
# so can leave hairline gaps between neighbours; points falling into one are
# snapped to the nearest region by the app's tolerance. Regions too small to
# survive simplification keep their original outline.
#
# Licence: Natural Earth data is in the public domain (naturalearthdata.com
# terms of use), so the simplified derivative may be bundled with the GPL-3
# app. Credit, though not required: "Made with Natural Earth".
#
# Run from the project root:  Rscript dev/build_admin1.R

box::use(
  rnaturalearth[ne_countries],
  sf,
  utils[download.file, read.csv, unzip],
)

src_url <- paste0(
  "https://naciscdn.org/naturalearth/10m/cultural/",
  "ne_10m_admin_1_states_provinces.zip"
)
out_path <- "app/logic/data/admin1.rds"
countries_path <- "app/logic/data/countries.csv"

# Simplification tolerance (metres) and stored coordinate precision (1e4 =
# 4 decimals, about 11 m): finer than a city-centre geocode, coarse enough to
# keep the file small.
tolerance_m <- 1000
precision <- 1e4

# How far (metres) a region may lie outside its parent's 1:50m outline before
# it is reported, matching the app's COUNTRY_TOLERANCE_M.
report_tolerance_m <- 25000

sf$sf_use_s2(TRUE)

dl_dir <- tempfile("ne_admin1_")
dir.create(dl_dir)
zip_path <- file.path(dl_dir, "admin1.zip")
download.file(src_url, zip_path, mode = "wb", quiet = TRUE)
unzip(zip_path, exdir = dl_dir)
raw <- sf$st_read(
  file.path(dl_dir, "ne_10m_admin_1_states_provinces.shp"),
  quiet = TRUE
)
stopifnot(!anyDuplicated(raw$adm1_code), !anyNA(raw$adm1_code))

# Natural Earth writes "-99" where a code does not exist.
code_or_empty <- function(x) ifelse(is.na(x) | x == "-99", "", x)

# First non-blank value of each row across the given columns.
first_filled <- function(...) {
  out <- rep(NA_character_, length(..1))
  for (v in list(...)) {
    v <- trimws(as.character(v))
    fill <- is.na(out) & !is.na(v) & nzchar(v)
    out[fill] <- v[fill]
  }
  out
}

vocab <- read.csv(countries_path, stringsAsFactors = FALSE, encoding = "UTF-8")
admin1 <- raw[, c("adm1_code", "adm0_a3", "iso_3166_2")]
admin1$name <- first_filled(raw$name_en, raw$name, "Unnamed area")
admin1$country <- first_filled(
  vocab$name[match(raw$adm0_a3, vocab$code)],
  raw$admin,
  raw$adm0_a3
)
admin1$iso_3166_2 <- code_or_empty(admin1$iso_3166_2)
admin1 <- admin1[, c("adm1_code", "name", "adm0_a3", "country", "iso_3166_2")]

# --- Parent codes against the country choropleth's admin-0 units ------------

countries50 <- ne_countries(scale = 50, returnclass = "sf")
admin0_codes <- countries50$adm0_a3
no_admin0 <- setdiff(unique(admin1$adm0_a3), admin0_codes)
cat("Admin-1 parents with no 1:50m admin-0 unit (kept under their own code):\n")
for (code in no_admin0) {
  cat(sprintf(
    "  %s  %s (%d region%s)\n",
    code,
    admin1$country[match(code, admin1$adm0_a3)],
    sum(admin1$adm0_a3 == code),
    if (sum(admin1$adm0_a3 == code) == 1) "" else "s"
  ))
}
uncovered <- setdiff(admin0_codes, admin1$adm0_a3)
cat(sprintf(
  "Admin-0 units without admin-1 regions: %s\n",
  if (length(uncovered)) paste(uncovered, collapse = ", ") else "none"
))

# Regions whose interior point lies outside every 1:50m unit (islands 1:50m
# leaves out), or only near a different unit than their parent.
inner <- suppressWarnings(sf$st_point_on_surface(sf$st_make_valid(sf$st_geometry(raw))))
near <- sf$st_is_within_distance(inner, countries50, dist = report_tolerance_m)
fit <- vapply(seq_along(near), function(i) {
  units <- admin0_codes[near[[i]]]
  if (!length(units)) {
    "none"
  } else if (admin1$adm0_a3[i] %in% units) {
    "parent"
  } else {
    paste(units, collapse = "/")
  }
}, character(1))
cat(sprintf(
  "Regions on their parent's 1:50m outline: %d; off every 1:50m unit: %d\n",
  sum(fit == "parent"),
  sum(fit == "none")
))
other <- !fit %in% c("parent", "none")
cat("Regions that sit on a different 1:50m unit than their parent:\n")
for (i in which(other)) {
  cat(sprintf(
    "  %-10s %-30s parent %s, lies on %s\n",
    admin1$adm1_code[i], admin1$name[i], admin1$adm0_a3[i], fit[i]
  ))
}

# --- Simplification ---------------------------------------------------------

admin1 <- sf$st_make_valid(admin1)
simplified <- if (requireNamespace("rmapshaper", quietly = TRUE)) {
  box::use(rmapshaper[ms_simplify], )
  cat("Simplifying with rmapshaper\n")
  ms_simplify(admin1, keep = 0.05, keep_shapes = TRUE)
} else {
  cat(sprintf("rmapshaper not installed: s2 simplification at %g m\n", tolerance_m))
  sf$st_simplify(admin1, dTolerance = tolerance_m)
}
collapsed <- sf$st_is_empty(simplified)
outlines <- sf$st_geometry(simplified)
outlines[collapsed] <- sf$st_geometry(admin1)[collapsed]
simplified <- sf$st_set_geometry(simplified, outlines)
cat(sprintf("Regions kept unsimplified (too small to survive): %d\n", sum(collapsed)))

# Repair can turn a region into a collection holding stray lines or points;
# only its polygons are kept.
simplified <- sf$st_make_valid(sf$st_set_precision(simplified, precision))
if (any(sf$st_geometry_type(simplified) == "GEOMETRYCOLLECTION")) {
  simplified <- sf$st_collection_extract(simplified, "POLYGON", warn = FALSE)
}
simplified <- sf$st_cast(simplified, "MULTIPOLYGON")
stopifnot(
  nrow(simplified) == nrow(raw),
  identical(simplified$adm1_code, admin1$adm1_code),
  !any(sf$st_is_empty(simplified)),
  all(sf$st_is_valid(simplified))
)

saveRDS(simplified, out_path, compress = "xz")
cat(sprintf(
  "Wrote %d regions to %s (%.2f MB)\n",
  nrow(simplified),
  out_path,
  file.size(out_path) / 1e6
))
