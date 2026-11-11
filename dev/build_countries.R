# dev/build_countries.R
#
# Builds the country vocabulary the app stores in `geo_loc_name_country` from
# the Natural Earth 1:50m admin-0 table, the same polygons the Map's
# choropleth draws, so every canonical name has exactly one polygon.
#
# Output: app/logic/data/countries.csv with one row per Natural Earth unit:
#   code     adm0_a3, unique per row (the choropleth join key)
#   name     canonical display name (Natural Earth `admin`, or `display_names`)
#   iso2     ISO 3166-1 alpha-2, empty where Natural Earth has none
#   iso3     ISO 3166-1 alpha-3, empty where Natural Earth has none
#   aliases  every other spelling the app accepts, "|"-separated, raw (the app
#            folds case, accents and punctuation itself when it loads them)
#
# Run from the project root:  Rscript dev/build_countries.R

library(rnaturalearthdata)
library(sf)

out_path <- "app/logic/data/countries.csv"

world <- st_drop_geometry(countries50)

# Disputed or uninhabited zones Natural Earth draws as their own unit but no
# isolate is sampled from. Left out of the vocabulary (and so the picker); the
# choropleth still draws their polygon, uncounted.
drop_codes <- c(
  KAS = "Siachen Glacier",
  ATC = "Ashmore and Cartier Islands"
)
world <- world[!world$adm0_a3 %in% names(drop_codes), , drop = FALSE]

# Display names where Natural Earth's `admin` reads oddly or is not the short
# name in common use. The Natural Earth name stays an accepted alias, so stored
# values written under it are rewritten to these on the next database load.
display_names <- c(
  ALD = "Åland Islands",
  BHS = "Bahamas",
  BLM = "Saint Barthélemy",
  CIV = "Côte d'Ivoire",
  HKG = "Hong Kong",
  MAC = "Macao",
  SGS = "South Georgia and the South Sandwich Islands",
  SRB = "Serbia",
  STP = "São Tomé and Príncipe",
  SWZ = "Eswatini",
  TLS = "Timor-Leste",
  TZA = "Tanzania"
)

# English spellings in common use that Natural Earth does not carry, by code.
# Constituent countries of the UK are deliberately absent: mapping "England"
# onto "United Kingdom" would silently drop information the user typed.
extra_aliases <- list(
  USA = c("USA", "U.S.A.", "US", "U.S.", "United States", "America"),
  GBR = c("UK", "U.K.", "Great Britain", "Britain"),
  ARE = c("UAE", "Emirates"),
  CZE = c("Czech Republic"),
  CIV = c("Cote d'Ivoire", "Ivory Coast"),
  COD = c("DR Congo", "DRC", "Congo-Kinshasa", "Congo (Kinshasa)"),
  COG = c("Congo-Brazzaville", "Congo (Brazzaville)"),
  KOR = c("Korea", "Korea, Republic of", "Republic of Korea"),
  PRK = c("DPRK", "Korea, Democratic People's Republic of"),
  RUS = c("Russian Federation"),
  IRN = c("Iran, Islamic Republic of"),
  SYR = c("Syrian Arab Republic"),
  LAO = c("Lao PDR", "Lao People's Democratic Republic"),
  VNM = c("Viet Nam"),
  TUR = c("Türkiye", "Turkiye"),
  NLD = c("Holland", "The Netherlands"),
  MKD = c("Macedonia", "FYROM"),
  SWZ = c("Swaziland"),
  MMR = c("Burma"),
  CPV = c("Cape Verde"),
  TLS = c("East Timor", "Timor Leste"),
  BOL = c("Bolivia, Plurinational State of"),
  VEN = c("Venezuela, Bolivarian Republic of"),
  TZA = c("Tanzania, United Republic of"),
  MDA = c("Moldova, Republic of"),
  PSE = c("Palestine, State of")
)

# Natural Earth writes "-99" where a code does not exist.
code_or_empty <- function(x) ifelse(is.na(x) | x == "-99", "", x)

alias_cols <- c(
  "name", "name_long", "admin", "geounit", "subunit", "brk_name",
  "name_sort", "name_alt", "formal_en", "formal_fr", "abbrev",
  setdiff(grep("^name_[a-z]{2,3}$", names(world), value = TRUE), "name_len")
)
alias_cols <- intersect(alias_cols, names(world))

rows <- lapply(seq_len(nrow(world)), function(i) {
  r <- world[i, ]
  code <- r$adm0_a3
  name <- if (code %in% names(display_names)) display_names[[code]] else r$admin
  vals <- c(
    unlist(r[alias_cols], use.names = FALSE),
    code_or_empty(r$iso_a2_eh), code_or_empty(r$iso_a3_eh), code,
    extra_aliases[[code]]
  )
  vals <- unique(trimws(vals[!is.na(vals) & nzchar(trimws(vals))]))
  data.frame(
    code = code,
    name = name,
    iso2 = code_or_empty(r$iso_a2_eh),
    iso3 = code_or_empty(r$iso_a3_eh),
    aliases = paste(setdiff(vals, name), collapse = "|"),
    stringsAsFactors = FALSE
  )
})
out <- do.call(rbind, rows)
out <- out[order(out$name), , drop = FALSE]

# An alias claimed by two units, or equal to another unit's canonical name
# (a dependency spelled like its sovereign), identifies neither: drop it, so
# the lookup never has to pick one. Compared case-insensitively, as the app
# matches them.
alias_list <- strsplit(out$aliases, "|", fixed = TRUE)
owner <- rep(out$code, lengths(alias_list))
key <- tolower(unlist(alias_list))
claimed <- tapply(owner, key, function(x) length(unique(x)))
ambiguous <- c(names(claimed)[claimed > 1], tolower(out$name))
out$aliases <- vapply(seq_along(alias_list), function(i) {
  a <- alias_list[[i]]
  mine <- tolower(a) %in% tolower(out$name[i])
  paste(a[!(tolower(a) %in% ambiguous) | mine], collapse = "|")
}, character(1))

stopifnot(!anyDuplicated(out$code), !anyDuplicated(out$name))
write.csv(out, out_path, row.names = FALSE, fileEncoding = "UTF-8")
cat(sprintf("Wrote %d countries to %s\n", nrow(out), out_path))
