# app/logic/geo.R
#
# Geographic vocabulary and parsing shared by the metadata grid, the table
# import, the database load and the Map: the controlled country list (Natural
# Earth admin-0 units, see dev/build_countries.R) and free-form coordinates.

box::use(
  stats[setNames],
  stringi[stri_replace_all_regex, stri_trans_general, stri_trans_nfc, stri_trans_tolower],
  utils[read.csv],
)

#' Labels of the Geographic Field Types
#'
#' Type slugs the fixed schema gives its country and coordinate columns, with
#' the label a rejection notice names them by.
#' @export
GEO_TYPE_LABELS <- c(country = "Country", coordinates = "Coordinates")

# Natural Earth admin-0 vocabulary, read once per process. It is shipped with
# the app, never derived from a database, so it needs no session teardown.
.vocab <- new.env(parent = emptyenv())

# Folds a spelling to its lookup key: accents, case, periods and apostrophes
# dropped, any other punctuation a single space, and a leading "the" ignored,
# so "U.S.A.", "usa" and "USA" or "Côte d'Ivoire" and "Cote dIvoire" meet.
.country_key <- function(x) {
  k <- stri_trans_tolower(stri_trans_general(stri_trans_nfc(as.character(x)), "Latin-ASCII"))
  k <- stri_replace_all_regex(k, "[.'’`]", "")
  k <- stri_replace_all_regex(k, "[^\\p{L}\\p{N}]+", " ")
  k <- trimws(k)
  sub("^the ", "", k)
}

# The vocabulary table plus its key -> code index, loaded on first use.
.countries <- function() {
  if (is.null(.vocab$table)) {
    tab <- read.csv(
      box::file("data", "countries.csv"),
      stringsAsFactors = FALSE,
      na.strings = character(0),
      encoding = "UTF-8"
    )
    spellings <- strsplit(tab$aliases, "|", fixed = TRUE)
    keys <- .country_key(c(tab$name, unlist(spellings)))
    codes <- c(tab$code, rep(tab$code, lengths(spellings)))
    # Canonical names come first, so a name always wins over an alias that
    # happens to fold to the same key.
    keep <- nzchar(keys) & !duplicated(keys)
    .vocab$index <- setNames(codes[keep], keys[keep])
    .vocab$table <- tab
  }
  .vocab
}

#' Canonical Country Names
#'
#' @return Character vector of every name `geo_loc_name_country` accepts as
#'   stored, sorted alphabetically.
#' @export
country_names <- function() {
  sort(.countries()$table$name)
}

#' Look Up the Natural Earth Code of Country Spellings
#'
#' @param x Character vector of country names, aliases (any Natural Earth
#'   language), or ISO 3166-1 alpha-2/alpha-3 codes.
#' @return Character vector of Natural Earth `adm0_a3` codes, `NA` where `x`
#'   is missing, blank or not recognised.
#' @export
country_code <- function(x) {
  v <- .countries()
  out <- unname(v$index[.country_key(x)])
  out[is.na(x)] <- NA_character_
  out
}

#' Normalise Country Spellings to Their Canonical Name
#'
#' @param x Character vector of country spellings.
#' @return Character vector of canonical names, `NA` where `x` is not
#'   recognised.
#' @export
match_country <- function(x) {
  tab <- .countries()$table
  tab$name[match(country_code(x), tab$code)]
}

#' Validate One Country Value
#'
#' @param v Trimmed, non-empty character scalar.
#' @return List with `ok`, `value` (the canonical name) and `reason`.
#' @export
coerce_country <- function(v) {
  hit <- match_country(v)
  if (is.na(hit)) {
    return(list(
      ok = FALSE,
      value = NA_character_,
      reason = "Not a recognised country. Pick one from the list or use its ISO code."
    ))
  }
  list(ok = TRUE, value = hit, reason = "")
}

# --- coordinates ---------------------------------------------------------------

# One coordinate component, number-like tokens and the markers around them.
.COORD_TOKEN <- paste0(
  "[+-]?(?:[0-9]+(?:\\.[0-9]*)?|\\.[0-9]+)",
  "|°|'|\"|[NSEWnsew]|[,;]|\\S"
)

# Unifies the many glyphs people type for degrees, minutes and seconds, and
# turns decimal commas into points when the value is unambiguous about it:
# no point anywhere, and exactly two digit,digit runs ("52,52 13,40").
.coord_prepare <- function(s) {
  s <- stri_trans_nfc(s)
  s <- gsub("[′‘’`´]", "'", s)
  s <- gsub("[″“”]|''", "\"", s)
  s <- gsub("[º˚]|\\bdeg\\b", "°", s, perl = TRUE)
  decimal_commas <- !grepl(".", s, fixed = TRUE) &&
    lengths(gregexpr("[0-9],[0-9]", s)) == 2L &&
    grepl("[;[:space:]]", s)
  if (decimal_commas) {
    s <- gsub("([0-9]),([0-9])", "\\1.\\2", s)
  }
  s
}

# Splits a prepared string into coordinate groups, each a list of `nums`, the
# `units` written after them (NA when positional) and an optional hemisphere
# letter `hem`. NULL when a token makes no sense in a coordinate.
.coord_groups <- function(s) {
  tok <- regmatches(s, gregexpr(.COORD_TOKEN, s, perl = TRUE))[[1]]
  unit_of <- c("°" = "d", "'" = "m", "\"" = "s")
  is_num <- grepl("[0-9]", tok)
  is_hem <- grepl("^[NSEWnsew]$", tok)
  if (!all(is_num | is_hem | tok %in% c(names(unit_of), ",", ";"))) {
    return(NULL)
  }
  # Two bare numbers and nothing else: "52.52 13.40".
  if (!any(is_hem | tok %in% c(names(unit_of), ",", ";")) && sum(is_num) == 2L) {
    return(lapply(as.numeric(tok), function(n) list(nums = n, units = NA, hem = NA_character_)))
  }

  groups <- list()
  cur <- list(nums = numeric(0), units = character(0), hem = NA_character_)
  close <- function() {
    if (length(cur$nums)) groups[[length(groups) + 1L]] <<- cur
    cur <<- list(nums = numeric(0), units = character(0), hem = NA_character_)
  }
  for (k in seq_along(tok)) {
    t <- tok[[k]]
    if (t %in% c(",", ";")) {
      close()
    } else if (is_hem[[k]]) {
      letter <- toupper(t)
      if (!length(cur$nums)) {
        cur$hem <- letter
      } else if (is.na(cur$hem)) {
        cur$hem <- letter
        close()
      } else {
        close()
        cur$hem <- letter
      }
    } else if (is_num[[k]]) {
      nxt <- if (k < length(tok)) tok[[k + 1L]] else ""
      unit <- if (nxt %in% names(unit_of)) unit_of[[nxt]] else NA_character_
      # A second degree value starts the next coordinate: 52°31' 13°24'.
      if (identical(unit, "d") && "d" %in% cur$units) {
        close()
      }
      cur$nums <- c(cur$nums, as.numeric(t))
      cur$units <- c(cur$units, unit)
    }
  }
  close()
  groups
}

# Decimal degrees of one group, or NA when its parts do not add up (minutes
# or seconds of 60 or more, a sign on anything but the degrees, or a sign
# contradicting the hemisphere letter).
.coord_value <- function(g) {
  n <- length(g$nums)
  if (n < 1L || n > 3L) {
    return(NA_real_)
  }
  units <- g$units
  units[is.na(units)] <- c("d", "m", "s")[seq_len(n)][is.na(units)]
  if (anyDuplicated(units) || !identical(units, sort(units))) {
    return(NA_real_)
  }
  part <- setNames(g$nums, units)
  deg <- part[["d"]]
  mins <- if ("m" %in% units) part[["m"]] else 0
  secs <- if ("s" %in% units) part[["s"]] else 0
  if (mins < 0 || secs < 0 || mins >= 60 || secs >= 60) {
    return(NA_real_)
  }
  val <- abs(deg) + mins / 60 + secs / 3600
  neg <- deg < 0
  if (!is.na(g$hem)) {
    if (neg) {
      return(NA_real_)
    }
    neg <- g$hem %in% c("S", "W")
  }
  if (neg) -val else val
}

# Parses one coordinate string into list(lat, lon, reason); reason is "" when
# it parsed and a user-facing explanation otherwise.
.parse_one <- function(s) {
  fail <- function(r) list(lat = NA_real_, lon = NA_real_, reason = r)
  groups <- .coord_groups(.coord_prepare(s))
  if (length(groups) != 2L) {
    return(fail("Write latitude, longitude, e.g. 52.52, 13.40."))
  }
  vals <- vapply(groups, .coord_value, numeric(1))
  if (anyNA(vals)) {
    return(fail("Write latitude, longitude, e.g. 52.52, 13.40 or 52°31'N 13°24'E."))
  }
  hems <- vapply(groups, function(g) g$hem, character(1))
  axis <- ifelse(hems %in% c("N", "S"), "lat", ifelse(hems %in% c("E", "W"), "lon", NA))
  if (identical(sum(is.na(axis)), 1L)) {
    axis[is.na(axis)] <- setdiff(c("lat", "lon"), axis[!is.na(axis)])
  }
  if (all(is.na(axis))) {
    axis <- c("lat", "lon")
  }
  if (!setequal(axis, c("lat", "lon"))) {
    return(fail("One value must be north/south, the other east/west."))
  }
  lat <- vals[axis == "lat"]
  lon <- vals[axis == "lon"]
  if (abs(lat) > 90 && abs(lon) <= 90 && all(is.na(hems))) {
    return(fail("Latitude goes first and lies within ±90; these look swapped."))
  }
  if (abs(lat) > 90 || abs(lon) > 180) {
    return(fail("Latitude must lie within ±90 and longitude within ±180."))
  }
  list(lat = lat, lon = lon, reason = "")
}

#' Parse Free-Form Coordinates
#'
#' Reads "latitude, longitude" in decimal degrees (comma, semicolon or space
#' separated, decimal commas included) or in degrees/minutes/seconds with
#' hemisphere letters before or after each value ("52°31'12\"N 13°24'E",
#' "N 52.52 E 13.4"). With letters the order is free; without them latitude
#' comes first.
#'
#' @param x Character vector.
#' @return Data frame with `latitude`, `longitude` (`NA` where unreadable) and
#'   `problem` ("" when parsed or blank, a user-facing reason otherwise), one
#'   row per element of `x`.
#' @export
parse_coordinates <- function(x) {
  x <- trimws(as.character(x))
  n <- length(x)
  out <- data.frame(
    latitude = rep(NA_real_, n),
    longitude = rep(NA_real_, n),
    problem = rep("", n),
    stringsAsFactors = FALSE
  )
  for (i in which(!is.na(x) & nzchar(x))) {
    res <- .parse_one(x[[i]])
    out$latitude[[i]] <- res$lat
    out$longitude[[i]] <- res$lon
    out$problem[[i]] <- res$reason
  }
  out
}

#' Format Decimal Degrees as Stored Coordinates
#'
#' @param lat,lon Numeric vectors of decimal degrees.
#' @return Character vector "lat, lon", at most six decimals (about 0.1 m)
#'   and no trailing zeros.
#' @export
format_coordinates <- function(lat, lon) {
  fmt <- function(v) {
    sub("\\.$", "", sub("0+$", "", formatC(round(v, 6), format = "f", digits = 6)))
  }
  paste0(fmt(lat), ", ", fmt(lon))
}

#' Validate One Coordinate Value
#'
#' @param v Trimmed, non-empty character scalar.
#' @return List with `ok`, `value` (canonical "lat, lon") and `reason`.
#' @export
coerce_coordinates <- function(v) {
  p <- parse_coordinates(v)
  if (nzchar(p$problem[[1]])) {
    return(list(ok = FALSE, value = NA_character_, reason = p$problem[[1]]))
  }
  list(ok = TRUE, value = format_coordinates(p$latitude, p$longitude), reason = "")
}
