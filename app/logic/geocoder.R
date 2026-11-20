# app/logic/geocoder.R
#
# Looks up place names with OpenStreetMap's Nominatim service at most once per second, as its
# usage policy demands, and gives up early when the service cannot be reached, refuses requests
# or answers with something other than results, so a missing or dropped connection never stalls
# the map's Generate for long.

box::use(
  curl[curl_fetch_memory, handle_setheaders, handle_setopt, new_handle, parse_headers_list],
  jsonlite[fromJSON],
)

box::use(
  app / logic / app_meta[APP_VERSION],
  app / logic / urls[WEBSITE, nominatim_search_url],
)

# Longest a single request may take, in seconds, and the part of it allowed for opening the
# connection. A blackholed network (no answer at all) costs a request the full timeout.
REQUEST_TIMEOUT_S <- 15
CONNECT_TIMEOUT_S <- 5

# Nominatim allows one request per second.
MIN_INTERVAL_S <- 1

# Pause before the one retry of a request that failed for a reason other than its answer.
RETRY_PAUSE_S <- 2

# Longest a Retry-After from Nominatim is honoured, in seconds; asked to wait longer, the
# run stops instead of blocking the session.
MAX_RETRY_AFTER_S <- 30

# Nominatim blocks generic library User-Agents; it asks clients to name the application.
USER_AGENT <- sprintf("PhyloTrace/%s (+%s)", APP_VERSION, WEBSITE)

# One lookup's result: `outcome` is "found", "not_found" or a failure ("network",
# "rate_limited", "blocked", "unavailable"), with the failure's `message`.
lookup_result <- function(outcome, latitude = NA_real_, longitude = NA_real_, message = "",
                          retry_after = NA_real_) {
  list(
    outcome = outcome,
    latitude = latitude,
    longitude = longitude,
    message = message,
    retry_after = retry_after
  )
}

#' Look Up One Place with Nominatim
#'
#' @description Sends one search request and classifies the answer. A 200 response that is not
#'   Nominatim's JSON (a captive portal's login page, a proxy's error page) counts as
#'   `"unavailable"`, never as a place that does not exist.
#'
#' @param place Character string, the place to look up.
#' @param timeout_s Longest the request may take, in seconds.
#' @param url Request URL; the Nominatim search for `place` unless given.
#' @return List with `outcome` (`"found"`, `"not_found"`, `"network"`, `"rate_limited"`,
#'   `"blocked"` or `"unavailable"`), `latitude`, `longitude` (`NA` unless found), `message`
#'   (the failure, empty otherwise) and `retry_after` (seconds Nominatim asked to wait, or `NA`).
#' @export
nominatim_lookup <- function(place, timeout_s = REQUEST_TIMEOUT_S,
                             url = nominatim_search_url(place)) {
  handle <- new_handle()
  handle_setopt(
    handle,
    useragent = USER_AGENT,
    timeout = timeout_s,
    connecttimeout = min(CONNECT_TIMEOUT_S, timeout_s),
    followlocation = TRUE
  )
  handle_setheaders(handle, Accept = "application/json")
  res <- tryCatch(
    curl_fetch_memory(url, handle),
    error = function(e) e
  )
  if (inherits(res, "error")) {
    return(lookup_result("network", message = conditionMessage(res)))
  }
  status <- res$status_code
  if (status == 429L) {
    wait <- suppressWarnings(as.numeric(parse_headers_list(res$headers)[["retry-after"]][1]))
    return(lookup_result(
      "rate_limited",
      message = "HTTP 429 Too Many Requests",
      retry_after = if (length(wait)) wait else NA_real_
    ))
  }
  if (status %in% c(401L, 403L)) {
    return(lookup_result("blocked", message = sprintf("HTTP %d", status)))
  }
  if (status != 200L) {
    return(lookup_result("unavailable", message = sprintf("HTTP %d", status)))
  }
  hits <- tryCatch(
    fromJSON(rawToChar(res$content)),
    error = function(e) NULL
  )
  if (is.data.frame(hits) && nrow(hits) && all(c("lat", "lon") %in% names(hits))) {
    lat <- suppressWarnings(as.numeric(hits$lat[1]))
    lon <- suppressWarnings(as.numeric(hits$lon[1]))
    if (is.finite(lat) && is.finite(lon)) {
      return(lookup_result("found", lat, lon))
    }
    return(lookup_result("not_found"))
  }
  # An empty result is "[]", which fromJSON reads as an empty list.
  if (is.list(hits) && !length(hits)) {
    return(lookup_result("not_found"))
  }
  lookup_result("unavailable", message = "the answer was not a Nominatim result")
}

#' Geocode Places One by One
#'
#' @description Looks up each place in turn, never faster than Nominatim's one request per
#'   second. A request that fails for a reason other than its answer is retried once (after the
#'   wait Nominatim asks for, when it asks for one); a second failure, or one refusal, stops the
#'   run, so every remaining place is left unresolved for the next attempt rather than each
#'   waiting out its own timeout. With no connection, a run therefore ends after at most two
#'   requests.
#'
#' @param places Character vector of distinct places.
#' @param progress Optional function called before each place with the number done, the number
#'   to do and the seconds spent so far.
#' @param lookup Function looking up one place, as [nominatim_lookup()].
#' @param wait Function pausing for a number of seconds.
#' @return List with `latitude` and `longitude` (one per place, `NA` where unresolved), `stopped`
#'   (why the run ended early: `""`, `"offline"`, `"connection_lost"`, `"rate_limited"`,
#'   `"blocked"` or `"unavailable"`) and `message` (the last failure's detail).
#' @export
geocode_places <- function(places, progress = NULL, lookup = nominatim_lookup,
                           wait = Sys.sleep) {
  n <- length(places)
  latitude <- rep(NA_real_, n)
  longitude <- rep(NA_real_, n)
  stopped <- ""
  message <- ""
  # Whether Nominatim has answered at all this run: a failure after that is a lost
  # connection, before it an absent one.
  answered <- FALSE
  started <- Sys.time()
  last_request <- NULL

  # Waits out the rest of the minimum interval since the previous request.
  pace <- function() {
    if (!is.null(last_request)) {
      gap <- MIN_INTERVAL_S - as.numeric(difftime(Sys.time(), last_request, units = "secs"))
      if (gap > 0) {
        wait(gap)
      }
    }
    last_request <<- Sys.time()
  }

  for (i in seq_len(n)) {
    if (is.function(progress)) {
      progress(i - 1L, n, as.numeric(difftime(Sys.time(), started, units = "secs")))
    }
    for (attempt in 1:2) {
      pace()
      res <- lookup(places[i])
      if (res$outcome %in% c("found", "not_found", "blocked") || attempt == 2L) {
        break
      }
      pause <- if (identical(res$outcome, "rate_limited")) res$retry_after else RETRY_PAUSE_S
      if (!is.finite(pause) || pause < 0) {
        pause <- RETRY_PAUSE_S
      }
      if (pause > MAX_RETRY_AFTER_S) {
        break
      }
      wait(pause)
    }
    if (identical(res$outcome, "found")) {
      latitude[i] <- res$latitude
      longitude[i] <- res$longitude
      answered <- TRUE
    } else if (identical(res$outcome, "not_found")) {
      answered <- TRUE
    } else {
      stopped <- if (!identical(res$outcome, "network")) {
        res$outcome
      } else if (answered) {
        "connection_lost"
      } else {
        "offline"
      }
      message <- res$message
      break
    }
  }
  list(latitude = latitude, longitude = longitude, stopped = stopped, message = message)
}

#' Why Geocoding Stopped Early
#'
#' @description One sentence for the user on a [geocode_places()] stop reason.
#'
#' @param stopped Stop reason as returned by [geocode_places()].
#' @return Character string; empty for `""`.
#' @export
geocode_stop_text <- function(stopped) {
  switch(
    stopped,
    offline = paste(
      "Nominatim (nominatim.openstreetmap.org) could not be reached. Check the internet",
      "connection, or a proxy or firewall in the way."
    ),
    connection_lost = "The connection to Nominatim was lost during geocoding.",
    rate_limited = "Nominatim asked PhyloTrace to slow down and kept refusing requests.",
    blocked = paste(
      "Nominatim refused the requests. It blocks clients that exceed its usage policy,",
      "sometimes for a few hours."
    ),
    unavailable = paste(
      "Nominatim did not answer properly. The service may be down, or a network login",
      "page or proxy is in the way."
    ),
    ""
  )
}
