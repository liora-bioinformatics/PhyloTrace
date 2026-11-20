# tests/testthat/test-geocoder.R

box::use(
  testthat[expect_equal, expect_identical, expect_match, expect_true, test_that],
)
box::use(
  app / logic / geocoder,
)

impl <- attr(geocoder, "namespace")

# A lookup answering from a queue of outcomes, recording each place it was asked for.
scripted_lookup <- function(outcomes) {
  calls <- character(0)
  fn <- function(place) {
    calls <<- c(calls, place)
    out <- outcomes[[min(length(calls), length(outcomes))]]
    if (identical(out, "found")) {
      return(impl$lookup_result("found", 1, 2))
    }
    if (is.list(out)) {
      return(out)
    }
    impl$lookup_result(out, message = out)
  }
  list(fn = fn, calls = function() calls)
}

no_wait <- function(s) NULL

test_that("every place is looked up when the service answers", {
  look <- scripted_lookup(list("found", "not_found", "found"))
  res <- geocoder$geocode_places(c("a", "b", "c"), lookup = look$fn, wait = no_wait)
  expect_identical(look$calls(), c("a", "b", "c"))
  expect_equal(res$latitude, c(1, NA, 1))
  expect_identical(res$stopped, "")
})

test_that("no connection stops the run after one retry", {
  look <- scripted_lookup(list("network"))
  res <- geocoder$geocode_places(letters, lookup = look$fn, wait = no_wait)
  expect_identical(look$calls(), c("a", "a"))
  expect_identical(res$stopped, "offline")
  expect_true(all(is.na(res$latitude)))
})

test_that("a connection dropping mid-run is reported as lost, keeping earlier results", {
  look <- scripted_lookup(list("found", "network"))
  res <- geocoder$geocode_places(c("a", "b", "c"), lookup = look$fn, wait = no_wait)
  expect_identical(look$calls(), c("a", "b", "b"))
  expect_identical(res$stopped, "connection_lost")
  expect_equal(res$latitude, c(1, NA, NA))
})

test_that("a passing failure is retried once and the run goes on", {
  look <- scripted_lookup(list("network", "found", "found"))
  res <- geocoder$geocode_places(c("a", "b"), lookup = look$fn, wait = no_wait)
  expect_identical(look$calls(), c("a", "a", "b"))
  expect_identical(res$stopped, "")
  expect_equal(res$latitude, c(1, 1))
})

test_that("a refusal stops the run without a retry", {
  look <- scripted_lookup(list("blocked"))
  res <- geocoder$geocode_places(c("a", "b"), lookup = look$fn, wait = no_wait)
  expect_identical(look$calls(), "a")
  expect_identical(res$stopped, "blocked")
})

test_that("rate limiting honours a short Retry-After and gives up on a long one", {
  waited <- numeric(0)
  record <- function(s) waited <<- c(waited, s)
  short <- impl$lookup_result("rate_limited", retry_after = 7)
  look <- scripted_lookup(list(short, "found", "found"))
  res <- geocoder$geocode_places(c("a", "b"), lookup = look$fn, wait = record)
  expect_identical(res$stopped, "")
  expect_true(7 %in% waited)

  long <- impl$lookup_result("rate_limited", retry_after = 3600)
  look <- scripted_lookup(list(long))
  res <- geocoder$geocode_places(c("a", "b"), lookup = look$fn, wait = no_wait)
  expect_identical(look$calls(), "a")
  expect_identical(res$stopped, "rate_limited")
})

test_that("requests are spaced at least the minimum interval apart", {
  waited <- numeric(0)
  record <- function(s) waited <<- c(waited, s)
  look <- scripted_lookup(list("found"))
  geocoder$geocode_places(c("a", "b", "c"), lookup = look$fn, wait = record)
  expect_identical(length(waited), 2L)
  expect_true(all(waited > 0 & waited <= impl$MIN_INTERVAL_S))
})

test_that("an unreachable host is classified as a network failure, quickly", {
  started <- Sys.time()
  # Port 9 (discard) has no listener on a test machine: the connection is refused at once.
  res <- geocoder$nominatim_lookup("Berlin", timeout_s = 2, url = "http://127.0.0.1:9/search")
  expect_identical(res$outcome, "network")
  expect_true(as.numeric(difftime(Sys.time(), started, units = "secs")) < 5)
})

test_that("each stop reason has an explanation for the user", {
  for (reason in c("offline", "connection_lost", "rate_limited", "blocked", "unavailable")) {
    expect_match(geocoder$geocode_stop_text(reason), "Nominatim")
  }
  expect_identical(geocoder$geocode_stop_text(""), "")
})
