box::use(
  testthat[expect_equal, expect_false, expect_identical, expect_match, expect_true, test_that],
)
box::use(
  app / logic / geo,
)

impl <- attr(geo, "namespace")

# --- countries -----------------------------------------------------------------

test_that("country spellings, codes and languages resolve to one canonical name", {
  got <- geo$match_country(c(
    "Germany", "germany", " Deutschland ", "DE", "DEU", "Allemagne",
    "USA", "U.S.A.", "us", "United States",
    "UK", "Great Britain",
    "Czech Republic", "the Netherlands", "Holland", "Côte d'Ivoire", "Cote dIvoire",
    "Россия"
  ))
  expect_identical(got, c(
    rep("Germany", 6),
    rep("United States of America", 4),
    rep("United Kingdom", 2),
    "Czechia", "Netherlands", "Netherlands", "Ivory Coast", "Ivory Coast",
    "Russia"
  ))
})

test_that("unknown, blank and missing countries are not guessed", {
  expect_identical(
    geo$match_country(c("Atlantis", "", NA, "England")),
    rep(NA_character_, 4)
  )
})

test_that("the two Congos stay apart", {
  expect_identical(
    geo$country_code(c("Congo", "DR Congo", "Republic of the Congo", "Dem. Rep. Congo")),
    c("COG", "COD", "COG", "COD")
  )
})

test_that("the vocabulary has one row per unit and every name resolves to itself", {
  names <- geo$country_names()
  expect_false(anyDuplicated(names) > 0)
  expect_identical(geo$match_country(names), names)
  expect_true(length(names) > 200)
})

test_that("coerce_country() stores the canonical name or explains the rejection", {
  ok <- geo$coerce_country("deutschland")
  expect_true(ok$ok)
  expect_identical(ok$value, "Germany")
  bad <- geo$coerce_country("Atlantis")
  expect_false(bad$ok)
  expect_match(bad$reason, "Not a recognised country")
})

# --- coordinates ---------------------------------------------------------------

test_that("decimal, decimal-comma and DMS coordinates all parse to the same point", {
  x <- c(
    "52.52, 13.40", "52.52 13.4", "52.52;13.40", "52,52 13,40", "52,52; 13,40",
    "52°31'12\"N 13°24'E", "52 31 12 N 13 24 0 E", "N 52.52 E 13.4",
    "13.4 E, 52.52 N", "52º31′12″N, 13º24′E"
  )
  p <- geo$parse_coordinates(x)
  expect_equal(p$latitude, rep(52.52, length(x)), tolerance = 1e-9)
  expect_equal(p$longitude, rep(13.4, length(x)), tolerance = 1e-9)
  expect_identical(p$problem, rep("", length(x)))
})

test_that("hemisphere letters and signs give southern and western values", {
  p <- geo$parse_coordinates(c("33.92 S 18.42 E", "-33.92, 18.42", "40.7 N 74.0 W"))
  expect_equal(p$latitude, c(-33.92, -33.92, 40.7))
  expect_equal(p$longitude, c(18.42, 18.42, -74))
})

test_that("a comma between two integers is a separator, not a decimal comma", {
  p <- geo$parse_coordinates("52,13")
  expect_equal(c(p$latitude, p$longitude), c(52, 13))
})

test_that("unreadable coordinates are NA with a reason, blanks are silent", {
  p <- geo$parse_coordinates(c("abc", "52.5", "95, 200", "S -33 18 E", "52°60'N 13E", "", NA))
  expect_true(all(is.na(p$latitude)))
  expect_true(all(nzchar(p$problem[1:5])))
  expect_identical(p$problem[6:7], c("", ""))
})

test_that("an out-of-range latitude before a valid one reads as swapped", {
  p <- geo$parse_coordinates("152.52, 13.4")
  expect_match(p$problem, "swapped")
})

test_that("coerce_coordinates() stores trimmed decimal degrees", {
  expect_identical(geo$coerce_coordinates("52°31'12\"N 13°24'E")$value, "52.52, 13.4")
  expect_identical(geo$coerce_coordinates("52.520008, 13.404954123")$value, "52.520008, 13.404954")
  expect_identical(geo$coerce_coordinates("0, -0.5")$value, "0, -0.5")
  expect_false(geo$coerce_coordinates("nowhere")$ok)
})

test_that("the lookup key folds case, accents, periods and a leading article", {
  expect_identical(
    impl$.country_key(c("U.S.A.", "Côte d'Ivoire", "The  Gambia")),
    c("usa", "cote divoire", "gambia")
  )
})
