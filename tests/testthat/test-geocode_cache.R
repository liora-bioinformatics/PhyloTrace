box::use(
  testthat[expect_equal, expect_false, expect_identical, expect_true, test_that],
  withr[local_tempdir],
)
box::use(
  app / logic / geocode_cache,
)

test_that("places round-trip through the database and a repeat replaces", {
  db <- file.path(local_tempdir(), "t.db")
  build_db(db, default_local(), metadata = meta_df(c("A", "B")))
  expect_identical(nrow(geocode_cache$read_geocodes(db)), 0L)

  geocode_cache$write_geocodes(db, data.frame(
    place = c("Berlin, Germany", "Nowhere"),
    latitude = c(52.52, NA),
    longitude = c(13.4, NA)
  ))
  got <- geocode_cache$read_geocodes(db)
  expect_identical(got$place, "Berlin, Germany")

  geocode_cache$write_geocodes(db, data.frame(
    place = "Berlin, Germany", latitude = 52.5, longitude = 13.3
  ))
  got <- geocode_cache$read_geocodes(db)
  expect_identical(nrow(got), 1L)
  expect_equal(got$latitude, 52.5)

  expect_true(geocode_cache$clear_geocodes(db))
  expect_identical(nrow(geocode_cache$read_geocodes(db)), 0L)
})

test_that("no database means an empty cache and no writes", {
  expect_identical(nrow(geocode_cache$read_geocodes(NULL)), 0L)
  expect_false(geocode_cache$write_geocodes(
    NULL, data.frame(place = "x", latitude = 1, longitude = 2)
  ))
  expect_false(geocode_cache$clear_geocodes(file.path(tempdir(), "missing.db")))
})
