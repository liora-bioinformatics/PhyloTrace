box::use(
  testthat[expect_false, expect_identical, expect_true, test_that],
)
box::use(
  app / logic / scheme_index,
  app / logic / scheme_sources[source_choices],
  app / logic / species[species_catalog, species_parts],
)

test_that("every index row is complete and points to a known provider", {
  index <- scheme_index$scheme_index()
  expect_true(all(index$type %in% c("cgMLST", "wgMLST", "MLST")))
  expect_true(all(index$provider %in% source_choices()))
  expect_true(all(grepl("^https://[^[:space:]]+$", index$url)))
  expect_true(all(nzchar(index$scheme)))
  expect_false(any(is.na(vapply(index$species, function(s) species_parts(s)$genus, ""))))
  expect_false(anyDuplicated(index[, c("species", "provider", "scheme", "url")]) > 0)
})

test_that("every catalog species finds a scheme for the cgMLST step", {
  for (label in species_catalog()$label) {
    hits <- scheme_index$schemes_for_species(label, type = scheme_index$CORE_TYPES)
    expect_true(nrow(hits) > 0, info = label)
  }
})

test_that("a species finds group schemes and genus-wide schemes", {
  kp <- scheme_index$schemes_for_species("Klebsiella pneumoniae")
  expect_true("Klebsiella pneumoniae/variicola/quasipneumoniae cgMLST" %in% kp$scheme)

  brucella <- scheme_index$schemes_for_species("Brucella melitensis")
  expect_true(all(c("Brucella melitensis cgMLST", "Brucella spp. cgMLST") %in% brucella$scheme))
  # cgMLST schemes come first.
  expect_false(is.unsorted(match(brucella$type, c("cgMLST", "wgMLST", "MLST"))))
  expect_true("MLST" %in% brucella$type)

  salmonella <- scheme_index$schemes_for_species("Salmonella enterica", type = "MLST")
  expect_true(all(c("pubmlst", "enterobase") %in% salmonella$provider))
  expect_true(all(salmonella$type == "MLST"))

  # A genus-wide row (EnteroBase's Streptococcus scheme) serves every species.
  strep <- scheme_index$schemes_for_species("Streptococcus pneumoniae", type = "cgMLST")
  expect_true("Streptococcus cgMLST V1" %in% strep$scheme)
})

test_that("wgMLST schemas are offered and listed after cgMLST", {
  salmonella <- scheme_index$schemes_for_species(
    "Salmonella enterica",
    type = scheme_index$CORE_TYPES
  )
  expect_true("wgMLST" %in% salmonella$type)
  expect_false(is.unsorted(match(salmonella$type, c("cgMLST", "wgMLST"))))
})

test_that("unknown or empty species find nothing", {
  expect_identical(nrow(scheme_index$schemes_for_species("Foo barus")), 0L)
  expect_identical(nrow(scheme_index$schemes_for_species(NA)), 0L)
  expect_identical(nrow(scheme_index$schemes_for_species("")), 0L)
  expect_identical(nrow(scheme_index$schemes_for_species("Klebsiella aerogenes", "cgMLST")), 0L)
})
