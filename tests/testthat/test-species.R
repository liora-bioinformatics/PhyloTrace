box::use(
  testthat[expect_false, expect_identical, expect_null, expect_true, test_that],
)
box::use(
  app / logic / species,
)

test_that("a species group is split into genus and epithets and composed back", {
  parts <- species$species_parts("Klebsiella pneumoniae/variicola")
  expect_identical(parts$genus, "Klebsiella")
  expect_identical(parts$epithets, c("pneumoniae", "variicola"))
  expect_identical(species$species_parts("Brucella spp")$epithets, character(0))
  expect_identical(
    species$compose_species("klebsiella", c("Pneumoniae", "variicola", "pneumoniae")),
    "Klebsiella pneumoniae/variicola"
  )
  expect_identical(species$compose_species("Salmonella"), "Salmonella")
})

test_that("a custom species is checked before it reaches the database", {
  expect_identical(species$species_problem("Klebsiella", "pneumoniae"), NA_character_)
  expect_false(is.na(species$species_problem("", "pneumoniae")))
  expect_false(is.na(species$species_problem("Kleb'siella", character(0))))
  expect_false(is.na(species$species_problem("Klebsiella", "pneu<b>")))
  expect_false(is.na(species$species_problem("Klebsiella", letters)))
})

test_that("the catalog has one row per species, each with metadata", {
  catalog <- species$species_catalog()
  expect_false(any(duplicated(catalog$label)))
  expect_true("Burkholderia mallei" %in% catalog$label)
  for (label in catalog$label) {
    expect_false(is.null(species$species_record(label)))
  }
  expect_null(species$species_record("Foo barus"))
  expect_identical(species$species_image("Foo barus"), NA_character_)
})

test_that("the default database name is file-name safe", {
  expect_identical(
    species$default_db_name("Klebsiella pneumoniae/variicola"),
    "Klebsiella_pneumoniae"
  )
  expect_identical(species$default_db_name(""), "PhyloTrace_database")
})
