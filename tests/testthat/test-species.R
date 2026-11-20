box::use(
  testthat[expect_false, expect_identical, expect_null, expect_setequal, expect_true, test_that],
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

test_that("every picture credit names author, licence and source", {
  credits <- species$image_credits()
  expect_true(nrow(credits) > 0)
  expect_true(all(nzchar(credits$author)))
  expect_true(all(grepl("^(Public domain|CC0|CC BY(-SA)? [0-9.]+)$", credits$license)))
  expect_true(all(grepl("^https://commons\\.wikimedia\\.org/wiki/File:", credits$source)))
  # Openly licensed pictures need a licence link; public-domain ones have none.
  open <- grepl("^CC BY", credits$license)
  expect_false(any(is.na(credits$license_url[open])))
  expect_true(all(is.na(credits$license_url[credits$license == "Public domain"])))
})

test_that("a catalog species without a picture of its own gets the generic one", {
  catalog <- species$species_catalog()
  expect_false(any(is.na(catalog$image)))
  expect_identical(species$species_image("Streptococcus uberis"), species$GENERIC_IMAGE)
  expect_true(file.exists(box::file("../../app/static/species/generic.svg")))
  # Every picture of its own is credited.
  own <- catalog$label[catalog$image != species$GENERIC_IMAGE]
  expect_setequal(own, species$image_credits()$label)
})

test_that("a species without a picture credit has none", {
  expect_null(species$image_credit("Not a species"))
  credit <- species$image_credit("Klebsiella pneumoniae/variicola/quasipneumoniae")
  expect_identical(credit$license, "Public domain")
})

test_that("Creative Commons licences link to their deed", {
  impl <- attr(species, "namespace")
  expect_identical(
    impl$license_url("CC BY-SA 4.0"),
    "https://creativecommons.org/licenses/by-sa/4.0/"
  )
  expect_identical(impl$license_url("CC BY 2.0"), "https://creativecommons.org/licenses/by/2.0/")
  expect_identical(impl$license_url("Public domain"), NA_character_)
})
