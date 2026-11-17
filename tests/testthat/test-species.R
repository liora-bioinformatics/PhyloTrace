box::use(
  testthat[expect_false, expect_identical, expect_null, expect_true, test_that],
)
box::use(
  app / logic / schemes[cgmlst_org_schemes],
  app / logic / species,
)

test_that("canonical_species repairs what pyMLST's lstrip ate", {
  expect_identical(
    species$canonical_species("Klebsiella neumoniae/variicola/quasipneumoniae"),
    "Klebsiella pneumoniae/variicola/quasipneumoniae"
  )
  expect_identical(species$canonical_species("Staphylococcus aureus"), "Staphylococcus aureus")
})

test_that("canonical_species leaves unknown names and blanks alone", {
  expect_identical(species$canonical_species("Foo barus"), "Foo barus")
  expect_identical(species$canonical_species(""), NA_character_)
  expect_identical(species$canonical_species(NA), NA_character_)
})

test_that("every scheme name survives a round trip through pyMLST's damage", {
  for (name in gsub("_", " ", cgmlst_org_schemes$species)) {
    repaired <- species$canonical_species(species$mangle_species(name))
    expect_true(repaired == name || species$mangle_species(repaired) == species$mangle_species(name))
  }
})

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
