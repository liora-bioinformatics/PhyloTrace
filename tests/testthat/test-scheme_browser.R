box::use(
  testthat[expect_equal, expect_false, expect_null, expect_true, test_that],
  withr[local_tempdir],
)
box::use(
  app / logic / database_functions[load_db_scheme_overview, load_db_species],
  app / logic / mlst_repo[mangle_species],
  app / logic / scheme_browser,
  app / logic / schemes[cgmlst_org_schemes],
)

impl <- attr(scheme_browser, "namespace")

scheme_keys <- function() cgmlst_org_schemes$species

# The `scheme_overview` table a scheme download leaves behind: only the URL row
# matters here, because that is what identifies the scheme (`get_scheme_overview()`
# writes it, `download_scheme_overview()` stores it under key/value).
overview_of <- function(abb) {
  url <- paste0("https://www.cgmlst.org/ncs/schema/", abb)
  data.frame(
    key = c("Name", "URL", "Locus Count"),
    value = c(
      "Test scheme",
      paste0('<a href="', url, '/" target="_blank">', url, '</a>'),
      "2838"
    ),
    stringsAsFactors = FALSE
  )
}

# A database exactly as a scheme download produces it: the species string pyMLST
# scrapes into `mlst_type` (curator suffix lost, leading letters eaten) plus the
# overview table this app writes afterwards.
build_scheme_db <- function(path, key, with_overview = TRUE) {
  build_db(path, default_local(), species = pymlst_species(key))

  if (with_overview) {
    abb <- cgmlst_org_schemes$abb[match(key, cgmlst_org_schemes$species)]
    con <- DBI::dbConnect(RSQLite::SQLite(), path)
    on.exit(DBI::dbDisconnect(con), add = TRUE)
    DBI::dbWriteTable(con, "scheme_overview", overview_of(abb), overwrite = TRUE)
  }

  invisible(path)
}

# What pyMLST stores for a scheme: the page carries no curator suffix, and its
# Genus/Species lines are lstrip-damaged. Verified against real databases in
# "the species string of a real download resolves" below.
pymlst_species <- function(key) {
  mangle_species(trimws(sub("\\s*\\([^)]*\\)$", "", gsub("_", " ", key))))
}

test_that("every scheme has a metadata record and a species image", {
  for (key in scheme_keys()) {
    details <- scheme_browser$get_species_details(key)
    expect_false(is.null(details), info = key)
    expect_true(is.list(details$lineage), info = key)
    expect_true(isTRUE(nzchar(details$summary)), info = key)

    img <- scheme_browser$get_species_img(key)
    expect_false(is.null(img), info = key)
    expect_true(file.exists(file.path("../..", img)), info = img)
  }
})

test_that("the scheme table and the metadata file agree on every abbreviation", {
  for (key in scheme_keys()) {
    expect_equal(
      scheme_browser$get_species_details(key)$abb,
      cgmlst_org_schemes$abb[match(key, cgmlst_org_schemes$species)],
      info = key
    )
  }
})

# The regression this file exists for: a downloaded database identifies its
# scheme only through `mlst_type.species` and `scheme_overview`, and the Scheme
# Info panel has to render metadata from those alone.
test_that("every downloaded scheme resolves back from its database", {
  dir <- local_tempdir()

  for (key in scheme_keys()) {
    db <- file.path(dir, paste0(gsub("[^A-Za-z0-9]", "_", key), ".db"))
    build_scheme_db(db, key)

    expect_equal(
      scheme_browser$resolve_scheme_key(
        load_db_species(db),
        load_db_scheme_overview(db)
      ),
      key,
      info = key
    )
    expect_false(
      is.null(scheme_browser$get_species_details(
        scheme_browser$resolve_scheme_key(
          load_db_species(db),
          load_db_scheme_overview(db)
        )
      )),
      info = key
    )
  }
})

test_that("a scheme resolves from the species string alone", {
  dir <- local_tempdir()

  # Without a scheme_overview table the two B. mallei schemes are
  # indistinguishable - every other scheme still resolves to itself.
  ambiguous <- c("Burkholderia_mallei_(RKI)", "Burkholderia_mallei_(FLI)")

  for (key in setdiff(scheme_keys(), ambiguous)) {
    db <- file.path(dir, paste0(gsub("[^A-Za-z0-9]", "_", key), ".db"))
    build_scheme_db(db, key, with_overview = FALSE)

    expect_equal(
      scheme_browser$resolve_scheme_key(load_db_species(db)),
      key,
      info = key
    )
  }

  for (key in ambiguous) {
    expect_equal(
      scheme_browser$resolve_scheme_key(pymlst_species(key)),
      ambiguous[1],
      info = key
    )
  }
})

test_that("the scheme URL outranks the species string", {
  # Both mallei schemes reach the database as "Burkholderia mallei"; only the
  # overview URL says which one it is.
  expect_equal(
    scheme_browser$resolve_scheme_key(
      "Burkholderia mallei",
      overview_of("Bmallei_fli")
    ),
    "Burkholderia_mallei_(FLI)"
  )
  expect_equal(
    scheme_browser$resolve_scheme_key(
      "Burkholderia mallei",
      overview_of("Bmallei_rki")
    ),
    "Burkholderia_mallei_(RKI)"
  )
  # An abbreviation no scheme claims is no identification: the species string
  # decides instead of the panel going blank.
  expect_equal(
    scheme_browser$resolve_scheme_key("Campylobacter jejuni/coli", overview_of("Bnope")),
    "Campylobacter_jejuni/coli"
  )
})

test_that("the species string of a real download resolves", {
  # Observed in `mlst_type.species` of databases downloaded on 2026-08-05: the
  # curator suffix is gone, and "pneumoniae" arrives as "neumoniae" because
  # pyMLST strips a character set, not a prefix.
  observed <- c(
    "Acinetobacter baumannii" = "Acinetobacter_baumannii",
    "Burkholderia mallei" = "Burkholderia_mallei_(RKI)",
    "Campylobacter jejuni/coli" = "Campylobacter_jejuni/coli",
    "Citrobacter freundii/portucalensis/braakii/europaeus" =
      "Citrobacter_freundii/portucalensis/braakii/europaeus",
    "Enterococcus faecium" = "Enterococcus_faecium",
    "Klebsiella neumoniae/variicola/quasipneumoniae" =
      "Klebsiella_pneumoniae/variicola/quasipneumoniae",
    "Pseudomonas aeruginosa" = "Pseudomonas_aeruginosa"
  )

  for (species in names(observed)) {
    expect_equal(
      scheme_browser$resolve_scheme_key(species),
      observed[[species]],
      info = species
    )
  }
})

test_that("an unidentifiable scheme resolves to NULL, not to a wrong one", {
  expect_null(scheme_browser$resolve_scheme_key("Wolbachia pipientis"))
  expect_null(scheme_browser$resolve_scheme_key(NULL))
  expect_null(scheme_browser$resolve_scheme_key(""))
  expect_null(scheme_browser$resolve_scheme_key(NA_character_))
  expect_null(scheme_browser$get_species_details("Wolbachia pipientis"))
  expect_null(scheme_browser$get_species_img("Wolbachia pipientis"))
})

test_that("the scheme URL is read out of a stored overview table", {
  expect_equal(impl$.overview_abb(overview_of("Kpneumoniae_complex")), "Kpneumoniae_complex")
  expect_null(impl$.overview_abb(NULL))
  expect_null(impl$.overview_abb(data.frame(key = "Name", value = "Test scheme")))
})
