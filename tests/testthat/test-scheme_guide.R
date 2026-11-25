box::use(
  testthat[expect_false, expect_identical, expect_match, expect_null, expect_true, test_that],
)

box::use(
  app / logic / scheme_guide,
  app / logic / scheme_index[scheme_index],
)

impl <- attr(scheme_guide, "namespace")

# One index row with the given fields.
index_row <- function(...) {
  row <- data.frame(
    species = "Listeria monocytogenes",
    type = "cgMLST",
    provider = "pasteur",
    scheme = "Listeria cgMLST1748",
    loci = 1748L,
    url = "https://bigsdb.pasteur.fr/cgi-bin/bigsdb/bigsdb.pl?db=pubmlst_listeria_seqdef",
    note = NA_character_,
    source_id = "pubmlst_listeria_seqdef/3",
    stringsAsFactors = FALSE
  )
  fields <- list(...)
  row[names(fields)] <- fields
  row
}

# All step text of a row as one HTML string.
steps_html <- function(row) {
  paste(
    vapply(scheme_guide$guide_steps(row), function(st) as.character(st$detail), ""),
    collapse = "\n"
  )
}

test_that("a PubMLST cgMLST scheme fetches only the first allele of each locus, anonymously", {
  html <- steps_html(index_row(
    provider = "pubmlst",
    scheme = "Borrelia spp cgMLST",
    source_id = "pubmlst_borrelia_seqdef/3"
  ))
  expect_match(html, 'API="https://rest.pubmlst.org/db/pubmlst_borrelia_seqdef"', fixed = TRUE)
  expect_match(html, "$API/schemes/3/loci", fixed = TRUE)
  expect_match(html, "/alleles/1|", fixed = TRUE)
  expect_match(html, "Borrelia_spp_cgMLST_references.fasta", fixed = TRUE)
  expect_false(grepl("alleles_fasta", html, fixed = TRUE))
  # An API key would unlock post-2024 data, which PubMLST's terms restrict for tools.
  expect_false(grepl("API-Key", html, fixed = TRUE))
})

test_that("a PubMLST MLST scheme also fetches the profile table, without an API key", {
  html <- steps_html(index_row(
    provider = "pubmlst",
    type = "MLST",
    scheme = "Salmonella spp MLST",
    source_id = "pubmlst_salmonella_seqdef/2"
  ))
  expect_match(html, "https://rest.pubmlst.org/db/pubmlst_salmonella_seqdef", fixed = TRUE)
  expect_match(html, "-o profiles.tsv \"$API/schemes/2/profiles_csv", fixed = TRUE)
  expect_false(grepl("API-Key", html, fixed = TRUE))
  expect_match(html, "Profile table", fixed = TRUE)
})

test_that("Institut Pasteur gets links to its pages but no commands", {
  html <- steps_html(index_row())
  expect_false(grepl("<pre>", html, fixed = TRUE))
  expect_match(
    html,
    "bigsdb.pl?db=pubmlst_listeria_seqdef&amp;page=downloadAlleles&amp;scheme_id=3",
    fixed = TRUE
  )
  mlst <- steps_html(index_row(type = "MLST", source_id = "pubmlst_listeria_seqdef/2"))
  expect_false(grepl("<pre>", mlst, fixed = TRUE))
  expect_match(mlst, "page=downloadProfiles&amp;scheme_id=2", fixed = TRUE)
})

test_that("a Chewie-NS schema is downloaded as one ZIP", {
  html <- steps_html(index_row(
    provider = "chewie_ns",
    scheme = "Chlamydia trachomatis cgMLST",
    source_id = "18/1"
  ))
  expect_match(
    html,
    'URL="https://chewbbaca.online/api/NS/api/species/18/schemas/1/zip"',
    fixed = TRUE
  )
  expect_match(html, '"$URL?request_type=download"', fixed = TRUE)
  expect_match(html, "Chlamydia_trachomatis_cgMLST.zip", fixed = TRUE)
})

test_that("EnteroBase uses its scheme folder when known, its website otherwise", {
  html <- steps_html(index_row(
    provider = "enterobase",
    type = "MLST",
    scheme = "Salmonella Achtman 7 Gene MLST",
    source_id = "enterobase:senterica:Achtman_MLST"
  ))
  expect_match(html, "schemes/Salmonella.Achtman7GeneMLST", fixed = TRUE)
  expect_match(html, "profiles.list.gz", fixed = TRUE)

  dsmz <- steps_html(index_row(
    provider = "enterobase",
    scheme = "Enterococcus faecium cgMLST",
    source_id = "enterobase:enterococcus:cgMLST_faecium"
  ))
  expect_false(grepl("<pre>", dsmz, fixed = TRUE))
  expect_match(dsmz, "Download Schemes", fixed = TRUE)
})

test_that("an id that is not of the expected form never reaches a command", {
  for (bad in c("pubmlst_x_seqdef/3; rm -rf ~", "$(id)/1", NA)) {
    html <- steps_html(index_row(provider = "pubmlst", source_id = bad))
    expect_false(grepl("<pre>", html, fixed = TRUE))
  }
  expect_null(impl$chewie_ids("18/1 && id"))
  expect_identical(
    impl$folder_name("Listeria cgMLST1748_v2 (Pasteur)"),
    "Listeria_cgMLST1748_v2_Pasteur"
  )
  expect_identical(impl$folder_name("$`\"'"), "scheme")
})

test_that("every indexed scheme gets a guide, with commands where a route is known", {
  index <- scheme_index()
  scripted <- vapply(seq_len(nrow(index)), function(i) {
    grepl("<pre>", steps_html(index[i, , drop = FALSE]), fixed = TRUE)
  }, logical(1))
  # Every PubMLST and Chewie-NS row has an id the guide can turn into commands;
  # Institut Pasteur's terms and cgMLST.org's robots rules leave theirs without.
  expect_true(all(scripted[index$provider %in% c("pubmlst", "chewie_ns")]))
  expect_false(any(scripted[index$provider %in% c("pasteur", "cgmlst_org")]))
})

test_that("the dialog names the provider's terms and links its page", {
  html <- as.character(scheme_guide$guide_modal(index_row()))
  expect_match(html, "scheme-guide-modal", fixed = TRUE)
  expect_match(html, "Institut Pasteur: academic, non-commercial use only", fixed = TRUE)
  expect_match(html, "https://bigsdb.pasteur.fr/policy/", fixed = TRUE)
  expect_match(html, "Download page", fixed = TRUE)
  # Without commands, the dialog does not talk about a terminal.
  expect_false(grepl("terminal", html, fixed = TRUE))
  pubmlst <- as.character(scheme_guide$guide_modal(index_row(
    provider = "pubmlst",
    source_id = "pubmlst_borrelia_seqdef/3"
  )))
  expect_match(pubmlst, "need <code>curl</code>", fixed = TRUE)
})
