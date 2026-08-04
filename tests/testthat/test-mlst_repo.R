box::use(
  testthat[expect_equal, expect_identical, expect_null, expect_snapshot, expect_true, test_that],
  utils[read.csv],
)
box::use(
  app / logic / mlst_repo,
  app / logic / schemes[cgmlst_org_schemes],
)

impl <- attr(mlst_repo, "namespace")

# Frozen listing of every seqdef database both repositories offered on
# 2026-08-04. The repositories keep adding databases, so this is deliberately a
# snapshot: it pins the *matching rules*, not the live catalogue.
repo_dbs <- function(repo) {
  all <- read.csv("fixtures/repo_databases.csv", stringsAsFactors = FALSE)
  all$description[all$repository == repo]
}

scheme_species <- function() gsub("_", " ", cgmlst_org_schemes$species)

test_that("canonical_species repairs what pyMLST's lstrip ate", {
  expect_identical(
    mlst_repo$canonical_species("Klebsiella neumoniae/variicola/quasipneumoniae"),
    "Klebsiella pneumoniae/variicola/quasipneumoniae"
  )
  expect_identical(mlst_repo$canonical_species("Escherichia oli"), "Escherichia coli")
  expect_identical(mlst_repo$canonical_species("Salmonella nterica"), "Salmonella enterica")
})

test_that("canonical_species leaves intact and unknown names alone", {
  expect_identical(mlst_repo$canonical_species("Escherichia coli"), "Escherichia coli")
  # Underscores are how the scheme table spells the same name.
  expect_identical(
    mlst_repo$canonical_species("Staphylococcus_aureus"),
    "Staphylococcus aureus"
  )
  expect_identical(mlst_repo$canonical_species("Wolbachia pipientis"), "Wolbachia pipientis")
  expect_identical(mlst_repo$canonical_species(NA_character_), NA_character_)
  expect_identical(mlst_repo$canonical_species(""), NA_character_)
})

test_that("every scheme name survives a round trip through pyMLST's damage", {
  # The one guarantee that matters for existing databases: whatever pyMLST
  # stored, canonical_species() must map it back to the scheme it came from.
  species <- scheme_species()
  damaged <- vapply(species, impl$mangle_species, character(1))
  repaired <- vapply(unname(damaged), mlst_repo$canonical_species, character(1))
  expect_identical(repaired, species)
})

test_that("match_species_db never crosses into another genus", {
  # A "pseudotuberculosis" query used to pull Yersinia for a Corynebacterium
  # scheme, because the repository search matched any single word.
  pubmlst <- repo_dbs("pubmlst")
  idx <- mlst_repo$match_species_db(pubmlst, "Corynebacterium pseudotuberculosis")
  expect_true(is.na(idx))
  expect_identical(
    mlst_repo$match_species_db(repo_dbs("pasteur"), "Corynebacterium pseudotuberculosis"),
    1L
  )
  # Neither does a genus-wide database serve a species it does not hold.
  expect_true(is.na(mlst_repo$match_species_db(pubmlst, "Yersinia enterocolitica")))
  expect_true(is.na(mlst_repo$match_species_db(pubmlst, "Bacillus anthracis")))
})

test_that("match_species_db prefers the species database over the genus one", {
  descriptions <- c("Klebsiella aerogenes", "Klebsiella oxytoca", "Klebsiella spp.")
  expect_identical(
    mlst_repo$match_species_db(descriptions, "Klebsiella oxytoca/grimontii"),
    2L
  )
  # Nothing species-level left, so the genus-wide database serves.
  expect_identical(
    mlst_repo$match_species_db(descriptions, "Klebsiella pneumoniae"),
    3L
  )
})

test_that("match_species_db reports rather than guesses between equals", {
  idx <- mlst_repo$match_species_db(c("Vibrio spp.", "Vibrio"), "Vibrio cholerae")
  expect_true(is.na(idx))
  expect_true(grepl("2 genus-level", attr(idx, "reason")))
})

test_that("scheme species resolve to a stable repository database", {
  # Automated tracking of the nomenclature mapping: this snapshot is the record
  # of which database every cgMLST scheme's species resolves to. A rule change
  # or a new scheme shows up here as a reviewable diff.
  pick <- function(repo, species) {
    dbs <- repo_dbs(repo)
    idx <- mlst_repo$match_species_db(dbs, species)
    if (is.na(idx)) "-" else dbs[idx]
  }
  mapping <- data.frame(
    species = scheme_species(),
    pubmlst = vapply(scheme_species(), pick, character(1), repo = "pubmlst"),
    pasteur = vapply(scheme_species(), pick, character(1), repo = "pasteur"),
    stringsAsFactors = FALSE
  )
  expect_snapshot(print(mapping, right = FALSE, row.names = FALSE))
})

test_that("match_mlst_scheme takes the repository's own plain MLST scheme", {
  # Enterococcus faecium: a 7-locus "MLST" beside an 8-locus revision. pyMLST
  # refuses to choose here, losing the ST for the whole run.
  expect_identical(
    mlst_repo$match_mlst_scheme(
      c("MLST", "MLST (Bezdicek et al.)"),
      c(7L, 8L),
      "Enterococcus faecium"
    ),
    1L
  )
})

test_that("match_mlst_scheme falls back to the seven-locus scheme", {
  # Escherichia: the 7-locus Achtman scheme is classical MLST, the 8-locus
  # Pasteur scheme is not.
  expect_identical(
    mlst_repo$match_mlst_scheme(
      c("MLST (Achtman)", "MLST (Pasteur)"),
      c(7L, 8L),
      "Escherichia coli"
    ),
    1L
  )
})

test_that("match_mlst_scheme uses the species when locus counts tie", {
  expect_identical(
    mlst_repo$match_mlst_scheme(
      c("enterocolitica MLST", "pseudotuberculosis MLST"),
      c(8L, 8L),
      "Yersinia enterocolitica"
    ),
    1L
  )
})

test_that("match_mlst_scheme reports an undecidable or empty set", {
  none <- mlst_repo$match_mlst_scheme(character(0), integer(0), "Yersinia enterocolitica")
  expect_true(is.na(none))
  expect_true(grepl("no classical MLST scheme", attr(none, "reason")))

  tie <- mlst_repo$match_mlst_scheme(c("scheme A", "scheme B"), c(8L, 8L), "Genus species")
  expect_true(is.na(tie))
  expect_true(grepl("2 schemes match", attr(tie, "reason")))
})

# A stand-in repository: one genus database holding two MLST schemes plus a
# cgMLST scheme, wired the way both REST APIs shape their responses.
fake_fetch <- function(host = "https://rest.pubmlst.org/db") {
  function(endpoint) {
    if (endpoint == "https://rest.pubmlst.org/db") {
      return(list(list(name = "bacteria", databases = list(
        list(name = "x_efaecium_seqdef",
             description = "Enterococcus faecium sequence/profile definitions",
             href = "https://x/efaecium"),
        list(name = "x_efaecium_isolates",
             description = "Enterococcus faecium isolates", href = "https://x/iso")
      ))))
    }
    if (endpoint == "https://bigsdb.pasteur.fr/api/db") {
      return(list(list(name = "bacteria", databases = list())))
    }
    if (endpoint == "https://x/efaecium/schemes") {
      return(list(schemes = list(
        list(scheme = "https://x/s1", description = "MLST"),
        list(scheme = "https://x/s2", description = "MLST (revised)"),
        list(scheme = "https://x/s3", description = "cgMLST")
      )))
    }
    if (endpoint == "https://x/s1") {
      return(list(
        profiles_csv = "https://x/s1/profiles_csv",
        last_added = "2026-07-29",
        loci = as.list(paste0("https://x/loci/", c("atpA", "ddl", "gdh")))
      ))
    }
    if (endpoint == "https://x/s2") {
      return(list(
        profiles_csv = "https://x/s2/profiles_csv",
        last_added = "2026-01-01",
        loci = as.list(paste0("https://x/loci/", letters[1:8]))
      ))
    }
    stop("unexpected endpoint: ", endpoint)
  }
}

test_that("resolve_mlst_scheme returns one downloadable scheme", {
  hit <- mlst_repo$resolve_mlst_scheme("Enterococcus faecium", fetch = fake_fetch())
  expect_identical(hit$repository, "pubmlst")
  expect_identical(hit$database, "Enterococcus faecium")
  expect_identical(hit$scheme, "MLST")
  expect_identical(hit$profiles_url, "https://x/s1/profiles_csv")
  expect_identical(hit$version, "2026-07-29")
  expect_equal(length(hit$loci), 3)
})

test_that("resolve_mlst_scheme repairs the species before looking it up", {
  hit <- mlst_repo$resolve_mlst_scheme("Enterococcus aecium", fetch = fake_fetch())
  expect_identical(hit$species, "Enterococcus faecium")
})

test_that("resolve_mlst_scheme gives up quietly when nothing resolves", {
  expect_null(mlst_repo$resolve_mlst_scheme("Escherichia coli", fetch = fake_fetch()))
  expect_null(mlst_repo$resolve_mlst_scheme(NA_character_, fetch = fake_fetch()))
  # A repository that is down must not fail the typing run.
  expect_null(
    mlst_repo$resolve_mlst_scheme(
      "Enterococcus faecium",
      fetch = function(endpoint) stop("connection refused")
    )
  )
})

test_that("write_scheme_spec emits one locus line per locus", {
  path <- tempfile()
  on.exit(unlink(path), add = TRUE)
  mlst_repo$write_scheme_spec(
    mlst_repo$resolve_mlst_scheme("Enterococcus faecium", fetch = fake_fetch()),
    path
  )
  lines <- readLines(path)

  expect_true("repository=pubmlst" %in% lines)
  expect_true("species=Enterococcus faecium" %in% lines)
  expect_true("scheme=MLST" %in% lines)
  expect_true("version=2026-07-29" %in% lines)
  expect_true("profiles=https://x/s1/profiles_csv" %in% lines)
  expect_identical(sum(grepl("^locus=", lines)), 3L)
})
