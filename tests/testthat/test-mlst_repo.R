box::use(
  testthat[expect_equal, expect_false, expect_identical, expect_setequal, expect_true, test_that],
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
  expect_identical(unname(repaired), species)
})

test_that("match_species_db never crosses into another genus", {
  # A "pseudotuberculosis" query used to pull Yersinia for a Corynebacterium
  # scheme, because the repository search matched any single word.
  pubmlst <- repo_dbs("pubmlst")
  idx <- mlst_repo$match_species_db(pubmlst, "Corynebacterium pseudotuberculosis")
  expect_true(is.na(idx))
  pasteur <- repo_dbs("pasteur")
  expect_identical(
    pasteur[mlst_repo$match_species_db(pasteur, "Corynebacterium pseudotuberculosis")],
    "Corynebacterium"
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

test_that("every cgMLST scheme species resolves to the expected database", {
  # Automated tracking of the nomenclature mapping: the record of which
  # repository database each of the app's cgMLST schemes lands on, walking the
  # repositories in default order. A rule change, a renamed scheme or a new row
  # in cgmlst_schemes.csv shows up here as a reviewable diff. "-" means no
  # classical MLST is available for that scheme at all.
  expected <- c(
    "Acinetobacter baumannii" = "pubmlst: Acinetobacter baumannii",
    "Bacillus anthracis" = "-",
    "Bordetella pertussis" = "pasteur: Bordetella",
    "Brucella melitensis" = "pubmlst: Brucella spp.",
    "Brucella spp" = "pubmlst: Brucella spp.",
    "Burkholderia mallei (RKI)" = "pubmlst: Burkholderia mallei",
    "Burkholderia mallei (FLI)" = "pubmlst: Burkholderia mallei",
    "Burkholderia pseudomallei" = "pubmlst: Burkholderia pseudomallei",
    "Campylobacter jejuni/coli" = "pubmlst: Campylobacter jejuni/coli",
    "Clostridioides difficile" = "pubmlst: Clostridioides difficile",
    "Clostridium perfringens" = "pubmlst: Clostridium perfringens",
    "Corynebacterium diphtheriae" = "pasteur: Corynebacterium",
    "Corynebacterium pseudotuberculosis" = "pasteur: Corynebacterium",
    "Cronobacter sakazakii/malonaticus" = "pubmlst: Cronobacter spp.",
    "Enterococcus faecalis" = "pubmlst: Enterococcus faecalis",
    "Enterococcus faecium" = "pubmlst: Enterococcus faecium",
    "Escherichia coli" = "pubmlst: Escherichia spp.",
    "Francisella tularensis" = "-",
    "Klebsiella oxytoca/grimontii/michiganensis/pasteurii" = "pubmlst: Klebsiella oxytoca",
    "Klebsiella pneumoniae/variicola/quasipneumoniae" =
      "pasteur: REST API access to Klebsiella seqdef database",
    "Legionella pneumophila" = "-",
    "Listeria monocytogenes" = "pasteur: REST API access to Listeria seqdef database",
    "Mycobacterium tuberculosis/bovis/africanum/canettii" = "-",
    "Mycobacteroides abscessus" = "pubmlst: Mycobacteroides abscessus complex",
    "Mycoplasma gallisepticum" = "pubmlst: Mycoplasma gallisepticum",
    "Paenibacillus larvae" = "pubmlst: Paenibacillus larvae",
    "Pseudomonas aeruginosa" = "pubmlst: Pseudomonas aeruginosa",
    "Salmonella enterica" = "pubmlst: Salmonella spp.",
    "Serratia marcescens" = "pubmlst: Serratia spp.",
    "Staphylococcus argenteus" = "-",
    "Staphylococcus aureus" = "pubmlst: Staphylococcus aureus",
    "Staphylococcus capitis" = "pubmlst: Staphylococcus capitis",
    "Streptococcus pyogenes" = "pubmlst: Streptococcus pyogenes",
    "Yersinia enterocolitica" = "pasteur: Yersinia",
    "Citrobacter freundii" = "pubmlst: Citrobacter spp.",
    "Citrobacter freundii/portucalensis/braakii/europaeus" = "pubmlst: Citrobacter spp.",
    "Enterobacter hormaechei" = "pubmlst: Enterobacter spp.",
    "Morganella morganii" = "pasteur: morganella",
    "Proteus mirabilis" = "pubmlst: Proteus spp.",
    "Providencia stuartii" = "pubmlst: Providencia spp."
  )

  resolved <- vapply(
    scheme_species(),
    function(species) {
      for (repo in c("pubmlst", "pasteur")) {
        dbs <- repo_dbs(repo)
        idx <- mlst_repo$match_species_db(dbs, species)
        if (!is.na(idx)) {
          return(paste0(repo, ": ", dbs[idx]))
        }
      }
      "-"
    },
    character(1)
  )

  expect_identical(resolved, expected)
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

test_that("match_mlst_scheme breaks a naming-convention tie by preference", {
  # Acinetobacter baumannii: two equally classical 7-locus schemes under
  # PubMLST, tied on locus count and neither named for the species - nothing
  # structural picks between them.
  descriptions <- c("MLST (Oxford)", "MLST (Pasteur)")
  loci <- c(7L, 7L)

  # Without a preference, the tie is reported rather than guessed.
  none <- mlst_repo$match_mlst_scheme(descriptions, loci, "Acinetobacter baumannii")
  expect_true(is.na(none))

  expect_identical(
    mlst_repo$match_mlst_scheme(
      descriptions, loci, "Acinetobacter baumannii",
      prefer = "pubmlst"
    ),
    1L
  )
  expect_identical(
    mlst_repo$match_mlst_scheme(
      descriptions, loci, "Acinetobacter baumannii",
      prefer = "pasteur"
    ),
    2L
  )
})

test_that("a preference that does not narrow the tie changes nothing", {
  # Both candidates would name the Pasteur convention, or neither does - the
  # preference then has no work to do and the tie is still reported.
  same <- mlst_repo$match_mlst_scheme(
    c("MLST (Pasteur, v1)", "MLST (Pasteur, v2)"), c(7L, 7L), "Genus species",
    prefer = "pasteur"
  )
  expect_true(is.na(same))
})

test_that("a structural tiebreak is never overridden by preference", {
  # Escherichia coli: Achtman (7 loci) vs Pasteur (8 loci) - the locus count
  # alone already decides this, regardless of which repository was preferred.
  expect_identical(
    mlst_repo$match_mlst_scheme(
      c("MLST (Achtman)", "MLST (Pasteur)"), c(7L, 8L), "Escherichia coli",
      prefer = "pasteur"
    ),
    1L
  )
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
  expect_true(hit$resolved)
  expect_identical(hit$repository, "pubmlst")
  expect_identical(hit$database, "Enterococcus faecium")
  expect_identical(hit$scheme, "MLST")
  expect_identical(hit$profiles_url, "https://x/s1/profiles_csv")
  expect_identical(hit$version, "2026-07-29")
  expect_equal(length(hit$loci), 3)
})

test_that("resolve_mlst_scheme normalizes the species before looking it up", {
  hit <- mlst_repo$resolve_mlst_scheme("Enterococcus_faecium ", fetch = fake_fetch())
  expect_identical(hit$species, "Enterococcus faecium")
})

# A stand-in repository shaped like Acinetobacter baumannii on the real
# PubMLST: one genus database, two equally classical 7-locus schemes, no
# structural signal to prefer either.
fake_fetch_tied <- function() {
  function(endpoint) {
    if (endpoint == "https://rest.pubmlst.org/db") {
      return(list(list(name = "bacteria", databases = list(
        list(name = "x_abaumannii_seqdef",
             description = "Acinetobacter baumannii sequence/profile definitions",
             href = "https://x/abaumannii")
      ))))
    }
    if (endpoint == "https://bigsdb.pasteur.fr/api/db") {
      return(list(list(name = "bacteria", databases = list())))
    }
    if (endpoint == "https://x/abaumannii/schemes") {
      return(list(schemes = list(
        list(scheme = "https://x/oxford", description = "MLST (Oxford)"),
        list(scheme = "https://x/pasteur", description = "MLST (Pasteur)")
      )))
    }
    if (endpoint == "https://x/oxford") {
      return(list(
        profiles_csv = "https://x/oxford/profiles_csv",
        last_added = "2026-07-24",
        loci = as.list(paste0("https://x/loci/", c("gltA", "gyrB", "gdhB", "recA", "cpn60", "gpi", "rpoD")))
      ))
    }
    if (endpoint == "https://x/pasteur") {
      return(list(
        profiles_csv = "https://x/pasteur/profiles_csv",
        last_added = "2026-07-13",
        loci = as.list(paste0("https://x/loci/", c("cpn60", "fusA", "gltA", "pyrG", "recA", "rplB", "rpoB")))
      ))
    }
    stop("unexpected endpoint: ", endpoint)
  }
}

test_that("resolve_mlst_scheme breaks a same-database naming tie by scheme source", {
  # The default "Scheme source" (PubMLST) keeps the non-Pasteur convention.
  default_pick <- mlst_repo$resolve_mlst_scheme(
    "Acinetobacter baumannii",
    repos = c("pubmlst", "pasteur"),
    fetch = fake_fetch_tied()
  )
  expect_true(default_pick$resolved)
  expect_identical(default_pick$scheme, "MLST (Oxford)")

  # Choosing "Pasteur" as scheme source picks the Pasteur-named scheme even
  # though it is hosted inside the PubMLST database, not a separate one.
  pasteur_pick <- mlst_repo$resolve_mlst_scheme(
    "Acinetobacter baumannii",
    repos = c("pasteur", "pubmlst"),
    fetch = fake_fetch_tied()
  )
  expect_true(pasteur_pick$resolved)
  expect_identical(pasteur_pick$repository, "pubmlst")
  expect_identical(pasteur_pick$scheme, "MLST (Pasteur)")
})

test_that("resolve_mlst_scheme reports failure rather than returning NULL", {
  # Escherichia coli is not part of the fake repository fixture at all, so both
  # repositories come back with "no database for this species".
  miss <- mlst_repo$resolve_mlst_scheme("Escherichia coli", fetch = fake_fetch())
  expect_false(miss$resolved)
  expect_identical(miss$species, "Escherichia coli")
  expect_true(all(nzchar(miss$attempts)))
  expect_setequal(names(miss$attempts), c("pubmlst", "pasteur"))

  none <- mlst_repo$resolve_mlst_scheme(NA_character_, fetch = fake_fetch())
  expect_false(none$resolved)
  expect_identical(none$attempts, character(0))

  # A repository that is down must not fail the typing run - it becomes an
  # attempt reason instead of an error escaping the call.
  down <- mlst_repo$resolve_mlst_scheme(
    "Enterococcus faecium",
    fetch = function(endpoint) stop("connection refused")
  )
  expect_false(down$resolved)
  expect_true(any(grepl("connection refused", down$attempts, fixed = TRUE)))
})

test_that("describe_resolution reports a resolved scheme", {
  note <- mlst_repo$describe_resolution(list(
    resolved = TRUE,
    species = "Enterococcus faecium",
    repository = "pubmlst",
    database = "Enterococcus faecium",
    scheme = "MLST",
    loci = c("atpA", "ddl", "gdh")
  ))
  expect_identical(
    note,
    "Classical MLST scheme resolved: pubmlst / Enterococcus faecium (MLST), 3 loci"
  )
})

test_that("describe_resolution lists every attempt behind an unresolved species", {
  # This is the failure the internal event log already carries per repository -
  # formatted here for the run's own transcript, where a user actually looks.
  note <- mlst_repo$describe_resolution(list(
    resolved = FALSE,
    species = "Acinetobacter baumannii",
    attempts = c(
      pubmlst = "2 schemes match: MLST (Oxford) (7 loci); MLST (Pasteur) (7 loci)",
      pasteur = "no database for this species"
    )
  ))
  expect_true(grepl("no single scheme resolved for 'Acinetobacter baumannii'", note, fixed = TRUE))
  expect_true(grepl("pubmlst: 2 schemes match", note, fixed = TRUE))
  expect_true(grepl("pasteur: no database for this species", note, fixed = TRUE))
})

test_that("describe_resolution reports a species that never got looked up", {
  expect_identical(
    mlst_repo$describe_resolution(list(resolved = FALSE, species = NA_character_, attempts = character(0))),
    "Classical MLST: no resolvable species for this scheme (skipping ST calls for this run)."
  )
  expect_identical(
    mlst_repo$describe_resolution(NULL),
    "Classical MLST: no resolvable species for this scheme (skipping ST calls for this run)."
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
