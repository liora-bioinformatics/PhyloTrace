box::use(
  testthat[
    expect_equal,
    expect_false,
    expect_identical,
    expect_named,
    expect_s3_class,
    expect_true,
    test_that
  ],
  withr[local_tempfile],
)
box::use(
  app / logic / amr_plot,
)

impl <- attr(amr_plot, "namespace")

# One row per detected element, shaped exactly like `amr_results`. Six isolates:
# ISO-5 carries nothing (it was screened and came back clean, which must survive
# as an all-zero row) and ISO-6 carries only a low-identity hit.
hits_fixture <- function() {
  data.frame(
    souche = c(
      "ISO-1", "ISO-1", "ISO-1",
      "ISO-2", "ISO-2",
      "ISO-3", "ISO-3",
      "ISO-4",
      "ISO-6"
    ),
    gene_symbol = c(
      "blaTEM", "gyrA", "fimH",
      "blaTEM", "sul1",
      "gyrA", "fimH",
      "blaTEM",
      "mexE"
    ),
    element_type = c(
      "AMR", "AMR", "VIRULENCE",
      "AMR", "AMR",
      "AMR", "VIRULENCE",
      "AMR",
      "AMR"
    ),
    element_subtype = "AMR",
    class = c(
      "BETA-LACTAM", "QUINOLONE", NA,
      "BETA-LACTAM", "SULFONAMIDE",
      "QUINOLONE", NA,
      "BETA-LACTAM",
      "EFFLUX"
    ),
    subclass = NA_character_,
    method = "EXACTX",
    pct_identity = c(100, 99.5, 98, 100, 97, 99.5, 98, 100, 82),
    pct_coverage = c(100, 100, 100, 100, 90, 100, 100, 100, 60),
    stringsAsFactors = FALSE
  )
}

# abritamr's curated rollup, shaped like `amr_summary`.
sections_fixture <- function() {
  data.frame(
    souche = c(
      "ISO-1", "ISO-1", "ISO-1",
      "ISO-2", "ISO-2",
      "ISO-3",
      "ISO-4"
    ),
    section = c(
      "matches", "partials", "virulence",
      "matches", "partials",
      "partials",
      "matches"
    ),
    drug_class = c(
      "Beta-lactam", "Quinolone", "Adhesion",
      "Beta-lactam", "Quinolone",
      "Quinolone",
      "Beta-lactam"
    ),
    genes = "x",
    stringsAsFactors = FALSE
  )
}

ISOLATES <- paste0("ISO-", 1:6)

# --- readers -----------------------------------------------------------------

test_that("readers return correctly shaped empty frames without a database", {
  hits <- amr_plot$load_amr_hits(NULL)
  expect_s3_class(hits, "data.frame")
  expect_identical(nrow(hits), 0L)
  expect_true(all(
    c("souche", "gene_symbol", "element_type", "class", "pct_identity") %in%
      names(hits)
  ))

  sections <- amr_plot$load_amr_sections("/no/such/file.db")
  expect_s3_class(sections, "data.frame")
  expect_identical(nrow(sections), 0L)
  expect_named(
    sections,
    c("souche", "section", "drug_class", "genes")
  )

  expect_false(amr_plot$has_amr_data(NULL))
  expect_false(amr_plot$has_amr_data("/no/such/file.db"))
})

test_that("readers see a database that was typed with AMR screening", {
  path <- local_tempfile(fileext = ".db")
  build_db(
    path,
    list(ref = c(acsA = "ACGT"), `ISO-1` = c(acsA = "ACGT")),
    metadata = data.frame(isolate = "ISO-1", stringsAsFactors = FALSE)
  )
  seed_results(path, "ISO-1", classical = FALSE, amr = TRUE)

  expect_true(amr_plot$has_amr_data(path))

  hits <- amr_plot$load_amr_hits(path)
  expect_identical(nrow(hits), 1L)
  expect_identical(hits$gene_symbol, "blaTEST")
  expect_identical(hits$element_type, "AMR")

  sections <- amr_plot$load_amr_sections(path)
  expect_identical(nrow(sections), 1L)
  expect_identical(sections$section, "matches")
})

test_that("a database without screening reads as empty, not as an error", {
  path <- local_tempfile(fileext = ".db")
  build_db(
    path,
    list(ref = c(acsA = "ACGT"), `ISO-1` = c(acsA = "ACGT")),
    metadata = data.frame(isolate = "ISO-1", stringsAsFactors = FALSE)
  )

  expect_false(amr_plot$has_amr_data(path))
  expect_identical(nrow(amr_plot$load_amr_hits(path)), 0L)
  expect_identical(nrow(amr_plot$load_amr_sections(path)), 0L)
})

# --- filtering ---------------------------------------------------------------

test_that("filter_amr_hits keeps only the requested element types", {
  hits <- hits_fixture()

  expect_identical(
    sort(unique(
      amr_plot$filter_amr_hits(hits, element_types = "AMR")$element_type
    )),
    "AMR"
  )
  expect_identical(nrow(amr_plot$filter_amr_hits(hits, "STRESS")), 0L)
  # NULL / empty means "no restriction", not "nothing".
  expect_identical(nrow(amr_plot$filter_amr_hits(hits)), nrow(hits))
})

test_that("the identity and coverage floors drop only weak hits", {
  hits <- hits_fixture()

  kept <- amr_plot$filter_amr_hits(hits, min_identity = 90)
  expect_false("ISO-6" %in% kept$souche)
  expect_identical(nrow(kept), nrow(hits) - 1L)

  expect_identical(
    nrow(amr_plot$filter_amr_hits(hits, min_coverage = 95)),
    nrow(hits) - 2L
  )
})

test_that("hits with no measured percentages survive a floor", {
  # Point mutations report neither percentage; a floor filters what was
  # measured, it does not demand that everything be measured.
  hits <- hits_fixture()
  hits$pct_identity[1] <- NA_real_
  hits$pct_coverage[1] <- NA_real_

  kept <- amr_plot$filter_amr_hits(hits, min_identity = 99, min_coverage = 99)
  expect_true("blaTEM" %in% kept$gene_symbol[kept$souche == "ISO-1"])
})

# --- presence matrix ---------------------------------------------------------

test_that("amr_presence_matrix keeps every isolate, including clean ones", {
  mat <- amr_plot$amr_presence_matrix(hits_fixture(), ISOLATES)

  expect_identical(rownames(mat), ISOLATES)
  expect_identical(unname(rowSums(mat)), c(3, 2, 2, 1, 0, 1))
  expect_true(all(mat %in% c(0L, 1L)))
})

test_that("amr_presence_matrix drops genes nothing in the selection carries", {
  hits <- hits_fixture()
  genes <- unique(hits$gene_symbol)

  # Only ISO-6 carries mexE, so selecting every gene but only the other
  # isolates leaves that column all-zero — and drop_empty removes it.
  mat <- amr_plot$amr_presence_matrix(hits, c("ISO-1", "ISO-2"), genes = genes)
  expect_false("mexE" %in% colnames(mat))

  kept <- amr_plot$amr_presence_matrix(
    hits,
    c("ISO-1", "ISO-2"),
    genes = genes,
    drop_empty = FALSE
  )
  expect_true("mexE" %in% colnames(kept))
  expect_equal(unname(colSums(kept)[["mexE"]]), 0)
})

test_that("without an explicit gene list the columns follow the isolates", {
  # No selection means "every gene these isolates carry", so a gene only ever
  # detected elsewhere is not a column in the first place.
  mat <- amr_plot$amr_presence_matrix(hits_fixture(), c("ISO-1", "ISO-2"))
  expect_identical(sort(colnames(mat)), c("blaTEM", "fimH", "gyrA", "sul1"))
})

test_that("an explicit gene selection restricts the columns", {
  mat <- amr_plot$amr_presence_matrix(
    hits_fixture(),
    ISOLATES,
    genes = c("blaTEM", "gyrA")
  )
  expect_identical(colnames(mat), c("blaTEM", "gyrA"))
})

test_that("the gene metadata attribute lines up with the columns", {
  mat <- amr_plot$amr_presence_matrix(hits_fixture(), ISOLATES)
  meta <- attr(mat, "genes")

  expect_identical(meta$gene, colnames(mat))
  expect_identical(
    meta$group[meta$gene == "blaTEM"],
    "BETA-LACTAM"
  )
  # AMRFinderPlus leaves `class` empty for virulence genes, so they group under
  # their element type rather than under a made-up class.
  expect_identical(meta$group[meta$gene == "fimH"], "Virulence")
  expect_identical(meta$element_type[meta$gene == "fimH"], "VIRULENCE")
})

test_that("a screen with no hits still yields one row per isolate", {
  mat <- amr_plot$amr_presence_matrix(amr_plot$load_amr_hits(NULL), ISOLATES)
  expect_identical(nrow(mat), length(ISOLATES))
  expect_identical(ncol(mat), 0L)
})

# --- class matrix ------------------------------------------------------------

test_that("amr_class_matrix ranks a confident call above a partial one", {
  mat <- amr_plot$amr_class_matrix(sections_fixture(), ISOLATES)

  expect_identical(mat["ISO-1", "Beta-lactam"], 2L)
  expect_identical(mat["ISO-1", "Quinolone"], 1L)
  expect_identical(mat["ISO-3", "Quinolone"], 1L)
  # Nothing reported for ISO-5 at all.
  expect_identical(unname(sum(mat["ISO-5", ])), 0L)
})

test_that("the strongest call wins when an isolate appears in two sections", {
  sections <- sections_fixture()
  sections <- rbind(
    sections,
    data.frame(
      souche = "ISO-1",
      section = "matches",
      drug_class = "Quinolone",
      genes = "x",
      stringsAsFactors = FALSE
    )
  )
  mat <- amr_plot$amr_class_matrix(sections, ISOLATES)
  expect_identical(mat["ISO-1", "Quinolone"], 2L)
})

test_that("the virulence attribute flags only virulence-only columns", {
  mat <- amr_plot$amr_class_matrix(sections_fixture(), ISOLATES)
  virulence <- attr(mat, "virulence")

  expect_identical(length(virulence), ncol(mat))
  expect_true(virulence[match("Adhesion", colnames(mat))])
  expect_false(virulence[match("Beta-lactam", colnames(mat))])
})

test_that("keep_sections restricts which calls are counted", {
  mat <- amr_plot$amr_class_matrix(
    sections_fixture(),
    ISOLATES,
    keep_sections = "matches"
  )
  expect_identical(colnames(mat), "Beta-lactam")
  expect_identical(unname(sum(mat["ISO-3", ])), 0L)
})

# --- prevalence --------------------------------------------------------------

test_that("gene prevalence counts distinct isolates, ranked", {
  df <- amr_plot$amr_prevalence(
    hits_fixture(),
    sections_fixture(),
    ISOLATES,
    level = "gene"
  )

  expect_named(df, c("item", "group", "n", "frac"))
  expect_identical(df$item[1], "blaTEM")
  expect_identical(df$n[1], 3L)
  expect_equal(df$frac[1], 0.5)
  expect_true(all(diff(df$n) <= 0))
  expect_identical(df$group[df$item == "fimH"], "Virulence")
})

test_that("class prevalence labels each class by its strongest section", {
  df <- amr_plot$amr_prevalence(
    hits_fixture(),
    sections_fixture(),
    ISOLATES,
    level = "class"
  )

  expect_identical(df$group[df$item == "Beta-lactam"], "Matches")
  expect_identical(df$group[df$item == "Quinolone"], "Partials")
  expect_identical(df$group[df$item == "Adhesion"], "Virulence")
})

test_that("prevalence truncates to the top n", {
  df <- amr_plot$amr_prevalence(
    hits_fixture(),
    sections_fixture(),
    ISOLATES,
    level = "gene",
    top_n = 2
  )
  expect_identical(nrow(df), 2L)
})

test_that("prevalence over a screen with nothing in it is empty, not an error", {
  empty <- amr_plot$load_amr_hits(NULL)
  df <- amr_plot$amr_prevalence(empty, empty, ISOLATES, level = "gene")
  expect_identical(nrow(df), 0L)
})

# --- gene choices ------------------------------------------------------------

test_that("amr_gene_choices groups genes by element type and drug class", {
  choices <- amr_plot$amr_gene_choices(hits_fixture())

  expect_true("Resistance - BETA-LACTAM" %in% names(choices))
  expect_identical(choices[["Resistance - BETA-LACTAM"]], "blaTEM")
  # A virulence gene has no drug class, so its group is just the element type —
  # not "Virulence - Virulence".
  expect_identical(choices[["Virulence"]], "fimH")
  expect_identical(
    sort(unlist(choices, use.names = FALSE)),
    sort(unique(hits_fixture()$gene_symbol))
  )
})

test_that("amr_gene_choices on an empty screen is an empty list", {
  expect_identical(amr_plot$amr_gene_choices(amr_plot$load_amr_hits(NULL)), list())
})

# --- sizing ------------------------------------------------------------------

test_that("amr_fit_fontsize steps down as the matrix grows", {
  sizes <- vapply(c(1, 15, 25, 40, 300), amr_plot$amr_fit_fontsize, numeric(1))
  expect_true(all(diff(sizes) < 0))
  expect_identical(amr_plot$amr_fit_fontsize(1), 14)
  expect_identical(amr_plot$amr_fit_fontsize(1000), 5)
})

# --- clustering guards -------------------------------------------------------

test_that("clustering is skipped when there is too little to cluster", {
  mat <- amr_plot$amr_presence_matrix(hits_fixture(), c("ISO-1", "ISO-2"))
  expect_false(impl$.dendrogram(mat, TRUE, "binary", "average"))
  # ... and when it is switched off.
  full <- amr_plot$amr_presence_matrix(hits_fixture(), ISOLATES)
  expect_false(impl$.dendrogram(full, FALSE, "binary", "average"))
})

test_that("binary distance between two clean isolates does not break hclust", {
  # dist(binary) of two all-zero rows is 0/0 = NaN, which hclust refuses. Two
  # isolates with nothing detected are identical, so that distance is 0.
  hits <- hits_fixture()
  mat <- amr_plot$amr_presence_matrix(
    hits,
    c("ISO-1", "ISO-2", "ISO-5", "ISO-7")
  )
  dend <- impl$.dendrogram(mat, TRUE, "binary", "average")
  expect_s3_class(dend, "hclust")
})

# --- plot building -----------------------------------------------------------

test_that("every column arrangement of the gene heatmap renders", {
  mat <- amr_plot$amr_presence_matrix(hits_fixture(), ISOLATES)
  anno <- stats::setNames(rep(c("DE", "FR", "IT"), 2), ISOLATES)

  for (grouping in c("element", "class", "cluster", "none")) {
    plot <- amr_plot$amr_as_ggplot(
      amr_plot$build_amr_heatmap(
        mat,
        list(
          column_grouping = grouping,
          show_class_anno = TRUE,
          anno_values = anno,
          anno_label = "Country"
        )
      )
    )
    expect_s3_class(plot, "ggplot")

    file <- local_tempfile(fileext = ".png")
    amr_plot$render_amr_png(plot, file, 600, 400)
    expect_true(file.exists(file))
  }
})

test_that("the drug-class heatmap renders clustered and split", {
  mat <- amr_plot$amr_class_matrix(sections_fixture(), ISOLATES)

  for (grouping in c("none", "cluster")) {
    plot <- amr_plot$amr_as_ggplot(
      amr_plot$build_amr_class_heatmap(mat, list(column_grouping = grouping))
    )
    expect_s3_class(plot, "ggplot")
  }
})

test_that("the prevalence chart renders and exports in every offered format", {
  df <- amr_plot$amr_prevalence(
    hits_fixture(),
    sections_fixture(),
    ISOLATES,
    level = "gene"
  )
  plot <- amr_plot$build_amr_prevalence(df, list(n_isolates = 6))
  expect_s3_class(plot, "ggplot")

  for (filetype in c("png", "jpeg", "pdf", "svg")) {
    file <- local_tempfile(fileext = paste0(".", filetype))
    amr_plot$save_amr_plot(plot, file, filetype)
    expect_true(file.exists(file))
  }
})
