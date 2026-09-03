box::use(
  testthat[
    expect_equal,
    expect_false,
    expect_identical,
    expect_gt,
    expect_lt,
    expect_s4_class,
    expect_named,
    expect_s3_class,
    expect_true,
    test_that
  ],
  utils[tail],
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
    isolate = c(
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
    isolate = c(
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
    c("isolate", "gene_symbol", "element_type", "class", "pct_identity") %in%
      names(hits)
  ))

  sections <- amr_plot$load_amr_sections("/no/such/file.db")
  expect_s3_class(sections, "data.frame")
  expect_identical(nrow(sections), 0L)
  expect_named(
    sections,
    c("isolate", "section", "drug_class", "genes")
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
  expect_false("ISO-6" %in% kept$isolate)
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
  expect_true("blaTEM" %in% kept$gene_symbol[kept$isolate == "ISO-1"])
})

test_that("amr_threshold_bounds fits the slider to the reported range", {
  b <- amr_plot$amr_threshold_bounds(c(87.3, 99.9, 92.0))

  # Floored/ceilinged so the fitted bounds never exclude the data that set
  # them, and the value starts at the floor - "no filter" for this range.
  expect_identical(b, list(min = 87, max = 100, value = 87))
})

test_that("amr_threshold_bounds ignores NA metrics", {
  b <- amr_plot$amr_threshold_bounds(c(NA, 95, NA, 100))

  expect_identical(b, list(min = 95, max = 100, value = 95))
})

test_that("amr_threshold_bounds falls back to 0-100 with nothing numeric", {
  expect_identical(
    amr_plot$amr_threshold_bounds(c(NA_real_, NA_real_)),
    list(min = 0, max = 100, value = 0)
  )
  expect_identical(
    amr_plot$amr_threshold_bounds(numeric(0)),
    list(min = 0, max = 100, value = 0)
  )
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
  # AMRFinderPlus shouts its vocabulary; the group is what a reader sees.
  expect_identical(
    meta$group[meta$gene == "blaTEM"],
    "Beta-lactam"
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
      isolate = "ISO-1",
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

  expect_true("Resistance - Beta-lactam" %in% names(choices))
  expect_identical(choices[["Resistance - Beta-lactam"]], "blaTEM")
  # A virulence gene has no drug class, so its group is just the element type —
  # not "Virulence - Virulence".
  expect_identical(choices[["Virulence"]], "fimH")
  expect_identical(
    sort(unlist(choices, use.names = FALSE)),
    sort(unique(hits_fixture()$gene_symbol))
  )
})

# abritamr's rollup for the genes the hits fixture actually carries, so the two
# vocabularies can be told apart: it files blaTEM under a carbapenemase heading
# where AMRFinderPlus says only "Beta-lactam", and never mentions mexE at all.
curated_fixture <- function() {
  data.frame(
    isolate = c("ISO-1", "ISO-2", "ISO-1", "ISO-2", "ISO-3"),
    section = c("matches", "matches", "matches", "partials", "matches"),
    drug_class = c(
      "Carbapenemase", "Carbapenemase", "Quinolone", "Quinolone", "Sulfonamide"
    ),
    genes = c("blaTEM", "blaTEM*", "gyrA", "gyrA^", "sul1"),
    stringsAsFactors = FALSE
  )
}

test_that("the curated vocabulary files a gene where the browser files it", {
  mat <- amr_plot$amr_presence_matrix(
    hits_fixture(),
    ISOLATES,
    sections = curated_fixture(),
    vocabulary = "rollup"
  )
  meta <- attr(mat, "genes")

  # The mismatch this vocabulary exists to settle: AMRFinderPlus calls blaTEM a
  # beta-lactam, abritamr a carbapenemase, and the database browser shows the
  # latter.
  expect_identical(meta$group[meta$gene == "blaTEM"], "Carbapenemase")
  expect_identical(meta$group[meta$gene == "sul1"], "Sulfonamide")
  # abritamr's quality flags travel on its gene names, the hit table's do not.
  expect_identical(meta$group[meta$gene == "gyrA"], "Quinolone")
  # A hit the rollup never summarised keeps AMRFinder's class rather than
  # falling out of the panel.
  expect_identical(meta$group[meta$gene == "mexE"], "Efflux")
})

test_that("the AMRFinder vocabulary ignores the rollup entirely", {
  mat <- amr_plot$amr_presence_matrix(
    hits_fixture(),
    ISOLATES,
    sections = curated_fixture(),
    vocabulary = "amrfinder"
  )
  meta <- attr(mat, "genes")

  expect_identical(meta$group[meta$gene == "blaTEM"], "Beta-lactam")
  expect_identical(meta$group[meta$gene == "gyrA"], "Quinolone")
})

test_that("the curated vocabulary without a rollup falls back throughout", {
  with_rollup <- attr(
    amr_plot$amr_presence_matrix(hits_fixture(), ISOLATES, vocabulary = "rollup"),
    "genes"
  )
  amrfinder <- attr(
    amr_plot$amr_presence_matrix(
      hits_fixture(),
      ISOLATES,
      vocabulary = "amrfinder"
    ),
    "genes"
  )
  expect_identical(with_rollup$group, amrfinder$group)
})

test_that("a gene the rollup files two ways takes the commoner class", {
  split_rollup <- data.frame(
    isolate = c("ISO-1", "ISO-2", "ISO-4"),
    section = "matches",
    drug_class = c("Carbapenemase", "Carbapenemase", "AmpC"),
    genes = "blaTEM",
    stringsAsFactors = FALSE
  )
  meta <- attr(
    amr_plot$amr_presence_matrix(
      hits_fixture(),
      ISOLATES,
      sections = split_rollup
    ),
    "genes"
  )
  expect_identical(meta$group[meta$gene == "blaTEM"], "Carbapenemase")
})

test_that("amr_gene_choices heads the picker in the vocabulary it is given", {
  choices <- amr_plot$amr_gene_choices(
    hits_fixture(),
    curated_fixture(),
    "rollup"
  )
  expect_identical(choices[["Resistance - Carbapenemase"]], "blaTEM")
  expect_false("Resistance - Beta-lactam" %in% names(choices))
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

strip_fixture <- function(label = "Country", palette = "Set1") {
  list(
    field = "country",
    label = label,
    palette = palette,
    continuous = FALSE,
    values = stats::setNames(rep(c("DE", "FR", "IT"), 2), ISOLATES)
  )
}

test_that("every column arrangement of the gene heatmap renders", {
  mat <- amr_plot$amr_presence_matrix(hits_fixture(), ISOLATES)

  for (grouping in c("element", "class", "cluster", "none")) {
    plot <- amr_plot$amr_as_ggplot(
      amr_plot$build_amr_heatmap(
        mat,
        list(
          column_grouping = grouping,
          show_class_anno = TRUE,
          anno_layers = list(strip_fixture())
        )
      )
    )
    expect_s3_class(plot, "ggplot")

    file <- local_tempfile(fileext = ".png")
    amr_plot$render_amr_png(plot, file, 600, 400)
    expect_true(file.exists(file))
  }
})

test_that("several element types are drawn as separate panels", {
  mat <- amr_plot$amr_presence_matrix(hits_fixture(), ISOLATES)
  # The fixture carries AMR and VIRULENCE hits, so this is a two-panel screen.
  expect_s4_class(amr_plot$build_amr_heatmap(mat, list()), "HeatmapList")

  # One element type is one panel, not a list of one.
  amr_only <- amr_plot$amr_presence_matrix(
    hits_fixture()[hits_fixture()$element_type == "AMR", , drop = FALSE],
    ISOLATES
  )
  expect_s4_class(amr_plot$build_amr_heatmap(amr_only, list()), "Heatmap")
})

test_that("several annotation strips travel in one rowAnnotation", {
  mat <- amr_plot$amr_presence_matrix(hits_fixture(), ISOLATES)
  host <- strip_fixture("Host", "Set2")
  host$field <- "host"
  host$values <- stats::setNames(rep(c("Human", "Env"), 3), ISOLATES)

  anno <- impl$.row_annotation(
    mat,
    list(strip_fixture(), host),
    "#000000",
    impl$.legend_gp("#000000", 9)
  )
  expect_identical(length(anno@anno_list), 2L)
  expect_named(anno@anno_list, c("Country", "Host"))
})

test_that("a continuous strip gets a ramp rather than a colour per value", {
  mat <- amr_plot$amr_presence_matrix(hits_fixture(), ISOLATES)
  layer <- list(
    field = "depth",
    label = "Depth",
    palette = "viridis",
    continuous = TRUE,
    values = stats::setNames(seq_along(ISOLATES) * 1.5, ISOLATES)
  )
  spec <- impl$.strip_spec(layer, mat)
  expect_true(is.function(spec$col))
  expect_true(is.numeric(spec$values))
})

test_that("a strip's unrecorded category is coloured last, never first", {
  # "NA" sorts wherever its label happens to fall — ahead of "Urine", behind
  # "Blood" — so the one category carrying no information could head the strip's
  # legend and take the palette's first colour.
  expect_identical(
    amr_plot$amr_strip_levels(c("Urine", NA, "Blood", "NA", "Wound")),
    c("Blood", "Urine", "Wound", amr_plot$AMR_MISSING_LABEL)
  )
  expect_identical(amr_plot$amr_strip_levels(c("b", "a")), c("a", "b"))

  mat <- amr_plot$amr_presence_matrix(hits_fixture(), ISOLATES)
  layer <- strip_fixture()
  layer$values <- stats::setNames(
    c("Urine", NA, "Blood", "", "Wound", "Urine")[seq_along(ISOLATES)],
    ISOLATES
  )
  spec <- impl$.strip_spec(layer, mat)

  # The colour vector's own order is what ComplexHeatmap lists the keys in.
  expect_identical(
    tail(names(spec$col), 1L),
    amr_plot$AMR_MISSING_LABEL
  )
  # And the blank came through as that category rather than as a gap.
  expect_true(amr_plot$AMR_MISSING_LABEL %in% spec$values)
})

test_that("an empty strip is dropped rather than drawn blank", {
  mat <- amr_plot$amr_presence_matrix(hits_fixture(), ISOLATES)
  gp <- impl$.legend_gp("#000000", 9)
  expect_null(impl$.row_annotation(mat, list(), "#000000", gp))
  expect_null(impl$.row_annotation(mat, NULL, "#000000", gp))
})

# --- automatic layout --------------------------------------------------------

test_that("the fit grows the plot taller as isolates are added", {
  small <- amr_plot$amr_auto_layout(10, 20)
  large <- amr_plot$amr_auto_layout(400, 20)
  expect_gt(large$aspect, small$aspect)
  # ... and stops, rather than producing a page nobody can scroll.
  expect_lt(large$aspect, 3)
})

test_that("the fit shrinks the column labels as columns are added", {
  wide <- amr_plot$amr_auto_layout(50, 300)
  narrow <- amr_plot$amr_auto_layout(50, 10)
  expect_lt(wide$fontsize_col, narrow$fontsize_col)
  # Cell borders come off once a cell is smaller than the border is wide.
  expect_identical(wide$grid_width, 0)
  expect_gt(narrow$grid_width, 0)
})

test_that("block titles turn vertical only when they do not fit", {
  # Eleven drug classes over thirty-two columns: "FLUOROQUINOLONE" is eight
  # times wider than the single column it sits over. This is the overlap the
  # rotation exists for.
  tight <- amr_plot$amr_auto_layout(
    100, 32,
    block_titles = c("AMINOGLYCOSIDE", "BETA-LACTAM", "FLUOROQUINOLONE"),
    block_cols = c(4L, 8L, 1L)
  )
  expect_identical(tight$title_rot, 90)

  roomy <- amr_plot$amr_auto_layout(
    100, 12,
    block_titles = c("AMR", "VIR"),
    block_cols = c(6L, 6L)
  )
  expect_identical(roomy$title_rot, 0)

  # Nothing to title is not a reason to rotate.
  expect_identical(amr_plot$amr_auto_layout(100, 12)$title_rot, 0)
})

test_that("a reader's aspect ratio is used as given and re-solves the sizes", {
  fitted <- amr_plot$amr_auto_layout(250, 32)
  taller <- amr_plot$amr_auto_layout(250, 32, aspect = 6)
  expect_identical(taller$aspect, 6)
  # A taller figure is more room per row, so the isolate labels grow with it
  # rather than the extra height becoming whitespace.
  expect_gt(taller$fontsize_row, fitted$fontsize_row)

  # Nonsense is ignored in favour of the fit rather than drawn.
  expect_identical(
    amr_plot$amr_auto_layout(250, 32, aspect = 0)$aspect,
    fitted$aspect
  )
  expect_identical(
    amr_plot$amr_auto_layout(250, 32, aspect = NA_real_)$aspect,
    fitted$aspect
  )
})

test_that("the fit reports when the row labels are past reading", {
  expect_true(amr_plot$amr_auto_layout(20, 20, show_row_names = TRUE)$legible)
  expect_false(
    amr_plot$amr_auto_layout(2000, 20, show_row_names = TRUE)$legible
  )
})

test_that("amr_column_blocks names the panels the reader will see", {
  mat <- amr_plot$amr_presence_matrix(hits_fixture(), ISOLATES)

  # Grouped by class, the titles that can collide are the class names.
  by_class <- amr_plot$amr_column_blocks(mat, "class")
  expect_true("Beta-lactam" %in% by_class$titles)
  expect_identical(sum(by_class$cols), ncol(mat))

  # Otherwise they are the element-type panels, in the vocabulary's own order.
  clustered <- amr_plot$amr_column_blocks(mat, "cluster")
  expect_identical(clustered$titles, c("Resistance", "Virulence"))
  expect_identical(sum(clustered$cols), ncol(mat))
})

test_that("one drug-class palette covers every panel of the heatmap", {
  mat <- amr_plot$amr_presence_matrix(hits_fixture(), ISOLATES)
  meta <- attr(mat, "genes")
  cols <- impl$.class_colors(meta, NULL)

  # Every class in the screen, resistance and virulence alike, gets a colour
  # from one tabulation. Fitted per panel each restarted at the palette's first
  # colour, which gave the leading class of every panel the same swatch under a
  # legend that claimed they were different classes.
  expect_identical(sort(names(cols)), sort(unique(meta$group)))
  expect_identical(length(unique(unname(cols))), length(cols))

  # The colour a class draws in does not depend on which panel it lands in.
  resistance <- meta[meta$element_type == "AMR", , drop = FALSE]
  expect_identical(
    impl$.class_colors(meta, NULL)[resistance$group[[1]]],
    cols[resistance$group[[1]]]
  )
})

test_that("each panel keys its own classes under its own element type", {
  mat <- amr_plot$amr_presence_matrix(hits_fixture(), ISOLATES)
  meta <- attr(mat, "genes")
  cols <- impl$.class_colors(meta, NULL)
  gp <- impl$.legend_gp("#000000", 9)
  resistance <- meta[meta$element_type == "AMR", , drop = FALSE]

  anno <- impl$.class_annotation(
    resistance$group,
    cols,
    gp,
    impl$.panel_label(resistance)
  )

  # ComplexHeatmap keys annotation legends by name and folds together every
  # annotation sharing one, so a key per panel needs a name per panel.
  expect_named(anno@anno_list, "Resistance")
  expect_identical(
    anno@anno_list$Resistance@color_mapping@levels,
    sort(unique(resistance$group))
  )
  # Only this panel's classes: the virulence gene is keyed by its own panel.
  expect_false("Virulence" %in% anno@anno_list$Resistance@color_mapping@levels)
})

test_that("split class keys still draw from one screen-wide palette", {
  mat <- amr_plot$amr_presence_matrix(hits_fixture(), ISOLATES)
  meta <- attr(mat, "genes")
  cols <- impl$.class_colors(meta, NULL)
  gp <- impl$.legend_gp("#000000", 9)

  panels <- lapply(c("AMR", "VIRULENCE"), function(et) {
    rows <- meta[meta$element_type == et, , drop = FALSE]
    impl$.class_annotation(rows$group, cols, gp, impl$.panel_label(rows))
  })
  swatches <- lapply(panels, function(a) {
    a@anno_list[[1]]@color_mapping@colors
  })

  # A class keeps the colour the whole-screen tabulation gave it — compared on
  # the RGB alone, since ComplexHeatmap appends an alpha channel of its own.
  expect_identical(
    substring(unname(swatches[[1]][["Beta-lactam"]]), 1, 7),
    unname(cols[["Beta-lactam"]])
  )
  # ... so nothing is reused between one panel's key and the next, which is
  # what a palette fitted per panel could not promise.
  expect_identical(
    length(intersect(swatches[[1]], swatches[[2]])),
    0L
  )
})

test_that("a panel whose only class names the panel drops its key", {
  mat <- amr_plot$amr_presence_matrix(hits_fixture(), ISOLATES)
  meta <- attr(mat, "genes")
  cols <- impl$.class_colors(meta, NULL)
  gp <- impl$.legend_gp("#000000", 9)

  # AMRFinderPlus leaves `class` empty for virulence genes, so the panel's one
  # block is labelled "Virulence" — the same word its key would be titled with.
  virulence <- meta[meta$element_type == "VIRULENCE", , drop = FALSE]
  vir <- impl$.class_annotation(
    virulence$group,
    cols,
    gp,
    impl$.panel_label(virulence)
  )
  resistance <- meta[meta$element_type == "AMR", , drop = FALSE]
  res <- impl$.class_annotation(
    resistance$group,
    cols,
    gp,
    impl$.panel_label(resistance)
  )

  expect_false(vir@anno_list$Virulence@show_legend)
  expect_true(res@anno_list$Resistance@show_legend)
})

test_that(".panel_label names a panel by the element type it holds", {
  meta <- attr(amr_plot$amr_presence_matrix(hits_fixture(), ISOLATES), "genes")

  expect_identical(
    impl$.panel_label(meta[meta$element_type == "AMR", , drop = FALSE]),
    "Resistance"
  )
  expect_identical(
    impl$.panel_label(meta[meta$element_type == "VIRULENCE", , drop = FALSE]),
    "Virulence"
  )
  expect_identical(impl$.panel_label(meta[0, , drop = FALSE]), "Drug class")
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

test_that("a dendrogram depth of zero hides the trees but keeps the order", {
  mat <- amr_plot$amr_presence_matrix(hits_fixture(), ISOLATES)
  hidden <- amr_plot$build_amr_heatmap(
    mat,
    list(column_grouping = "cluster", dend_size = 0)
  )
  # The row order still comes from the clustering, so the panels still line up.
  expect_s3_class(hidden@ht_list[[1]]@row_dend_param$obj, "hclust")
  expect_false(hidden@ht_list[[1]]@row_dend_param$show)
})

test_that("the section filter reaches the class-level prevalence bars", {
  all_sections <- amr_plot$amr_prevalence(
    hits_fixture(), sections_fixture(), ISOLATES,
    level = "class"
  )
  matches <- amr_plot$amr_prevalence(
    hits_fixture(), sections_fixture(), ISOLATES,
    level = "class", keep_sections = "matches"
  )
  expect_true("Adhesion" %in% all_sections$item)
  expect_false("Adhesion" %in% matches$item)
  expect_identical(matches$item, "Beta-lactam")
})

test_that("the canvas height reaches the device the legends are packed on", {
  # The regression this guards: grid.grabExpr() opens a 7x7 inch device unless
  # told otherwise, and ComplexHeatmap packs legends against `par("din")`. Left
  # at that default, a legend column taller than seven inches wrapped into a
  # second column beside the first however tall the real figure was — which is
  # what a heatmap with several annotation strips produces every time.
  #
  # Asserted as a difference rather than as a column count, because the wrap
  # happens inside ComplexHeatmap and leaves nothing addressable behind: drawn
  # to the *same* pixel canvas, a grab told it has seven inches and one told it
  # has forty differ only because the height was honoured. Ignored, both grabs
  # would use 7in and the two images would be identical.
  mat <- amr_plot$amr_presence_matrix(hits_fixture(), ISOLATES)
  strips <- lapply(1:8, function(i) {
    list(
      field = paste0("v", i),
      label = paste("Variable", i),
      palette = "Set1",
      continuous = FALSE,
      values = stats::setNames(rep(c("a", "b", "c"), 2), ISOLATES)
    )
  })
  ht <- amr_plot$build_amr_heatmap(
    mat,
    list(show_class_anno = TRUE, anno_layers = strips)
  )

  render_at <- function(height_in) {
    file <- local_tempfile(fileext = ".png")
    amr_plot$render_amr_png(
      amr_plot$amr_as_ggplot(ht, width_in = 9, height_in = height_in),
      file,
      900,
      2400
    )
    unname(tools::md5sum(file))
  }
  expect_false(identical(render_at(7), render_at(40)))
})

test_that("the prevalence chart renders to a real image", {
  df <- amr_plot$amr_prevalence(
    hits_fixture(),
    sections_fixture(),
    ISOLATES,
    level = "gene"
  )
  plot <- amr_plot$build_amr_prevalence(df, list(n_isolates = 6))
  expect_s3_class(plot, "ggplot")

  # The on-screen path. File export across every offered format is
  # save_plot_export()'s job — see test-viz_export.R.
  file <- local_tempfile(fileext = ".png")
  amr_plot$render_amr_png(plot, file, width_px = 800, height_px = 520)
  expect_true(file.exists(file))
  expect_gt(file.size(file), 0)
})
