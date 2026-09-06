box::use(
  ComplexHeatmap,
  grid[convertHeight, convertWidth, grid.grabExpr, grobHeight],
  stats[setNames],
  testthat[
    expect_equal,
    expect_false,
    expect_gt,
    expect_gte,
    expect_identical,
    expect_length,
    expect_lt,
    expect_lte,
    expect_named,
    expect_null,
    expect_s3_class,
    expect_s4_class,
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

test_that("amr_top_n_bounds caps the slider at how many items there are", {
  b <- amr_plot$amr_top_n_bounds(8, default = 30L)
  expect_identical(b, list(min = 5L, max = 8L, value = 8L, step = 1L))

  # A screen with plenty of genes/classes keeps the old floor and default.
  wide <- amr_plot$amr_top_n_bounds(300, default = 30L)
  expect_identical(wide$min, 5L)
  expect_identical(wide$max, 300L)
  expect_identical(wide$value, 30L)
  expect_identical(wide$step, 5L)
})

test_that("amr_top_n_bounds keeps the reader's own value, only clamping it", {
  # Fits inside the new range untouched.
  expect_identical(amr_plot$amr_top_n_bounds(50, current = 20L)$value, 20L)
  # Past the new ceiling, clamps down to it rather than snapping to default.
  expect_identical(amr_plot$amr_top_n_bounds(10, current = 40L)$value, 10L)
  # Below the new floor (a screen shrunk to fewer items than 5), clamps up.
  expect_identical(amr_plot$amr_top_n_bounds(3, current = 1L)$value, 3L)
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

# --- gene call confidence -----------------------------------------------------

test_that("amr_method_confidence ranks AMRFinderPlus's own method hierarchy", {
  expect_identical(
    amr_plot$amr_method_confidence(
      c("ALLELE", "EXACTX", "BLAST", "PARTIAL_CONTIG_ENDX", "HMM")
    ),
    c(4L, 4L, 3L, 2L, 1L)
  )
  # Case and stray whitespace from the database do not change the call.
  expect_identical(amr_plot$amr_method_confidence(" exact "), 4L)
  # A method this table does not recognise (or none at all) reads as a
  # confident hit rather than being dropped to the floor.
  expect_identical(amr_plot$amr_method_confidence("SOMETHING_NEW"), 3L)
  expect_identical(amr_plot$amr_method_confidence(NA_character_), 3L)
})

test_that("the presence matrix carries a same-shape confidence attribute", {
  mat <- amr_plot$amr_presence_matrix(hits_fixture(), ISOLATES)
  conf <- attr(mat, "confidence")
  # A bare copy of mat's values, stripped of its own "genes"/"confidence"
  # attributes, which `mat * 4L` would otherwise carry along too.
  bare <- mat
  attr(bare, "genes") <- NULL
  attr(bare, "confidence") <- NULL

  expect_identical(dim(conf), dim(mat))
  expect_identical(dimnames(conf), dimnames(mat))
  # Every fixture hit is method "EXACTX" (rank 4), so confidence and presence
  # agree everywhere: 4 where present, 0 where absent.
  expect_identical(conf, bare * 4L)
})

test_that("a weaker duplicate hit in the same cell does not pull the rank down", {
  hits <- rbind(
    hits_fixture(),
    data.frame(
      isolate = "ISO-1",
      gene_symbol = "blaTEM",
      element_type = "AMR",
      element_subtype = "AMR",
      class = "BETA-LACTAM",
      subclass = NA_character_,
      method = "PARTIAL",
      pct_identity = 91,
      pct_coverage = 70,
      stringsAsFactors = FALSE
    )
  )
  conf <- attr(amr_plot$amr_presence_matrix(hits, ISOLATES), "confidence")
  # ISO-1/blaTEM already had an EXACTX (rank 4) hit; the second, weaker PARTIAL
  # (rank 2) hit for the same cell does not overwrite it.
  expect_identical(conf["ISO-1", "blaTEM"], 4L)
})

test_that("amr_confidence_frame is a wide isolate-keyed frame of the tiers", {
  frame <- amr_plot$amr_confidence_frame(hits_fixture(), ISOLATES)

  expect_identical(frame$isolate, ISOLATES)
  # One factor column per gene present, levelled on the shared five tiers.
  expect_setequal(
    setdiff(names(frame), "isolate"),
    c("blaTEM", "gyrA", "fimH", "sul1", "mexE")
  )
  expect_identical(levels(frame$blaTEM), amr_plot$AMR_CONFIDENCE_STATES)
  # Every fixture hit is EXACTX (Perfect); an isolate with no hit reads Absent.
  expect_identical(
    as.character(frame$blaTEM),
    c("Perfect", "Perfect", "Absent", "Perfect", "Absent", "Absent")
  )
  # mexE is a low-identity EXACTX hit for ISO-6 only.
  expect_identical(
    as.character(frame$mexE),
    c("Absent", "Absent", "Absent", "Absent", "Absent", "Perfect")
  )
})

test_that("amr_confidence_frame keeps requested genes even when all-absent", {
  frame <- amr_plot$amr_confidence_frame(
    amr_plot$load_amr_hits(NULL),
    ISOLATES,
    genes = c("blaKPC", "vanA")
  )
  expect_identical(names(frame), c("isolate", "blaKPC", "vanA"))
  expect_true(all(vapply(
    frame[c("blaKPC", "vanA")],
    function(col) all(as.character(col) == "Absent"),
    logical(1)
  )))
})

test_that("amr_confidence_palette runs through the reader's own four colors", {
  pal <- amr_plot$amr_confidence_palette("#EFEFEF", "#E5C494", "#8C6E3D", "#000000")
  expect_identical(names(pal), amr_plot$AMR_CONFIDENCE_STATES)
  # The four configured colors land exactly on Absent, Partial, Strong and
  # Perfect; Putative is the one tier without its own picker, and comes back
  # as a blend of Absent and Partial rather than one of the four verbatim.
  expect_identical(unname(pal["Absent"]), "#EFEFEF")
  expect_identical(unname(pal["Partial"]), "#E5C494")
  expect_identical(unname(pal["Strong"]), "#8C6E3D")
  expect_identical(unname(pal["Perfect"]), "#000000")
  expect_false(unname(pal["Putative"]) %in% c("#EFEFEF", "#E5C494"))
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

test_that("the AMRFinderPlus vocabulary counts classes off amr_results, not the rollup", {
  # sections_fixture() is passed but must be ignored entirely under this
  # vocabulary — passing NULL instead must not change the result.
  df <- amr_plot$amr_prevalence(
    hits_fixture(),
    sections_fixture(),
    ISOLATES,
    level = "class",
    vocabulary = "amrfinder"
  )
  same <- amr_plot$amr_prevalence(
    hits_fixture(),
    NULL,
    ISOLATES,
    level = "class",
    vocabulary = "amrfinder"
  )
  expect_identical(df, same)

  # Filed straight off amr_results' own class field (Title Case via
  # amr_class_label()), not abritamr's curated rollup - "gyrA" is filed under
  # its raw AMRFinder class here, same as the gene heatmap's own AMRFinderPlus
  # vocabulary would file it.
  expect_true("Quinolone" %in% df$item)
  # fimH carries no AMRFinder class at all, so it falls back to its element
  # type the same way amr_gene_meta()'s own fallback chain does.
  expect_identical(df$group[df$item == "Virulence"], "Virulence")
  expect_identical(df$group[df$item == "Beta-lactam"], "Resistance")
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
    values = setNames(rep(c("DE", "FR", "IT"), 2), ISOLATES)
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
  host$values <- setNames(rep(c("Human", "Env"), 3), ISOLATES)

  anno <- impl$.row_annotation(
    mat,
    list(strip_fixture(), host),
    "#000000",
    impl$.legend_gp("#000000", 9)
  )
  expect_identical(length(anno$anno@anno_list), 2L)
  expect_named(anno$anno@anno_list, c("Country", "Host"))
  # Each strip's own legend travels alongside the annotation rather than being
  # left for ComplexHeatmap to collect - see build_amr_heatmap's extra_legends.
  expect_length(anno$legends, 2L)
})

test_that("a continuous strip gets a ramp rather than a colour per value", {
  mat <- amr_plot$amr_presence_matrix(hits_fixture(), ISOLATES)
  layer <- list(
    field = "depth",
    label = "Depth",
    palette = "viridis",
    continuous = TRUE,
    values = setNames(seq_along(ISOLATES) * 1.5, ISOLATES)
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
  layer$values <- setNames(
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
  expect_null(impl$.row_annotation(mat, list(), "#000000", gp)$anno)
  expect_null(impl$.row_annotation(mat, NULL, "#000000", gp)$anno)
  expect_length(impl$.row_annotation(mat, list(), "#000000", gp)$legends, 0L)
})

# --- explicit legend ordering -------------------------------------------------

# Every text label under a grob, walked recursively - used where a grob's own
# text is the only observable record of something the S4 object's slots do
# not carry (a Legend's title; whether ComplexHeatmap auto-titled a drawn
# column split).
.grob_texts <- function(g) {
  out <- character(0)
  if (inherits(g, "text")) {
    out <- c(out, as.character(g$label))
  }
  for (ch in c(g$children, g$grobs)) {
    out <- c(out, .grob_texts(ch))
  }
  out
}

.grob_has_text <- function(g, needle) {
  needle %in% .grob_texts(g)
}

# The titles of a packed legend column, in the order they are drawn. Legends()
# carries no title slot of its own (@name is an internal, auto-generated grob
# id, not what `title =` was set to), so this reads them back off the rendered
# text grobs, keeping only the ones that name a legend.
.legend_titles <- function(ht, expected) {
  legends <- attr(ht, "extra_legends")
  texts <- unlist(lapply(legends, function(l) .grob_texts(l@grob)))
  texts[texts %in% expected]
}

test_that("the whole legend column is assembled here, fill legend first", {
  # None of it is left to ComplexHeatmap to collect: its own collection walks
  # the panels and slots each one's fill legend in after that panel's other
  # legends, which buried "Gene call" behind every class key on the plot.
  # The strip's own key only draws where the strip does — see .gene_panel's
  # top_annotation — so this needs the columns clustered rather than split by
  # class to exercise it at all.
  mat <- amr_plot$amr_presence_matrix(hits_fixture(), ISOLATES)
  ht <- amr_plot$build_amr_heatmap(
    mat,
    list(column_grouping = "cluster", anno_layers = list(strip_fixture()))
  )
  expect_identical(
    .legend_titles(ht, c("Gene call", "Resistance", "Virulence", "Country")),
    c("Gene call", "Resistance", "Country")
  )
  # The virulence panel's only class is the panel's own name, so it keys
  # nothing - and the mapped strip, turned off, leaves nothing behind either.
  bare <- amr_plot$build_amr_heatmap(mat, list(column_grouping = "cluster"))
  expect_identical(
    .legend_titles(bare, c("Gene call", "Resistance", "Country")),
    c("Gene call", "Resistance")
  )
})

test_that("only the call states a screen reached are keyed", {
  # Every fixture hit is an EXACTX (rank 4, "Perfect"), so nothing on this
  # screen is Putative, Partial or Strong and the key does not offer them.
  mat <- amr_plot$amr_presence_matrix(hits_fixture(), ISOLATES)
  ht <- amr_plot$build_amr_heatmap(mat, list())
  states <- .legend_titles(ht, amr_plot$AMR_CONFIDENCE_STATES)
  expect_identical(states, c("Absent", "Perfect"))
})

test_that("the legend column is packed against the height it is drawn into", {
  # The regression this guards: ComplexHeatmap packs legends against the whole
  # device while drawing the column from the top of the matrix body downwards,
  # so it believes it has more page than it does and runs the tail of a long
  # key list off the bottom edge. Packed here against the fit's own answer, a
  # column too tall for its room wraps sideways instead.
  mat <- amr_plot$amr_presence_matrix(hits_fixture(), ISOLATES)
  strips <- lapply(1:8, function(i) {
    layer <- strip_fixture(paste("Variable", i))
    layer$field <- paste0("v", i)
    layer
  })
  packed_in <- function(room) {
    ht <- amr_plot$build_amr_heatmap(
      mat,
      list(
        anno_layers = strips,
        legend_height_in = room
      )
    )
    as.numeric(convertHeight(
      grobHeight(attr(ht, "extra_legends")[[1]]@grob),
      "in"
    ))
  }
  expect_lt(packed_in(2), 2)
  expect_gt(packed_in(30), 2)
})

# --- column arrangement -------------------------------------------------------

test_that("clustering by drug class runs within each block, not across them", {
  mat <- amr_plot$amr_presence_matrix(hits_fixture(), ISOLATES)
  meta <- attr(mat, "genes")
  amr <- meta[meta$element_type == "AMR", , drop = FALSE]
  amr_mat <- mat[, amr$gene, drop = FALSE]

  layout <- impl$.column_layout(amr_mat, "class", amr, "binary", "ward.D2")
  expect_false(is.null(layout$split))
  # A logical TRUE, not a precomputed dendrogram: ComplexHeatmap has to derive
  # one tree per split slice itself, which it cannot do from a single tree
  # handed in for the whole panel (see the comment on .column_layout).
  expect_true(isTRUE(layout$cluster))

  # The block order stays the curated one - nothing here should let a
  # within-block similarity score reshuffle which class comes first. A single
  # element type keeps this a plain Heatmap rather than a HeatmapList, which
  # is what carries the column_dend_param slot this checks.
  amr_only <- amr_plot$amr_presence_matrix(
    hits_fixture()[hits_fixture()$element_type == "AMR", , drop = FALSE],
    ISOLATES
  )
  ht <- amr_plot$build_amr_heatmap(amr_only, list(column_grouping = "class"))
  expect_false(ht@column_dend_param$cluster_slices)
})

test_that(".column_dist_fn clusters on presence, not on the drawn labels", {
  mat <- amr_plot$amr_presence_matrix(hits_fixture(), ISOLATES)
  dist_fn <- impl$.column_dist_fn(mat, "binary")
  # ComplexHeatmap hands this a genes-by-isolates matrix (columns of the
  # heatmap, one row per gene) - here stood in for by hand, keyed by the same
  # gene names amr_presence_matrix gave the real matrix.
  fake <- matrix(
    "Perfect",
    nrow = 2,
    ncol = nrow(mat),
    dimnames = list(c("blaTEM", "gyrA"), rownames(mat))
  )
  d <- dist_fn(fake)
  expect_s3_class(d, "dist")
  expect_true(all(is.finite(d)))
})

test_that("the class heading draws as text only where columns are split by class", {
  # Grouped by class, each block is titled with its own drug class as text,
  # drawn by hand into a reserved strip above its own dendrogram rather than
  # through ComplexHeatmap's own column_title (see .class_dend_reserve() and
  # .decorate_class_dend()) - which is why this has to replicate
  # amr_as_ggplot()'s draw()-then-decorate sequence rather than only
  # draw()ing, the way a title living in column_title could be found with.
  # Clustered, there is no split left to title and the block carries a
  # colour strip instead - no text of the class name at all, drawn or
  # otherwise (see .gene_panel's top_annotation). Which of the two happens is
  # column_grouping's call alone, with no switch of its own.
  amr_only <- amr_plot$amr_presence_matrix(
    hits_fixture()[hits_fixture()$element_type == "AMR", , drop = FALSE],
    ISOLATES
  )
  draw_grob <- function(grouping) {
    ht <- amr_plot$build_amr_heatmap(amr_only, list(column_grouping = grouping))
    grid.grabExpr({
      ht_drawn <- ComplexHeatmap$draw(ht, merge_legends = TRUE)
      decorations <- attr(ht, "class_decorations")
      if (is.null(decorations)) decorations <- list()
      for (d in decorations) {
        cd <- ComplexHeatmap$column_dend(ht_drawn, name = d$panel_name)
        if (inherits(cd, "dendrogram")) cd <- list(cd)
        for (k in seq_along(d$cats)) {
          impl$.decorate_class_dend(
            d$annotation, k, d$cats[[k]], cd[[k]],
            d$title_in, d$dend_cm, d$text_color, d$size, d$rot
          )
        }
      }
    })
  }
  expect_true(.grob_has_text(draw_grob("class"), "Beta-lactam"))
  expect_false(.grob_has_text(draw_grob("cluster"), "Beta-lactam"))
})

test_that("classes too narrow to name are coloured into a strip instead", {
  # The other half of the same choice: split by class, but so finely that no
  # legible title fits (titles_legible, from the fit). Drawing them anyway put
  # a row of four-point smudges above the matrix and charged the page a full
  # title row for them; the strip says the same thing at a size that reads.
  amr_only <- amr_plot$amr_presence_matrix(
    hits_fixture()[hits_fixture()$element_type == "AMR", , drop = FALSE],
    ISOLATES
  )
  named <- amr_plot$build_amr_heatmap(
    amr_only,
    list(column_grouping = "class", titles_legible = TRUE)
  )
  strip <- amr_plot$build_amr_heatmap(
    amr_only,
    list(column_grouping = "class", titles_legible = FALSE)
  )
  expect_length(attr(named, "class_decorations"), 1L)
  expect_null(attr(strip, "class_decorations"))
  expect_s4_class(strip@top_annotation, "HeatmapAnnotation")

  # And the key that names the colours comes with it - the classes are only
  # unnamed on the plot, never unnamed altogether.
  expect_length(.legend_titles(named, "Resistance"), 0L)
  expect_identical(.legend_titles(strip, "Resistance"), "Resistance")
})

test_that("the class strip draws at the exact height the fit budgets for it", {
  # Both read AMR_CLASS_STRIP_IN, but from two different places (.class_
  # annotation's simple_anno_size and amr_auto_layout's overhead) - a bottom
  # of the page cut off is what it looks like when those two silently drift
  # apart, so this pins them to the same number rather than trusting they
  # agree by construction.
  amr_only <- amr_plot$amr_presence_matrix(
    hits_fixture()[hits_fixture()$element_type == "AMR", , drop = FALSE],
    ISOLATES
  )
  ht <- amr_plot$build_amr_heatmap(amr_only, list(column_grouping = "cluster"))
  expect_equal(
    as.numeric(convertHeight(ht@top_annotation@height, "in")),
    impl$AMR_CLASS_STRIP_IN
  )

  fit <- amr_plot$amr_auto_layout(
    100, 40,
    block_titles = c("Resistance"),
    block_cols = 40L,
    column_grouping = "cluster",
    aspect = 2
  )
  fit_no_strip <- amr_plot$amr_auto_layout(
    100, 40,
    block_titles = c("Resistance"),
    block_cols = 40L,
    column_grouping = "class",
    aspect = 2
  )
  # The two are alternatives, so what separates their budgets is exactly one
  # title row against one strip - the element-type row below the body is the
  # same in both and cancels. Getting this wrong is a band of white under one
  # of them and a title running off the page under the other.
  expect_equal(fit_no_strip$fontsize_title, fit$fontsize_title)
  expect_equal(
    fit$row_pitch_in - fit_no_strip$row_pitch_in,
    (fit_no_strip$title_in - impl$AMR_CLASS_STRIP_IN) / 100,
    tolerance = 0.001
  )
})

test_that("hiding gene names is read straight through to the panel", {
  amr_only <- amr_plot$amr_presence_matrix(
    hits_fixture()[hits_fixture()$element_type == "AMR", , drop = FALSE],
    ISOLATES
  )
  shown <- amr_plot$build_amr_heatmap(amr_only, list())
  hidden <- amr_plot$build_amr_heatmap(amr_only, list(show_col_names = FALSE))
  expect_true(shown@column_names_param$show)
  expect_false(hidden@column_names_param$show)
})

test_that("gene names kept on over a crowded shape still get room to sit in", {
  # The fit only seeds the switch off (refit_labels() in visualization_amr.R);
  # a reader is free to turn the names back on. Budgeting against `cols_
  # legible` rather than against the switch left that reader a zero-height
  # band: the names ran off the foot of the page and through the element-type
  # row, which is laid out under the room they were given rather than under
  # the room they take.
  crowded <- function(...) {
    amr_plot$amr_auto_layout(
      100, 244,
      width_in = 9,
      col_label_chars = 14,
      block_titles = paste0("Class-", 1:20),
      block_cols = rep(12L, 20),
      column_grouping = "class",
      aspect = 0.65,
      ...
    )
  }
  kept <- crowded(show_col_names = TRUE)
  expect_false(kept$cols_legible)
  expect_gt(kept$col_label_in, 0)

  # And the room comes back to the rows where the names are off, rather than
  # leaving a band of white under the matrix.
  dropped <- crowded(show_col_names = FALSE)
  expect_equal(dropped$col_label_in, 0)
  expect_gt(dropped$row_pitch_in, kept$row_pitch_in)
})

test_that("the panel draws its gene names into exactly the room fitted", {
  # The same pinning as the class strip above, on the other side of the
  # matrix: the fit's col_label_in and the panel's column_names_max_height
  # have to be one number, since the element-type title sits directly below
  # whatever height that reserves.
  amr_only <- amr_plot$amr_presence_matrix(
    hits_fixture()[hits_fixture()$element_type == "AMR", , drop = FALSE],
    ISOLATES
  )
  fit <- amr_plot$amr_auto_layout(
    6, ncol(amr_only),
    col_label_chars = max(nchar(colnames(amr_only))),
    show_col_names = TRUE
  )
  ht <- amr_plot$build_amr_heatmap(amr_only, c(list(show_col_names = TRUE), fit))
  cap <- as.numeric(convertHeight(ht@column_names_param$max_height, "in"))
  # Not equality: ComplexHeatmap adds a small gap of its own on top of the
  # height it is handed. The bounds are what matter - never less than the fit
  # set aside, and never the 2.4in fallback that would mean the two had come
  # apart.
  expect_gte(cap, fit$col_label_in)
  expect_lt(cap, fit$col_label_in + 0.2)
})

test_that("the element-type row is only budgeted where it is drawn", {
  # Grouped by class it is a row of its own below the block titles; clustered
  # it *is* the block-title row, moved to the foot of the matrix - so the room
  # switching it off gives back comes out of a different budget in each case.
  fitted <- function(...) {
    amr_plot$amr_auto_layout(
      100, 40,
      block_titles = c("Resistance", "Virulence"),
      block_cols = c(20L, 20L),
      aspect = 2,
      ...
    )
  }
  for (grouping in c("class", "cluster")) {
    shown <- fitted(column_grouping = grouping, show_element_names = TRUE)
    hidden <- fitted(column_grouping = grouping, show_element_names = FALSE)
    expect_gt(hidden$row_pitch_in, shown$row_pitch_in)
  }
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

test_that("rotated titles shrink to the tightest pair of neighbours", {
  # Every block here is a single column: a rotated title's footprint across the
  # page is its line height, so at any ordinary size each one would run into
  # the next. This is the collision the cap exists for.
  many <- amr_plot$amr_auto_layout(
    100, 60,
    width_in = 9,
    block_titles = paste0("Class-", 1:20),
    block_cols = rep(1L, 20)
  )
  expect_identical(many$title_rot, 90)
  expect_lt(many$fontsize_title, 8)

  # Also rotated (one title is too long to fit horizontally either way), but
  # three columns per block is room enough that the cap never engages - the
  # size stays at what fontsize_col + 3 alone would give it.
  few <- amr_plot$amr_auto_layout(
    100, 60,
    width_in = 9,
    block_titles = c("Aminoglycoside/Quinolone/Tobramycin", "Beta-lactam"),
    block_cols = c(3L, 3L)
  )
  expect_identical(few$title_rot, 90)
  expect_gt(few$fontsize_title, many$fontsize_title)
})

test_that("one narrow class does not shrink every title on the plot", {
  # The measure is the distance between two titles' centres, which is half of
  # each of their blocks - so a single-column class sitting beside a wide one
  # has that neighbour's half to lean into and collides with nothing. Measured
  # against the narrowest *block* instead, this screen came out at a size
  # nobody could read because one class happened to carry a single gene.
  lone <- amr_plot$amr_auto_layout(
    100, 60,
    width_in = 9,
    block_titles = c("Aminoglycoside", "Colistin", "Beta-lactam"),
    block_cols = c(20L, 1L, 39L)
  )
  crowd <- amr_plot$amr_auto_layout(
    100, 60,
    width_in = 9,
    block_titles = c("Aminoglycoside", "Colistin", "Beta-lactam"),
    block_cols = c(1L, 1L, 58L)
  )
  expect_gt(lone$fontsize_title, crowd$fontsize_title)
  expect_true(lone$titles_legible)
})

test_that("the fit flags a shape that leaves no room for its labels", {
  # Two hundred and forty genes over forty drug classes: each title has a
  # third of a millimetre of page and each gene name barely more. The gene
  # names go off outright when this fires (see refit_labels() in
  # visualization_amr.R); a drug-class title stays on screen at its floor
  # size instead - see the row-pitch test just below.
  # The shape a real screen takes: most drug classes carry one or two genes,
  # a handful carry dozens.
  crowded_cols <- c(rep(1L, 35L), 30L, 40L, 50L, 60L, 27L)
  crowded <- amr_plot$amr_auto_layout(
    100, 244,
    width_in = 9,
    block_titles = paste0("Class-", seq_along(crowded_cols)),
    block_cols = crowded_cols
  )
  expect_false(crowded$titles_legible)
  expect_false(crowded$cols_legible)

  roomy <- amr_plot$amr_auto_layout(
    60, 24,
    width_in = 9,
    block_titles = c("Beta-lactam", "Quinolone"),
    block_cols = c(12L, 12L)
  )
  expect_true(roomy$titles_legible)
  expect_true(roomy$cols_legible)
})

test_that("a class title stays on screen at its floor size, illegible or not", {
  # Unlike the gene names under the matrix, a drug-class title is never turned
  # off just because it hit its floor - the reader's own switch is the only
  # thing that does that (see refit_labels() in visualization_amr.R). The fit
  # still budgets room for it either way, so the row pitch does not grow just
  # because the titles came out tiny.
  crowded_cols <- c(rep(1L, 35L), 30L, 40L, 50L, 60L, 27L)
  pitch_at <- function(cols, titles) {
    amr_plot$amr_auto_layout(
      100, 244,
      width_in = 9,
      block_titles = titles,
      block_cols = cols,
      aspect = 2
    )$row_pitch_in
  }
  expect_equal(
    pitch_at(crowded_cols, paste0("Class-", seq_along(crowded_cols))),
    pitch_at(c(122L, 122L), c("Beta-lactam", "Quinolone")),
    tolerance = 0.01
  )
})

test_that("the legend is sized against the height it is drawn into", {
  few <- amr_plot$amr_auto_layout(
    100, 40,
    block_titles = c("Beta-lactam", "Quinolone")
  )
  many <- amr_plot$amr_auto_layout(
    100, 40,
    block_titles = paste0("Class-", 1:80)
  )
  expect_gt(few$fontsize_legend, many$fontsize_legend)
  # A mapped strip's own categories count too, even without a block title for
  # each of them - amr_auto_layout has no tabulated values yet to count.
  stripped <- amr_plot$amr_auto_layout(
    100, 40,
    block_titles = c("Beta-lactam", "Quinolone"),
    n_strips = 20L
  )
  expect_lt(stripped$fontsize_legend, few$fontsize_legend)

  # The same key list on a taller page is drawn larger, because the constraint
  # is the room rather than the count: a table of key counts alone left a
  # forty-class screen listing at a size that still ran off the bottom.
  tall <- amr_plot$amr_auto_layout(
    100, 40,
    block_titles = paste0("Class-", 1:80),
    aspect = 6
  )
  expect_gt(tall$fontsize_legend, many$fontsize_legend)
  expect_gt(tall$legend_rows, many$legend_rows)
})

test_that("a legend longer than its column wraps its keys instead", {
  # The last resort, once the type is already at the floor: the type shrinks
  # first because a second column of drug-class names is width the matrix
  # would otherwise have had.
  expect_identical(amr_plot$amr_legend_ncol(8L, 20L), 1L)
  expect_identical(amr_plot$amr_legend_ncol(40L, 20L), 2L)
  # Never past the cap, however long the list.
  expect_identical(amr_plot$amr_legend_ncol(400L, 20L), 3L)
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

test_that("the element-type row is fitted to its panel, not to a class block", {
  # Forty drug classes over two hundred genes leaves each class title a
  # fraction of an inch to sit in - and the element-type label under a
  # seven-inch panel used to be set at that same size, because the two shared
  # one number. They are separate rooms and now separate fits.
  fit <- amr_plot$amr_auto_layout(
    100, 244,
    width_in = 12.5,
    block_titles = paste0("Class-", 1:40),
    block_cols = rep(6L, 40),
    element_titles = c("Resistance", "Virulence", "Stress"),
    element_cols = c(219L, 8L, 17L),
    aspect = 0.65
  )
  expect_gt(fit$fontsize_element, fit$fontsize_title)
  expect_gte(fit$fontsize_element, impl$AMR_ELEMENT_MIN_PT)
  expect_true(fit$elements_legible)
})

test_that("a panel too narrow to carry its name flat turns it on its side", {
  wide <- amr_plot$amr_auto_layout(
    100, 200,
    width_in = 10,
    element_titles = "Resistance",
    element_cols = 200L,
    aspect = 1
  )
  expect_identical(wide$element_rot, 0)

  # Five virulence genes beside a hundred and ninety-five resistance ones:
  # "Virulence" is many times wider than the panel it belongs to, so the whole
  # row turns rather than any one label shrinking past reading.
  narrow <- amr_plot$amr_auto_layout(
    100, 200,
    width_in = 10,
    element_titles = c("Resistance", "Virulence"),
    element_cols = c(195L, 5L),
    aspect = 1
  )
  expect_identical(narrow$element_rot, 90)
  expect_gte(narrow$fontsize_element, impl$AMR_ELEMENT_MIN_PT)
  # Turned, what it costs the page is its length rather than two lines of
  # type, and the rows are what pay for it.
  expect_lt(narrow$row_pitch_in, wide$row_pitch_in)
})

test_that("the page, not the panel, is what caps the element-type row", {
  # One label under one whole panel has room to grow to a size nobody wants to
  # read a heading at, so the ceiling comes off the canvas width instead.
  args <- list(
    100, 40,
    element_titles = "Resistance",
    element_cols = 40L,
    aspect = 1
  )
  small <- do.call(amr_plot$amr_auto_layout, c(args, list(width_in = 5)))
  large <- do.call(amr_plot$amr_auto_layout, c(args, list(width_in = 20)))
  expect_lt(small$fontsize_element, large$fontsize_element)
  expect_lte(large$fontsize_element, impl$AMR_ELEMENT_MAX_PT)
  expect_gte(small$fontsize_element, impl$AMR_ELEMENT_MIN_PT)
})

test_that("no label is ever set below the floor for its own role", {
  # The rule the whole fit runs on: fit the type to the room, and where even
  # the floor will not fit, say so (so the builders can drop the label) rather
  # than set it smaller. A four-point label is a smudge that still costs the
  # page a full row.
  crowded <- amr_plot$amr_auto_layout(
    400, 400,
    width_in = 6,
    col_label_chars = 20,
    block_titles = paste0("Class-", 1:40),
    block_cols = rep(10L, 40),
    element_titles = c("Resistance", "Virulence"),
    element_cols = c(395L, 5L),
    n_strips = 3L,
    aspect = 1
  )
  expect_gte(crowded$fontsize_title, impl$AMR_TITLE_MIN_PT)
  expect_gte(crowded$fontsize_element, impl$AMR_ELEMENT_MIN_PT)
  expect_gte(crowded$fontsize_anno, impl$AMR_ANNO_NAME_MIN_PT)
  expect_gte(crowded$fontsize_legend, impl$AMR_LEGEND_MIN_PT)
  expect_false(crowded$titles_legible)
  expect_false(crowded$cols_legible)
})

test_that("a mapped strip is drawn as wide as the body the fit took it from", {
  # The same pinning as the class strip's height: amr_auto_layout() takes
  # AMR_STRIP_IN out of the body for each mapped variable, so that is what the
  # annotation has to draw at - ComplexHeatmap's own 5mm default left the body
  # narrower than every column width had been solved against.
  mat <- amr_plot$amr_presence_matrix(hits_fixture(), ISOLATES)
  layers <- list(list(
    label = "Country",
    values = setNames(rep(c("DE", "FR"), length.out = nrow(mat)), rownames(mat)),
    palette = "Set2"
  ))
  legend_gp <- impl$.legend_gp("#000000", 8)
  anno <- impl$.row_annotation(mat, layers, "#000000", legend_gp, list())
  expect_equal(
    as.numeric(convertWidth(anno$anno@width, "in")),
    impl$AMR_STRIP_IN
  )

  # And its name is set to that width rather than to the legend's size, and
  # goes off entirely where the width will not carry the floor - the legend it
  # keys is titled with the same variable name either way.
  named <- impl$.row_annotation(
    mat, layers, "#000000", legend_gp,
    list(anno_names_legible = TRUE, fontsize_anno = 6.2)
  )
  unnamed <- impl$.row_annotation(
    mat, layers, "#000000", legend_gp,
    list(anno_names_legible = FALSE, fontsize_anno = 6.2)
  )
  expect_true(named$anno@anno_list[[1]]@name_param$show)
  expect_false(unnamed$anno@anno_list[[1]]@name_param$show)
  expect_equal(named$anno@anno_list[[1]]@name_param$gp$fontsize, 6.2)
})

test_that("the element panels are measured before the heatmap exists", {
  mat <- amr_plot$amr_presence_matrix(hits_fixture(), ISOLATES)
  blocks <- amr_plot$amr_element_blocks(mat)
  expect_identical(blocks$titles, c("Resistance", "Virulence"))
  expect_identical(sum(blocks$cols), ncol(mat))
})

test_that("the prevalence chart grows with its bars and sets type to the pitch", {
  few <- amr_plot$amr_prevalence_layout(8, 10)
  many <- amr_plot$amr_prevalence_layout(80, 10)
  expect_gt(many$aspect, few$aspect)
  # ... and stops, rather than a chart nobody can scroll.
  expect_lte(many$aspect, impl$AMR_PREVALENCE_MAX)
  expect_lt(many$fontsize_row, few$fontsize_row)

  # The same bar count on a narrower canvas is less room per bar, which the
  # step table of counts this replaced could not see at all.
  expect_lt(amr_plot$amr_prevalence_layout(80, 5)$fontsize_row, many$fontsize_row)
  expect_true(few$legible)

  # The legend and axis title scale off the same bar pitch too, so a crowded
  # chart never leaves them looking oversized beside tiny bar labels - though,
  # unlike the row labels, they only start shrinking once the page is
  # genuinely packed.
  crowded <- amr_plot$amr_prevalence_layout(500, 10)
  expect_lt(crowded$fontsize_legend, few$fontsize_legend)
  expect_lte(few$fontsize_legend, 11)
})

test_that("the Prevalence bar scale only ever lands on Dark2 or viridis", {
  # The reader's own pick stands while it still names every group.
  expect_identical(amr_plot$amr_bar_scale_fit("Set3", 4), "Set3")
  # Dark2 is the default, and covers up to its own eight colors.
  expect_identical(amr_plot$amr_bar_scale_fit(NULL, 3), "Dark2")
  expect_identical(amr_plot$amr_bar_scale_fit("Dark2", 8), "Dark2")
  # Past that, it is viridis - never some other qualitative palette in
  # between, unlike amr_fit_scale()'s general ladder.
  expect_identical(amr_plot$amr_bar_scale_fit("Dark2", 9), "viridis")
  expect_identical(amr_plot$amr_bar_scale_fit("Set3", 20), "viridis")
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
  # The strip never carries its own legend - .class_legend() builds that one
  # separately, so build_amr_heatmap can place it ahead of "Gene call".
  expect_false(anno@anno_list$Resistance@show_legend)
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
  vir <- impl$.class_legend(
    virulence$group,
    cols,
    gp,
    impl$.panel_label(virulence)
  )
  resistance <- meta[meta$element_type == "AMR", , drop = FALSE]
  res <- impl$.class_legend(
    resistance$group,
    cols,
    gp,
    impl$.panel_label(resistance)
  )

  expect_null(vir)
  expect_false(is.null(res))
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

test_that("a screen with several strips still renders at its true canvas", {
  # grid.grabExpr() opens a 7x7 inch device unless told otherwise, and
  # ComplexHeatmap measures against the device it is handed; the legend column
  # no longer depends on that (amr_auto_layout solves its height and
  # .pack_legends holds it to it), but the grab is still given the real canvas
  # so nothing else it measures is answering to a size the figure never had.
  mat <- amr_plot$amr_presence_matrix(hits_fixture(), ISOLATES)
  strips <- lapply(1:8, function(i) {
    layer <- strip_fixture(paste("Variable", i))
    layer$field <- paste0("v", i)
    layer
  })
  ht <- amr_plot$build_amr_heatmap(
    mat,
    list(anno_layers = strips, legend_height_in = 20)
  )

  for (height_in in c(7, 40)) {
    file <- local_tempfile(fileext = ".png")
    amr_plot$render_amr_png(
      amr_plot$amr_as_ggplot(ht, width_in = 9, height_in = height_in),
      file,
      900,
      2400
    )
    expect_true(file.exists(file))
  }
})

test_that("every text element on the prevalence chart takes the fitted sizes", {
  df <- amr_plot$amr_prevalence(
    hits_fixture(),
    sections_fixture(),
    ISOLATES,
    level = "gene"
  )
  plot <- amr_plot$build_amr_prevalence(
    df,
    list(fontsize_row = 6, fontsize_legend = 8)
  )
  expect_equal(plot$theme$axis.text$size, 6)
  expect_equal(plot$theme$axis.title$size, 8)
  expect_equal(plot$theme$legend.text$size, 8)
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
