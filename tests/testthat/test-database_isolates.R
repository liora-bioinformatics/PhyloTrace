box::use(
  shiny[reactive, testServer],
  testthat[expect_false, expect_identical, expect_match, expect_no_error, expect_true, test_that],
  withr[local_tempdir],
)
box::use(
  app / logic / isolate_overview[build_isolate_overview, load_isolate_overview],
  app / view / database_isolates,
  dev / mock_database,
)

impl <- attr(database_isolates, "namespace")

quiet <- function(expr) suppressMessages(expr)

# Every output of the module, read once; the per-isolate ones only where there
# is an isolate to select.
OUTPUTS <- c(
  "collection_table", "collection_summary", "iso_table", "size_bars", "contig_bars",
  "timeline_bars", "iso_runs", "checks", "environments"
)
SELECTED_OUTPUTS <- c("isolate_checks", "isolate_detail")

render_all <- function(path, selected = TRUE) {
  testServer(database_isolates$server, args = list(db_path = reactive(path)), {
    session$flushReact()
    for (id in OUTPUTS) {
      expect_no_error(output[[id]])
    }
    if (selected) {
      session$setInputs(iso_table_rows_selected = 1L)
      for (id in SELECTED_OUTPUTS) {
        expect_no_error(output[[id]])
      }
      session$setInputs(copy_name = 1L, copy_digest = 1L, copy_sha = 1L)
    }
  })
}

mock <- function(scenarios = character(0), n = 40L) {
  path <- file.path(local_tempdir(.local_envir = parent.frame()), "mock.db")
  quiet(mock_database$mock_database(path, n_isolates = n, n_loci = 12, scenarios = scenarios))
  path
}

test_that("every output renders for a database with every scenario", {
  scenarios <- setdiff(
    names(mock_database$MOCK_SCENARIOS),
    c("skipped", "no_classical", "amr_skipped", "no_amr")
  )
  render_all(mock(scenarios, n = 120L))
})

test_that("every output renders for a clean database", {
  render_all(mock())
})

test_that("every output renders without classical or AMR tables", {
  render_all(mock(c("no_classical", "no_amr", "no_assembly_record")))
})

test_that("every output renders for a database without record tables", {
  path <- file.path(local_tempdir(), "bare.db")
  build_db(path, default_local(), metadata = meta_df(c("A", "B")))
  render_all(path)
})

test_that("every output renders for a single isolate", {
  path <- file.path(local_tempdir(), "one.db")
  alleles <- default_local()
  build_db(path, alleles[c("ref", setdiff(names(alleles), "ref")[1])])
  render_all(path)
})

test_that("every output renders for a database without isolates", {
  path <- file.path(local_tempdir(), "empty.db")
  build_db(path, default_local()["ref"])
  render_all(path, selected = FALSE)
})

test_that("the isolate table puts flagged rows first", {
  ov <- load_isolate_overview(mock(c("assembly_changed", "duplicate_assemblies")))
  rows <- impl$isolate_rows(ov$isolates)
  flagged <- nzchar(rows$Flags)

  expect_true(any(flagged))
  expect_false(is.unsorted(!flagged))
  expect_identical(nrow(rows), nrow(ov$isolates))
})

test_that("the collection metadata drops what is not recorded", {
  ov <- build_isolate_overview(isolates = c("A", "B"))
  rows <- impl$collection_rows(ov, NULL)
  shown <- names(rows)[!is.na(rows)]

  expect_identical(unname(rows["Assembly Records"]), "0 of 2 isolates")
  expect_false(any(c("Species", "Typing Runs", "Assembly Length") %in% shown))
  expect_identical(unname(rows["Origin"]), "All typed in this database")
})

test_that("mixed values point to Diagnostics instead of listing them", {
  expect_match(impl$one_or_mixed(c("1.5.0", "1.6.1", NA)), "Diagnostics")
  expect_identical(impl$one_or_mixed(c("1.6.1", NA, "1.6.1")), "1.6.1")
  expect_true(is.na(impl$one_or_mixed(NA)))
})

test_that("an isolate's details show what is recorded", {
  ov <- load_isolate_overview(mock("assembly_changed"))
  iso <- ov$isolates
  i <- which(iso$changed)[1]
  html <- as.character(impl$detail_rows(iso[i, , drop = FALSE], ov$median_length, 50))

  expect_match(html, "Typed From", fixed = TRUE)
  expect_match(html, "vs. median", fixed = TRUE)

  bare <- build_isolate_overview(isolates = "A")$isolates
  html <- as.character(impl$detail_rows(bare, NA, NA))
  expect_match(html, "Typed here", fixed = TRUE)
  expect_false(grepl("Digest", html, fixed = TRUE))
})
