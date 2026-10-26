box::use(
  testthat[expect_identical, expect_true, test_that],
  withr[local_tempdir],
)
box::use(
  app / logic / amr_overview[load_amr_overview],
  app / logic / cgmlst_overview[load_cgmlst_overview],
  app / logic / db_compat[check_db_loadable],
  app / logic / isolate_overview[load_isolate_overview],
  app / logic / mlst_overview[load_mlst_overview],
  dev / mock_edge_cases,
)

dir <- local_tempdir()
written <- suppressMessages(mock_edge_cases$write_edge_cases(dir, scale = 0.05))
path_of <- function(name) file.path(dir, paste0("edge_", name, ".db"))
count_of <- function(name, sql) {
  con <- DBI::dbConnect(RSQLite::SQLite(), path_of(name))
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  DBI::dbGetQuery(con, sql)[[1]]
}

test_that("every edge case is written, listed in the README and loadable", {
  expect_identical(written$file, paste0("edge_", names(mock_edge_cases$EDGE_CASES), ".db"))
  readme <- readLines(file.path(dir, "README.md"))
  listed <- vapply(written$file, function(f) any(grepl(f, readme, fixed = TRUE)), logical(1))
  loadable <- vapply(file.path(dir, written$file), function(p) check_db_loadable(p)$ok, logical(1))
  expect_true(all(listed))
  expect_true(all(loadable))
})

test_that("all four overviews load every edge case", {
  loaders <- list(
    load_isolate_overview,
    load_cgmlst_overview,
    load_mlst_overview,
    load_amr_overview
  )
  for (file in written$file) {
    for (load in loaders) {
      expect_true(is.list(load(file.path(dir, file))), info = file)
    }
  }
})

test_that("the edge cases have the shapes they promise", {
  isolates <- "SELECT COUNT(DISTINCT souche) FROM mlst WHERE souche != 'ref'"
  expect_identical(count_of("single_isolate", isolates), 1L)
  expect_identical(count_of("empty", isolates), 0L)
  expect_true(any(grepl("<script>", count_of(
    "special_characters",
    "SELECT group_concat(isolate) FROM metadata"
  ), fixed = TRUE)))
  expect_true(count_of("many_releases", "SELECT COUNT(DISTINCT scheme) FROM classical_mlst") > 1)
  expect_true(
    count_of(
      "amr_heavy",
      "SELECT MAX(n) FROM (SELECT COUNT(*) n FROM amr_results GROUP BY isolate)"
    ) > 20
  )
  expect_identical(
    count_of("legacy", "SELECT COUNT(*) FROM sqlite_master WHERE name = 'typing_provenance'"),
    0L
  )
  expect_identical(count_of("all_failed", "SELECT COUNT(*) FROM classical_mlst"), 0L)
})
