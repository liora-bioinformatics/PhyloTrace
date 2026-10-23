box::use(
  DBI[dbConnect, dbDisconnect, dbExecute],
  openxlsx[write.xlsx],
  RSQLite[SQLite],
  testthat[
    expect_equal,
    expect_error,
    expect_false,
    expect_identical,
    expect_length,
    expect_match,
    expect_setequal,
    expect_named,
    expect_true,
    test_that
  ],
  app / logic / custom_fields,
  app / logic / custom_import,
)

# Isolates A and B, with a metadata table (country "Germany", dates 2024-01-0x).
fixture <- function() {
  path <- tempfile(fileext = ".db")
  build_db(path, default_local(), metadata = meta_df(c("A", "B")))
  path
}

# Writes `lines` to a temporary file with extension `ext`.
write_table <- function(lines, ext = "csv") {
  path <- tempfile(fileext = paste0(".", ext))
  writeLines(lines, path, useBytes = TRUE)
  path
}

test_that("read_import_table sniffs the delimiter and keeps text as text", {
  for (sep in c(",", ";", "\t")) {
    path <- write_table(c(
      paste("isolate", "zip", "ct", sep = sep),
      paste("A", "01067", "1.5", sep = sep)
    ))
    r <- custom_import$read_import_table(path)
    expect_named(r$data, c("isolate", "zip", "ct"))
    expect_identical(r$data$zip, "01067")
  }
})

test_that("read_import_table strips a BOM and renames blank or repeated headers", {
  path <- write_table(c("﻿isolate,x,,x", "A,1,2,3"))
  r <- custom_import$read_import_table(path)
  expect_identical(names(r$data), c("isolate", "x", "column_3", "x_1"))
  expect_identical(unname(r$renamed), c("", "x"))
})

test_that("read_import_table reads Excel dates and numbers as typed-in text", {
  path <- tempfile(fileext = ".xlsx")
  write.xlsx(
    data.frame(
      isolate = c("A", "B"),
      when = as.Date(c("2024-03-01", NA)),
      n = c(100000, 2.5)
    ),
    path
  )
  r <- custom_import$read_import_table(path)
  expect_identical(r$data$when, c("2024-03-01", NA))
  expect_identical(r$data$n, c("100000", "2.5"))
  expect_identical(r$sheet, "Sheet 1")
})

test_that("read_import_table refuses unsupported files", {
  expect_error(custom_import$read_import_table(write_table("x", "pdf")), "Unsupported")
})

test_that("missing tokens are recognised case-insensitively", {
  expect_identical(
    custom_import$is_missing_value(c("", " NA ", "n/a", "NULL", "-", "0", "none", NA)),
    c(TRUE, TRUE, TRUE, TRUE, TRUE, FALSE, FALSE, TRUE)
  )
})

test_that("key_column prefers an 'isolate' header, then the best-matching column", {
  key <- custom_import$key_column(c("id", "Isolate", "x"))
  expect_identical(as.character(key), "Isolate")
  expect_identical(attr(key, "source"), "header")

  key <- custom_import$key_column(c("id", "x"))
  expect_identical(as.character(key), "id")
  expect_identical(attr(key, "source"), "first")

  df <- data.frame(id = c("N1", "N2"), acc = c("A", " B "), stringsAsFactors = FALSE)
  key <- custom_import$key_column(names(df), df, c("A", "B"))
  expect_identical(as.character(key), "acc")
  expect_identical(attr(key, "source"), "matches")

  # Nothing matches: back to the left-most column.
  key <- custom_import$key_column(names(df), df, c("Z"))
  expect_identical(attr(key, "source"), "first")
})

test_that("suggest_name produces names validate_custom_name accepts", {
  expect_identical(
    custom_import$suggest_name(c("Zip Code", "2nd value", "custom_x", "ct (%)")),
    c("Zip_Code", "v_2nd_value", "imported_custom_x", "ct")
  )
})

test_that("prepare_rows collapses verbatim repeats and excludes contradicting ones", {
  df <- data.frame(
    isolate = c("A", "A", "B", "B", "", "C"),
    x = c("1", "1", "1", "2", "9", "NA"),
    stringsAsFactors = FALSE
  )
  p <- custom_import$prepare_rows(df, "isolate")
  expect_identical(p$rows$isolate, c("A", "C"))
  expect_identical(p$rows$x, c("1", NA))
  expect_equal(p$n_blank_keys, 1L)
  # What the key-less row holds, so the user can recognise e.g. a footnote.
  expect_identical(p$blank_preview, "9")
  expect_equal(p$n_repeated, 1L)
  expect_identical(p$conflicting, "B")
})

test_that("feasible_types never offers a type some value does not fit", {
  ft <- custom_import$feasible_types
  expect_identical(ft(c("a", "b")), c("category", "text"))
  expect_identical(ft(c("1", "0")), c("boolean", "integer", "numeric", "category", "text"))
  expect_identical(ft(c("1", "2")), c("integer", "numeric", "category", "text"))
  expect_identical(ft(c("1.5", "2")), c("numeric", "category", "text"))
  expect_identical(ft(c("2024-01-01", "2024-02-30")), c("category", "text"))
  expect_identical(ft(c("2024-01-01x")), c("category", "text"))
  # Leading zeros are identifiers a number type would strip.
  expect_false("integer" %in% ft(c("01067", "10115")))
  # as.numeric() takes these, a number column should not.
  expect_false("numeric" %in% ft(c("Inf", "1")))
  expect_false("numeric" %in% ft(c("0x1A")))
  # Beyond the 32-bit range: decimal only.
  expect_false("integer" %in% ft("99999999999"))
  expect_true("numeric" %in% ft("99999999999"))
  # Too many levels for a sensible category.
  expect_false("category" %in% ft(as.character(seq_len(custom_import$CATEGORY_MAX_LEVELS + 1))))
})

test_that("guess_type prefers yes/no words, numbers, dates, then categories", {
  g <- custom_import$guess_type
  expect_identical(g(c("yes", "no", "Y")), "boolean")
  expect_identical(g(c("0", "1", "1")), "integer")
  expect_identical(g(c("1.5", "2")), "numeric")
  expect_identical(g(c("2024-01-01")), "date")
  expect_identical(g(c("blood", "urine", "blood", "blood")), "category")
  expect_identical(g(c("x1", "x2")), "text")
  expect_identical(g(character(0)), "text")
})

test_that("assess_import reports matches, case-only misses and name clashes", {
  path <- fixture()
  custom_fields$create_custom_field(path, "ward", "category", levels = c("icu", "er"))
  df <- data.frame(
    isolate = c("A", "b", "Z"),
    ward = c("ICU", "er", "x"),
    geo_loc_name_country = c("France", "Spain", "x"),
    organism = c("x", "y", "z"),
    ct = c("1.5", "NA", "3"),
    stringsAsFactors = FALSE
  )
  a <- custom_import$assess_import(path, df)

  expect_identical(a$key_source, "header")
  expect_identical(a$matched, "A")
  expect_setequal(a$unmatched, c("Z", "b"))
  expect_identical(a$case_only, "b")
  expect_equal(a$n_db_untouched, 1L)

  cols <- a$columns
  expect_identical(cols$clash, c("custom", "metadata", "readonly", "none"))
  expect_identical(cols$clash_target[1:3], c("ward", "geo_loc_name_country", "organism"))
  expect_equal(cols$merge_failures[1:2], c(0L, 0L))
  expect_identical(cols$guess[[4]], "numeric")
  # Profiled over matched rows only.
  expect_equal(cols$n_values[[4]], 1L)
})

test_that("assess_import keeps suggested names unique ignoring case", {
  path <- fixture()
  df <- data.frame(isolate = "A", Ward = "1", ward = "2", check.names = FALSE)
  a <- custom_import$assess_import(path, df)
  expect_identical(a$columns$suggested, c("Ward", "ward_2"))
})

test_that("validate_plan catches clashes, unfit types and read-only targets", {
  path <- fixture()
  rows <- data.frame(isolate = "A", x = "abc", y = "2", z = "3", stringsAsFactors = FALSE)
  plan <- data.frame(
    header = c("x", "y", "z"),
    action = c("new", "new", "fill"),
    name = c("geo_loc_name_country", "y", "organism"),
    type = c("text", "boolean", NA),
    stringsAsFactors = FALSE
  )
  problems <- custom_import$validate_plan(path, rows, plan)
  expect_length(problems, 3L)
  expect_match(problems[[1]], "metadata field")
  expect_match(problems[[2]], "fits")
  expect_match(problems[[3]], "editable")

  ok <- data.frame(header = "x", action = "new", name = "x", type = "integer")
  expect_match(custom_import$validate_plan(path, rows, ok)[["x"]], "fits")
})

test_that("apply_import creates typed variables and never stores missing values", {
  path <- fixture()
  rows <- data.frame(
    isolate = c("A", "B"),
    ct = c("0.123456789", NA),
    site = c("Blood", "blood"),
    stringsAsFactors = FALSE
  )
  plan <- data.frame(
    header = c("ct", "site"),
    action = c("new", "new"),
    name = c("ct_value", "site"),
    type = c("numeric", "category"),
    stringsAsFactors = FALSE
  )
  counts <- custom_import$apply_import(path, rows, plan)
  expect_equal(counts$created, 2L)
  expect_equal(counts$values, 3L)
  expect_false(counts$metadata)

  defs <- custom_fields$list_custom_fields(path)
  expect_identical(defs$name, c("ct_value", "site"))
  expect_identical(custom_fields$field_levels(defs$levels[[2]]), "Blood")

  wide <- custom_fields$load_custom_values(path)
  expect_equal(wide$custom_ct_value[wide$isolate == "A"], 0.123456789)
  expect_identical(wide$custom_site[order(wide$isolate)], c("Blood", "Blood"))
  expect_equal(nrow(qdf(path, "SELECT * FROM phylotrace_custom_values WHERE field_id = 1")), 1L)
})

test_that("apply_import fills only empty cells unless asked to overwrite", {
  path <- fixture()
  con <- dbConnect(SQLite(), path)
  dbExecute(con, "UPDATE metadata SET geo_loc_name_country = NULL WHERE isolate = 'B'")
  dbDisconnect(con)

  rows <- data.frame(
    isolate = c("A", "B"),
    country = c("France", "Spain"),
    when = c("2025-05-05", NA),
    stringsAsFactors = FALSE
  )
  fill <- data.frame(
    header = c("country", "when"),
    action = c("fill", "fill"),
    name = c("geo_loc_name_country", "sample_collection_date"),
    type = NA_character_,
    stringsAsFactors = FALSE
  )
  preview <- custom_import$merge_preview(path, rows, "country", "metadata", "geo_loc_name_country")
  expect_identical(unlist(preview), c(fill = 1L, same = 0L, differ = 1L))

  counts <- custom_import$apply_import(path, rows, fill)
  expect_equal(counts$filled, 1L)
  expect_equal(counts$overwritten, 0L)
  meta <- qdf(path, "SELECT * FROM metadata ORDER BY isolate")
  expect_identical(meta$geo_loc_name_country, c("Germany", "Spain"))
  expect_identical(meta$sample_collection_date, c("2024-01-01", "2024-01-02"))

  fill$action <- "overwrite"
  counts <- custom_import$apply_import(path, rows, fill)
  expect_equal(counts$overwritten, 2L)
  meta <- qdf(path, "SELECT * FROM metadata ORDER BY isolate")
  expect_identical(meta$geo_loc_name_country, c("France", "Spain"))
  # A missing upload value never clears what is stored.
  expect_identical(meta$sample_collection_date, c("2025-05-05", "2024-01-02"))
})

test_that("apply_import merges into an existing custom variable", {
  path <- fixture()
  id <- custom_fields$create_custom_field(path, "ward", "category", levels = c("icu", "er"))
  custom_fields$save_custom_values(
    path,
    data.frame(field_id = id, isolate = "A", value = "icu")
  )
  rows <- data.frame(isolate = c("A", "B"), ward = c("ER", "ER"), stringsAsFactors = FALSE)
  plan <- data.frame(header = "ward", action = "fill", name = "ward", type = NA)

  counts <- custom_import$apply_import(path, rows, plan)
  expect_equal(counts$filled, 1L)
  wide <- custom_fields$load_custom_values(path)
  expect_identical(wide$custom_ward[order(wide$isolate)], c("icu", "er"))
})

test_that("apply_import writes nothing when any part of the plan is invalid", {
  path <- fixture()
  rows <- data.frame(isolate = "A", x = "1", y = "abc", stringsAsFactors = FALSE)
  plan <- data.frame(
    header = c("x", "y"),
    action = c("new", "new"),
    name = c("x", "y"),
    type = c("integer", "integer"),
    stringsAsFactors = FALSE
  )
  expect_error(custom_import$apply_import(path, rows, plan), "fits")
  expect_equal(nrow(custom_fields$list_custom_fields(path)), 0L)
})

test_that("apply_import rolls back every write when one fails mid-way", {
  path <- fixture()
  # Rows never repeat an isolate after prepare_rows(); here they do, so the
  # second column's insert breaks the primary key after the first column's
  # metadata overwrite already ran inside the transaction.
  rows <- data.frame(
    isolate = c("A", "A"),
    country = c("France", "France"),
    y = c("1", "1"),
    stringsAsFactors = FALSE
  )
  plan <- data.frame(
    header = c("country", "y"),
    action = c("overwrite", "new"),
    name = c("geo_loc_name_country", "y"),
    type = c(NA, "integer"),
    stringsAsFactors = FALSE
  )
  expect_error(custom_import$apply_import(path, rows, plan))
  expect_equal(nrow(custom_fields$list_custom_fields(path)), 0L)
  meta <- qdf(path, "SELECT geo_loc_name_country FROM metadata WHERE isolate = 'A'")
  expect_identical(meta$geo_loc_name_country, "Germany")
})

test_that("assess_import matches on the column holding the database's isolates", {
  path <- fixture()
  df <- data.frame(
    `Sample ID` = c("N1", "N2", NA),
    accession = c("A", "B", "n = 2"),
    x = c("1", "2", NA),
    check.names = FALSE,
    stringsAsFactors = FALSE
  )
  a <- custom_import$assess_import(path, df)
  expect_identical(a$key, "accession")
  expect_identical(a$key_source, "matches")
  expect_identical(a$matched, c("A", "B"))
  expect_identical(a$columns$header, c("Sample ID", "x"))

  # An explicit key is used as given, even when nothing matches.
  a <- custom_import$assess_import(path, df, key = "Sample ID")
  expect_identical(a$key_source, "user")
  expect_length(a$matched, 0L)
  expect_identical(a$prepared$blank_preview, "n = 2")
})

test_that("assess_import maps standard fields by exact column name only", {
  path <- fixture()
  df <- data.frame(
    isolate = "A",
    Country = "France",
    `Collection Date` = "2025-01-01",
    Organism = "x",
    check.names = FALSE,
    stringsAsFactors = FALSE
  )
  cols <- custom_import$assess_import(path, df)$columns
  # "Country" / "Collection Date" only read like the standard fields: flagged,
  # never merged into them. "Organism" is the exact (read-only) column name.
  expect_identical(cols$clash, c("label", "label", "readonly"))
  expect_identical(
    cols$clash_target,
    c("geo_loc_name_country", "sample_collection_date", "organism")
  )
  expect_true(all(is.na(cols$merge_failures[1:2])))

  # Imported as a new variable under that name, it is refused.
  plan <- data.frame(header = "Country", action = "new", name = "Country", type = "text")
  expect_match(custom_import$validate_plan(path, df, plan)[["Country"]], "already shown")
})

test_that("validate_plan refuses two columns writing into the same field", {
  path <- fixture()
  rows <- data.frame(isolate = "A", a = "France", b = "Spain", stringsAsFactors = FALSE)
  plan <- data.frame(
    header = c("a", "b"),
    action = c("fill", "overwrite"),
    name = "geo_loc_name_country",
    type = NA,
    stringsAsFactors = FALSE
  )
  problems <- custom_import$validate_plan(path, rows, plan)
  expect_length(problems, 2L)
  expect_match(problems[["b"]], "Another column")
})
