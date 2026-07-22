box::use(
  testthat[
    expect_equal,
    expect_false,
    expect_identical,
    expect_null,
    expect_true,
    expect_error,
    test_that
  ],
  app / logic / custom_fields,
)

impl <- attr(custom_fields, "namespace")

# A minimal database with three isolates and a metadata table, reused by every
# test below. Built with the shared helpers (see helper-phylotrace-db.R).
fixture <- function() {
  path <- tempfile(fileext = ".db")
  build_db(
    path,
    default_local(),
    metadata = meta_df(c("A", "B"))
  )
  path
}

test_that("ensure_custom_schema creates both tables and is idempotent", {
  path <- fixture()

  expect_true(custom_fields$ensure_custom_schema(path))
  expect_true(custom_fields$ensure_custom_schema(path))

  tables <- qdf(path, "SELECT name FROM sqlite_master WHERE type = 'table'")$name
  expect_true("phylotrace_custom_fields" %in% tables)
  expect_true("phylotrace_custom_values" %in% tables)

  # No database loaded: a no-op, never an error.
  expect_false(custom_fields$ensure_custom_schema(NULL))
})

test_that("list_custom_fields is empty and correctly shaped before any use", {
  path <- fixture()
  fields <- custom_fields$list_custom_fields(path)

  expect_equal(nrow(fields), 0L)
  expect_true(all(
    c("id", "name", "type", "description", "n_filled") %in% names(fields)
  ))
})

test_that("create_custom_field stores the definition and counts fills", {
  path <- fixture()

  id <- custom_fields$create_custom_field(
    path,
    "ward",
    "text",
    description = "Hospital ward the isolate came from"
  )
  expect_true(is.integer(id))

  fields <- custom_fields$list_custom_fields(path)
  expect_equal(nrow(fields), 1L)
  expect_identical(fields$name, "ward")
  expect_identical(fields$type, "text")
  expect_equal(fields$n_filled, 0L)

  custom_fields$save_custom_values(
    path,
    data.frame(field_id = id, souche = "A", value = "ICU")
  )
  expect_equal(custom_fields$list_custom_fields(path)$n_filled, 1L)
})

test_that("validate_custom_name rejects unusable names", {
  path <- fixture()
  custom_fields$create_custom_field(path, "ward", "text")

  expect_null(custom_fields$validate_custom_name(path, "sampling_site"))
  # Taken, case-insensitively.
  expect_true(is.character(custom_fields$validate_custom_name(path, "WARD")))
  # Collides with a metadata column.
  expect_true(is.character(custom_fields$validate_custom_name(path, "organism")))
  # Reserved prefixes of the derived column families.
  expect_true(is.character(custom_fields$validate_custom_name(path, "mlst_x")))
  expect_true(is.character(custom_fields$validate_custom_name(path, "amr_x")))
  expect_true(is.character(custom_fields$validate_custom_name(path, "custom_x")))
  # Not SQL-safe / not a usable column name.
  expect_true(is.character(custom_fields$validate_custom_name(path, "2fast")))
  expect_true(is.character(custom_fields$validate_custom_name(path, "a b")))
  expect_true(is.character(custom_fields$validate_custom_name(path, "")))

  expect_error(custom_fields$create_custom_field(path, "ward", "text"))
  expect_error(custom_fields$create_custom_field(path, "ok_name", "nonsense"))
  expect_error(custom_fields$create_custom_field(path, "ok_name", "category"))
})

test_that("coerce_custom_value canonicalises per type and rejects the rest", {
  coerce <- custom_fields$coerce_custom_value
  value <- function(...) coerce(...)$value
  ok <- function(...) coerce(...)$ok

  # An empty cell always clears, whatever the type.
  expect_true(ok("", "integer"))
  expect_identical(value("  ", "integer"), NA_character_)
  expect_identical(value(NA, "date"), NA_character_)

  expect_identical(value(" ICU ", "text"), "ICU")

  expect_identical(value("42", "integer"), "42")
  expect_identical(value("-7", "integer"), "-7")
  expect_false(ok("4.5", "integer"))
  expect_false(ok("abc", "integer"))

  expect_identical(value("12.5", "numeric"), "12.5")
  # Never stored in scientific notation - it must round-trip as written.
  expect_identical(value("1000000", "numeric"), "1000000")
  expect_false(ok("12,5", "numeric"))

  expect_identical(value("2026-07-21", "date"), "2026-07-21")
  expect_false(ok("21.07.2026", "date"))
  expect_false(ok("2026-13-01", "date"))

  expect_identical(value("Yes", "boolean"), "yes")
  expect_identical(value("0", "boolean"), "no")
  expect_false(ok("maybe", "boolean"))

  levels <- custom_fields$encode_levels(c("blood", "urine"))
  expect_identical(value("BLOOD", "category", levels), "blood")
  expect_false(ok("sputum", "category", levels))
  # The level vector may also be passed directly.
  expect_identical(value("urine", "category", c("blood", "urine")), "urine")
})

test_that("levels round-trip through encode_levels / field_levels", {
  json <- custom_fields$encode_levels(c(" blood ", "urine", "", "blood"))
  expect_identical(custom_fields$field_levels(json), c("blood", "urine"))

  expect_null(custom_fields$encode_levels(character(0)))
  expect_identical(custom_fields$field_levels(NA_character_), character(0))
  expect_identical(custom_fields$field_levels("not json"), character(0))
})

test_that("save_custom_values upserts and clears", {
  path <- fixture()
  id <- custom_fields$create_custom_field(path, "ward", "text")

  custom_fields$save_custom_values(
    path,
    data.frame(
      field_id = id,
      souche = c("A", "B"),
      value = c("ICU", "ER"),
      stringsAsFactors = FALSE
    )
  )
  expect_equal(q1(path, "SELECT COUNT(*) FROM phylotrace_custom_values"), 2L)

  # Same key again: replaced, not duplicated.
  custom_fields$save_custom_values(
    path,
    data.frame(field_id = id, souche = "A", value = "WARD3")
  )
  expect_equal(q1(path, "SELECT COUNT(*) FROM phylotrace_custom_values"), 2L)
  expect_identical(
    q1(
      path,
      "SELECT value FROM phylotrace_custom_values WHERE souche = 'A'"
    ),
    "WARD3"
  )

  # An emptied cell deletes its row rather than storing a blank.
  custom_fields$save_custom_values(
    path,
    data.frame(field_id = id, souche = "A", value = NA_character_)
  )
  expect_equal(q1(path, "SELECT COUNT(*) FROM phylotrace_custom_values"), 1L)
})

test_that("load_custom_values pivots wide and types numeric variables", {
  path <- fixture()
  ward <- custom_fields$create_custom_field(path, "ward", "text")
  ct <- custom_fields$create_custom_field(path, "ct_value", "numeric")
  custom_fields$create_custom_field(path, "empty_var", "text")

  custom_fields$save_custom_values(
    path,
    data.frame(
      field_id = c(ward, ward, ct),
      souche = c("A", "B", "B"),
      value = c("ICU", "ER", "12.5"),
      stringsAsFactors = FALSE
    )
  )

  wide <- custom_fields$load_custom_values(path)
  expect_identical(
    names(wide),
    c("isolate", "custom_ward", "custom_ct_value", "custom_empty_var")
  )
  expect_identical(wide$custom_ward[wide$isolate == "A"], "ICU")
  # Numeric so a DataTable sorts it as a number, not as a string.
  expect_true(is.numeric(wide$custom_ct_value))
  expect_equal(wide$custom_ct_value[wide$isolate == "B"], 12.5)
  expect_true(is.na(wide$custom_ct_value[wide$isolate == "A"]))

  # Restricted to a subset of variables.
  expect_identical(
    names(custom_fields$load_custom_values(path, fields = "ward")),
    c("isolate", "custom_ward")
  )

  # No custom variables at all.
  expect_null(custom_fields$load_custom_values(fixture()))
})

test_that("append_custom merges by isolate and records what it added", {
  path <- fixture()
  ward <- custom_fields$create_custom_field(path, "ward", "text")
  custom_fields$save_custom_values(
    path,
    data.frame(field_id = ward, souche = "A", value = "ICU")
  )

  meta <- data.frame(
    isolate = c("A", "B"),
    organism = "Testus organismus",
    stringsAsFactors = FALSE
  )
  merged <- custom_fields$append_custom(meta, path)

  expect_identical(attr(merged, "custom_cols"), "custom_ward")
  expect_identical(merged$custom_ward, c("ICU", NA_character_))

  # A database with no custom variables leaves the frame alone.
  plain <- custom_fields$append_custom(meta, fixture())
  expect_identical(attr(plain, "custom_cols"), character(0))
  expect_identical(names(plain), names(meta))
})

test_that("update_custom_field renames and delete_custom_field cascades", {
  path <- fixture()
  id <- custom_fields$create_custom_field(path, "ward", "text")
  custom_fields$save_custom_values(
    path,
    data.frame(field_id = id, souche = "A", value = "ICU")
  )

  custom_fields$update_custom_field(path, id, name = "unit", description = "x")
  fields <- custom_fields$list_custom_fields(path)
  expect_identical(fields$name, "unit")
  expect_identical(fields$description, "x")
  # Renaming never touches the values.
  expect_equal(fields$n_filled, 1L)

  # The type is not renameable through this path, so stored values stay valid.
  expect_identical(fields$type, "text")

  custom_fields$delete_custom_field(path, id)
  expect_equal(nrow(custom_fields$list_custom_fields(path)), 0L)
  expect_equal(q1(path, "SELECT COUNT(*) FROM phylotrace_custom_values"), 0L)
})

test_that("custom_col prefixes with the shared reserved prefix", {
  expect_identical(custom_fields$custom_col("ward"), "custom_ward")
})
