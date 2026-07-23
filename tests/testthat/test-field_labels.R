box::use(
  testthat[test_that, expect_identical, expect_named, expect_type],
  app /
    logic /
    field_labels[
      CUSTOM_COL_PREFIX,
      field_label,
      grouped_field_choices,
      MLST_COL_PREFIX
    ],
)

test_that("grouped_field_choices stays flat without MLST columns", {
  fields <- c("isolate", "geo_loc_name_country")
  ch <- grouped_field_choices(fields)

  # A plain labelled named vector, no optgroups.
  expect_type(ch, "character")
  expect_identical(unname(ch), fields)
  expect_identical(names(ch), c("Isolate", "Country"))
})

test_that("grouped_field_choices splits MLST columns into their own group", {
  fields <- c(
    "isolate",
    "geo_loc_name_country",
    paste0(MLST_COL_PREFIX, "st"),
    paste0(MLST_COL_PREFIX, "adk")
  )
  ch <- grouped_field_choices(fields)

  expect_type(ch, "list")
  expect_named(ch, c("Sample metadata", "Classical MLST"))
  # Values are raw column names; labels are the friendly ones.
  expect_identical(unname(ch[["Sample metadata"]]), fields[1:2])
  expect_identical(
    unname(ch[["Classical MLST"]]),
    fields[3:4]
  )
  expect_identical(
    names(ch[["Classical MLST"]]),
    c("Sequence Type (ST)", "adk")
  )
})

test_that("grouped_field_choices honours an explicit mlst_cols set", {
  fields <- c("isolate", "custom_col")
  # An explicitly declared set wins over prefix detection: "custom_col" carries
  # the custom-variable prefix but the caller declares it MLST.
  ch <- grouped_field_choices(fields, mlst_cols = "custom_col")

  expect_named(ch, c("Sample metadata", "Classical MLST"))
  expect_identical(unname(ch[["Classical MLST"]]), "custom_col")
})

test_that("grouped_field_choices splits custom variables into their own group", {
  fields <- c(
    "isolate",
    paste0(MLST_COL_PREFIX, "st"),
    paste0(CUSTOM_COL_PREFIX, "ward"),
    paste0(CUSTOM_COL_PREFIX, "ct_value")
  )
  ch <- grouped_field_choices(fields)

  expect_named(
    ch,
    c("Sample metadata", "Classical MLST", "Custom variables")
  )
  expect_identical(unname(ch[["Custom variables"]]), fields[3:4])
  # A custom variable reads as its own name, prettified.
  expect_identical(names(ch[["Custom variables"]]), c("Ward", "Ct Value"))
})

test_that("field_label prettifies a custom variable's own name", {
  expect_identical(field_label("custom_sampling_site"), "Sampling Site")
})
