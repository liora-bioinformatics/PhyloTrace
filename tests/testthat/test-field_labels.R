box::use(
  testthat[test_that, expect_identical, expect_named, expect_type],
  app / logic / field_labels[grouped_field_choices, MLST_COL_PREFIX],
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
  # custom_col has no MLST prefix but is declared MLST by the caller.
  ch <- grouped_field_choices(fields, mlst_cols = "custom_col")

  expect_named(ch, c("Sample metadata", "Classical MLST"))
  expect_identical(unname(ch[["Classical MLST"]]), "custom_col")
})
