box::use(
  testthat[
    expect_false,
    expect_identical,
    expect_named,
    expect_true,
    expect_type,
    test_that,
  ],
  app /
    logic /
    field_labels[
      amr_class_label,
      amr_field_map,
      CUSTOM_COL_PREFIX,
      field_label,
      grouped_field_choices,
      MLST_COL_PREFIX
    ],
)

test_that("amr_class_label cases AMRFinderPlus's vocabulary for reading", {
  expect_identical(
    amr_class_label(c("BETA-LACTAM", "QUATERNARY AMMONIUM", "AMINOGLYCOSIDE")),
    c("Beta-lactam", "Quaternary ammonium", "Aminoglycoside")
  )
  # A class naming several drugs capitalises each of them, the way abritamr
  # writes the same class in its own rollup.
  expect_identical(
    amr_class_label("AMIKACIN/KANAMYCIN/TOBRAMYCIN"),
    "Amikacin/Kanamycin/Tobramycin"
  )
})

test_that("amr_class_label leaves anything already cased alone", {
  # abritamr's rollup and gene symbols reach the same legends; lowering these
  # would be the regression the all-caps test guards against in reverse.
  expect_identical(
    amr_class_label(c("AmpC", "Beta-lactam", "blaOXA")),
    c("AmpC", "Beta-lactam", "blaOXA")
  )
})

test_that("amr_class_label preserves missing and empty values", {
  expect_identical(
    amr_class_label(c(NA, "", "  ", "MERCURY")),
    c(NA, "", "", "Mercury")
  )
  expect_identical(amr_class_label(character(0)), character(0))
})

# A metadata frame as the two AMR appenders leave it: two drug-class columns and
# two gene columns, with the attributes that say which gene and class each of
# the latter stands for.
amr_meta <- function() {
  meta <- data.frame(
    isolate = c("A", "B"),
    `amr_Beta-lactam` = c("blaOXA-2", ""),
    amr_Metal = c("merA", "merA, merD"),
    amr_g1 = c("Match", "Absent"),
    amr_g2 = c("Absent", "Match"),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
  attr(meta, "amr_cols") <- c("amr_Beta-lactam", "amr_Metal", "amr_g1", "amr_g2")
  attr(meta, "amr_class_sections") <- c(
    `amr_Beta-lactam` = "Resistance",
    amr_Metal = "Virulence / stress"
  )
  attr(meta, "amr_gene_labels") <- c(amr_g1 = "blaOXA-2", amr_g2 = "merD")
  attr(meta, "amr_gene_groups") <- c(amr_g1 = "Beta-lactam", amr_g2 = "Metal")
  attr(meta, "amr_gene_sections") <- c(
    amr_g1 = "Resistance",
    amr_g2 = "Virulence / stress"
  )
  meta
}

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

test_that("amr_field_map tells a gene column from a drug-class one", {
  map <- amr_field_map(amr_meta())
  expect_identical(map$field, c("amr_Beta-lactam", "amr_Metal", "amr_g1", "amr_g2"))
  expect_identical(map$role, c("class", "class", "gene", "gene"))
  # A class column carries its class in its own name; a gene column cannot, so
  # the gene and the class it belongs to come off the attributes.
  expect_identical(map$class, c("Beta-lactam", "Metal", "Beta-lactam", "Metal"))
  expect_identical(map$gene, c(NA, NA, "blaOXA-2", "merD"))
  # A gene is filed with its class, so the two arrive together in a picker.
  expect_identical(map$subgroup[[1]], map$subgroup[[3]])
  expect_identical(map$subgroup[[1]], "AMR \u00b7 Beta-lactam")
  # Virulence and stress do not read as resistance.
  expect_identical(map$subgroup[[2]], "Virulence / stress \u00b7 Metal")
})

test_that("amr_field_map survives what abritamr can actually report", {
  # No AMR at all.
  expect_identical(nrow(amr_field_map(data.frame(isolate = "A"))), 0L)

  # An empty drug class and an unnamed gene: virtual-select draws a row per
  # group whatever the label says, so a blank one is an unselectable line.
  meta <- data.frame(isolate = "A", amr_g1 = "Match", stringsAsFactors = FALSE)
  attr(meta, "amr_cols") <- "amr_g1"
  attr(meta, "amr_gene_labels") <- c(amr_g1 = "  ")
  attr(meta, "amr_gene_groups") <- c(amr_g1 = "")
  map <- amr_field_map(meta)
  expect_identical(map$class, "Other")
  expect_identical(map$gene, "amr_g1")
  # An unknown section is resistance, which is what abritamr reports by default.
  expect_identical(map$subgroup, "AMR \u00b7 Other")
})

test_that("field_label no longer claims an AMR profile column", {
  # The comma-joined summary of every other AMR column is gone: it could not be
  # mapped to anything a reader could act on.
  expect_identical(field_label("amr_Beta-lactam"), "Beta-lactam")
  expect_false(identical(field_label("amr_profile"), "AMR Profile"))
  expect_true(nzchar(field_label("amr_profile")))
})
