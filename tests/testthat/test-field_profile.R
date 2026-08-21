box::use(
  app / logic / field_labels[GROUP_ORDER],
  app / logic / field_profile,
)

# A metadata table shaped like a real one: a column that cannot group (one
# value), one that groups perfectly (a handful of well-filled groups), one with
# more groups than a ColorBrewer palette has colours, one that is nearly all
# empty, and one with a different value per isolate.
meta_fixture <- function(n = 40) {
  data.frame(
    isolate = sprintf("ISO-%03d", seq_len(n)),
    organism = rep("P. aeruginosa", n),
    purpose = rep(c("surveillance", "outbreak", "screening", "referral"),
      length.out = n),
    country = sprintf("C%02d", seq_len(n) %% 20),
    source = c(rep(NA_character_, n - 3), "blood", "urine", "wound"),
    collected_by = sprintf("lab-%03d", seq_len(n)),
    stringsAsFactors = FALSE
  )
}

# --- Level counting ----------------------------------------------------------

test_that("only a fully unique column is excluded from a mapping", {
  fields <- field_profile$mapping_fields(meta_fixture())

  # A per-isolate column groups nothing (every row is its own bucket), and
  # `isolate` is never a mapping. A constant column still groups, trivially,
  # into one bucket, so it stays offered rather than being hidden outright.
  expect_true("organism" %in% fields)
  expect_false("collected_by" %in% fields)
  expect_false("isolate" %in% fields)
  expect_true(all(c("purpose", "country", "source") %in% fields))
})

test_that("the best mapping column leads, so the default is the best one", {
  fields <- field_profile$mapping_fields(meta_fixture())

  # Four well-filled groups beats twenty, which beats a column filled for three
  # isolates out of forty.
  expect_identical(fields[1], "purpose")
  expect_lt(match("country", fields), match("source", fields))
})

test_that("a shape mapping is capped at what ggplot2 can draw", {
  fields <- field_profile$mapping_fields(
    meta_fixture(),
    max_levels = field_profile$MAX_SHAPE_LEVELS
  )
  # Twenty countries would silently drop fourteen levels' worth of tips.
  expect_false("country" %in% fields)
  expect_true("purpose" %in% fields)
})

test_that("an empty or unusable metadata table yields no mapping", {
  expect_identical(field_profile$mapping_fields(NULL), character(0))
  expect_identical(
    field_profile$mapping_fields(data.frame(isolate = c("A", "B"))),
    character(0)
  )
})

test_that("the palette family follows the number of groups", {
  # Inside the smallest brewer palette, a qualitative scale reads best.
  expect_identical(
    field_profile$scale_categories_for(rep(letters[1:4], 5), "Sequential")[1],
    "Qualitative"
  )
  # Past it, brewer runs out of colours and draws the rest grey, so a generated
  # scale leads instead.
  expect_identical(
    field_profile$scale_categories_for(as.character(1:40), "Sequential")[1],
    "Gradient"
  )
  # Numbers keep the numeric families entirely.
  expect_identical(
    field_profile$scale_categories_for(1:40, c("Sequential", "Gradient")),
    c("Sequential", "Gradient")
  )
})

test_that("empty strings do not count as a group", {
  expect_identical(field_profile$field_levels(c("a", "b", "", NA, "  ")), 2L)
})

# --- The profile frame -------------------------------------------------------

profiled <- function(...) {
  field_profile$field_profiles(meta_fixture(), ...)
}

test_that("every column is profiled, including the one that cannot group", {
  p <- profiled()

  # This is the whole point of the rewrite: the old shape picker *hid* the
  # columns it could not draw, so a user who went looking for `country` found
  # nothing and no reason why. Now every column is listed and says what it is.
  expect_setequal(p$field, names(meta_fixture()))

  # A constant column still groups, trivially, into a single bucket.
  expect_true(field_profile$profile_for(p, "organism")$groupable)
  # A column with a different value per isolate cannot group at all.
  expect_false(field_profile$profile_for(p, "collected_by")$groupable)
  expect_true(field_profile$profile_for(p, "purpose")$groupable)
})

test_that("the usable columns lead their group", {
  p <- profiled()
  base <- p[p$group == GROUP_ORDER[[1]], ]

  # Ordering the picker is the recommendation, so the columns that group come
  # before the ones that cannot.
  expect_identical(base$field[[1]], "purpose")
  expect_lt(match("country", base$field), match("source", base$field))
  expect_gt(match("organism", base$field), match("source", base$field))
})

test_that("shape eligibility follows the level count, not the column's name", {
  p <- profiled()
  expect_true(field_profile$profile_for(p, "purpose")$shapeable)
  # Twenty levels is past ggplot2's six-shape ceiling.
  expect_false(field_profile$profile_for(p, "country")$shapeable)
})

test_that("a declared type survives a column R reads as text", {
  # SQLite hands every metadata column back as character, so `is.numeric()` on
  # a date column says FALSE. A discrete scale over 300 distinct dates is 300
  # unordered colours, which is why the declared type has to win here.
  md <- data.frame(
    isolate = c("A", "B", "C"),
    sample_collection_date = c("2024-01-01", "2024-06-01", "2024-09-01"),
    stringsAsFactors = FALSE
  )
  row <- field_profile$profile_for(field_profile$field_profiles(md),
    "sample_collection_date")

  expect_identical(row$type, "date")
  expect_false(row$numeric)
  expect_true(row$continuous)
})

test_that("a date column nothing parses is still declared a date", {
  # The module's thesis: a type is declared, never guessed. Guessing from
  # contents is exactly what made the built-in and custom date columns behave
  # differently in the first place.
  md <- data.frame(
    isolate = c("A", "B"),
    sample_collection_date = c("not a date", "also not"),
    stringsAsFactors = FALSE
  )
  row <- field_profile$profile_for(field_profile$field_profiles(md),
    "sample_collection_date")

  expect_identical(row$type, "date")
  expect_true(row$continuous)
})

test_that("derived columns carry the type their prefix declares", {
  md <- data.frame(
    isolate = c("A", "B", "C"),
    mlst_adk = c("12", "44", "12"),
    `amr_Beta-lactam` = c("blaOXA", "", "blaOXA, blaPDC"),
    amr_g1 = factor(c("Match", "Absent", "Match")),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
  p <- field_profile$field_profiles(md)

  # An allele identifier is a name that happens to look like a number.
  expect_identical(field_profile$profile_for(p, "mlst_adk")$type, "category")
  # Both AMR levels are categories: a set of genes, or a call on one gene.
  expect_identical(
    field_profile$profile_for(p, "amr_Beta-lactam")$type, "category"
  )
  expect_identical(field_profile$profile_for(p, "amr_g1")$type, "category")
})

test_that("groups match the picker's optgroups, in the picker's order", {
  md <- data.frame(
    isolate = c("A", "B", "C"),
    organism = c("x", "y", "x"),
    mlst_st = c("1", "2", "1"),
    amr_gly = c("a", "b", "a"),
    custom_site = c("p", "q", "p"),
    stringsAsFactors = FALSE
  )
  p <- field_profile$field_profiles(md)

  expect_true(all(p$group %in% GROUP_ORDER))
  # prepare_choices() derives its optgroups from unique(group) in row order, so
  # this ordering IS the picker's ordering — and it has to agree with
  # grouped_field_choices(), which builds its optgroups from the same list.
  expect_identical(unique(p$group), GROUP_ORDER)
})

test_that("only the derived families are offered for a heatmap", {
  md <- data.frame(
    isolate = c("A", "B", "C"),
    organism = c("x", "y", "x"),
    mlst_st = c("1", "2", "1"),
    custom_site = c("p", "q", "p"),
    stringsAsFactors = FALSE
  )
  p <- field_profile$field_profiles(md)

  # A heatmap is a matrix under one shared fill scale, which only means
  # something when its columns are the same kind of measurement repeated.
  # Sample metadata is a bag of unrelated fields.
  expect_false(field_profile$profile_for(p, "organism")$heatmapable)
  expect_true(field_profile$profile_for(p, "mlst_st")$heatmapable)
  expect_true(field_profile$profile_for(p, "custom_site")$heatmapable)
})

test_that("the option sub-text names the count and the type", {
  p <- profiled()
  desc <- field_profile$profile_description(p)

  expect_identical(desc[p$field == "country"], "20 values · Text")
  # Singular, and no caveat — a constant column still groups, trivially.
  expect_identical(desc[p$field == "organism"], "1 value · Text")
})

test_that("an empty metadata table profiles to an empty frame", {
  p <- field_profile$field_profiles(data.frame())
  expect_identical(nrow(p), 0L)
  expect_true(all(c("field", "levels", "groupable") %in% names(p)))
  expect_null(field_profile$profile_for(p, "anything"))
})
