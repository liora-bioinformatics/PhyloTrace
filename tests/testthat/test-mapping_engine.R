box::use(
  rlang[`%||%`],
  testthat[
    expect_false,
    expect_identical,
    expect_length,
    expect_null,
    expect_no_warning,
    expect_true,
    test_that
  ],
)
box::use(
  app / logic / field_profile,
  app / logic / mapping_engine,
  app / logic / tree_plot,
)

# One profile row, built the way field_profiles() would, for a column with
# `levels` distinct values across `n` isolates.
prof <- function(levels, n = 40, type = "text", field = "v", continuous = NULL) {
  vals <- rep(sprintf("g%02d", seq_len(levels)), length.out = n)
  md <- data.frame(isolate = sprintf("i%03d", seq_len(n)), stringsAsFactors = FALSE)
  md[[field]] <- vals
  p <- field_profile$field_profiles(md, types = stats::setNames(type, field))
  row <- p[p$field == field, , drop = FALSE]
  if (!is.null(continuous)) {
    row$continuous <- continuous
  }
  row
}

layers_of <- function(...) {
  specs <- list(...)
  out <- list()
  for (i in seq_along(specs)) {
    l <- mapping_engine$assign_mapping_layer(specs[[i]], out, id = paste0("L", i))
    out <- c(out, list(l))
  }
  out
}

# --- Which aesthetic ---------------------------------------------------------

test_that("a narrow variable takes shape, the scarcest aesthetic", {
  # Shape has a hard ceiling of six, so a variable that can use it should,
  # leaving the unbounded colour aesthetics for variables that cannot.
  l <- mapping_engine$assign_mapping_layer(prof(4))
  expect_identical(l$aesthetic, "tippoint_shape")
})

test_that("a wide variable takes a tile strip, not a colour", {
  # The reported case: 46 countries. Forty-six coloured 2 mm characters is
  # noise; a band of shades beside the tree is a gradient a reader can follow.
  l <- mapping_engine$assign_mapping_layer(prof(46, n = 346))
  expect_identical(l$aesthetic, "tile")
  expect_identical(l$family, "Gradient")
})

test_that("a mid-width variable colours the label", {
  l <- mapping_engine$assign_mapping_layer(prof(8))
  expect_identical(l$aesthetic, "tiplab_color")
  expect_identical(l$family, "Qualitative")
})

test_that("a continuous variable never lands on shape", {
  l <- mapping_engine$assign_mapping_layer(prof(5, continuous = TRUE))
  expect_false(identical(l$aesthetic, "tippoint_shape"))
  expect_identical(l$family, "Gradient")
})

test_that("a variable unique to every isolate is offered no aesthetic at all", {
  expect_length(mapping_engine$eligible_aesthetics(prof(40, n = 40)), 0L)
  expect_null(mapping_engine$assign_mapping_layer(prof(40, n = 40)))
})

test_that("a constant variable still takes an aesthetic", {
  # It groups trivially into one bucket rather than being hidden outright —
  # useful to confirm a trait every isolate here shares.
  l <- mapping_engine$assign_mapping_layer(prof(1))
  expect_identical(l$aesthetic, "tippoint_shape")
})

test_that("each exclusive aesthetic is claimed at most once", {
  # A tip has one label, one point colour and one shape; those cannot repeat.
  ls <- layers_of(prof(3, field = "a"), prof(4, field = "b"),
    prof(5, field = "c"), prof(6, field = "d"))
  aes <- vapply(ls, function(l) l$aesthetic, character(1))
  excl <- aes[!aes %in% mapping_engine$REPEATABLE_AESTHETICS]
  expect_identical(length(unique(excl)), length(excl))
})

test_that("tile strips stack, up to their own limit", {
  # Several strips side by side is the normal way to read a few categorical
  # variables against a tree, so `tile` is the one aesthetic that repeats.
  wide <- lapply(1:6, function(i) prof(20, n = 300, field = paste0("t", i)))
  ls <- do.call(layers_of, wide)
  tiles <- Filter(function(l) identical(l$aesthetic, "tile"), ls)
  expect_identical(length(tiles), mapping_engine$MAX_TILES)
})

test_that("a layer past the ceiling has nowhere to go", {
  # Three exclusive aesthetics plus MAX_TILES strips, and then no more: every
  # strip widens the canvas, so the tree's share of the page shrinks with each.
  specs <- lapply(
    seq_len(mapping_engine$MAX_LAYERS),
    function(i) prof(if (i <= 3) i + 2 else 20, n = 300, field = paste0("f", i))
  )
  ls <- do.call(layers_of, specs)
  expect_identical(length(ls), mapping_engine$MAX_LAYERS)
  expect_null(mapping_engine$assign_mapping_layer(prof(3, field = "z"), ls))
})

# --- Which palette -----------------------------------------------------------

test_that("two colour layers never share a palette", {
  # Same family, so the ring is the only thing keeping them apart.
  ls <- layers_of(prof(8, field = "a"), prof(8, field = "b"))
  pals <- vapply(ls, function(l) l$palette %||% "", character(1))
  expect_identical(length(unique(pals)), 2L)

  # And again for the generated family.
  wide <- layers_of(prof(30, n = 300, field = "a"),
    prof(40, n = 300, field = "b"))
  wpals <- vapply(wide, function(l) l$palette %||% "", character(1))
  expect_identical(length(unique(wpals)), 2L)
})

test_that("a shape layer contributes no palette to collide with", {
  l <- mapping_engine$assign_mapping_layer(prof(4))
  expect_null(l$palette)
})

test_that("the palette family follows the level count", {
  expect_identical(mapping_engine$palette_family(prof(9)), "Qualitative")
  # Past the smallest brewer palette a tabulated scale runs out of colours.
  expect_identical(mapping_engine$palette_family(prof(10)), "Gradient")
})

# --- The reported bug, caught at the decision --------------------------------

test_that("no palette overflows, however many levels it is asked for", {
  # This is the exact console warning the user reported:
  #   "n too large, allowed maximum for palette Set1 is 9"
  # which left 37 of 46 countries grey.
  expect_no_warning(cols <- tree_plot$tree_discrete_colors("Set1", 46))
  expect_length(cols, 46L)
  expect_identical(length(unique(cols)), 46L)
})

test_that("a palette within capacity is unchanged", {
  # The fix must not repaint trees that were already correct.
  expect_identical(
    tree_plot$tree_discrete_colors("Set1", 5),
    RColorBrewer::brewer.pal(5, "Set1")
  )
})

# --- Rebalancing -------------------------------------------------------------

test_that("rebalancing leaves a layer the user edited exactly as it was", {
  ls <- layers_of(prof(4, field = "a"), prof(8, field = "b"))
  ls[[1]]$auto <- FALSE
  ls[[1]]$palette <- "Accent"
  ls[[1]]$aesthetic <- "tile"

  md <- data.frame(
    isolate = sprintf("i%03d", 1:40),
    a = rep(sprintf("g%d", 1:4), length.out = 40),
    b = rep(sprintf("h%d", 1:8), length.out = 40),
    stringsAsFactors = FALSE
  )
  out <- mapping_engine$rebalance_layers(ls, field_profile$field_profiles(md))

  expect_identical(out[[1]], ls[[1]])
  # And the automatic one moves off the aesthetic the pinned one now holds.
  expect_false(identical(out[[2]]$aesthetic, "tile"))
})

test_that("rebalancing never drops a layer", {
  ls <- layers_of(prof(3, field = "a"), prof(4, field = "b"))
  md <- data.frame(
    isolate = sprintf("i%03d", 1:40),
    a = rep(sprintf("g%d", 1:3), length.out = 40),
    b = rep(sprintf("h%d", 1:4), length.out = 40),
    stringsAsFactors = FALSE
  )
  out <- mapping_engine$rebalance_layers(ls, field_profile$field_profiles(md))
  expect_identical(length(out), 2L)
  expect_identical(vapply(out, function(l) l$field, character(1)), c("a", "b"))
})

# --- Explaining a refusal ----------------------------------------------------

test_that("an unavailable aesthetic says why", {
  # A dialog that silently omits the option leaves the user to guess, which is
  # the failure this rewrite exists to fix.
  msg <- mapping_engine$aesthetic_block_reason(prof(46, n = 346),
    "tippoint_shape")
  expect_true(grepl("46", msg))
  expect_null(mapping_engine$aesthetic_block_reason(prof(4), "tippoint_shape"))
})

# --- Grouping a date -----------------------------------------------------

# A date column spanning `days` at one isolate per step, profiled as a date.
date_prof <- function(n = 40, by = 9, field = "sample_collection_date") {
  vals <- as.character(
    seq(as.Date("2024-01-01"), by = paste(by, "days"), length.out = n)
  )
  md <- data.frame(isolate = sprintf("i%03d", seq_len(n)),
    stringsAsFactors = FALSE)
  md[[field]] <- vals
  p <- field_profile$field_profiles(md)
  list(profile = p[p$field == field, , drop = FALSE], values = vals, meta = md)
}

test_that("a raw collection date groups nothing, which is the reported bug", {
  d <- date_prof()
  # One distinct date per isolate, so every group is a singleton.
  expect_false(d$profile$groupable)
  expect_true(d$profile$continuous)
})

test_that("binning turns a date into the discrete variable it now is", {
  d <- date_prof()
  g <- mapping_engine$granularity_profile(d$profile, d$values, "month")

  expect_identical(g$levels, 12L)
  expect_false(g$continuous)
  expect_true(g$groupable)
  # Twelve months is past the six-shape ceiling.
  expect_false(g$shapeable)

  # A year collapses them far enough to take a shape.
  y <- mapping_engine$granularity_profile(d$profile, d$values, "year")
  expect_true(y$shapeable)
})

test_that("granularity_profile leaves a non-date, or an unbinned date, alone", {
  d <- date_prof()
  expect_identical(mapping_engine$granularity_profile(d$profile, d$values, NULL),
    d$profile)
  expect_identical(
    mapping_engine$granularity_profile(d$profile, d$values, "none"),
    d$profile
  )
  # A text column is not a date however it is labelled.
  expect_identical(mapping_engine$granularity_profile(prof(4), NULL, "month"),
    prof(4))
})

test_that("picking a date produces a mapping instead of a dead end", {
  # Without a granularity the raw column is refused outright, which is what
  # made every date variable unusable.
  d <- date_prof()
  expect_null(mapping_engine$assign_mapping_layer(d$profile))

  l <- mapping_engine$assign_mapping_layer(d$profile, values = d$values)
  expect_identical(l$granularity, "month")
  expect_identical(l$n_levels, 12L)
  expect_false(l$continuous)
  expect_identical(l$transform, "as_date")
})

test_that("changing the granularity re-derives what binning changes", {
  d <- date_prof()
  l <- mapping_engine$assign_mapping_layer(d$profile, values = d$values)

  y <- mapping_engine$set_layer_granularity(l, "year", d$values)
  expect_identical(y$granularity, "year")
  expect_identical(y$n_levels, 1L)
  expect_false(y$continuous)

  # Back to the exact date: continuous again, and no granularity recorded.
  none <- mapping_engine$set_layer_granularity(l, "none", d$values)
  expect_null(none$granularity)
  expect_true(none$continuous)
})

test_that("a palette follows the variable across the continuous boundary", {
  d <- date_prof()
  l <- mapping_engine$assign_mapping_layer(d$profile, values = d$values)
  l$aesthetic <- "tiplab_color"
  l$palette <- "viridis"
  l$family <- "Gradient"

  # A year is one level, which a qualitative palette can carry.
  y <- mapping_engine$set_layer_granularity(l, "year", d$values)
  expect_identical(y$family, "Qualitative")
  expect_true(y$palette %in% mapping_engine$PALETTE_RINGS$Qualitative)
})

test_that("set_layer_granularity ignores a variable that is not a date", {
  l <- mapping_engine$assign_mapping_layer(prof(4))
  expect_identical(mapping_engine$set_layer_granularity(l, "month", NULL), l)
})

test_that("a rebalance keeps the granularity the user chose", {
  # rebalance_layers() rebuilds automatic layers from the profile, and the
  # granularity is the one thing on them that the profile cannot supply.
  d <- date_prof()
  l <- mapping_engine$assign_mapping_layer(d$profile, values = d$values)
  p <- field_profile$field_profiles(d$meta)

  out <- mapping_engine$rebalance_layers(list(l), p, "tree", d$meta)
  expect_identical(out[[1]]$granularity, "month")
  expect_identical(out[[1]]$n_levels, 12L)
})

test_that("a refused date says how to make it work", {
  d <- date_prof()
  msg <- mapping_engine$aesthetic_block_reason(d$profile, "tippoint_shape")
  expect_true(grepl("month", msg))
})
