box::use(
  testthat[
    expect_equal,
    expect_false,
    expect_gte,
    expect_identical,
    expect_lte,
    expect_named,
    expect_true,
    test_that
  ],
)
box::use(
  app / logic / viz_legend,
)

test_that("a guide longer than its column folds its keys, once", {
  # Past one fold the answer is fewer keys, not a guide box wider than the
  # drawing beside it.
  expect_identical(viz_legend$legend_ncol(8L, 20L), 1L)
  expect_identical(viz_legend$legend_ncol(40L, 20L), 2L)
  expect_identical(
    viz_legend$legend_ncol(400L, 20L),
    viz_legend$LEGEND_KEY_COLS
  )
})

test_that("the plan budgets keys per guide and costs the rows they take", {
  demands <- c(`fill:Gene call` = 4L, `class:Resistance` = 40L, `strip:Ward` = 6L)
  plan <- viz_legend$legend_plan(demands, room = 30L)
  expect_named(plan$keys, names(demands))
  # The short lists are completed; the long one is trimmed to what is left.
  expect_equal(unname(plan$keys[["fill:Gene call"]]), 4L)
  expect_equal(unname(plan$keys[["strip:Ward"]]), 6L)
  expect_lte(unname(plan$keys[["class:Resistance"]]), 40L)
  expect_lte(plan$rows, plan$room)
  expect_identical(unname(plan$order), seq_along(demands))

  # A box too short for every guide's floor keeps the floor and reports the
  # overflow, which is the engine's cue to shrink the type.
  tight <- viz_legend$legend_plan(demands, room = 5L)
  expect_true(tight$rows > tight$room)
  expect_gte(min(tight$keys), viz_legend$LEGEND_MIN_KEYS)

  # Nothing to plan is an empty plan, not an error.
  empty <- viz_legend$legend_plan(setNames(integer(0), character(0)), room = 10L)
  expect_identical(empty$rows, 0L)
})

test_that("a missing level always keeps its key", {
  levels <- c(sprintf("v%02d", 1:20), "NA")
  cut <- viz_legend$legend_breaks(levels, max_keys = 5L, missing = "NA")
  expect_true("NA" %in% cut$breaks)
  expect_identical(cut$total, 21L)
  expect_true(viz_legend$LEGEND_GAP_KEY %in% cut$breaks)

  # Without it named as missing, it is one more level to rank.
  plain <- viz_legend$legend_breaks(levels, max_keys = 5L)
  expect_false("NA" %in% plain$breaks)
})

test_that("a trimmed guide's title counts what it shows", {
  expect_identical(viz_legend$legend_title("Ward", 0L), "Ward")
  expect_identical(viz_legend$legend_title("Ward", 42L, 60L), "Ward\n18 of 60 shown")
  expect_identical(viz_legend$legend_title("Ward", 42L), "Ward\n+ 42 more")
})

test_that("the gap key's swatch is colourless and admitted by the limits", {
  cols <- c(a = "#111111", b = "#222222")
  expect_identical(viz_legend$legend_values(cols, c("a", "b")), cols)
  gapped <- viz_legend$legend_values(cols, c("a", viz_legend$LEGEND_GAP_KEY))
  expect_identical(
    unname(gapped[[viz_legend$LEGEND_GAP_KEY]]),
    viz_legend$LEGEND_GAP_COLOR
  )
  expect_identical(
    viz_legend$legend_limits(c("a", "b"), c("a", viz_legend$LEGEND_GAP_KEY)),
    c("a", "b", viz_legend$LEGEND_GAP_KEY)
  )
  expect_identical(viz_legend$legend_limits(c("a", "b"), c("a", "b")), NULL)
})

test_that("a legend column shrinks its type before it drops a key", {
  demands <- c(`fill:Gene call` = 4L, `strip:Country` = 8L, `strip:Ward` = 36L)
  caps <- c(4L, 8L, viz_legend$LEGEND_FULL_MAX)
  # Rows the column has at a type size: the taller the type, the fewer.
  plan_at <- function(room_at_9pt) {
    function(pt) {
      viz_legend$legend_plan(demands, floor(room_at_9pt * 9 / pt), full_max = caps)
    }
  }

  # Room for everything at 9pt: the size asked for, nothing trimmed but the
  # capped population.
  roomy <- viz_legend$legend_fit(9, 5.5, plan_at(60))
  expect_equal(roomy$size, 9)
  expect_true(roomy$complete)
  expect_equal(unname(roomy$plan$keys[["strip:Country"]]), 8L)
  expect_equal(unname(roomy$plan$keys[["strip:Ward"]]), viz_legend$LEGEND_FULL_MAX)

  # Not at 9pt, but at a smaller legible size: shrunk, still complete.
  squeezed <- viz_legend$legend_fit(9, 5.5, plan_at(30))
  expect_true(squeezed$size < 9 && squeezed$size >= 5.5)
  expect_true(squeezed$complete)

  # Not even at the floor: the floor, and only then are keys trimmed.
  tight <- viz_legend$legend_fit(9, 5.5, plan_at(12))
  expect_equal(tight$size, 5.5)
  expect_false(tight$complete)
  expect_true(unname(tight$plan$keys[["strip:Ward"]]) < viz_legend$LEGEND_FULL_MAX)
})

test_that("an engine with a tall column may let a long guide run down it", {
  # The Tree folds past LEGEND_MAX_ROWS to keep its box narrow; the AMR column
  # raises the ceiling so a vocabulary folds only when its share runs out.
  expect_identical(viz_legend$legend_max_rows(2L, 200L), viz_legend$LEGEND_MAX_ROWS)
  expect_identical(viz_legend$legend_max_rows(2L, 200L, cap = 40L), 40L)
  demands <- c(`class:Resistance` = 21L, `fill:Gene call` = 4L)
  narrow <- viz_legend$legend_plan(demands, 200L, full_max = demands)
  tall <- viz_legend$legend_plan(demands, 200L, full_max = demands, max_rows_cap = 21L)
  expect_identical(unname(narrow$ncol[["class:Resistance"]]), 2L)
  expect_identical(unname(tall$ncol[["class:Resistance"]]), 1L)
})
