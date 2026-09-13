box::use(
  testthat[
    expect_equal,
    expect_false,
    expect_gt,
    expect_gte,
    expect_identical,
    expect_null,
    expect_true,
    test_that
  ],
)
box::use(
  app / logic / viz_fit,
)

impl <- attr(viz_fit, "namespace")

test_that("a label is fitted to its room and never shrunk past the floor", {
  # As large as asked for where the slot has room.
  expect_equal(viz_fit$fit_type(9, room = 20), 9)
  # Cut back to the room where it does not.
  expect_equal(viz_fit$fit_type(9, room = 6), 6)
  # Asked for less than the floor, it is still set at the floor.
  expect_equal(viz_fit$fit_type(3, room = 20), viz_fit$MIN_PRINT_PT)
  # A room under the floor comes back as the room itself, which is what says
  # the label cannot be drawn legibly at all.
  expect_equal(viz_fit$fit_type(9, room = 3), 3)
  expect_false(viz_fit$type_drawn(viz_fit$fit_type(9, room = 3)))
  expect_true(viz_fit$type_drawn(viz_fit$fit_type(9, room = 6)))
  expect_false(viz_fit$type_drawn(NA_real_))
})

test_that("the text scale is held to its range and cleans bad values", {
  expect_equal(viz_fit$text_scale(1.4), 1.4)
  expect_equal(viz_fit$text_scale(99), viz_fit$TEXT_SCALE_MAX)
  expect_equal(viz_fit$text_scale(0.01), viz_fit$TEXT_SCALE_MIN)
  for (bad in list(NULL, NA, "big", c(1, 2), -1, Inf)) {
    expect_equal(viz_fit$text_scale(bad), viz_fit$TEXT_SCALE_DEFAULT)
  }
  # The slider speaks percent; the engines a multiplier.
  expect_equal(viz_fit$text_scale_percent(150), 1.5)
  expect_equal(viz_fit$text_scale_percent(NULL), 1)
  expect_equal(viz_fit$TEXT_SIZE_MIN, viz_fit$TEXT_SCALE_MIN * 100)
  expect_equal(viz_fit$TEXT_SIZE_MAX, viz_fit$TEXT_SCALE_MAX * 100)
})

test_that("largest_fitting finds the biggest size that fits, or the floor", {
  fits <- function(pt) pt <= 7.3
  got <- viz_fit$largest_fitting(10, fits)
  expect_true(got <= 7.3 && got > 7.2)
  # Asked for something that fits, it is returned untouched.
  expect_equal(viz_fit$largest_fitting(6, fits), 6)
  # Nothing fits: the floor, for the caller to act on.
  expect_equal(
    viz_fit$largest_fitting(10, function(pt) FALSE),
    viz_fit$MIN_PRINT_PT
  )
})

test_that("the smallest drawn type ignores labels that are not drawn", {
  expect_equal(viz_fit$min_type_pt(9, NULL, 6.5, NA), 6.5)
  expect_equal(viz_fit$min_type_pt(), Inf)
})

test_that("the export only warns about type under the print floor", {
  expect_null(viz_fit$legibility_note(7))
  expect_null(viz_fit$legibility_note(Inf))
  note <- viz_fit$legibility_note(4.2, "Raise the text size.")
  expect_true(grepl("4.2 pt", note, fixed = TRUE))
  expect_true(grepl("Raise the text size.", note, fixed = TRUE))
})

test_that("strings are measured by what they set, line by line", {
  expect_gt(viz_fit$string_em("LONGNAME"), viz_fit$string_em("longname"))
  expect_equal(viz_fit$string_em(c("", NA)), c(0, 0))
  expect_equal(
    viz_fit$text_width_in(c("ab", "abcd"), 72),
    viz_fit$string_em("abcd")
  )
  # A two-line label is as wide as its longer line.
  expect_equal(
    viz_fit$text_width_in("ab\nabcd", 72),
    viz_fit$string_em("abcd")
  )
})

test_that("a canvas side is drawn in pixels no larger than the ceiling", {
  expect_identical(viz_fit$canvas_px(5.5), as.integer(5.5 * viz_fit$PLOT_RES))
  expect_identical(viz_fit$canvas_px(1000), as.integer(viz_fit$PLOT_MAX_PX))
})

test_that("a character the width table has never seen is booked at the widest", {
  # The caption is the one string on the figure the reader types, and since it
  # takes free text it can carry anything: a Greek letter in a gene name, an en
  # dash in a range, a middle dot in a unit. Almost all of those set narrower
  # than the Latin mean and one guess is as good as another — but an em dash,
  # an arrow and a CJK glyph set a full em, and a column short by two thirds is
  # the caption clipped at the panel edge. So the fallback covers the widest
  # rather than the average.
  expect_gt(impl$CHAR_EM_UNKNOWN, viz_fit$MEAN_CHAR_EM)
  expect_gte(impl$CHAR_EM_UNKNOWN, impl$CHAR_EM[["W"]])
  expect_equal(viz_fit$string_em("\u4e2d"), impl$CHAR_EM_UNKNOWN)

  grDevices::png(tempfile(), width = 6, height = 4, units = "in", res = 100)
  on.exit(grDevices::dev.off(), add = TRUE)
  graphics::par(ps = 12)
  em <- graphics::strwidth("M", units = "inches") / impl$CHAR_EM[["M"]]
  # Every one of these has to come out reserved for, never short. Checked
  # against the device rather than against a second table, so the two cannot
  # agree with each other and both be wrong.
  for (ch in c("\u03b2", "\u00b5", "\u2014", "\u2265", "\u00ab", "\u2192",
               "\u00e9", "\u00b0", "\u03a9", "\u4e2d")) {
    expect_gte(
      viz_fit$string_em(ch),
      graphics::strwidth(ch, units = "inches") / em - 0.02
    )
  }
})
