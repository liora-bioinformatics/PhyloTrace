box::use(
  shiny[MockShinySession],
  testthat[expect_no_error, expect_true, test_that],
)
box::use(
  app / logic / field_profile[field_profiles],
  app / logic / viz_helpers,
)

meta <- function() {
  data.frame(
    isolate = sprintf("ISO-%02d", 1:8),
    organism = rep("P. aeruginosa", 8),
    purpose = rep(c("outbreak", "surveillance"), 4),
    stringsAsFactors = FALSE
  )
}

# These call shinyWidgets for real rather than through an engine's observer.
# That matters: an error raised inside shiny$observe() is logged and swallowed,
# so a testServer assertion about the resulting state stays green while the
# control it was meant to fill never gets populated. That is exactly how an
# inverted argument order reached a running app — updateVirtualSelect() takes
# `inputId` first and `session` last, the opposite of updatePickerInput(), so
# passing the session positionally sends it in as the id.

test_that("a field picker builds from a profile frame", {
  expect_no_error(
    viz_helpers$field_select(
      function(x) x,
      "some_id",
      "Variable",
      profiles = field_profiles(meta())
    )
  )
})

test_that("a field picker builds with a sentinel entry", {
  expect_no_error(
    viz_helpers$field_select(
      function(x) x,
      "some_id",
      "Stratify by",
      profiles = field_profiles(meta()),
      extra = c(`No stratification` = "")
    )
  )
})

test_that("a field picker builds before any database is loaded", {
  expect_no_error(viz_helpers$field_select(function(x) x, "some_id", "Variable"))
})

test_that("refilling a field picker reaches shinyWidgets without erroring", {
  session <- MockShinySession$new()
  expect_no_error(
    viz_helpers$update_field_select(
      session,
      "some_id",
      field_profiles(meta()),
      selected = "purpose"
    )
  )
})

test_that("refilling with a sentinel and a disabled column both survive", {
  session <- MockShinySession$new()
  # `organism` is one value for every isolate, so it is listed but disabled —
  # the path that passes disabledChoices through.
  expect_no_error(
    viz_helpers$update_field_select(
      session,
      "some_id",
      field_profiles(meta()),
      selected = "",
      extra = c(`No annotation` = "")
    )
  )
})

test_that("refilling with no profiles is a no-op rather than an error", {
  session <- MockShinySession$new()
  expect_no_error(viz_helpers$update_field_select(session, "some_id", NULL))
})

test_that("a colour swatch row carries an id the module can toggle", {
  html <- as.character(viz_helpers$viz_color(
    function(x) paste0("mod-", x),
    "nj_tiplab_color",
    "Tip Label",
    "#000000"
  ))
  # The row, not the hidden input: colorPickr renders its swatch as a sibling,
  # so disabling the input would leave the swatch live.
  expect_true(grepl('id="mod-nj_tiplab_color_row"', html, fixed = TRUE))
})
