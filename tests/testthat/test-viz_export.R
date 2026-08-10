box::use(
  app / logic / viz_export,
  ggplot2[aes, element_rect, geom_col, ggplot, theme, theme_minimal],
)

impl <- attr(viz_export, "namespace")

fixture_plot <- function(bg = "#fdf6e3") {
  ggplot(
    data.frame(x = LETTERS[1:6], y = c(3, 7, 2, 9, 4, 6)),
    aes(.data$x, .data$y)
  ) +
    geom_col(fill = "#1f4e5d") +
    theme_minimal(base_size = 12) +
    theme(plot.background = element_rect(fill = bg, colour = NA))
}

# The first byte triple of a PNG's first scanline, which is the top-left pixel.
# Read straight out of the file rather than through an image library, since none
# of the ones that could do it are dependencies here.
png_corner <- function(file) {
  raw_bytes <- readBin(file, "raw", file.size(file))
  i <- 9L
  idat <- raw()
  while (i < length(raw_bytes)) {
    len <- sum(as.integer(raw_bytes[i:(i + 3)]) * 256^(3:0))
    type <- rawToChar(raw_bytes[(i + 4):(i + 7)])
    if (len > 0 && identical(type, "IDAT")) {
      idat <- c(idat, raw_bytes[(i + 8):(i + 7 + len)])
    }
    i <- i + 12L + len
  }
  # Scanline 0 is prefixed with its filter byte; for a flat background the
  # filter is "none", so the raw bytes after it are the pixel itself.
  as.integer(memDecompress(idat, "gzip")[2:(1 + 3)])
}

# Reads a PNG's own pHYs chunk (if any) and reports it as DPI, independent of
# save_plot_export()'s own chunk-writing code — a test that called the same
# helper the implementation uses to build the chunk would only prove the two
# agree with each other, not that either is right against the PNG spec (unit
# 1 = pixels/metre; 1 in = 0.0254 m). NULL means no pHYs chunk was found,
# which is exactly the state a stock ggsave() PNG is in.
png_dpi <- function(file) {
  raw_bytes <- readBin(file, "raw", file.size(file))
  i <- 9L
  while (i < length(raw_bytes)) {
    len <- sum(as.integer(raw_bytes[i:(i + 3)]) * 256^(3:0))
    type <- rawToChar(raw_bytes[(i + 4):(i + 7)])
    if (identical(type, "pHYs")) {
      data <- raw_bytes[(i + 8):(i + 7 + len)]
      ppux <- sum(as.integer(data[1:4]) * 256^(3:0))
      ppuy <- sum(as.integer(data[5:8]) * 256^(3:0))
      unit <- as.integer(data[9])
      if (unit != 1L) {
        return(NULL) # not metres; DPI is not what this chunk records
      }
      return(list(x = round(ppux * 0.0254), y = round(ppuy * 0.0254)))
    }
    i <- i + 12L + len
  }
  NULL
}

test_that("every offered format is written by a device that can produce it", {
  plot <- fixture_plot()
  for (format in viz_export$export_formats$ggplot) {
    file <- withr::local_tempfile(fileext = paste0(".", format))
    viz_export$save_plot_export(
      plot,
      file,
      format,
      width_cm = 12,
      aspect = 0.6,
      dpi = 150
    )
    expect_true(file.exists(file), info = format)
    expect_gt(file.size(file), 0)
  }
})

test_that("a raster is written at the requested physical size and resolution", {
  file <- withr::local_tempfile(fileext = ".png")
  viz_export$save_plot_export(
    fixture_plot(),
    file,
    "png",
    width_cm = 10,
    aspect = 0.5,
    dpi = 300
  )
  # 10 cm at 300 dpi is 1181 px; the device rounds, so allow a pixel either way.
  header <- readBin(file, "raw", 33)
  width <- sum(as.integer(header[17:20]) * 256^(3:0))
  height <- sum(as.integer(header[21:24]) * 256^(3:0))
  expect_lt(abs(width - 1181), 2)
  expect_lt(abs(height - 591), 2)
})

test_that("the plot's own background reaches the file", {
  # Not cosmetic: the engines set plot.background from their colour picker, and
  # the letterbox around a fixed-aspect panel has to take the same colour as the
  # plot it surrounds. Leaving bg to ggsave is what keeps that true.
  file <- withr::local_tempfile(fileext = ".png")
  viz_export$save_plot_export(
    fixture_plot(bg = "#fdf6e3"),
    file,
    "png",
    width_cm = 6,
    aspect = 0.5,
    dpi = 72
  )
  expect_identical(png_corner(file), c(253L, 246L, 227L))
})

test_that("vector formats really are vector", {
  file <- withr::local_tempfile(fileext = ".svg")
  viz_export$save_plot_export(fixture_plot(), file, "svg", width_cm = 12)
  head <- paste(readLines(file, n = 2, warn = FALSE), collapse = "")
  expect_true(grepl("<svg", head, fixed = TRUE))
})

test_that("a PNG records its own resolution, not just its pixel count", {
  # png(res=)/ggsave() size the pixel grid correctly but write no pHYs chunk at
  # all — confirmed independently with ImageMagick's `identify -verbose`
  # ("Units: Undefined") and Pillow (`Image.info["dpi"]` absent) before this
  # existed. Without it, a raster's physical size is implied only by comparing
  # pixel count to a size no one downstream is told — a print shop, a journal
  # submission checker, or a generic "what DPI is this" tool reads exactly this
  # chunk and nothing else.
  for (dpi in c(150, 300, 600)) {
    file <- withr::local_tempfile(fileext = ".png")
    viz_export$save_plot_export(
      fixture_plot(),
      file,
      "png",
      width_cm = 12,
      aspect = 0.6,
      dpi = dpi
    )
    found <- png_dpi(file)
    expect_false(is.null(found), info = dpi)
    # The pHYs chunk only stores an integer pixels-per-metre value, so the
    # round trip back to DPI is exact to within rounding, not bit-for-bit.
    expect_lt(abs(found$x - dpi), 1)
    expect_lt(abs(found$y - dpi), 1)
  }
})

test_that("a PNG's resolution chunk survives real-world readers", {
  skip_if_not(nzchar(Sys.which("identify")), "ImageMagick not installed")
  file <- withr::local_tempfile(fileext = ".png")
  viz_export$save_plot_export(
    fixture_plot(),
    file,
    "png",
    width_cm = 10,
    aspect = 0.6,
    dpi = 300
  )
  out <- system2("identify", c("-format", "%x", file), stdout = TRUE)
  # ImageMagick reports in its own default unit (pixels/cm here); 300 dpi is
  # 300/2.54 px/cm.
  expect_lt(abs(as.numeric(out) - 300 / 2.54), 0.5)
})

test_that("vector and non-PNG raster formats are left untouched", {
  # The pHYs fix is PNG-specific: JPEG/TIFF already write their own resolution
  # metadata (JFIF density, XResolution/YResolution — confirmed independently),
  # and vector formats have no pixel grid to attach one to.
  for (format in c("jpeg", "tiff", "pdf", "svg")) {
    file <- withr::local_tempfile(fileext = paste0(".", format))
    viz_export$save_plot_export(fixture_plot(), file, format, width_cm = 10, dpi = 300)
    expect_true(file.exists(file) && file.size(file) > 0, info = format)
  }
})

test_that("resolved_dpi caps a request that would not fit in memory", {
  # 60 cm at 1200 dpi is ~28000 px across; the DPI comes down to fit rather than
  # letting the device fail on allocation.
  expect_lt(viz_export$resolved_dpi(60, 1, 1200), 1200)
  expect_identical(viz_export$resolved_dpi(60, 1, 1200) * 60 / 2.54 <= 20000, TRUE)
  # A request that already fits is passed through untouched.
  expect_identical(viz_export$resolved_dpi(12, 0.6, 300), 300)
})

test_that("write_data_uri refuses an empty or absent capture", {
  file <- withr::local_tempfile(fileext = ".png")
  expect_false(viz_export$write_data_uri(NULL, file))
  expect_false(viz_export$write_data_uri("", file))
  expect_false(viz_export$write_data_uri(NA_character_, file))
})

test_that("write_data_uri decodes a data URI to its bytes", {
  file <- withr::local_tempfile(fileext = ".png")
  # "hello" in base64.
  expect_true(viz_export$write_data_uri("data:image/png;base64,aGVsbG8=", file))
  expect_identical(rawToChar(readBin(file, "raw", 5)), "hello")
})

test_that("the hint states what each format will actually produce", {
  # A raster is described in pixels, because DPI alone says nothing without a
  # size to apply it to.
  expect_match(
    viz_export$export_hint("ggplot", "png", 10, 300, 0.5),
    "^1181 . 59[01] px at 300 dpi"
  )
  expect_match(viz_export$export_hint("ggplot", "pdf", 10, 300), "Vector")
  expect_match(viz_export$export_hint("widget", "png", NA, 4000), "4,000")
  expect_match(viz_export$export_hint("widget", "html", NA, 4000), "interactive")
})

test_that("the widget hint calls out low-detail targets", {
  expect_match(viz_export$export_hint("widget", "png", NA, 6000), "enough for print")
  expect_match(viz_export$export_hint("widget", "png", NA, 800), "raise for more detail")
})

test_that("export file names are date-stamped and filesystem-safe", {
  name <- viz_export$export_filename("amr class", "png")
  expect_identical(name, paste0(Sys.Date(), "_amr_class.png"))
})

test_that("each engine kind offers only formats it can actually write", {
  # The widget engines have no server-side plot object, so no vector format can
  # be offered for them; HTML is their lossless export instead.
  expect_length(intersect(
    viz_export$export_formats$widget,
    viz_export$vector_formats
  ), 0L)
  expect_true("html" %in% viz_export$export_formats$widget)
  expect_false("html" %in% viz_export$export_formats$ggplot)
})

test_that("an unknown format is refused rather than silently written", {
  expect_error(impl$.device_for("webp", 300), "Unsupported export format")
})

test_that("export_presets defines the same four use cases for both kinds", {
  # Every preset must be able to drive either engine kind, since a preset id
  # travels from the modal (which doesn't know the engine's kind) to whichever
  # kind is actually open.
  ids <- vapply(viz_export$export_presets, `[[`, character(1), "id")
  expect_identical(anyDuplicated(ids), 0L)
  for (p in viz_export$export_presets) {
    expect_true(all(c("format", "width_cm", "dpi") %in% names(p$ggplot)), info = p$id)
    expect_true(all(c("format", "target_px") %in% names(p$widget)), info = p$id)
  }
})

test_that("export_preset looks up a preset by id, or returns NULL", {
  found <- viz_export$export_preset("publication")
  expect_identical(found$id, "publication")
  expect_null(viz_export$export_preset("no-such-preset"))
})

test_that("preset ggplot widths stay within the exportable range", {
  # numericInput's own min/max (4-60cm) would silently clamp a preset that
  # asked for something outside it, quietly producing a different plot than
  # the preset's name promises.
  for (p in viz_export$export_presets) {
    expect_true(p$ggplot$width_cm >= 4 && p$ggplot$width_cm <= 60, info = p$id)
  }
})

test_that("the modal offers no raw size/quality controls, only presets", {
  # A preset is the only thing that decides width/DPI (ggplot) or target pixel
  # width (widget) — an earlier version tucked a second, freely-typed size
  # control into a collapsed "Advanced" section beside the four preset
  # choices, which added a decision without adding a use case.
  ggplot_modal <- as.character(viz_export$export_modal(identity, "e", "ggplot"))
  widget_modal <- as.character(viz_export$export_modal(identity, "e", "widget"))

  expect_false(grepl("e_width", ggplot_modal, fixed = TRUE))
  expect_false(grepl("e_dpi", ggplot_modal, fixed = TRUE))
  expect_false(grepl("e_target_px", ggplot_modal, fixed = TRUE))
  expect_false(grepl("e_target_px", widget_modal, fixed = TRUE))
  expect_false(grepl("e_width", widget_modal, fixed = TRUE))
  expect_false(grepl("e_dpi", widget_modal, fixed = TRUE))
})

test_that("every preset is offered as a choice in the modal", {
  html <- as.character(viz_export$export_modal(identity, "e", "ggplot"))
  for (p in viz_export$export_presets) {
    expect_true(grepl(p$label, html, fixed = TRUE), info = p$id)
  }
})

test_that("the format picker spans the same width as the preset row", {
  # pickerInput's own container is a fixed-width form-group by default
  # (bootstrap-select's stock CSS caps it at 220px) unless told otherwise, so
  # without an explicit width it renders narrower than the preset buttons
  # beneath it — the two rows visibly disagreeing on how wide "full width"
  # is. `width=` here reaches the same .form-group wrapper the preset row's
  # own `width=` reaches on its outer div, so both actually agree.
  html <- as.character(viz_export$export_modal(identity, "e", "ggplot"))
  expect_true(grepl(
    '<div class="form-group shiny-input-container" style="width:100%;">',
    html,
    fixed = TRUE
  ))
})

test_that("the modal footer commits or abandons the export", {
  html <- as.character(viz_export$export_modal(identity, "e", "ggplot"))
  expect_true(grepl("e_download", html, fixed = TRUE))
  expect_true(grepl("e_cancel", html, fixed = TRUE))
  # The download target belongs to the sidebar, not here — removeModal() would
  # destroy it before the server ever clicks it. Matched on the exact id, since
  # "e_filetype" contains "e_file".
  expect_false(grepl("id=\"e_file\"", html, fixed = TRUE))
})

test_that("reopening the modal restores the format and preset last chosen", {
  # Shiny keeps an input's value after its UI is removed, but re-rendering the
  # control resets it to its declaration — so the values have to be handed back
  # in, or every export would start from the defaults again.
  html <- as.character(viz_export$export_modal(
    identity,
    "e",
    "ggplot",
    values = list(filetype = "svg", preset = "print")
  ))
  expect_true(grepl("<option value=\"svg\" selected>", html, fixed = TRUE))
  expect_true(grepl('value="print" checked', html, fixed = TRUE))
})

test_that("the sidebar panel is just a trigger plus the download target", {
  html <- as.character(viz_export$export_panel(identity, "e"))
  expect_true(grepl("e_open", html, fixed = TRUE))
  expect_true(grepl("e_file", html, fixed = TRUE))
  expect_false(grepl("e_filetype", html, fixed = TRUE))
})
