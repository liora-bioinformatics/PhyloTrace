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
  expect_match(viz_export$export_hint("widget", "png", NA, 4), "4")
  expect_match(viz_export$export_hint("widget", "html", NA, 4), "interactive")
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
