# app/logic/viz_export.R
#
# Plot export: the shared "Export Plot" panel, the devices behind each file
# format, and the size/resolution arithmetic every visualization engine writes
# its file through.
#
# Two kinds of plot are exported by two different routes, which is what the
# `kind` argument threaded through this file distinguishes.
#
# "ggplot" — the Tree, the Epi curve and the AMR views. The plot object lives on
# the server, so the export re-renders it at whatever physical size and
# resolution the user asks for. This is the route that reaches publication
# quality trivially: a vector PDF/SVG has no resolution at all, and a raster is
# drawn at the requested DPI rather than scraped off the screen.
#
# "widget" — the MST and the Map. Both are drawn by the browser, so there is no
# server-side plot object and the image has to come back from the client. The
# capture is NOT a screenshot of the on-screen pixels: app/view/visualization.R's
# `phylotrace_capture` handler makes the widget redraw itself into a larger
# buffer first (see the notes there), so a 4x export is a genuine re-render at
# four times the linear resolution rather than an upscale. Both widget engines
# also offer self-contained interactive HTML, which is the lossless option.

box::use(
  base64enc[base64decode],
  digest[digest],
  ggplot2[ggsave],
  grDevices[cairo_pdf, jpeg, png, tiff],
  shiny[
    actionButton,
    div,
    downloadButton,
    icon,
    modalDialog,
    tagList,
    tags,
    uiOutput,
  ],
  rlang[`%||%`],
  shinyWidgets[pickerInput, sliderTextInput],
  stats[setNames],
  svglite[svglite],
)

# --- Format definitions ------------------------------------------------------

#' File formats offered per engine kind.
#'
#' The widget engines have no server-side plot object, so they offer the two
#' raster formats their canvas can produce plus the interactive HTML that is
#' their lossless export. The ggplot engines add the two vector formats and
#' TIFF, which is what journals ask for when they do not take vector art.
#' @export
export_formats <- list(
  ggplot = c(
    "PNG" = "png",
    "JPEG" = "jpeg",
    "TIFF" = "tiff",
    "PDF (vector)" = "pdf",
    "SVG (vector)" = "svg"
  ),
  widget = c(
    "PNG" = "png",
    "JPEG" = "jpeg",
    "Interactive HTML" = "html"
  )
)

#' Formats with no resolution of their own, so DPI does not apply to them.
#' @export
vector_formats <- c("pdf", "svg")

#' Formats written by the browser rather than by a device on the server.
#' @export
raster_formats <- c("png", "jpeg", "tiff")

# Upper bound on either side of an exported raster, in pixels. A 60 cm wide
# figure at 1200 dpi is 28000 px across and would allocate ~3 GB; the DPI is
# reduced to fit rather than letting the device fail on allocation.
MAX_EXPORT_PX <- 20000L

#' Centimetres per inch. Exported because the plot modules size their exports
#' in inches and the export panel asks for centimetres.
#' @export
CM_PER_IN <- 2.54

# --- Sizes and resolutions ---------------------------------------------------
#' @export
export_sizes <- list(
  list(id = "column", label = "Journal column", width_cm = 8.9),
  list(id = "double", label = "Journal double column", width_cm = 18),
  list(id = "slide", label = "Slide", width_cm = 25),
  list(id = "poster", label = "Poster", width_cm = 50)
)

#' The size record for an id, or NULL.
#' @param id Character.
#' @return A list, or NULL.
#' @export
export_size <- function(id) {
  for (sz in export_sizes) {
    if (identical(sz$id, id)) {
      return(sz)
    }
  }
  NULL
}

#' Raster resolutions offered, coarsest first.
#'
#' Three, not a free number: dpi only decides how finely the *same* figure is
#' rasterised, so the useful range is "screen", "print" and "journal" and
#' anything between them is a distinction without a difference. Vector formats
#' take none of it.
#' Target pixel widths for the widget engines, which have no physical size of
#' their own — an interactive network is rendered at whatever width it is asked
#' for, so "how detailed" is the only question it can answer.
#' @export
export_widget_px <- c(
  `Screen` = 1600L,
  `Print` = 4000L,
  `Poster` = 6000L
)

#' @export
export_qualities <- c(
  `Screen` = 150L,
  `Print` = 300L,
  `Journal` = 600L
)


# --- UI ----------------------------------------------------------------------

#' Shared Visualization Export Panel UI
#'
#' The sidebar's "Export Plot" panel: a single button that opens the settings
#' modal. The settings themselves live in `export_modal()` rather than here,
#' since a sidebar column is a poor place to lay out a form and the export is a
#' deliberate, one-off action rather than a live control.
#'
#' The hidden `downloadButton` stays in the sidebar and must not move into the
#' modal: `removeModal()` destroys the modal's DOM, and the server clicks this
#' target *after* the modal is gone — for the widget engines, several seconds
#' after, once the browser has finished re-rendering.
#'
#' @param ns Function. Module namespace function (`session$ns`).
#' @param prefix Character. ID prefix unique to the calling module.
#' @return A `div` holding the trigger and the download target.
#' @export
export_panel <- function(ns, prefix) {
  id <- function(suffix) ns(paste0(prefix, "_", suffix))
  div(
    class = "viz-export",
    actionButton(
      id("open"),
      "Save plot",
      icon = icon("download"),
      width = "100%"
    ),
    # Clicked from the server once the file is ready to be handed over.
    div(
      class = "d-none",
      downloadButton(id("file"), "Download")
    )
  )
}

#' Export Settings Modal
#'
#' Two questions, and only two: what file to write, and — for a raster — how
#' finely to rasterise it.
#'
#' There used to be a third, a "Figure size" preset naming a journal column or
#' a slide. It has no place on an engine that already draws its plot at a size
#' the reader set: the aspect ratio, the type sizes and the room beside the
#' tree are all fitted to one canvas, and asking for the figure again at
#' another width means either rebuilding it (a different figure from the one on
#' screen) or stretching it (the same figure, wrongly proportioned). Engines
#' that know their own physical size say so (`width_cm` on their export
#' contract) and the picker is dropped for them; the rest still get it.
#'
#' File format is its own control rather than part of a preset: it is a
#' decision about the destination (a document, a print shop, a browser), and
#' most formats make sense for most destinations.
#'
#' Re-created on each open, so the caller passes the values the controls last
#' held — Shiny keeps an input's value after its UI is removed, but re-rendering
#' the control resets it to whatever the declaration says. Seeding them is what
#' stops the modal from forgetting the last choice between exports, however it
#' was dismissed.
#'
#' @param ns Function. Module namespace function (`session$ns`).
#' @param prefix Character. ID prefix unique to the calling module.
#' @param kind Character. "ggplot" or "widget"; see the file header.
#' @param values Named list of previously held control values.
#' @param sizes Logical. Offer the figure-size picker. FALSE for an engine that
#'   exports at the size it is already drawn at.
#' @return A `modalDialog` wrapped in its styling container.
#' @export
export_modal <- function(
  ns,
  prefix,
  kind = "ggplot",
  values = list(),
  sizes = TRUE
) {
  id <- function(suffix) ns(paste0(prefix, "_", suffix))
  held <- function(name, default) values[[name]] %||% default

  div(
    class = "export-modal",
    modalDialog(
      title = "Export plot",
      pickerInput(
        id("filetype"),
        "File format",
        choices = as.list(export_formats[[kind]]),
        selected = held("filetype", "png"),
        width = "100%"
      ),
      # Size is how big the figure is *made* — it decides how much type fits
      # beside the drawing, so it changes the figure. Quality is only how
      # finely that figure is rasterised, and a vector file has no such
      # question. Offered only to engines that do not already know their own
      # physical size; see `sizes`.
      if (identical(kind, "ggplot") && isTRUE(sizes)) {
        pickerInput(
          id("size"),
          "Figure size",
          choices = setNames(
            vapply(export_sizes, `[[`, character(1), "id"),
            vapply(
              export_sizes,
              function(s) sprintf("%s (%g cm)", s$label, s$width_cm),
              character(1)
            )
          ),
          selected = held("size", "double"),
          width = "100%"
        )
      },
      div(
        id = id("quality_wrap"),
        if (identical(kind, "ggplot")) {
          sliderTextInput(
            id("quality"),
            "Resolution",
            choices = names(export_qualities),
            selected = held("quality", "Print"),
            grid = TRUE,
            width = "100%"
          )
        } else {
          sliderTextInput(
            id("quality"),
            "Detail",
            choices = names(export_widget_px),
            selected = held("quality", names(export_widget_px)[[2]]),
            grid = TRUE,
            width = "100%"
          )
        }
      ),
      uiOutput(id("hint"), class = "viz-export-hint small text-muted"),
      footer = tagList(
        actionButton(id("cancel"), "Cancel"),
        actionButton(
          id("download"),
          "Save",
          icon = icon("download"),
          class = "btn-primary"
        )
      ),
      easyClose = TRUE
    )
  )
}

#' Describe what the current export settings will actually produce.
#'
#' The two numbers that decide the file and nothing else: how big the figure
#' is, and how finely it is rasterised. The pixel count they multiply out to
#' was a third way of saying the same thing, and "Exactly as displayed" was a
#' promise the modal keeps by construction — an engine that knows its own size
#' is not offered another one (see `sizes`).
#'
#' @param kind Character. "ggplot" or "widget".
#' @param format Character. Selected file format.
#' @param width_cm Numeric. Requested width in centimetres (ggplot only).
#' @param quality Numeric. DPI (ggplot) or target pixel width (widget).
#' @param aspect Numeric. Height-to-width ratio of the plot.
#' @return A single-line character description.
#' @export
export_hint <- function(kind, format, width_cm, quality, aspect = 0.62) {
  if (identical(format, "html")) {
    return("Interactive HTML file.")
  }
  if (identical(kind, "widget")) {
    px <- round(as.numeric(quality))
    return(sprintf("%s px wide.", prettyNum(px, big.mark = ",")))
  }
  # A vector file is the same drawing at any size, so what it is described by
  # is the size it is *drawn* at — the figure width a journal asks for — and
  # there is nothing about resolution to report: saying "600 dpi" of a PDF is
  # not a detail, it is wrong.
  if (format %in% vector_formats) {
    return(sprintf(
      "%.1f \u00d7 %.1f cm \u00b7 vector",
      width_cm,
      width_cm * aspect
    ))
  }
  # A raster file is a pixel count, and that is what a reader checks it
  # against — a submission limit, a slide, a screen. The centimetres are only
  # the width control restated, and the dpi says how the two relate.
  dpi <- resolved_dpi(width_cm, aspect, quality)
  sprintf(
    "%d \u00d7 %d px \u00b7 %d dpi",
    as.integer(round(width_cm / 2.54 * dpi)),
    as.integer(round(width_cm * aspect / 2.54 * dpi)),
    as.integer(dpi)
  )
}

# --- Server-side rendering ---------------------------------------------------

#' Largest DPI that keeps both sides of the image under `MAX_EXPORT_PX`.
#'
#' @param width_cm Numeric. Requested width in centimetres.
#' @param aspect Numeric. Height-to-width ratio.
#' @param dpi Numeric. Requested resolution.
#' @return Numeric DPI, reduced only when the request would not fit.
#' @export
resolved_dpi <- function(width_cm, aspect, dpi) {
  longest_in <- max(width_cm / CM_PER_IN, width_cm / CM_PER_IN * aspect)
  max(48, min(as.numeric(dpi), floor(MAX_EXPORT_PX / longest_in)))
}

# The device ggsave opens for a given format, already carrying the settings that
# separate a publication-quality file from a merely correct one: cairo
# rasterisation (properly antialiased text and hairlines), JPEG at a quality
# that does not ring around thin edges, LZW-compressed TIFF, and a cairo PDF so
# text is embedded as text rather than outlines.
#
# ggsave calls a function device as dev(filename=, width=, height=, bg=, ...)
# with the size in inches and no resolution, so `dpi` is closed over here.
.device_for <- function(format, dpi) {
  raster <- function(fun, ...) {
    args <- list(...)
    function(filename, width, height, bg = "white", ...) {
      do.call(
        fun,
        c(
          list(
            filename = filename,
            width = width,
            height = height,
            units = "in",
            res = dpi,
            bg = bg
          ),
          args
        )
      )
    }
  }
  switch(
    format,
    png = raster(png, type = "cairo-png"),
    jpeg = raster(jpeg, type = "cairo", quality = 100),
    tiff = raster(tiff, type = "cairo", compression = "lzw"),
    pdf = function(filename, width, height, bg = "white", ...) {
      cairo_pdf(filename, width = width, height = height, bg = bg)
    },
    svg = function(filename, width, height, bg = "white", ...) {
      svglite(filename, width = width, height = height, bg = bg)
    },
    stop("Unsupported export format: ", format)
  )
}

#' Write a ggplot to disk at a requested physical size and resolution.
#'
#' `bg` is left to ggsave, which derives it from the plot's own
#' `plot.background` — the engines set that from their background colour
#' picker, and the letterboxing around a fixed-aspect panel has to take the same
#' colour as the plot it surrounds.
#'
#' @param plot ggplot (or ggdraw) object.
#' @param file Character. Destination path.
#' @param format Character. One of `export_formats$ggplot`.
#' @param width_cm Numeric. Output width in centimetres.
#' @param aspect Numeric. Height-to-width ratio.
#' @param dpi Numeric. Requested resolution; ignored for vector formats.
#' @export
save_plot_export <- function(
  plot,
  file,
  format,
  width_cm = 25,
  aspect = 0.62,
  dpi = 300
) {
  width_in <- max(1, width_cm) / CM_PER_IN
  if (!format %in% vector_formats) {
    dpi <- resolved_dpi(width_cm, aspect, dpi)
  }
  ggsave(
    filename = file,
    plot = plot,
    device = .device_for(format, dpi),
    width = width_in,
    height = width_in * aspect,
    limitsize = FALSE
  )
  # png(res=) sizes the pixel grid correctly but writes no record of what that
  # resolution *was* — verified against a stock ggsave PNG with both
  # `identify -verbose` (reports "Units: Undefined") and Pillow
  # (`Image.info["dpi"]` absent). JPEG and TIFF do not have this gap: their own
  # devices already write it (checked against the JFIF APP0 segment and the
  # TIFF XResolution/YResolution tags respectively), so only PNG needs help.
  if (identical(format, "png")) {
    .write_png_dpi(file, dpi)
  }
}

# Insert a pHYs chunk recording pixels-per-metre, so the file itself carries
# the resolution a viewer, print shop or journal submission checker would
# otherwise have no way to read. Written by hand rather than pulled from a
# package: pHYs is nine bytes laid out by a decades-stable spec (two
# big-endian pixels-per-unit uint32s plus a unit byte, 1 = metre), and the only
# non-trivial part — the chunk's CRC32 — is exactly what `digest`'s "crc32"
# algorithm computes (verified against the RFC 1952 test vector, CRC-32("123456789")
# == 0xCBF43926, before relying on it here).
#
# Spliced in right after IHDR, which is always the PNG's first chunk. The PNG
# spec allows an ancillary chunk anywhere between IHDR and IDAT, so this does
# not need to know or care what else — gAMA, sRGB — the encoder already wrote.
.write_png_dpi <- function(file, dpi) {
  raw <- readBin(file, "raw", file.info(file)$size)
  ihdr_len <- .be_u32(raw[9:12])
  # 8 (signature) + 4 (length field) + 4 (type field) + IHDR's own data + 4 (CRC field).
  ihdr_end <- 8L + 12L + ihdr_len

  ppm <- as.integer(round(dpi / 0.0254)) # pixels per metre; 1 in = 0.0254 m
  type <- charToRaw("pHYs")
  data <- c(.u32_be(ppm), .u32_be(ppm), as.raw(1L))
  chunk <- c(.u32_be(length(data)), type, data, .crc32_be(c(type, data)))

  writeBin(
    c(raw[seq_len(ihdr_end)], chunk, raw[(ihdr_end + 1L):length(raw)]),
    file
  )
}

.be_u32 <- function(b4) {
  as.integer(sum(as.numeric(b4) * 256^(3:0)))
}

.u32_be <- function(x) {
  x <- as.integer(x)
  as.raw(c(
    bitwAnd(bitwShiftR(x, 24L), 255L),
    bitwAnd(bitwShiftR(x, 16L), 255L),
    bitwAnd(bitwShiftR(x, 8L), 255L),
    bitwAnd(x, 255L)
  ))
}

.crc32_be <- function(bytes) {
  hex <- digest(bytes, algo = "crc32", serialize = FALSE)
  as.raw(strtoi(
    substring(hex, c(1L, 3L, 5L, 7L), c(2L, 4L, 6L, 8L)),
    base = 16L
  ))
}

#' Decode a browser-captured data URI into an image file.
#'
#' @param uri Character. `data:image/...;base64,...` string from the client.
#' @param file Character. Destination path.
#' @return TRUE when bytes were written, FALSE for an empty or malformed URI.
#' @export
write_data_uri <- function(uri, file) {
  if (is.null(uri) || length(uri) != 1 || is.na(uri) || !nzchar(uri)) {
    return(FALSE)
  }
  payload <- sub("^data:[^;]*;base64,", "", uri)
  if (identical(payload, uri) && !grepl("^[A-Za-z0-9+/=\\s]+$", payload)) {
    return(FALSE)
  }
  writeBin(base64decode(payload), file)
  TRUE
}

#' Default file name for an exported plot.
#'
#' @param label Character. Plot-type label, e.g. "MST".
#' @param format Character. File extension.
#' @return Character file name, date-stamped.
#' @export
export_filename <- function(label, format) {
  slug <- gsub("[^A-Za-z0-9]+", "_", label)
  paste0(Sys.Date(), "_", slug, ".", format)
}
