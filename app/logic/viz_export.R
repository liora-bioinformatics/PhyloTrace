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
  ggplot2[ggsave],
  grDevices[cairo_pdf, jpeg, png, tiff],
  shiny[
    actionButton,
    div,
    downloadButton,
    icon,
    numericInput,
    sliderInput,
    tagList,
    uiOutput,
  ],
  shinyWidgets[pickerInput],
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

CM_PER_IN <- 2.54

# --- UI ----------------------------------------------------------------------

#' Shared Visualization Export Panel UI
#'
#' The sidebar's "Export Plot" controls: file format, physical size, and the
#' quality control appropriate to the engine kind (DPI for a server-rendered
#' plot, a linear scale factor for a browser-rendered widget).
#'
#' Returns a plain container rather than a `nav_panel`, since it is mounted
#' inside the setup sidebar's accordion.
#'
#' The download itself goes through a hidden `downloadButton`: the visible
#' control is an `actionButton` so the server can refuse (and say why) before a
#' file is offered, and so the widget engines can run their asynchronous capture
#' between the click and the download.
#'
#' @param ns Function. Module namespace function (`session$ns`).
#' @param prefix Character. ID prefix unique to the calling module.
#' @param kind Character. "ggplot" or "widget"; see the file header.
#' @return A `div` of export controls.
#' @export
export_panel <- function(ns, prefix, kind = "ggplot") {
  id <- function(suffix) ns(paste0(prefix, "_", suffix))
  widget <- identical(kind, "widget")

  div(
    class = "viz-export",
    pickerInput(
      id("filetype"),
      "File format",
      choices = as.list(export_formats[[kind]]),
      selected = "png"
    ),
    # The two kinds measure size and quality differently, and neither reading
    # transfers to the other. A server-rendered plot is laid out in inches and
    # rasterised at a chosen DPI, so it is asked for in physical units. A widget
    # is laid out in CSS pixels by the browser and can only be redrawn at a
    # multiple of them — there is no physical size to ask for, so offering one
    # would be offering a control that changes nothing.
    if (widget) {
      sliderInput(
        id("scale"),
        "Render scale",
        min = 1,
        max = 6,
        value = 3,
        step = 1,
        ticks = FALSE
      )
    } else {
      tagList(
        numericInput(
          id("width"),
          "Width (cm)",
          value = 25,
          min = 4,
          max = 60,
          step = 1
        ),
        pickerInput(
          id("dpi"),
          "Resolution",
          choices = list(
            "150 dpi (screen)" = "150",
            "300 dpi (print)" = "300",
            "600 dpi (high)" = "600",
            "1200 dpi (line art)" = "1200"
          ),
          selected = "300"
        )
      )
    },
    uiOutput(id("hint"), class = "viz-export-hint small text-muted"),
    actionButton(
      id("download"),
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

#' Describe what the current export settings will actually produce.
#'
#' Stated in the units the user cares about — final pixel dimensions for a
#' raster, "scales with the page" for vector art — because neither DPI nor a
#' scale factor means anything on its own.
#'
#' @param kind Character. "ggplot" or "widget".
#' @param format Character. Selected file format.
#' @param width_cm Numeric. Requested width in centimetres (ggplot only).
#' @param quality Numeric. DPI (ggplot) or scale factor (widget).
#' @param aspect Numeric. Height-to-width ratio of the plot.
#' @return A single-line character description.
#' @export
export_hint <- function(kind, format, width_cm, quality, aspect = 0.62) {
  if (identical(format, "html")) {
    return("Self-contained interactive file; opens in any browser.")
  }
  if (format %in% vector_formats) {
    return("Vector output — resolution-independent at any print size.")
  }
  if (identical(kind, "widget")) {
    n <- as.integer(quality)
    return(sprintf(
      "Redrawn at %d× the on-screen size%s",
      n,
      if (n >= 3) ", enough for print." else " — raise for print."
    ))
  }
  dpi <- resolved_dpi(width_cm, aspect, quality)
  w <- round(width_cm / CM_PER_IN * dpi)
  sprintf("%d × %d px at %d dpi.", w, round(w * aspect), as.integer(dpi))
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
