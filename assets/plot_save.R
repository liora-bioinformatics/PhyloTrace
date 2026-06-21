# Make plot name
make_filename <- function(filetype, scheme) {
  filename <- paste0(
    Sys.Date(),
    "_",
    gsub(" ", "_", scheme),
    "_Tree.",
    filetype
  )

  log_print(paste0(
    "Saved tree plot: ",
    filename
  ))

  return(filename)
}

# Helper function to get and validate plot width from session client data
get_plot_width <- function(session, unit = "pixel", dpi = 192) {
  width <- session$clientData$output_tree_plot_width
  if (is.null(width) || !is.numeric(width) || width <= 0) {
    width <- 800 # Fallback
  } else {
    width <- as.integer(width)
  }

  # Convert pixels to inches
  if (unit == "pixel") {
    return(width)
  } else if (unit == "inches") {
    return(width / dpi)
  } else {
    return(NULL)
  }
}

# Helper function to get and validate plot height based on width and aspect ratio
get_plot_height <- function(
  width,
  aspect_ratio_val,
  unit = "pixel",
  dpi = 192
) {
  height <- as.integer(width * aspect_ratio_val)
  if (!is.numeric(height) || height <= 0) {
    height <- as.integer(800 * 0.6) # Fallback (equivalent to 800px width * 0.6 ratio)
  }

  # Convert pixels to inches
  if (unit == "pixel") {
    return(height)
  } else if (unit == "inches") {
    return(height / dpi)
  } else {
    return(NULL)
  }
}

# Function to handle plot saving logic for downloadHandler
save_plot_content <- function(
  file,
  session,
  filetype,
  aspect_ratio,
  plot,
  dpi = 192
) {
  # Validate reactive dependencies
  req(
    session$clientData$output_tree_plot_width,
    aspect_ratio
  )

  # Get pixel-based dimensions for raster formats
  pixel_width <- get_plot_width(session)
  pixel_height <- get_plot_height(
    width = pixel_width,
    aspect_ratio_val = aspect_ratio
  )

  # Handle different file types
  if (filetype == "png") {
    png(
      file = file,
      width = pixel_width,
      height = pixel_height,
      res = dpi
    )
    print(plot)
    dev.off()
  } else if (filetype == "jpeg") {
    jpeg(
      file = file,
      quality = 100,
      width = pixel_width,
      height = pixel_height,
      res = dpi
    )
    print(plot)
    dev.off()
  } else if (filetype == "svg") {
    # Convert pixels to inches for SVG
    inch_width <- get_plot_width(session, unit = "inches")
    inch_height <- get_plot_height(
      width = inch_width,
      aspect_ratio_val = aspect_ratio
    )
    plot <- plot
    ggsave(
      file = file,
      plot = plot,
      device = "svg",
      width = inch_width,
      height = inch_height,
      dpi = dpi,
      units = "in"
    )
  } else if (filetype == "bmp") {
    bmp(
      file,
      width = pixel_width,
      height = pixel_height,
      res = dpi
    )
    print(plot)
    dev.off()
  }
}
