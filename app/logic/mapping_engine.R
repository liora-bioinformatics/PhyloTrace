# app/logic/mapping_engine.R
#
# Which aesthetic a variable should drive, and in which palette.
#
# The control panel used to ask this question the other way round: it listed the
# aesthetics — tip-label colour, tip-point colour, tip-point shape, tile strip —
# and asked the user to fill each one in. That put two decisions on them that
# have defensible answers, and punished the wrong one silently: a variable with
# 46 levels sent to a ColorBrewer palette drew nine countries in colour and the
# other 37 in grey, and a variable with more than six levels sent to shape had
# its surplus tips vanish from the plot entirely.
#
# Here the user picks the *variable* and this module picks the rest, from the
# variable's profile (app/logic/field_profile.R) and from what the other layers
# already occupy. Every rule below is a rule about legibility, and each one
# names the failure it exists to prevent.
#
# Pure: no shiny, no database, no ggplot.

box::use(
  app / logic / field_profile[MAX_QUAL_LEVELS, MAX_SHAPE_LEVELS],
)

#' Aesthetics a mapping layer can drive.
#' @export
AESTHETIC_POOL <- c(
  "tippoint_shape",
  "tippoint_color",
  "tiplab_color",
  "tile"
)

#' Display names for each aesthetic.
#' @export
AESTHETIC_LABELS <- c(
  tippoint_shape = "Tip point shape",
  tippoint_color = "Tip point colour",
  tiplab_color = "Tip label colour",
  tile = "Tile strip"
)

#' Aesthetics that carry a colour scale, and so must not repeat a palette.
#' @export
COLOR_AESTHETICS <- c("tippoint_color", "tiplab_color", "tile")

#' Most layers that can be drawn at once — one per aesthetic.
#' @export
MAX_LAYERS <- length(AESTHETIC_POOL)

#' Palettes per family, most-distinguishable first.
#'
#' Ordered so that consecutive entries are hue-disjoint rather than merely
#' different: Set1 is saturated primaries, Dark2 dark and muted, Set2 pastel;
#' viridis runs blue to yellow, plasma purple to orange, mako navy to green.
#' Two concurrent colour layers take consecutive entries, so they cannot come
#' out looking like two shades of the same idea.
#' @export
PALETTE_RINGS <- list(
  Qualitative = c("Set1", "Dark2", "Set2", "Paired", "Accent", "Set3",
                  "Pastel1"),
  Gradient = c("viridis", "plasma", "mako", "cividis", "turbo", "magma",
               "inferno"),
  Sequential = c("Blues", "Reds", "Greens", "Purples", "Oranges", "YlGnBu",
                 "Greys"),
  Diverging = c("Spectral", "RdBu", "PRGn", "PuOr", "RdYlGn", "PiYG", "BrBG")
)

#' Palette family suited to one variable.
#'
#' Continuous variables need a generated ramp. Discrete ones get a qualitative
#' palette only while they fit inside the smallest ColorBrewer set; past that
#' the tabulated palettes run out of colours and draw the remainder grey, so a
#' generated scale takes over.
#'
#' @param profile One-row profile frame from `field_profile$field_profiles()`.
#' @return Name of a `PALETTE_RINGS` entry.
#' @export
palette_family <- function(profile) {
  if (isTRUE(profile$continuous)) {
    "Gradient"
  } else if (profile$levels <= MAX_QUAL_LEVELS) {
    "Qualitative"
  } else {
    "Gradient"
  }
}

#' First palette of a family not already spoken for.
#'
#' @param family Name of a `PALETTE_RINGS` entry.
#' @param taken Character vector of palettes other colour layers hold.
#' @return A palette name.
#' @export
next_palette <- function(family, taken = character(0)) {
  ring <- PALETTE_RINGS[[family]] %||% PALETTE_RINGS[["Qualitative"]]
  free <- setdiff(ring, taken)
  if (length(free)) free[[1]] else ring[[1]]
}

#' Aesthetics this variable could drive, best first.
#'
#' The order is the recommendation, and it turns on the level count:
#'
#' * continuous — shape cannot express a continuum at all, so colour only.
#' * up to 6 levels — shape leads. It is the scarcest aesthetic (a hard ceiling
#'   of six), so a variable that can use it should, leaving the unbounded
#'   colour aesthetics free for variables that have no alternative.
#' * 7 to 9 levels — still inside a qualitative palette, and reads best as the
#'   colour of the label the user is already looking at.
#' * more than 9 — a tile strip. Forty-six differently coloured 2 mm characters
#'   is noise; the same forty-six as a band of shades beside the tree is a
#'   gradient a reader can actually follow.
#'
#' @param profile One-row profile frame.
#' @param taken Character vector of aesthetics already occupied.
#' @return Character vector of aesthetic names, best first, possibly empty.
#' @export
eligible_aesthetics <- function(profile, taken = character(0)) {
  if (is.null(profile) || !isTRUE(profile$groupable)) {
    return(character(0))
  }
  order <- if (isTRUE(profile$continuous)) {
    c("tippoint_color", "tiplab_color", "tile")
  } else if (profile$levels <= MAX_SHAPE_LEVELS) {
    c("tippoint_shape", "tippoint_color", "tiplab_color", "tile")
  } else if (profile$levels <= MAX_QUAL_LEVELS) {
    c("tiplab_color", "tippoint_color", "tile")
  } else {
    c("tile", "tippoint_color", "tiplab_color")
  }
  setdiff(order, taken)
}

#' Why an aesthetic is unavailable for this variable, for the edit dialog.
#'
#' Returns NULL when it is available. A dialog that simply omits the option
#' leaves the user to guess, which is the failure this whole rewrite is about.
#'
#' @param profile One-row profile frame.
#' @param aesthetic Name of an `AESTHETIC_POOL` entry.
#' @return A sentence, or NULL.
#' @export
aesthetic_block_reason <- function(profile, aesthetic) {
  if (is.null(profile)) {
    return("No such variable.")
  }
  if (!isTRUE(profile$groupable)) {
    return(sprintf(
      "%s has %d distinct value%s across %d isolates, so it cannot group them.",
      profile$label, profile$levels,
      if (profile$levels == 1L) "" else "s", profile$n
    ))
  }
  if (identical(aesthetic, "tippoint_shape")) {
    if (isTRUE(profile$continuous)) {
      return("A shape cannot show a continuous variable.")
    }
    if (profile$levels > MAX_SHAPE_LEVELS) {
      return(sprintf(
        "Shape carries at most %d values; %s has %d.",
        MAX_SHAPE_LEVELS, profile$label, profile$levels
      ))
    }
  }
  NULL
}

#' Build the layer a variable should become, given what is already mapped.
#'
#' @param profile One-row profile frame.
#' @param existing List of layer records already present.
#' @param id Stable identifier for the new layer.
#' @return A layer record, or NULL when no aesthetic is free.
#' @export
assign_mapping_layer <- function(profile, existing = list(), id = "L1") {
  taken <- vapply(existing, function(l) l$aesthetic, character(1))
  choice <- eligible_aesthetics(profile, taken)
  if (!length(choice)) {
    return(NULL)
  }
  .layer(profile, choice[[1]], existing, id)
}

# One layer record. `auto` records that the engine chose this layout rather
# than the user, which is what lets rebalance_layers() revisit it later.
.layer <- function(profile, aesthetic, others, id, auto = TRUE,
                   palette = NULL) {
  colour_layer <- aesthetic %in% COLOR_AESTHETICS
  family <- palette_family(profile)
  if (is.null(palette) && colour_layer) {
    taken <- vapply(
      Filter(function(l) l$aesthetic %in% COLOR_AESTHETICS, others),
      function(l) l$palette %||% "",
      character(1)
    )
    palette <- next_palette(family, taken)
  }
  list(
    id = id,
    field = profile$field,
    title = profile$label,
    aesthetic = aesthetic,
    palette = if (colour_layer) palette else NULL,
    family = if (colour_layer) family else NULL,
    n_levels = as.integer(profile$levels),
    continuous = isTRUE(profile$continuous),
    # A declared date arrives as character, so the renderer has to be told to
    # parse it before it reaches a continuous scale. The one place a declared
    # type changes what is drawn.
    transform = if (profile$type %in% c("date", "datetime")) "as_date",
    auto = auto
  )
}

#' Re-derive the automatic layers after the set has changed.
#'
#' Adding a four-level variable should be able to take shape away from a layer
#' that only had it because nothing better was competing; deleting a layer
#' should let the remaining ones spread back out. Both mean revisiting choices
#' this module made — and only those. A layer the user has edited carries
#' `auto = FALSE` and is returned byte-identical, because silently overriding a
#' deliberate choice is worse than a merely suboptimal layout.
#'
#' @param layers List of layer records, in draw order.
#' @param profiles Profile frame, for the current level counts.
#' @return The list, same length and order, automatic entries re-derived.
#' @export
rebalance_layers <- function(layers, profiles) {
  if (!length(layers)) {
    return(layers)
  }
  pinned <- Filter(function(l) !isTRUE(l$auto), layers)
  taken <- vapply(pinned, function(l) l$aesthetic, character(1))
  settled <- pinned

  out <- vector("list", length(layers))
  for (i in seq_along(layers)) {
    l <- layers[[i]]
    if (!isTRUE(l$auto)) {
      out[[i]] <- l
      next
    }
    prof <- .profile_row(profiles, l$field)
    if (is.null(prof)) {
      out[[i]] <- l
      next
    }
    choice <- eligible_aesthetics(prof, taken)
    # Nothing free: keep what it had rather than dropping the layer, so a
    # rebalance can never lose a mapping the user asked for.
    aesthetic <- if (length(choice)) choice[[1]] else l$aesthetic
    fresh <- .layer(prof, aesthetic, settled, l$id)
    out[[i]] <- fresh
    taken <- c(taken, aesthetic)
    settled <- c(settled, list(fresh))
  }
  out
}

.profile_row <- function(profiles, field) {
  if (is.null(profiles) || !nrow(profiles)) {
    return(NULL)
  }
  hit <- which(profiles$field == field)
  if (!length(hit)) {
    return(NULL)
  }
  profiles[hit[[1]], , drop = FALSE]
}

`%||%` <- function(x, y) if (is.null(x)) y else x
