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
# Two media, one engine. A tree draws onto tip points, tip labels and tile
# strips; an MST draws onto node fill, node shape, node border and node label.
# The *rules* are the same in both — a shape cannot show a continuum, a
# tabulated palette cannot show 46 countries — so they are written once here and
# the medium only supplies its own list of channels (`MAPPING_MEDIA`).
#
# Pure: no shiny, no database, no ggplot.

box::use(
  app / logic / field_profile[MAX_QUAL_LEVELS, MAX_SHAPE_LEVELS],
)

#' The channels each drawing medium has, and how to rank them for a variable.
#'
#' `order` returns the aesthetics a variable of this profile should prefer, best
#' first, and is where the medium's own legibility argument lives.
#'
#' The MST's ranking differs from the tree's in one deliberate way: fill leads
#' for *every* variable, where the tree gives shape priority for low-cardinality
#' ones. An MST node is a set of isolates merged because their allele profiles
#' are identical, so a categorical variable over it is a distribution — the fill
#' is a pie and can show all of it, while a shape or a border has to collapse to
#' the majority value and quietly lose the rest. Scarcity is why the tree ranks
#' shape first; fidelity is why the MST does not.
#' @export
MAPPING_MEDIA <- list(
  tree = list(
    pool = c("tippoint_shape", "tippoint_color", "tiplab_color", "tile"),
    labels = c(
      tippoint_shape = "Tip point shape",
      tippoint_color = "Tip point colour",
      tiplab_color = "Tip label colour",
      tile = "Tile strip"
    ),
    color = c("tippoint_color", "tiplab_color", "tile"),
    shape = "tippoint_shape",
    # A tip has one label, one point colour and one shape, so those three are
    # exclusive. Tile strips are not: they stack outward beside the tips, and
    # several side by side is the normal way to read a handful of categorical
    # variables against a tree.
    repeatable = "tile",
    caps = c(tile = 4L),
    # Three exclusive aesthetics plus room for the strips. The ceiling is about
    # legibility rather than mechanism: every strip widens the canvas
    # (tree_plot$annotation_total), so the tree gets a smaller share of the page
    # with each one.
    max_layers = 7L,
    order = function(profile) {
      if (isTRUE(profile$continuous)) {
        c("tippoint_color", "tiplab_color", "tile")
      } else if (profile$levels <= MAX_SHAPE_LEVELS) {
        c("tippoint_shape", "tippoint_color", "tiplab_color", "tile")
      } else if (profile$levels <= MAX_QUAL_LEVELS) {
        c("tiplab_color", "tippoint_color", "tile")
      } else {
        c("tile", "tippoint_color", "tiplab_color")
      }
    }
  ),
  mst = list(
    pool = c("node_fill", "node_shape", "node_border", "label_color"),
    labels = c(
      node_fill = "Node fill",
      node_shape = "Node shape",
      node_border = "Node border",
      label_color = "Node label colour"
    ),
    color = c("node_fill", "node_border", "label_color"),
    shape = "node_shape",
    # Nothing stacks on a network node: it has one fill, one outline, one shape
    # and one label. The MST's answer to "more variables than channels" is the
    # tooltip, which carries every mapped field whether or not it has a channel.
    repeatable = character(0),
    caps = integer(0),
    max_layers = 4L,
    order = function(profile) {
      if (isTRUE(profile$continuous)) {
        c("node_fill", "node_border", "label_color")
      } else if (profile$levels <= MAX_SHAPE_LEVELS) {
        c("node_fill", "node_shape", "node_border", "label_color")
      } else {
        c("node_fill", "node_border", "label_color")
      }
    }
  )
)

# The medium's record, by name. An unknown name is a caller bug rather than a
# runtime condition, so it fails loudly instead of defaulting to the tree.
.medium <- function(medium) {
  spec <- MAPPING_MEDIA[[medium %||% "tree"]]
  if (is.null(spec)) {
    stop("Unknown mapping medium: ", medium)
  }
  spec
}

#' Display names for one medium's aesthetics.
#' @param medium Name of a `MAPPING_MEDIA` entry.
#' @return Named character vector.
#' @export
aesthetic_labels <- function(medium = "tree") .medium(medium)$labels

#' Most layers one medium can draw at once.
#' @param medium Name of a `MAPPING_MEDIA` entry.
#' @return Integer.
#' @export
max_layers <- function(medium = "tree") .medium(medium)$max_layers

# The tree's own channels, kept as exported constants because the tree module
# reads them directly — but taken from the medium table rather than written out
# again, so there is one definition of what a tree can draw onto.
#' Aesthetics a tree mapping layer can drive.
#' @export
AESTHETIC_POOL <- MAPPING_MEDIA$tree$pool

#' Display names for each tree aesthetic.
#' @export
AESTHETIC_LABELS <- MAPPING_MEDIA$tree$labels

#' Aesthetics that carry a colour scale, and so must not repeat a palette.
#' @export
COLOR_AESTHETICS <- unique(c(
  MAPPING_MEDIA$tree$color,
  MAPPING_MEDIA$mst$color
))

#' Aesthetics that can carry more than one variable at a time.
#' @export
REPEATABLE_AESTHETICS <- MAPPING_MEDIA$tree$repeatable

#' Most layers a tree can draw at once.
#' @export
MAX_LAYERS <- MAPPING_MEDIA$tree$max_layers

#' Most tile strips that can be drawn at once.
#' @export
MAX_TILES <- unname(MAPPING_MEDIA$tree$caps[["tile"]])

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
#' @param medium Name of a `MAPPING_MEDIA` entry.
#' @return Character vector of aesthetic names, best first, possibly empty.
#' @export
eligible_aesthetics <- function(
  profile,
  taken = character(0),
  medium = "tree"
) {
  if (is.null(profile) || !isTRUE(profile$groupable)) {
    return(character(0))
  }
  spec <- .medium(medium)
  order <- spec$order(profile)
  # A repeatable aesthetic is never used up by an existing layer; the exclusive
  # ones are. A capped one is used up once it is at its cap.
  spent <- setdiff(taken, spec$repeatable)
  for (aes in names(spec$caps)) {
    if (sum(taken == aes) >= spec$caps[[aes]]) {
      spent <- c(spent, aes)
    }
  }
  setdiff(order, spent)
}

#' Why an aesthetic is unavailable for this variable, for the edit dialog.
#'
#' Returns NULL when it is available. A dialog that simply omits the option
#' leaves the user to guess, which is the failure this whole rewrite is about.
#'
#' @param profile One-row profile frame.
#' @param aesthetic Name of an aesthetic in this medium's pool.
#' @param medium Name of a `MAPPING_MEDIA` entry.
#' @return A sentence, or NULL.
#' @export
aesthetic_block_reason <- function(profile, aesthetic, medium = "tree") {
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
  if (identical(aesthetic, .medium(medium)$shape)) {
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
#' @param medium Name of a `MAPPING_MEDIA` entry.
#' @return A layer record, or NULL when no aesthetic is free.
#' @export
assign_mapping_layer <- function(
  profile,
  existing = list(),
  id = "L1",
  medium = "tree"
) {
  taken <- vapply(existing, function(l) l$aesthetic, character(1))
  choice <- eligible_aesthetics(profile, taken, medium)
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
#' @param medium Name of a `MAPPING_MEDIA` entry.
#' @return The list, same length and order, automatic entries re-derived.
#' @export
rebalance_layers <- function(layers, profiles, medium = "tree") {
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
    choice <- eligible_aesthetics(prof, taken, medium)
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

#' Rebuild layer records from whatever a saved snapshot deserialised into.
#'
#' jsonlite reads a JSON array of same-shaped objects back as a *data.frame*, so
#' a saved analysis's layer list does NOT come back as the list-of-lists the
#' modules hold. Assigning that straight in corrupts it. This rebuilds the
#' canonical shape from whatever JSON handed over, filling anything absent from
#' the defaults so older or partial snapshots restore cleanly too.
#'
#' @param x A list, a data frame, or NULL.
#' @param defaults Named list of every field a record must end up with.
#' @return List of records, or NULL when `x` carried nothing.
#' @export
normalize_layer_records <- function(x, defaults) {
  if (is.null(x)) {
    return(NULL)
  }
  rows <- if (is.data.frame(x)) {
    lapply(seq_len(nrow(x)), function(i) as.list(x[i, , drop = FALSE]))
  } else if (is.list(x)) {
    x
  } else {
    return(NULL)
  }

  lapply(rows, function(row) {
    rec <- defaults
    if (is.list(row)) {
      for (f in names(row)) {
        v <- row[[f]]
        # A JSON null arrives as NULL or NA — keep the default for those.
        if (is.null(v) || (!is.list(v) && length(v) == 1L && is.na(v))) {
          next
        }
        rec[[f]] <- if (is.list(v) && length(v) == 1L) {
          unname(v[[1]])
        } else {
          unname(v)
        }
      }
    }
    rec
  })
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
