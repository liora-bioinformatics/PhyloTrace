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
# strips; an MST draws onto node fill alone — a merged node holds several
# isolates, and the fill's pie is the one channel that can show all of their
# values rather than collapsing to one. The *rules* are the same across both
# media — a shape cannot show a continuum, a tabulated palette cannot show 46
# countries — so they are written once here and the medium only supplies its
# own list of channels (`MAPPING_MEDIA`).
#
# Pure: no shiny, no database, no ggplot.

box::use(
  app / logic / date_bins[
    binned_levels,
    DATE_TYPES,
    is_binned,
    mapped_granularity
  ],
  app / logic / field_profile[MAX_QUAL_LEVELS, MAX_SHAPE_LEVELS],
  rlang[`%||%`],
)

#' Most tips a tree can draw before the per-tip channels stop being readable.
#'
#' A tip point is a few pixels across and a tip label a couple of millimetres
#' tall, and both shrink as rows are added: past about sixty isolates a reader
#' can no longer tell one point colour from the next, let alone a triangle from
#' a diamond, and the mapping is decoration rather than information. A tile
#' strip does not shrink — it is a band beside the tips whose width is set by
#' the canvas, not by the tip count — so it is the one channel that still
#' carries a variable at that size.
#'
#' This demotes the per-tip channels, it does not remove them: the edit dialog
#' still offers all four, because a user who wants tip colour on three hundred
#' tips is making a deliberate choice. Only the automatic layout changes.
#' @export
TIP_MAPPING_MAX <- 60L

#' Whether a tree of this many tips is too dense for the per-tip channels.
#'
#' Deliberately not read off the profile's own `n`: that counts every isolate
#' in the database, because a variable has to describe itself the same way in
#' every tab, while this is about the isolates one plot actually draws.
#'
#' @param n_units Tips the tree will draw, or NULL when the caller cannot say.
#' @return TRUE past `TIP_MAPPING_MAX` tips; FALSE when the count is unknown.
#' @export
crowded_tips <- function(n_units) {
  !is.null(n_units) &&
    length(n_units) == 1L &&
    !is.na(n_units) &&
    n_units > TIP_MAPPING_MAX
}

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
#'
#' Both orderings also turn on how many isolates are drawn — see
#' `TIP_MAPPING_MAX`.
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
    # Ten strips. A strip is a narrow band whose width comes from the canvas
    # rather than from the tip count (see tree_plot$TILE_COL_IN), so ten of them
    # is a legible column of variables beside the tree rather than a crush —
    # and past sixty tips the strips are the *only* channel the engine will
    # reach for, so the cap is what a crowded tree can show at all.
    caps = c(tile = 10L),
    # Three exclusive aesthetics plus room for the strips. The ceiling is about
    # legibility rather than mechanism: every strip widens the canvas
    # (tree_plot$annotation_total), so the tree gets a smaller share of the page
    # with each one.
    max_layers = 13L,
    order = function(profile, n_units = NULL) {
      ranked <- if (isTRUE(profile$continuous)) {
        c("tippoint_color", "tiplab_color", "tile")
      } else if (profile$levels <= MAX_SHAPE_LEVELS) {
        c("tippoint_shape", "tippoint_color", "tiplab_color", "tile")
      } else if (profile$levels <= MAX_QUAL_LEVELS) {
        c("tiplab_color", "tippoint_color", "tile")
      } else {
        c("tile", "tippoint_color", "tiplab_color")
      }
      if (crowded_tips(n_units)) {
        c("tile", setdiff(ranked, "tile"))
      } else {
        ranked
      }
    }
  ),
  mst = list(
    # Fill only. Node shape, border colour and label colour were channels here
    # too, and all three were unreadable at the size an MST node is actually
    # drawn: a 1 px outline, a caption and a marker shape cannot carry a
    # 46-level variable, and — shape most of all — each collapses a merged
    # node's distribution to its majority value, which is exactly what the
    # pie exists to avoid. The border now belongs to the edge colour and the
    # label to the text colour, as plain styling; shape is simply gone.
    pool = c("node_fill"),
    labels = c(
      node_fill = "Node fill"
    ),
    color = "node_fill",
    # Nothing stacks on a network node: it has one fill. The MST's answer to
    # "more variables than channels" is the tooltip, which carries every
    # mapped field whether or not it has a channel.
    repeatable = character(0),
    caps = integer(0),
    max_layers = 1L,
    order = function(profile, n_units = NULL) "node_fill"
  ),
  amr = list(
    # One channel, and it stacks. An AMR heatmap's rows are the isolates, so a
    # mapped variable is drawn as a colour strip down the side of the matrix —
    # the same band the tree draws as a tile strip, and for the same reason: it
    # keeps its width whatever the row count, where a per-row mark would not.
    #
    # Several strips side by side is the normal way to read a handful of
    # variables against a matrix, so the channel is repeatable. Six of them,
    # because each one takes width from the heatmap body itself; past that the
    # matrix the strips exist to annotate is the smaller half of the picture.
    pool = c("annotation"),
    labels = c(annotation = "Annotation strip"),
    color = "annotation",
    repeatable = "annotation",
    caps = c(annotation = 6L),
    max_layers = 6L,
    order = function(profile, n_units = NULL) "annotation"
  ),
  epi = list(
    # One channel and it does not stack. An epi curve's bars are already a
    # stack — one segment per stratum — so the mapped variable *is* the stack,
    # and a second variable would have to be composited into the first
    # ("Urine | Germany"), multiplying the level count and the legend with it.
    # A curve split forty ways is not a curve any more.
    pool = c("bar_fill"),
    labels = c(bar_fill = "Bar fill"),
    color = "bar_fill",
    repeatable = character(0),
    caps = integer(0),
    max_layers = 1L,
    order = function(profile, n_units = NULL) "bar_fill"
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
#'
#' Read off every medium rather than listed by hand: a medium added without its
#' channel here draws layers with no palette at all, which is what the AMR
#' strips did.
#' @export
COLOR_AESTHETICS <- unique(unlist(
  lapply(MAPPING_MEDIA, `[[`, "color"),
  use.names = FALSE
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

#' Whether a variable is a date, and so can be grouped by a calendar interval.
#'
#' @param profile One-row profile frame.
#' @return TRUE for a declared date or datetime column.
#' @export
is_date_profile <- function(profile) {
  !is.null(profile) &&
    !is.null(profile$type) &&
    isTRUE(profile$type %in% DATE_TYPES)
}

#' Re-profile a date variable as the grouping a granularity turns it into.
#'
#' Binning changes what the variable *is*: six hundred distinct collection
#' dates become twelve months, and a continuum becomes a category. Every rule
#' downstream — which aesthetics are eligible, whether shape can carry it,
#' which palette family fits — reads those two properties off the profile, so
#' re-deriving them here is what makes a binned date behave like the discrete
#' variable it now is, in one place rather than at each decision.
#'
#' Returns the profile unchanged when nothing is binned.
#'
#' @param profile One-row profile frame.
#' @param values The variable's raw column values, for the binned level count.
#' @param granularity One of `date_bins$DATE_GRANULARITIES`, or NULL/"none".
#' @return A one-row profile frame.
#' @export
granularity_profile <- function(profile, values, granularity) {
  if (is.null(profile) || !is_binned(granularity) || !is_date_profile(profile)) {
    return(profile)
  }
  # Without the column to count, the variable is still no longer a continuum —
  # only its level count is unknown, so the profile's own count stands in.
  levels <- if (is.null(values)) {
    as.integer(profile$levels)
  } else {
    as.integer(binned_levels(values, granularity))
  }
  profile$levels <- levels
  profile$continuous <- FALSE
  profile$numeric <- FALSE
  profile$groupable <- levels >= 1L && levels < profile$n
  profile$shapeable <- profile$groupable && levels <= MAX_SHAPE_LEVELS
  profile
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
#' Past `TIP_MAPPING_MAX` tips the per-tip channels are demoted rather than
#' withdrawn: they stay in the returned vector, behind the tile strip, so the
#' edit dialog still offers them and only the automatic choice changes.
#'
#' @param profile One-row profile frame.
#' @param taken Character vector of aesthetics already occupied.
#' @param medium Name of a `MAPPING_MEDIA` entry.
#' @param n_units Units the medium will draw — tips for a tree.
#' @param off Aesthetics this plot is not drawing at all — tip-label colour on
#'   a tree whose labels are switched off. Withdrawn rather than demoted: a
#'   mapping put there would be drawn onto nothing.
#' @return Character vector of aesthetic names, best first, possibly empty.
#' @export
eligible_aesthetics <- function(
  profile,
  taken = character(0),
  medium = "tree",
  n_units = NULL,
  off = character(0)
) {
  if (is.null(profile) || !isTRUE(profile$groupable)) {
    return(character(0))
  }
  spec <- .medium(medium)
  order <- spec$order(profile, n_units)
  # A repeatable aesthetic is never used up by an existing layer; the exclusive
  # ones are. A capped one is used up once it is at its cap.
  spent <- setdiff(taken, spec$repeatable)
  for (aes in names(spec$caps)) {
    if (sum(taken == aes) >= spec$caps[[aes]]) {
      spent <- c(spent, aes)
    }
  }
  setdiff(order, c(spent, off %||% character(0)))
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
    # A raw collection date is the usual way to land here, and it has a fix the
    # user can act on rather than a dead end.
    if (is_date_profile(profile)) {
      return(sprintf(
        paste(
          "%s has %d distinct dates across %d isolates, so it cannot group",
          "them. Group it by week, month or year to use it here."
        ),
        profile$label, profile$levels, profile$n
      ))
    }
    return(sprintf(
      "%s has %d distinct value%s across %d isolates, so it cannot group them.",
      profile$label, profile$levels,
      if (profile$levels == 1L) "" else "s", profile$n
    ))
  }
  if (!is.null(aesthetic) && identical(aesthetic, .medium(medium)$shape)) {
    if (isTRUE(profile$continuous)) {
      if (is_date_profile(profile)) {
        return(paste(
          "A shape cannot show a continuous variable.",
          "Group this date by year to use it here."
        ))
      }
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
#' A date that groups nothing raw is binned rather than refused: picking
#' "Collection Date" should produce a mapping, not a dead end, and the finest
#' readable interval is a better opening move than making the user find the
#' setting first. A date that already groups on its own is left continuous.
#'
#' @param profile One-row profile frame.
#' @param existing List of layer records already present.
#' @param id Stable identifier for the new layer.
#' @param medium Name of a `MAPPING_MEDIA` entry.
#' @param values The variable's raw column values, for the date default.
#' @param n_units Units the medium will draw — tips for a tree.
#' @param off Aesthetics this plot is not drawing at all.
#' @param granularity Calendar interval to group a date by, instead of the one
#'   this function would choose. For rebuilding a saved layer: the grouping the
#'   plot was drawn with is a fact about that plot, not a choice to re-make, and
#'   a date the automatic rule cannot group at all (six dates over six isolates)
#'   is exactly the one a saved coarser grouping rescues.
#' @return A layer record, or NULL when no aesthetic is free.
#' @export
assign_mapping_layer <- function(
  profile,
  existing = list(),
  id = "L1",
  medium = "tree",
  values = NULL,
  n_units = NULL,
  off = character(0),
  granularity = NULL
) {
  # A date is grouped before anything else is decided about it. Not only when
  # it is unique per isolate, which is what this used to test: 213 distinct
  # collection dates across 253 isolates pass that test — they *do* group — and
  # still make an unreadable scale. `mapped_granularity()` owns the rule.
  if (is_date_profile(profile)) {
    if (!is_binned(granularity)) {
      granularity <- mapped_granularity(values)
    }
    profile <- granularity_profile(profile, values, granularity)
  } else {
    granularity <- NULL
  }
  taken <- vapply(existing, function(l) l$aesthetic, character(1))
  choice <- eligible_aesthetics(profile, taken, medium, n_units, off)
  if (!length(choice)) {
    return(NULL)
  }
  .layer(profile, choice[[1]], existing, id, granularity = granularity)
}

# One layer record. `auto` records that the engine chose this layout rather
# than the user, which is what lets rebalance_layers() revisit it later.
.layer <- function(profile, aesthetic, others, id, auto = TRUE,
                   palette = NULL, granularity = NULL) {
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
    transform = if (profile$type %in% DATE_TYPES) "as_date",
    # Calendar interval a date is grouped by, NULL for none. A raw collection
    # date is near-unique per isolate, so it only becomes a usable grouping
    # once the user coarsens it.
    granularity = if (is_binned(granularity)) as.character(granularity),
    auto = auto
  )
}

#' Set a date layer's granularity, re-deriving everything binning changes.
#'
#' Grouping by month does not just relabel the values: the variable stops being
#' a continuum and its level count collapses, which decides whether a
#' qualitative palette fits. The edit dialogs call this so those three stay
#' consistent with each other instead of drifting apart.
#'
#' A layer that is not a date is returned untouched.
#'
#' @param layer A layer record.
#' @param granularity One of `date_bins$DATE_GRANULARITIES`, or NULL/"none".
#' @param values The variable's raw column values.
#' @return The layer, updated.
#' @export
set_layer_granularity <- function(layer, granularity, values) {
  if (!identical(layer$transform, "as_date")) {
    return(layer)
  }
  binned <- is_binned(granularity)
  layer$granularity <- if (binned) as.character(granularity)
  layer$continuous <- !binned
  layer$n_levels <- as.integer(binned_levels(values, layer$granularity))
  if (!is.null(layer$palette)) {
    family <- palette_family(
      list(continuous = layer$continuous, levels = layer$n_levels)
    )
    # Only re-pick the palette when the family it belongs to has changed;
    # otherwise a deliberate choice would be reset by an unrelated edit.
    if (!identical(family, layer$family)) {
      layer$family <- family
      layer$palette <- next_palette(family)
    }
  }
  layer
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
#' @param values Metadata frame, needed only to count a binned date's groups.
#' @param n_units Units the medium will draw — tips for a tree.
#' @param off Aesthetics this plot is not drawing at all. A layer already
#'   sitting on one is moved, pinned or not: the alternative is a mapping the
#'   user asked for that the plot silently does not draw.
#' @return The list, same length and order, automatic entries re-derived.
#' @export
rebalance_layers <- function(layers, profiles, medium = "tree", values = NULL,
                             n_units = NULL, off = character(0)) {
  if (!length(layers)) {
    return(layers)
  }
  off <- off %||% character(0)
  # A pinned layer keeps its aesthetic only while the plot still draws it.
  stranded <- function(l) l$aesthetic %in% off
  pinned <- Filter(function(l) !isTRUE(l$auto) && !stranded(l), layers)
  taken <- vapply(pinned, function(l) l$aesthetic, character(1))
  settled <- pinned

  out <- vector("list", length(layers))
  for (i in seq_along(layers)) {
    l <- layers[[i]]
    if (!isTRUE(l$auto) && !stranded(l)) {
      out[[i]] <- l
      next
    }
    prof <- .profile_row(profiles, l$field)
    if (is.null(prof)) {
      out[[i]] <- l
      next
    }
    # A granularity is a user choice on a layer the engine still lays out
    # automatically, so it has to survive the rebuild — and it has to be
    # applied before the aesthetic is picked, since binning is what makes a
    # date discrete enough for one.
    prof <- granularity_profile(prof, values[[l$field]], l$granularity)
    choice <- eligible_aesthetics(prof, taken, medium, n_units, off)
    # Nothing free: keep what it had rather than dropping the layer, so a
    # rebalance can never lose a mapping the user asked for.
    aesthetic <- if (length(choice)) choice[[1]] else l$aesthetic
    # Rebuilt as automatic even if it was pinned. The channel the user chose is
    # not being drawn, so their choice cannot be honoured — handing the layer
    # back to the engine is what lets it be re-picked when that channel returns,
    # rather than frozen wherever it was moved to.
    fresh <- .layer(prof, aesthetic, settled, l$id, granularity = l$granularity)
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
