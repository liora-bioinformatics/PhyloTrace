# app/logic/viz_legend.R
#
# The legend planner shared by the Tree's guide box and the AMR heatmap's
# legend column: how many keys each guide may list in the height it has, which
# keys those are, how a trimmed guide says so, and how many columns a long one
# folds into. Drawing stays with each engine — ggplot2 guides for the Tree,
# ComplexHeatmap legends for the AMR plot — and so does the height of one key
# row, which the two draw differently.

box::use(
  rlang[`%||%`],
  stats[setNames],
  utils[head, tail],
)
box::use(
  app / logic / viz_fit[clamp, largest_fitting],
)

#' Keys a guide lists when nothing has told it how much room it has.
#'
#' The answer to "how long a list is worth drawing" is mostly the box's height
#' (`legend_key_budget()`). This is what a scale built outside that solve falls
#' back to — a handful of swatches, which is what a key list is read for.
#' @export
LEGEND_MAX_KEYS <- 9L

#' Fewest keys a guide is cut back to before it stops being worth drawing.
#'
#' The floor the budget starts every guide at: a guide cut below four keys is
#' not worth the rows it stands in. Where even the floor will not fit, the
#' engine shrinks the type instead.
#' @export
LEGEND_MIN_KEYS <- 4L

#' Keys one column of a guide holds before its keys fold into another.
#' @export
LEGEND_MAX_ROWS <- 18L

#' Keys any one guide lists, however much room the box has.
#'
#' One column's worth, and deliberately the same number: a guide allowed more
#' keys than a column holds buys them by folding into a second column, and a
#' second column is width taken off the drawing. So the height decides
#' everything under this, and past it a scale is a population rather than a
#' vocabulary — the colours still say where the same value recurs, which is the
#' job they go on doing when the guide only samples them.
#' @export
LEGEND_FULL_MAX <- LEGEND_MAX_ROWS

#' Columns one guide's own keys may fold into.
#'
#' One fold, not three: past a fold the answer is fewer keys, not a guide box
#' wider than the drawing beside it.
#' @export
LEGEND_KEY_COLS <- 2L

#' The blank key that stands where a run of levels was left out.
#'
#' A trimmed guide reads as a complete list unless it says otherwise. The title
#' says how many levels there are ("9 of 81 shown"), but not *where* the gap
#' falls — and for an ordered scale, whose keys come from both ends, that is the
#' one thing the reader has to know. Drawn as a swatch with no colour in it.
#'
#' Three full stops, not the typographic ellipsis: U+22EF is missing from
#' enough of the export fonts that R substitutes a dot per byte and warns.
#' @export
LEGEND_GAP_KEY <- "..."

#' The colour a gap key's swatch is filled with, which is none.
#' @export
LEGEND_GAP_COLOR <- "transparent"

#' Keys one guide may list, given the rows it has been budgeted.
#'
#' @param max_rows Integer. Rows per guide, from `legend_max_rows()`.
#' @return Integer key budget.
#' @export
legend_max_keys <- function(max_rows = LEGEND_MAX_ROWS) {
  rows <- suppressWarnings(as.integer(max_rows))
  if (length(rows) != 1L || is.na(rows)) {
    rows <- LEGEND_MAX_ROWS
  }
  as.integer(clamp(rows, LEGEND_MIN_KEYS, LEGEND_MAX_KEYS))
}

#' Rows guides stand in.
#'
#' Listing `keys` of `demand` levels in `ncol` columns: a title, the keys, and
#' the blank line before the next guide — plus, where a guide is not listing
#' everything, the title's second line ("9 of 81 shown") and the gap key.
#'
#' @param keys,demand,ncol Integer vectors, one entry per guide.
#' @return Integer rows per guide.
#' @export
legend_guide_rows <- function(keys, demand, ncol = 1L) {
  as.integer(ceiling(keys / pmax(ncol, 1L))) + 2L + 2L * (keys < demand)
}

#' Keys each guide may list, sharing the rows the box has between them.
#'
#' An equal share is the wrong answer twice over: it costs a guide that wants
#' four keys the same as one that wants eighty, and a guide *one key short* of
#' complete pays two extra rows for saying so, which an equal share never
#' notices it could recover. So the short lists are completed first, shortest
#' first, and whatever is left is handed round the guides still trimmed, one
#' key at a time, so they grow together.
#'
#' @param demands Integer vector. Levels each guide holds, in stacking order.
#' @param room Integer. Rows the whole box has.
#' @param full_max Integer. Most keys any one guide lists.
#' @return Integer vector of key budgets, one per guide.
#' @export
legend_key_budget <- function(
  demands,
  room = LEGEND_MAX_ROWS,
  full_max = LEGEND_FULL_MAX
) {
  d <- suppressWarnings(as.integer(demands))
  d <- d[!is.na(d)]
  n <- length(d)
  if (!n) {
    return(integer(0))
  }
  d <- pmax(d, 1L)
  cap <- pmin(d, full_max)
  give <- pmin(cap, LEGEND_MIN_KEYS)
  cost <- function(g) sum(legend_guide_rows(g, d))
  # The floor is not negotiable: a box that cannot hold it has its type shrunk.
  budget <- max(suppressWarnings(as.integer(room %||% LEGEND_MAX_ROWS)), cost(give))
  for (i in order(d)) {
    trial <- give
    trial[[i]] <- cap[[i]]
    if (cost(trial) <= budget) {
      give <- trial
    }
  }
  repeat {
    moved <- FALSE
    for (i in which(give < cap)) {
      trial <- give
      trial[[i]] <- trial[[i]] + 1L
      if (cost(trial) <= budget) {
        give <- trial
        moved <- TRUE
      }
    }
    if (!moved) {
      break
    }
  }
  as.integer(give)
}

#' Rows one guide may run to before its keys fold into another column.
#'
#' The box's height shared between the guides it shows, less the title and the
#' blank line each of them takes.
#'
#' @param n_guides Integer. Guides in the box.
#' @param room Integer. Rows the box has.
#' @param cap Integer. Most rows one column may run to however much room
#'   there is: the Tree folds past `LEGEND_MAX_ROWS` to keep its guide box
#'   narrow; an engine whose guides stand in a tall column can let a long
#'   vocabulary run down it instead.
#' @return Integer rows per guide, at least 3.
#' @export
legend_max_rows <- function(n_guides, room, cap = LEGEND_MAX_ROWS) {
  if (n_guides < 1L) {
    return(as.integer(cap))
  }
  as.integer(clamp(floor(room / n_guides) - 2L, 3L, cap))
}

#' Columns one guide's keys fold into.
#'
#' @param n_levels Integer. Keys the guide lists.
#' @param max_rows Integer. Rows one column may run to.
#' @return Integer, 1 to `LEGEND_KEY_COLS`.
#' @export
legend_ncol <- function(n_levels, max_rows = LEGEND_MAX_ROWS) {
  max_rows <- max(as.integer(max_rows), 1L)
  if (n_levels <= max_rows) {
    return(1L)
  }
  as.integer(min(LEGEND_KEY_COLS, ceiling(n_levels / max_rows)))
}

#' The guide box's plan: how many keys each guide lists, how many columns it
#' folds into, and the rows the whole box then stands in.
#'
#' One solve for the whole box, because what a guide may list depends on what
#' the others need. The rows are costed at one column per guide while a long
#' guide is drawn folded: conservative, since a budget that could buy keys by
#' folding would spend the drawing's width on them.
#'
#' @param demands Named integer vector. Levels each guide holds, keyed by guide
#'   id, in stacking order.
#' @param room Integer. Rows the box has.
#' @param full_max Integer. Most keys any one guide lists: one number, or one
#'   per guide.
#' @param max_rows_cap Integer. Most rows one column may run to before its
#'   keys fold (see `legend_max_rows()`).
#' @return list(ids, demand, cap, keys, ncol, order, max_rows, room, rows), the
#'   per-guide entries named by id.
#' @export
legend_plan <- function(
  demands,
  room,
  full_max = LEGEND_FULL_MAX,
  max_rows_cap = LEGEND_MAX_ROWS
) {
  ids <- names(demands) %||% character(0)
  d <- pmax(suppressWarnings(as.integer(demands)), 1L)
  room <- max(suppressWarnings(as.integer(room)), 1L)
  keys <- legend_key_budget(d, room, full_max)
  max_rows <- legend_max_rows(length(d), room, max_rows_cap)
  ncol <- vapply(keys, legend_ncol, integer(1), max_rows = max_rows)
  list(
    ids = ids,
    demand = setNames(d, ids),
    cap = setNames(as.integer(pmin(d, full_max)), ids),
    keys = setNames(keys, ids),
    ncol = setNames(as.integer(ncol), ids),
    order = setNames(seq_along(ids), ids),
    max_rows = max_rows,
    room = room,
    rows = as.integer(sum(legend_guide_rows(keys, d, ncol)))
  )
}

# Whether a set of levels has ends worth showing: numbers, and the four shapes
# a binned date takes ("2024", "2024-03", "2024-W12", "2024-03-05"). For
# anything else "first" and "last" are accidents of the alphabet.
.levels_are_ordered <- function(x) {
  if (length(x) < 2L) {
    return(FALSE)
  }
  if (!anyNA(suppressWarnings(as.numeric(x)))) {
    return(TRUE)
  }
  all(grepl("^\\d{4}(-(W\\d{2}|\\d{2}(-\\d{2})?))?$", x))
}

# How often each level occurs in the values the scale was built from. Absent
# values leave every level equal, which falls back to the scale's own order.
.level_counts <- function(levels, values) {
  if (is.null(values)) {
    return(rep(1L, length(levels)))
  }
  tab <- table(as.character(values))
  counts <- as.integer(tab[levels])
  counts[is.na(counts)] <- 0L
  counts
}

#' The keys one guide should list, and what to say about the rest.
#'
#' - An **ordered** scale (numbers, a binned date) is read for its range, so the
#'   budget is split between its two ends.
#' - A **nominal** scale has no ends: its keys go to the levels the reader will
#'   actually meet — the most frequent — restored to the scale's own order.
#'
#' A "missing" level keeps its key wherever it appears: its colour cannot be
#' guessed from the others, and an unexplained grey swatch is worse than one
#' fewer real category.
#'
#' @param levels Character vector of the scale's levels, in draw order.
#' @param values The mapped values, for the frequency order. Optional.
#' @param max_keys Integer. Keys this guide has room for.
#' @param missing Character. Levels that always keep their key.
#' @return list(breaks = <character>, hidden = <integer>, total = <integer>).
#' @export
legend_breaks <- function(
  levels,
  values = NULL,
  max_keys = LEGEND_MAX_KEYS,
  missing = character(0)
) {
  levels <- as.character(levels)
  n <- length(levels)
  k <- max(suppressWarnings(as.integer(max_keys)), 2L)
  if (is.na(k) || n <= k) {
    return(list(breaks = levels, hidden = 0L, total = n))
  }
  kept_missing <- intersect(missing, levels)
  real <- setdiff(levels, kept_missing)
  budget <- max(k - length(kept_missing), 1L)
  keep <- if (.levels_are_ordered(real)) {
    head_n <- ceiling(budget / 2)
    c(head(real, head_n), tail(real, budget - head_n))
  } else {
    ranked <- order(-.level_counts(real, values), seq_along(real))
    real[sort(head(ranked, budget))]
  }
  list(
    breaks = c(.with_gap_key(real, keep), kept_missing),
    hidden = n - length(keep) - length(kept_missing),
    total = n
  )
}

# The kept keys with one blank key marking where the list was cut: at the
# first place it stops being contiguous, counting the two ends. One marker and
# not several, or a guide scattered across a long scale spends half its rows
# on punctuation.
.with_gap_key <- function(all, keep) {
  at <- match(keep, all)
  at <- at[!is.na(at)]
  if (!length(at) || length(at) == length(all)) {
    return(keep)
  }
  if (at[[1L]] > 1L) {
    return(c(LEGEND_GAP_KEY, keep))
  }
  gap <- which(diff(at) > 1L)
  if (length(gap)) {
    i <- gap[[1L]]
    return(c(head(keep, i), LEGEND_GAP_KEY, tail(keep, length(keep) - i)))
  }
  if (at[[length(at)]] < length(all)) c(keep, LEGEND_GAP_KEY) else keep
}

#' A palette with a colourless swatch added for the gap key.
#'
#' @param cols Named character vector of colours.
#' @param breaks Character vector from `legend_breaks()`.
#' @param blank The gap key's fill.
#' @return `cols`, with the gap key's entry where the breaks carry one.
#' @export
legend_values <- function(cols, breaks, blank = LEGEND_GAP_COLOR) {
  if (!LEGEND_GAP_KEY %in% breaks) {
    return(cols)
  }
  c(cols, setNames(blank, LEGEND_GAP_KEY))
}

#' The limits a discrete ggplot2 scale needs to admit the gap key.
#'
#' A break outside a scale's limits is dropped without comment, and the gap
#' key is no level of anything.
#'
#' @param levels Character vector of the scale's levels.
#' @param breaks Character vector from `legend_breaks()`.
#' @return Character limits, or NULL when there is no gap key.
#' @export
legend_limits <- function(levels, breaks) {
  if (!LEGEND_GAP_KEY %in% breaks) {
    return(NULL)
  }
  c(as.character(levels), LEGEND_GAP_KEY)
}

#' A guide title that says how many values it is not showing.
#'
#' "9 of 81 shown" rather than "+ 72 more": with the keys of an ordered scale
#' taken from both ends, the reader has to know the list is a *sample*.
#'
#' @param name Character. The variable's title.
#' @param hidden Integer. Levels the guide is not listing.
#' @param total Integer. Levels the scale holds. Optional.
#' @return Character.
#' @export
legend_title <- function(name, hidden, total = NULL) {
  if (!isTRUE(hidden > 0)) {
    return(name)
  }
  if (is.null(total) || !isTRUE(is.finite(total) && total > hidden)) {
    return(paste0(name %||% "", "\n+ ", hidden, " more"))
  }
  paste0(name %||% "", "\n", total - hidden, " of ", total, " shown")
}

#' The type size a legend column is set at, and its plan at that size.
#'
#' Shrink before trimming. A key is a label like any other: it is dropped only
#' where it cannot be set legibly even at the floor. So the column takes the
#' largest size, up to `want`, at which every guide lists all of its keys up to
#' its own cap; and only where not even the floor holds them all does the key
#' budget trim them, with the type left at the floor so as few as possible go.
#'
#' @param want Numeric. Type size the design asks for.
#' @param floor Numeric. Smallest legible legend type.
#' @param plan_at Function of one size, returning a `legend_plan()` for the
#'   rows the column has at that size.
#' @return list(size, plan, complete), `complete` saying whether every guide
#'   lists everything up to its cap.
#' @export
legend_fit <- function(want, floor, plan_at) {
  complete <- function(pt) {
    plan <- plan_at(pt)
    all(plan$keys >= plan$cap) && plan$rows <= plan$room
  }
  size <- if (isTRUE(complete(floor))) {
    largest_fitting(want, complete, floor = floor)
  } else {
    floor
  }
  list(size = size, plan = plan_at(size), complete = isTRUE(complete(size)))
}
