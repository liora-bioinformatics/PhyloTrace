# app/logic/dist_cache.R
#
# Session-scoped cache of allelic distance matrices, shared by every Tree and MST
# tab, so a repeated Generate, an algorithm switch or a second tab over the same
# isolates reuses the matrix instead of reloading profiles and recomputing it.

box::use(
  rlang[`%||%`, hash],
  shiny[isolate],
)

box::use(
  app / logic / db_events,
  app / logic / logging[log_event],
  app / logic / phylo[compute_distance],
)

# Policies under which a pair's distance depends on that pair's profiles alone,
# so a matrix computed for one isolate set is exact for every subset of it.
# `omit` is not one: the loci it drops are decided by the whole set, so adding or
# removing a single isolate can change every distance.
PAIRWISE_POLICIES <- c("ignore_na", "category")

#' Create a Distance Matrix Cache
#'
#' One cache per session, created by app/view/visualization.R and handed to every
#' distance engine.
#'
#' Validity follows the revision bus, the same contract as the shared metadata
#' store (app/logic/db_store.R): entries belong to one database path and one
#' value of its `isolates`, `schema` and `staged` counters, and are dropped at
#' the first lookup after any of those moves. Every write that can change an
#' allele profile bumps one of them - typing, isolate removal, merge, restore,
#' staging. The flip side is inherited too: a write nobody has announced yet
#' (pyMLST mid-run, which typing only announces at finalize) is not seen, which
#' is exactly what keeps a plot generated during a run from picking up
#' half-written isolates.
#'
#' Under a pairwise policy one matrix per staged-set combination grows to the
#' union of the isolates requested and every later subset is sliced from it.
#' `omit` is keyed on the exact selection. A NULL selection ("whatever `mlst`
#' holds right now") is never cached - it is resolved live by design, see
#' `phylo::load_allele_profile()`.
#'
#' @param db_rev A bus from `db_events::new_bus()`.
#' @param max_entries Most matrices held for the current revision.
#' @param compute Function with the signature of `phylo::compute_distance()`.
#' @return A list of `get(db_path, na_handling, isolates, imported_sets)`, which
#'   returns the labelled distance matrix (or NULL when no profile matches), and
#'   `clear()`.
#' @export
new_dist_cache <- function(
  db_rev = db_events$new_bus(),
  max_entries = 3L,
  compute = compute_distance
) {
  entries <- list()

  # Look up, or compute and store, the distance matrix for one request.
  get <- function(db_path, na_handling, isolates, imported_sets = NULL) {
    na_handling <- na_handling %||% "ignore_na"
    if (is.null(isolates)) {
      return(compute(db_path, na_handling, isolates, imported_sets))
    }

    scope <- list(
      db_path,
      isolate(db_events$revision(db_rev, "isolates", "schema", "staged"))
    )
    # Revision counters only move forward, so an entry from another scope can
    # never be hit again.
    entries <<- Filter(function(e) identical(e$scope, scope), entries)

    pairwise <- na_handling %in% PAIRWISE_POLICIES
    imported_sets <- sort(unique(as.integer(imported_sets)))
    key <- hash(list(
      na_handling,
      imported_sets,
      if (!pairwise) sort(unique(isolates))
    ))

    entry <- entries[[key]]
    if (!is.null(entry) && all(isolates %in% entry$asked)) {
      entries[[key]] <<- NULL
      entries[[key]] <<- entry
      out <- .slice(entry$dist, isolates)
      log_event(
        "PHYLO",
        "Distance matrix reused",
        sprintf("%d isolates | %s", nrow(out), na_handling)
      )
      return(out)
    }

    asked <- if (pairwise) union(entry$asked, isolates) else isolates
    started <- Sys.time()
    dist <- compute(db_path, na_handling, asked, imported_sets)
    if (is.null(dist)) {
      return(NULL)
    }
    log_event(
      "PHYLO",
      "Distance matrix computed",
      sprintf(
        "%d isolates | %s | %.2fs",
        nrow(dist),
        na_handling,
        as.numeric(difftime(Sys.time(), started, units = "secs"))
      )
    )

    entries[[key]] <<- NULL
    entries[[key]] <<- list(scope = scope, asked = asked, dist = dist)
    if (length(entries) > max_entries) {
      entries <<- utils::tail(entries, max_entries)
    }
    .slice(dist, isolates)
  }

  # Drop every held matrix, e.g. when the database is closed.
  clear <- function() {
    entries <<- list()
    invisible(NULL)
  }

  list(get = get, clear = clear)
}

# Helper: Restrict a labelled distance matrix to `isolates`, keeping the matrix's
# own row order - the order a fresh computation over that subset would produce,
# so a cached and an uncached tree are built from identical input.
.slice <- function(dist, isolates) {
  keep <- rownames(dist) %in% isolates
  if (!any(keep)) {
    return(NULL)
  }
  if (all(keep)) {
    return(dist)
  }
  dist[keep, keep, drop = FALSE]
}
