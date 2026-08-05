# app/logic/db_store.R
#
# The one reactive that reads the loaded database's `metadata` table, shared
# by every module that displays it (Database Browser, Visualization, Analysis
# Dashboard, Export).
#
# Before this existed, each of those four modules called
# `read_metadata_table()` (nee `make_metadata_table()`) from its own reactive,
# independently guarded by its own `db_events::depend()` list. That worked in
# principle - all four watched the same revision bus - but it meant "does the
# whole app agree on what the metadata table looks like" depended on every
# one of those four call sites separately getting its dependency list right,
# forever. A store makes that structural instead of conventional: there is
# exactly one reactive that reads the table, so there is exactly one place
# that can get the dependency list wrong, and every consumer is, by
# construction, looking at the same cached value in the same reactive flush.
#
# Scope is deliberately narrow: only the `metadata` table is cached here. It
# is the one shared dataset with a write hidden behind it historically (see
# `read_metadata_table()`'s docs) and hence the one that actually went stale
# inconsistently across modules. Classical MLST, AMR and custom-variable data
# are also read by more than one module, but every one of those reads is a
# plain query with no write side effect and no shared risk of disagreeing -
# and the four modules compose them differently (Visualization wants the AMR
# drug-class summary, the Database Browser wants the per-gene call matrix), so
# unifying them here would trade simple, independent, already-correct code
# for a shared abstraction that does not actually remove a bug. Large,
# rarely-read data - allele profiles, sequences, allele FASTAs - is
# deliberately not cached anywhere: it is read only when a distance
# computation actually runs, and caching it would mean paying to hold it in
# memory for every session regardless of whether anyone ever presses Generate.

box::use(
  shiny[reactive, req],
)
box::use(
  app / logic / database_functions[read_metadata_table],
  app / logic / db_events,
)

#' Create the Shared Database Store
#'
#' One store per loaded database, created once in `app/main.R` and passed to
#' every module that displays metadata. Cheap to create; the actual query only
#' runs (once) the first time some module reads `metadata()`, same as any
#' other reactive.
#'
#' @param db_path Reactive character scalar - the loaded database's path.
#' @param db_rev A bus from `db_events::new_bus()`.
#' @return List of reactives. Currently just `metadata`.
#' @export
new_store <- function(db_path = reactive(NULL), db_rev = db_events$new_bus()) {
  # `isolates` and `metadata`: without both, this reactive would keep serving
  # the version it read when the database was first opened - db_path() does
  # not change on a write, and neither domain alone covers every write that
  # changes this table (a cell edit is `metadata` only; typing/import/removal
  # move the isolate pool, so they are `isolates` only until the sync each of
  # them runs also updates `metadata`).
  metadata <- reactive({
    db_events$depend(db_rev, "isolates", "metadata")
    req(db_path())
    read_metadata_table(db_path())
  })

  list(metadata = metadata)
}
