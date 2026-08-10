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
# Scope is deliberately narrow: the `metadata` table and the isolate *name*
# pool. Those are the two shared answers that modules were resolving
# independently and could therefore disagree about. Classical MLST, AMR and
# custom-variable data are also read by more than one module, but every one of
# those reads is a plain query with no write side effect and no shared risk of
# disagreeing - and the four modules compose them differently (Visualization
# wants the AMR drug-class summary, the Database Browser wants the per-gene
# call matrix), so unifying them here would trade simple, independent,
# already-correct code for a shared abstraction that does not remove a bug.
#
# Allele *profiles* stay uncached on purpose: they are large, and they are read
# only when a distance computation actually runs, so caching them would cost
# memory in every session regardless of whether anyone presses Generate. That
# is fine - what is not fine is letting a `NULL` ("all isolates") selection
# reach the live profile query and be resolved there, because then the UI and
# the computation answer "which isolates?" from different tables at different
# moments. See `isolates()` below.

box::use(
  shiny[reactive, req],
)
box::use(
  app / logic / database_functions[read_metadata_table],
  app / logic / db_events,
  app / logic / pymlst[existing_strains],
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
#' @return List of reactives: `metadata` (the whole table) and `isolates` (the
#'   isolate name pool).
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

  # The isolate pool: which isolates exist at all, read from `mlst` (the table
  # typing actually writes) rather than from `metadata` (which only catches up
  # at the next sync).
  #
  # Cached here because "which isolates exist?" is the question the app was
  # answering inconsistently. A `NULL` selection means "all of them" throughout
  # the UI, and every place that has to turn that back into a concrete list
  # must get the same list. Resolving it separately - as
  # `load_allele_profile()` does, with its own live `mlst` query - is what let
  # a tree computed during a typing run pick up half-written isolates that the
  # sidebar, the selection modal and the tip labels all knew nothing about.
  #
  # Names only, never profiles: a few hundred short strings, so caching costs
  # nothing, and this deliberately does not pull the allele matrix into memory.
  isolates <- reactive({
    db_events$depend(db_rev, "isolates")
    req(db_path())
    existing_strains(db_path())
  })

  list(metadata = metadata, isolates = isolates)
}
