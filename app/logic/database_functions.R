# app/logic/database_functions.R

box::use(
  DBI[
    dbConnect,
    dbDisconnect,
    dbListTables,
    dbSendQuery,
    dbFetch,
    dbClearResult,
    dbReadTable,
    dbWriteTable,
    dbListFields,
    dbGetQuery,
    dbExecute,
    dbBegin,
    dbCommit,
    dbRollback
  ],
  RSQLite[SQLite],
  app / logic / field_labels[MLST_COL_PREFIX, AMR_COL_PREFIX],
  app / logic / db_compat[REF_SOUCHE],
)

# The scheme's `targets` table stores loci as "FTL_0001" while the `mlst` table
# stores the same locus as "FTL-0001"; normalise the separator so the two can
# be matched.
.norm_locus <- function(x) gsub("[-_]", "-", x)

# PhyloTrace-owned tables that key on an isolate name. pyMLST's own `mlst` is
# deliberately absent: it spells the same key `souche`, that column is written by
# pyMLST/alembic, and it is the one place in the schema allowed to say so.
ISOLATE_KEYED_TABLES <- c(
  "classical_mlst",
  "amr_results",
  "amr_summary",
  "phylotrace_custom_values",
  "genome_hashes"
)

### Bring a database's isolate key up to the current spelling
# These tables used to inherit pyMLST's `souche` for their isolate column. They
# now say `isolate`, so a database written before that change carries columns no
# query can find - the app fails on load with "no such column: isolate". The
# rename is metadata-only (SQLite rewrites the schema, not the rows), so it costs
# nothing on a large database and is safe to attempt on every load: a table
# already using `isolate`, or missing entirely, is skipped.
#
# Runs beside hash_database() when a database is opened. Best-effort by design -
# a failure here must not stop the database from loading, and the caller finds
# out through the same "no such column" error it would have had anyway.
#' @export
migrate_isolate_key <- function(db_path) {
  if (
    is.null(db_path) ||
      length(db_path) != 1 ||
      is.na(db_path) ||
      !file.exists(db_path)
  ) {
    return(invisible(character(0)))
  }

  con <- dbConnect(SQLite(), db_path, busy_timeout = 5000)
  on.exit(dbDisconnect(con))

  tables <- tryCatch(dbListTables(con), error = function(e) character(0))
  renamed <- character(0)

  for (nm in intersect(ISOLATE_KEYED_TABLES, tables)) {
    cols <- tryCatch(dbListFields(con, nm), error = function(e) character(0))
    if (!("souche" %in% cols) || "isolate" %in% cols) {
      next
    }
    ok <- tryCatch(
      {
        dbExecute(
          con,
          sprintf("ALTER TABLE %s RENAME COLUMN souche TO isolate", nm)
        )
        TRUE
      },
      error = function(e) FALSE
    )
    if (isTRUE(ok)) renamed <- c(renamed, nm)
  }

  if (length(renamed)) {
    message(paste(
      "Renamed souche -> isolate in:",
      paste(renamed, collapse = ", ")
    ))
  }

  invisible(renamed)
}


#' @export
load_db_scheme_overview <- function(db_path) {
  con <- dbConnect(SQLite(), db_path)
  on.exit(dbDisconnect(con))

  tables <- dbListTables(con)

  if (isFALSE("scheme_overview" %in% tables)) {
    message("Database does not contain 'scheme_overview' table")
    return(NULL)
  }

  return(dbReadTable(con, "scheme_overview"))
}

#' Read the organism/species name stored in the database's `mlst_type` table.
#'
#' This is the authoritative species of the loaded scheme (written at typing
#' time) and is more robust than parsing it out of the scheme overview. Returns
#' a single species string, or NULL when the table or value is absent.
#' @export
load_db_species <- function(db_path) {
  con <- dbConnect(SQLite(), db_path)
  on.exit(dbDisconnect(con))

  tables <- dbListTables(con)

  if (isFALSE("mlst_type" %in% tables)) {
    message("Database does not contain 'mlst_type' table")
    return(NULL)
  }

  species <- dbReadTable(con, "mlst_type")$species
  species <- species[!is.na(species) & nzchar(species)]

  if (!length(species)) {
    return(NULL)
  }

  species[[1]]
}

#' @export
make_metadata_table <- function(db_path) {
  con <- dbConnect(SQLite(), db_path)
  on.exit(dbDisconnect(con))

  tables <- dbListTables(con)

  if (isFALSE(all(c("mlst", "mlst_type", "sequences") %in% tables))) {
    message("Database does not contain expected tables")
    return()
  }

  # Get present isolates
  res <- dbSendQuery(con, "SELECT DISTINCT souche FROM mlst")
  all_isolates <- unname(unlist(dbFetch(res)))
  dbClearResult(res)
  isolates <- all_isolates[all_isolates != "ref"]
  if (!length(isolates)) {
    return()
  }

  # Get current organism
  organism <- dbReadTable(con, "mlst_type")$species

  # If metadata table exists, only append rows for isolates not yet listed
  if ("metadata" %in% tables) {
    # Migrate metadata tables created before newer columns were added: SQLite
    # appends new columns at the end, matching the build-path column order.
    added_cols <- c("geo_loc_name_city", "geo_loc_name_postal_code")
    for (col in setdiff(added_cols, dbListFields(con, "metadata"))) {
      dbExecute(con, sprintf('ALTER TABLE metadata ADD COLUMN "%s" TEXT', col))
    }

    existing <- dbReadTable(con, "metadata")
    new_isolates <- setdiff(isolates, existing$isolate)

    if (length(new_isolates)) {
      new_rows <- data.frame(
        isolate = new_isolates,
        primary_laboratory_sample_id = NA_character_,
        specimen_source_id = NA_character_,
        sample_collection_date = NA_character_,
        geo_loc_name_country = NA_character_,
        geo_loc_name_state_province = NA_character_,
        sample_collected_by = NA_character_,
        sequence_submitted_by = NA_character_,
        organism = organism,
        purpose_of_sampling = NA_character_,
        purpose_of_sequencing = NA_character_,
        geo_loc_name_city = NA_character_,
        geo_loc_name_postal_code = NA_character_,
        stringsAsFactors = FALSE
      )
      dbWriteTable(con, "metadata", new_rows, append = TRUE)
    }

    return(dbReadTable(con, "metadata"))
  }

  # Build standard metadata table (GenEpiO-aligned fields)
  metadata <- data.frame(
    isolate = isolates,
    primary_laboratory_sample_id = NA_character_,
    specimen_source_id = NA_character_,
    sample_collection_date = NA_character_,
    geo_loc_name_country = NA_character_,
    geo_loc_name_state_province = NA_character_,
    sample_collected_by = NA_character_,
    sequence_submitted_by = NA_character_,
    organism = organism,
    purpose_of_sampling = NA_character_,
    purpose_of_sequencing = NA_character_,
    geo_loc_name_city = NA_character_,
    geo_loc_name_postal_code = NA_character_,
    stringsAsFactors = FALSE
  )

  # Write table to database
  dbWriteTable(con, "metadata", metadata)

  return(metadata)
}

#' The `metadata` table's column names, or `character(0)` when the database has
#' no metadata table. Read-only: unlike `make_metadata_table()` this never
#' creates or migrates anything, so it is safe to call from a UI renderer.
#' @export
metadata_columns <- function(db_path) {
  if (
    is.null(db_path) ||
      length(db_path) != 1 ||
      is.na(db_path) ||
      !file.exists(db_path)
  ) {
    return(character(0))
  }

  con <- dbConnect(SQLite(), db_path)
  on.exit(dbDisconnect(con))

  if (!"metadata" %in% dbListTables(con)) {
    return(character(0))
  }

  dbListFields(con, "metadata")
}

# First non-empty value of `v`, or NA. Classical-MLST fields that are
# strain-level (st, status) repeat across an isolate's per-locus rows, so any of
# them stands in for the isolate.
.first_nonempty <- function(v) {
  v <- v[!is.na(v) & nzchar(v)]
  if (length(v)) v[[1]] else NA_character_
}

#' Per-isolate classical-MLST summary, wide, for display only.
#'
#' `classical_mlst` (written by pymlst at typing time) stores one row per
#' (isolate, locus), with the strain-level ST and status repeated on each of the
#' isolate's rows. This pivots it to one row per isolate: a `<prefix>st` column
#' plus one `<prefix><locus>` column per locus holding that isolate's allele,
#' where `<prefix>` is `MLST_COL_PREFIX`. Loci keep the order in which they were
#' typed. Every column is character.
#'
#' The sequence type shows the registered ST number for a known profile;
#' otherwise it shows the call `status` (e.g. "novel", "partial"), because a
#' novel/partial profile has no ST to report and that distinction is what the
#' viewer needs to see. This is a display convenience only.
#'
#' This never touches the `metadata` table - it is a read-only companion other
#' views merge in at display time (see `append_classical_mlst`). Returns NULL
#' when the database has no `classical_mlst` table or it holds no rows.
#' @export
load_classical_mlst <- function(db_path) {
  if (
    is.null(db_path) ||
      length(db_path) != 1 ||
      is.na(db_path) ||
      !file.exists(db_path)
  ) {
    return(NULL)
  }

  con <- dbConnect(SQLite(), db_path)
  on.exit(dbDisconnect(con))

  if (!"classical_mlst" %in% dbListTables(con)) {
    return(NULL)
  }

  # `status` is part of the pymlst schema, but tolerate its absence so a
  # partially-written table never errors the whole browse view.
  has_status <- "status" %in% dbListFields(con, "classical_mlst")
  cm <- dbGetQuery(
    con,
    sprintf(
      "SELECT isolate, gene, allele, st%s FROM classical_mlst ORDER BY id",
      if (has_status) ", status" else ""
    )
  )
  if (!nrow(cm)) {
    return(NULL)
  }
  if (!has_status) {
    cm$status <- NA_character_
  }

  isolates <- unique(cm$isolate)
  out <- data.frame(isolate = isolates, stringsAsFactors = FALSE)

  # ST display: the registered number when known, otherwise the status word
  # ("novel"/"partial") so a profile without an ST is still self-explanatory.
  st <- vapply(
    isolates,
    function(s) .first_nonempty(cm$st[cm$isolate == s]),
    character(1)
  )
  status <- vapply(
    isolates,
    function(s) .first_nonempty(cm$status[cm$isolate == s]),
    character(1)
  )
  missing <- is.na(st) | !nzchar(st)
  st[missing] <- status[missing]
  out[[paste0(MLST_COL_PREFIX, "st")]] <- unname(st)

  # One column per locus (first-typed order), holding the isolate's allele.
  for (locus in unique(cm$gene)) {
    sub <- cm[cm$gene == locus, c("isolate", "allele"), drop = FALSE]
    allele <- tapply(sub$allele, sub$isolate, function(v) v[[1]])
    out[[paste0(MLST_COL_PREFIX, locus)]] <- as.character(allele[isolates])
  }

  out
}

#' Merge the display-only classical-MLST columns into a metadata frame keyed on
#' `isolate`.
#'
#' Reusable across views (the browse table, the visualization engines): given a
#' metadata data frame and the database path, it appends the
#' `load_classical_mlst` columns, matched by isolate, and records the names it
#' added in the result's `"mlst_cols"` attribute (so callers can style them
#' read-only, group them, or strip them before a save). Columns that would
#' shadow an existing metadata field are skipped, so real metadata always wins.
#' `meta` is returned unchanged (with an empty `"mlst_cols"`) when it is not a
#' non-empty isolate-keyed frame or the database has no classical typing.
#' @export
append_classical_mlst <- function(meta, db_path) {
  if (
    !is.data.frame(meta) || !nrow(meta) || isFALSE("isolate" %in% names(meta))
  ) {
    return(meta)
  }

  appended <- character(0)
  mlst <- load_classical_mlst(db_path)
  if (!is.null(mlst)) {
    add <- setdiff(names(mlst), c("isolate", names(meta)))
    if (length(add)) {
      idx <- match(meta$isolate, mlst$isolate)
      for (col in add) {
        meta[[col]] <- mlst[[col]][idx]
      }
      appended <- add
    }
  }

  attr(meta, "mlst_cols") <- appended
  meta
}

#' Per-isolate AMR-screening summary, wide, for display only.
#'
#' `amr_summary` (written by the AMR screen at typing time) stores one row per
#' (isolate, section, drug_class, gene), tidy. This pivots it to one row per
#' isolate: an `<prefix>profile` column listing the drug classes with a confident
#' (`matches`) hit, plus one `<prefix><drug_class>` column per drug class /
#' virulence-stress group holding that isolate's genes (comma-joined, keeping the
#' `*`/`^` quality flags), where `<prefix>` is `AMR_COL_PREFIX`. Drug classes keep
#' the order they first appear in the table. Every column is character.
#'
#' Display-only companion, mirroring `load_classical_mlst`: never touches
#' `metadata`. Returns NULL when the database has no `amr_summary` table or it
#' holds no rows.
#' @export
load_amr <- function(db_path) {
  if (
    is.null(db_path) ||
      length(db_path) != 1 ||
      is.na(db_path) ||
      !file.exists(db_path)
  ) {
    return(NULL)
  }

  con <- dbConnect(SQLite(), db_path)
  on.exit(dbDisconnect(con))

  if (!"amr_summary" %in% dbListTables(con)) {
    return(NULL)
  }

  as <- dbGetQuery(
    con,
    "SELECT isolate, section, drug_class, genes FROM amr_summary ORDER BY id"
  )
  if (!nrow(as)) {
    return(NULL)
  }

  isolates <- unique(as$isolate)
  out <- data.frame(isolate = isolates, stringsAsFactors = FALSE)

  # Resistance profile: the drug classes with a confident (matches) hit.
  profile <- vapply(
    isolates,
    function(s) {
      cls <- unique(as$drug_class[
        as$isolate == s & as$section == "matches"
      ])
      if (length(cls)) paste(cls, collapse = ", ") else NA_character_
    },
    character(1)
  )
  out[[paste0(AMR_COL_PREFIX, "profile")]] <- unname(profile)

  # One column per drug class / group (first-seen order), holding the isolate's
  # genes across all sections, comma-joined.
  for (dc in unique(as$drug_class)) {
    sub <- as[as$drug_class == dc, c("isolate", "genes"), drop = FALSE]
    genes <- tapply(
      sub$genes,
      sub$isolate,
      function(v) paste(unique(v[!is.na(v) & nzchar(v)]), collapse = ", ")
    )
    out[[paste0(AMR_COL_PREFIX, dc)]] <- as.character(genes[isolates])
  }

  out
}

#' Merge the display-only AMR-screening columns into a metadata frame keyed on
#' `isolate`, recording the added names in the result's `"amr_cols"` attribute.
#' The AMR analogue of `append_classical_mlst`: reusable across views, columns
#' that would shadow an existing metadata field are skipped, and `meta` is
#' returned unchanged (empty `"amr_cols"`) when it is not a non-empty
#' isolate-keyed frame or the database has no AMR screening.
#' @export
append_amr <- function(meta, db_path) {
  if (
    !is.data.frame(meta) || !nrow(meta) || isFALSE("isolate" %in% names(meta))
  ) {
    return(meta)
  }

  appended <- character(0)
  amr <- load_amr(db_path)
  if (!is.null(amr)) {
    add <- setdiff(names(amr), c("isolate", names(meta)))
    if (length(add)) {
      idx <- match(meta$isolate, amr$isolate)
      for (col in add) {
        meta[[col]] <- amr[[col]][idx]
      }
      appended <- add
    }
  }

  attr(meta, "amr_cols") <- appended
  meta
}

# The call-quality states an AMR gene cell can hold, best first. `min(rank)`
# picks the state to show when one isolate has several rows for the same gene
# (two copies found, one exact and one not).
#' @export
AMR_CALL_STATES <- c("Match", "Inexact", "Partial")

#' Per-isolate AMR gene-presence matrix, wide, hierarchical, for display only.
#'
#' A different pivot of `amr_summary` from `load_amr()`'s: instead of one
#' comma-joined column per drug class, this holds one column per *individual*
#' gene, with abritamr's trailing `*`/`^` quality flag stripped off the column's
#' gene name and moved into the cell *value* instead. The flag is a property of
#' one isolate's call, not of the gene: abritamr appends `*` when a gene-family
#' hit came from an inexact BLAST match rather than an exact/allele one (see
#' Collate.py's `ANNOTATIONS` / `MATCH`), so "mexX" and "mexX*" are the same
#' gene called with different confidence in different isolates. Keeping them as
#' two columns would split one gene across the grid and hide that they are
#' comparable; keeping the distinction in the cell preserves it.
#'
#' Every gene column is therefore a factor over `AMR_CALL_STATES`, `NA` where
#' the isolate has no row for that gene:
#'   * `"Match"`   - reported by abritamr without a quality flag. For an AMR
#'                   gene family that means an exact/allele match; point
#'                   mutations and virulence/stress genes are never flagged at
#'                   all, so they always land here.
#'   * `"Inexact"` - flagged `*`/`^`: a BLAST hit close to, but not identical
#'                   to, a known reference allele.
#'   * `"Partial"` - from `amr_summary`'s `partials` section: a method outside
#'                   abritamr's `MATCH` set (e.g. an internal stop codon), so
#'                   the gene is likely truncated or non-functional. Takes
#'                   precedence over the `*` flag, which such rows also carry.
#'
#' Columns are grouped by drug class / virulence-stress group, first-seen order
#' in `amr_summary`; genes within a group keep their first-seen order too. The
#' grouping a caller needs to render a hierarchical header (or to let a picker
#' show/hide a whole group at once) comes back as two attributes, both named by
#' gene column and in column order:
#'   * `"amr_gene_groups"` - the drug class / group label each column belongs to
#'   * `"amr_gene_labels"` - the bare (canonical) gene symbol, for a leaf header
#'
#' Display-only companion, mirroring `load_amr`: never touches `metadata`.
#' Returns NULL when the database has no `amr_summary` table or it holds no
#' rows.
#' @export
load_amr_matrix <- function(db_path) {
  if (
    is.null(db_path) ||
      length(db_path) != 1 ||
      is.na(db_path) ||
      !file.exists(db_path)
  ) {
    return(NULL)
  }

  con <- dbConnect(SQLite(), db_path)
  on.exit(dbDisconnect(con))

  if (!"amr_summary" %in% dbListTables(con)) {
    return(NULL)
  }

  as <- dbGetQuery(
    con,
    "SELECT isolate, section, drug_class, genes FROM amr_summary ORDER BY id"
  )
  if (!nrow(as)) {
    return(NULL)
  }

  # Canonical gene key: abritamr's own quality flag, stripped off the name and
  # kept as the call state below.
  as$gene <- sub("[*^]+$", "", as$genes)
  as$state <- ifelse(
    as$section == "partials",
    "Partial",
    ifelse(grepl("[*^]$", as$genes), "Inexact", "Match")
  )

  isolates <- unique(as$isolate)
  out <- data.frame(isolate = isolates, stringsAsFactors = FALSE)
  rank <- stats::setNames(seq_along(AMR_CALL_STATES), AMR_CALL_STATES)

  col_group <- character(0)
  col_gene <- character(0)
  n <- 0L
  for (grp in unique(as$drug_class)) {
    for (gene in unique(as$gene[as$drug_class == grp])) {
      n <- n + 1L
      col <- paste0(AMR_COL_PREFIX, "g", n)
      sub <- as[as$drug_class == grp & as$gene == gene, , drop = FALSE]
      best <- tapply(unname(rank[sub$state]), sub$isolate, min)
      out[[col]] <- factor(
        AMR_CALL_STATES[best[isolates]],
        levels = AMR_CALL_STATES
      )
      col_group[col] <- grp
      col_gene[col] <- gene
    }
  }

  attr(out, "amr_gene_groups") <- col_group
  attr(out, "amr_gene_labels") <- col_gene
  out
}

#' Merge the display-only AMR gene-matrix columns into a metadata frame keyed
#' on `isolate`. The gene-level analogue of `append_amr()` - see
#' `load_amr_matrix()` for the column shape. Adds the same `"amr_cols"`
#' attribute `append_amr()` does (here one column per gene - readonly, hidden
#' by default, stripped before save, exactly like the per-drug-class columns it
#' replaces for this consumer), plus `"amr_gene_groups"` / `"amr_gene_labels"`,
#' subset and reordered to the columns actually added, for a caller that
#' renders a hierarchical header.
#' @export
append_amr_matrix <- function(meta, db_path) {
  if (
    !is.data.frame(meta) || !nrow(meta) || isFALSE("isolate" %in% names(meta))
  ) {
    return(meta)
  }

  appended <- character(0)
  groups <- character(0)
  labels <- character(0)
  mat <- load_amr_matrix(db_path)
  if (!is.null(mat)) {
    add <- setdiff(names(mat), c("isolate", names(meta)))
    if (length(add)) {
      idx <- match(meta$isolate, mat$isolate)
      for (col in add) {
        meta[[col]] <- mat[[col]][idx]
      }
      appended <- add
      all_groups <- attr(mat, "amr_gene_groups", exact = TRUE)
      all_labels <- attr(mat, "amr_gene_labels", exact = TRUE)
      groups <- all_groups[intersect(add, names(all_groups))]
      labels <- all_labels[intersect(add, names(all_labels))]
    }
  }

  attr(meta, "amr_cols") <- appended
  attr(meta, "amr_gene_groups") <- groups
  attr(meta, "amr_gene_labels") <- labels
  meta
}

### Delete isolates and everything that hangs off them
# One isolate spans the whole schema, and the pieces are joined by *name* rather
# than by a foreign key, so removal has to visit each table explicitly:
#
#   mlst                      pyMLST's own table - the key is `souche` there
#   sequences / hashes        allele storage, keyed on seqid; pruned to whatever
#                             `mlst` still references
#   metadata                  keyed on `isolate`
#   ISOLATE_KEYED_TABLES      classical_mlst / amr_* / custom values / digests
#
# Two properties this has to guarantee:
#
#   * Atomicity. A half-finished removal leaves an isolate present in some
#     tables and gone from others, and because the key is a name, the next
#     isolate typed under that name silently inherits the leftovers. The whole
#     removal therefore runs in one transaction and rolls back as a unit.
#   * The scheme reference survives. `ref` is not an isolate: it is the seed
#     genome whose `mlst` rows define the scheme. Deleting it would strip the
#     file of its scheme while leaving the isolates in place, so it is filtered
#     out here rather than trusted not to arrive.
#
# `phylotrace_analyses` is deliberately NOT pruned: a saved Analysis records the
# isolate set it was built from, and the dashboard reports the drift ("n isolates
# removed"). Rewriting the selection would erase exactly that signal.
#
# `keep_alleles = TRUE` (the default) removes the isolate's `mlst` mapping but
# leaves its allele DNA in `sequences`/`hashes`, so imports that later match by
# sequence hash re-use the same allele identity and nomenclature stays stable.
# A kept allele with no remaining `mlst` reference is dormant - invisible to the
# allele counts (which count `DISTINCT seqid FROM mlst`) but live for matching.
# The one invariant this must never break is keeping `sequences` without their
# `hashes`: a stale hash whose id a later insert reuses gives one seqid two hash
# rows (see db_import.R). So the two are always pruned together or kept together,
# never split.
#
# Returns TRUE when a removal was attempted, FALSE when there was nothing to do
# or the transaction rolled back.
#' @export
remove_isolates <- function(db_path, isolates, keep_alleles = TRUE) {
  isolates <- setdiff(unique(isolates[!is.na(isolates)]), REF_SOUCHE)
  if (!length(isolates)) {
    return(invisible(FALSE))
  }

  con <- dbConnect(SQLite(), db_path, busy_timeout = 5000)
  on.exit(dbDisconnect(con))

  tables <- dbListTables(con)
  placeholders <- paste(rep("?", length(isolates)), collapse = ", ")

  dbBegin(con)
  ok <- tryCatch(
    {
      if ("mlst" %in% tables) {
        dbExecute(
          con,
          sprintf("DELETE FROM mlst WHERE souche IN (%s)", placeholders),
          params = as.list(isolates)
        )
        if (isFALSE(keep_alleles)) {
          # Remove sequences no longer referenced by any strain
          dbExecute(
            con,
            "DELETE FROM sequences WHERE id NOT IN (SELECT DISTINCT seqid FROM mlst)"
          )
          # …and their hashes. Leaving orphans behind is not cosmetic: a later
          # insert allocating `MAX(sequences.id) + n` can reuse an id that a
          # stale `hashes` row still holds, giving one seqid two hash rows and
          # silently corrupting every query that joins mlst to hashes. This is
          # also why keep_alleles never prunes just one of the two: keeping
          # `sequences` holds `MAX(id)` high, so no id is reused, and keeping
          # `hashes` alongside them means there is no orphan to reuse against.
          if ("hashes" %in% tables) {
            dbExecute(
              con,
              "DELETE FROM hashes WHERE id NOT IN (SELECT id FROM sequences)"
            )
          }
        }
      }

      if ("metadata" %in% tables) {
        dbExecute(
          con,
          sprintf("DELETE FROM metadata WHERE isolate IN (%s)", placeholders),
          params = as.list(isolates)
        )
      }

      # Every remaining table keys on `isolate` with no foreign key onto `mlst`
      # (whose matching column is pyMLST's `souche`), so nothing cascades the
      # delete for us - each has to be pruned here. Leaving the rows behind is
      # not cosmetic, because the key is a *name*: a later isolate typed under
      # the same name silently inherits the removed one's classical ST, AMR
      # calls and custom-variable values. For `genome_hashes` the consequence is
      # sharper still - a stale digest makes that new isolate read as a re-type
      # of the deleted one, or raises a name conflict against an assembly the
      # database no longer holds, corrupting the very signal the table exists to
      # give.
      for (nm in intersect(ISOLATE_KEYED_TABLES, tables)) {
        dbExecute(
          con,
          sprintf("DELETE FROM %s WHERE isolate IN (%s)", nm, placeholders),
          params = as.list(isolates)
        )
      }
      TRUE
    },
    error = function(e) FALSE
  )
  if (isTRUE(ok)) dbCommit(con) else dbRollback(con)

  invisible(ok)
}

#' @export
save_metadata_table <- function(db_path, data) {
  con <- dbConnect(SQLite(), db_path)
  on.exit(dbDisconnect(con))
  dbWriteTable(con, "metadata", data, overwrite = TRUE)
  invisible(TRUE)
}

#' Read the scheme's `targets` (loci) table and enrich it with the number of
#' distinct alleles stored per locus.
#'
#' Returns a data frame with the display columns `Locus`, `Gene`, `Start`,
#' `Length`, `Product`, `Allele Count`, plus an internal `.gene` column that
#' carries the matching `mlst` gene name (the loci-detail queries key on the
#' `mlst` spelling, not the `targets` one). Returns NULL when the database is
#' missing the `targets` or `mlst` table.
#' @export
load_loci_info <- function(db_path) {
  con <- dbConnect(SQLite(), db_path)
  on.exit(dbDisconnect(con))

  tables <- dbListTables(con)
  if (isFALSE(all(c("targets", "mlst") %in% tables))) {
    message("Database does not contain 'targets' and 'mlst' tables")
    return(NULL)
  }

  targets <- dbReadTable(con, "targets")

  # Distinct alleles per locus (the synthetic "ref" allele is counted too, as
  # it is a valid scheme allele).
  counts <- dbGetQuery(
    con,
    "SELECT gene, COUNT(DISTINCT seqid) AS n FROM mlst GROUP BY gene"
  )

  idx <- match(.norm_locus(targets$Locus), .norm_locus(counts$gene))

  targets$.gene <- counts$gene[idx]
  allele_count <- counts$n[idx]
  allele_count[is.na(allele_count)] <- 0L
  targets[["Allele Count"]] <- allele_count

  # `Start`/`Length` come back as character when stored as TEXT in SQLite, which
  # makes the DataTable sort them lexicographically. Coerce to numeric so they
  # (and the already-integer allele count) sort numerically.
  for (col in c("Start", "Length")) {
    if (col %in% names(targets)) {
      targets[[col]] <- suppressWarnings(as.numeric(targets[[col]]))
    }
  }

  # Replace the raw ".fasta" filename column with the integer count.
  targets$Alleles <- NULL

  targets
}

#' Allele usage for one locus.
#'
#' `gene` is the `mlst` gene name (the `.gene` column of `load_loci_info`).
#' Returns a data frame of every distinct allele stored for the locus with
#' columns `seqid` (integer allele index), `count` (isolates carrying it,
#' excluding the synthetic "ref") and `present` (`count > 0`). Rows are ordered
#' present-first, then by descending count.
#' @export
load_locus_alleles <- function(db_path, gene) {
  con <- dbConnect(SQLite(), db_path)
  on.exit(dbDisconnect(con))

  all_ids <- dbGetQuery(
    con,
    "SELECT DISTINCT seqid FROM mlst WHERE gene = ?",
    params = list(gene)
  )$seqid

  usage <- dbGetQuery(
    con,
    "SELECT seqid, COUNT(*) AS count FROM mlst
       WHERE gene = ? AND souche != 'ref' GROUP BY seqid",
    params = list(gene)
  )

  df <- data.frame(seqid = all_ids, stringsAsFactors = FALSE)
  df$count <- usage$count[match(df$seqid, usage$seqid)]
  df$count[is.na(df$count)] <- 0L
  df$present <- df$count > 0L

  df[order(!df$present, -df$count), , drop = FALSE]
}

#' Nucleotide sequence (character scalar) for a single allele index, or NULL
#' when the index is not stored.
#' @export
load_allele_sequence <- function(db_path, seqid) {
  con <- dbConnect(SQLite(), db_path)
  on.exit(dbDisconnect(con))

  res <- dbGetQuery(
    con,
    "SELECT sequence FROM sequences WHERE id = ?",
    params = list(seqid)
  )$sequence

  if (!length(res)) NULL else res[[1]]
}

#' All alleles of a locus as FASTA text: one `>index` / sequence record per
#' distinct allele stored for `gene`. Returns a character vector (one element
#' per record), or an empty vector when the locus has no alleles.
#' @export
locus_fasta <- function(db_path, gene) {
  con <- dbConnect(SQLite(), db_path)
  on.exit(dbDisconnect(con))

  res <- dbGetQuery(
    con,
    "SELECT DISTINCT s.id AS seqid, s.sequence AS sequence
       FROM mlst m JOIN sequences s ON s.id = m.seqid
      WHERE m.gene = ? ORDER BY s.id",
    params = list(gene)
  )

  if (!nrow(res)) {
    return(character(0))
  }

  paste0(">", res$seqid, "\n", res$sequence)
}
