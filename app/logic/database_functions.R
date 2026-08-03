# app/logic/database_functions.R
#
# Core database utilities for PhyloTrace, providing schema migration, metadata
# table initialization and updates, and data fetching for classical MLST, AMR
# screening, locus information, and isolate deletions.

box::use(
  DBI[
    dbBegin,
    dbClearResult,
    dbCommit,
    dbDisconnect,
    dbExecute,
    dbFetch,
    dbGetQuery,
    dbListFields,
    dbListTables,
    dbReadTable,
    dbRollback,
    dbSendQuery,
    dbWriteTable
  ],
  app / logic / db_compat[REF_SOUCHE],
  app / logic / db_connect[connect],
  app / logic / db_sources[SOURCE_COL, SOURCE_LOCAL],
  app / logic / field_labels[AMR_COL_PREFIX, MLST_COL_PREFIX],
  app / logic / logging[log_event],
)

# Helper: normalizes locus names by converting underscores to hyphens for matching.
.norm_locus <- function(x) gsub("[-_]", "-", x)

#' Tables Keyed on Isolate Name
#'
#' Character vector of PhyloTrace-managed tables that use `isolate` as a key.
#' Note: pyMLST's own `mlst` table is deliberately excluded as it uses `souche`.
#' @export
ISOLATE_KEYED_TABLES <- c(
  "classical_mlst",
  "amr_results",
  "amr_summary",
  "phylotrace_custom_values",
  "genome_hashes"
)

#' Migrate Database Isolate Column Names
#'
#' Legacy database versions used `souche` for isolate keys in custom tables.
#' This function renames `souche` columns to `isolate` in relevant tables if needed.
#'
#' @param db_path Character path to the SQLite database file.
#' @return Invisible character vector listing the names of modified tables.
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

  con <- connect(db_path)
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
    log_event(
      "DB",
      "migrate isolate key",
      sprintf(
        "renamed souche -> isolate in: %s",
        paste(renamed, collapse = ", ")
      )
    )
  }

  invisible(renamed)
}

#' Load Scheme Overview Data
#'
#' Reads the high-level scheme summary table from the database if available.
#'
#' @param db_path Character path to the SQLite database file.
#' @return Data frame containing scheme overview information, or `NULL` if missing.
#' @export
load_db_scheme_overview <- function(db_path) {
  con <- connect(db_path)
  on.exit(dbDisconnect(con))

  tables <- dbListTables(con)

  if (isFALSE("scheme_overview" %in% tables)) {
    message("Database does not contain 'scheme_overview' table")
    return(NULL)
  }

  return(dbReadTable(con, "scheme_overview"))
}

#' Load Database Organism / Species
#'
#' Extracts the species name stored in the `mlst_type` table.
#'
#' @param db_path Character path to the SQLite database file.
#' @return Character scalar with the species name, or `NULL` if missing or unpopulated.
#' @export
load_db_species <- function(db_path) {
  con <- connect(db_path)
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

#' Construct or Synchronize Metadata Table
#'
#' Ensures the `metadata` table exists in the database and includes all present isolates.
#' Automatically handles missing schema columns and appends unlisted isolates.
#'
#' @param db_path Character path to the SQLite database file.
#' @return Data frame representing the full `metadata` table, or `NULL` if prerequisites missing.
#' @export
make_metadata_table <- function(db_path) {
  con <- connect(db_path)
  on.exit(dbDisconnect(con))

  tables <- dbListTables(con)

  if (isFALSE(all(c("mlst", "mlst_type", "sequences") %in% tables))) {
    message("Database does not contain expected tables")
    return()
  }

  # Query distinct isolates excluding reference strains
  res <- dbSendQuery(con, "SELECT DISTINCT souche FROM mlst")
  all_isolates <- unname(unlist(dbFetch(res)))
  dbClearResult(res)
  isolates <- all_isolates[all_isolates != "ref"]
  if (!length(isolates)) {
    return()
  }

  organism <- dbReadTable(con, "mlst_type")$species
  now <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")

  # Lookup typing timestamp from genome_hashes if available
  hashed_at <- character(0)
  if ("genome_hashes" %in% tables) {
    gh <- tryCatch(
      dbGetQuery(con, "SELECT isolate, hashed_at FROM genome_hashes"),
      error = function(e) NULL
    )
    if (!is.null(gh) && nrow(gh)) {
      hashed_at <- stats::setNames(
        substr(as.character(gh$hashed_at), 1, 19),
        gh$isolate
      )
    }
  }
  called_at_for <- function(iso) {
    stamp <- unname(hashed_at[iso])
    stamp[is.na(stamp) | !nzchar(stamp)] <- now
    stamp
  }

  # Append new isolates or migrate missing columns if table exists
  if ("metadata" %in% tables) {
    # Migrate metadata tables created before newer columns were added: SQLite
    # appends new columns at the end, matching the build-path column order.
    added_cols <- c(
      "geo_loc_name_city",
      "geo_loc_name_postal_code",
      "called_at",
      "geo_loc_coordinates",
      SOURCE_COL
    )
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
        called_at = called_at_for(new_isolates),
        geo_loc_coordinates = NA_character_,
        stringsAsFactors = FALSE
      )
      new_rows[[SOURCE_COL]] <- SOURCE_LOCAL
      dbWriteTable(con, "metadata", new_rows, append = TRUE)
      log_event(
        "DB",
        "metadata",
        sprintf("%d new isolate row(s) appended", length(new_isolates))
      )
    }

    return(dbReadTable(con, "metadata"))
  }

  # Initialize new metadata structure
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
    called_at = called_at_for(isolates),
    geo_loc_coordinates = NA_character_,
    stringsAsFactors = FALSE
  )
  metadata[[SOURCE_COL]] <- SOURCE_LOCAL

  dbWriteTable(con, "metadata", metadata)
  log_event(
    "DB",
    "metadata",
    sprintf("initialised, %d row(s) written", nrow(metadata))
  )

  return(metadata)
}

#' Get Metadata Fields
#'
#' Retrieves list of column names present in the database `metadata` table.
#'
#' @param db_path Character path to the SQLite database file.
#' @return Character vector of field names, or `character(0)` if table missing/invalid.
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

  con <- connect(db_path)
  on.exit(dbDisconnect(con))

  if (!"metadata" %in% dbListTables(con)) {
    return(character(0))
  }

  dbListFields(con, "metadata")
}

# Helper: extracts the first non-null/non-empty string from a character vector.
.first_nonempty <- function(v) {
  v <- v[!is.na(v) & nzchar(v)]
  if (length(v)) v[[1]] else NA_character_
}

#' Load Classical MLST Results
#'
#' Pivots non-7-gene classical MLST database records into a wide per-isolate display layout.
#'
#' @param db_path Character path to the SQLite database file.
#' @return Wide data frame with isolate columns and locus alleles, or `NULL` if missing.
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

  con <- connect(db_path)
  on.exit(dbDisconnect(con))

  if (!"classical_mlst" %in% dbListTables(con)) {
    return(NULL)
  }

  # absence of `status` is tolerated so a
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
  # ("novel"/"partial")
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

#' Append Classical MLST Data to Metadata
#'
#' Joins wide classical MLST profile columns onto an existing isolate-keyed metadata data frame.
#'
#' @param meta Metadata data frame with an `isolate` column.
#' @param db_path Character path to the SQLite database file.
#' @return Input data frame extended with MLST columns, with added names recorded in `"mlst_cols"` attribute.
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

#' Load Summary AMR Results
#'
#' Pivots antimicrobial resistance summary records into a wide per-isolate display layout.
#'
#' @param db_path Character path to the SQLite database file.
#' @return Wide data frame with drug class profile summary columns, or `NULL` if unavailable.
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

  con <- connect(db_path)
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

  # Which section a drug class belongs to. abritamr reports resistance
  # (matches/partials) and virulence/stress in separate files, and a class only
  # ever comes from one of them — so this labels the column rather than
  # splitting it. Consumers that show AMR columns to the user (the tree's
  # heatmap picker) group by it.
  class_section <- character(0)

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
    class_section[paste0(AMR_COL_PREFIX, dc)] <- if (
      all(as$section[as$drug_class == dc] == "virulence")
    ) {
      "Virulence / stress"
    } else {
      "Resistance"
    }
  }

  attr(out, "amr_class_sections") <- class_section
  out
}

#' Append AMR Summary Data to Metadata
#'
#' Joins wide AMR summary columns onto an existing isolate-keyed metadata data frame.
#'
#' @param meta Metadata data frame with an `isolate` column.
#' @param db_path Character path to the SQLite database file.
#' @return Input data frame extended with AMR summary columns, tracking added fields in `"amr_cols"`.
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
  # Which of them are resistance and which virulence/stress, for consumers that
  # group AMR columns when offering them (the tree's heatmap picker).
  attr(meta, "amr_class_sections") <- if (is.null(amr)) {
    character(0)
  } else {
    (attr(amr, "amr_class_sections") %||% character(0))[appended]
  }
  meta
}

#' Priority Ordered AMR Match Call States
#' @export
AMR_CALL_STATES <- c("Match", "Inexact", "Partial")

#' Load Detailed Matrix of AMR Gene Calls
#'
#' Converts gene-level AMR findings into a factor matrix categorized by quality state.
#'
#' @param db_path Character path to the SQLite database file.
#' @return Wide data frame with factor columns for gene presence, or `NULL` if missing.
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

  con <- connect(db_path)
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
  col_section <- character(0)
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
      col_section[col] <- if (all(sub$section == "virulence")) {
        "Virulence / stress"
      } else {
        "Resistance"
      }
    }
  }

  attr(out, "amr_gene_groups") <- col_group
  attr(out, "amr_gene_labels") <- col_gene
  attr(out, "amr_gene_sections") <- col_section
  out
}

#' Append Detailed AMR Gene Matrix to Metadata
#'
#' Joins individual AMR gene call state factors onto a metadata data frame.
#'
#' @param meta Metadata data frame containing an `isolate` column.
#' @param db_path Character path to the SQLite database file.
#' @return Data frame augmented with individual AMR gene presence columns.
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

#' Purge Isolates from Database
#'
#' Removes specified isolates and all associated profiles across all database tables.
#' Runs within a transaction to guarantee atomic updates.
#'
#' @param db_path Character path to the SQLite database file.
#' @param isolates Character vector of isolate IDs/names to remove.
#' @param keep_alleles Logical; if `TRUE`, orphan allele sequences are kept for matching.
#' @return Invisible logical `TRUE` on successful deletion transaction, `FALSE` otherwise.
#' @export
remove_isolates <- function(db_path, isolates, keep_alleles = TRUE) {
  isolates <- setdiff(unique(isolates[!is.na(isolates)]), REF_SOUCHE)
  if (!length(isolates)) {
    return(invisible(FALSE))
  }

  con <- connect(db_path)
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
  if (isTRUE(ok)) {
    dbCommit(con)
  } else {
    dbRollback(con)
  }

  log_event(
    "DB",
    "remove-isolates",
    sprintf(
      "%d isolate(s) removed (keep_alleles=%s) %s",
      length(isolates),
      keep_alleles,
      if (isTRUE(ok)) "" else "failed (rolled back)"
    )
  )

  invisible(ok)
}

#' Overwrite Database Metadata Table
#'
#' Replaces contents of the `metadata` table with the provided data frame.
#'
#' @param db_path Character path to the SQLite database file.
#' @param data Data frame containing complete updated metadata.
#' @return Invisible logical `TRUE` after writing to database.
#' @export
save_metadata_table <- function(db_path, data) {
  con <- connect(db_path)
  on.exit(dbDisconnect(con))
  dbWriteTable(con, "metadata", data, overwrite = TRUE)
  log_event(
    "DB",
    "metadata",
    sprintf("saved (overwrite), %d row(s)", nrow(data))
  )
  invisible(TRUE)
}

#' Fetch Scheme Loci Summary with Allele Counts
#'
#' Returns scheme targets supplemented with distinct allele counts per target locus.
#'
#' @param db_path Character path to the SQLite database file.
#' @return Data frame listing locus attributes and allele counts, or `NULL` if missing.
#' @export
load_loci_info <- function(db_path) {
  con <- connect(db_path)
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

  targets$Alleles <- NULL

  targets
}

#' Load Locus Allele Usage
#'
#' Retrieves distinct allele IDs for a locus and counts isolate occurrences.
#'
#' @param db_path Character path to the SQLite database file.
#' @param gene Character scalar indicating the locus gene name.
#' @return Data frame listing `seqid`, occurrence `count`, and `present` logical indicator.
#' @export
load_locus_alleles <- function(db_path, gene) {
  con <- connect(db_path)
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

#' Fetch Allele Sequence Text
#'
#' Looks up the nucleotide sequence string corresponding to a specific sequence ID.
#'
#' @param db_path Character path to the SQLite database file.
#' @param seqid Integer sequence identifier.
#' @return Character scalar nucleotide sequence, or `NULL` if not found.
#' @export
load_allele_sequence <- function(db_path, seqid) {
  con <- connect(db_path)
  on.exit(dbDisconnect(con))

  res <- dbGetQuery(
    con,
    "SELECT sequence FROM sequences WHERE id = ?",
    params = list(seqid)
  )$sequence

  if (!length(res)) NULL else res[[1]]
}

#' Format Locus Alleles as FASTA
#'
#' Exports all sequence entries assigned to a specified locus as FASTA formatted strings.
#'
#' @param db_path Character path to the SQLite database file.
#' @param gene Character scalar specifying the locus gene.
#' @return Character vector of formatted FASTA entries (one entry per record).
#' @export
locus_fasta <- function(db_path, gene) {
  con <- connect(db_path)
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
