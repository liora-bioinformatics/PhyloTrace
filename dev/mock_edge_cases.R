# dev/mock_edge_cases.R
#
# Databases built to stress the Database overviews' UI (Isolate, cgMLST, MLST,
# AMR) rather than their data logic: extreme names, single and empty databases,
# dozens of releases or settings groups, a locus with hundreds of alleles, very
# heavy AMR screens, everything failing at once, and sheer size. Each starts
# from a mock_database() and is reshaped through plain SQL or the app's own
# helpers (remove_isolates(), hash_database()), so it still opens like a typed
# database.
#
# write_edge_cases() writes the whole set into one folder together with a
# README.md checklist naming, for each database, what it stresses and where in
# the app to look.
#
# Usage (from the project root):
#   Rscript -e 'box::use(dev/mock_edge_cases); mock_edge_cases$write_edge_cases("~/edge_cases")'
#   Rscript -e 'box::use(dev/mock_edge_cases)
#     mock_edge_cases$write_edge_cases("~/edge_cases", cases = c("long_names", "empty"))'

box::use(
  DBI[dbAppendTable, dbDisconnect, dbExecute, dbGetQuery, dbListTables, dbWriteTable],
  stats[runif],
)
box::use(
  app / logic / database_functions[ISOLATE_KEYED_TABLES, remove_isolates],
  app / logic / db_connect[connect],
  app / logic / pymlst[hash_database],
  dev / mock_database[MOCK_SCENARIOS, mock_database],
)

# Scenarios that claim a slice of isolates; the whole-database ones replace a
# whole analysis and so cannot be combined with the rest.
SLICES <- setdiff(names(MOCK_SCENARIOS), c("skipped", "no_classical", "amr_skipped", "no_amr"))

# Runs `fun(con)` on an open connection to `path`.
with_db <- function(path, fun) {
  con <- connect(path)
  on.exit(dbDisconnect(con))
  fun(con)
}

# Isolates of a database, the scheme reference excluded.
isolates_of <- function(path) {
  with_db(path, function(con) {
    dbGetQuery(con, "SELECT DISTINCT souche FROM mlst WHERE souche != 'ref'")$souche
  })
}

# Renames isolates everywhere they are keyed: the allele table, metadata and
# every isolate-keyed result table.
rename_isolates <- function(path, from, to) {
  with_db(path, function(con) {
    tables <- dbListTables(con)
    dbWriteTable(con, "rename_map", data.frame(old = from, new = to), temporary = TRUE)
    dbExecute(
      con,
      "UPDATE mlst SET souche = (SELECT new FROM rename_map WHERE old = souche)
        WHERE souche IN (SELECT old FROM rename_map)"
    )
    for (table in intersect(c("metadata", ISOLATE_KEYED_TABLES), tables)) {
      dbExecute(
        con,
        sprintf(
          "UPDATE %1$s SET isolate = (SELECT new FROM rename_map WHERE old = isolate)
            WHERE isolate IN (SELECT old FROM rename_map)",
          table
        )
      )
    }
  })
}

# Every slice scenario at once: the longest Checks lists, several flags per
# isolate and several groups in every settings card.
build_all_checks <- function(path, n) {
  suppressMessages(mock_database(path, n_isolates = n, n_loci = 150, scenarios = SLICES))
}

# Isolate, locus, element and class names far longer than any column.
build_long_names <- function(path, n) {
  suppressMessages(mock_database(
    path,
    n_isolates = n,
    n_loci = 80,
    scenarios = c("mixed_schemes", "low_completeness", "amr_mixed_db")
  ))
  old <- isolates_of(path)
  rename_isolates(
    path,
    old,
    sprintf(
      "GCA_%09d.1_%s_assembly_from_a_very_long_sequencing_run_name_lane_%d_genomic",
      seq_along(old),
      old,
      seq_along(old)
    )
  )
  with_db(path, function(con) {
    suffix <- "_hypothetical_conserved_membrane_transport_protein_family_domain"
    dbExecute(con, sprintf("UPDATE mlst SET gene = gene || '%s'", suffix))
    dbExecute(con, sprintf("UPDATE targets SET Locus = Locus || '%s'", suffix))
    dbExecute(
      con,
      "UPDATE amr_results SET gene_symbol = gene_symbol || '_variant_of_an_extended_name',
              class = class || '/AMINOGLYCOSIDE/BETA-LACTAM/QUINOLONE/TETRACYCLINE',
              sequence_name = sequence_name || ' with a very long descriptive reference name'"
    )
    dbExecute(
      con,
      "UPDATE classical_mlst SET scheme = scheme || ' - extended multilocus scheme definition'"
    )
    dbExecute(
      con,
      "UPDATE typing_provenance SET cla_scheme = cla_scheme ||
         ' - extended multilocus scheme definition' WHERE cla_scheme IS NOT NULL"
    )
  })
}

# Isolate and element names with HTML, quotes, ampersands, spaces and non-ASCII
# characters, which every table, tooltip, label and list must show as text.
build_special_characters <- function(path, n) {
  suppressMessages(mock_database(path, n_isolates = n, n_loci = 60))
  old <- isolates_of(path)
  marks <- c(
    "<b>bold</b>", "a & b", "it's \"quoted\"", "space separated name", "Z\u00fcrich \u00b5-strain",
    "<script>alert(1)</script>", "plus+minus-(1)", "\u65e5\u672c\u682a"
  )
  rename_isolates(path, old, paste(old, rep_len(marks, length(old))))
  with_db(path, function(con) {
    dbExecute(
      con,
      "UPDATE amr_results SET gene_symbol = gene_symbol || '<i>&amp;</i>'
        WHERE id IN (SELECT id FROM amr_results ORDER BY id LIMIT 40)"
    )
  })
}

# One isolate: shares of 100%, no diversity, medians of one value.
build_single_isolate <- function(path, n) {
  suppressMessages(mock_database(path, n_isolates = 2, n_loci = 60))
  remove_isolates(path, isolates_of(path)[2])
}

# A scheme without any isolate: every card's empty state.
build_empty <- function(path, n) {
  suppressMessages(mock_database(path, n_isolates = 2, n_loci = 60))
  remove_isolates(path, isolates_of(path))
}

# A database from before provenance, assembly hashes, classical MLST and AMR
# screening existed: only allele calls and metadata.
build_legacy <- function(path, n) {
  suppressMessages(mock_database(
    path,
    n_isolates = n,
    n_loci = 80,
    scenarios = c("no_classical", "no_amr")
  ))
  with_db(path, function(con) {
    for (table in intersect(c("typing_provenance", "genome_hashes"), dbListTables(con))) {
      dbExecute(con, sprintf("DROP TABLE %s", table))
    }
  })
}

# Classical calls spread over many schemes and profile-table releases: a long
# Scheme Releases list and tallies that fold into "N more".
build_many_releases <- function(path, n) {
  suppressMessages(mock_database(path, n_isolates = n, n_loci = 60))
  iso <- isolates_of(path)
  schemes <- sprintf("Testus spp. (MLST scheme %d)", sample(1:6, length(iso), replace = TRUE))
  releases <- sprintf(
    "2026-%02d-%02d",
    sample(1:9, length(iso), TRUE),
    sample(1:28, length(iso), TRUE)
  )
  repos <- ifelse(runif(length(iso)) < 0.5, "pubmlst", "pasteur")
  with_db(path, function(con) {
    dbWriteTable(
      con,
      "release_map",
      data.frame(isolate = iso, scheme = schemes, version = releases, repo = repos),
      temporary = TRUE
    )
    lookup <- function(col, table) {
      sprintf("(SELECT %s FROM release_map m WHERE m.isolate = %s.isolate)", col, table)
    }
    dbExecute(con, sprintf(
      "UPDATE classical_mlst SET scheme = %s, scheme_version = %s, repository = %s",
      lookup("scheme", "classical_mlst"),
      lookup("version", "classical_mlst"),
      lookup("repo", "classical_mlst")
    ))
    dbExecute(con, sprintf(
      "UPDATE typing_provenance SET cla_scheme = %s, cla_scheme_version = %s, cla_repository = %s",
      lookup("scheme", "typing_provenance"),
      lookup("version", "typing_provenance"),
      lookup("repo", "typing_provenance")
    ))
  })
}

# Every isolate typed and screened with its own mix of thresholds, tool and
# database versions: long Typing Settings and Screening Settings lists.
build_many_settings <- function(path, n) {
  suppressMessages(mock_database(path, n_isolates = n, n_loci = 60))
  iso <- isolates_of(path)
  pick <- function(values) sample(values, length(iso), replace = TRUE)
  settings <- data.frame(
    isolate = iso,
    identity = pick(c(0.9, 0.92, 0.95, 0.97, 0.99)),
    coverage = pick(c(0.8, 0.85, 0.9, 0.95)),
    pymlst = pick(c("2.1.5", "2.1.6", "2.2.0", "2.2.2")),
    blat = pick(c("35", "36", "37x1")),
    scheme_version = pick(c("0.8", "0.9", "1.0")),
    db = pick(c("2023-04-17.1", "2023-11-15.1", "2024-01-31.1", "2024-07-22.1")),
    organism = pick(c("Testus_organismus", NA))
  )
  with_db(path, function(con) {
    dbWriteTable(con, "settings_map", settings, temporary = TRUE)
    lookup <- function(col, table) {
      sprintf("(SELECT %s FROM settings_map m WHERE m.isolate = %s.isolate)", col, table)
    }
    dbExecute(con, sprintf(
      "UPDATE typing_provenance SET cg_identity = %s, cg_coverage = %s, pymlst_version = %s,
         blat_version = %s, cg_scheme_version = %s, amr_amrfinder_db_version = %s,
         amr_organism = %s",
      lookup("identity", "typing_provenance"), lookup("coverage", "typing_provenance"),
      lookup("pymlst", "typing_provenance"), lookup("blat", "typing_provenance"),
      lookup("scheme_version", "typing_provenance"), lookup("db", "typing_provenance"),
      lookup("organism", "typing_provenance")
    ))
    dbExecute(con, sprintf(
      "UPDATE amr_results SET amrfinder_db_version = %s, organism = %s,
         point_mutations = CASE WHEN %s IS NULL THEN 0 ELSE 1 END",
      lookup("db", "amr_results"), lookup("organism", "amr_results"),
      lookup("organism", "amr_results")
    ))
  })
}

# One locus with a distinct allele in every isolate and many loci with over a
# hundred alleles: a long virtual allele list and the top bins of the charts.
build_allele_rich <- function(path, n) {
  suppressMessages(mock_database(path, n_isolates = n, n_loci = 40))
  with_db(path, function(con) {
    calls <- dbGetQuery(
      con,
      "SELECT m.id, m.gene, s.sequence FROM mlst m JOIN sequences s ON s.id = m.seqid
        WHERE m.souche != 'ref' AND m.gene IN
          (SELECT DISTINCT gene FROM mlst ORDER BY gene LIMIT 12)"
    )
    # The first locus gets a distinct allele per isolate, the others one for
    # every second isolate.
    first <- calls$gene == min(calls$gene)
    change <- first | runif(nrow(calls)) < 0.5
    calls <- calls[change, , drop = FALSE]
    pos <- sample(nchar(calls$sequence[1]) - 1L, nrow(calls), replace = TRUE)
    bases <- c("A", "C", "G", "T")
    calls$sequence <- paste0(
      substr(calls$sequence, 1, pos - 1L),
      sample(bases, nrow(calls), replace = TRUE),
      sample(bases, nrow(calls), replace = TRUE),
      substr(calls$sequence, pos + 2L, nchar(calls$sequence))
    )
    known <- dbGetQuery(con, "SELECT id, sequence FROM sequences")
    fresh <- unique(calls$sequence[!(calls$sequence %in% known$sequence)])
    next_id <- max(known$id) + seq_along(fresh)
    dbAppendTable(con, "sequences", data.frame(id = next_id, sequence = fresh))
    ids <- c(known$id, next_id)[match(calls$sequence, c(known$sequence, fresh))]
    dbWriteTable(con, "seq_map", data.frame(id = calls$id, seqid = ids), temporary = TRUE)
    dbExecute(
      con,
      "UPDATE mlst SET seqid = (SELECT seqid FROM seq_map WHERE seq_map.id = mlst.id)
        WHERE id IN (SELECT id FROM seq_map)"
    )
  })
  hash_database(path)
}

# Dozens of AMR elements per isolate across many classes: the "Over 20" bin,
# folded group lists, very long profiles and a long element table.
build_amr_heavy <- function(path, n) {
  suppressMessages(mock_database(path, n_isolates = n, n_loci = 60))
  classes <- c(
    "AMINOGLYCOSIDE", "BETA-LACTAM", "QUINOLONE", "TETRACYCLINE", "MACROLIDE",
    "PHENICOL", "SULFONAMIDE", "TRIMETHOPRIM", "COLISTIN", "FOSFOMYCIN", "GLYCOPEPTIDE",
    "LINCOSAMIDE", "STREPTOGRAMIN", "RIFAMYCIN", "MUPIROCIN", "FUSIDIC_ACID", "BLEOMYCIN",
    "NITROFURAN", "OXAZOLIDINONE", "PLEUROMUTILIN"
  )
  with_db(path, function(con) {
    iso <- dbGetQuery(con, "SELECT DISTINCT isolate FROM amr_results")$isolate
    template <- dbGetQuery(con, "SELECT * FROM amr_results LIMIT 1")
    rows <- do.call(rbind, lapply(iso, function(i) {
      k <- sample(20:45, 1)
      genes <- sample(60, k)
      type <- ifelse(genes <= 40, "AMR", ifelse(genes <= 50, "VIRULENCE", "STRESS"))
      data.frame(
        isolate = i,
        gene_symbol = sprintf("syn%s%02d", substr(type, 1, 1), genes),
        sequence_name = sprintf("synthetic element %02d", genes),
        element_type = type,
        element_subtype = ifelse(type == "AMR", "AMR", ifelse(type == "STRESS", "METAL", type)),
        class = ifelse(type == "VIRULENCE", NA, classes[(genes %% length(classes)) + 1L]),
        subclass = NA,
        method = sample(c("EXACTX", "ALLELEX", "BLASTX", "PARTIALX"), k, TRUE, c(5, 2, 2, 1)),
        pct_coverage = 100,
        pct_identity = round(runif(k, 95, 100), 2),
        contig = sprintf("contig_%d", sample(1:60, k, TRUE)),
        start = sample(1000:900000, k),
        stop = NA_integer_,
        strand = "+",
        ref_accession = sprintf("WP_%09d.1", genes),
        ref_name = sprintf("synthetic reference %02d", genes),
        organism = template$organism,
        point_mutations = template$point_mutations,
        abritamr_version = template$abritamr_version,
        amrfinder_version = template$amrfinder_version,
        amrfinder_db_version = template$amrfinder_db_version,
        called_at = template$called_at
      )
    }))
    rows$stop <- rows$start + 900L
    dbAppendTable(con, "amr_results", rows)
  })
}

# Every analysis failing for every isolate, and every profile incomplete:
# danger and warning everywhere, charts with almost nothing to draw.
build_all_failed <- function(path, n) {
  suppressMessages(mock_database(path, n_isolates = n, n_loci = 80))
  with_db(path, function(con) {
    ids <- dbGetQuery(con, "SELECT id FROM mlst WHERE souche != 'ref'")$id
    dbWriteTable(
      con,
      "drop_ids",
      data.frame(id = sample(ids, round(length(ids) * 0.12))),
      temporary = TRUE
    )
    dbExecute(con, "DELETE FROM mlst WHERE id IN (SELECT id FROM drop_ids)")
    dbExecute(con, "DELETE FROM classical_mlst")
    dbExecute(con, "DELETE FROM amr_results")
    dbExecute(con, "DELETE FROM amr_summary")
    dbExecute(
      con,
      "UPDATE typing_provenance SET cla_status = 'failed', amr_status = 'failed',
         amr_elements = NULL"
    )
  })
}

# Thousands of isolates: load time, table scrolling and chart sizes at scale.
build_large <- function(path, n) {
  suppressMessages(mock_database(
    path,
    n_isolates = n,
    n_loci = 300,
    scenarios = c("mixed_releases", "low_completeness", "amr_mixed_db", "long_read_assemblies")
  ))
}

#' Edge-Case Databases
#'
#' @description Every edge case `write_edge_cases()` can build: a builder, the
#'   isolate count it uses by default, what it stresses and where to look.
#'
#' @export
EDGE_CASES <- list(
  all_checks = list(
    build = build_all_checks, n = 240,
    what = "Every slice scenario at once",
    look = paste(
      "Diagnostics tab of all four overviews: long Checks lists (scrolling, wrapping),",
      "several flags in one cell, several groups in every settings card."
    )
  ),
  long_names = list(
    build = build_long_names, n = 60,
    what = "Very long isolate, locus, element, class and scheme names",
    look = paste(
      "Every table (two-line clamp, hover for the full value), bar labels (ellipsis),",
      "the allele picker and AMR carrier search, Isolate Overview details, Scheme Releases."
    )
  ),
  special_characters = list(
    build = build_special_characters, n = 24,
    what = "Names with HTML, quotes, ampersands, spaces and non-ASCII characters",
    look = paste(
      "Names must read as typed everywhere - tables, hover titles, check texts,",
      "carrier search, detail panels - and nothing may render as HTML."
    )
  ),
  single_isolate = list(
    build = build_single_isolate, n = 1,
    what = "A single isolate",
    look = "Summaries and charts at 100%, diversity shown as '-', medians of one value."
  ),
  empty = list(
    build = build_empty, n = 0,
    what = "A scheme without any isolate",
    look = "Every card of all four overviews in its empty state; nothing errors."
  ),
  legacy = list(
    build = build_legacy, n = 60,
    what = "No provenance, assembly hash, classical MLST or AMR tables",
    look = paste(
      "MLST and AMR: 'no results' messages. Isolate and cgMLST: provenance and",
      "assembly columns empty, 'No provenance' checks."
    )
  ),
  many_releases = list(
    build = build_many_releases, n = 150,
    what = "Classical calls over 6 schemes, over a hundred releases and 2 repositories",
    look = "MLST Diagnostics: long Scheme Releases list, Mixed schemes check folding into 'N more'."
  ),
  many_settings = list(
    build = build_many_settings, n = 150,
    what = "Every isolate with its own mix of thresholds, tool and database versions",
    look = "cgMLST Typing Settings and AMR Screening Settings: dozens of groups, scrolling."
  ),
  allele_rich = list(
    build = build_allele_rich, n = 600,
    what = "A locus with an allele per isolate, many loci over a hundred alleles",
    look = paste(
      "cgMLST Loci Info: hundreds of alleles in the virtual picker (search, scroll),",
      "the folded frequency chart; Distribution: the top allele bins."
    )
  ),
  amr_heavy = list(
    build = build_amr_heavy, n = 120,
    what = "20-45 extra AMR elements per isolate over 20 drug classes",
    look = paste(
      "AMR Distribution: 'Over 20' bins, folded groups, very long profiles;",
      "Elements: a long table; Screening Summary figures."
    )
  ),
  all_failed = list(
    build = build_all_failed, n = 60,
    what = "Every classical call and AMR screen failed, every profile incomplete",
    look = "All four Diagnostics in red and yellow; MLST and AMR distributions empty."
  ),
  large = list(
    build = build_large, n = 2000,
    what = "2,000 isolates with 300 loci",
    look = "Load time of each overview, table scrolling, picker and search speed."
  )
)

#' Write the Edge-Case Databases
#'
#' @description Builds every requested edge case into `dir`, with a README.md
#'   checklist of what each stresses and where in the app to look.
#'
#' @param dir Character directory, created when missing.
#' @param cases Character names of `EDGE_CASES`.
#' @param seed Integer random seed, so the same call builds the same databases.
#' @param scale Numeric factor on each case's isolate count, e.g. `0.1` for a
#'   quick build; a case keeps at least its own minimum.
#' @param overwrite Logical; replace existing databases.
#'
#' @return Invisibly, a data frame of the databases written.
#' @export
write_edge_cases <- function(
  dir,
  cases = names(EDGE_CASES),
  seed = 1L,
  scale = 1,
  overwrite = TRUE
) {
  unknown <- setdiff(cases, names(EDGE_CASES))
  if (length(unknown)) {
    stop("Unknown edge case(s): ", paste(unknown, collapse = ", "))
  }
  dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  if (exists(".Random.seed", envir = globalenv())) {
    previous <- get(".Random.seed", envir = globalenv())
    on.exit(assign(".Random.seed", previous, envir = globalenv()), add = TRUE)
  }

  rows <- lapply(cases, function(name) {
    case <- EDGE_CASES[[name]]
    path <- file.path(dir, paste0("edge_", name, ".db"))
    if (file.exists(path)) {
      if (!isTRUE(overwrite)) {
        stop("File exists: ", path)
      }
      unlink(paste0(path, c("", "-wal", "-shm")))
    }
    set.seed(seed)
    n <- if (case$n <= 2) case$n else max(10L, as.integer(round(case$n * scale)))
    case$build(path, max(n, 2L))
    message("Edge case written: ", basename(path))
    data.frame(file = basename(path), what = case$what, look = case$look)
  })
  out <- do.call(rbind, rows)

  writeLines(
    c(
      "# Overview edge cases",
      "",
      "Databases built by `dev/mock_edge_cases.R` to check the Database overviews",
      "(Isolate, cgMLST, MLST, AMR) against UI-stressing data. Open each from the",
      "landing page and go through what it lists.",
      "",
      "| Database | Stresses | Where to look |",
      "|---|---|---|",
      sprintf("| `%s` | %s | %s |", out$file, out$what, out$look)
    ),
    file.path(dir, "README.md")
  )
  invisible(out)
}
