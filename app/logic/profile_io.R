# app/logic/profile_io.R
#
# Read and write cgMLST *profile tables* — the lingua franca of allelic typing.
#
# Every relevant tool agrees on the shape: one row per isolate, one column per
# locus, an allele identifier in each cell. They disagree only on the ID column
# header and the sentinel for a missing call, which is what `PROFILE_PRESETS`
# captures. (pyMLST is the exception: its default form transposes the table.)
#
# Two identifier kinds:
#
#   "hash"  sha256 of the allele's DNA sequence. The only *portable* identity —
#           the same allele hashes the same anywhere. This is what chewBBACA's
#           `--hash-profiles` emits, and what an interoperable export should use.
#
#   "index" the integer `sequences.id`. Note this is pyMLST's GLOBAL sequence id,
#           NOT a per-locus allele number: locus PA5568 may carry alleles 3868
#           and 2 while PA5569 carries 1. Conventional cgMLST numbering (SeqSphere,
#           cgMLST.org, chewBBACA) restarts at 1 for every locus, so our integers
#           are NOT comparable with theirs. They are only meaningful inside one
#           database lineage — which is exactly what makes them verifiable on
#           import (see db_staging.R).

box::use(
  DBI[dbDisconnect, dbGetQuery, dbListTables, dbWriteTable],
  openssl[sha256],
  openxlsx[
    addWorksheet,
    createWorkbook,
    freezePane,
    getSheetNames,
    read.xlsx,
    saveWorkbook,
    writeData,
  ],
  rlang[`%||%`],
  stats[setNames],
  tools[file_ext, file_path_sans_ext],
  utils[read.table],
  zip[zip],
)

box::use(
  app / logic / db_compat[REF_SOUCHE, connect_ro],
)

#' Normalize Locus Names Across Formats
#'
#' Replaces hyphens and underscores with uniform hyphens (e.g., converts "PA0195_1" to "PA0195-1")[cite: 12].
#'
#' @param x Vector of locus/gene identifier strings[cite: 12].
#' @return Vector with hyphens and underscores replaced by hyphens[cite: 12].
#' @export
norm_locus <- function(x) gsub("[-_]", "-", x)

# Dialects of the same table. `missing` is what we WRITE for an absent call;
# the parser accepts every sentinel in MISSING_TOKENS regardless of preset.
#' Format specification presets for external typing export dialects[cite: 12].
#' @export
PROFILE_PRESETS <- list(
  phylotrace = list(
    label = "PhyloTrace",
    id_header = "isolate",
    missing = "",
    loci_as_rows = FALSE
  ),
  grapetree = list(
    label = "GrapeTree",
    id_header = "#Name",
    missing = "0",
    loci_as_rows = FALSE
  ),
  chewbbaca = list(
    label = "chewBBACA / cgmlst-dists",
    id_header = "FILE",
    missing = "LNF",
    loci_as_rows = FALSE
  ),
  pymlst = list(
    label = "pyMLST (loci as rows)",
    id_header = "#GeneId",
    missing = "",
    loci_as_rows = TRUE
  )
)

# Everything the ecosystem uses to say "no call". chewBBACA's codes describe *why*
# the call failed; for a distance matrix the reason is irrelevant — all of them
# mean "unknown allele". "0"/"-1"/"-" cover GrapeTree and pyMLST's grapetree form.
#' Vector of standard missing-call missing tokens across external tools[cite: 12].
#' @export
MISSING_TOKENS <- c(
  "",
  "0",
  "-",
  "-1",
  "NA",
  "N",
  "LNF",
  "PLNF",
  "PLOT",
  "PLOT3",
  "PLOT5",
  "LOTSC",
  "NIPH",
  "NIPHEM",
  "PAMA",
  "ASM",
  "ALM"
)

# ---------------------------------------------------------------------------
# Reading the database
# ---------------------------------------------------------------------------

#' Retrieve Loci Order Defined by Reference Genome
#'
#' Returns all loci in the scheme in reference genome sequence order[cite: 12].
#'
#' @param db_path File path to SQLite database[cite: 12].
#' @return Vector of gene identifiers[cite: 12].
#' @export
scheme_loci <- function(db_path) {
  con <- connect_ro(db_path)
  on.exit(dbDisconnect(con), add = TRUE)

  dbGetQuery(
    con,
    "SELECT gene FROM mlst WHERE souche = ? ORDER BY id",
    params = list(REF_SOUCHE)
  )$gene
}

#' Construct Wide Profile Matrix Table for Selected Isolates
#'
#' Fetches allele profile identifiers for specified isolates across all scheme loci[cite: 12].
#' Missing calls are populated as NA[cite: 12].
#'
#' @param db_path File path to SQLite database[cite: 12].
#' @param isolates Vector of isolate identifiers[cite: 12].
#' @param value_kind Identifier type: "hash" (sha256) or "index" (local sequence id)[cite: 12].
#' @return Data frame with `isolate` column followed by scheme locus columns[cite: 12].
#' @export
build_profile_table <- function(db_path, isolates, value_kind = "hash") {
  value_kind <- match.arg(value_kind, c("hash", "index"))

  con <- connect_ro(db_path)
  on.exit(dbDisconnect(con), add = TRUE)

  isolates <- setdiff(unique(isolates), REF_SOUCHE)
  loci <- scheme_loci(db_path)

  empty <- data.frame(isolate = character(0), stringsAsFactors = FALSE)
  for (locus in loci) {
    empty[[locus]] <- character(0)
  }
  if (!length(isolates) || !length(loci)) {
    return(empty)
  }

  write_selection(con, isolates)

  # The isolate list can exceed SQLite's bound-parameter limit, so the selection
  # is joined in as a temp table (the idiom db_export.R already uses). Temp
  # tables work fine on a read-only connection.
  if (identical(value_kind, "hash")) {
    long <- dbGetQuery(
      con,
      sprintf(
        "SELECT m.souche AS isolate, m.gene AS gene, m.seqid AS seqid, %s AS value
           FROM mlst m
           JOIN sel ON sel.isolate = m.souche
           %s",
        if (.has_hashes(con)) "h.hash" else "NULL",
        if (.has_hashes(con)) "LEFT JOIN hashes h ON h.id = m.seqid" else ""
      )
    )
    long$value <- fill_missing_hashes(con, long$seqid, long$value)
  } else {
    long <- dbGetQuery(
      con,
      "SELECT m.souche AS isolate, m.gene AS gene, CAST(m.seqid AS TEXT) AS value
         FROM mlst m
         JOIN sel ON sel.isolate = m.souche"
    )
  }

  long_to_wide(long, isolates, loci)
}

# TRUE when the database has a `hashes` table.
.has_hashes <- function(con) "hashes" %in% dbListTables(con)

# Typing adds sequences without hashing them; `hashes` catches up only when the
# database is next loaded. Computes the missing ones (`NA` in `hash`) from the
# sequences instead, so an export never comes out with blank calls.
fill_missing_hashes <- function(con, seqid, hash) {
  hash <- as.character(hash)
  missing <- unique(seqid[is.na(hash)])
  if (!length(missing)) {
    return(hash)
  }
  dbWriteTable(
    con,
    "missing_ids",
    data.frame(id = missing),
    temporary = TRUE,
    overwrite = TRUE
  )
  seqs <- dbGetQuery(
    con,
    "SELECT s.id AS id, s.sequence AS sequence
       FROM sequences s JOIN missing_ids m ON m.id = s.id
      WHERE s.sequence IS NOT NULL"
  )
  if (nrow(seqs)) {
    fill <- is.na(hash)
    hash[fill] <- as.character(sha256(seqs$sequence))[match(seqid[fill], seqs$id)]
  }
  hash
}

# Write isolate list into temporary selection table to prevent SQLite parameter caps
write_selection <- function(con, isolates) {
  dbWriteTable(
    con,
    "sel",
    data.frame(isolate = isolates, stringsAsFactors = FALSE),
    temporary = TRUE,
    overwrite = TRUE
  )
}

#' Pivot Long-Format Profile Query Frame to Fixed-Width Grid
#'
#' Reshapes database long profile table to wide format aligned against loci and isolates[cite: 12].
#'
#' @param long Data frame with columns `isolate`, `gene`, `value`[cite: 12].
#' @param isolates Target isolate vector[cite: 12].
#' @param loci Target locus vector[cite: 12].
#' @return Reshaped wide data frame[cite: 12].
#' @export
long_to_wide <- function(long, isolates, loci) {
  mat <- matrix(
    NA_character_,
    nrow = length(isolates),
    ncol = length(loci),
    dimnames = list(isolates, loci)
  )

  keep <- long$isolate %in% isolates & long$gene %in% loci
  long <- long[keep, , drop = FALSE]
  if (nrow(long)) {
    idx <- cbind(
      match(long$isolate, isolates),
      match(long$gene, loci)
    )
    mat[idx] <- as.character(long$value)
  }

  out <- data.frame(isolate = isolates, stringsAsFactors = FALSE)
  cbind(out, as.data.frame(mat, stringsAsFactors = FALSE), row.names = NULL)
}

#' Extract Allele Sequences as FASTA Header Vector
#'
#' Queries distinct allele sequences for isolates using portable `>locus|sha256` headers[cite: 12].
#'
#' @param db_path File path to SQLite database[cite: 12].
#' @param isolates Vector of isolate identifiers[cite: 12].
#' @return Character vector of FASTA entries[cite: 12].
#' @export
allele_fasta <- function(db_path, isolates) {
  con <- connect_ro(db_path)
  on.exit(dbDisconnect(con), add = TRUE)

  isolates <- setdiff(unique(isolates), REF_SOUCHE)
  if (!length(isolates)) {
    return(character(0))
  }

  write_selection(con, isolates)

  res <- dbGetQuery(
    con,
    sprintf(
      "SELECT DISTINCT m.gene AS gene, %s AS hash, s.sequence AS sequence
         FROM mlst m
         JOIN sel ON sel.isolate = m.souche
         JOIN sequences s ON s.id = m.seqid
         %s",
      if (.has_hashes(con)) "h.hash" else "NULL",
      if (.has_hashes(con)) "LEFT JOIN hashes h ON h.id = m.seqid" else ""
    )
  )

  if (!nrow(res)) {
    return(character(0))
  }
  # Hashed straight from the sequence that is already in hand.
  miss <- is.na(res$hash)
  res$hash[miss] <- as.character(sha256(res$sequence[miss]))
  res <- res[order(res$gene, res$hash), , drop = FALSE]

  paste0(">", res$gene, "|", res$hash, "\n", res$sequence)
}

#' Extract Allele FASTA Records Grouped per Locus
#'
#' Organizes FASTA allele sequences into a named list split by locus[cite: 12].
#'
#' @param db_path File path to SQLite database[cite: 12].
#' @param isolates Vector of isolate identifiers[cite: 12].
#' @return Named list of locus -> FASTA character strings[cite: 12].
#' @export
allele_fasta_per_locus <- function(db_path, isolates) {
  records <- allele_fasta(db_path, isolates)
  if (!length(records)) {
    return(list())
  }
  gene <- sub("^>([^|]*)\\|.*$", "\\1", sub("\n.*$", "", records))
  split(records, gene)
}

# ---------------------------------------------------------------------------
# Formatting
# ---------------------------------------------------------------------------

#' Format Profile Grid into Tool Specification Preset
#'
#' Transforms wide profile matrix into target tool export format and handles transposing[cite: 12].
#'
#' @param profile Wide profile data frame[cite: 12].
#' @param preset Target scheme dialect preset name[cite: 12].
#' @return Formatted character data frame[cite: 12].
#' @export
format_profile <- function(profile, preset = "phylotrace") {
  spec <- PROFILE_PRESETS[[preset]]
  if (is.null(spec)) {
    stop("Unknown profile preset: ", preset)
  }

  isolates <- profile$isolate
  body <- profile[, setdiff(names(profile), "isolate"), drop = FALSE]
  body[] <- lapply(body, function(x) {
    x <- as.character(x)
    x[is.na(x)] <- spec$missing
    x
  })

  if (isTRUE(spec$loci_as_rows)) {
    t_body <- as.data.frame(t(as.matrix(body)), stringsAsFactors = FALSE)
    names(t_body) <- isolates
    out <- cbind(
      setNames(
        data.frame(names(body), stringsAsFactors = FALSE),
        spec$id_header
      ),
      t_body
    )
    return(out)
  }

  cbind(
    setNames(data.frame(isolates, stringsAsFactors = FALSE), spec$id_header),
    body
  )
}

# A cell a spreadsheet would run as a formula (CSV injection), unless it is
# plainly a number such as "-12.5".
.FORMULA_RE <- "^[=+@\t\r-]"
.NUMBER_RE <- "^[+-]?([0-9]+[.]?[0-9]*|[.][0-9]+)([eE][+-]?[0-9]+)?$"

#' Make Cells Safe to Open in a Spreadsheet
#'
#' Prefixes a single quote to every cell starting with `=`, `+`, `-`, `@`, a
#' tab or a carriage return that is not a plain number, so a spreadsheet shows
#' it as text instead of running it as a formula. `unguard_cells()` undoes it.
#'
#' @param x Character vector.
#' @return `x`, guarded.
#' @export
guard_cells <- function(x) {
  hit <- !is.na(x) & grepl(.FORMULA_RE, x) & !grepl(.NUMBER_RE, x)
  x[hit] <- paste0("'", x[hit])
  x
}

#' Undo `guard_cells()`
#'
#' Strips the single quote `guard_cells()` put in front of a cell, so a table
#' PhyloTrace exported reads back with the values it was written from.
#'
#' @param x Character vector.
#' @return `x` without the guard quote.
#' @export
unguard_cells <- function(x) {
  hit <- !is.na(x) & grepl("^'[=+@\t\r-]", x)
  x[hit] <- substring(x[hit], 2L)
  x
}

# Quotes a field when it holds the separator, a quote or a line break (RFC
# 4180), doubling embedded quotes; everything else is written bare so tools
# that do not understand quoting still read a plain profile.
.quote_field <- function(x, sep) {
  x[is.na(x)] <- ""
  needs <- grepl(sep, x, fixed = TRUE) | grepl("[\"\r\n]", x)
  x[needs] <- paste0('"', gsub('"', '""', x[needs], fixed = TRUE), '"')
  x
}

#' Write Delimited Data Frame File
#'
#' Writes a data frame as UTF-8 text. A field is quoted only where it has to
#' be (it holds the separator, a quote or a line break), and cells a
#' spreadsheet would run as a formula are guarded with `guard_cells()`.
#'
#' @param df Target data frame.
#' @param path Destination file path.
#' @param sep Field separator character.
#' @return `path`, invisibly.
#' @export
write_delim_table <- function(df, path, sep = "\t") {
  cols <- lapply(df, function(x) .quote_field(guard_cells(as.character(x)), sep))
  header <- paste(.quote_field(guard_cells(names(df)), sep), collapse = sep)
  body <- if (nrow(df)) do.call(paste, c(unname(cols), sep = sep)) else character(0)
  con <- file(path, open = "wb")
  on.exit(close(con))
  writeLines(enc2utf8(c(header, body)), con, sep = "\n", useBytes = TRUE)
  invisible(path)
}

# Excel caps a sheet at 16,384 columns; a wgMLST scheme can exceed that.
XLSX_MAX_COLS <- 16384L

#' Export Multi-Sheet Data Frames to Excel Workbook
#'
#' Writes named data frames into tabs while enforcing Excel column maximum limits[cite: 12].
#'
#' @param sheets Named list of data frames[cite: 12].
#' @param path Destination xlsx file path[cite: 12].
#' @return Invalidation path invisibly[cite: 12].
#' @export
write_xlsx_sheets <- function(sheets, path) {
  wide <- vapply(sheets, ncol, integer(1))
  if (any(wide > XLSX_MAX_COLS)) {
    stop(
      "This scheme has ",
      max(wide),
      " columns, over Excel's limit of ",
      XLSX_MAX_COLS,
      ". Export as TSV or CSV instead."
    )
  }

  wb <- createWorkbook()
  for (nm in names(sheets)) {
    addWorksheet(wb, nm)
    writeData(wb, nm, sheets[[nm]])
    freezePane(wb, nm, firstRow = TRUE, firstCol = TRUE)
  }
  saveWorkbook(wb, path, overwrite = TRUE)
  invisible(path)
}

# ---------------------------------------------------------------------------
# Export orchestration
# ---------------------------------------------------------------------------

#' Fast Typing Profile Summary Statistics Calculation
#'
#' Calculates typing call counts and missing space metrics without building profile matrix[cite: 12].
#'
#' @param db_path File path to SQLite database[cite: 12].
#' @param isolates Selected isolate identifier vector[cite: 12].
#' @return List of typing call summary integers[cite: 12].
#' @export
typing_preview <- function(db_path, isolates) {
  con <- connect_ro(db_path)
  on.exit(dbDisconnect(con), add = TRUE)

  isolates <- setdiff(unique(isolates), REF_SOUCHE)
  n_loci <- length(scheme_loci(db_path))

  if (!length(isolates)) {
    return(list(
      n_isolates = 0L,
      n_loci = n_loci,
      n_alleles = 0L,
      n_calls = 0L,
      n_cells = 0L,
      n_missing = 0L
    ))
  }

  write_selection(con, isolates)
  counts <- dbGetQuery(
    con,
    "SELECT COUNT(*) AS calls, COUNT(DISTINCT m.seqid) AS alleles
       FROM mlst m JOIN sel ON sel.isolate = m.souche"
  )

  cells <- length(isolates) * n_loci
  list(
    n_isolates = length(isolates),
    n_loci = n_loci,
    n_alleles = as.integer(counts$alleles),
    n_calls = as.integer(counts$calls),
    n_cells = cells,
    n_missing = cells - as.integer(counts$calls)
  )
}

#' Resolve Target Export Filenames and Archival Bundle Strategy
#'
#' Determines output paths and whether multiple components require zip packaging[cite: 12].
#'
#' @param dest_path Base destination file path[cite: 12].
#' @param format File format ("xlsx", "tsv", or "csv")[cite: 12].
#' @param include_metadata Boolean flag for metadata inclusion[cite: 12].
#' @param sequences FASTA output structure ("none", "fasta", or "per_locus")[cite: 12].
#' @return Named list describing path structure, packaging mode, and included parts[cite: 12].
#' @export
typing_export_target <- function(
  dest_path,
  format = "xlsx",
  include_metadata = TRUE,
  sequences = "none"
) {
  parts <- c(
    "profile",
    if (isTRUE(include_metadata)) "metadata",
    if (!identical(sequences, "none")) "sequences"
  )

  # An Excel workbook holds the two tables as sheets, so only sequences force a
  # bundle. A delimited export needs one file per table.
  bundled <- if (identical(format, "xlsx")) {
    !identical(sequences, "none")
  } else {
    length(parts) > 1L
  }

  ext <- if (bundled) {
    "zip"
  } else if (identical(format, "xlsx")) {
    "xlsx"
  } else {
    format
  }

  list(
    path = replace_ext(dest_path, ext),
    bundled = bundled,
    ext = ext,
    parts = parts
  )
}

# Extensions replace_ext() swaps out; any other dotted ending is part of the name.
EXPORT_EXTENSIONS <- c("db", "xlsx", "xls", "tsv", "csv", "tab", "txt", "zip")

#' Replace Extension of Output File Path
#'
#' Swaps a known export extension for `ext`, or appends `ext` when the name
#' ends in anything else, so a dotted name such as "run.2026.11" keeps its
#' last part.
#'
#' @param path Original path string.
#' @param ext Replacement extension string.
#' @return String with modified extension.
#' @export
replace_ext <- function(path, ext) {
  known <- paste0("\\.(", paste(EXPORT_EXTENSIONS, collapse = "|"), ")$")
  paste0(sub(known, "", path, ignore.case = TRUE), ".", ext)
}

#' Execute Typing Profile Export Pipeline
#'
#' Assembles profile, metadata, and FASTA files into staging directory and packages to destination[cite: 12].
#'
#' @param db_path Path to SQLite database[cite: 12].
#' @param dest_path Destination file path[cite: 12].
#' @param isolates Target isolate identifiers[cite: 12].
#' @param metadata Optional metadata data frame[cite: 12].
#' @param format Export format ("xlsx", "tsv", "csv")[cite: 12].
#' @param value_kind Identifier mode ("hash" or "index")[cite: 12].
#' @param preset Format specification preset name[cite: 12].
#' @param sequences FASTA output layout ("none", "fasta", "per_locus")[cite: 12].
#' @param progress Progress callback function[cite: 12].
#' @return Summary list detailing exported archive characteristics[cite: 12].
#' @export
export_typing_results <- function(
  db_path,
  dest_path,
  isolates,
  metadata = NULL,
  format = "xlsx",
  value_kind = "hash",
  preset = "phylotrace",
  sequences = "none",
  progress = NULL
) {
  progress <- progress %||% function(frac, msg) invisible(NULL)

  isolates <- setdiff(unique(isolates), REF_SOUCHE)
  if (!length(isolates)) {
    stop("Select at least one isolate to export.")
  }

  target <- typing_export_target(
    dest_path,
    format,
    !is.null(metadata),
    sequences
  )
  if (dir.exists(target$path)) {
    stop("The export target is a folder. Choose a file name.")
  }

  progress(0.1, "Building profile table …")
  profile <- build_profile_table(db_path, isolates, value_kind)
  formatted <- format_profile(profile, preset)

  # Assemble into a staging directory of this export's own, then either move
  # the single file out or zip the directory. Nothing lands at `dest_path`
  # until it is complete.
  stage <- tempfile("phylotrace_export_")
  dir.create(stage, recursive = TRUE)
  on.exit(unlink(stage, recursive = TRUE), add = TRUE)

  base <- file_path_sans_ext(basename(target$path))
  written <- character(0)

  if (identical(format, "xlsx")) {
    progress(0.4, "Writing workbook …")
    sheets <- list(profile = formatted)
    if (!is.null(metadata)) {
      sheets$metadata <- metadata
    }
    f <- file.path(stage, paste0(base, ".xlsx"))
    write_xlsx_sheets(sheets, f)
    written <- c(written, f)
  } else {
    sep <- if (identical(format, "csv")) "," else "\t"
    progress(0.4, "Writing profile table …")
    f <- file.path(stage, paste0("profile.", format))
    write_delim_table(formatted, f, sep)
    written <- c(written, f)

    if (!is.null(metadata)) {
      f <- file.path(stage, paste0("metadata.", format))
      write_delim_table(metadata, f, sep)
      written <- c(written, f)
    }
  }

  if (identical(sequences, "fasta")) {
    progress(0.7, "Writing allele sequences …")
    f <- file.path(stage, "alleles.fasta")
    writeLines(allele_fasta(db_path, isolates), f)
    written <- c(written, f)
  } else if (identical(sequences, "per_locus")) {
    progress(0.7, "Writing per-locus sequences …")
    per <- allele_fasta_per_locus(db_path, isolates)
    dir.create(file.path(stage, "alleles"))
    for (locus in names(per)) {
      writeLines(
        per[[locus]],
        file.path(stage, "alleles", paste0(locus, ".fasta"))
      )
    }
    written <- c(written, file.path(stage, "alleles"))
  }

  progress(0.9, "Finalising …")

  # Written next to the target as `.part` and renamed over it, so an existing
  # file is only replaced by a complete one.
  part <- file.path(
    normalizePath(dirname(target$path), mustWork = FALSE),
    paste0(basename(target$path), ".part")
  )
  unlink(part)
  on.exit(unlink(part), add = TRUE)
  if (target$bundled) {
    zip(
      zipfile = part,
      files = basename(written),
      root = stage,
      mode = "cherry-pick"
    )
  } else if (!file.copy(written[[1]], part)) {
    stop("Could not write to ", target$path)
  }
  if (!file.rename(part, target$path)) {
    stop("Could not write to ", target$path)
  }

  progress(1, "Done")

  list(
    path = target$path,
    n_isolates = length(isolates),
    n_loci = ncol(profile) - 1L,
    value_kind = value_kind,
    preset = preset,
    bundled = target$bundled,
    bytes = file.size(target$path)
  )
}

# ---------------------------------------------------------------------------
# Parsing (import side)
# ---------------------------------------------------------------------------

#' Clean and Normalize Allele Cell Values
#'
#' Strips whitespace and prefixes (e.g. chewBBACA `INF-`) and converts tokens to NA[cite: 12].
#'
#' @param x Vector of raw string values[cite: 12].
#' @return Vector of cleaned allele identifiers with NAs[cite: 12].
#' @export
clean_cell <- function(x) {
  x <- trimws(as.character(x))
  x <- sub("^INF-", "", x)
  x[x %in% MISSING_TOKENS] <- NA_character_
  x
}

#' Classify Allele Identifier Kind
#'
#' Evaluates whether allele values represent SHA256 hashes, integers, or mixed tokens[cite: 12].
#'
#' @param values Vector of non-missing allele identifier strings[cite: 12].
#' @return Kind string: "empty", "hash", "index", "hash_other", or "mixed"[cite: 12].
#' @export
detect_value_kind <- function(values) {
  v <- values[!is.na(values)]
  if (!length(v)) {
    return("empty")
  }
  if (all(grepl("^[0-9a-fA-F]{64}$", v))) {
    return("hash")
  }
  if (all(grepl("^[0-9]+$", v))) {
    return("index")
  }
  if (all(grepl("^[0-9a-fA-F]+$", v))) {
    return("hash_other")
  }
  "mixed"
}

#' Parse External Profile File into Long Format Table
#'
#' Auto-detects layout structure (loci as columns/rows) and standardizes profile entries[cite: 12].
#'
#' @param path Target input profile file path[cite: 12].
#' @param loci Known scheme locus names for alignment[cite: 12].
#' @return List describing parsed long data frame, value kinds, and locus stats[cite: 12];
#'   `duplicate_isolates` names isolates the table lists more than once.
#' @export
parse_profile_file <- function(path, loci) {
  raw <- read_table_any(path)

  if (ncol(raw) < 2L || nrow(raw) < 1L) {
    stop("That file does not look like a profile table (too few rows/columns).")
  }

  norm_scheme <- norm_locus(loci)

  header_hits <- sum(norm_locus(names(raw)[-1]) %in% norm_scheme)
  col1_hits <- sum(norm_locus(as.character(raw[[1]])) %in% norm_scheme)

  loci_as_rows <- col1_hits > header_hits
  if (max(header_hits, col1_hits) == 0L) {
    stop(
      "None of the identifiers in that file match this scheme's loci. ",
      "Is it a profile table for the same scheme?"
    )
  }

  if (loci_as_rows) {
    gene <- as.character(raw[[1]])
    body <- raw[, -1, drop = FALSE]
    isolates <- names(body)
    long <- data.frame(
      isolate = rep(isolates, each = nrow(body)),
      gene = rep(gene, times = ncol(body)),
      value = unlist(lapply(body, as.character), use.names = FALSE),
      stringsAsFactors = FALSE
    )
  } else {
    isolates <- as.character(raw[[1]])
    body <- raw[, -1, drop = FALSE]
    gene <- names(body)
    long <- data.frame(
      isolate = rep(isolates, times = ncol(body)),
      gene = rep(gene, each = nrow(body)),
      value = unlist(lapply(body, as.character), use.names = FALSE),
      stringsAsFactors = FALSE
    )
  }

  long$gene <- norm_locus(long$gene)
  long$value <- clean_cell(long$value)

  n_missing <- sum(is.na(long$value))
  long <- long[!is.na(long$value), , drop = FALSE]

  loci_seen <- intersect(unique(long$gene), norm_scheme)
  loci_unknown <- setdiff(unique(long$gene), norm_scheme)
  long <- long[long$gene %in% norm_scheme, , drop = FALSE]

  list(
    long = long,
    value_kind = detect_value_kind(long$value),
    duplicate_isolates = unique(isolates[duplicated(isolates)]),
    loci_as_rows = loci_as_rows,
    isolates = unique(long$isolate),
    loci_seen = loci_seen,
    loci_unknown = loci_unknown,
    n_missing = n_missing
  )
}

#' Generic Table Reader for TSV, CSV, and Excel Formats
#'
#' Sniffs table delimiter or loads Excel sheets while preserving character data types[cite: 12].
#'
#' @param path Target table path[cite: 12].
#' @param sheet Optional sheet identifier for Excel files[cite: 12].
#' @return Character data frame of target table[cite: 12].
#' @export
read_table_any <- function(path, sheet = NULL) {
  ext <- tolower(file_ext(path))

  if (ext %in% c("xlsx", "xlsm")) {
    sheets <- getSheetNames(path)
    pick <- sheet %||% sheets[[1]]
    if (!pick %in% sheets) {
      stop(
        "Sheet '",
        pick,
        "' not found. Sheets: ",
        paste(sheets, collapse = ", ")
      )
    }
    df <- read.xlsx(
      path,
      sheet = pick,
      colNames = TRUE,
      check.names = FALSE
    )
    df[] <- lapply(df, as.character)
    return(df)
  }

  lines <- read_text_lines(path)
  first <- lines[1]
  sep <- if (
    length(first) &&
      lengths(regmatches(first, gregexpr("\t", first))) >=
        lengths(regmatches(first, gregexpr(",", first)))
  ) {
    "\t"
  } else {
    ","
  }

  df <- read.table(
    text = lines,
    sep = sep,
    header = TRUE,
    quote = "\"",
    comment.char = "",
    check.names = FALSE,
    colClasses = "character",
    na.strings = character(0),
    encoding = "UTF-8"
  )
  names(df) <- unguard_cells(names(df))
  df[] <- lapply(df, unguard_cells)
  df
}

#' Read a Text File as UTF-8 Lines
#'
#' Reads the raw bytes, drops a UTF-8 byte order mark and line-end carriage
#' returns, and converts every line that is not valid UTF-8 from Windows-1252
#' (Latin-1 where a byte is undefined there). Nothing is dropped silently: a
#' file holding binary data is refused.
#'
#' @param path Path to the file.
#' @return Character vector of lines, marked UTF-8.
#' @export
read_text_lines <- function(path) {
  size <- file.size(path)
  if (is.na(size)) {
    stop("The file no longer exists.")
  }
  bytes <- readBin(path, "raw", n = size)
  if (length(bytes) >= 3L && identical(bytes[1:3], as.raw(c(0xef, 0xbb, 0xbf)))) {
    bytes <- bytes[-(1:3)]
  }
  if (any(bytes == as.raw(0L))) {
    stop("This is not a text table: the file contains binary data.")
  }
  if (!length(bytes)) {
    stop("The file is empty.")
  }
  lines <- strsplit(rawToChar(bytes), "\n", fixed = TRUE, useBytes = TRUE)[[1]]
  lines <- sub("\r$", "", lines, useBytes = TRUE)
  bad <- !validUTF8(lines)
  if (any(bad)) {
    converted <- iconv(lines[bad], from = "CP1252", to = "UTF-8")
    fallback <- is.na(converted)
    converted[fallback] <- iconv(lines[bad][fallback], from = "latin1", to = "UTF-8")
    lines[bad] <- converted
  }
  Encoding(lines) <- "UTF-8"
  lines
}

#' Read Input FASTA File into Data Frame
#'
#' Parses FASTA file records into a data frame with header and sequence columns[cite: 12].
#'
#' @param path Target FASTA file path[cite: 12].
#' @return Data frame with `header` and `sequence` columns[cite: 12].
#' @export
read_fasta <- function(path) {
  lines <- readLines(path, warn = FALSE)
  lines <- lines[nzchar(trimws(lines))]
  if (!length(lines)) {
    return(data.frame(
      header = character(0),
      sequence = character(0),
      stringsAsFactors = FALSE
    ))
  }

  is_hdr <- startsWith(lines, ">")
  if (!is_hdr[[1]]) {
    stop("Not a FASTA file: it does not begin with '>'.")
  }

  idx <- cumsum(is_hdr)
  headers <- sub("^>", "", lines[is_hdr])
  seqs <- vapply(
    split(lines[!is_hdr], idx[!is_hdr]),
    function(x) paste(x, collapse = ""),
    character(1)
  )

  # A record with a header but no sequence lines is dropped by `split`; realign
  # by name so headers and sequences cannot drift apart.
  out <- data.frame(
    header = headers,
    sequence = unname(seqs[as.character(seq_along(headers))]),
    stringsAsFactors = FALSE
  )
  out$sequence[is.na(out$sequence)] <- ""
  out[nzchar(out$sequence), , drop = FALSE]
}

#' Parse FASTA Header Annotations to Gene/Allele Pairs
#'
#' Extracts locus and allele identification details from standard FASTA headers[cite: 12].
#'
#' @param headers Character vector of FASTA line headers[cite: 12].
#' @param locus_hint Optional locus name override (e.g., from filename)[cite: 12].
#' @return Data frame containing normalized `gene` and `allele` columns[cite: 12].
#' @export
parse_fasta_headers <- function(headers, locus_hint = NULL) {
  h <- sub("\\s.*$", "", headers) # drop anything after the first space

  if (!is.null(locus_hint)) {
    return(data.frame(
      gene = locus_hint,
      allele = sub("^.*[|_]", "", h),
      stringsAsFactors = FALSE
    ))
  }

  piped <- grepl("|", h, fixed = TRUE)
  gene <- ifelse(piped, sub("\\|.*$", "", h), sub("_[^_]*$", "", h))
  allele <- ifelse(piped, sub("^[^|]*\\|", "", h), sub("^.*_", "", h))

  data.frame(
    gene = norm_locus(gene),
    allele = allele,
    stringsAsFactors = FALSE
  )
}
