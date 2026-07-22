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
  RSQLite[SQLite],
  DBI[dbConnect, dbDisconnect, dbGetQuery, dbListTables, dbWriteTable],
  stats[setNames],
  utils[read.table, write.table],
)

box::use(
  app / logic / db_compat[REF_SOUCHE, connect_ro],
)

`%||%` <- function(a, b) if (is.null(a)) b else a

# `targets` spells a locus "PA0195_1" where `mlst` spells it "PA0195-1".
#' @export
norm_locus <- function(x) gsub("[-_]", "-", x)

# Dialects of the same table. `missing` is what we WRITE for an absent call;
# the parser accepts every sentinel in MISSING_TOKENS regardless of preset.
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

#' The scheme's loci, in the order the reference genome defines them (which is
#' the order pyMLST emits). Every locus of the scheme, not merely those seen in
#' the selected isolates — a profile table is a fixed-width view of the scheme.
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

#' Wide profile table for `isolates`: an `isolate` column followed by one column
#' per locus of the scheme. Cells hold the allele identifier as a character
#' string, or NA where the isolate has no call for that locus.
#'
#' `value_kind` is "hash" (sha256, portable) or "index" (local `sequences.id`).
#' @export
build_profile_table <- function(db_path, isolates, value_kind = "hash") {
  value_kind <- match.arg(value_kind, c("hash", "index"))

  con <- connect_ro(db_path)
  on.exit(dbDisconnect(con), add = TRUE)

  if (identical(value_kind, "hash") && !"hashes" %in% dbListTables(con)) {
    stop("This database has no `hashes` table; reload it to hash the sequences.")
  }

  isolates <- setdiff(unique(isolates), REF_SOUCHE)
  loci <- scheme_loci(db_path)

  empty <- data.frame(isolate = character(0), stringsAsFactors = FALSE)
  for (locus in loci) empty[[locus]] <- character(0)
  if (!length(isolates) || !length(loci)) {
    return(empty)
  }

  write_selection(con, isolates)

  # The isolate list can exceed SQLite's bound-parameter limit, so the selection
  # is joined in as a temp table (the idiom db_export.R already uses). Temp
  # tables work fine on a read-only connection.
  sql <- if (identical(value_kind, "hash")) {
    "SELECT m.souche AS isolate, m.gene AS gene, h.hash AS value
       FROM mlst m
       JOIN sel ON sel.isolate = m.souche
       JOIN hashes h ON h.id = m.seqid"
  } else {
    "SELECT m.souche AS isolate, m.gene AS gene, CAST(m.seqid AS TEXT) AS value
       FROM mlst m
       JOIN sel ON sel.isolate = m.souche"
  }

  long_to_wide(dbGetQuery(con, sql), isolates, loci)
}

# Join-in selection: no SQLITE_MAX_VARIABLE_NUMBER ceiling, same query plan.
write_selection <- function(con, isolates) {
  dbWriteTable(
    con,
    "sel",
    data.frame(isolate = isolates, stringsAsFactors = FALSE),
    temporary = TRUE,
    overwrite = TRUE
  )
}

#' Pivot a long (isolate, gene, value) frame into the fixed-width table.
#' Implemented by indexing rather than `pivot_wider` so the column set is
#' exactly `loci` and the row order exactly `isolates`, even when a locus or an
#' isolate has no calls at all.
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

#' The distinct alleles used by `isolates`, as FASTA records.
#'
#' Header is `>{locus}|{sha256}` — pyMLST's `SequenceExtractor` convention
#' (`>gene|seqid strains`) but keyed on the portable identity, so the records
#' join back to a hash profile without any database-local state.
#'
#' Returns a character vector, one element per record (the idiom already used by
#' `locus_fasta()` in database_functions.R).
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
    "SELECT DISTINCT m.gene AS gene, h.hash AS hash, s.sequence AS sequence
       FROM mlst m
       JOIN sel ON sel.isolate = m.souche
       JOIN hashes h ON h.id = m.seqid
       JOIN sequences s ON s.id = m.seqid
      ORDER BY m.gene, h.hash"
  )

  if (!nrow(res)) {
    return(character(0))
  }

  paste0(">", res$gene, "|", res$hash, "\n", res$sequence)
}

#' The same alleles grouped per locus, for the "zip of per-locus FASTA" layout
#' (the chewBBACA / cgMLST.org schema convention). Returns a named list: locus ->
#' character vector of FASTA records.
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

#' Render the wide table into the character frame that gets written: missing
#' cells become the preset's sentinel and the ID column takes the preset's
#' header. `loci_as_rows` transposes (pyMLST's default form).
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

#' @export
write_delim_table <- function(df, path, sep = "\t") {
  write.table(
    df,
    path,
    sep = sep,
    quote = FALSE,
    row.names = FALSE,
    col.names = TRUE,
    na = ""
  )
  invisible(path)
}

# Excel caps a sheet at 16,384 columns; a wgMLST scheme can exceed that.
XLSX_MAX_COLS <- 16384L

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

  wb <- openxlsx::createWorkbook()
  for (nm in names(sheets)) {
    openxlsx::addWorksheet(wb, nm)
    openxlsx::writeData(wb, nm, sheets[[nm]])
    openxlsx::freezePane(wb, nm, firstRow = TRUE, firstCol = TRUE)
  }
  openxlsx::saveWorkbook(wb, path, overwrite = TRUE)
  invisible(path)
}

# ---------------------------------------------------------------------------
# Export orchestration
# ---------------------------------------------------------------------------

#' Counts for the Export panel, without building the (potentially 1.1 M cell)
#' table. Unlike the `.db` preview these exclude the synthetic `ref` souche,
#' which never appears in a profile table.
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

#' What file will `export_typing_results()` actually produce?
#'
#' The deliverable is always a single artefact: the bare table when that is all
#' there is, otherwise a zip bundling the pieces. Exposed separately so the UI
#' can show the destination before anything is written.
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

#' @export
replace_ext <- function(path, ext) {
  paste0(sub("\\.[A-Za-z0-9]{1,5}$", "", path), ".", ext)
}

#' Write the typing results for `isolates`.
#'
#' `metadata` is the already-subset metadata data.frame (the Export panel builds
#' it from its existing column picker), or NULL. Returns a description of what
#' was written.
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

  progress(0.1, "Building profile table …")
  profile <- build_profile_table(db_path, isolates, value_kind)
  formatted <- format_profile(profile, preset)

  # Assemble into a staging directory, then either move the single file out or
  # zip the directory. Nothing lands at `dest_path` until it is complete.
  stage <- file.path(tempdir(), paste0("phylotrace_export_", Sys.getpid()))
  unlink(stage, recursive = TRUE)
  dir.create(stage, recursive = TRUE)
  on.exit(unlink(stage, recursive = TRUE), add = TRUE)

  base <- tools::file_path_sans_ext(basename(target$path))
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
      writeLines(per[[locus]], file.path(stage, "alleles", paste0(locus, ".fasta")))
    }
    written <- c(written, file.path(stage, "alleles"))
  }

  progress(0.9, "Finalising …")

  if (target$bundled) {
    zip::zip(
      zipfile = target$path,
      files = basename(written),
      root = stage,
      mode = "cherry-pick"
    )
  } else {
    if (file.exists(target$path)) unlink(target$path)
    if (!file.copy(written[[1]], target$path)) {
      stop("Could not write to ", target$path)
    }
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

#' Is this cell a real allele call, or one of the ecosystem's many ways of
#' saying "no call"? chewBBACA's `INF-2` marks an inferred allele — a real call,
#' with the prefix stripped.
#' @export
clean_cell <- function(x) {
  x <- trimws(as.character(x))
  x <- sub("^INF-", "", x)
  x[x %in% MISSING_TOKENS] <- NA_character_
  x
}

#' Classify the identifier kind of a vector of cleaned cells.
#'   "hash"    64 hex chars (sha256) — what we and chewBBACA's --hash-profiles use
#'   "hash_other" hex, but not 64 chars (crc32/md5/sha1): a hash we cannot
#'             recompute against, so it is only linkable via a sequence file
#'   "index"   integers
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

#' Read a profile table from TSV / CSV / Excel and normalise it to long form.
#'
#' Sniffs the delimiter, the orientation (loci as columns or — pyMLST's default
#' form — as rows) and the identifier kind. `loci` is the local scheme's locus
#' list, used to decide the orientation: whichever axis matches the scheme is
#' the locus axis.
#'
#' Returns `list(long, value_kind, loci_as_rows, isolates, loci_seen,
#' loci_unknown, n_missing)` where `long` is a data.frame of
#' (isolate, gene, value) with no-call rows dropped.
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
    loci_as_rows = loci_as_rows,
    isolates = unique(long$isolate),
    loci_seen = loci_seen,
    loci_unknown = loci_unknown,
    n_missing = n_missing
  )
}

#' Read a table from .tsv/.csv/.txt (delimiter sniffed) or .xlsx (first sheet,
#' or the sheet named "profile"/"metadata" when present). Everything comes back
#' as character — allele identifiers must never be coerced to numeric.
#' @export
read_table_any <- function(path, sheet = NULL) {
  ext <- tolower(tools::file_ext(path))

  if (ext %in% c("xlsx", "xlsm")) {
    sheets <- openxlsx::getSheetNames(path)
    pick <- sheet %||% sheets[[1]]
    if (!pick %in% sheets) {
      stop("Sheet '", pick, "' not found. Sheets: ", paste(sheets, collapse = ", "))
    }
    df <- openxlsx::read.xlsx(
      path,
      sheet = pick,
      colNames = TRUE,
      check.names = FALSE
    )
    df[] <- lapply(df, as.character)
    return(df)
  }

  first <- readLines(path, n = 1L, warn = FALSE)
  sep <- if (
    length(first) &&
      lengths(regmatches(first, gregexpr("\t", first))) >=
        lengths(regmatches(first, gregexpr(",", first)))
  ) {
    "\t"
  } else {
    ","
  }

  read.table(
    path,
    sep = sep,
    header = TRUE,
    quote = "\"",
    comment.char = "",
    check.names = FALSE,
    colClasses = "character",
    na.strings = character(0)
  )
}

#' Read a FASTA file into a data.frame of (header, sequence).
#'
#' Plain reader — the app has no Biostrings dependency, and the counterpart
#' writer is just `paste0(">", id, "\n", seq)`.
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

#' Interpret FASTA headers as (locus, allele-id) pairs.
#'
#' Accepted conventions:
#'   `>locus|hash`            our own export
#'   `>locus|allele_id`       pyMLST-ish
#'   `>locus_allele_id`       chewBBACA / cgMLST.org schema files
#' When `locus_hint` is given (a per-locus FASTA whose *filename* names the
#' locus), the header only needs to carry the allele id.
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
