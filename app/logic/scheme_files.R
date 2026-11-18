# app/logic/scheme_files.R
#
# Reads and validates user-supplied typing schemes: a cgMLST scheme (one allele
# FASTA per locus, or one reference FASTA), an optional locus annotation table
# and a classical MLST scheme (profile table plus one allele FASTA per locus).
# Every file is treated as untrusted input; nothing here writes to a database.

box::use(
  stats[setNames],
  tools[file_ext],
  utils[count.fields, head, read.table, unzip],
)

box::use(
  app / logic / custom_import[read_import_table],
)

#' Scheme Import Limits
#'
#' @description Upper bounds applied to user-supplied scheme files. They keep a
#'   malformed or hostile archive (zip bomb, runaway FASTA) from exhausting
#'   memory or disk, while staying far above any real scheme: the largest
#'   public cgMLST schemes have ~4,000 loci of a few kilobases each.
#' @export
SCHEME_LIMITS <- list(
  max_loci = 25000L,
  max_mlst_loci = 100L,
  max_locus_name = 100L,
  max_sequence = 100000L,
  max_entry_bytes = 1024^3,
  max_archive_bytes = 16 * 1024^3,
  max_mlst_file_bytes = 512 * 1024^2,
  max_table_bytes = 256 * 1024^2,
  max_annotation_cols = 50L,
  batch_size = 250L
)

# FASTA file names, optionally gzip-compressed (R's file() reads those as-is).
FASTA_RE <- "\\.(fasta|fas|fa|fna|ffn|fsa|tfa|seq)(\\.gz)?$"

#' File Pattern of Allele Sources
#'
#' @description Files a scheme's alleles can be picked from: FASTA files or a
#'   ZIP archive of them.
#' @export
ALLELE_FILE_RE <- paste0("(", FASTA_RE, ")|(\\.zip$)")

#' File Pattern of Scheme Tables
#'
#' @description Files a locus annotation or profile table can be read from.
#' @export
TABLE_FILE_RE <- "\\.(csv|tsv|tab|txt|xlsx|xlsm)$"

# Locus names end up as file names (claMLST reads loci from allele file names)
# and as pyMLST gene ids, so only a conservative character set is accepted.
LOCUS_RE <- "^[A-Za-z0-9][A-Za-z0-9._-]*$"

# Bacterial genetic code (NCBI table 11), as pyMLST checks a reference with
# Biopython's `translate(cds = TRUE, table = 11)`.
START_CODONS <- c("ATG", "GTG", "TTG", "CTG", "ATT", "ATC", "ATA")
STOP_CODONS <- c("TAA", "TAG", "TGA")

# Profile columns that describe a sequence type rather than name a locus.
PROFILE_META_RE <- paste0(
  "^(clonal_?complex|cc|species|lineage|sublineage|clade|mlst_clade|",
  "comments?|notes?|source|date|reference)$"
)

# Builds the issue table every reader reports problems in.
issue_rows <- function(locus = character(0), problem = character(0)) {
  data.frame(
    locus = locus,
    problem = rep(problem, length.out = length(locus)),
    stringsAsFactors = FALSE
  )
}

#' Is a Path a FASTA File?
#'
#' @param path Character vector of paths or archive entry names.
#'
#' @return Logical vector.
#' @export
is_fasta_path <- function(path) {
  grepl(FASTA_RE, path, ignore.case = TRUE)
}

#' Derive a Locus Name from a FASTA File Name
#'
#' @param path Character vector of paths or archive entry names.
#'
#' @return Character vector of base names with the FASTA extension removed.
#' @export
locus_from_path <- function(path) {
  # Archives made on Windows can separate folders with backslashes.
  sub(FASTA_RE, "", basename(gsub("\\", "/", path, fixed = TRUE)), ignore.case = TRUE)
}

#' Check Locus Names
#'
#' @param locus Character vector of locus names.
#'
#' @return Logical vector, `TRUE` where a name is safe to use as a locus id.
#' @export
valid_locus_name <- function(locus) {
  !is.na(locus) &
    nchar(locus) <= SCHEME_LIMITS$max_locus_name &
    grepl(LOCUS_RE, locus) &
    !grepl("..", locus, fixed = TRUE)
}

#' pyMLST Gene Id of a Locus
#'
#' @description pyMLST stores every locus with underscores replaced by hyphens
#'   (`clean_geneid()`), so two names differing only there collide.
#'
#' @param locus Character vector of locus names.
#'
#' @return Character vector of gene ids as pyMLST stores them.
#' @export
pymlst_gene_id <- function(locus) {
  gsub("_", "-", locus, fixed = TRUE)
}

#' Reverse Complement DNA Sequences
#'
#' @param x Character vector of upper-case `ACGT` sequences.
#'
#' @return Character vector of reverse complements.
#' @export
reverse_complement <- function(x) {
  vapply(
    strsplit(chartr("ACGT", "TGCA", x), "", fixed = TRUE),
    function(ch) paste(rev(ch), collapse = ""),
    character(1)
  )
}

# TRUE where a sequence is a complete coding sequence under table 11: whole
# codons, start codon first, stop codon last and no stop codon in between.
is_cds <- function(x) {
  n <- nchar(x)
  ok <- !is.na(x) &
    n >= 6L &
    n %% 3L == 0L &
    substr(x, 1L, 3L) %in% START_CODONS &
    substr(x, n - 2L, n) %in% STOP_CODONS
  for (i in which(ok & n > 6L)) {
    inner <- seq(4L, n[i] - 5L, by = 3L)
    codons <- substring(x[i], inner, inner + 2L)
    ok[i] <- !any(codons %in% STOP_CODONS)
  }
  ok
}

#' Orient Reference Sequences as pyMLST Does
#'
#' @description pyMLST keeps a reference that is a valid coding sequence,
#'   reverse-complements one that is valid only that way round and silently
#'   skips the rest. This reproduces that decision ahead of the build so the
#'   user sees which loci would be dropped.
#'
#' @param x Character vector of upper-case `ACGT` sequences.
#'
#' @return A list with `sequence` (oriented, `NA` where invalid) and `flipped`
#'   (logical, reverse-complemented).
#' @export
orient_cds <- function(x) {
  forward <- is_cds(x)
  flipped <- rep(FALSE, length(x))
  out <- x
  out[!forward] <- NA_character_
  todo <- which(!forward & !is.na(x))
  if (length(todo)) {
    rc <- reverse_complement(x[todo])
    good <- is_cds(rc)
    out[todo[good]] <- rc[good]
    flipped[todo[good]] <- TRUE
  }
  list(sequence = out, flipped = flipped)
}

# Splits FASTA lines into records: id is the first word of the header, the
# sequence is upper-cased with all whitespace removed.
parse_fasta_lines <- function(lines) {
  lines <- sub("\r$", "", lines)
  header <- startsWith(lines, ">")
  if (!any(header)) {
    return(data.frame(id = character(0), sequence = character(0)))
  }
  first <- which(header)[1]
  # Anything but blank lines before the first header means a damaged file
  # (a broken header line): skipping it would quietly make the second record
  # the first, and the first allele of a locus is its reference.
  if (first > 1L && any(nzchar(trimws(lines[seq_len(first - 1L)])))) {
    stop("the file does not start with a FASTA header ('>' line)", call. = FALSE)
  }
  lines <- lines[first:length(lines)]
  header <- header[first:length(header)]
  record <- cumsum(header)
  ids <- sub("^>\\s*", "", lines[header])
  ids <- sub("\\s.*$", "", ids)
  body <- split(lines[!header], factor(record[!header], levels = seq_along(ids)))
  seqs <- vapply(body, paste, character(1), collapse = "")
  data.frame(
    id = ids,
    sequence = toupper(gsub("[[:space:]]", "", seqs)),
    stringsAsFactors = FALSE,
    row.names = NULL
  )
}

#' Read a FASTA File
#'
#' @param path Character path; gzip-compressed files are read transparently.
#' @param max_bytes Numeric. Files larger than this are refused.
#'
#' @return Data frame with `id` and `sequence`.
#' @export
read_fasta <- function(path, max_bytes = SCHEME_LIMITS$max_mlst_file_bytes) {
  size <- file.size(path)
  if (is.na(size)) {
    stop(sprintf("'%s' cannot be read.", basename(path)))
  }
  if (size > max_bytes) {
    stop(sprintf(
      "'%s' is larger than the %d MB allowed.",
      basename(path),
      max_bytes %/% 1024^2
    ))
  }
  lines <- read_lines_capped(path, max_bytes)
  if (is.null(lines)) {
    stop(sprintf("'%s' is not a text file.", basename(path)))
  }
  if (isTRUE(attr(lines, "truncated"))) {
    stop(sprintf(
      "'%s' unpacks to more than the %d MB allowed.",
      basename(path),
      max_bytes %/% 1024^2
    ))
  }
  tryCatch(
    parse_fasta_lines(lines),
    error = function(e) {
      stop(sprintf("'%s': %s.", basename(path), conditionMessage(e)), call. = FALSE)
    }
  )
}

# Reads only the first record of a FASTA file, in chunks, so a locus file with
# thousands of alleles costs no more than its reference. NULL for no record.
read_first_record <- function(path) {
  # The first record plus room for the next header; a file with more but no
  # second header holds a reference longer than allowed.
  cap <- SCHEME_LIMITS$max_sequence * 2L + 10000L
  kept <- read_lines_capped(path, cap)
  if (is.null(kept)) {
    stop("file is not a text file")
  }
  heads <- which(startsWith(kept, ">"))
  if (length(heads) >= 2L) {
    # From the very first line: text before the first header is an error.
    kept <- kept[seq_len(heads[2] - 1L)]
  } else if (isTRUE(attr(kept, "truncated"))) {
    stop("reference sequence is longer than allowed")
  }
  rec <- parse_fasta_lines(kept)
  if (nrow(rec)) rec[1, , drop = FALSE] else NULL
}

# Text lines as UTF-8: lines that are not valid UTF-8 are taken as Latin-1,
# the encoding older spreadsheet exports use.
to_utf8 <- function(lines) {
  bad <- !validUTF8(lines)
  lines[bad] <- iconv(lines[bad], "latin1", "UTF-8", sub = "?")
  Encoding(lines) <- "UTF-8"
  lines
}

# Reads at most `max_bytes` of a file as text lines, decompressing gzip on the
# way: limits apply to what is read, so a small compressed file cannot expand
# into gigabytes. Returns the lines (CR, BOM and encoding handled) with the
# attribute "truncated" when the file holds more; NULL for binary content.
read_lines_capped <- function(path, max_bytes) {
  con <- gzcon(file(path, "rb"))
  on.exit(close(con))
  chunks <- list()
  total <- 0
  repeat {
    chunk <- readBin(con, "raw", min(8 * 1024^2, max_bytes + 1 - total))
    if (!length(chunk)) {
      break
    }
    chunks[[length(chunks) + 1L]] <- chunk
    total <- total + length(chunk)
    if (total > max_bytes) {
      break
    }
  }
  bytes <- unlist(chunks, use.names = FALSE)
  truncated <- length(bytes) > max_bytes
  if (truncated) {
    bytes <- bytes[seq_len(max_bytes)]
  }
  if (any(bytes == as.raw(0L))) {
    return(NULL)
  }
  # Split byte-wise: the text may not be valid UTF-8 until to_utf8() ran.
  lines <- strsplit(rawToChar(bytes), "\n", fixed = TRUE, useBytes = TRUE)[[1]]
  lines <- to_utf8(sub("\r$", "", lines, useBytes = TRUE))
  if (length(lines)) {
    lines[1] <- sub("^﻿", "", lines[1])
  }
  structure(lines, truncated = truncated)
}

#' Detect a File's Type From Its Content
#'
#' @description Browsers save some downloads without a usable name - a
#'   cgMLST.org allele archive can arrive as a file called `null` - so the
#'   importers go by what a file holds, not by its extension.
#'
#' @param path Character path of a regular file.
#'
#' @return `"zip"` (ZIP archive, which includes Excel workbooks), `"fasta"`
#'   (first non-blank line starts with `>`, gzip-compressed or not), `"text"`
#'   or `"binary"`.
#' @export
sniff_file <- function(path) {
  magic <- tryCatch(readBin(path, "raw", n = 4L), error = function(e) raw(0))
  if (length(magic) >= 4L && identical(magic, as.raw(c(0x50, 0x4b, 0x03, 0x04)))) {
    return("zip")
  }
  # file() decompresses gzip transparently, so a .fasta.gz reads like a FASTA.
  lines <- tryCatch(read_lines_capped(path, 65536L), error = function(e) NULL)
  if (is.null(lines)) {
    return("binary")
  }
  first <- lines[nzchar(trimws(lines))][1]
  if (!is.na(first) && startsWith(trimws(first), ">")) "fasta" else "text"
}

# Lists the FASTA entries of a ZIP archive. Refuses archives with entries that
# could escape the extraction folder or whose declared size is implausible.
zip_fasta_entries <- function(zip_path) {
  entries <- tryCatch(
    unzip(zip_path, list = TRUE),
    error = function(e) stop("The file is not a readable ZIP archive.")
  )
  names <- entries$Name
  unsafe <- grepl("(^|[/\\\\])\\.\\.([/\\\\]|$)", names) |
    grepl("^([/\\\\]|[A-Za-z]:)", names)
  if (any(unsafe)) {
    stop("The archive contains entries with unsafe paths and was not opened.")
  }
  # Hidden members and macOS resource forks; a leading "./" is no hidden name.
  regular <- !grepl("/$", names) & !grepl("(^|/)(__MACOSX(/|$)|\\.[^/])", names)
  # FASTA-named members when there are any; otherwise every member, each then
  # checked by content like any other locus file.
  fasta <- regular & is_fasta_path(names)
  entries <- entries[if (any(fasta)) fasta else regular, , drop = FALSE]
  if (!nrow(entries)) {
    stop("The ZIP archive contains no files.")
  }
  if (any(entries$Length > SCHEME_LIMITS$max_entry_bytes)) {
    stop("The archive contains a file larger than allowed.")
  }
  if (sum(as.numeric(entries$Length)) > SCHEME_LIMITS$max_archive_bytes) {
    stop("The archive unpacks to more data than allowed.")
  }
  entries$Name
}

#' Collect Allele Files
#'
#' @description Resolves what the user picked - a folder, a ZIP archive or one
#'   or more FASTA files - into one row per locus file.
#'
#' @param paths Character vector of selected paths.
#'
#' @return A list with `files` (data frame: `locus`, `path`, `entry`; `entry` is
#'   the archive member, `NA` for a plain file), `single` (`TRUE` when the
#'   selection is one FASTA file holding every locus) and `issues`.
#' @export
collect_allele_files <- function(paths) {
  paths <- unique(paths[!is.na(paths) & nzchar(paths)])
  if (!length(paths)) {
    stop("No files selected.")
  }
  missing <- paths[!file.exists(paths)]
  if (length(missing)) {
    stop(sprintf("'%s' does not exist.", missing[1]))
  }

  rows <- list()
  single <- FALSE
  if (length(paths) == 1L && dir.exists(paths)) {
    found <- list.files(paths, full.names = TRUE)
    found <- found[!dir.exists(found)]
    named <- found[is_fasta_path(found)]
    # A folder of FASTA-named files is taken as is; one without any is checked
    # file by file, so extensionless locus files are found too.
    if (!length(named)) {
      named <- found[vapply(found, sniff_file, character(1)) == "fasta"]
    }
    rows <- data.frame(path = named, entry = NA_character_, stringsAsFactors = FALSE)
  } else if (length(paths) == 1L && sniff_file(paths) == "zip") {
    entries <- zip_fasta_entries(paths)
    rows <- data.frame(path = rep(paths, length(entries)), entry = entries)
  } else {
    kinds <- vapply(paths, sniff_file, character(1))
    if (any(kinds == "zip")) {
      stop("Select a ZIP archive on its own, not together with other files.")
    }
    if (any(kinds != "fasta")) {
      stop(sprintf("'%s' is not a FASTA file.", basename(paths[kinds != "fasta"][1])))
    }
    single <- length(paths) == 1L
    rows <- data.frame(path = paths, entry = NA_character_, stringsAsFactors = FALSE)
  }
  if (!nrow(rows)) {
    stop("No FASTA files were found in the selection.")
  }

  rows$locus <- locus_from_path(ifelse(is.na(rows$entry), rows$path, rows$entry))
  # Radix order is the C locale's, so which of two clashing loci is kept never
  # depends on the machine's language settings.
  rows <- rows[order(rows$locus, method = "radix"), c("locus", "path", "entry"), drop = FALSE]
  dup <- duplicated(rows$locus)
  issues <- issue_rows(rows$locus[dup], "duplicate locus file, only the first is used")
  rows <- rows[!dup, , drop = FALSE]
  rownames(rows) <- NULL
  list(files = rows, single = single, issues = issues)
}

#' Visit Allele Files
#'
#' @description Calls `fun(locus, file)` for every locus file. Archive members
#'   are extracted in batches into a private temporary folder - with paths
#'   flattened, so no entry can be written outside it - and removed after each
#'   batch, so disk use stays bounded by one batch however large the archive.
#'
#' @param files Data frame from `collect_allele_files()$files`.
#' @param fun Function of `locus` and a local file path.
#' @param progress Optional function of `done` and `total`.
#'
#' @return List of `fun` results, in `files` order.
#' @export
visit_allele_files <- function(files, fun, progress = NULL) {
  n <- nrow(files)
  out <- vector("list", n)
  batches <- split(seq_len(n), ceiling(seq_len(n) / SCHEME_LIMITS$batch_size))
  # Removed however the visit ends, including when `fun` stops on a file.
  batch_root <- tempfile("scheme_batches_")
  on.exit(unlink(batch_root, recursive = TRUE), add = TRUE)
  for (idx in batches) {
    zipped <- idx[!is.na(files$entry[idx])]
    local <- files$path[idx]
    if (length(zipped)) {
      dir <- file.path(batch_root, idx[1])
      dir.create(dir, recursive = TRUE)
      unzip(
        files$path[zipped[1]],
        files = files$entry[zipped],
        exdir = dir,
        junkpaths = TRUE
      )
      local[match(zipped, idx)] <- file.path(dir, basename(files$entry[zipped]))
    }
    for (k in seq_along(idx)) {
      out[[idx[k]]] <- fun(files$locus[idx[k]], local[k])
    }
    if (length(zipped)) {
      unlink(dir, recursive = TRUE)
    }
    if (is.function(progress)) {
      progress(max(idx), n)
    }
  }
  out
}

#' Read a cgMLST Scheme
#'
#' @description Builds the reference set of a cgMLST scheme: one sequence per
#'   locus, the first allele of each locus file (as pyMLST's importer and the
#'   cgmlst.org, BIGSdb and chewBBACA layouts all agree on), or every record of
#'   a single reference FASTA. Loci pyMLST would reject or choke on - unsafe
#'   names, non-`ACGT` or non-coding references, names or sequences colliding
#'   with an earlier locus - are dropped and reported.
#'
#' @param paths Character vector of selected paths (folder, ZIP or FASTA files).
#' @param progress Optional function of `done` and `total`.
#'
#' @return A list with `loci` (data frame: `locus`, `sequence`, `flipped`),
#'   `issues` (data frame: `locus`, `problem`) and `n_files`.
#' @export
read_cg_scheme <- function(paths, progress = NULL) {
  src <- collect_allele_files(paths)
  issues <- src$issues

  if (src$single) {
    refs <- read_fasta(src$files$path, SCHEME_LIMITS$max_mlst_file_bytes)
    names(refs) <- c("locus", "sequence")
    # One locus file picked instead of the scheme: its headers are allele
    # numbers (">1") or the file's locus plus a number (">abcA_1").
    stem <- sub("[_-]?[0-9]+$", "", refs$locus)
    if (
      nrow(refs) > 1L &&
        (all(grepl("^[0-9]+$", refs$locus)) || all(stem == locus_from_path(src$files$path)))
    ) {
      stop(paste(
        "This file holds the alleles of a single locus, not one reference per locus.",
        "Select the folder or ZIP archive with one file per locus instead."
      ))
    }
  } else {
    read <- visit_allele_files(
      src$files,
      function(locus, file) {
        rec <- tryCatch(read_first_record(file), error = function(e) conditionMessage(e))
        if (is.character(rec)) {
          list(sequence = NA_character_, error = rec)
        } else if (is.null(rec) || !nzchar(rec$sequence)) {
          list(sequence = NA_character_, error = "file holds no sequence")
        } else {
          list(sequence = rec$sequence, error = NA_character_)
        }
      },
      progress = progress
    )
    seqs <- vapply(read, function(r) r$sequence, character(1))
    errors <- vapply(read, function(r) r$error, character(1))
    failed <- !is.na(errors)
    issues <- rbind(issues, issue_rows(src$files$locus[failed], errors[failed]))
    refs <- data.frame(
      locus = src$files$locus[!failed],
      sequence = seqs[!failed],
      stringsAsFactors = FALSE
    )
  }

  if (nrow(refs) > SCHEME_LIMITS$max_loci) {
    stop(sprintf(
      "The scheme has %d loci; at most %d are supported.",
      nrow(refs),
      SCHEME_LIMITS$max_loci
    ))
  }

  drop <- function(bad, problem) {
    issues <<- rbind(issues, issue_rows(refs$locus[bad], problem))
    refs <<- refs[!bad, , drop = FALSE]
  }
  # Name collisions are checked last, so a locus is never dropped for
  # colliding with one that is itself rejected.
  drop(!valid_locus_name(refs$locus), "invalid locus name (letters, digits, . _ - only)")
  drop(!grepl("^[ACGT]+$", refs$sequence), "reference contains characters other than A, C, G, T")
  drop(nchar(refs$sequence) > SCHEME_LIMITS$max_sequence, "reference sequence is too long")

  oriented <- orient_cds(refs$sequence)
  refs$sequence <- oriented$sequence
  refs$flipped <- oriented$flipped
  drop(is.na(refs$sequence), "reference is not a complete coding sequence")
  drop(duplicated(refs$sequence), "reference sequence identical to an earlier locus")
  drop(duplicated(pymlst_gene_id(refs$locus)), "locus name collides with an earlier locus")

  if (!nrow(refs)) {
    stop("No usable locus was found in the selected files.")
  }
  rownames(refs) <- NULL
  list(loci = refs, issues = issues, n_files = nrow(src$files))
}

#' Write a pyMLST Core Genome FASTA
#'
#' @param loci Data frame with `locus` and `sequence`.
#' @param path Character destination path.
#'
#' @return `path`, invisibly.
#' @export
write_coregene <- function(loci, path) {
  wrapped <- vapply(
    loci$sequence,
    function(s) {
      starts <- seq(1L, nchar(s), by = 80L)
      paste(substring(s, starts, pmin(starts + 79L, nchar(s))), collapse = "\n")
    },
    character(1)
  )
  writeLines(paste0(">", loci$locus, "\n", wrapped), path)
  invisible(path)
}

#' Read a Scheme Table
#'
#' @description Reads a locus annotation or MLST profile table (CSV, TSV or
#'   Excel) into an all-character data frame. Tolerates the cgmlst.org locus
#'   export, whose data rows carry one more (empty) leading field than its
#'   header.
#'
#' @param path Character path.
#'
#' @return Data frame with character columns.
#' @export
read_scheme_table <- function(path) {
  size <- file.size(path)
  if (is.na(size)) {
    stop("The file cannot be read.")
  }
  if (size > SCHEME_LIMITS$max_table_bytes) {
    stop("The table is larger than allowed.")
  }
  kind <- sniff_file(path)
  if (kind == "zip") {
    # An Excel workbook is a ZIP archive; the reader goes by extension, so an
    # extensionless download is handed over under a copy that has one.
    if (!tolower(file_ext(path)) %in% c("xlsx", "xlsm")) {
      copy <- tempfile(fileext = ".xlsx")
      on.exit(unlink(copy))
      file.copy(path, copy)
      path <- copy
    }
    return(tryCatch(
      read_import_table(path)$data,
      error = function(e) stop("The file is a ZIP archive, not a table or Excel workbook.")
    ))
  }
  if (kind != "text") {
    stop(if (kind == "fasta") "This is a FASTA file, not a table." else "The file is not a table.")
  }

  lines <- read_lines_capped(path, SCHEME_LIMITS$max_table_bytes)
  if (isTRUE(attr(lines, "truncated"))) {
    stop("The table is larger than allowed.")
  }
  lines <- lines[nzchar(trimws(lines))]
  if (length(lines) < 2L) {
    stop("The table has no data rows.")
  }
  counts <- vapply(
    c("\t", ";", ","),
    function(s) lengths(regmatches(lines[1], gregexpr(s, lines[1], fixed = TRUE))),
    integer(1)
  )
  sep <- if (all(counts == 0L)) "\t" else names(counts)[which.max(counts)]
  # Tab-separated exports do not quote fields, and a product name such as
  # 5" end must not open a quote there; comma and semicolon files may quote.
  quote <- if (sep == "\t") "" else "\""
  fields <- count.fields(
    textConnection(lines),
    sep = sep,
    quote = quote,
    comment.char = "",
    blank.lines.skip = FALSE
  )
  if (anyNA(fields)) {
    stop("The table has an unmatched quote character and cannot be read reliably.")
  }
  split <- function(text, ncol) {
    read.table(
      text = text,
      sep = sep,
      header = FALSE,
      quote = quote,
      comment.char = "",
      colClasses = "character",
      na.strings = character(0),
      fill = TRUE,
      strip.white = TRUE,
      col.names = paste0("V", seq_len(ncol))
    )
  }
  header <- trimws(unlist(split(lines[1], fields[1]), use.names = FALSE))
  header <- header[nzchar(header) | seq_along(header) < length(header)]
  body <- split(lines[-1], max(fields))
  shift <- ncol(body) > length(header) && all(!nzchar(body[[1]]))
  cols <- seq_len(min(ncol(body) - shift, length(header))) + shift
  body <- body[, cols, drop = FALSE]
  header <- header[seq_along(cols)]
  header[!nzchar(header)] <- paste0("column_", which(!nzchar(header)))
  names(body) <- make.unique(header, sep = "_")
  rownames(body) <- NULL
  body
}

# Picks the column whose header matches one of `patterns`, NA when none does.
match_header <- function(headers, patterns) {
  for (p in patterns) {
    hit <- which(grepl(p, headers, ignore.case = TRUE))
    if (length(hit)) {
      return(headers[hit[1]])
    }
  }
  NA_character_
}

#' Guess the Locus Column of an Annotation Table
#'
#' @description The column naming the most of the scheme's loci, compared the
#'   way pyMLST stores locus names.
#'
#' @param table Data frame from `read_scheme_table()`.
#' @param loci Character vector of the scheme's locus names.
#'
#' @return A list with `locus` (column name, `NA` when no column names any
#'   locus) and `matched` (loci that column names).
#' @export
guess_locus_columns <- function(table, loci) {
  keys <- unique(pymlst_gene_id(loci))
  overlap <- vapply(
    table,
    function(col) sum(unique(pymlst_gene_id(locus_from_path(col))) %in% keys),
    integer(1)
  )
  best <- length(overlap) && max(overlap) > 0L
  list(
    locus = if (best) names(table)[which.max(overlap)] else NA_character_,
    matched = if (best) unname(max(overlap)) else 0L
  )
}

# Column names the cgMLST overview adds to the locus table itself; an
# annotation column of the same name is imported under another one.
OVERVIEW_COLUMNS <- c("Locus", "Allele Count", "Isolates Called", "Missing", ".gene")

#' Build the Locus Annotation Table
#'
#' @description Produces the `targets` table the cgMLST overview shows: every
#'   locus of the scheme, in scheme order, followed by the chosen columns of
#'   the annotation table where it has a row for the locus. Loci without one
#'   keep empty fields, so the overview still lists them.
#'
#' @param loci Character vector of the scheme's locus names.
#' @param table Data frame from `read_scheme_table()`, or `NULL`.
#' @param locus_col Character name of the table's locus column, or `NULL`.
#' @param columns Character names of the table's columns to import.
#'
#' @return A list with `targets` (data frame: `Locus` and one character
#'   column per imported column) and `annotated` (loci with a row).
#' @export
build_targets <- function(loci, table = NULL, locus_col = NULL, columns = character(0)) {
  out <- data.frame(Locus = loci, stringsAsFactors = FALSE)
  if (is.null(table) || is.null(locus_col) || is.na(locus_col) || !locus_col %in% names(table)) {
    return(list(targets = out, annotated = 0L))
  }
  key <- pymlst_gene_id(locus_from_path(table[[locus_col]]))
  at <- match(pymlst_gene_id(loci), key)
  columns <- setdiff(intersect(columns, names(table)), locus_col)
  columns <- head(columns, SCHEME_LIMITS$max_annotation_cols)
  for (col in columns) {
    value <- trimws(as.character(table[[col]][at]))
    value[!is.na(value) & !nzchar(value)] <- NA_character_
    name <- col
    while (name %in% c(OVERVIEW_COLUMNS, names(out))) {
      name <- paste(name, "annotation")
    }
    out[[name]] <- substr(value, 1L, 1000L)
  }
  list(targets = out, annotated = sum(!is.na(at)))
}

#' Read a Classical MLST Profile Table
#'
#' @description Reads a PubMLST/BIGSdb-style profile table: an `ST` column,
#'   one column per locus, and optionally descriptive columns such as
#'   `clonal_complex`, which are ignored.
#'
#' @param path Character path (CSV, TSV or Excel).
#'
#' @return A list with `table` (all-character data frame), `st` (the ST column
#'   name) and `candidates` (columns that can hold allele numbers).
#' @export
read_mlst_profiles <- function(path) {
  table <- read_scheme_table(path)
  if (!nrow(table)) {
    stop("The profile table has no rows.")
  }
  st <- match_header(names(table), c("^st$", "^sequence[ _]?type$", "^st[ _]?id$"))
  if (is.na(st)) {
    stop("The profile table has no ST column (a column named 'ST' holding the sequence types).")
  }
  # A locus column holds allele numbers in most rows; the odd "N" or blank is
  # tolerated here and its rows are dropped when the scheme is assembled. A
  # column without a header (a row-number column R or Excel added) is no locus.
  numeric_share <- vapply(table, function(v) mean(grepl("^[0-9]+$", trimws(v))), numeric(1))
  unnamed <- grepl("^column_[0-9]+$", names(table))
  candidates <- setdiff(
    names(table)[numeric_share >= 0.5 & !unnamed],
    c(st, names(table)[grepl(PROFILE_META_RE, names(table), ignore.case = TRUE)])
  )
  if (!length(candidates)) {
    stop("No locus columns with allele numbers were found in the profile table.")
  }
  # Locus names become claMLST gene ids and file names, so a name that is not
  # one, or two names claMLST or a case-blind file system would merge, stops
  # the import: leaving a locus out would change which STs can be called.
  bad <- candidates[!valid_locus_name(candidates)]
  if (length(bad)) {
    stop(sprintf(
      paste(
        "Locus name %s cannot be used: locus names may only contain letters,",
        "digits, '.', '_' and '-', and must start with a letter or digit."
      ),
      paste0("'", head(bad, 5L), "'", collapse = ", ")
    ))
  }
  clash <- duplicated(tolower(pymlst_gene_id(candidates)))
  if (any(clash)) {
    stop(sprintf(
      paste(
        "Locus name %s collides with another locus",
        "('_' and '-' and upper/lower case count as equal)."
      ),
      paste0("'", head(candidates[clash], 5L), "'", collapse = ", ")
    ))
  }
  list(table = table, st = st, candidates = candidates)
}

#' Read Classical MLST Alleles
#'
#' @description Reads every allele of the scheme's loci. Accepts one FASTA per
#'   locus (named after the locus, as the profile table spells it) or a single
#'   FASTA whose headers carry the locus as prefix (`>adk_1`). The allele number
#'   is the trailing number of each header, the rule `claMLST create` applies.
#'
#' @param paths Character vector of selected paths (folder, ZIP or FASTA files).
#' @param loci Character vector of locus names from the profile table.
#'
#' @return A list with `alleles` (data frame: `locus`, `allele` (integer),
#'   `sequence`) and `issues`.
#' @export
read_mlst_alleles <- function(paths, loci) {
  src <- collect_allele_files(paths)
  issues <- src$issues

  if (src$single) {
    recs <- read_fasta(src$files$path)
    prefix <- sub("[_-]?[0-9]+$", "", recs$id)
    recs$locus <- ifelse(prefix %in% loci, prefix, NA_character_)
    unknown <- unique(prefix[is.na(recs$locus)])
    if (length(unknown)) {
      issues <- rbind(issues, issue_rows(head(unknown, 20L), "header names no profile locus"))
    }
    recs <- recs[!is.na(recs$locus), , drop = FALSE]
  } else {
    files <- src$files[src$files$locus %in% loci, , drop = FALSE]
    extra <- setdiff(src$files$locus, loci)
    if (length(extra)) {
      issues <- rbind(issues, issue_rows(head(extra, 20L), "file names no profile locus, ignored"))
    }
    if (nrow(files) > SCHEME_LIMITS$max_mlst_loci) {
      stop("Too many allele files for a classical MLST scheme.")
    }
    read <- visit_allele_files(files, function(locus, file) {
      recs <- read_fasta(file)
      if (nrow(recs)) recs$locus <- locus
      recs
    })
    empty <- data.frame(id = character(0), sequence = character(0), locus = character(0))
    recs <- do.call(rbind, c(list(empty), read))
  }

  number <- regmatches(recs$id, regexpr("[0-9]+$", recs$id))
  has_number <- grepl("[0-9]+$", recs$id)
  if (any(!has_number)) {
    issues <- rbind(issues, issue_rows(
      unique(recs$locus[!has_number]),
      "allele header without a trailing allele number, skipped"
    ))
  }
  recs <- recs[has_number, , drop = FALSE]
  recs$allele <- suppressWarnings(as.integer(number))
  too_big <- is.na(recs$allele)
  if (any(too_big)) {
    issues <- rbind(issues, issue_rows(
      unique(recs$locus[too_big]),
      "allele number too large, skipped"
    ))
  }

  bad_seq <- !grepl("^[ACGT]+$", recs$sequence) | nchar(recs$sequence) > SCHEME_LIMITS$max_sequence
  if (any(bad_seq)) {
    issues <- rbind(issues, issue_rows(
      unique(recs$locus[bad_seq]),
      "allele with characters other than A, C, G, T skipped"
    ))
  }
  recs <- recs[!bad_seq & !is.na(recs$allele), , drop = FALSE]
  dup <- duplicated(recs[, c("locus", "allele")])
  if (any(dup)) {
    issues <- rbind(issues, issue_rows(unique(recs$locus[dup]), "duplicate allele number skipped"))
  }
  recs <- recs[!dup, c("locus", "allele", "sequence"), drop = FALSE]
  # claMLST stores every sequence once across all loci and refuses a second
  # allele with the same sequence, so only the lowest-numbered one is kept.
  recs <- recs[order(recs$locus, recs$allele, method = "radix"), , drop = FALSE]
  same_seq <- duplicated(recs$sequence)
  if (any(same_seq)) {
    issues <- rbind(issues, issue_rows(
      unique(recs$locus[same_seq]),
      "allele with the same sequence as a lower-numbered allele skipped"
    ))
  }
  recs <- recs[!same_seq, , drop = FALSE]
  rownames(recs) <- NULL
  list(alleles = recs, issues = issues)
}

#' Assemble a Classical MLST Scheme
#'
#' @description Joins the profile table with the alleles into the long form the
#'   database stores. Loci are the profile's allele columns that have alleles;
#'   an allele column without any is an error, since no profile using it could
#'   ever be matched. Profile rows with a non-numeric ST or allele are dropped.
#'
#' @param profiles List from `read_mlst_profiles()`.
#' @param alleles List from `read_mlst_alleles()`.
#'
#' @return A list with `loci` (character, profile order), `profiles` (data
#'   frame: `st`, `locus`, `allele`, integers but `locus`), `alleles`, `issues`
#'   and `summary` (named counts).
#' @export
assemble_mlst_scheme <- function(profiles, alleles) {
  have <- unique(alleles$alleles$locus)
  loci <- intersect(profiles$candidates, have)
  missing <- setdiff(profiles$candidates, have)
  if (length(missing)) {
    stop(sprintf(
      "No alleles were found for profile locus %s.",
      paste0("'", head(missing, 5L), "'", collapse = ", ")
    ))
  }
  if (!length(loci)) {
    stop("The allele files match none of the profile loci.")
  }
  if (length(loci) > SCHEME_LIMITS$max_mlst_loci) {
    stop("Too many loci for a classical MLST scheme.")
  }
  # pyMLST seeds the search of every locus from its allele 1 (`open_cla(ref = 1)`),
  # so a locus without one could never be called and no ST ever assigned.
  seeded <- unique(alleles$alleles$locus[alleles$alleles$allele == 1L])
  unseeded <- setdiff(loci, seeded)
  if (length(unseeded)) {
    stop(sprintf(
      "Allele 1 is missing for locus %s. pyMLST searches each locus from its allele 1.",
      paste0("'", head(unseeded, 5L), "'", collapse = ", ")
    ))
  }

  table <- profiles$table
  st <- trimws(table[[profiles$st]])
  values <- vapply(loci, function(l) trimws(table[[l]]), character(nrow(table)))
  if (!is.matrix(values)) {
    values <- matrix(values, ncol = length(loci), dimnames = list(NULL, loci))
  }
  # Up to nine digits fits an integer; "1" and "01" are the same ST.
  number_re <- "^[0-9]{1,9}$"
  valid <- grepl(number_re, st) & apply(values, 1L, function(r) all(grepl(number_re, r)))
  st_number <- suppressWarnings(as.integer(st))
  valid <- valid & !duplicated(ifelse(valid, st_number, NA), incomparables = NA)
  issues <- alleles$issues
  if (any(!valid)) {
    issues <- rbind(issues, issue_rows(
      "profiles",
      sprintf(
        "%d profile row(s) with a missing, duplicate or non-numeric value skipped",
        sum(!valid)
      )
    ))
  }
  if (!any(valid)) {
    stop("The profile table holds no usable sequence type.")
  }

  long <- data.frame(
    st = rep(st_number[valid], times = length(loci)),
    locus = rep(loci, each = sum(valid)),
    allele = as.integer(values[valid, , drop = FALSE]),
    stringsAsFactors = FALSE
  )
  known <- paste(alleles$alleles$locus, alleles$alleles$allele)
  unknown <- long$allele > 0L & !paste(long$locus, long$allele) %in% known
  if (any(unknown)) {
    issues <- rbind(issues, issue_rows(
      "profiles",
      sprintf("%d profile allele(s) have no sequence in the allele files", sum(unknown))
    ))
  }
  kept <- alleles$alleles[alleles$alleles$locus %in% loci, , drop = FALSE]
  list(
    loci = loci,
    profiles = long,
    alleles = kept,
    issues = issues,
    summary = setNames(
      c(length(loci), sum(valid), nrow(kept)),
      c("loci", "profiles", "alleles")
    )
  )
}
