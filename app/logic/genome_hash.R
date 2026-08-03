# app/logic/genome_hash.R
#
# Sequence normalization, GA4GH refget seqcol hashing, and genome assembly identity
# verification against the database to trace provenance and flag duplicates.

box::use(
  DBI[
    dbDisconnect,
    dbExecute,
    dbGetQuery,
    dbListTables,
  ],
  openssl[base64_encode, sha256, sha512],
  stats[setNames],
)
box::use(
  app / logic / db_connect[connect],
  app / logic / logging[log_event],
)

#' Algorithm specification identifier for GA4GH-compatible sorted sequence digests.
#' @export
GENOME_HASH_ALGORITHM <- "ga4gh-sorted-sequences-v1"

#' Data definition language query for creating the `genome_hashes` table.
#' @export
GENOME_HASHES_DDL <- "CREATE TABLE IF NOT EXISTS genome_hashes (
       isolate TEXT PRIMARY KEY,
       genome_digest TEXT NOT NULL,
       file_sha256 TEXT,
       algorithm TEXT NOT NULL,
       n_contigs INTEGER,
       total_length INTEGER,
       file_bytes INTEGER,
       hashed_at TEXT
     )"

#' Compute GA4GH refget sha512t24u Sequence Digest
#'
#' Truncates a SHA-512 digest to 24 bytes and applies RFC 4648 §5 base64url encoding.
#'
#' @param x Character string or raw vector to digest.
#' @return 32-character base64url string.
#' @export
sha512t24u <- function(x) {
  if (is.character(x)) {
    x <- charToRaw(paste(x, collapse = ""))
  }
  gsub(
    "=",
    "",
    chartr("+/", "-_", base64_encode(sha512(x)[1:24])),
    fixed = TRUE
  )
}

# Refget normalization: Strip whitespace and filter non-alphabetic characters
.normalize <- function(x) gsub("[^A-Z]", "", toupper(x))

# Parse FASTA records and discard headers, returning normalized sequence strings
.read_fasta <- function(path) {
  lines <- readLines(path, warn = FALSE)
  header <- startsWith(lines, ">")
  if (!any(header)) {
    return(character(0))
  }

  record <- cumsum(header)[!header]
  body <- lines[!header]
  body <- body[record > 0L]
  record <- record[record > 0L]
  if (!length(body)) {
    return(character(0))
  }

  seqs <- vapply(
    split(body, record),
    function(x) paste0(x, collapse = ""),
    character(1),
    USE.NAMES = FALSE
  )
  seqs <- .normalize(seqs)
  seqs[nzchar(seqs)]
}

# Stream raw file bytes to generate standard sha256 checksum
.file_sha256 <- function(path) {
  con <- file(path, "rb")
  on.exit(try(close(con), silent = TRUE), add = TRUE)
  as.character(sha256(con))
}

#' Generate Order-Invariant Content Digest of Genome Assembly
#'
#' Normalizes sequence contigs, computes refget digests per contig, and hashes
#' the canonical sorted JSON array of contig digests according to the
#' `ga4gh-sorted-sequences-v1` specification.
#'
#' @param path File path to input FASTA assembly.
#' @return Named list of assembly digest metrics, or NULL if path/FASTA is invalid.
#' @export
genome_digest <- function(path) {
  if (
    is.null(path) ||
      length(path) != 1 ||
      is.na(path) ||
      !nzchar(path) ||
      !file.exists(path)
  ) {
    return(NULL)
  }

  seqs <- .read_fasta(path)
  if (!length(seqs)) {
    return(NULL)
  }

  digests <- sort(vapply(
    seqs,
    function(s) sha512t24u(charToRaw(s)),
    character(1),
    USE.NAMES = FALSE
  ))

  canonical <- paste0('["', paste(digests, collapse = '","'), '"]')

  list(
    genome_digest = sha512t24u(charToRaw(canonical)),
    file_sha256 = .file_sha256(path),
    algorithm = GENOME_HASH_ALGORITHM,
    n_contigs = length(seqs),
    total_length = sum(as.numeric(nchar(seqs))),
    file_bytes = as.numeric(file.size(path))
  )
}

#' Persist Assembly Digest to Database
#'
#' @param db_path Path to target SQLite database.
#' @param strain Isolate name string.
#' @param genome_file Path to input FASTA assembly.
#' @param digest Optional precomputed list from `genome_digest()`.
#' @return Invisible logical indicating whether insertion succeeded.
#' @export
store_genome_hash <- function(db_path, strain, genome_file, digest = NULL) {
  if (
    is.null(db_path) ||
      length(db_path) != 1 ||
      is.na(db_path) ||
      !file.exists(db_path) ||
      is.null(strain) ||
      length(strain) != 1 ||
      is.na(strain) ||
      !nzchar(strain)
  ) {
    return(invisible(FALSE))
  }

  if (is.null(digest)) {
    digest <- genome_digest(genome_file)
  }
  if (is.null(digest)) {
    return(invisible(FALSE))
  }

  con <- connect(db_path)
  on.exit(dbDisconnect(con))

  ok <- tryCatch(
    {
      dbExecute(con, GENOME_HASHES_DDL)
      dbExecute(
        con,
        "INSERT OR REPLACE INTO genome_hashes
           (isolate, genome_digest, file_sha256, algorithm, n_contigs,
            total_length, file_bytes, hashed_at)
         VALUES (?, ?, ?, ?, ?, ?, ?, ?)",
        params = list(
          strain,
          digest$genome_digest,
          digest$file_sha256,
          digest$algorithm,
          digest$n_contigs,
          digest$total_length,
          digest$file_bytes,
          as.character(Sys.time())
        )
      )
      TRUE
    },
    error = function(e) FALSE
  )

  log_event(
    "DB",
    "genome_hashes",
    sprintf(
      "isolate=%s | %s",
      strain,
      if (isTRUE(ok)) "stored" else "failed"
    )
  )

  invisible(ok)
}

#' Map Database Isolates to Recorded Genome Digests
#'
#' @param db_path Path to target SQLite database.
#' @return Named character vector mapping isolates to genome digests.
#' @export
genome_hash_map <- function(db_path) {
  empty <- setNames(character(0), character(0))
  if (
    is.null(db_path) ||
      length(db_path) != 1 ||
      is.na(db_path) ||
      !file.exists(db_path)
  ) {
    return(empty)
  }

  con <- connect(db_path)
  on.exit(dbDisconnect(con))

  if (!"genome_hashes" %in% dbListTables(con)) {
    return(empty)
  }
  res <- dbGetQuery(con, "SELECT isolate, genome_digest FROM genome_hashes")
  if (!nrow(res)) {
    return(empty)
  }
  setNames(res$genome_digest, res$isolate)
}

#' Classify Input Assembly Status Against Known Database Records
#'
#' Evaluates whether an input assembly file is new, a identical re-type, a duplicate
#' under a different isolate name (`same_genome`), or a conflicting assembly under an
#' existing isolate name (`name_conflict`).
#'
#' @param strain Target isolate identifier.
#' @param file Path to assembly file.
#' @param recorded Map of recorded genome digests from `genome_hash_map()`.
#' @param known_strains Character vector of isolate names existing in database.
#' @return List containing `digest`, `status` classification string, and `other` isolate name.
#' @export
classify_genome <- function(strain, file, recorded, known_strains) {
  digest <- tryCatch(genome_digest(file), error = function(e) NULL)
  if (is.null(digest)) {
    return(list(digest = NA_character_, status = "new", other = NA_character_))
  }
  d <- digest$genome_digest

  known <- strain %in% known_strains
  stored <- if (strain %in% names(recorded)) recorded[[strain]] else NULL
  elsewhere <- setdiff(names(recorded)[recorded == d], strain)

  if (length(elsewhere)) {
    list(digest = d, status = "same_genome", other = elsewhere[1])
  } else if (!known) {
    list(digest = d, status = "new", other = NA_character_)
  } else if (is.null(stored)) {
    list(digest = d, status = "unknown", other = NA_character_)
  } else if (identical(stored, d)) {
    list(digest = d, status = "retype", other = NA_character_)
  } else {
    list(digest = d, status = "name_conflict", other = NA_character_)
  }
}

#' Batch Check and Classify Multiple Genome Assemblies
#'
#' @param db_path Path to target SQLite database.
#' @param strains Vector of isolate identifiers.
#' @param files Vector of assembly file paths.
#' @param known_strains Vector of isolates already on record.
#' @param progress Optional progress callback function `function(value, detail)`.
#' @return Data frame listing strain, file path, digest, classification status, and collateral isolate match.
#' @export
check_genomes <- function(
  db_path,
  strains,
  files,
  known_strains = NULL,
  progress = NULL
) {
  n <- length(files)
  out <- data.frame(
    strain = as.character(strains),
    file = as.character(files),
    digest = rep(NA_character_, n),
    status = rep("new", n),
    other = rep(NA_character_, n),
    stringsAsFactors = FALSE
  )
  if (!n) {
    return(out)
  }

  recorded <- genome_hash_map(db_path)
  if (is.null(known_strains)) {
    known_strains <- names(recorded)
  }

  for (i in seq_len(n)) {
    if (is.function(progress)) {
      progress(i / n, basename(out$file[i]))
    }
    r <- classify_genome(out$strain[i], out$file[i], recorded, known_strains)
    out$digest[i] <- r$digest
    out$status[i] <- r$status
    out$other[i] <- r$other
  }

  out
}
