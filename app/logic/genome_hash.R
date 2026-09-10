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
  parallel[detectCores, mccollect, mclapply, mcparallel],
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
#' @param with_file_sha256 Compute the raw-file SHA-256 checksum. Defaults to
#'   TRUE; the pre-run genome check sets it FALSE because that checksum plays no
#'   part in classifying a file, and its full-file read is pure overhead there.
#' @return Named list of assembly digest metrics, or NULL if path/FASTA is invalid.
#'   `file_sha256` is `NA` when `with_file_sha256` is FALSE.
#' @export
genome_digest <- function(path, with_file_sha256 = TRUE) {
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
    file_sha256 = if (isTRUE(with_file_sha256)) .file_sha256(path) else NA_character_,
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
  # A digest carried over from the pre-run check omits the raw-file checksum
  # (that pass skips it on purpose). Fill it in now, from the file still on
  # disk, so the stored row is complete however the digest was obtained.
  if (is.null(digest$file_sha256) || is.na(digest$file_sha256)) {
    digest$file_sha256 <- tryCatch(
      .file_sha256(genome_file),
      error = function(e) NA_character_
    )
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

#' Read One Isolate's Recorded Assembly Digests
#'
#' Returns the stored digests and assembly measures for a single isolate, as a
#' list ready to be merged into a provenance row. The timestamp is left out:
#' when the isolate was typed is recorded by the provenance row itself.
#'
#' @param db_path Path to target SQLite database.
#' @param isolate Isolate identifier.
#' @return Named list of digest fields; all `NA` when the isolate has no row.
#' @export
genome_hash_row <- function(db_path, isolate) {
  empty <- list(
    genome_digest = NA_character_,
    file_sha256 = NA_character_,
    algorithm = NA_character_,
    n_contigs = NA_integer_,
    total_length = NA_real_,
    file_bytes = NA_real_
  )
  if (
    is.null(db_path) ||
      length(db_path) != 1 ||
      is.na(db_path) ||
      !file.exists(db_path) ||
      is.null(isolate) ||
      length(isolate) != 1 ||
      is.na(isolate)
  ) {
    return(empty)
  }

  con <- tryCatch(connect(db_path), error = function(e) NULL)
  if (is.null(con)) {
    return(empty)
  }
  on.exit(dbDisconnect(con))

  if (!"genome_hashes" %in% dbListTables(con)) {
    return(empty)
  }
  res <- tryCatch(
    dbGetQuery(
      con,
      "SELECT genome_digest, file_sha256, algorithm, n_contigs, total_length,
              file_bytes
         FROM genome_hashes WHERE isolate = ?",
      params = list(isolate)
    ),
    error = function(e) NULL
  )
  if (is.null(res) || !nrow(res)) {
    return(empty)
  }
  as.list(res[1, , drop = FALSE])
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
#' Reports two independent facts about an input assembly, which must not be
#' conflated: whether its **isolate name** is already taken, and whether its
#' **assembly content** matches something already stored.
#'
#' `status` is the name axis, and it alone decides whether a file can be typed
#' at all - pyMLST's `wgMLST add` keys on the strain name and rejects a name
#' that already exists, whatever the file contains:
#'
#' * `new` - the name is free; this is the only status that can be typed.
#' * `retype` - name taken, and the stored assembly for that same isolate is
#'   byte-identical. A no-op re-run.
#' * `name_conflict` - name taken, but the stored assembly differs. The
#'   alarming case: two different assemblies are competing for one name.
#' * `name_untracked` - name taken, with no recorded digest to compare against
#'   (an isolate typed before genome hashing existed). The majority state for
#'   older databases, and never a mismatch in itself.
#'
#' `other` is the content axis, filled in independently of `status`: the name of
#' a *different* isolate whose stored assembly is byte-identical to this file,
#' or `NA`. It is advisory only - identical content under a new name is
#' legitimate (a re-deposit or a rename), and only the metadata can settle
#' whether two records are really the same epidemiological isolate.
#'
#' Both axes can be true at once (a taken name whose content also matches some
#' third isolate), which is why the content match must not overwrite the name
#' verdict - doing so queues a file pyMLST is certain to reject.
#'
#' @param strain Target isolate identifier.
#' @param file Path to assembly file.
#' @param recorded Map of recorded genome digests from `genome_hash_map()`.
#' @param known_strains Character vector of isolate names existing in database.
#' @return List with `digest`, the name-axis `status`, and the content-axis
#'   `other` isolate name (`NA` when no other isolate shares this assembly).
#' @export
classify_genome <- function(strain, file, recorded, known_strains) {
  digest <- tryCatch(
    genome_digest(file, with_file_sha256 = FALSE),
    error = function(e) NULL
  )
  classify_with_digest(strain, digest, recorded, known_strains)
}

#' Classify an Already-Computed Assembly Digest Against Known Database Records
#'
#' The name-and-content classification half of `classify_genome()`, split out so
#' a batch can hash its assemblies in one (optionally parallel) pass and then
#' classify the results serially. See `classify_genome()` for what the two axes
#' mean.
#'
#' @param strain Target isolate identifier.
#' @param digest A list from `genome_digest()`, or `NULL` if the file could not
#'   be read as FASTA.
#' @param recorded Map of recorded genome digests from `genome_hash_map()`.
#' @param known_strains Character vector of isolate names existing in database.
#' @return List with `digest`, the name-axis `status`, and the content-axis
#'   `other` isolate name (`NA` when no other isolate shares this assembly).
#' @export
classify_with_digest <- function(strain, digest, recorded, known_strains) {
  if (is.null(digest)) {
    return(list(digest = NA_character_, status = "new", other = NA_character_))
  }
  d <- digest$genome_digest

  known <- strain %in% known_strains
  stored <- if (strain %in% names(recorded)) recorded[[strain]] else NULL
  # Content axis: some *other* isolate already holds this exact assembly.
  elsewhere <- setdiff(names(recorded)[recorded == d], strain)
  other <- if (length(elsewhere)) elsewhere[1] else NA_character_

  # Name axis: decided on its own, so a content match can never mask a taken
  # name and smuggle an unusable file into the queue.
  status <- if (!known) {
    "new"
  } else if (is.null(stored)) {
    "name_untracked"
  } else if (identical(stored, d)) {
    "retype"
  } else {
    "name_conflict"
  }

  list(digest = d, status = status, other = other)
}

# Cores held back from parallel hashing so the running app stays responsive:
# one for the R main process / event loop, one for the browser and OS. The
# hashing burst runs inside a live desktop session, not on a dedicated box.
HASH_WORKER_RESERVE <- 2L

# Cached answer to "can this R process fork?", probed once and lazily. mclapply
# needs fork(): unavailable on Windows, and hard-errored (not merely warned) by
# front-ends that own the R session - Positron stops outright, RStudio disables
# it. Where fork is out, the check runs serially instead.
.fork_state <- new.env(parent = emptyenv())

.can_fork <- function() {
  if (!is.null(.fork_state$ok)) {
    return(.fork_state$ok)
  }
  hostile_frontend <- identical(Sys.getenv("POSITRON"), "1") ||
    identical(Sys.getenv("RSTUDIO"), "1")
  ok <- .Platform$OS.type == "unix" &&
    !hostile_frontend &&
    tryCatch(
      isTRUE(mccollect(mcparallel(TRUE))[[1L]]),
      error = function(e) FALSE,
      warning = function(e) FALSE
    )
  .fork_state$ok <- ok
  ok
}

# Upper bound on hashing workers regardless of core count. Past this, memory
# (each worker holds transient copies of a multi-MB assembly), disk contention
# on network shares / spinning disks, and fork-collect overhead erode the gain
# faster than the extra worker adds. Deliberately conservative; raise it with
# options(phylotrace.hash_workers = N) on a big workstation.
HASH_WORKER_CAP <- 8L

#' Choose a Worker Count for Parallel Assembly Hashing
#'
#' Holds `HASH_WORKER_RESERVE` cores back for the app and the system, never
#' spawns more workers than there are assemblies to hash, and caps the count at
#' `HASH_WORKER_CAP`. Returns 1 (serial) wherever `fork()` is unavailable - see
#' `.can_fork()`. Override the whole calculation with
#' `options(phylotrace.hash_workers = N)`; it is still clamped to 1 when forking
#' is impossible, since there is no parallelism to be had.
#'
#' @param n_items Number of assemblies to be hashed.
#' @param reserve Cores to leave free (default `HASH_WORKER_RESERVE`).
#' @param cap Hard upper bound on workers (default `HASH_WORKER_CAP`).
#' @return Positive integer; 1 means "run serially".
#' @export
resolve_hash_workers <- function(
  n_items,
  reserve = HASH_WORKER_RESERVE,
  cap = HASH_WORKER_CAP
) {
  n_items <- suppressWarnings(as.integer(n_items))
  if (is.na(n_items) || n_items < 2L || !.can_fork()) {
    return(1L)
  }
  override <- getOption("phylotrace.hash_workers", NA)
  if (!is.na(override)) {
    return(max(1L, min(suppressWarnings(as.integer(override)), n_items)))
  }
  cores <- tryCatch(detectCores(logical = TRUE), error = function(e) NA_integer_)
  if (is.na(cores) || cores < 2L) {
    return(1L)
  }
  max(1L, min(cores - as.integer(reserve), n_items, as.integer(cap)))
}

#' Hash Several Assemblies, Optionally in Parallel
#'
#' @param files Character vector of assembly file paths.
#' @param workers Worker count from `resolve_hash_workers()`; 1 runs serially.
#' @param with_file_sha256 Passed through to `genome_digest()`.
#' @return List parallel to `files`; each element is a `genome_digest()` list, or
#'   `NULL` for an unreadable / non-FASTA input.
#' @export
genome_digests <- function(files, workers = 1L, with_file_sha256 = TRUE) {
  files <- as.character(files)
  one <- function(f) {
    tryCatch(
      genome_digest(f, with_file_sha256 = with_file_sha256),
      error = function(e) NULL
    )
  }
  if (workers <= 1L || length(files) < 2L) {
    return(lapply(files, one))
  }
  out <- tryCatch(
    mclapply(files, one, mc.cores = workers, mc.preschedule = TRUE),
    error = function(e) NULL
  )
  if (is.null(out)) {
    # fork() refused at run time (a sandboxed front-end, an rlimit) - fall back
    # to a serial pass rather than failing the check.
    return(lapply(files, one))
  }
  # A worker killed mid-fork leaves a try-error in its slot; recompute just
  # those serially rather than failing the whole batch.
  bad <- vapply(out, function(x) inherits(x, "try-error"), logical(1))
  if (any(bad)) {
    out[bad] <- lapply(files[bad], one)
  }
  out
}

#' Batch Check and Classify Multiple Genome Assemblies
#'
#' @param db_path Path to target SQLite database.
#' @param strains Vector of isolate identifiers.
#' @param files Vector of assembly file paths.
#' @param known_strains Vector of isolates already on record.
#' @param progress Optional progress callback function `function(value, detail)`.
#' @param workers Worker count for hashing; `NULL` picks one via
#'   `resolve_hash_workers()`. Hashing happens up front in one pass, so
#'   `progress` advances quickly once it returns.
#' @return Data frame of one row per input, with the same `digest` / `status` /
#'   `other` columns `classify_genome()` returns (see there for the two axes).
#' @export
check_genomes <- function(
  db_path,
  strains,
  files,
  known_strains = NULL,
  progress = NULL,
  workers = NULL
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
  if (is.null(workers)) {
    workers <- resolve_hash_workers(n)
  }

  digests <- genome_digests(out$file, workers = workers, with_file_sha256 = FALSE)

  for (i in seq_len(n)) {
    if (is.function(progress)) {
      progress(i / n, basename(out$file[i]))
    }
    r <- classify_with_digest(
      out$strain[i],
      digests[[i]],
      recorded,
      known_strains
    )
    out$digest[i] <- r$digest
    out$status[i] <- r$status
    out$other[i] <- r$other
  }

  out
}
