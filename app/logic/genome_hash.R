# app/logic/genome_hash.R
#
# Content identity for the *assembly* an isolate was typed from.
#
# An isolate is identified by its name, which loop-pymlst.sh derives from the
# assembly's file name (basename minus extension) and pyMLST stores as
# `mlst.souche` - the one column in the schema still carrying pyMLST's French
# spelling. Every PhyloTrace-owned table calls the same key `isolate`.
#
# Keying on a name makes identity a property of the file name rather than of the
# data: renaming an assembly and re-typing it silently creates a second isolate,
# and two different assemblies that happen to share a file name silently
# collapse into one (pyMLST rejects the second as `StrainAlreadyPresent`, which
# the UI reports as "Duplicate"). Neither is detectable today, because the
# assembly itself is never stored.
#
# `genome_hashes` closes that gap by recording a content digest of the assembly
# next to the isolate it produced. What it does and does not prove matters:
#
#   * A match proves the two runs consumed the byte-identical *assembly*.
#   * A mismatch proves nothing at all. Re-assembling the same reads with a
#     different assembler, assembler version or read depth yields a different
#     digest AND a materially different allele profile, so "different digest"
#     never means "different isolate".
#
# It is therefore a one-way alarm and a provenance record - not an isolate
# identity, and never a uniqueness constraint. The isolate name stays the key
# that `metadata` / `amr_*` / `classical_mlst` join on; the digest is one more
# attribute hanging off it. For the *biological* "same isolate?" question at
# cgMLST resolution, `isolate_profile_hashes()` in db_import.R remains the better
# instrument - it is robust to reassembly, which this deliberately is not.
#
# ## The digest
#
# Hashing the file bytes would make the answer depend on FASTA headers, line
# wrapping and contig order - none of which are part of the sequence. GA4GH
# solved exactly this for refget: normalise a sequence by stripping whitespace
# and restricting to A-Z (headers never participate), then digest it with
# `sha512t24u` - SHA-512 truncated to 24 bytes, base64url-encoded.
#
# An assembly is a *collection* of sequences, which is refget Sequence
# Collections (seqcol) territory. We deliberately do NOT compute seqcol's
# top-level ("level 0") digest: only `names` and `sequences` are inherent there,
# so renaming a contig changes it - and draft-assembly contig names are assembler
# noise (`NODE_1_length_..._cov_...`). We compute seqcol's `sorted_sequences`
# attribute instead, which exists precisely to give a name-invariant and
# order-invariant content identity:
#
#   1. discard every header
#   2. per record: strip whitespace, uppercase, keep A-Z   (refget normalisation)
#   3. digest each record with sha512t24u
#   4. sort the digests lexicographically
#   5. digest the canonical JSON array of them with sha512t24u
#
# Step 5's canonical JSON (RFC-8785) needs no library here: the members are
# base64url strings, whose alphabet holds no JSON-escapable character, so the
# canonical form is exactly `["<d1>","<d2>",...]`.
#
# Each per-record digest is a genuine refget identifier - prefix it `ga4gh:SQ.`
# and it is comparable against ENA, refget servers and VRS. That portability is
# the point, and is why this module uses sha512t24u where the `hashes` table
# (allele sequences) uses sha256 hex: the allele table speaks chewBBACA's dialect
# because that is its ecosystem, and this one speaks refget's.
#
# There is no R implementation of seqcol (tooling is Python: refgenie/refget,
# seqcolapi). We implement one narrow, fully specified attribute of it, not the
# standard - hence the explicit `algorithm` column, so a future move to a true
# seqcol digest becomes a new value rather than a schema migration.
#
# Reverse-complement / circularity canonicalisation (seqhash-style) is
# deliberately out of scope: a re-run of the same pipeline is byte-identical
# anyway, a genuine reassembly moves contig boundaries so orientation is moot,
# and handling palindromes / IUPAC codes correctly is real risk for no gain.

box::use(
  RSQLite[SQLite],
  DBI[
    dbConnect,
    dbDisconnect,
    dbExecute,
    dbGetQuery,
    dbListTables,
  ],
  openssl[base64_encode, sha256, sha512],
  stats[setNames],
)
box::use(
  app / logic / logging[log_event],
)

# Bumped only if the construction above changes. Stored per row so a database
# written by an older build stays interpretable.
#' @export
GENOME_HASH_ALGORITHM <- "ga4gh-sorted-sequences-v1"

# `isolate` is the primary key: one assembly per isolate, and re-typing REPLACEs
# rather than accumulating. No file-name column - the isolate name already *is*
# the file name, and a lab-internal path is exactly what should not ride along
# in an export.
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

### The GA4GH digest: SHA-512, truncated to 24 bytes, base64url, 32 chars
# Verified against the specification's test vector:
#   sha512t24u(charToRaw("ACGT")) == "aKF498dAxcJAqme6QYQ7EZ07-fiw8Kw2"
# i.e. `ga4gh:SQ.aKF498dAxcJAqme6QYQ7EZ07-fiw8Kw2`. base64url is plain base64
# with "+/" mapped to "-_" and the "=" padding dropped (RFC 4648 §5).
#' @export
sha512t24u <- function(x) {
  if (is.character(x)) {
    x <- charToRaw(paste(x, collapse = ""))
  }
  gsub("=", "", chartr("+/", "-_", base64_encode(sha512(x)[1:24])), fixed = TRUE)
}

# refget normalisation: strip every whitespace character, uppercase, keep only
# A-Z. Soft-masked (lowercase) bases, `*`, `-` and stray digits therefore never
# change the digest, and neither does line wrapping.
.normalize <- function(x) gsub("[^A-Z]", "", toupper(x))

### Normalised sequences of a FASTA file, headers discarded
# Returns one string per record, in file order (the caller sorts their digests).
# Records that normalise to nothing are dropped: an empty record carries no
# sequence, so it must not change the collection's identity. Anything with no
# ">" line at all is not FASTA and yields character(0).
.read_fasta <- function(path) {
  lines <- readLines(path, warn = FALSE)
  header <- startsWith(lines, ">")
  if (!any(header)) {
    return(character(0))
  }

  # cumsum over the header flags numbers the records; body lines preceding the
  # first header (group 0) belong to no record and are dropped.
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

# Streamed rather than read whole: openssl hashes a connection incrementally, but
# leaves it open, so closing is on us.
.file_sha256 <- function(path) {
  con <- file(path, "rb")
  on.exit(try(close(con), silent = TRUE), add = TRUE)
  as.character(sha256(con))
}

### Content digest of one assembly
# Returns NULL when the file is missing or holds no usable FASTA record - the
# caller treats that as "nothing to record", never as an error. `file_sha256` is
# the plain sha256 of the file bytes: the audit anchor a bare `sha256sum` can
# reproduce. When it differs but `genome_digest` matches, the file was
# re-wrapped, re-headered or recompressed and the sequence content is untouched.
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

  # One call per record rather than openssl's vectorised form: that returns hex
  # strings, and we need the raw bytes to truncate. A draft assembly has a few
  # hundred contigs at most, so the loop is free next to reading the file.
  digests <- sort(vapply(
    seqs,
    function(s) sha512t24u(charToRaw(s)),
    character(1),
    USE.NAMES = FALSE
  ))

  # RFC-8785 canonical JSON for an array of base64url strings (see header).
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

### Persist one isolate's assembly digest into the mother database
# Mirrors store_amr_results(): best-effort, additive, and never able to disturb
# cgMLST results. `digest` accepts a precomputed genome_digest() so the typing
# run does not hash the same assembly twice. Re-typing REPLACEs the row, since
# `isolate` is the primary key.
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

  con <- dbConnect(SQLite(), db_path, busy_timeout = 5000)
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

### Every recorded digest, as isolate -> genome_digest
# Empty when the database has never typed a genome with this build.
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

  con <- dbConnect(SQLite(), db_path, busy_timeout = 5000)
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

### Classify assemblies about to be typed against what the database already holds
# Returns one row per input file: `strain`, `file`, `digest` (NA when the file
# could not be read), `status` and `other` (the isolate a "same_genome" collides
# with). Statuses:
#
#   new           - neither the name nor the assembly is on record
#   same_genome   - this exact assembly is already stored under ANOTHER isolate
#                   (`other`): typing it again would enter one isolate twice
#   retype        - same isolate, same assembly - a genuine re-run
#   name_conflict - the isolate exists but was typed from a DIFFERENT assembly;
#                   pyMLST will reject this file as a duplicate and its data
#                   will be silently dropped
#   unknown       - the isolate exists but predates digest recording, so nothing
#                   can be said. This is the majority state for older databases
#                   and must never be reported as a mismatch.
#
# Purely advisory. Nothing here blocks a run: re-typing an assembly under a new
# name is legitimate (a different scheme, a re-analysis), and only the metadata
# can settle whether two records are really the same epidemiological isolate.
# Classify a single assembly against what the database already holds. Split out
# of check_genomes so callers that need to walk a large selection without
# blocking (the typing module runs this one genome per reactive tick, so the
# check stays interruptible and drives the progress bar) can drive the loop
# themselves. `recorded` is the genome_hash_map() for the target DB and
# `known_strains` the isolates already on record; both are read once by the
# caller and passed in so a batch does not re-query the database per file.
# Returns a one-element list: `digest` (NA when the file could not be read),
# `status` (see check_genomes for the vocabulary) and `other`.
#' @export
classify_genome <- function(strain, file, recorded, known_strains) {
  digest <- tryCatch(genome_digest(file), error = function(e) NULL)
  if (is.null(digest)) {
    # Unreadable / non-FASTA: nothing the digest can say, so it stays "new" -
    # the run itself will report whatever pyMLST makes of the file.
    return(list(digest = NA_character_, status = "new", other = NA_character_))
  }
  d <- digest$genome_digest

  known <- strain %in% known_strains
  stored <- if (strain %in% names(recorded)) recorded[[strain]] else NULL

  # A digest already on record under a different isolate is the interesting
  # case, and outranks the name-based verdict: it is the same assembly.
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
