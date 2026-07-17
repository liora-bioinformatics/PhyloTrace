# app/logic/pymslt.R

box::use(
  RSQLite[SQLite],
  DBI[
    dbConnect,
    dbListTables,
    dbReadTable,
    dbGetQuery,
    dbWriteTable,
    dbExecute,
    dbBegin,
    dbCommit,
    dbRollback,
    dbDisconnect,
  ],
  processx[run, process],
  openssl[sha256],
  tidyr[pivot_wider],
  dplyr[select, left_join],
)

### Download cgmlst scheme
# db_path - target database path including '.db' file ending
# scheme - scheme corresponding to cgmlst.org schemes
# overwrite - overwrite existing database
#' @export
download_cgmlst_scheme <- function(
  scheme,
  db_path,
  env_name,
  overwrite = FALSE
) {
  download_status <- tryCatch(
    run(
      command = "conda",
      args = c(
        "run",
        "-n",
        env_name,
        "wgMLST",
        "import",
        if (overwrite) {
          "--force"
        },
        "--no-prompt",
        basename(db_path),
        scheme
      ),
      wd = dirname(db_path),
      echo_cmd = TRUE,
      echo = TRUE,
      stderr_to_stdout = TRUE,
      error_on_status = FALSE
    ),
    error = function(e) e
  )

  return(download_status)
}

### Assemble the loop-pymlst.sh command-line flags
# Shared by the blocking `type_genomes()` and the non-blocking `start_typing()`
# so the database / genome / parameter contract lives in a single place.
# `genome_files` is the explicit vector of assembly files to type; each is
# passed as a positional argument (after a `--` guard so file names are never
# mistaken for options) and typed in turn. Resolving the file list in R - rather
# than letting the script glob a directory - lets the caller drop assemblies
# that are already in the database before they ever reach `wgMLST add`.
# `species` / `repo` / `cla_db` request the classical-MLST pass: when a species
# could be resolved from the mother DB it is passed via `-s` (and the repository
# via `-r`), and the script builds a claMLST reference DB at `cla_db` (`-m`),
# derives the ST per genome, and leaves the DB in place for the R caller to read
# its reference sequences / metadata and then delete. Omitting `-s`/`-m` makes
# the script skip classical MLST entirely.
typing_args <- function(
  db_path,
  genome_files,
  identity,
  coverage,
  env,
  species = NA_character_,
  repo = "pubmlst",
  cla_db = NA_character_
) {
  args <- c(
    "-d",
    basename(db_path),
    "-i",
    as.character(identity),
    "-c",
    as.character(coverage),
    "-e",
    env
  )
  scalar_chr <- function(x) {
    !is.null(x) && length(x) == 1 && !is.na(x) && nzchar(x)
  }
  if (scalar_chr(species)) {
    args <- c(args, "-s", species, "-r", repo)
    if (scalar_chr(cla_db)) {
      args <- c(args, "-m", cla_db)
    }
  }
  c(args, "--", genome_files)
}

### Typing isolates
#' @export
type_genomes <- function(
  database,
  db_path,
  genome_files,
  script_path = "app/logic/loop-pymlst.sh",
  identity = 0.95,
  coverage = 0.9,
  env = "pymlst"
) {
  # Run the process. `bash <script>` avoids depending on the script's execute
  # bit.
  typing_status <- run(
    command = "bash",
    args = c(
      normalizePath(script_path, mustWork = TRUE),
      typing_args(db_path, genome_files, identity, coverage, env)
    ),
    wd = dirname(db_path),
    echo_cmd = TRUE,
    echo = TRUE,
    stderr_to_stdout = TRUE,
    error_on_status = FALSE
  )

  # Parse logs
  stdout_text <- typing_status$stdout

  # Extract BLAT gene count
  gene_match <- regmatches(
    stdout_text,
    regexec("found ([0-9]+) genes", stdout_text)
  )
  typing_status$genes_found_by_blat <- if (length(gene_match[[1]]) > 1) {
    as.numeric(gene_match[[1]][2])
  } else {
    0
  }

  # Check for "Already Present" error
  typing_status$already_present <- grepl(
    "already present in the base",
    stdout_text
  )

  # Check for "Core Genome Path" error (The species mismatch/quality error)
  typing_status$species_mismatch <- grepl(
    "No path was found for the core genome",
    stdout_text
  )

  # Define success: Exit status 0 AND no "Error:" string in the output
  typing_status$success <- (typing_status$status == 0 &&
    !grepl("Error:", stdout_text))

  # Console Feedback
  if (typing_status$already_present) {
    message("DUPLICATE: Entry already present. No action taken.")
  } else if (typing_status$species_mismatch) {
    warning(
      "INCOMPATIBLE: ",
      typing_status$genes_found_by_blat,
      " hits found, but none passed QC. Verify scheme."
    )
  } else if (typing_status$success) {
    message("OK: New strain added successfully.")
  }

  # Read database with newly added genomes
  database <- read_database(db_path)

  return(database)
}

### Start typing in the background (non-blocking)
# Launches loop-pymlst.sh as a detached processx process that streams its
# combined stdout/stderr into `log_file`. Returns the live `process` object so
# the caller (the Typing module) can poll `is_alive()`, tail the log for live
# progress, and `kill()` it on demand. Unlike `type_genomes()` this does not
# block the R session and does not touch the database itself - read the result
# with `read_database()` once the process has finished.
#' @export
start_typing <- function(
  db_path,
  genome_files,
  log_file,
  script_path = "app/logic/loop-pymlst.sh",
  identity = 0.95,
  coverage = 0.9,
  env = "pymlst",
  species = NA_character_,
  repo = "pubmlst",
  cla_db = NA_character_
) {
  process$new(
    command = "bash",
    args = c(
      normalizePath(script_path, mustWork = TRUE),
      typing_args(
        db_path,
        genome_files,
        identity,
        coverage,
        env,
        species,
        repo,
        cla_db
      )
    ),
    wd = dirname(db_path),
    stdout = log_file,
    stderr = "2>&1",
    # The bash wrapper spawns `conda run` -> python (wgMLST), and it is that
    # descendant that actually holds the SQLite lock. cleanup_tree tags the whole
    # subtree so it can be killed as a unit (via kill_tree() or on GC) - killing
    # just the bash wrapper would orphan pymlst and leave the database locked.
    cleanup_tree = TRUE
  )
}

### Classify a classical MLST result: known / novel / partial
# From the ST cell and the "gene=allele,..." profile string that claMLST search
# reports:
#   known   - a registered ST number was returned
#   novel   - no ST, but every locus was called (a complete, unregistered profile)
#   partial - no ST and at least one locus could not be called
#   NA      - nothing was called (no usable profile) => not a classical result
# In genomic epidemiology the allele profile - not the ST label - is the ground
# truth, so novel and partial results are retained (see store_clamlst_results),
# not discarded: a novel ST is a positive finding (a potentially emerging clone).
#' @export
clamlst_status <- function(st, alleles) {
  if (
    !is.null(st) &&
      length(st) == 1 &&
      !is.na(st) &&
      nzchar(trimws(as.character(st)))
  ) {
    return("known")
  }
  if (
    is.null(alleles) ||
      length(alleles) != 1 ||
      is.na(alleles) ||
      !nzchar(alleles)
  ) {
    return(NA_character_)
  }
  pairs <- strsplit(alleles, ",", fixed = TRUE)[[1]]
  vals <- trimws(sub("^[^=]*=", "", pairs))
  n_present <- sum(nzchar(vals))
  if (n_present == 0) {
    return(NA_character_)
  }
  if (n_present == length(vals)) "novel" else "partial"
}

### Parse a (possibly partial) typing log into a per-strain status table
# `log_lines` is the captured loop-pymlst.sh output (character vector or single
# string); `strains` is the ordered vector of expected strain names (assembly
# file names without extension). Returns one row per expected strain so it can
# drive a live progress / results table. Columns:
#   status  - Pending | Running | Added | Duplicate | Incompatible | Error
#   found   - genes located by BLAT
#   added   - new MLST genes stored (len(genes) - bad)
#   partial  - partial genes detected
#   filled   - partial genes recovered
#   removed  - genes dropped (bad coverage / failed CDS test)
#   finished - wall-clock time of the "DONE" log line ("HH:MM:SS")
#   elapsed  - analysis duration in seconds (DONE minus first timestamp)
#   detail   - short human-readable explanation
#   st         - classical MLST Sequence Type (NA when unavailable)
#   alleles    - classical MLST per-gene allele profile ("gene=allele,...")
#   cla_status - classical MLST outcome: known | novel | partial | NA
#
# The outcomes mirror every branch `wgMLST add` (pymlst/wg/core.py::add_strain
# and pymlst/common/blat.py::run_blat) can take:
#   Added        - run reached "DONE"
#   Duplicate    - StrainAlreadyPresent ("already present in the base")
#   Incompatible - CoreGenomePathNotFound ("No path was found for the core
#                  genome"): BLAT matched no core gene, i.e. wrong species /
#                  unusable assembly
#   Error        - BinaryNotFound, BLAT failure, bad identity/coverage range,
#                  ChromosomeNotFound, invalid strain name, or any other
#                  ClickException printed as "Error: ..."
#' @export
parse_typing_log <- function(log_lines, strains) {
  log_text <- paste(log_lines, collapse = "\n")
  finished_all <- grepl("Done!", log_text, fixed = TRUE)

  # Each strain section is introduced by the script's "Processing Strain:" line.
  parts <- strsplit(log_text, "Processing Strain: ", fixed = TRUE)[[1]]
  sections <- list()
  order_seen <- character(0)
  for (part in parts[-1]) {
    name <- trimws(sub("\n.*$", "", part))
    sections[[name]] <- part
    order_seen <- c(order_seen, name)
  }

  num <- function(chunk, pattern) {
    match <- regmatches(chunk, regexec(pattern, chunk))[[1]]
    if (length(match) > 1) as.integer(match[2]) else NA_integer_
  }

  # String capture (first group) with the literal "NA" and empty strings mapped
  # to NA - used for the classical-MLST ST and allele-profile log lines.
  strval <- function(chunk, pattern) {
    match <- regmatches(chunk, regexec(pattern, chunk))[[1]]
    if (length(match) > 1) {
      value <- trimws(match[2])
      if (identical(value, "NA") || !nzchar(value)) NA_character_ else value
    } else {
      NA_character_
    }
  }

  # Per-strain wall-clock timing taken from the "[INFO: <ts>]" log prefixes
  # (e.g. "[INFO: 2026-06-25 21:16:40,224] DONE"). `finished` is the time on the
  # DONE line; `elapsed` is DONE minus the strain's first timestamp (the BLAT
  # search). Both are NA when the strain never reached DONE.
  ts_pattern <-
    "\\[INFO: ([0-9]{4}-[0-9]{2}-[0-9]{2} [0-9]{2}:[0-9]{2}:[0-9]{2}[.,][0-9]+)\\]"
  timing <- function(chunk) {
    to_posix <- function(x) {
      as.POSIXct(gsub(",", ".", x), format = "%Y-%m-%d %H:%M:%OS")
    }
    matches <- regmatches(chunk, gregexpr(ts_pattern, chunk))[[1]]
    stamps <- if (length(matches)) {
      to_posix(sub(ts_pattern, "\\1", matches))
    } else {
      as.POSIXct(character(0))
    }
    done <- regmatches(
      chunk,
      regexec(paste0(ts_pattern, "[ \t]*DONE"), chunk)
    )[[1]]
    done_ts <- if (length(done) > 1) to_posix(done[2]) else as.POSIXct(NA)
    start_ts <- if (length(stamps)) {
      min(stamps, na.rm = TRUE)
    } else {
      as.POSIXct(NA)
    }
    list(
      finished = if (!is.na(done_ts)) {
        format(done_ts, "%H:%M:%S")
      } else {
        NA_character_
      },
      elapsed = if (!is.na(done_ts) && !is.na(start_ts)) {
        as.numeric(difftime(done_ts, start_ts, units = "secs"))
      } else {
        NA_real_
      }
    )
  }

  classify <- function(chunk, complete) {
    times <- timing(chunk)
    metrics <- list(
      found = num(chunk, "found ([0-9]+) genes"),
      added = num(chunk, "Added ([0-9]+) new MLST genes"),
      partial = num(chunk, "Found ([0-9]+) partial genes"),
      filled = num(chunk, "partial genes, filled ([0-9]+)"),
      removed = num(chunk, "Removed ([0-9]+) genes"),
      finished = times$finished,
      elapsed = times$elapsed
    )
    # Classical MLST (best-effort, may be absent): the ST and the per-gene allele
    # profile echoed by loop-pymlst.sh after `claMLST search`, plus a known /
    # novel / partial classification of the result.
    cla_st <- strval(chunk, "Classical MLST ST:[ \t]*(\\S+)")
    cla_alleles <- strval(chunk, "Classical MLST alleles:[ \t]*([^\n]+)")
    metrics$st <- cla_st
    metrics$alleles <- cla_alleles
    metrics$cla_status <- clamlst_status(cla_st, cla_alleles)
    outcome <- function(status, detail) {
      c(metrics, list(status = status, detail = detail))
    }

    if (!complete) {
      return(outcome("Running", "Typing in progress ..."))
    }
    if (grepl("already present in the base", chunk)) {
      return(outcome("Duplicate", "Strain already present in the database."))
    }
    if (grepl("No path was found for the core genome", chunk)) {
      return(outcome(
        "Incompatible",
        "No core genes matched - assembly likely does not match the scheme species."
      ))
    }
    if (grepl("BLAT binary was not found", chunk)) {
      return(outcome(
        "Error",
        "BLAT binary not found in the pymlst environment."
      ))
    }
    if (grepl("An error occurred while running BLAT", chunk)) {
      return(outcome("Error", "BLAT failed to run on this assembly."))
    }
    if (grepl("must be in range", chunk)) {
      return(outcome("Error", "Identity / coverage must be within [0-1]."))
    }
    if (grepl("Chromosome .* not found", chunk)) {
      return(outcome(
        "Error",
        "A matched contig was missing from the assembly."
      ))
    }
    if (grepl("contains", chunk) && grepl("symbol", chunk)) {
      return(outcome("Error", "Invalid strain name (unsupported character)."))
    }
    if (grepl("DONE", chunk) || !is.na(metrics$added)) {
      # The gene counts are surfaced as their own columns; nothing extra to say.
      return(outcome("Added", ""))
    }
    # Complete but unrecognised: surface any explicit "Error: ..." line.
    err <- regmatches(chunk, regexec("Error:[ \t]*(.+)", chunk))[[1]]
    outcome(
      "Error",
      if (length(err) > 1) trimws(err[2]) else "Unrecognised outcome - see log."
    )
  }

  rows <- lapply(strains, function(strain) {
    if (!strain %in% names(sections)) {
      info <- list(
        status = "Pending",
        found = NA_integer_,
        added = NA_integer_,
        partial = NA_integer_,
        filled = NA_integer_,
        removed = NA_integer_,
        finished = NA_character_,
        elapsed = NA_real_,
        detail = "Waiting in queue ...",
        st = NA_character_,
        alleles = NA_character_,
        cla_status = NA_character_
      )
    } else {
      # A section is complete once another section follows it or the run is over.
      idx <- match(strain, order_seen)
      complete <- idx < length(order_seen) || finished_all
      info <- classify(sections[[strain]], complete)
    }
    data.frame(
      strain = strain,
      status = info$status,
      found = info$found,
      added = info$added,
      partial = info$partial,
      filled = info$filled,
      removed = info$removed,
      finished = info$finished,
      elapsed = info$elapsed,
      detail = info$detail,
      st = info$st,
      alleles = info$alleles,
      cla_status = info$cla_status,
      stringsAsFactors = FALSE
    )
  })

  do.call(rbind, rows)
}

### Run-level classical-MLST provenance from a typing log
# The claMLST reference DB is built once per run, so its provenance is emitted
# once (before the first strain) rather than per strain. Returns the repository
# actually used (after any fallback), the resolved scheme/species, the reference
# database's release date, and the pyMLST version - each NA when its line is
# absent (e.g. no classical scheme was found for the species).
#' @export
parse_clamlst_meta <- function(log_lines) {
  log_text <- paste(log_lines, collapse = "\n")
  grab <- function(pattern) {
    match <- regmatches(log_text, regexec(pattern, log_text))[[1]]
    if (length(match) > 1) {
      value <- trimws(match[2])
      if (nzchar(value)) value else NA_character_
    } else {
      NA_character_
    }
  }
  list(
    repository = grab("Classical MLST repository:[ \t]*([^\n]+)"),
    scheme = grab("Classical MLST scheme:[ \t]*([^\n]+)"),
    scheme_version = grab("Classical MLST scheme version:[ \t]*([^\n]+)"),
    pymlst_version = grab("pyMLST version:[ \t]*([^\n]+)")
  )
}

### Species recorded for the loaded scheme
# pymlst stores the scheme's species in the mother DB's native `mlst_type` table
# (one row per scheme, e.g. species = "Pseudomonas aeruginosa"). This is what
# tells `claMLST import` which classical scheme to fetch - with no user input.
# Returns NA when unavailable (no DB / no table / empty value).
#' @export
db_species <- function(db_path) {
  if (
    is.null(db_path) ||
      length(db_path) != 1 ||
      is.na(db_path) ||
      !file.exists(db_path)
  ) {
    return(NA_character_)
  }

  con <- dbConnect(SQLite(), db_path, synchronous = NULL, busy_timeout = 5000)
  on.exit(dbDisconnect(con))

  if (!"mlst_type" %in% dbListTables(con)) {
    return(NA_character_)
  }

  res <- tryCatch(
    dbGetQuery(
      con,
      "SELECT species FROM mlst_type
       WHERE species IS NOT NULL AND species != '' LIMIT 1"
    ),
    error = function(e) NULL
  )
  if (is.null(res) || !nrow(res)) {
    return(NA_character_)
  }
  species <- trimws(as.character(res$species[1]))
  if (nzchar(species)) species else NA_character_
}

### Read reference sequences + schema marker from a claMLST reference database
# The interim claMLST DB (built by `claMLST import` during a typing run) holds a
# `sequences` table (one row per gene+allele reference sequence) and an
# `alembic_version` table (pymlst schema-migration marker). Returns a list with
# `sequences` (named character vector keyed "<gene>\t<allele>") and `alembic`
# (scalar version_num), both empty/NA when the DB is missing or unreadable. The
# reference sequence for a called allele is byte-identical to the sequence in the
# assembly, since a classical ST is only assigned on exact allele matches.
clamlst_refs <- function(cla_db_path) {
  empty <- list(sequences = character(0), alembic = NA_character_)
  if (
    is.null(cla_db_path) ||
      length(cla_db_path) != 1 ||
      is.na(cla_db_path) ||
      !nzchar(cla_db_path) ||
      !file.exists(cla_db_path)
  ) {
    return(empty)
  }

  con <- tryCatch(
    dbConnect(SQLite(), cla_db_path, synchronous = NULL, busy_timeout = 5000),
    error = function(e) NULL
  )
  if (is.null(con)) {
    return(empty)
  }
  on.exit(dbDisconnect(con))

  tables <- dbListTables(con)
  sequences <- character(0)
  if ("sequences" %in% tables) {
    seqs <- tryCatch(
      dbGetQuery(con, "SELECT gene, allele, sequence FROM sequences"),
      error = function(e) NULL
    )
    if (!is.null(seqs) && nrow(seqs)) {
      sequences <- as.character(seqs$sequence)
      names(sequences) <- paste(seqs$gene, seqs$allele, sep = "\t")
    }
  }
  alembic <- NA_character_
  if ("alembic_version" %in% tables) {
    av <- tryCatch(
      dbGetQuery(con, "SELECT version_num FROM alembic_version LIMIT 1"),
      error = function(e) NULL
    )
    if (!is.null(av) && nrow(av)) {
      alembic <- as.character(av$version_num[1])
    }
  }

  list(sequences = sequences, alembic = alembic)
}

### Persist classical MLST results (+ provenance) into the mother database
# Stored long / per-allele, mirroring the mother DB's own `mlst` table
# (one row per locus, keyed on `souche`): each row is one gene's allele call for
# a strain, columns `souche, gene, allele, sequence`. The strain's ST and the
# run-level provenance (scheme, reference release, alembic schema marker,
# repository, identity/coverage, pyMLST version, timestamp) are carried on every
# allele row so each is self-describing - strains may be typed across separate
# runs with different parameters. This denormalisation matches how the mother DB
# stores `mlst` (souche repeated per gene) rather than a wide one-row-per-strain
# layout.
#
# `results` is the parse_typing_log() data frame (needs `strain`, `st`, and
# optionally `alleles`). Every strain that produced a usable profile is written -
# `known` (registered ST), `novel` (complete profile, no registered ST) and
# `partial` (some loci uncalled) alike - because the allele profile, not the ST
# label, is the result worth keeping; only strains where nothing was called are
# skipped. The `st` column holds the registered number for `known` and is NULL
# otherwise, with the `status` column carrying the classification. A strain's
# existing rows are replaced so re-typing never duplicates them. `cla_db_path`,
# when given, is the interim claMLST reference DB from the same run, read for the
# per-allele reference `sequence` and the `alembic_version` marker. Best-effort:
# does nothing (returns FALSE) when there is nothing to store.
#' @export
store_clamlst_results <- function(
  db_path,
  results,
  cla_db_path = NA_character_,
  identity = NA_real_,
  coverage = NA_real_,
  repository = NA_character_,
  scheme = NA_character_,
  scheme_version = NA_character_,
  pymlst_version = NA_character_
) {
  if (
    is.null(db_path) ||
      length(db_path) != 1 ||
      is.na(db_path) ||
      !file.exists(db_path) ||
      is.null(results) ||
      !nrow(results) ||
      !all(c("strain", "st") %in% names(results))
  ) {
    return(invisible(FALSE))
  }

  has_alleles <- "alleles" %in% names(results)

  # Classify each strain (known / novel / partial / NA) and keep everything that
  # yielded a profile - novel and partial results are retained, not discarded.
  status_vec <- vapply(
    seq_len(nrow(results)),
    function(i) {
      clamlst_status(
        results$st[i],
        if (has_alleles) results$alleles[i] else NA_character_
      )
    },
    character(1)
  )
  keep <- !is.na(status_vec)
  results <- results[keep, , drop = FALSE]
  status_vec <- status_vec[keep]
  if (!nrow(results)) {
    return(invisible(FALSE))
  }

  # Reference sequences + schema marker from this run's claMLST reference DB.
  refs <- clamlst_refs(cla_db_path)
  seq_for <- function(gene, allele) {
    if (
      is.na(gene) || is.na(allele) || !length(refs$sequences)
    ) {
      return(NA_character_)
    }
    val <- refs$sequences[[paste(gene, allele, sep = "\t")]]
    if (is.null(val)) NA_character_ else val
  }
  alembic_version <- refs$alembic

  # "gene=allele,gene=allele,..." -> data frame(gene, allele), one row per locus.
  split_alleles <- function(spec) {
    if (is.null(spec) || is.na(spec) || !nzchar(spec)) {
      return(data.frame(
        gene = character(0),
        allele = character(0),
        stringsAsFactors = FALSE
      ))
    }
    pairs <- strsplit(spec, ",", fixed = TRUE)[[1]]
    kv <- regmatches(pairs, regexec("^[ \t]*([^=]+)=(.*)$", pairs))
    gene <- vapply(
      kv,
      function(m) if (length(m) == 3) trimws(m[2]) else NA_character_,
      character(1)
    )
    allele <- vapply(
      kv,
      function(m) if (length(m) == 3) trimws(m[3]) else NA_character_,
      character(1)
    )
    # Keep only loci that were actually called (drop empty alleles, e.g. the
    # missing locus of a partial result), mirroring how `mlst` stores one row per
    # called locus - so a strain's row count reflects its called loci.
    ok <- !is.na(gene) & nzchar(gene) & !is.na(allele) & nzchar(allele)
    data.frame(gene = gene[ok], allele = allele[ok], stringsAsFactors = FALSE)
  }

  con <- dbConnect(SQLite(), db_path, busy_timeout = 5000)
  on.exit(dbDisconnect(con))

  dbExecute(
    con,
    "CREATE TABLE IF NOT EXISTS classical_mlst (
       id INTEGER PRIMARY KEY AUTOINCREMENT,
       souche TEXT,
       gene TEXT,
       allele TEXT,
       sequence TEXT,
       st TEXT,
       status TEXT,
       scheme TEXT,
       scheme_version TEXT,
       alembic_version TEXT,
       repository TEXT,
       identity REAL,
       coverage REAL,
       pymlst_version TEXT,
       called_at TEXT
     )"
  )

  now <- as.character(Sys.time())
  dbBegin(con)
  ok <- tryCatch(
    {
      for (i in seq_len(nrow(results))) {
        souche <- as.character(results$strain[i])
        status <- status_vec[i]
        # The ST number is only meaningful for a registered (known) profile;
        # novel / partial results carry NULL st and are identified by `status`.
        st <- if (identical(status, "known")) {
          as.character(results$st[i])
        } else {
          NA_character_
        }
        spec <- if (has_alleles) as.character(results$alleles[i]) else NA_character_
        genes <- split_alleles(spec)

        # Nothing concrete to store (should not happen for a kept strain): leave
        # any existing rows untouched.
        if (!nrow(genes)) {
          next
        }

        # Refresh this strain's rows so re-typing never duplicates them.
        dbExecute(
          con,
          "DELETE FROM classical_mlst WHERE souche = ?",
          params = list(souche)
        )

        for (j in seq_len(nrow(genes))) {
          dbExecute(
            con,
            "INSERT INTO classical_mlst
               (souche, gene, allele, sequence, st, status, scheme,
                scheme_version, alembic_version, repository, identity, coverage,
                pymlst_version, called_at)
             VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)",
            params = list(
              souche,
              genes$gene[j],
              genes$allele[j],
              seq_for(genes$gene[j], genes$allele[j]),
              st,
              status,
              scheme,
              scheme_version,
              alembic_version,
              repository,
              identity,
              coverage,
              pymlst_version,
              now
            )
          )
        }
      }
      TRUE
    },
    error = function(e) FALSE
  )
  if (isTRUE(ok)) dbCommit(con) else dbRollback(con)

  invisible(ok)
}

### Distinct strain names already stored in a database
# Used to flag selected assemblies that are already present (their wgMLST `add`
# would be rejected as a duplicate). The synthetic "ref" core-genome entry is
# excluded.
#' @export
existing_strains <- function(db_path) {
  if (
    is.null(db_path) ||
      length(db_path) != 1 ||
      is.na(db_path) ||
      !file.exists(db_path)
  ) {
    return(character(0))
  }

  con <- dbConnect(SQLite(), db_path, synchronous = NULL, busy_timeout = 5000)
  on.exit(dbDisconnect(con))

  if (!"mlst" %in% dbListTables(con)) {
    return(character(0))
  }

  souches <- dbGetQuery(con, "SELECT DISTINCT souche FROM mlst")$souche
  setdiff(souches, "ref")
}

### Total number of loci in the scheme
# The reference core genome is stored in `mlst` under the synthetic strain
# "ref", one row per locus, so the count of "ref" rows is the scheme size. Used
# as the denominator of the completeness (QC) metric and shown to the user.
#' @export
scheme_size <- function(db_path) {
  if (
    is.null(db_path) ||
      length(db_path) != 1 ||
      is.na(db_path) ||
      !file.exists(db_path)
  ) {
    return(NA_integer_)
  }

  con <- dbConnect(SQLite(), db_path, synchronous = NULL, busy_timeout = 5000)
  on.exit(dbDisconnect(con))

  if (!"mlst" %in% dbListTables(con)) {
    return(NA_integer_)
  }

  as.integer(
    dbGetQuery(con, "SELECT COUNT(*) AS n FROM mlst WHERE souche = 'ref'")$n
  )
}

### Number of loci called for each strain
# One `mlst` row is stored per locus successfully called for a strain, so the
# row count per `souche` is the number of genes present. Divided by
# `scheme_size()` this gives the completeness (QC) metric. Returns an integer
# vector named by (and aligned to) `strains`; strains absent from the database
# are NA.
#' @export
strain_gene_counts <- function(db_path, strains) {
  counts <- rep(NA_integer_, length(strains))
  names(counts) <- strains

  if (
    !length(strains) ||
      is.null(db_path) ||
      length(db_path) != 1 ||
      is.na(db_path) ||
      !file.exists(db_path)
  ) {
    return(counts)
  }

  con <- dbConnect(SQLite(), db_path, synchronous = NULL, busy_timeout = 5000)
  on.exit(dbDisconnect(con))

  if (!"mlst" %in% dbListTables(con)) {
    return(counts)
  }

  placeholders <- paste(rep("?", length(strains)), collapse = ",")
  result <- dbGetQuery(
    con,
    paste0(
      "SELECT souche, COUNT(*) AS n FROM mlst WHERE souche IN (",
      placeholders,
      ") GROUP BY souche"
    ),
    params = as.list(strains)
  )

  counts[result$souche] <- as.integer(result$n)
  counts
}

# Checks the database hashing status and fills missing values
#' @export
hash_database <- function(db_path) {
  message("Checking database hashing status ...")

  con <- dbConnect(
    SQLite(),
    db_path,
    synchronous = NULL,
    busy_timeout = 5000
  )
  on.exit(dbDisconnect(con))

  sequences <- dbReadTable(con, "sequences")

  if (any("hashes" == dbListTables(con))) {
    # case 'hash' table is present in database
    # hashing only unindexed sequences

    hash_table <- dbReadTable(con, "hashes")

    if (nrow(sequences) != nrow(hash_table)) {
      # Case sequence ids missing in hash_table
      new <- sequences[!sequences$id %in% hash_table$id, ]
      new$hash <- vapply(
        new$sequence,
        sha256,
        character(1)
      )

      hash_table <- rbind(hash_table, new[, c("id", "hash")])

      message(paste(nrow(new), "sequence hashes added"))
    }
  } else {
    # case no 'hash' table present
    # hashing the whole database
    hash_table <- data.frame(
      id = sequences$id,
      hash = vapply(sequences$sequence, sha256, character(1))
    )

    message(paste(
      "Hash table initiated;",
      nrow(hash_table),
      "sequence hashes added"
    ))
  }

  dbWriteTable(con, "hashes", hash_table, overwrite = TRUE)
}

# TODO
# documentation hint for potential implementation
mlst_profile <- function(database) {
  # Get MLST profile
  # https://pymlst.readthedocs.io/en/latest/documentation/cgmlst/export_res.html#mlst
}

# TODO
# documentation hint for potential implementation
stage_genomes <- function(database) {
  # Validate staged genomes
  # https://pymlst.readthedocs.io/en/latest/documentation/cgmlst/check.html#validate-strains
}
