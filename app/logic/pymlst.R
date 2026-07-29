# app/logic/pymlst.R

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
box::use(
  app / logic / logging[log_event],
)

# Central Conda environment where all external bioinformatics CLI dependencies
# (cgMLST, classical MLST, BLAT, MAFFT, abritamr, AMRFinderPlus) reside.
#' Central Conda Environment Name
#'
#' @description Defines the central Conda environment name containing external CLI tools.
#' @export
conda_env <- "PhyloTrace"

#' Resolve Base Conda Executable Path
#'
#' @description Resolves the system path for the Conda binary. Prefers `CONDA_EXE`
#'   to target the base Conda installation rather than an active sub-environment binary.
#'
#' @return Character string specifying the Conda executable path.
#' @export
conda_exe <- function() {
  exe <- Sys.getenv("CONDA_EXE", unset = "")
  if (nzchar(exe) && file.exists(exe)) {
    return(exe)
  }
  "conda"
}

#' Download cgMLST Scheme via wgMLST
#'
#' @description Executes `wgMLST import` in the configured Conda environment to
#'   retrieve a scheme from cgmlst.org into a target database file.
#'
#' @param scheme Character string. Target scheme identifier.
#' @param db_path Character string. Output path for the SQLite database file.
#' @param env_name Character string. Conda environment name. Defaults to `conda_env`.
#' @param overwrite Logical. If `TRUE`, forces re-download and overwrites existing data.
#'
#' @return A `processx` execution status object.
#' @export
download_cgmlst_scheme <- function(
  scheme,
  db_path,
  env_name = conda_env,
  overwrite = FALSE
) {
  download_status <- tryCatch(
    run(
      command = conda_exe(),
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

# Constructs command-line arguments for loop-pymlst.sh.
# Explicit genome file paths are passed after a `--` separator to prevent file names
# from being parsed as CLI options. Optional parameters (species, classical MLST,
# AMR screening) are appended only when non-empty strings are provided.
typing_args <- function(
  db_path,
  genome_files,
  identity,
  coverage,
  env,
  species = NA_character_,
  repo = "pubmlst",
  cla_db = NA_character_,
  amr_env = NA_character_,
  amr_species = NA_character_,
  amr_out = NA_character_
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
  if (scalar_chr(amr_out) && scalar_chr(amr_env)) {
    args <- c(args, "-A", amr_env, "-o", amr_out)
    if (scalar_chr(amr_species)) {
      args <- c(args, "-p", amr_species)
    }
  }
  c(args, "--", genome_files)
}

#' Execute Synchronous Genome Typing Pipeline
#'
#' @description Runs the typing shell wrapper in synchronous (blocking) mode, parses
#'   BLAT gene matches and error conditions from stdout, and reloads the updated database.
#'
#' @param database Data object or list representing the in-memory database state.
#' @param db_path Character string. File path to the target SQLite database.
#' @param genome_files Character vector. File paths to input assembly FASTA files.
#' @param script_path Character string. Path to `loop-pymlst.sh`. Defaults to `"app/logic/loop-pymlst.sh"`.
#' @param identity Numeric. BLAT sequence identity cutoff (0 to 1). Defaults to `0.95`.
#' @param coverage Numeric. BLAT coverage cutoff (0 to 1). Defaults to `0.9`.
#' @param env Character string. Conda environment name. Defaults to `conda_env`.
#'
#' @return Refreshed database structure loaded from `db_path`.
#' @export
type_genomes <- function(
  database,
  db_path,
  genome_files,
  script_path = "app/logic/loop-pymlst.sh",
  identity = 0.95,
  coverage = 0.9,
  env = conda_env
) {
  # Invoking bash explicitly avoids depending on the wrapper script's execute permission bit
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

#' Launch Background Typing Process
#'
#' @description Executes `loop-pymlst.sh` as a detached, non-blocking background process
#'   and redirects standard output/error streams to a log file for live polling.
#'
#' @param db_path Character string. Path to target SQLite database.
#' @param genome_files Character vector. Input genome assembly file paths.
#' @param log_file Character string. Output log destination file path.
#' @param script_path Character string. Path to `loop-pymlst.sh`. Defaults to `"app/logic/loop-pymlst.sh"`.
#' @param identity Numeric. BLAT sequence identity cutoff (0 to 1). Defaults to `0.95`.
#' @param coverage Numeric. BLAT target coverage cutoff (0 to 1). Defaults to `0.9`.
#' @param env Character string. Conda environment name. Defaults to `conda_env`.
#' @param species Character string (optional). Species name for classical MLST lookup.
#' @param repo Character string. Classical MLST repository source. Defaults to `"pubmlst"`.
#' @param cla_db Character string (optional). File path for intermediate classical MLST database.
#' @param amr_env Character string (optional). Conda environment for AMR screening tools.
#' @param amr_species Character string (optional). Species token for point mutation screening.
#' @param amr_out Character string (optional). Output directory for AMR results.
#'
#' @return A `processx::process` instance.
#' @export
start_typing <- function(
  db_path,
  genome_files,
  log_file,
  script_path = "app/logic/loop-pymlst.sh",
  identity = 0.95,
  coverage = 0.9,
  env = conda_env,
  species = NA_character_,
  repo = "pubmlst",
  cla_db = NA_character_,
  amr_env = NA_character_,
  amr_species = NA_character_,
  amr_out = NA_character_
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
        cla_db,
        amr_env,
        amr_species,
        amr_out
      )
    ),
    wd = dirname(db_path),
    stdout = log_file,
    stderr = "2>&1",
    # cleanup_tree terminates child Python subprocesses to prevent orphaned locks on the SQLite database
    cleanup_tree = TRUE
  )
}

#' Classify Classical MLST Result Profile
#'
#' @description Categorizes classical MLST outcomes based on Sequence Type (ST) and
#'   allele profile strings.
#'
#' @details
#' Profile categories:
#' - `known`: Standard registered ST returned.
#' - `novel`: Unregistered ST, but all loci successfully called.
#' - `partial`: At least one locus failed to call.
#' - `NA`: No usable loci called.
#'
#' @param st Character string or numeric. Sequence Type assigned by search.
#' @param alleles Character string. Comma-delimited locus-allele key-value string (`"gene=allele"`).
#'
#' @return Character string (`"known"`, `"novel"`, `"partial"`, or `NA_character_`).
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

#' Parse Typing Run Logs into Structured Table
#'
#' @description Parses raw output logs from `loop-pymlst.sh` into a structured
#'   per-strain summary data frame used for real-time progress monitoring and reporting.
#'
#' @param log_lines Character vector or scalar string containing raw log lines.
#' @param strains Character vector of expected strain identifiers (assembly names without extension).
#'
#' @return A `data.frame` containing metrics, execution timing, classical MLST calls,
#'   and AMR status per strain.
#' @export
parse_typing_log <- function(log_lines, strains) {
  log_text <- paste(log_lines, collapse = "\n")
  finished_all <- grepl("Done!", log_text, fixed = TRUE)

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

  strval <- function(chunk, pattern) {
    match <- regmatches(chunk, regexec(pattern, chunk))[[1]]
    if (length(match) > 1) {
      value <- trimws(match[2])
      if (identical(value, "NA") || !nzchar(value)) NA_character_ else value
    } else {
      NA_character_
    }
  }

  # Extracts completion timestamps and overall duration for each strain pipeline pass
  ts_pattern <-
    "\\[INFO: ([0-9]{4}-[0-9]{2}-[0-9]{2} [0-9]{2}:[0-9]{2}:[0-9]{2}[.,][0-9]+)\\]"
  timing <- function(chunk) {
    to_posix <- function(x) {
      as.POSIXct(gsub(",", ".", x), format = "%Y-%m-%d %H:%M:%OS")
    }
    done <- regmatches(
      chunk,
      regexec(paste0(ts_pattern, "[ \t]*DONE"), chunk)
    )[[1]]
    done_ts <- if (length(done) > 1) to_posix(done[2]) else as.POSIXct(NA)
    total_elapsed <- num(chunk, "Strain elapsed:[ \t]*([0-9]+)")
    total_finished <- strval(chunk, "Strain finished:[ \t]*([0-9:]+)")
    list(
      finished = total_finished,
      elapsed = if (!is.na(total_elapsed)) {
        as.numeric(total_elapsed)
      } else {
        NA_real_
      },
      cg_done = !is.na(done_ts)
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
      elapsed = times$elapsed,
      cg_done = times$cg_done
    )

    cla_st <- strval(chunk, "Classical MLST ST:[ \t]*(\\S+)")
    cla_alleles <- strval(chunk, "Classical MLST alleles:[ \t]*([^\n]+)")
    metrics$st <- cla_st
    metrics$alleles <- cla_alleles
    metrics$cla_status <- clamlst_status(cla_st, cla_alleles)

    amr_elements <- num(chunk, "AMR: done .* \\(([0-9]+) elements\\)")
    metrics$amr_elements <- amr_elements
    metrics$amr_status <- if (!is.na(amr_elements)) {
      "done"
    } else if (grepl("AMR: failed", chunk, fixed = TRUE)) {
      "failed"
    } else if (grepl("AMR: screening", chunk, fixed = TRUE)) {
      "screening"
    } else {
      NA_character_
    }
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
      return(outcome("Added", ""))
    }

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
        cg_done = FALSE,
        detail = "Waiting in queue ...",
        st = NA_character_,
        alleles = NA_character_,
        cla_status = NA_character_,
        amr_status = NA_character_,
        amr_elements = NA_integer_
      )
    } else {
      # A strain entry is considered complete when another strain follows it,
      # the global run finishes, or the explicit end-of-strain marker is printed.
      idx <- match(strain, order_seen)
      chunk <- sections[[strain]]
      complete <- idx < length(order_seen) ||
        finished_all ||
        grepl("Strain finished:", chunk, fixed = TRUE)
      info <- classify(chunk, complete)
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
      cg_done = info$cg_done,
      detail = info$detail,
      st = info$st,
      alleles = info$alleles,
      cla_status = info$cla_status,
      amr_status = info$amr_status,
      amr_elements = info$amr_elements,
      stringsAsFactors = FALSE
    )
  })

  do.call(rbind, rows)
}

#' Parse Classical MLST Provenance Metadata
#'
#' @description Extracts run-level classical MLST scheme details, repository sources,
#'   and tool versions from typing output logs.
#'
#' @param log_lines Character vector containing log output lines.
#'
#' @return A list with elements `repository`, `scheme`, `scheme_version`, and `pymlst_version`.
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

#' Query Target Species from Database
#'
#' @description Reads the species associated with the database's schema from the `mlst_type` table.
#'
#' @param db_path Character string. File path to SQLite database.
#'
#' @return Character string of target species name, or `NA_character_` if unavailable.
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

# Reads allele reference sequences and database schema migration markers from the intermediate claMLST database
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

#' Store Classical MLST Results in Database
#'
#' @description Persists classical MLST calls, locus allele profiles, and execution
#'   provenance metadata into the `classical_mlst` database table within an atomic transaction.
#'
#' @param db_path Character string. File path to destination SQLite database.
#' @param results Data frame of parsed typing outcomes from `parse_typing_log()`.
#' @param cla_db_path Character string (optional). Path to temporary classical MLST reference database.
#' @param identity Numeric (optional). Sequence identity threshold used during execution.
#' @param coverage Numeric (optional). Coverage threshold used during execution.
#' @param repository Character string (optional). Repository source name.
#' @param scheme Character string (optional). Classical scheme identifier.
#' @param scheme_version Character string (optional). Classical scheme version.
#' @param pymlst_version Character string (optional). Executable tool version string.
#'
#' @return Logical indicating transaction success invisibly.
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

  # Retain registered, novel, and partial profiles; drop only completely missing calls
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

  refs <- clamlst_refs(cla_db_path)
  seq_for <- function(gene, allele) {
    if (is.na(gene) || is.na(allele) || !length(refs$sequences)) {
      return(NA_character_)
    }
    val <- refs$sequences[[paste(gene, allele, sep = "\t")]]
    if (is.null(val)) NA_character_ else val
  }
  alembic_version <- refs$alembic

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
    ok <- !is.na(gene) & nzchar(gene) & !is.na(allele) & nzchar(allele)
    data.frame(gene = gene[ok], allele = allele[ok], stringsAsFactors = FALSE)
  }

  con <- dbConnect(SQLite(), db_path, busy_timeout = 5000)
  on.exit(dbDisconnect(con))

  dbExecute(
    con,
    "CREATE TABLE IF NOT EXISTS classical_mlst (
       id INTEGER PRIMARY KEY AUTOINCREMENT,
       isolate TEXT,
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
  n_iso <- 0L
  n_rows <- 0L
  dbBegin(con)
  ok <- tryCatch(
    {
      for (i in seq_len(nrow(results))) {
        isolate <- as.character(results$strain[i])
        status <- status_vec[i]
        st <- if (identical(status, "known")) {
          as.character(results$st[i])
        } else {
          NA_character_
        }
        spec <- if (has_alleles) {
          as.character(results$alleles[i])
        } else {
          NA_character_
        }
        genes <- split_alleles(spec)

        if (!nrow(genes)) {
          next
        }

        # Replace existing records for this strain to avoid duplicate entries upon re-typing
        dbExecute(
          con,
          "DELETE FROM classical_mlst WHERE isolate = ?",
          params = list(isolate)
        )
        n_iso <- n_iso + 1L
        n_rows <- n_rows + nrow(genes)

        for (j in seq_len(nrow(genes))) {
          dbExecute(
            con,
            "INSERT INTO classical_mlst
               (isolate, gene, allele, sequence, st, status, scheme,
                scheme_version, alembic_version, repository, identity, coverage,
                pymlst_version, called_at)
             VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)",
            params = list(
              isolate,
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
  if (isTRUE(ok)) {
    dbCommit(con)
  } else {
    dbRollback(con)
  }

  log_event(
    "DB",
    "classical_mlst",
    sprintf(
      "%d isolate(s), %d allele row(s) %s",
      n_iso,
      n_rows,
      if (isTRUE(ok)) "written" else "failed (rolled back)"
    )
  )

  invisible(ok)
}

#' Query Stored Strain Names
#'
#' @description Retrieves unique strain names present in the database, excluding the
#'   synthetic `"ref"` core genome entry.
#'
#' @param db_path Character string. File path to SQLite database.
#'
#' @return Character vector of distinct strain names.
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

#' Get Total Scheme Locus Count
#'
#' @description Queries the number of loci in the scheme by counting reference rows
#'   assigned to the synthetic strain `"ref"`.
#'
#' @param db_path Character string. Target SQLite database path.
#'
#' @return Integer total locus count, or `NA_integer_` if unreadable.
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

#' Query Called Loci Count per Strain
#'
#' @description Returns the number of successfully called loci for each specified
#'   strain to assess typing completeness.
#'
#' @param db_path Character string. Target SQLite database path.
#' @param strains Character vector of strain names to query.
#'
#' @return Named integer vector aligned with `strains`.
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

#' Check Pending Sequence Hashes
#'
#' @description Checks if sequence SHA256 hashes are missing or out of sync with the `sequences` table.
#'
#' @param db_path Character string. Target SQLite database file path.
#'
#' @return Logical indicating whether `hash_database()` needs to run.
#' @export
hashes_pending <- function(db_path) {
  con <- dbConnect(
    SQLite(),
    db_path,
    synchronous = NULL,
    busy_timeout = 5000
  )
  on.exit(dbDisconnect(con))

  tables <- dbListTables(con)
  if (!any("sequences" == tables)) {
    return(FALSE)
  }
  if (!any("hashes" == tables)) {
    return(TRUE)
  }

  n_seq <- dbGetQuery(con, "SELECT COUNT(*) AS n FROM sequences")$n
  n_hash <- dbGetQuery(con, "SELECT COUNT(*) AS n FROM hashes")$n
  n_seq != n_hash
}

#' Populate Database Sequence Hashes
#'
#' @description Computes SHA256 hashes for unindexed sequence entries and updates the `hashes` table.
#'
#' @param db_path Character string. Target SQLite database file path.
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
  updated <- FALSE

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
      updated <- TRUE
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
    updated <- TRUE
  }

  # Avoid rewriting the table or modifying file mtime if no new hashes were generated
  if (updated) {
    dbWriteTable(con, "hashes", hash_table, overwrite = TRUE)
    log_event(
      "DB",
      "hashes",
      sprintf("rewritten, %d sequence hash(es)", nrow(hash_table))
    )
  }
}
