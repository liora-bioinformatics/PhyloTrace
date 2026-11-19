# app/logic/pymlst.R

box::use(
  DBI[
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
  openssl[sha256],
  processx[process],
)
box::use(
  app / logic / db_connect[connect],
  app / logic / logging[log_event],
)

#' Classical MLST Results Table
#'
#' @description Definition of the per-isolate classical MLST results table;
#'   shared with the database merge, which creates it from this when missing.
#' @export
CLASSICAL_MLST_DDL <- "CREATE TABLE IF NOT EXISTS classical_mlst (
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

# Central Conda environment where all external bioinformatics CLI dependencies
# (cgMLST, classical MLST, BLAT, MAFFT, abritamr, AMRFinderPlus) reside.
#' Central Conda Environment Name
#'
#' @description Defines the central Conda environment name containing external CLI tools.
#' @export
conda_env <- "PhyloTrace"

# BLAT search thresholds, kept as pyMLST's own CLI defaults rather than one
# shared pair: `wgMLST add` defaults to 0.95 identity, `claMLST search` to 0.9,
# and the looser classical cutoff is deliberate. Allele calling searches
# thousands of loci, so a strict identity costs little; the classical search has
# seven, each sought from one fixed reference allele (pyMLST's `open_cla(ref=1)`
# always seeds from allele 1), so a locus whose allele 1 happens to be divergent
# - Oxf_gpi in the Acinetobacter baumannii Oxford scheme sits at ~93% against
# typical genomes - misses a 0.95 cutoff and is reported uncalled.

#' Default cgMLST Allele Calling Identity
#'
#' @description Minimum BLAT identity for `wgMLST add`, as pyMLST defaults it.
#' @export
CG_IDENTITY_DEFAULT <- 0.95

#' Default cgMLST Allele Calling Coverage
#'
#' @description Minimum BLAT coverage for `wgMLST add`, as pyMLST defaults it.
#' @export
CG_COVERAGE_DEFAULT <- 0.9

#' Default Classical MLST Search Identity
#'
#' @description Minimum BLAT identity for `claMLST search`, as pyMLST defaults it.
#' @export
CLA_IDENTITY_DEFAULT <- 0.9

#' Default Classical MLST Search Coverage
#'
#' @description Minimum BLAT coverage for `claMLST search`, as pyMLST defaults it.
#' @export
CLA_COVERAGE_DEFAULT <- 0.9

#' Resolve Bash Executable Path
#'
#' @description Resolves the system path for `bash` by checking standard install
#'   locations directly rather than relying on PATH lookup, since IDE-launched R
#'   sessions (e.g. Positron) may not inherit the full PATH of the shell they were
#'   started from, causing plain `"bash"` to fail process lookup.
#'
#' @return Character string specifying the bash executable path.
#' @export
bash_exe <- function() {
  candidates <- c("/usr/bin/bash", "/bin/bash")
  found <- candidates[file.exists(candidates)]
  if (length(found) > 0) {
    return(found[1])
  }
  "bash"
}

# Strips what `conda run` would read as shell syntax: it rebuilds a command
# line from its arguments, and a database from elsewhere may hold anything in
# the scheme species or version.
conda_safe <- function(x) gsub("[$`\\\\\"'\r\n]", "", x)

# Constructs command-line arguments for loop-pymlst.sh.
# Explicit genome file paths are passed after a `--` separator to prevent file names
# from being parsed as CLI options. Optional parameters (classical MLST, AMR
# screening) are appended only when non-empty strings are provided. Classical
# MLST needs both the reference path to build into and the folder
# mlst_scheme$materialize_mlst_scheme() wrote the stored scheme to.
typing_args <- function(
  db_path,
  genome_files,
  identity,
  coverage,
  cla_identity = CLA_IDENTITY_DEFAULT,
  cla_coverage = CLA_COVERAGE_DEFAULT,
  cla_db = NA_character_,
  cla_dir = NA_character_,
  cla_species = NA_character_,
  cla_version = NA_character_,
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
    as.character(coverage)
  )
  scalar_chr <- function(x) {
    !is.null(x) && length(x) == 1 && !is.na(x) && nzchar(x)
  }
  scalar_num <- function(x) {
    !is.null(x) && length(x) == 1 && !is.na(x) && is.finite(as.numeric(x))
  }
  if (scalar_chr(cla_db) && scalar_chr(cla_dir)) {
    args <- c(args, "-m", cla_db, "-M", cla_dir)
    if (scalar_chr(cla_species) && nzchar(conda_safe(cla_species))) {
      args <- c(args, "-s", conda_safe(cla_species))
    }
    if (scalar_chr(cla_version) && nzchar(conda_safe(cla_version))) {
      args <- c(args, "-V", conda_safe(cla_version))
    }
    # The classical search has thresholds of its own; the script falls back to
    # the cgMLST pair only when they are not given.
    if (scalar_num(cla_identity)) {
      args <- c(args, "-I", as.character(cla_identity))
    }
    if (scalar_num(cla_coverage)) {
      args <- c(args, "-C", as.character(cla_coverage))
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

#' Launch Background Typing Process
#'
#' @description Executes `loop-pymlst.sh` as a detached, non-blocking background process
#'   and redirects standard output/error streams to a log file for live polling.
#'
#' @param db_path Character string. Path to target SQLite database.
#' @param genome_files Character vector. Input genome assembly file paths.
#' @param log_file Character string. Output log destination file path.
#' @param script_path Character string. Path to `loop-pymlst.sh`. Defaults to `"app/logic/loop-pymlst.sh"`.
#' @param identity Numeric. cgMLST BLAT identity cutoff (0 to 1). Defaults to `CG_IDENTITY_DEFAULT`.
#' @param coverage Numeric. cgMLST BLAT coverage cutoff (0 to 1). Defaults to `CG_COVERAGE_DEFAULT`.
#' @param env Character string. Conda environment name. Defaults to `conda_env`.
#' @param cla_identity Numeric. Classical MLST BLAT identity cutoff.
#'   Defaults to `CLA_IDENTITY_DEFAULT`.
#' @param cla_coverage Numeric. Classical MLST BLAT coverage cutoff.
#'   Defaults to `CLA_COVERAGE_DEFAULT`.
#' @param cla_db Character string (optional). File path for intermediate classical MLST database.
#' @param cla_dir Character string (optional). Folder holding the stored scheme
#'   as written by `materialize_mlst_scheme()` (`profiles.tsv`, `locus/*.fasta`).
#' @param cla_species Character string (optional). Species recorded in the
#'   intermediate classical MLST database.
#' @param cla_version Character string (optional). Scheme version recorded there.
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
  identity = CG_IDENTITY_DEFAULT,
  coverage = CG_COVERAGE_DEFAULT,
  env = conda_env,
  cla_identity = CLA_IDENTITY_DEFAULT,
  cla_coverage = CLA_COVERAGE_DEFAULT,
  cla_db = NA_character_,
  cla_dir = NA_character_,
  cla_species = NA_character_,
  cla_version = NA_character_,
  amr_env = NA_character_,
  amr_species = NA_character_,
  amr_out = NA_character_
) {
  # processx has no append mode for `stdout` (a plain path is opened truncated,
  # and it silently fails to start the process at all if given a `>>` prefix -
  # that syntax is shell redirection, not something processx's C exec layer
  # understands). So the append the caller relies on - preserving a note
  # already written into a fresh log_file before launching - is done by bash
  # itself: `exec` replaces the wrapper shell with the real script in place
  # (so cleanup_tree still tracks a single process), while "$0" "$@" hand the
  # script path and every typing arg through untouched, exactly like passing
  # them as a plain argv vector would.
  process$new(
    command = bash_exe(),
    args = c(
      "-c",
      paste("exec \"$0\" \"$@\" >>", shQuote(log_file), "2>&1"),
      normalizePath(script_path, mustWork = TRUE),
      typing_args(
        db_path,
        genome_files,
        identity,
        coverage,
        cla_identity,
        cla_coverage,
        cla_db,
        cla_dir,
        cla_species,
        cla_version,
        amr_env,
        amr_species,
        amr_out
      )
    ),
    wd = dirname(db_path),
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
#' The allele profile decides the category, not the ST field: `claMLST search`
#' does not require a unique ST before printing one. Whenever a locus is missing
#' it intersects only the loci it did call, so its ST column can hold every
#' candidate profile compatible with the partial call, semicolon-separated
#' (e.g. `"1953;3684;11;..."`). Such a list is not a Sequence Type and must never
#' be read as one. The same separator appears inside a locus when one gene
#' matched several alleles.
#'
#' Profile categories:
#' - `known`: All loci called with registered alleles and exactly one matching ST.
#' - `novel`: All loci called, but the profile is unregistered or carries a new allele.
#' - `partial`: At least one locus failed to call.
#' - `ambiguous`: Loci fully called, yet the search could not settle on one ST.
#' - `NA`: No usable loci called.
#'
#' @param st Character string or numeric. Sequence Type assigned by search.
#' @param alleles Character string. Comma-delimited locus-allele key-value string (`"gene=allele"`).
#'
#' @return Character string (`"known"`, `"novel"`, `"partial"`, `"ambiguous"`, or
#'   `NA_character_`).
#' @export
clamlst_status <- function(st, alleles) {
  st_vals <- if (is.null(st) || length(st) != 1 || is.na(st)) {
    character(0)
  } else {
    vals <- trimws(strsplit(as.character(st), ";", fixed = TRUE)[[1]])
    vals[nzchar(vals)]
  }

  # Without a profile to check against, only a single ST can be trusted.
  if (
    is.null(alleles) ||
      length(alleles) != 1 ||
      is.na(alleles) ||
      !nzchar(alleles)
  ) {
    return(switch(
      as.character(length(st_vals)),
      "0" = NA_character_,
      "1" = "known",
      "ambiguous"
    ))
  }

  pairs <- strsplit(alleles, ",", fixed = TRUE)[[1]]
  vals <- trimws(sub("^[^=]*=", "", pairs))
  n_present <- sum(nzchar(vals))
  if (n_present == 0) {
    return(NA_character_)
  }
  if (n_present < length(vals)) {
    return("partial")
  }
  # A locus reported as "new" has no registered allele, so the profile is novel
  # by definition; several alleles at one locus leave the profile undecided.
  if (any(vals == "new")) {
    return("novel")
  }
  if (any(grepl(";", vals, fixed = TRUE))) {
    return("ambiguous")
  }
  switch(
    as.character(length(st_vals)),
    "0" = "novel",
    "1" = "known",
    "ambiguous"
  )
}

# Splits the "gene=allele,gene=allele" string the typing script logs into a
# data frame, keeping loci that were not called (empty allele).
parse_allele_spec <- function(spec) {
  empty <- data.frame(
    gene = character(0),
    allele = character(0),
    stringsAsFactors = FALSE
  )
  if (is.null(spec) || length(spec) != 1 || is.na(spec) || !nzchar(spec)) {
    return(empty)
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
  ok <- !is.na(gene) & nzchar(gene)
  data.frame(gene = gene[ok], allele = allele[ok], stringsAsFactors = FALSE)
}

# Locus names travel through pyMLST's clean_geneid() (underscores become
# hyphens), so profile columns and reported loci are compared stripped down.
norm_locus <- function(x) gsub("[^[:alnum:]]", "", tolower(x))

#' Look Up a Sequence Type in the Scheme's Profile Table
#'
#' @description Resolves an allele profile against the scheme's own profile
#'   table.
#'
#' @details
#' A repository records a locus that is absent from a lineage as allele `0` -
#' Enterococcus faecium carries 35 such STs, the pstS deletion. `claMLST search`
#' cannot assign those: pyMLST drops allele `0` rows when it builds the
#' reference, so a genome without pstS matches every ST sharing its other six
#' alleles and comes back with a list instead of a call. Reading the profile
#' table directly settles it - an uncalled locus matches the `0` the scheme uses
#' for exactly that situation - and it is only ever accepted when a single row
#' matches.
#'
#' @param alleles Character string. Comma-delimited `"gene=allele"` string, empty
#'   allele for an uncalled locus.
#' @param profiles Data frame of the scheme's profiles: `ST` first, one
#'   character column per locus (`materialize_mlst_scheme()`'s `profiles`).
#'
#' @return The Sequence Type as a character string, or `NA_character_`.
#' @export
st_from_profile <- function(alleles, profiles) {
  called <- parse_allele_spec(alleles)
  if (is.null(profiles) || !nrow(profiles) || !nrow(called)) {
    return(NA_character_)
  }
  # A new or multiply-hit allele has no profile to match against.
  if (any(called$allele == "new" | grepl(";", called$allele, fixed = TRUE))) {
    return(NA_character_)
  }

  columns <- match(norm_locus(called$gene), norm_locus(names(profiles)))
  if (anyNA(columns)) {
    return(NA_character_)
  }

  keep <- rep(TRUE, nrow(profiles))
  for (i in seq_len(nrow(called))) {
    wanted <- if (nzchar(called$allele[i])) called$allele[i] else "0"
    keep <- keep & trimws(profiles[[columns[i]]]) == wanted
  }
  hits <- which(keep)
  if (length(hits) != 1) {
    return(NA_character_)
  }
  trimws(as.character(profiles[[1]][hits]))
}

#' Resolve a Classical MLST Call
#'
#' @description Combines what `claMLST search` reported with a profile-table
#'   lookup into the Sequence Type to store and how it was reached.
#'
#' @details
#' The search's own call is authoritative whenever it settled on one ST. The
#' profile table is consulted only for the cases it left undetermined, and a
#' single matching row there is reported as `inferred` - the scheme's own
#' profile, reached without the search's help.
#'
#' @param st Character string. ST field as printed by `claMLST search`.
#' @param alleles Character string. Comma-delimited `"gene=allele"` string.
#' @param profiles Data frame of the scheme's profiles (see `st_from_profile()`),
#'   or `NULL`.
#'
#' @return A list with `st` (character or `NA`) and `status`.
#' @export
resolve_clamlst_call <- function(st, alleles, profiles = NULL) {
  status <- clamlst_status(st, alleles)
  if (identical(status, "known")) {
    return(list(st = trimws(as.character(st)), status = status))
  }
  if (!is.na(status)) {
    hit <- st_from_profile(alleles, profiles)
    if (!is.na(hit)) {
      return(list(st = hit, status = "inferred"))
    }
  }
  list(st = NA_character_, status = status)
}

#' Parse Typing Run Logs into Structured Table
#'
#' @description Parses raw output logs from `loop-pymlst.sh` into a structured
#'   per-strain summary data frame used for real-time progress monitoring and reporting.
#'
#' @param log_lines Character vector or scalar string containing raw log lines.
#' @param strains Character vector of expected strain identifiers (assembly names without extension).
#' @param profiles Data frame of the scheme's profiles (optional, see
#'   `st_from_profile()`). Resolves the
#'   Sequence Types `claMLST search` leaves undetermined.
#'
#' @return A `data.frame` containing metrics, execution timing, classical MLST calls,
#'   and AMR status per strain.
#' @export
parse_typing_log <- function(log_lines, strains, profiles = NULL) {
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
    call <- resolve_clamlst_call(cla_st, cla_alleles, profiles)
    metrics$st <- call$st
    metrics$alleles <- cla_alleles
    metrics$cla_status <- call$status

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

#' Per-Strain Allele Calling Outcome
#'
#' @description The cgMLST verdict for each strain of a parsed run, known before
#'   the strain's whole pipeline is over.
#'
#' @details
#' A strain's log section only closes once the steps behind allele calling report
#' in, so `status` still reads "Running" all through the classical MLST search
#' and the AMR screen. Allele calling itself is over the moment pyMLST prints its
#' DONE line, and only a successful run reaches it, so this can never pre-empt a
#' failure verdict. It is what says whether the mother database has accepted the
#' isolate - the condition for storing anything keyed to it.
#'
#' @param results Data frame from `parse_typing_log()`.
#'
#' @return Character vector of outcomes, parallel to `results`.
#' @export
cg_outcome <- function(results) {
  if (is.null(results) || !nrow(results)) {
    return(character(0))
  }
  cg_done <- if ("cg_done" %in% names(results)) {
    results$cg_done
  } else {
    rep(FALSE, nrow(results))
  }
  ifelse(results$status == "Running" & cg_done, "Added", results$status)
}

#' Parse Allele Calling Tool Versions
#'
#' @description Extracts the versions of the tools behind allele calling from a
#'   typing log: pyMLST, which drives both allele calling and the ST search, and
#'   the BLAT / MAFFT binaries it finds and aligns loci with.
#'
#' @param log_lines Character vector containing log output lines.
#'
#' @return A list with elements `pymlst`, `blat` and `mafft`.
#' @export
parse_tool_meta <- function(log_lines) {
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
    pymlst = grab("pyMLST version:[ \t]*([^\n]+)"),
    blat = grab("BLAT version:[ \t]*([^\n]+)"),
    mafft = grab("MAFFT version:[ \t]*([^\n]+)")
  )
}

#' Query Target Species from Database
#'
#' @description Reads the species associated with the database's schema from the
#'   `mlst_type` table, as the user entered it when the database was created.
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

  con <- connect(db_path, synchronous = NULL)
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

#' Read the Schema Marker of a claMLST Reference Database
#'
#' @description Reads the alembic migration marker out of a run's interim
#'   claMLST database: the pyMLST schema the reference was built under.
#'
#' @param cla_db_path Character path to the interim claMLST database.
#'
#' @return Character string, or `NA_character_` when unreadable.
#' @export
clamlst_alembic <- function(cla_db_path) {
  if (
    is.null(cla_db_path) ||
      length(cla_db_path) != 1 ||
      is.na(cla_db_path) ||
      !nzchar(cla_db_path) ||
      !file.exists(cla_db_path)
  ) {
    return(NA_character_)
  }

  con <- tryCatch(
    connect(cla_db_path, synchronous = NULL),
    error = function(e) NULL
  )
  if (is.null(con)) {
    return(NA_character_)
  }
  on.exit(dbDisconnect(con))

  av <- tryCatch(
    dbGetQuery(con, "SELECT version_num FROM alembic_version LIMIT 1"),
    error = function(e) NULL
  )
  if (is.null(av) || !nrow(av)) {
    return(NA_character_)
  }
  as.character(av$version_num[1])
}

#' Store Classical MLST Results in Database
#'
#' @description Persists classical MLST calls, locus allele profiles, and execution
#'   provenance metadata into the `classical_mlst` database table within an atomic transaction.
#'
#' @param db_path Character string. File path to destination SQLite database.
#' @param results Data frame of parsed typing outcomes from `parse_typing_log()`.
#' @param identity Numeric (optional). Sequence identity threshold used during execution.
#' @param coverage Numeric (optional). Coverage threshold used during execution.
#' @param repository Character string (optional). Repository source name.
#' @param scheme Character string (optional). Classical scheme identifier.
#' @param scheme_version Character string (optional). Classical scheme version.
#' @param pymlst_version Character string (optional). Executable tool version string.
#' @param refs List (optional) with `sequences` - the scheme's allele sequences,
#'   named `gene<TAB>allele` (`materialize_mlst_scheme()`'s `sequences`) - and
#'   `alembic`, the reference's schema marker (`clamlst_alembic()`). Without it
#'   no sequence or marker is stored.
#'
#' @return Logical indicating transaction success invisibly.
#' @export
store_clamlst_results <- function(
  db_path,
  results,
  identity = NA_real_,
  coverage = NA_real_,
  repository = NA_character_,
  scheme = NA_character_,
  scheme_version = NA_character_,
  pymlst_version = NA_character_,
  refs = NULL
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

  # `parse_typing_log()` has already resolved every call (including the profile
  # lookup, which needs the scheme's profile table); its verdict is taken as is
  # so a call is never classified twice. Frames without it - anything not coming
  # from the log parser - are classified here from the search output alone.
  # Registered, inferred, novel and partial profiles are all retained; only
  # completely missing calls are dropped.
  status_vec <- if ("cla_status" %in% names(results)) {
    as.character(results$cla_status)
  } else {
    vapply(
      seq_len(nrow(results)),
      function(i) {
        clamlst_status(
          results$st[i],
          if (has_alleles) results$alleles[i] else NA_character_
        )
      },
      character(1)
    )
  }
  keep <- !is.na(status_vec)
  results <- results[keep, , drop = FALSE]
  status_vec <- status_vec[keep]
  if (!nrow(results)) {
    return(invisible(FALSE))
  }

  seq_for <- function(gene, allele) {
    if (is.na(gene) || is.na(allele) || !length(refs$sequences)) {
      return(NA_character_)
    }
    # Single-bracket lookup: an allele the reference does not hold ("new", a
    # multi-hit "3;7") must come back NA. `[[` throws on a missing name, which
    # rolled back the isolate's whole call.
    unname(refs$sequences[paste(gene, allele, sep = "\t")])
  }
  alembic_version <- if (is.null(refs$alembic)) NA_character_ else refs$alembic

  # Only loci that were actually called get a row; an uncalled locus is absent
  # from the table rather than stored as an empty allele.
  split_alleles <- function(spec) {
    called <- parse_allele_spec(spec)
    called[nzchar(called$allele), , drop = FALSE]
  }

  con <- connect(db_path)
  on.exit(dbDisconnect(con))

  dbExecute(con, CLASSICAL_MLST_DDL)

  now <- as.character(Sys.time())
  n_iso <- 0L
  n_rows <- 0L
  dbBegin(con)
  ok <- tryCatch(
    {
      for (i in seq_len(nrow(results))) {
        isolate <- as.character(results$strain[i])
        status <- status_vec[i]
        # A Sequence Type is stored only where one was actually determined -
        # never the candidate list `claMLST search` prints for a call it could
        # not settle.
        st <- if (status %in% c("known", "inferred")) {
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

  con <- connect(db_path, synchronous = NULL)
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

  con <- connect(db_path, synchronous = NULL)
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

  con <- connect(db_path, synchronous = NULL)
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
  # A path that no longer resolves (database deleted or moved mid-session) would
  # otherwise make connect() create an empty file here; treat it as "nothing to do".
  if (
    is.null(db_path) ||
      length(db_path) != 1 ||
      is.na(db_path) ||
      !file.exists(db_path)
  ) {
    return(FALSE)
  }

  con <- connect(db_path, synchronous = NULL)
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

  # Bail out quietly when the database is gone or is not a pyMLST file: the load
  # and reload paths call this before any module reads, so an uncaught error
  # here (e.g. "no such table: sequences" on a database deleted mid-session)
  # would abort the whole Shiny session.
  if (
    is.null(db_path) ||
      length(db_path) != 1 ||
      is.na(db_path) ||
      !file.exists(db_path)
  ) {
    message("Database file not found; skipping hash maintenance")
    return(invisible(FALSE))
  }

  con <- connect(db_path, synchronous = NULL)
  on.exit(dbDisconnect(con))

  if (!any("sequences" == dbListTables(con))) {
    message("Database has no 'sequences' table; skipping hash maintenance")
    return(invisible(FALSE))
  }

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
