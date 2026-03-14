library(RSQLite)
library(DBI)
library(processx)
library(openssl)
library(tidyr)
library(dplyr)

### Download cgmlst scheme
download_cgmlst_scheme <- function(scheme, db_path) {
  download_status <- processx::run(
    command = "conda",
    args = c(
      "run",
      "-n",
      "pymlst",
      "wgMLST",
      "import",
      "--no-prompt",
      basename(db_path),
      scheme
    ),
    wd = dirname(db_path),
    echo_cmd = TRUE,
    echo = TRUE,
    stderr_to_stdout = TRUE
  )

  return(download_status)
}

### Read database
read_database <- function(db_path) {
  message(paste(
    "Reading",
    basename(db_path),
    "from",
    dirname(db_path),
    "..."
  ))

  # Connect to database
  con <- DBI::dbConnect(RSQLite::SQLite(), db_path)

  # List tables
  tables <- DBI::dbListTables(con)

  # Iterate over tables to summarize in list
  database <- list()
  for (table in tables) {
    database[[table]] <- DBI::dbReadTable(con, table)
  }

  # Digest new sequences with SHA-256
  database <- add_sequence_hashes(database)

  # Disconnect from the database
  DBI::dbDisconnect(con)

  return(database)
}

### Synchronize local changes with remote database
synchronize_database <- function(database, db_path) {
  # Connect to remote database
  con <- DBI::dbConnect(RSQLite::SQLite(), db_path)

  # Write local changes
  for (table in names(database)) {
    dbWriteTable(con, table, database[[table]], overwrite = TRUE)
  }

  # Disconnect from the database
  DBI::dbDisconnect(con)
}

### Typing isolates
type_genomes <- function(
  database,
  db_path,
  genome_input,
  script_path = "bin/loop-pymlst.sh",
  identity = 0.95,
  coverage = 0.9
) {
  # Setup paths and arguments
  cmd_args <- c("-d", basename(db_path))
  if (dir.exists(genome_input)) {
    cmd_args <- c(cmd_args, "-g", genome_input)
  } else if (file.exists(genome_input)) {
    cmd_args <- c(cmd_args, "-f", genome_input)
  }
  cmd_args <- c(
    cmd_args,
    "-i",
    as.character(identity),
    "-c",
    as.character(coverage)
  )

  # Run the process
  typing_status <- processx::run(
    command = script_path,
    args = cmd_args,
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

add_sequence_hashes <- function(database) {
  database$sequences$sha256 <- vapply(
    database$sequences$sequence,
    function(x) {
      if (is.na(x)) {
        NA_character_
      } else {
        as.character(openssl::sha256(chartr("", "", x)))
      }
    },
    character(1)
  )

  return(database)
}

get_isolate_allele_profiles <- function(database) {
  # Pivoting by genes and joining with sequences
  mlst_wide <- database$mlst |>
    dplyr::select(souche, gene, seqid) |>
    dplyr::left_join(
      database$sequences[, c("id", "sha256")],
      by = c("seqid" = "id")
    ) |>
    dplyr::select(souche, gene, sha256) |>
    tidyr::pivot_wider(names_from = gene, values_from = sha256)

  return(mlst_wide)
}

stage_genomes <- function(database) {
  # Validate staged genomes
  # https://pymlst.readthedocs.io/en/latest/documentation/cgmlst/check.html#validate-strains
}

remove_genomes <- function(database) {
  # https://pymlst.readthedocs.io/en/latest/documentation/cgmlst/check.html#remove-strains-or-genes
}

push_genomes <- function(database) {
  # Merge new genomes with existing database
}

mlst_profile <- function(database) {
  # Get MLST profile
  # https://pymlst.readthedocs.io/en/latest/documentation/cgmlst/export_res.html#mlst
}
