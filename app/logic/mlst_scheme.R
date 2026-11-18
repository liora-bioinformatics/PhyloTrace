# app/logic/mlst_scheme.R
#
# The classical (7-gene) MLST scheme a database carries. The user imports it
# once - profile table plus allele FASTAs - and it is stored inside the database
# itself, so typing never contacts a repository and an exported database keeps
# its scheme. For each typing run the stored scheme is written back out as the
# files `claMLST create` builds its reference from (see loop-pymlst.sh).

box::use(
  DBI[
    dbAppendTable,
    dbBegin,
    dbCommit,
    dbDisconnect,
    dbExecute,
    dbGetQuery,
    dbListTables,
    dbRollback,
  ],
  rlang[`%||%`],
  stats[complete.cases],
  utils[head, write.table],
)

box::use(
  app / logic / db_connect[connect],
  app / logic / logging[log_event],
  app / logic / scheme_files[pymlst_gene_id, valid_locus_name],
)

#' Classical MLST Scheme Tables
#'
#' @description Tables holding the stored scheme, copied along with the cgMLST
#'   scheme tables when a database is exported.
#' @export
MLST_SCHEME_TABLES <- c(
  "cla_scheme_info",
  "cla_scheme_loci",
  "cla_scheme_profiles",
  "cla_scheme_alleles"
)

MLST_SCHEME_DDL <- c(
  "CREATE TABLE cla_scheme_info (key TEXT PRIMARY KEY, value TEXT)",
  "CREATE TABLE cla_scheme_loci (position INTEGER PRIMARY KEY, locus TEXT NOT NULL UNIQUE)",
  paste(
    "CREATE TABLE cla_scheme_profiles (st INTEGER NOT NULL, locus TEXT NOT NULL,",
    "allele INTEGER NOT NULL, PRIMARY KEY (st, locus)) WITHOUT ROWID"
  ),
  paste(
    "CREATE TABLE cla_scheme_alleles (locus TEXT NOT NULL, allele INTEGER NOT NULL,",
    "sequence TEXT NOT NULL, PRIMARY KEY (locus, allele)) WITHOUT ROWID"
  )
)

# Keeps only scalar, non-empty text values of a key/value list.
info_rows <- function(info) {
  values <- vapply(
    info,
    function(v) if (length(v) == 1L && !is.na(v)) as.character(v) else NA_character_,
    character(1)
  )
  keep <- !is.na(values) & nzchar(values)
  data.frame(key = names(info)[keep], value = unname(values[keep]), stringsAsFactors = FALSE)
}

#' Write a Classical MLST Scheme into an Open Database
#'
#' @description Replaces whatever scheme the database held. Runs inside the
#'   caller's transaction, so a failure leaves the previous scheme intact.
#'
#' @param con DBI connection.
#' @param scheme List from `scheme_files$assemble_mlst_scheme()`.
#' @param info Named list of scheme details (`name`, `source`, `source_label`,
#'   `version`, `species`, `url`, `citation`, ...).
#'
#' @return Invisible `TRUE`.
#' @export
write_mlst_scheme <- function(con, scheme, info) {
  for (table in MLST_SCHEME_TABLES) {
    dbExecute(con, sprintf("DROP TABLE IF EXISTS %s", table))
  }
  for (ddl in MLST_SCHEME_DDL) {
    dbExecute(con, ddl)
  }
  info$imported_at <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  info$loci <- length(scheme$loci)
  info$profiles <- length(unique(scheme$profiles$st))
  info$alleles <- nrow(scheme$alleles)
  dbAppendTable(con, "cla_scheme_info", info_rows(info))
  dbAppendTable(
    con,
    "cla_scheme_loci",
    data.frame(position = seq_along(scheme$loci), locus = scheme$loci)
  )
  dbAppendTable(con, "cla_scheme_profiles", scheme$profiles[, c("st", "locus", "allele")])
  dbAppendTable(con, "cla_scheme_alleles", scheme$alleles[, c("locus", "allele", "sequence")])
  invisible(TRUE)
}

#' Store a Classical MLST Scheme in a Database
#'
#' @description Adds or replaces the scheme of an existing database in one
#'   transaction.
#'
#' @inheritParams write_mlst_scheme
#' @param db_path Character path to the database.
#'
#' @return Invisible `TRUE`; stops on failure with nothing changed.
#' @export
store_mlst_scheme <- function(db_path, scheme, info) {
  con <- connect(db_path)
  on.exit(dbDisconnect(con))
  dbBegin(con)
  ok <- tryCatch(
    {
      write_mlst_scheme(con, scheme, info)
      TRUE
    },
    error = function(e) e
  )
  if (!isTRUE(ok)) {
    dbRollback(con)
    stop(conditionMessage(ok))
  }
  dbCommit(con)
  log_event(
    "DB",
    "cla_scheme",
    sprintf(
      "stored '%s': %d loci, %d profiles, %d alleles",
      info$name %||% "",
      length(scheme$loci),
      length(unique(scheme$profiles$st)),
      nrow(scheme$alleles)
    )
  )
  invisible(TRUE)
}

#' Read the Stored Classical MLST Scheme Details
#'
#' @param db_path Character path to the database.
#'
#' @return Named list of the stored details plus `loci` (character, scheme
#'   order), or `NULL` when the database carries no scheme.
#' @export
mlst_scheme_info <- function(db_path) {
  if (is.null(db_path) || length(db_path) != 1L || is.na(db_path) || !file.exists(db_path)) {
    return(NULL)
  }
  con <- connect(db_path, synchronous = NULL)
  on.exit(dbDisconnect(con))
  if (!all(MLST_SCHEME_TABLES %in% dbListTables(con))) {
    return(NULL)
  }
  kv <- dbGetQuery(con, "SELECT key, value FROM cla_scheme_info")
  loci <- dbGetQuery(con, "SELECT locus FROM cla_scheme_loci ORDER BY position")$locus
  if (!length(loci)) {
    return(NULL)
  }
  info <- as.list(kv$value)
  names(info) <- kv$key
  info$loci <- loci
  info
}

# Writes one FASTA file per locus, headers `locus_allele`.
write_allele_files <- function(alleles, loci, dir) {
  for (locus in loci) {
    rows <- alleles[alleles$locus == locus, , drop = FALSE]
    writeLines(
      paste0(">", locus, "_", rows$allele, "\n", rows$sequence),
      file.path(dir, paste0(locus, ".fasta"))
    )
  }
}

#' Write the Stored Scheme Out for claMLST
#'
#' @description Recreates the profile table (`ST` then the loci in scheme
#'   order, tab-separated) and one allele FASTA per locus under `dir`, the
#'   input `claMLST create` expects.
#'
#' @param db_path Character path to the database.
#' @param dir Character existing, empty folder.
#'
#' @return A scheme specification list for `write_scheme_spec()` with
#'   `resolved = TRUE`, or `list(resolved = FALSE)` without a stored scheme.
#' @export
materialize_mlst_scheme <- function(db_path, dir) {
  info <- mlst_scheme_info(db_path)
  if (is.null(info)) {
    return(list(resolved = FALSE))
  }
  # Loci become file names below; a database from elsewhere is not trusted to
  # hold only names the import would have accepted.
  bad <- !valid_locus_name(info$loci) | duplicated(tolower(pymlst_gene_id(info$loci)))
  if (any(bad)) {
    stop(sprintf(
      paste(
        "The stored MLST scheme has unusable locus names (%s).",
        "Replace the scheme in MLST Overview > Scheme Setup."
      ),
      paste0("'", head(info$loci[bad], 5L), "'", collapse = ", ")
    ))
  }
  con <- connect(db_path, synchronous = NULL)
  on.exit(dbDisconnect(con))
  profiles <- dbGetQuery(con, "SELECT st, locus, allele FROM cla_scheme_profiles")
  alleles <- dbGetQuery(
    con,
    "SELECT locus, allele, sequence FROM cla_scheme_alleles ORDER BY locus, allele"
  )

  wide <- data.frame(ST = sort(unique(profiles$st)))
  for (locus in info$loci) {
    rows <- profiles[profiles$locus == locus, , drop = FALSE]
    wide[[locus]] <- rows$allele[match(wide$ST, rows$st)]
  }
  wide <- wide[complete.cases(wide), , drop = FALSE]
  write.table(
    wide,
    file.path(dir, "profiles.tsv"),
    sep = "\t",
    quote = FALSE,
    row.names = FALSE
  )
  locus_dir <- file.path(dir, "locus")
  dir.create(locus_dir, showWarnings = FALSE)
  write_allele_files(alleles, info$loci, locus_dir)

  list(
    resolved = TRUE,
    repository = info$source_label %||% info$source %||% "user-supplied",
    species = info$species %||% NA_character_,
    scheme = info$name %||% "Classical MLST",
    version = info$version %||% NA_character_,
    local_dir = dir,
    loci = info$loci
  )
}

#' Write a Scheme Specification File
#'
#' @description Serializes a materialized scheme into the `key=value` file
#'   loop-pymlst.sh reads to build the reference with `claMLST create`.
#'
#' @param spec List returned by `materialize_mlst_scheme()`.
#' @param path Character string. Destination file.
#'
#' @return `path`, invisibly.
#' @export
write_scheme_spec <- function(spec, path) {
  one_line <- function(x) gsub("[\r\n]+", " ", if (is.null(x) || is.na(x)) "" else x)
  # Species and version reach `conda run`'s command line; a database from
  # elsewhere may hold anything there.
  plain <- function(x) gsub("[$`\\\\\"']", "", one_line(x))
  lines <- c(
    paste0("repository=", one_line(spec$repository)),
    paste0("species=", plain(spec$species)),
    paste0("scheme=", one_line(spec$scheme)),
    paste0("version=", plain(spec$version)),
    paste0("local_dir=", one_line(spec$local_dir)),
    paste0("locus=", vapply(spec$loci, one_line, character(1), USE.NAMES = FALSE))
  )
  writeLines(lines, path)
  invisible(path)
}

#' Describe the Classical MLST Scheme of a Run
#'
#' @description Formats, for a typing run's own transcript, whether classical
#'   MLST will run and against which stored scheme.
#'
#' @param spec List from `materialize_mlst_scheme()`, or `NULL`.
#'
#' @return Character string.
#' @export
describe_resolution <- function(spec) {
  if (isTRUE(spec$resolved)) {
    return(sprintf(
      "Classical MLST for this run: %s from %s, %d loci",
      spec$scheme,
      spec$repository,
      length(spec$loci)
    ))
  }
  paste(
    "Classical MLST: this database carries no classical MLST scheme",
    "(skipping ST calls for this run)."
  )
}
