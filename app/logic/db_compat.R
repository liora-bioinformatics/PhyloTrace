# app/logic/db_compat.R
#
# Compatibility gate for importing peer PhyloTrace database files.
#
# A PhyloTrace SQLite database (.db) can only be merged into another instance
# when both share an identical typing scheme: identical organism, scheme identity,
# locus definitions, and reference alleles.
#
# The synthetic "ref" strain (souche) in the `mlst` table contains one entry per
# locus derived from the scheme's seed genome. Evaluating the set of
# (gene, sha256(sequence)) pairs ensures compatibility even when two databases
# share identical locus nomenclature but originate from different underlying
# reference genomes.
#
# Read-only enforcement: All database connections established in this module
# are opened strictly in read-only mode to prevent unintended state mutations.

box::use(
  RSQLite[SQLite, SQLITE_RO],
  DBI[
    dbConnect,
    dbDisconnect,
    dbExecute,
    dbListTables,
    dbListFields,
    dbReadTable,
    dbGetQuery
  ],
  openssl[sha256],
)

# Tables without which a file is not a PhyloTrace/pyMLST database at all.
#' @export
CORE_TABLES <- c("mlst", "mlst_type", "sequences")

# The synthetic strain holding the scheme's reference alleles.
#' @export
REF_SOUCHE <- "ref"

# Standardizes locus identifiers by replacing underscores with hyphens.
.norm_locus <- function(x) gsub("[-_]", "-", x)

# Returns fallback value if target is NULL or NA.
`%||na%` <- function(a, b) if (is.null(a) || is.na(a)) b else a

#' Open Read-Only Database Connection
#'
#' Establishes a read-only SQLite connection. Callers must handle `dbDisconnect()`.
#'
#' @param db_path Character path to the target database.
#' @return DBI connection object.
#' @export
connect_ro <- function(db_path) {
  # synchronous = NULL skips default PRAGMA, matching app pattern and failing silently for non-DBs
  con <- dbConnect(
    SQLite(),
    db_path,
    flags = SQLITE_RO,
    synchronous = NULL
  )
  # As a PRAGMA, not a dbConnect() argument: RSQLite accepts and silently drops
  # an unknown `busy_timeout =` argument, leaving the timeout at 0. Typing runs
  # pyMLST in a separate process that writes this same file, so a read taken
  # while that process holds the write lock must wait rather than fail outright.
  # Safe on a file that is not a database - the PRAGMA succeeds and the first
  # real query still raises "file is not a database", as callers expect.
  dbExecute(con, "PRAGMA busy_timeout = 5000")
  con
}

.connect_ro <- connect_ro

# Formats a file path into a SQLite-safe file: URI string with encoded path segments.
.db_uri <- function(path) {
  # Normalize slashes and clean up leading slash sequences
  p <- normalizePath(path, winslash = "/", mustWork = FALSE)
  p <- sub("^/{2,}", "/", p)

  # Percent-encode individual path segments to prevent query string truncation or bad URI authorities
  segments <- strsplit(p, "/", fixed = TRUE)[[1]]
  encoded <- vapply(
    segments,
    utils::URLencode,
    character(1),
    reserved = TRUE,
    USE.NAMES = FALSE
  )
  paste0("file:", paste(encoded, collapse = "/"))
}

#' Attach Database as Read-Only Alias
#'
#' Attaches an external SQLite database file under a designated alias in read-only mode using URIs.
#'
#' @param con Active DBI database connection.
#' @param path Character path to the target database file.
#' @param alias Character alias for the attached database.
#' @export
attach_ro <- function(con, path, alias) {
  # SQLite requires explicit ?mode=ro on URIs to prevent standard read-write attachments
  dbExecute(
    con,
    sprintf("ATTACH DATABASE ? AS %s", alias),
    params = list(paste0(.db_uri(path), "?mode=ro"))
  )
}

# Extracts the first non-null/non-empty scalar value from a vector.
.scalar <- function(x) {
  x <- x[!is.na(x) & nzchar(x)]
  if (!length(x)) NA_character_ else as.character(x[[1]])
}

# Trims whitespace and normalizes string casing for comparisons.
.norm_cmp <- function(x) {
  if (is.na(x)) NA_character_ else tolower(trimws(x))
}

#' Extract Database Compatibility Signature
#'
#' Reads schema metadata, locus sets, and reference allele hashes needed for database comparison.
#'
#' @param db_path Character path to the SQLite database file.
#' @return A list containing schema parameters, loci, and reference hashes, or placeholder values if invalid.
#' @export
read_db_signature <- function(db_path) {
  empty <- list(
    ok = FALSE,
    path = db_path,
    tables = character(0),
    species = NA_character_,
    scheme_name = NA_character_,
    scheme_source = NA_character_,
    scheme_version = NA_character_,
    alembic = NA_character_,
    genes = character(0),
    ref_alleles = character(0),
    metadata_cols = NULL,
    isolates = character(0)
  )

  if (
    is.null(db_path) ||
      length(db_path) != 1 ||
      is.na(db_path) ||
      !file.exists(db_path)
  ) {
    return(empty)
  }

  con <- tryCatch(.connect_ro(db_path), error = function(e) NULL)
  if (is.null(con)) {
    return(empty)
  }
  on.exit(dbDisconnect(con), add = TRUE)

  tables <- tryCatch(dbListTables(con), error = function(e) character(0))
  empty$tables <- tables

  # Verify presence of base required schema tables
  if (!all(CORE_TABLES %in% tables)) {
    return(empty)
  }

  mlst_type <- dbReadTable(con, "mlst_type")

  souches <- dbGetQuery(con, "SELECT DISTINCT souche FROM mlst")$souche

  list(
    ok = TRUE,
    path = db_path,
    tables = tables,
    species = .scalar(mlst_type$species),
    scheme_name = .scalar(mlst_type$name),
    scheme_source = .scalar(mlst_type$source),
    scheme_version = .scalar(mlst_type$version),
    alembic = if ("alembic_version" %in% tables) {
      .scalar(dbReadTable(con, "alembic_version")$version_num)
    } else {
      NA_character_
    },
    genes = sort(unique(.norm_locus(
      dbGetQuery(con, "SELECT DISTINCT gene FROM mlst")$gene
    ))),
    ref_alleles = .ref_alleles(con, tables),
    metadata_cols = if ("metadata" %in% tables) {
      dbListFields(con, "metadata")
    } else {
      NULL
    },
    isolates = sort(setdiff(souches, REF_SOUCHE))
  )
}

# Constructs tab-separated 'gene\thash' character entries for reference strains.
# Prefers precomputed 'hashes' table when available; falls back to SHA-256 in R.
.ref_alleles <- function(con, tables) {
  # Query using existing hashes table when populated
  if ("hashes" %in% tables) {
    res <- dbGetQuery(
      con,
      "SELECT m.gene AS gene, h.hash AS hash
         FROM mlst m JOIN hashes h ON h.id = m.seqid
        WHERE m.souche = ?",
      params = list(REF_SOUCHE)
    )
    n_ref <- dbGetQuery(
      con,
      "SELECT COUNT(*) AS n FROM mlst WHERE souche = ?",
      params = list(REF_SOUCHE)
    )$n

    if (nrow(res) == n_ref && n_ref > 0L) {
      return(sort(paste(.norm_locus(res$gene), res$hash, sep = "\t")))
    }
  }

  # Fallback: compute SHA-256 directly on reference allele sequences
  res <- dbGetQuery(
    con,
    "SELECT m.gene AS gene, s.sequence AS sequence
       FROM mlst m JOIN sequences s ON s.id = m.seqid
      WHERE m.souche = ?",
    params = list(REF_SOUCHE)
  )
  if (!nrow(res)) {
    return(character(0))
  }

  sort(paste(
    .norm_locus(res$gene),
    as.character(sha256(res$sequence)),
    sep = "\t"
  ))
}

# Constructs a standardized compatibility result row data frame.
.row <- function(check, status, detail) {
  data.frame(
    check = check,
    status = status,
    detail = detail,
    stringsAsFactors = FALSE
  )
}

# Creates a concise, human-readable summary string of differing elements.
.summarise_diff <- function(only_a, only_b, label_a, label_b) {
  parts <- character(0)
  fmt <- function(x, label) {
    shown <- utils::head(sub("\t.*$", "", x), 3L)
    sprintf(
      "%d only in %s (%s%s)",
      length(x),
      label,
      paste(shown, collapse = ", "),
      if (length(x) > 3L) ", …" else ""
    )
  }
  if (length(only_a)) {
    parts <- c(parts, fmt(only_a, label_a))
  }
  if (length(only_b)) {
    parts <- c(parts, fmt(only_b, label_b))
  }
  paste(parts, collapse = "; ")
}

#' Validate Compatibility for Peer Database Import
#'
#' Evaluates whether an external database matches the schema, species, scheme, loci, and reference alleles of the loaded database.
#'
#' @param local_path Character path to the target/local SQLite database file.
#' @param ext_path Character path to the incoming external SQLite database file.
#' @return A data frame containing check names, status ("pass", "warn", "fail", "skipped"), and descriptive details.
#' @export
check_import_compatibility <- function(local_path, ext_path) {
  local <- read_db_signature(local_path)
  ext <- read_db_signature(ext_path)

  # Attaches result metadata attributes upon return completion
  finish <- function(df) {
    attr(df, "blocked") <- any(df$status == "fail")
    attr(df, "local") <- local
    attr(df, "ext") <- ext
    df
  }

  # 1. Core tables ----------------------------------------------------------
  missing_ext <- setdiff(CORE_TABLES, ext$tables)
  if (length(missing_ext) || !ext$ok) {
    rows <- .row(
      "Core tables",
      "fail",
      if (!length(ext$tables)) {
        "File is missing, unreadable, or not an SQLite database"
      } else if (length(missing_ext)) {
        paste0("Missing: ", paste(missing_ext, collapse = ", "))
      } else {
        "Not a readable PhyloTrace database"
      }
    )
    skipped <- c(
      "Organism",
      "Scheme identity",
      "Locus set",
      "Reference alleles",
      "Schema revision"
    )
    return(finish(rbind(
      rows,
      .row(skipped, "skipped", "Not checked — core tables missing")
    )))
  }

  if (!local$ok) {
    return(finish(.row(
      "Core tables",
      "fail",
      "The loaded database is unreadable or incomplete"
    )))
  }

  checks <- .row(
    "Core tables",
    "pass",
    sprintf("All present; %d isolate(s) in file", length(ext$isolates))
  )

  # 2. Organism -------------------------------------------------------------
  # Single species enforcement across metadata and mlst_type
  same_species <- identical(.norm_cmp(local$species), .norm_cmp(ext$species))
  checks <- rbind(
    checks,
    .row(
      "Organism",
      if (same_species) "pass" else "fail",
      if (same_species) {
        local$species
      } else {
        sprintf(
          "Loaded: %s ≠ external: %s",
          local$species %||na% "—",
          ext$species %||na% "—"
        )
      }
    )
  )

  # 3. Scheme identity ------------------------------------------------------
  ident <- c("scheme_name", "scheme_source", "scheme_version")
  diff_ident <- ident[vapply(
    ident,
    function(f) !identical(.norm_cmp(local[[f]]), .norm_cmp(ext[[f]])),
    logical(1)
  )]
  checks <- rbind(
    checks,
    .row(
      "Scheme identity",
      if (length(diff_ident)) "fail" else "pass",
      if (length(diff_ident)) {
        paste0(
          "Differs: ",
          paste(sub("^scheme_", "", diff_ident), collapse = ", ")
        )
      } else {
        parts <- c(ext$scheme_name, ext$scheme_source, ext$scheme_version)
        paste(parts[!is.na(parts)], collapse = " / ")
      }
    )
  )

  # 4. Locus set ------------------------------------------------------------
  # Strict check: mismatched gene sets break scheme sizing and distance metrics
  only_local <- setdiff(local$genes, ext$genes)
  only_ext <- setdiff(ext$genes, local$genes)
  same_loci <- !length(only_local) && !length(only_ext)
  checks <- rbind(
    checks,
    .row(
      "Locus set",
      if (same_loci) "pass" else "fail",
      if (same_loci) {
        sprintf("%d loci match", length(local$genes))
      } else {
        .summarise_diff(only_local, only_ext, "loaded", "external")
      }
    )
  )

  # 5. Reference alleles ----------------------------------------------------
  if (!length(local$ref_alleles) || !length(ext$ref_alleles)) {
    checks <- rbind(
      checks,
      .row(
        "Reference alleles",
        "fail",
        "No 'ref' souche found — cannot verify the scheme reference"
      )
    )
  } else {
    only_local_r <- setdiff(local$ref_alleles, ext$ref_alleles)
    only_ext_r <- setdiff(ext$ref_alleles, local$ref_alleles)
    same_ref <- !length(only_local_r) && !length(only_ext_r)
    checks <- rbind(
      checks,
      .row(
        "Reference alleles",
        if (same_ref) "pass" else "fail",
        if (same_ref) {
          sprintf("%d reference alleles identical", length(local$ref_alleles))
        } else {
          sprintf(
            "%d locus/loci carry a different reference sequence",
            length(union(
              sub("\t.*$", "", only_local_r),
              sub("\t.*$", "", only_ext_r)
            ))
          )
        }
      )
    )
  }

  # 6. Schema revision (advisory) -------------------------------------------
  # alembic_version tracks pyMLST migrations; mismatch is non-blocking
  same_alembic <- identical(local$alembic, ext$alembic)
  checks <- rbind(
    checks,
    .row(
      "Schema revision",
      if (same_alembic) "pass" else "warn",
      if (same_alembic) {
        ext$alembic %||na% "—"
      } else {
        sprintf(
          "Loaded: %s, external: %s (advisory)",
          local$alembic %||na% "—",
          ext$alembic %||na% "—"
        )
      }
    )
  )

  finish(checks)
}
