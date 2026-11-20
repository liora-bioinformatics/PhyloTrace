# app/logic/db_compat.R
#
# Compatibility gate for importing peer PhyloTrace database files.
#
# A PhyloTrace SQLite database (.db) can only be merged into another instance
# when both share an identical typing scheme: identical organism, locus
# definitions, and reference alleles. The scheme's labels (source, version) are
# compared too, but only as an advisory.
#
# The synthetic "ref" strain (souche) in the `mlst` table contains one entry per
# locus derived from the scheme's seed genome. Evaluating the set of
# (gene, sha256(sequence)) pairs ensures compatibility even when two databases
# share identical locus nomenclature but originate from different underlying
# reference genomes.
#
# The optional classical MLST scheme and the AMR point-mutation setting are
# compared as well. Neither can block a merge: they only decide which advisory
# rows appear and what the import screen offers (see `mlst_scheme_relation`).
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

# The stored classical MLST scheme (app/logic/mlst_scheme.R). Spelled out here
# because importing mlst_scheme would close an import cycle (mlst_scheme ->
# scheme_files -> custom_import -> database_functions -> db_compat); a test
# keeps the two lists equal.
MLST_TABLES <- c(
  "cla_scheme_info",
  "cla_scheme_loci",
  "cla_scheme_profiles",
  "cla_scheme_alleles"
)

# phylotrace_meta key of the AMR point-mutation organism (amr$AMR_ORGANISM_KEY).
AMR_KEY <- "amr_organism"

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

#' Check Whether a File Can Be Loaded as a PhyloTrace Database
#'
#' Cheap pre-flight used before a database is opened for a session. Opens the
#' file read-only (so a missing path is *not* silently created) and confirms the
#' core pyMLST tables are present.
#'
#' @param db_path Character path to a candidate SQLite database file.
#' @return A list with `ok` (logical) and, when `ok` is `FALSE`, a
#'   human-readable `reason` naming the failure.
#' @export
check_db_loadable <- function(db_path) {
  if (
    is.null(db_path) ||
      length(db_path) != 1 ||
      is.na(db_path) ||
      !nzchar(db_path)
  ) {
    return(list(ok = FALSE, reason = "No database file is selected."))
  }

  if (!file.exists(db_path)) {
    return(list(
      ok = FALSE,
      reason = paste0(
        "The database file is no longer at this location. It may have been ",
        "moved, renamed, deleted, or is on a disconnected drive."
      )
    ))
  }

  con <- tryCatch(.connect_ro(db_path), error = function(e) NULL)
  if (is.null(con)) {
    return(list(
      ok = FALSE,
      reason = "The file could not be opened as an SQLite database."
    ))
  }
  on.exit(dbDisconnect(con), add = TRUE)

  tables <- tryCatch(dbListTables(con), error = function(e) character(0))
  missing <- setdiff(CORE_TABLES, tables)
  if (length(missing)) {
    return(list(
      ok = FALSE,
      reason = sprintf(
        "The file is not a valid PhyloTrace database (missing table(s): %s).",
        paste(missing, collapse = ", ")
      )
    ))
  }

  list(ok = TRUE, reason = NA_character_)
}

#' Describe a PhyloTrace Database
#'
#' Cheap read-only summary shown on the start screen before a database is
#' opened: the species, the cgMLST scheme and how many isolates it holds. Every
#' field is optional: a database that lacks a table (or is not readable) yields
#' `NA` for that field rather than an error, so the caller can always render.
#'
#' @param db_path Character path to a database file that passed
#'   [check_db_loadable()].
#' @return A list with `species`, `scheme` (character, `NA` when unknown) and
#'   `isolates` (integer, `NA` when unknown).
#' @export
describe_db <- function(db_path) {
  out <- list(species = NA_character_, scheme = NA_character_, isolates = NA_integer_)
  con <- tryCatch(connect_ro(db_path), error = function(e) NULL)
  if (is.null(con)) {
    return(out)
  }
  on.exit(dbDisconnect(con), add = TRUE)

  tables <- tryCatch(dbListTables(con), error = function(e) character(0))
  # First non-empty value of a query's single column, or NA.
  first_value <- function(sql) {
    v <- tryCatch(dbGetQuery(con, sql)[[1]], error = function(e) NULL)
    v <- v[!is.na(v) & nzchar(v)]
    if (length(v)) as.character(v[[1]]) else NA_character_
  }

  if ("mlst_type" %in% tables) {
    out$species <- first_value("SELECT species FROM mlst_type")
    out$scheme <- first_value("SELECT source FROM mlst_type")
  }
  # The scheme's own display name ("L. monocytogenes cgMLST") beats the bare
  # provider recorded in mlst_type.
  if ("scheme_overview" %in% tables) {
    name <- first_value("SELECT value FROM scheme_overview WHERE key = 'Name'")
    if (!is.na(name)) out$scheme <- name
  }
  if ("metadata" %in% tables) {
    n <- tryCatch(dbGetQuery(con, "SELECT COUNT(*) AS n FROM metadata")$n, error = function(e) NA)
    out$isolates <- as.integer(n %||na% NA_integer_)
  }
  out
}

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
    # A name such as "partner%20lab.db" holds a literal "%"; without
    # `repeated` it would be taken as already encoded and open another file.
    repeated = TRUE,
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
    isolates = character(0),
    mlst_scheme = NULL,
    amr_organism = NA_character_
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
    isolates = sort(setdiff(souches, REF_SOUCHE)),
    mlst_scheme = .read_mlst_scheme(con, tables),
    amr_organism = .read_amr_organism(con, tables)
  )
}

# Reads the stored classical MLST scheme: its labels, loci, profiles and a
# sha256 per allele sequence, plus one digest over profiles and alleles. NULL
# when the database carries no scheme.
.read_mlst_scheme <- function(con, tables) {
  if (!all(MLST_TABLES %in% tables)) {
    return(NULL)
  }
  loci <- dbGetQuery(con, "SELECT locus FROM cla_scheme_loci ORDER BY position")$locus
  if (!length(loci)) {
    return(NULL)
  }
  kv <- dbGetQuery(con, "SELECT key, value FROM cla_scheme_info")
  info <- function(key) .scalar(kv$value[kv$key == key])
  profiles <- dbGetQuery(
    con,
    "SELECT st, locus, allele FROM cla_scheme_profiles ORDER BY st, locus"
  )
  alleles <- dbGetQuery(
    con,
    "SELECT locus, allele, sequence FROM cla_scheme_alleles ORDER BY locus, allele"
  )
  alleles$hash <- as.character(sha256(alleles$sequence))
  alleles$sequence <- NULL
  profile_keys <- paste(profiles$st, profiles$locus, profiles$allele, sep = "\t")
  allele_keys <- paste(alleles$locus, alleles$allele, alleles$hash, sep = "\t")
  list(
    name = info("name"),
    version = info("version"),
    loci = loci,
    profiles = profiles,
    alleles = alleles,
    digest = as.character(sha256(paste(
      c(sort(profile_keys), "", sort(allele_keys)),
      collapse = "\n"
    )))
  )
}

# Reads the AMR point-mutation organism; "" means acquired genes only and NA
# that the setting is absent.
.read_amr_organism <- function(con, tables) {
  if (!"phylotrace_meta" %in% tables) {
    return(NA_character_)
  }
  value <- dbGetQuery(
    con,
    "SELECT value FROM phylotrace_meta WHERE key = ?",
    params = list(AMR_KEY)
  )$value
  if (!length(value) || is.na(value[1])) "" else as.character(value[1])
}

# Sorted, normalized locus set of a stored scheme.
.scheme_loci <- function(scheme) sort(unique(tolower(trimws(scheme$loci))))

#' Relate Two Stored Classical MLST Schemes
#'
#' Classifies how the imported database's classical MLST scheme relates to the
#' loaded one and what the import screen should offer.
#'
#' @param local Scheme list from `read_db_signature()$mlst_scheme`, or `NULL`.
#' @param ext Scheme list from `read_db_signature()$mlst_scheme`, or `NULL`.
#' @return A list with `relation` (`"none"`, `"local_only"`, `"ext_only"`,
#'   `"identical"`, `"older"`, `"newer"` or `"different"`), `status` and
#'   `detail` for the check row, `adopt_offer` and `adopt_default` (whether to
#'   offer adopting the imported scheme, and ticked or not), `classical_default`
#'   (whether "Classical MLST results" starts ticked) and `note` (explanation
#'   shown next to those options, or `NA`).
#' @export
mlst_scheme_relation <- function(local, ext) {
  out <- function(relation, status, detail, adopt = FALSE, adopt_default = FALSE,
                  classical = TRUE, note = NA_character_) {
    list(
      relation = relation,
      status = status,
      detail = detail,
      adopt_offer = adopt,
      adopt_default = adopt_default,
      classical_default = classical,
      note = note
    )
  }
  label <- function(s) {
    paste0(s$name %||na% "unnamed scheme", if (!is.na(s$version)) paste0(" ", s$version))
  }

  if (is.null(local) && is.null(ext)) {
    return(out("none", "pass", "Neither database has a classical MLST scheme"))
  }
  if (is.null(ext)) {
    return(out(
      "local_only",
      "pass",
      sprintf("Only the loaded database has one (%s); it is kept", label(local))
    ))
  }
  if (is.null(local)) {
    return(out(
      "ext_only",
      "pass",
      sprintf("Only the imported database has one (%s); it can be adopted", label(ext)),
      adopt = TRUE,
      adopt_default = TRUE,
      note = paste(
        "The loaded database has no classical MLST scheme. Adopt the imported one",
        "to type sequence types for new isolates too."
      )
    ))
  }

  different <- function(why) {
    out(
      "different",
      "warn",
      sprintf("Different scheme: %s (advisory)", why),
      classical = FALSE,
      note = paste(
        "The imported database uses a different classical MLST scheme, so its",
        "sequence types are not comparable with the local ones. Its classical",
        "MLST results are left out unless you tick them."
      )
    )
  }
  if (!identical(.norm_cmp(local$name), .norm_cmp(ext$name))) {
    return(different(sprintf("%s vs %s", label(local), label(ext))))
  }
  if (!identical(.scheme_loci(local), .scheme_loci(ext))) {
    return(different("the loci differ"))
  }

  # Shared allele numbers must name the same sequence and shared STs the same
  # profile; otherwise the same numbers mean different things.
  key_a <- function(s) paste(tolower(s$alleles$locus), s$alleles$allele, sep = "\t")
  shared_a <- match(key_a(local), key_a(ext))
  hit <- !is.na(shared_a)
  if (any(local$alleles$hash[hit] != ext$alleles$hash[shared_a[hit]])) {
    return(different("the same allele numbers have different sequences"))
  }
  key_p <- function(s) paste(s$profiles$st, tolower(s$profiles$locus), sep = "\t")
  shared_p <- match(key_p(local), key_p(ext))
  hit <- !is.na(shared_p)
  if (any(local$profiles$allele[hit] != ext$profiles$allele[shared_p[hit]])) {
    return(different("the same sequence types have different profiles"))
  }

  if (identical(local$digest, ext$digest)) {
    return(out(
      "identical",
      "pass",
      if (identical(.norm_cmp(local$version), .norm_cmp(ext$version))) {
        sprintf("Identical (%s)", label(local))
      } else {
        sprintf("Identical content (%s); labelled version %s", label(local),
                ext$version %||na% "—")
      }
    ))
  }

  ext_extra <- sum(!key_p(ext) %in% key_p(local)) + sum(!key_a(ext) %in% key_a(local))
  if (!ext_extra) {
    return(out(
      "older",
      "pass",
      sprintf(
        "Same scheme; the loaded database has the more complete copy (%s), which is kept",
        label(local)
      )
    ))
  }
  n_st <- function(s) length(unique(s$profiles$st))
  out(
    "newer",
    "warn",
    sprintf(
      "Same scheme, the imported copy is newer: %s with %d profiles (loaded: %s with %d)",
      label(ext),
      n_st(ext),
      label(local),
      n_st(local)
    ),
    adopt = TRUE,
    adopt_default = FALSE,
    note = paste(
      "The imported database carries a newer copy of the same classical MLST",
      "scheme. Adopt it to replace the local copy."
    )
  )
}

# Builds the advisory AMR row: point mutations screened for another organism
# are not comparable, but the local setting always stays.
.amr_row <- function(local, ext) {
  show <- function(x) {
    if (is.na(x)) "not set" else if (!nzchar(x)) "acquired genes only" else x
  }
  has_results <- "amr_results" %in% ext$tables
  same <- identical(local$amr_organism, ext$amr_organism)
  if (same || !has_results) {
    return(.row(
      "AMR organism",
      "pass",
      if (same) show(local$amr_organism) else "No AMR results in the imported database"
    ))
  }
  .row(
    "AMR organism",
    "warn",
    sprintf(
      paste(
        "AMR results were screened with a different point-mutation catalogue",
        "(imported: %s, loaded: %s); the local setting is kept"
      ),
      show(ext$amr_organism),
      show(local$amr_organism)
    )
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
#'   Attributes: `blocked`, the two signatures `local` and `ext`, and
#'   `mlst_scheme` (from `mlst_scheme_relation()`, `NULL` when not checked).
#' @export
check_import_compatibility <- function(local_path, ext_path) {
  local <- read_db_signature(local_path)
  ext <- read_db_signature(ext_path)
  scheme <- NULL

  # Attaches result metadata attributes upon return completion
  finish <- function(df) {
    attr(df, "blocked") <- any(df$status == "fail")
    attr(df, "local") <- local
    attr(df, "ext") <- ext
    attr(df, "mlst_scheme") <- scheme
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
      "Schema revision",
      "Classical MLST scheme",
      "AMR organism"
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

  # 3. Scheme identity (advisory) -------------------------------------------
  # Name, source and version are labels - typed in by whoever set the database
  # up - so two databases built from the very same scheme files can spell them
  # differently. What makes profiles comparable is checked exactly below (locus
  # set and reference alleles); a label difference alone only warns.
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
      if (length(diff_ident)) "warn" else "pass",
      if (length(diff_ident)) {
        paste0(
          "Labelled differently: ",
          paste(sub("^scheme_", "", diff_ident), collapse = ", "),
          " (advisory)"
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

  # 7. Classical MLST scheme (advisory) ---------------------------------------
  scheme <- mlst_scheme_relation(local$mlst_scheme, ext$mlst_scheme)
  checks <- rbind(
    checks,
    .row("Classical MLST scheme", scheme$status, scheme$detail),
    # 8. AMR point-mutation organism (advisory) -------------------------------
    .amr_row(local, ext)
  )

  finish(checks)
}
