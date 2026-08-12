# app/logic/db_compat.R
#
# Compatibility gate for importing a peer PhyloTrace database.
#
# A PhyloTrace `.db` is only mergeable into another one when both were built
# from the *same scheme*: same organism, same scheme identity, same locus set,
# and — the strongest signal — the same reference alleles. The synthetic "ref"
# souche in `mlst` holds one row per locus for the scheme's seed genome, so
# comparing its `(gene, sha256(sequence))` set catches the nasty case of two
# databases that agree on locus *names* but were built from different reference
# genomes.
#
# Nothing here writes: every connection is opened read-only.

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

# `targets` spells a locus "PA0195_1" where `mlst` spells it "PA0195-1".
# Mirrors the private `.norm_locus()` in database_functions.R.
.norm_locus <- function(x) gsub("[-_]", "-", x)

`%||%` <- function(a, b) if (is.null(a) || is.na(a)) b else a

#' Open a database read-only. Callers must `dbDisconnect()`.
#'
#' `synchronous = NULL` skips the PRAGMA RSQLite would otherwise issue on
#' connect, matching the rest of the app and keeping the failure quiet when the
#' file turns out not to be a database at all.
#' @export
connect_ro <- function(db_path) {
  dbConnect(
    SQLite(),
    db_path,
    flags = SQLITE_RO,
    synchronous = NULL,
    busy_timeout = 5000
  )
}

.connect_ro <- connect_ro

#' A `file:` URI for `path`, safe to hand to SQLite.
#'
#' Two traps, both reachable from the file picker's "Root" volume:
#'
#'  * `shinyFiles` yields `//tmp/x.db` for a Root-relative pick. Pasted after
#'    `file:` that reads as `file://tmp/...`, where `tmp` is a URI *authority*
#'    and SQLite refuses it ("invalid uri authority").
#'  * SQLite splits the query string at the first `?`, so a database named
#'    `odd?.db` would truncate the path *and* discard the `mode=ro` flag,
#'    silently attaching a different file read-write.
#'
#' Collapsing the leading slashes and percent-encoding each path segment fixes
#' both.
.db_uri <- function(path) {
  p <- normalizePath(path, winslash = "/", mustWork = FALSE)
  p <- sub("^/{2,}", "/", p)
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

#' ATTACH `path` under `alias`, read-only.
#'
#' RSQLite compiles SQLite with URI filename support and enforces `?mode=ro` on
#' an attached database — a plain-path ATTACH is attached read-write, and a
#' stray write would corrupt the peer's file. Verified behaviour; do not
#' "simplify" this to the bare path.
#' @export
attach_ro <- function(con, path, alias) {
  dbExecute(
    con,
    sprintf("ATTACH DATABASE ? AS %s", alias),
    params = list(paste0(.db_uri(path), "?mode=ro"))
  )
}

.scalar <- function(x) {
  x <- x[!is.na(x) & nzchar(x)]
  if (!length(x)) NA_character_ else as.character(x[[1]])
}

.norm_cmp <- function(x) {
  if (is.na(x)) NA_character_ else tolower(trimws(x))
}

#' Read everything needed to compare two databases, without loading the bulk of
#' either. Returns a list; `ok` is FALSE when the core tables are missing, in
#' which case the remaining fields are placeholders.
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

# The reference genome's alleles as a sorted "gene\thash" character vector.
# Uses the `hashes` table when it covers the ref seqids; otherwise hashes just
# the ~3.9k reference sequences in R rather than the whole file.
.ref_alleles <- function(con, tables) {
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

.row <- function(check, status, detail) {
  data.frame(
    check = check,
    status = status,
    detail = detail,
    stringsAsFactors = FALSE
  )
}

# "3 differ (PA0001, PA0002, PA0003 …)" — keeps the detail column readable.
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

#' Compare an external database against the loaded one.
#'
#' Returns a data.frame of `check` / `status` ("pass" | "warn" | "fail") /
#' `detail`, with `attr(x, "blocked")` TRUE when any check failed. Checks after
#' a structural failure are reported as "skipped" rather than guessed at.
#' @export
check_import_compatibility <- function(local_path, ext_path) {
  local <- read_db_signature(local_path)
  ext <- read_db_signature(ext_path)

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
  # One species per database is a load-bearing invariant: `organism` in
  # `metadata` and `species` in `mlst_type` are uneditable in the UI because the
  # whole typing/visualisation stack assumes a single organism.
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
          local$species %||% "—",
          ext$species %||% "—"
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
  # Hard gate. A superset would introduce loci with no `ref` row, silently
  # corrupting the scheme_size() denominator behind the QC completeness metric
  # and the distance matrices.
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
  # `alembic_version` is Alembic's (SQLAlchemy's migration tool) revision stamp,
  # written by pyMLST. PhyloTrace never reads it; a mismatch only means the two
  # files were created by different pyMLST versions. Advisory, never blocking.
  same_alembic <- identical(local$alembic, ext$alembic)
  checks <- rbind(
    checks,
    .row(
      "Schema revision",
      if (same_alembic) "pass" else "warn",
      if (same_alembic) {
        ext$alembic %||% "—"
      } else {
        sprintf(
          "Loaded: %s, external: %s (advisory)",
          local$alembic %||% "—",
          ext$alembic %||% "—"
        )
      }
    )
  )

  finish(checks)
}
