# Builders for miniature PhyloTrace databases.
#
# The DDL below mirrors a real database exactly: `mlst` keeps its INTEGER
# PRIMARY KEY (a rowid alias), its foreign key onto `sequences`, and the four
# `ix_*` indexes, while the R-written tables (`sequences`, `hashes`, `metadata`,
# `targets`, `scheme_overview`) carry no keys — just as `dbWriteTable` leaves
# them. Tests that assert the export preserves DDL depend on this fidelity.

MLST_DDL <- "CREATE TABLE mlst (
  id INTEGER NOT NULL,
  souche TEXT,
  gene TEXT,
  seqid INTEGER,
  PRIMARY KEY (id),
  FOREIGN KEY(seqid) REFERENCES sequences (id)
)"

MLST_INDEXES <- c(
  "CREATE INDEX ix_gene ON mlst (gene)",
  "CREATE INDEX ix_seqid ON mlst (seqid)",
  "CREATE INDEX ix_souche ON mlst (souche)",
  "CREATE INDEX ix_souche_gene_seqid ON mlst (gene, souche, seqid)"
)

#' Build a database from `souche_alleles`: a named list mapping souche -> named
#' character vector of gene -> nucleotide sequence. It must contain a "ref"
#' entry (the scheme reference), which is what makes the file a valid scheme.
#'
#' `metadata` is a data.frame with an `isolate` column, or NULL for no metadata
#' table. `with_hashes = FALSE` omits the `hashes` table, exercising the
#' hash-on-the-fly fallbacks.
build_db <- function(
  path,
  souche_alleles,
  species = "Testus organismus",
  scheme_name = "wg",
  scheme_source = "test.org",
  scheme_version = "1.0",
  alembic = "a793f8f3fd83",
  metadata = NULL,
  with_hashes = TRUE,
  with_targets = TRUE
) {
  stopifnot("ref" %in% names(souche_alleles))

  calls <- do.call(
    rbind,
    lapply(names(souche_alleles), function(s) {
      a <- souche_alleles[[s]]
      data.frame(
        souche = s,
        gene = names(a),
        sequence = unname(a),
        stringsAsFactors = FALSE
      )
    })
  )

  # An allele is identified by (gene, sequence); each seqid belongs to exactly
  # one gene, exactly as in a real database.
  alleles <- unique(calls[, c("gene", "sequence")])
  alleles <- alleles[order(alleles$gene, alleles$sequence), , drop = FALSE]
  alleles$id <- seq_len(nrow(alleles))

  calls$seqid <- alleles$id[match(
    paste(calls$gene, calls$sequence),
    paste(alleles$gene, alleles$sequence)
  )]
  calls$id <- seq_len(nrow(calls))

  con <- DBI::dbConnect(RSQLite::SQLite(), path)
  on.exit(DBI::dbDisconnect(con), add = TRUE)

  DBI::dbExecute(
    con,
    "CREATE TABLE alembic_version (
       version_num VARCHAR(32) NOT NULL,
       CONSTRAINT alembic_version_pkc PRIMARY KEY (version_num))"
  )
  DBI::dbExecute(con, "INSERT INTO alembic_version VALUES (?)", list(alembic))

  DBI::dbExecute(
    con,
    "CREATE TABLE mlst_type (
       name VARCHAR(4) NOT NULL, source VARCHAR, species VARCHAR, version VARCHAR,
       PRIMARY KEY (name))"
  )
  DBI::dbExecute(
    con,
    "INSERT INTO mlst_type VALUES (?, ?, ?, ?)",
    list(scheme_name, scheme_source, species, scheme_version)
  )

  DBI::dbExecute(con, MLST_DDL)
  DBI::dbWriteTable(
    con,
    "mlst",
    calls[, c("id", "souche", "gene", "seqid")],
    append = TRUE
  )
  for (sql in MLST_INDEXES) DBI::dbExecute(con, sql)

  DBI::dbWriteTable(
    con,
    "sequences",
    data.frame(
      id = alleles$id,
      sequence = alleles$sequence,
      stringsAsFactors = FALSE
    )
  )

  if (with_hashes) {
    DBI::dbWriteTable(
      con,
      "hashes",
      data.frame(
        id = alleles$id,
        hash = as.character(openssl::sha256(alleles$sequence)),
        stringsAsFactors = FALSE
      )
    )
  }

  if (with_targets) {
    genes <- sort(unique(calls$gene))
    DBI::dbWriteTable(
      con,
      "targets",
      data.frame(
        Locus = genes,
        Gene = genes,
        Start = "1",
        Length = "10",
        Product = "test product",
        Alleles = paste0(genes, ".fasta"),
        stringsAsFactors = FALSE
      )
    )
    DBI::dbWriteTable(
      con,
      "scheme_overview",
      data.frame(
        key = c("Name", "Locus Count"),
        value = c("Test scheme", as.character(length(genes))),
        stringsAsFactors = FALSE
      )
    )
  }

  if (!is.null(metadata)) {
    DBI::dbWriteTable(con, "metadata", metadata)
  }

  invisible(path)
}

# A deterministic pseudo-sequence; only its sha256 matters to the code.
seqv <- function(tag) {
  paste0("ATGC", paste(rep(tag, 3), collapse = ""), "TAA")
}

# The standard three-locus reference used by most fixtures.
ref_alleles <- function() {
  c(g1 = seqv("R1"), g2 = seqv("R2"), g3 = seqv("R3"))
}

# `local` has isolates A and B; the peer has B (identical) and C (new).
default_local <- function() {
  list(
    ref = ref_alleles(),
    A = c(g1 = seqv("A1"), g2 = seqv("A2"), g3 = seqv("A3")),
    B = c(g1 = seqv("B1"), g2 = seqv("B2"), g3 = seqv("B3"))
  )
}

default_peer <- function() {
  list(
    ref = ref_alleles(),
    B = c(g1 = seqv("B1"), g2 = seqv("B2"), g3 = seqv("B3")),
    C = c(g1 = seqv("C1"), g2 = seqv("A2"), g3 = seqv("R3"))
  )
}

meta_df <- function(isolates, species = "Testus organismus", extra = NULL) {
  df <- data.frame(
    isolate = isolates,
    organism = species,
    sample_collection_date = paste0("2024-01-0", seq_along(isolates)),
    geo_loc_name_country = "Germany",
    stringsAsFactors = FALSE
  )
  if (!is.null(extra)) {
    for (nm in names(extra)) df[[nm]] <- extra[[nm]]
  }
  df
}

# Seed the isolate-keyed analysis-result tables (classical_mlst, amr_results,
# amr_summary) for one or more souches, using the same DDL the app creates
# (pymlst.R / amr.R). Each souche gets a couple of deterministic rows per table.
# `classical` / `amr` gate which tables are written, so a fixture can carry one
# family without the other.
seed_results <- function(path, souche, classical = TRUE, amr = TRUE) {
  con <- DBI::dbConnect(RSQLite::SQLite(), path)
  on.exit(DBI::dbDisconnect(con), add = TRUE)

  if (classical) {
    DBI::dbExecute(
      con,
      "CREATE TABLE IF NOT EXISTS classical_mlst (
         id INTEGER PRIMARY KEY AUTOINCREMENT,
         souche TEXT, gene TEXT, allele TEXT, sequence TEXT, st TEXT,
         status TEXT, scheme TEXT, scheme_version TEXT, alembic_version TEXT,
         repository TEXT, identity REAL, coverage REAL, pymlst_version TEXT,
         called_at TEXT)"
    )
    for (s in souche) {
      for (g in c("acsA", "aroE")) {
        DBI::dbExecute(
          con,
          "INSERT INTO classical_mlst
             (souche, gene, allele, st, status, scheme, called_at)
           VALUES (?, ?, ?, ?, ?, ?, ?)",
          list(s, g, "1", "42", "known", "Testus organismus", "2026-01-01")
        )
      }
    }
  }

  if (amr) {
    DBI::dbExecute(
      con,
      "CREATE TABLE IF NOT EXISTS amr_results (
         id INTEGER PRIMARY KEY AUTOINCREMENT,
         souche TEXT, gene_symbol TEXT, sequence_name TEXT, element_type TEXT,
         element_subtype TEXT, class TEXT, subclass TEXT, method TEXT,
         pct_coverage REAL, pct_identity REAL, contig TEXT, start INTEGER,
         stop INTEGER, strand TEXT, ref_accession TEXT, ref_name TEXT,
         organism TEXT, point_mutations INTEGER, identity_threshold REAL,
         abritamr_version TEXT, amrfinder_version TEXT,
         amrfinder_db_version TEXT, called_at TEXT)"
    )
    DBI::dbExecute(
      con,
      "CREATE TABLE IF NOT EXISTS amr_summary (
         id INTEGER PRIMARY KEY AUTOINCREMENT,
         souche TEXT, section TEXT, drug_class TEXT, genes TEXT, called_at TEXT)"
    )
    for (s in souche) {
      DBI::dbExecute(
        con,
        "INSERT INTO amr_results
           (souche, gene_symbol, element_type, class, method, called_at)
         VALUES (?, ?, ?, ?, ?, ?)",
        list(s, "blaTEST", "AMR", "BETA-LACTAM", "EXACTX", "2026-01-01")
      )
      DBI::dbExecute(
        con,
        "INSERT INTO amr_summary (souche, section, drug_class, genes, called_at)
         VALUES (?, ?, ?, ?, ?)",
        list(s, "matches", "Beta-lactam", "blaTEST", "2026-01-01")
      )
    }
  }

  invisible(path)
}

q1 <- function(path, sql, params = NULL) {
  con <- DBI::dbConnect(RSQLite::SQLite(), path)
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  DBI::dbGetQuery(con, sql, params = params)[[1]]
}

qdf <- function(path, sql, params = NULL) {
  con <- DBI::dbConnect(RSQLite::SQLite(), path)
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  DBI::dbGetQuery(con, sql, params = params)
}
