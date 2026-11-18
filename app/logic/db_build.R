# app/logic/db_build.R
#
# Builds a new PhyloTrace database from a validated setup plan: pyMLST creates
# the cgMLST reference in a background process, then the scheme description,
# locus annotation, classical MLST scheme and screening settings are written in
# one transaction. The database only appears under its final name once all of
# that has succeeded.

box::use(
  DBI[
    dbBegin,
    dbCommit,
    dbDisconnect,
    dbExecute,
    dbGetQuery,
    dbRollback,
    dbWriteTable,
  ],
  processx[process],
)

box::use(
  app / logic / amr[AMR_ORGANISM_KEY],
  app / logic / analysis_store[ensure_schema],
  app / logic / app_meta[APP_VERSION],
  app / logic / db_compat[REF_SOUCHE],
  app / logic / db_connect[connect],
  app / logic / logging[log_event],
  app / logic / mlst_scheme[write_mlst_scheme],
  app / logic / pymlst[conda_env],
  app / logic / scheme_files[pymlst_gene_id, write_coregene],
  app / logic / species[TAXID_KEY, species_parts],
)

# The conda executable, as the typing scripts resolve it.
conda_exe <- function() {
  exe <- Sys.getenv("CONDA_EXE")
  if (nzchar(exe)) exe else "conda"
}

# TRUE for a single non-empty, non-NA string.
has_text <- function(x) {
  is.character(x) && length(x) == 1L && !is.na(x) && nzchar(trimws(x))
}

#' Path of a Build's Unfinished Database
#'
#' @description The file a build writes until it is complete. It sits next to
#'   the target so the final rename never crosses a file system.
#'
#' @param target Character path the finished database will have.
#'
#' @return Character path.
#' @export
partial_path <- function(target) {
  file.path(dirname(target), paste0(".", basename(target), ".partial"))
}

#' Scheme Overview Rows of a Setup Plan
#'
#' @description The key/value description stored as `scheme_overview`. Keys
#'   shared with databases imported from cgmlst.org keep their names, so every
#'   reader (cgMLST overview, typing provenance, the default MST threshold)
#'   treats both kinds alike. Values are plain text; nothing is stored as HTML.
#'
#' @param plan Setup plan list (see `start_db_build()`).
#' @param locus_count Integer number of loci in the built reference.
#'
#' @return Data frame with `key` and `value`.
#' @export
overview_rows <- function(plan, locus_count) {
  info <- plan$cg$info
  parts <- species_parts(plan$species)
  distance <- info$cluster_distance
  has_distance <- is.numeric(distance) && length(distance) == 1L && !is.na(distance)
  rows <- list(
    "Name" = info$name,
    "URL" = info$url,
    "Database" = info$source_label,
    "Version" = info$version,
    "Seed Genome" = info$seed_genome,
    "Genus" = parts$genus,
    "Species" = paste(parts$epithets, collapse = "/"),
    "Locus Count" = format(locus_count, big.mark = ","),
    "Complex Type Distance" = if (has_distance) as.character(distance),
    "Publication" = info$citation,
    "Provider Citation" = info$provider_citation,
    "Acknowledgement" = info$acknowledgement,
    "Terms of Use" = info$terms_url,
    "Imported" = format(Sys.Date()),
    "Imported From" = info$files
  )
  keep <- vapply(rows, has_text, logical(1))
  data.frame(
    key = names(rows)[keep],
    value = trimws(unlist(rows[keep], use.names = FALSE)),
    stringsAsFactors = FALSE
  )
}

#' Start Building a Database
#'
#' @description Writes the scheme's reference sequences to a temporary FASTA
#'   and launches `wgMLST create` on it without blocking the session. Poll the
#'   returned process, then call `finish_db_build()`, or `cancel_db_build()`.
#'
#' @details
#' The setup applies pyMLST's own checks to the references beforehand (coding
#' sequence, gene id collisions, duplicate sequences), so pyMLST runs without
#' `-r`: a duplicate the setup missed fails the build with pyMLST's message
#' rather than silently dropping both loci involved.
#'
#' @param plan Setup plan: `species`, `taxid`, `target`, `amr_organism`, `cg`
#'   (`loci`, `targets`, `info`) and optionally `mlst` (`scheme`, `info`).
#' @param env Character conda environment holding pyMLST.
#'
#' @return List with `proc`, `target`, `partial`, `coregene` and `log`.
#' @export
start_db_build <- function(plan, env = conda_env) {
  if (file.exists(plan$target)) {
    stop("A file already exists at the chosen location.")
  }
  partial <- partial_path(plan$target)
  unlink(c(partial, paste0(partial, "-journal")))
  coregene <- tempfile("coregene_", fileext = ".fasta")
  write_coregene(plan$cg$loci, coregene)
  log <- tempfile("db_build_", fileext = ".log")

  version <- plan$cg$info$version
  proc <- tryCatch(
    process$new(
      command = conda_exe(),
      args = c(
        "run", "--no-capture-output", "-n", env,
        "wgMLST", "create",
        "-s", plan$species,
        if (has_text(version)) c("-V", trimws(version)),
        partial,
        coregene
      ),
      stdout = log,
      stderr = "2>&1",
      cleanup_tree = TRUE
    ),
    error = function(e) {
      unlink(c(coregene, log))
      stop("pyMLST could not be started: ", conditionMessage(e), call. = FALSE)
    }
  )
  log_event(
    "DB",
    "build started",
    sprintf("%s | %d loci | %s", plan$target, nrow(plan$cg$loci), plan$species)
  )
  list(proc = proc, target = plan$target, partial = partial, coregene = coregene, log = log)
}

#' Cancel or Clean Up a Build
#'
#' @description Kills the build's process tree, if it still runs, and removes
#'   its unfinished database and temporary files.
#'
#' @param build List from `start_db_build()`.
#'
#' @return Invisible `NULL`.
#' @export
cancel_db_build <- function(build) {
  if (is.null(build)) {
    return(invisible(NULL))
  }
  if (!is.null(build$proc) && build$proc$is_alive()) {
    tryCatch(build$proc$kill_tree(), error = function(e) NULL)
    # Windows refuses to delete a file a dying process still holds open.
    tryCatch(build$proc$wait(3000), error = function(e) NULL)
  }
  unlink(c(build$partial, paste0(build$partial, "-journal"), build$coregene, build$log))
  invisible(NULL)
}

#' Reason a Build Failed
#'
#' @param log_lines Character vector of the build log.
#'
#' @return The last error line pyMLST or conda printed, or `NA`.
#' @export
build_error <- function(log_lines) {
  errors <- grep("error", log_lines, value = TRUE, ignore.case = TRUE)
  errors <- errors[!grepl("conda.cli.main_run", errors, fixed = TRUE)]
  if (!length(errors)) {
    return(NA_character_)
  }
  trimws(sub("^.*?Error:?\\s*", "", errors[length(errors)], perl = TRUE))
}

# Runs `fun(con)` on a connection that is closed again before returning, so no
# handle is left on a file that is about to be renamed or removed.
with_db <- function(path, fun) {
  con <- connect(path)
  on.exit(dbDisconnect(con))
  fun(con)
}

# Writes everything PhyloTrace adds to pyMLST's reference, in the caller's
# transaction. Returns the loci the reference actually holds.
write_setup <- function(con, plan) {
  built <- dbGetQuery(
    con,
    "SELECT gene FROM mlst WHERE souche = ?",
    params = list(REF_SOUCHE)
  )$gene
  if (!length(built)) {
    stop("pyMLST accepted none of the scheme's loci.")
  }
  info <- plan$cg$info
  dbExecute(
    con,
    "UPDATE mlst_type SET source = ?, species = ? WHERE name = 'wg'",
    params = list(info$source_label, plan$species)
  )
  dbWriteTable(con, "scheme_overview", overview_rows(plan, length(built)), overwrite = TRUE)

  targets <- plan$cg$targets
  targets <- targets[pymlst_gene_id(targets$Locus) %in% built, , drop = FALSE]
  dbWriteTable(con, "targets", targets, overwrite = TRUE)

  if (!is.null(plan$mlst)) {
    write_mlst_scheme(con, plan$mlst$scheme, plan$mlst$info)
  }
  built
}

# Records the screening settings and origin of a freshly built database.
write_meta <- function(con, plan) {
  meta <- list(
    if (has_text(plan$amr_organism)) plan$amr_organism else "",
    if (is.null(plan$taxid) || is.na(plan$taxid)) "" else as.character(plan$taxid),
    paste("PhyloTrace", APP_VERSION)
  )
  names(meta) <- c(AMR_ORGANISM_KEY, TAXID_KEY, "created_with")
  for (key in names(meta)) {
    dbExecute(
      con,
      "INSERT OR REPLACE INTO phylotrace_meta (key, value) VALUES (?, ?)",
      params = list(key, meta[[key]])
    )
  }
  invisible(TRUE)
}

#' Finish Building a Database
#'
#' @description Completes a build whose pyMLST process has exited: verifies the
#'   reference, writes the scheme description, locus annotation, classical MLST
#'   scheme and settings, and only then moves the database to its final path.
#'   Any failure removes the unfinished file and stops with the reason.
#'
#' @param build List from `start_db_build()`.
#' @param plan The setup plan the build was started with.
#'
#' @return List with `path`, `loci` (loci in the reference) and `skipped`
#'   (loci of the plan pyMLST did not accept).
#' @export
finish_db_build <- function(build, plan) {
  log_lines <- if (file.exists(build$log)) readLines(build$log, warn = FALSE) else character(0)
  fail <- function(reason) {
    cancel_db_build(build)
    log_event("DB", "build failed", reason)
    stop(reason, call. = FALSE)
  }
  if (!identical(build$proc$get_exit_status(), 0L) || !file.exists(build$partial)) {
    reason <- build_error(log_lines)
    fail(if (is.na(reason)) "pyMLST could not create the database." else reason)
  }

  built <- tryCatch(
    with_db(build$partial, function(con) {
      dbBegin(con)
      tryCatch(
        {
          loci <- write_setup(con, plan)
          dbCommit(con)
          loci
        },
        error = function(e) {
          dbRollback(con)
          stop(e)
        }
      )
    }),
    error = function(e) e
  )
  if (inherits(built, "error")) {
    fail(conditionMessage(built))
  }

  meta <- tryCatch(
    {
      ensure_schema(build$partial)
      with_db(build$partial, function(con) write_meta(con, plan))
    },
    error = function(e) e
  )
  if (inherits(meta, "error")) {
    fail(conditionMessage(meta))
  }

  if (file.exists(build$target)) {
    fail("A file appeared at the chosen location while the database was built.")
  }
  if (!file.rename(build$partial, build$target)) {
    fail("The finished database could not be moved to the chosen location.")
  }
  unlink(c(build$coregene, build$log))

  skipped <- setdiff(pymlst_gene_id(plan$cg$loci$locus), built)
  log_event(
    "DB",
    "build finished",
    sprintf("%s | %d loci, %d skipped", build$target, length(built), length(skipped))
  )
  list(path = build$target, loci = length(built), skipped = skipped)
}
