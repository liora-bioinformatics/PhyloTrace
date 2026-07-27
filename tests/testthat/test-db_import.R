box::use(
  testthat[
    expect_equal,
    expect_error,
    expect_false,
    expect_identical,
    expect_setequal,
    expect_true,
    test_that
  ],
  withr[local_tempdir],
)
box::use(
  app /
    logic /
    db_import[
      classify_isolate_collisions,
      default_resolutions,
      import_preview,
      importable_custom_fields,
      isolate_profile_hashes,
      list_backups,
      merge_databases,
      restore_backup,
      suggest_rename
    ],
  app / logic / database_functions[remove_isolates],
)

# `merge_databases()` and `hash_database()` are chatty; the messages are useful
# in the app but only noise here.
quiet <- function(expr) suppressMessages(expr)

pair <- function(dir, local_meta = TRUE, peer_meta = TRUE, ...) {
  local <- file.path(dir, "local.db")
  peer <- file.path(dir, "peer.db")
  build_db(
    local,
    default_local(),
    metadata = if (local_meta) meta_df(c("A", "B")) else NULL
  )
  build_db(
    peer,
    default_peer(),
    metadata = if (peer_meta) meta_df(c("B", "C")) else NULL,
    ...
  )
  list(local = local, peer = peer)
}

resolve <- function(isolate, action, final = isolate) {
  data.frame(
    ext_isolate = isolate,
    action = action,
    final_isolate = final,
    stringsAsFactors = FALSE
  )
}

test_that("isolate_profile_hashes keys by isolate and excludes ref", {
  dir <- local_tempdir()
  p <- pair(dir)

  h <- isolate_profile_hashes(p$local)

  expect_setequal(names(h), c("A", "B"))
  expect_false("ref" %in% names(h))
  expect_true(all(nchar(h) == 64L))
})

test_that("collisions are classified by allele profile, not by name alone", {
  dir <- local_tempdir()
  p <- pair(dir)

  cl <- classify_isolate_collisions(p$local, p$peer)

  # B exists locally with an identical profile; C is new.
  expect_identical(cl$status[cl$isolate == "B"], "identical_duplicate")
  expect_identical(cl$status[cl$isolate == "C"], "new")
})

test_that("a same-named isolate with a different profile is a name clash", {
  dir <- local_tempdir()
  local <- file.path(dir, "local.db")
  peer <- file.path(dir, "peer.db")
  build_db(local, default_local())

  peer_alleles <- default_peer()
  peer_alleles$B[["g2"]] <- seqv("DIVERGED")
  build_db(peer, peer_alleles)

  cl <- classify_isolate_collisions(local, peer)
  expect_identical(cl$status[cl$isolate == "B"], "name_clash")
})

test_that("import_preview counts novel vs shared alleles without writing", {
  dir <- local_tempdir()
  p <- pair(dir)
  before <- tools::md5sum(p$local)

  pv <- import_preview(p$local, p$peer)

  expect_equal(pv$n_new_isolates, 1L)
  expect_equal(pv$n_identical_dupes, 1L)
  expect_equal(pv$n_name_clashes, 0L)
  # Only C is accepted by default: g1=C1 is novel; g2=A2 and g3=R3 already exist.
  expect_equal(pv$n_new_alleles, 1L)
  expect_equal(pv$n_shared_alleles, 2L)
  expect_identical(tools::md5sum(p$local), before)
})

test_that("merging adds new isolates and preserves their allele identity", {
  dir <- local_tempdir()
  p <- pair(dir)
  peer_profiles <- isolate_profile_hashes(p$peer)

  res <- quiet(merge_databases(
    p$local,
    p$peer,
    default_resolutions(classify_isolate_collisions(p$local, p$peer)),
    metadata_cols = "sample_collection_date",
    backup = FALSE
  ))

  expect_equal(res$added, 1L)
  expect_equal(res$skipped, 1L)
  expect_equal(res$new_alleles, 1L)

  after <- isolate_profile_hashes(p$local)
  expect_setequal(names(after), c("A", "B", "C"))
  # The imported isolate's profile hash is identical to the peer's: every allele
  # survived the seqid remap with its content intact.
  expect_identical(after[["C"]], peer_profiles[["C"]])
})

test_that("every stored hash still matches its sequence after a merge", {
  dir <- local_tempdir()
  p <- pair(dir)
  quiet(merge_databases(
    p$local,
    p$peer,
    default_resolutions(classify_isolate_collisions(p$local, p$peer)),
    backup = FALSE
  ))

  rows <- qdf(
    p$local,
    "SELECT s.sequence AS seq, h.hash AS hash
       FROM sequences s JOIN hashes h ON h.id = s.id"
  )
  expect_identical(as.character(openssl::sha256(rows$seq)), rows$hash)
})

test_that("a shared allele is reused and a novel one extends the id space", {
  dir <- local_tempdir()
  p <- pair(dir)
  n_before <- q1(p$local, "SELECT COUNT(*) FROM sequences")
  max_before <- q1(p$local, "SELECT MAX(id) FROM sequences")

  quiet(merge_databases(
    p$local,
    p$peer,
    default_resolutions(classify_isolate_collisions(p$local, p$peer)),
    backup = FALSE
  ))

  # Exactly one novel allele (C's g1); the other two were content-identical to
  # alleles the local database already held, so no rows were duplicated.
  expect_equal(q1(p$local, "SELECT COUNT(*) FROM sequences"), n_before + 1L)
  expect_equal(q1(p$local, "SELECT MAX(id) FROM sequences"), max_before + 1L)

  expect_equal(
    q1(p$local, "SELECT COUNT(*) FROM (SELECT id FROM sequences GROUP BY id HAVING COUNT(*) > 1)"),
    0L
  )
  expect_equal(
    q1(p$local, "SELECT COUNT(*) FROM (SELECT id FROM mlst GROUP BY id HAVING COUNT(*) > 1)"),
    0L
  )
})

test_that("the scheme reference is never imported as an isolate", {
  dir <- local_tempdir()
  p <- pair(dir)
  ref_before <- q1(p$local, "SELECT COUNT(*) FROM mlst WHERE souche = 'ref'")

  quiet(merge_databases(
    p$local,
    p$peer,
    default_resolutions(classify_isolate_collisions(p$local, p$peer)),
    backup = FALSE
  ))

  expect_equal(
    q1(p$local, "SELECT COUNT(*) FROM mlst WHERE souche = 'ref'"),
    ref_before
  )
  expect_false("ref" %in% names(isolate_profile_hashes(p$local)))
  expect_equal(q1(p$local, "SELECT COUNT(*) FROM metadata WHERE isolate = 'ref'"), 0L)
})

test_that("skip leaves the local isolate untouched", {
  dir <- local_tempdir()
  p <- pair(dir)
  before <- isolate_profile_hashes(p$local)

  quiet(merge_databases(p$local, p$peer, resolve("C", "add"), backup = FALSE))

  after <- isolate_profile_hashes(p$local)
  expect_identical(after[c("A", "B")], before[c("A", "B")])
})

test_that("rename imports alongside the original", {
  dir <- local_tempdir()
  local <- file.path(dir, "local.db")
  peer <- file.path(dir, "peer.db")
  build_db(local, default_local(), metadata = meta_df(c("A", "B")))
  peer_alleles <- default_peer()
  peer_alleles$B[["g2"]] <- seqv("DIVERGED")
  build_db(peer, peer_alleles, metadata = meta_df(c("B", "C")))

  before <- isolate_profile_hashes(local)
  new_name <- suggest_rename("B", names(before))
  expect_identical(new_name, "B_imp")

  quiet(merge_databases(local, peer, resolve("B", "rename", new_name), backup = FALSE))

  after <- isolate_profile_hashes(local)
  expect_setequal(names(after), c("A", "B", "B_imp"))
  expect_identical(after[["B"]], before[["B"]])
  expect_false(identical(after[["B_imp"]], before[["B"]]))
  expect_identical(q1(local, "SELECT COUNT(*) FROM metadata WHERE isolate = 'B_imp'"), 1L)
})

test_that("overwrite replaces the local isolate exactly once", {
  dir <- local_tempdir()
  local <- file.path(dir, "local.db")
  peer <- file.path(dir, "peer.db")
  build_db(local, default_local(), metadata = meta_df(c("A", "B")))
  peer_alleles <- default_peer()
  peer_alleles$B[["g2"]] <- seqv("DIVERGED")
  build_db(peer, peer_alleles, metadata = meta_df(c("B", "C")))

  before <- isolate_profile_hashes(local)
  peer_profiles <- isolate_profile_hashes(peer)

  quiet(merge_databases(local, peer, resolve("B", "overwrite"), backup = FALSE))

  after <- isolate_profile_hashes(local)
  expect_setequal(names(after), c("A", "B"))
  expect_identical(after[["B"]], peer_profiles[["B"]])
  expect_identical(after[["A"]], before[["A"]])

  # No leftover rows from the replaced isolate.
  expect_equal(q1(local, "SELECT COUNT(*) FROM mlst WHERE souche = 'B'"), 3L)
  expect_equal(q1(local, "SELECT COUNT(*) FROM metadata WHERE isolate = 'B'"), 1L)
})

test_that("invalid resolutions are rejected before anything is written", {
  dir <- local_tempdir()
  p <- pair(dir)
  before <- tools::md5sum(p$local)

  expect_error(quiet(merge_databases(p$local, p$peer, resolve("C", "rename", "A"), backup = FALSE)), "already in the database")
  expect_error(quiet(merge_databases(p$local, p$peer, resolve("C", "overwrite", "ghost"), backup = FALSE)), "do not exist locally")
  expect_error(quiet(merge_databases(p$local, p$peer, resolve("ghost", "add"), backup = FALSE)), "Not present in the external")
  expect_error(quiet(merge_databases(p$local, p$peer, resolve("ref", "add"), backup = FALSE)), "not an importable isolate")
  expect_error(quiet(merge_databases(p$local, p$peer, resolve("C", "skip"), backup = FALSE)), "Nothing selected")
  expect_error(
    quiet(merge_databases(
      p$local,
      p$peer,
      rbind(resolve("B", "rename", "X"), resolve("C", "rename", "X")),
      backup = FALSE
    )),
    "Duplicate target name"
  )

  expect_identical(tools::md5sum(p$local), before)
})

test_that("an incompatible peer database is refused", {
  dir <- local_tempdir()
  local <- file.path(dir, "local.db")
  peer <- file.path(dir, "peer.db")
  build_db(local, default_local())
  build_db(peer, default_peer(), species = "Escherichia coli")

  expect_error(
    quiet(merge_databases(local, peer, resolve("C", "add"), backup = FALSE)),
    "not compatible"
  )
})

test_that("selected metadata columns are adopted and the rest defaulted", {
  dir <- local_tempdir()
  local <- file.path(dir, "local.db")
  peer <- file.path(dir, "peer.db")
  build_db(
    local,
    default_local(),
    metadata = meta_df(c("A", "B"), extra = list(local_only = c("l1", "l2")))
  )
  build_db(
    peer,
    default_peer(),
    species = "Testus organismus",
    metadata = meta_df(
      c("B", "C"),
      species = "Wrong organism",
      extra = list(ward_id = c("W1", "W2"))
    )
  )

  quiet(merge_databases(
    local,
    peer,
    resolve("C", "add"),
    metadata_cols = c("ward_id", "sample_collection_date"),
    backup = FALSE
  ))

  row <- qdf(local, "SELECT * FROM metadata WHERE isolate = 'C'")

  # An external-only column the user selected becomes a new local column…
  expect_true("ward_id" %in% names(row))
  expect_identical(row$ward_id, "W2")
  # …a local-only column is simply empty for the import…
  expect_true(is.na(row$local_only))
  # …and organism is taken from the LOCAL mlst_type, never from the peer.
  expect_identical(row$organism, "Testus organismus")
})

test_that("unselected external metadata columns are not adopted", {
  dir <- local_tempdir()
  local <- file.path(dir, "local.db")
  peer <- file.path(dir, "peer.db")
  build_db(local, default_local(), metadata = meta_df(c("A", "B")))
  build_db(
    peer,
    default_peer(),
    metadata = meta_df(c("B", "C"), extra = list(secret = c("s1", "s2")))
  )

  quiet(merge_databases(local, peer, resolve("C", "add"), metadata_cols = character(0), backup = FALSE))

  expect_false("secret" %in% names(qdf(local, "SELECT * FROM metadata LIMIT 0")))
})

test_that("a peer without a metadata table still imports", {
  dir <- local_tempdir()
  p <- pair(dir, peer_meta = FALSE)

  quiet(merge_databases(p$local, p$peer, resolve("C", "add"), backup = FALSE))

  expect_true("C" %in% names(isolate_profile_hashes(p$local)))
  row <- qdf(p$local, "SELECT * FROM metadata WHERE isolate = 'C'")
  expect_equal(nrow(row), 1L)
  expect_identical(row$organism, "Testus organismus")
})

test_that("a peer without a hashes table is hashed on the fly", {
  dir <- local_tempdir()
  local <- file.path(dir, "local.db")
  peer <- file.path(dir, "peer.db")
  build_db(local, default_local(), metadata = meta_df(c("A", "B")))
  build_db(peer, default_peer(), with_hashes = FALSE)
  peer_md5 <- tools::md5sum(peer)

  quiet(merge_databases(local, peer, resolve("C", "add"), backup = FALSE))

  expect_true("C" %in% names(isolate_profile_hashes(local)))
  # Staging happens on a copy: the peer's own file is never modified.
  expect_identical(tools::md5sum(peer), peer_md5)
})

test_that("a merge into a hash-orphaned database repairs it", {
  dir <- local_tempdir()
  p <- pair(dir)

  # Simulate a database written by the older remove_isolates(), which pruned
  # `sequences` but left `hashes` behind. The stale row is parked at exactly the
  # id the merge will allocate next, which is the case that actually corrupts:
  # without a prune, that seqid ends up with two hash rows and every
  # mlst-to-hashes join doubles the isolate's profile.
  next_id <- q1(p$local, "SELECT MAX(id) + 1 FROM sequences")
  con <- DBI::dbConnect(RSQLite::SQLite(), p$local)
  DBI::dbExecute(
    con,
    "INSERT INTO hashes (id, hash) VALUES (?, 'stale-orphan-hash')",
    list(next_id)
  )
  DBI::dbDisconnect(con)

  quiet(merge_databases(p$local, p$peer, resolve("C", "add"), backup = FALSE))

  expect_equal(
    q1(p$local, "SELECT COUNT(*) FROM hashes h LEFT JOIN sequences s ON s.id = h.id WHERE s.id IS NULL"),
    0L
  )
  expect_equal(
    q1(p$local, "SELECT COUNT(*) FROM (SELECT id FROM hashes GROUP BY id HAVING COUNT(*) > 1)"),
    0L
  )
  # The stale hash was replaced by the real one for the newly imported allele.
  expect_equal(q1(p$local, "SELECT COUNT(*) FROM hashes WHERE hash = 'stale-orphan-hash'"), 0L)
  rows <- qdf(
    p$local,
    "SELECT s.sequence AS seq, h.hash AS hash FROM sequences s JOIN hashes h ON h.id = s.id"
  )
  expect_identical(as.character(openssl::sha256(rows$seq)), rows$hash)
})

test_that("a peer referencing a missing sequence rolls the merge back", {
  dir <- local_tempdir()
  p <- pair(dir)

  # C's g1 allele is unique to C, so removing it leaves the ref intact and the
  # compatibility gate happy — the failure must be caught by the merge itself.
  con <- DBI::dbConnect(RSQLite::SQLite(), p$peer)
  victim <- DBI::dbGetQuery(
    con,
    "SELECT seqid FROM mlst WHERE souche='C' AND gene='g1'"
  )$seqid
  DBI::dbExecute(con, "DELETE FROM sequences WHERE id = ?", list(victim))
  DBI::dbExecute(con, "DELETE FROM hashes WHERE id = ?", list(victim))
  DBI::dbDisconnect(con)

  before <- tools::md5sum(p$local)
  expect_error(
    quiet(merge_databases(p$local, p$peer, resolve("C", "add"), backup = FALSE)),
    "rolled back"
  )

  expect_identical(tools::md5sum(p$local), before)
  expect_identical(list.files(dir, pattern = "import-work"), character(0))
})

test_that("a backup is written and can be restored", {
  dir <- local_tempdir()
  p <- pair(dir)
  before <- isolate_profile_hashes(p$local)

  res <- quiet(merge_databases(
    p$local,
    p$peer,
    default_resolutions(classify_isolate_collisions(p$local, p$peer)),
    backup = TRUE
  ))

  expect_true(file.exists(res$backup_path))
  expect_identical(list_backups(p$local), res$backup_path)
  expect_true("C" %in% names(isolate_profile_hashes(p$local)))

  restore_backup(p$local, res$backup_path)

  expect_identical(isolate_profile_hashes(p$local), before)
  # The restore backs up what it displaced, so the merged state is recoverable.
  expect_equal(length(list_backups(p$local)), 2L)
})

test_that("remove_isolates prunes orphan hashes along with sequences when keep_alleles = FALSE", {
  dir <- local_tempdir()
  p <- pair(dir)

  # A carries three alleles no other strain references (A1/A2/A3).
  n_before <- q1(p$local, "SELECT COUNT(*) FROM sequences")
  remove_isolates(p$local, "A", keep_alleles = FALSE)

  # Its unique alleles are gone...
  expect_equal(q1(p$local, "SELECT COUNT(*) FROM sequences"), n_before - 3L)

  # ...and sequences/hashes stay in lockstep: no orphan on either side.
  expect_equal(
    q1(p$local, "SELECT COUNT(*) FROM hashes h LEFT JOIN sequences s ON s.id = h.id WHERE s.id IS NULL"),
    0L
  )
  expect_equal(
    q1(p$local, "SELECT COUNT(*) FROM sequences s LEFT JOIN hashes h ON h.id = s.id WHERE h.id IS NULL"),
    0L
  )
})

test_that("remove_isolates keeps allele sequences by default", {
  dir <- local_tempdir()
  p <- pair(dir)

  n_before <- q1(p$local, "SELECT COUNT(*) FROM sequences")
  remove_isolates(p$local, "A")

  # The mlst mapping is gone, but the allele DNA - and its hashes - stay behind
  # so a later hash-match re-uses the same allele identity. Keeping both sides
  # preserves parity, so there is no orphan to corrupt a future id allocation.
  expect_equal(q1(p$local, "SELECT COUNT(*) FROM mlst WHERE souche = 'A'"), 0L)
  expect_equal(q1(p$local, "SELECT COUNT(*) FROM sequences"), n_before)
  expect_equal(
    q1(p$local, "SELECT COUNT(*) FROM hashes h LEFT JOIN sequences s ON s.id = h.id WHERE s.id IS NULL"),
    0L
  )
})

test_that("a purged isolate can be re-imported to a byte-equal profile", {
  # The purge path (keep_alleles = FALSE): removal drops B's unique alleles, so
  # the merge re-adds them fresh and the sequence count returns to where it
  # started - nothing duplicated.
  dir <- local_tempdir()
  p <- pair(dir)
  before <- isolate_profile_hashes(p$local)
  n_seq <- q1(p$local, "SELECT COUNT(*) FROM sequences")

  remove_isolates(p$local, "B", keep_alleles = FALSE)
  expect_false("B" %in% names(isolate_profile_hashes(p$local)))

  quiet(merge_databases(p$local, p$peer, resolve("B", "add"), backup = FALSE))

  after <- isolate_profile_hashes(p$local)
  expect_identical(after[["B"]], before[["B"]])
  # No allele was duplicated on the way back in.
  expect_equal(q1(p$local, "SELECT COUNT(*) FROM sequences"), n_seq)
})

test_that("re-importing a kept isolate reuses its orphan alleles, no duplicates", {
  # The keep path (default keep_alleles = TRUE): removal leaves B's alleles as
  # orphans. The merge's (gene, hash) matcher can't see them (no gene survives),
  # but the orphan-reuse step matches them by hash and re-points the new mlst
  # rows at the existing seqids - so the count is unchanged and the orphans go
  # live again instead of a second hash-sharing copy being inserted.
  dir <- local_tempdir()
  p <- pair(dir)
  before <- isolate_profile_hashes(p$local)
  n_seq <- q1(p$local, "SELECT COUNT(*) FROM sequences")

  remove_isolates(p$local, "B") # keep_alleles = TRUE
  expect_false("B" %in% names(isolate_profile_hashes(p$local)))
  # Orphans linger while B is gone.
  expect_equal(q1(p$local, "SELECT COUNT(*) FROM sequences"), n_seq)

  quiet(merge_databases(p$local, p$peer, resolve("B", "add"), backup = FALSE))

  after <- isolate_profile_hashes(p$local)
  expect_identical(after[["B"]], before[["B"]])
  # No duplicate sequence and no duplicate hash: the orphans were reused.
  expect_equal(q1(p$local, "SELECT COUNT(*) FROM sequences"), n_seq)
  expect_equal(
    q1(p$local, "SELECT COUNT(*) FROM (SELECT hash FROM hashes GROUP BY hash HAVING COUNT(*) > 1)"),
    0L
  )
  # And every allele is referenced again - no orphans left over.
  expect_equal(
    q1(p$local, "SELECT COUNT(*) FROM sequences WHERE id NOT IN (SELECT seqid FROM mlst)"),
    0L
  )
})

# --- Analysis-result tables (classical_mlst / amr_*) -----------------------

test_that("result tables merge for accepted isolates when requested", {
  dir <- local_tempdir()
  p <- pair(dir)
  seed_results(p$peer, c("B", "C")) # peer carries classical + AMR results

  res <- quiet(merge_databases(
    p$local,
    p$peer,
    default_resolutions(classify_isolate_collisions(p$local, p$peer)),
    include_classical = TRUE,
    include_amr = TRUE,
    backup = FALSE
  ))

  # Tables were created in a local DB that never ran these analyses.
  tbls <- qdf(p$local, "SELECT name FROM sqlite_master WHERE type='table'")$name
  expect_true(all(
    c("classical_mlst", "amr_results", "amr_summary") %in% tbls
  ))

  # Only C is accepted (B is an identical duplicate -> skipped), so only C's
  # result rows come across.
  for (tbl in c("classical_mlst", "amr_results", "amr_summary")) {
    expect_setequal(
      q1(p$local, sprintf("SELECT DISTINCT isolate FROM %s", tbl)),
      "C"
    )
  }
  # 2 classical rows + 1 amr_results + 1 amr_summary for C.
  expect_equal(res$result_rows, 4L)
})

test_that("a renamed import remaps result-table isolate to the new name", {
  dir <- local_tempdir()
  p <- pair(dir)
  seed_results(p$peer, "C")

  quiet(merge_databases(
    p$local,
    p$peer,
    resolve("C", "rename", "C_imp"),
    include_classical = TRUE,
    include_amr = TRUE,
    backup = FALSE
  ))

  expect_setequal(
    q1(p$local, "SELECT DISTINCT isolate FROM classical_mlst"),
    "C_imp"
  )
  expect_equal(
    q1(p$local, "SELECT COUNT(*) FROM amr_results WHERE isolate = 'C'"),
    0L
  )
})

test_that("overwrite replaces result rows and re-import is idempotent", {
  dir <- local_tempdir()
  p <- pair(dir)
  seed_results(p$local, "B", classical = FALSE, amr = TRUE) # local already has B's AMR
  seed_results(p$peer, "B", classical = FALSE, amr = TRUE)

  merge_once <- function() {
    quiet(merge_databases(
      p$local,
      p$peer,
      resolve("B", "overwrite"),
      include_amr = TRUE,
      backup = FALSE
    ))
  }

  merge_once()
  expect_equal(
    q1(p$local, "SELECT COUNT(*) FROM amr_results WHERE isolate = 'B'"),
    1L # replaced, not appended to the pre-existing local row
  )

  merge_once()
  expect_equal(
    q1(p$local, "SELECT COUNT(*) FROM amr_results WHERE isolate = 'B'"),
    1L # a second identical import stays stable
  )
})

test_that("result tables are left untouched when the flags are off", {
  dir <- local_tempdir()
  p <- pair(dir)
  seed_results(p$peer, "C")

  res <- quiet(merge_databases(
    p$local,
    p$peer,
    default_resolutions(classify_isolate_collisions(p$local, p$peer)),
    backup = FALSE
  ))

  tbls <- qdf(p$local, "SELECT name FROM sqlite_master WHERE type='table'")$name
  expect_false("classical_mlst" %in% tbls)
  expect_false("amr_results" %in% tbls)
  expect_equal(res$result_rows, 0L)
  # The ordinary merge still happened.
  expect_setequal(names(isolate_profile_hashes(p$local)), c("A", "B", "C"))
})

test_that("selected custom variables are merged and mapped onto local ids", {
  dir <- local_tempdir()
  p <- pair(dir)
  # The local database already knows `ward`; `outbreak_id` is new to it.
  seed_custom(p$local, list(ward = list(type = "text", values = c(A = "ICU"))))
  seed_custom(
    p$peer,
    list(
      ward = list(type = "text", values = c(C = "ER")),
      outbreak_id = list(type = "text", values = c(C = "OB-1")),
      withheld = list(type = "text", values = c(C = "secret"))
    )
  )

  quiet(merge_databases(
    p$local,
    p$peer,
    resolve("C", "add"),
    custom_fields = c("ward", "outbreak_id"),
    backup = FALSE
  ))

  defs <- qdf(p$local, "SELECT id, name FROM phylotrace_custom_fields")
  # `ward` is merged into the existing definition, not duplicated.
  expect_setequal(defs$name, c("ward", "outbreak_id"))

  values <- qdf(
    p$local,
    "SELECT f.name AS name, v.isolate AS isolate, v.value AS value
       FROM phylotrace_custom_values v
       JOIN phylotrace_custom_fields f ON f.id = v.field_id
      ORDER BY f.name, v.isolate"
  )
  # The local value survives, the peer's lands under the local field id.
  expect_identical(values$value[values$name == "ward" & values$isolate == "A"], "ICU")
  expect_identical(values$value[values$name == "ward" & values$isolate == "C"], "ER")
  expect_identical(values$value[values$name == "outbreak_id"], "OB-1")
})

test_that("unselected custom variables are not adopted", {
  dir <- local_tempdir()
  p <- pair(dir)
  seed_custom(p$peer, list(secret = list(type = "text", values = c(C = "s"))))

  quiet(merge_databases(
    p$local,
    p$peer,
    resolve("C", "add"),
    custom_fields = character(0),
    backup = FALSE
  ))

  tbls <- qdf(p$local, "SELECT name FROM sqlite_master WHERE type='table'")$name
  expect_false("phylotrace_custom_fields" %in% tbls)
})

test_that("a custom variable with a clashing type is never imported", {
  dir <- local_tempdir()
  p <- pair(dir)
  seed_custom(p$local, list(ct_value = list(type = "text")))
  seed_custom(
    p$peer,
    list(ct_value = list(type = "numeric", values = c(C = "12.5")))
  )

  split <- importable_custom_fields(p$local, p$peer)
  expect_identical(split$importable, character(0))
  expect_identical(split$conflicts$name, "ct_value")
  expect_identical(split$conflicts$local_type, "text")
  expect_identical(split$conflicts$ext_type, "numeric")

  # Even when the caller asks for it anyway, the merge leaves it behind rather
  # than adopting values canonicalised under another type.
  quiet(merge_databases(
    p$local,
    p$peer,
    resolve("C", "add"),
    custom_fields = "ct_value",
    backup = FALSE
  ))

  expect_equal(q1(p$local, "SELECT COUNT(*) FROM phylotrace_custom_values"), 0L)
  expect_identical(
    q1(p$local, "SELECT type FROM phylotrace_custom_fields WHERE name='ct_value'"),
    "text"
  )
})

test_that("custom values follow a renamed isolate and a rewritten one", {
  dir <- local_tempdir()
  p <- pair(dir)
  seed_custom(
    p$peer,
    list(ward = list(type = "text", values = c(B = "peer-B", C = "peer-C")))
  )

  quiet(merge_databases(
    p$local,
    p$peer,
    rbind(resolve("B", "rename", "B_ext"), resolve("C", "add")),
    custom_fields = "ward",
    backup = FALSE
  ))

  values <- qdf(
    p$local,
    "SELECT isolate, value FROM phylotrace_custom_values ORDER BY isolate"
  )
  # The renamed isolate carries its value under its new name, never its old one.
  expect_setequal(values$isolate, c("B_ext", "C"))
  expect_identical(values$value[values$isolate == "B_ext"], "peer-B")
})

test_that("overwriting an isolate replaces its custom values", {
  dir <- local_tempdir()
  p <- pair(dir)
  seed_custom(
    p$local,
    list(ward = list(type = "text", values = c(B = "stale")))
  )
  seed_custom(p$peer, list(ward = list(type = "text", values = c(B = "fresh"))))

  quiet(merge_databases(
    p$local,
    p$peer,
    resolve("B", "overwrite"),
    custom_fields = "ward",
    backup = FALSE
  ))

  expect_equal(q1(p$local, "SELECT COUNT(*) FROM phylotrace_custom_values"), 1L)
  expect_identical(q1(p$local, "SELECT value FROM phylotrace_custom_values"), "fresh")
})

test_that("import_preview classifies the peer's custom variables", {
  dir <- local_tempdir()
  p <- pair(dir)
  seed_custom(
    p$local,
    list(ward = list(type = "text"), ct_value = list(type = "text"))
  )
  seed_custom(
    p$peer,
    list(
      ward = list(type = "text"),
      ct_value = list(type = "numeric"),
      outbreak_id = list(type = "text")
    )
  )

  pv <- quiet(import_preview(p$local, p$peer))

  expect_setequal(pv$custom_importable, c("ward", "outbreak_id"))
  expect_identical(pv$custom_shared, "ward")
  expect_identical(pv$custom_only_ext, "outbreak_id")
  expect_identical(pv$custom_conflicts$name, "ct_value")
})
