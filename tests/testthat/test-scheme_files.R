box::use(
  processx[run],
  stats[setNames],
  testthat[
    expect_equal,
    expect_error,
    expect_false,
    expect_identical,
    expect_match,
    expect_setequal,
    expect_true,
    test_that,
  ],
  withr[local_tempdir],
)
box::use(
  app / logic / scheme_files,
)

impl <- attr(scheme_files, "namespace")

# A complete coding sequence under table 11: start, `n` sense codons, stop.
cds <- function(n, codons = c("GCT", "GAA", "AAA", "CTG", "GGT", "ACC")) {
  paste0("ATG", paste(rep_len(codons, n), collapse = ""), "TAA")
}

# Writes one FASTA file per locus into `dir`: allele 1 is the given sequence.
write_loci <- function(dir, seqs) {
  for (locus in names(seqs)) {
    writeLines(
      c(">1", seqs[[locus]], ">2", sub("GCT", "GCC", seqs[[locus]])),
      file.path(dir, paste0(locus, ".fasta"))
    )
  }
  dir
}

test_that("is_cds follows the bacterial code", {
  expect_true(impl$is_cds(cds(10)))
  expect_true(impl$is_cds("GTGAAATAG"))
  expect_false(impl$is_cds("ATGAAATAGAAATAA"))
  expect_false(impl$is_cds("ATGAAAATAA"))
  expect_false(impl$is_cds("CCCAAATAA"))
})

test_that("orient_cds reverse-complements only what reads as coding that way", {
  fwd <- cds(5)
  out <- scheme_files$orient_cds(c(fwd, scheme_files$reverse_complement(fwd), "ATGTAGTAA"))
  expect_identical(out$sequence, c(fwd, fwd, NA))
  expect_identical(out$flipped, c(FALSE, TRUE, FALSE))
})

test_that("read_cg_scheme takes the first allele and reports every reject", {
  # Loci are visited in name order, so "earlier" means alphabetically first.
  dir <- write_loci(local_tempdir(), list(
    LOC_1 = cds(30),
    LOC_2 = cds(31, c("CTG", "AAA")),
    REV_1 = scheme_files$reverse_complement(cds(32, c("GGT", "ACC"))),
    ZDUP_1 = cds(30),
    "ZOC-1" = cds(33, c("GAA")),
    ZOC_1 = cds(34, c("ACC", "GAA")),
    STOP_1 = "ATGAAATAGAAATAA",
    N_1 = "ATGNNNTAA"
  ))
  writeLines(c(">1", cds(5)), file.path(dir, "bad name!.fasta"))

  scheme <- scheme_files$read_cg_scheme(dir)

  expect_setequal(scheme$loci$locus, c("LOC_1", "LOC_2", "REV_1", "ZOC-1"))
  expect_identical(scheme$loci$sequence[scheme$loci$locus == "LOC_1"], cds(30))
  expect_true(scheme$loci$flipped[scheme$loci$locus == "REV_1"])
  problems <- setNames(scheme$issues$problem, scheme$issues$locus)
  expect_match(problems[["ZDUP_1"]], "identical to an earlier locus")
  expect_match(problems[["ZOC_1"]], "collides", fixed = TRUE)
  expect_match(problems[["STOP_1"]], "coding sequence")
  expect_match(problems[["N_1"]], "A, C, G, T")
  expect_match(problems[["bad name!"]], "invalid locus name")
})

test_that("a ZIP archive and a single reference FASTA read like the folder", {
  dir <- write_loci(local_tempdir(), list(A_1 = cds(20), B_2 = cds(21, c("CTG"))))
  from_dir <- scheme_files$read_cg_scheme(dir)

  archive <- file.path(local_tempdir(), "scheme.zip")
  run("zip", c("-q", archive, list.files(dir)), wd = dir)
  from_zip <- scheme_files$read_cg_scheme(archive)

  coregene <- file.path(local_tempdir(), "ref.fasta")
  scheme_files$write_coregene(from_dir$loci, coregene)
  from_fasta <- scheme_files$read_cg_scheme(coregene)

  expect_identical(from_zip$loci, from_dir$loci)
  expect_identical(from_fasta$loci$sequence, from_dir$loci$sequence)
})

test_that("an archive with an escaping path is refused before anything is extracted", {
  dir <- local_tempdir()
  inner <- file.path(dir, "a", "b")
  dir.create(inner, recursive = TRUE)
  writeLines(c(">1", cds(5)), file.path(dir, "evil.fasta"))
  archive <- file.path(dir, "evil.zip")
  # zip -r stores the relative path as given, "../" included.
  run("zip", c("-q", archive, "../../evil.fasta"), wd = inner)

  expect_error(scheme_files$read_cg_scheme(archive), "unsafe paths")
})

test_that("the locus table is matched by content and keeps every scheme locus", {
  path <- file.path(local_tempdir(), "loci.tsv")
  # The cgmlst.org export: data rows carry one more, empty, leading field.
  writeLines(
    c(
      "Locus\tGene\tStart\tLength\tProduct",
      "\tLOC_1\tabcA\t1\t99\tsome protein",
      "\tUNRELATED\tzzz\t5\t10\tother"
    ),
    path
  )
  table <- scheme_files$read_scheme_table(path)
  mapping <- scheme_files$guess_locus_columns(table, c("LOC-1", "LOC_2"))
  built <- scheme_files$build_targets(
    c("LOC_1", "LOC_2"),
    table,
    mapping$locus,
    setdiff(names(table), "Product")
  )

  expect_identical(mapping$locus, "Locus")
  expect_identical(built$annotated, 1L)
  expect_identical(built$targets$Locus, c("LOC_1", "LOC_2"))
  expect_identical(built$targets$Gene, c("abcA", NA))
  expect_false("Product" %in% names(built$targets))
})

test_that("any annotation column is imported, renamed where the overview uses the name", {
  table <- data.frame(
    id = c("A", "B"),
    Missing = c("x", "y"),
    `Essential?` = c("yes", "no"),
    check.names = FALSE
  )
  built <- scheme_files$build_targets(c("A", "B", "C"), table, "id", c("Missing", "Essential?"))
  expect_identical(names(built$targets), c("Locus", "Missing annotation", "Essential?"))
  expect_identical(built$targets[["Essential?"]], c("yes", "no", NA))
  expect_identical(built$annotated, 2L)
})

test_that("an MLST scheme is assembled from a profile table and one FASTA", {
  dir <- local_tempdir()
  profiles <- file.path(dir, "profiles.tsv")
  writeLines(
    c(
      "ST\tadk\tfumC\tclonal_complex",
      "1\t1\t1\tCC1",
      "2\t1\t2\tCC1",
      "3\tN\t2\t"
    ),
    profiles
  )
  alleles <- file.path(dir, "alleles.fasta")
  writeLines(
    c(">adk_1", cds(4), ">fumC_1", cds(5), ">fumC_2", cds(6), ">fumC-3", cds(7)),
    alleles
  )

  prof <- scheme_files$read_mlst_profiles(profiles)
  scheme <- scheme_files$assemble_mlst_scheme(
    prof,
    scheme_files$read_mlst_alleles(alleles, prof$candidates)
  )

  expect_identical(prof$candidates, c("adk", "fumC"))
  expect_identical(scheme$loci, c("adk", "fumC"))
  expect_identical(scheme$summary[["profiles"]], 2L)
  expect_identical(scheme$summary[["alleles"]], 4L)
  expect_equal(sort(unique(scheme$profiles$st)), c(1L, 2L))
  expect_match(scheme$issues$problem, "1 profile row", all = FALSE)
})

test_that("an MLST locus without alleles or without allele 1 is refused", {
  dir <- local_tempdir()
  profiles <- file.path(dir, "profiles.csv")
  writeLines(c("ST,adk,fumC", "1,1,1"), profiles)
  prof <- scheme_files$read_mlst_profiles(profiles)

  only_adk <- file.path(dir, "adk.fasta")
  writeLines(c(">adk_1", cds(4)), only_adk)
  expect_error(
    scheme_files$assemble_mlst_scheme(prof, scheme_files$read_mlst_alleles(only_adk, prof$candidates)),
    "No alleles were found for profile locus 'fumC'"
  )

  fumc <- file.path(dir, "fumC.fasta")
  writeLines(c(">fumC_2", cds(5)), fumc)
  expect_error(
    scheme_files$assemble_mlst_scheme(
      prof,
      scheme_files$read_mlst_alleles(c(only_adk, fumc), prof$candidates)
    ),
    "Allele 1 is missing for locus 'fumC'"
  )
})

test_that("one locus file picked instead of the cgMLST scheme is refused", {
  dir <- local_tempdir()
  numbered <- file.path(dir, "BP0001.fasta")
  writeLines(c(">1", cds(4), ">2", cds(5), ">3", cds(6)), numbered)
  expect_error(scheme_files$read_cg_scheme(numbered), "alleles of a single locus")
  prefixed <- file.path(dir, "abcA.fasta")
  writeLines(c(">abcA_1", cds(4), ">abcA_2", cds(5)), prefixed)
  expect_error(scheme_files$read_cg_scheme(prefixed), "alleles of a single locus")
})

test_that("an unusable locus file is reported by reason, never by its sequence", {
  dir <- write_loci(local_tempdir(), list(A = cds(20)))
  writeLines(c(">1", "ATG-AAA"), file.path(dir, "B.fasta"))
  writeLines(">1", file.path(dir, "C.fasta"))
  res <- scheme_files$read_cg_scheme(dir)
  expect_identical(res$loci$locus, "A")
  expect_true(all(nchar(res$issues$problem) < 100L))
  expect_true(all(nzchar(res$issues$problem)))
})

test_that("tables read robustly whatever their quoting and encoding", {
  dir <- local_tempdir()
  tsv <- file.path(dir, "a.tsv")
  writeLines(c("Locus\tProduct", "a1\t5\" end", "a2\tplain"), tsv)
  expect_identical(scheme_files$read_scheme_table(tsv)$Product, c("5\" end", "plain"))

  quoted <- file.path(dir, "b.csv")
  writeLines(c("\"Locus\",\"Gene, name\"", "a1,\"g, 1\""), quoted)
  expect_identical(scheme_files$read_scheme_table(quoted)[["Gene, name"]], "g, 1")

  open_quote <- file.path(dir, "c.csv")
  writeLines(c("Locus,Product", "a1,\"5 end", "a2,plain"), open_quote)
  expect_error(scheme_files$read_scheme_table(open_quote), "unmatched quote")

  latin1 <- file.path(dir, "d.csv")
  writeBin(c(charToRaw("Locus,Product\na1,Prot"), as.raw(0xe9), charToRaw("ine\n")), latin1)
  expect_identical(scheme_files$read_scheme_table(latin1)$Product, "Prot\u00e9ine")
})

test_that("size limits apply to decompressed content", {
  path <- file.path(local_tempdir(), "big.fasta")
  con <- gzfile(path, "w")
  writeLines(c(">a", strrep("A", 3 * 1024^2)), con)
  close(con)
  expect_true(file.size(path) < 1024^2)
  expect_error(scheme_files$read_fasta(path, max_bytes = 1024^2), "more than the 1 MB")
  expect_error(impl$read_first_record(path), "longer than allowed")
})

test_that("ZIP members under ./ are read, and an empty archive says so", {
  dir <- write_loci(local_tempdir(), list(A = cds(20), B = cds(21, c("CTG"))))
  archive <- file.path(local_tempdir(), "dot.zip")
  # The zip tool strips a leading "./"; Python's zipfile keeps it as given.
  run("python3", c(
    "-I", "-c",
    paste(
      "import sys, zipfile",
      "z = zipfile.ZipFile(sys.argv[1], 'w')",
      "[z.write(f, './' + f.split('/')[-1]) for f in sys.argv[2:]]",
      "z.close()",
      sep = "; "
    ),
    archive, file.path(dir, c("A.fasta", "B.fasta"))
  ))
  expect_identical(scheme_files$read_cg_scheme(archive)$loci$locus, c("A", "B"))

  empty_dir <- local_tempdir()
  dir.create(file.path(empty_dir, "sub"))
  empty <- file.path(local_tempdir(), "empty.zip")
  run("zip", c("-q", "-r", empty, "sub"), wd = empty_dir)
  expect_error(scheme_files$read_cg_scheme(empty), "contains no files")
})

test_that("a locus file with a damaged first header is rejected, not shifted", {
  dir <- write_loci(local_tempdir(), list(A = cds(20), B = cds(21, c("CTG"))))
  writeLines(c("asd>1", "ATG00AAATAA", ">2", cds(22)), file.path(dir, "C.fasta"))
  res <- scheme_files$read_cg_scheme(dir)
  expect_identical(res$loci$locus, c("A", "B"))
  expect_match(res$issues$problem[res$issues$locus == "C"], "does not start with a FASTA header")

  single <- file.path(local_tempdir(), "alleles.fasta")
  writeLines(c("junk", ">adk_1", cds(4)), single)
  expect_error(scheme_files$read_fasta(single), "alleles.fasta.*does not start with a FASTA header")

  # Blank lines before the first header are harmless.
  blank <- file.path(local_tempdir(), "blank.fasta")
  writeLines(c("", "  ", ">1", cds(4)), blank)
  expect_identical(nrow(scheme_files$read_fasta(blank)), 1L)
})

test_that("MLST locus names that are no safe gene id or would merge are refused", {
  dir <- local_tempdir()
  profile <- function(header) {
    path <- file.path(dir, "profiles.tsv")
    writeLines(c(header, "1\t1\t1"), path)
    path
  }
  expect_error(scheme_files$read_mlst_profiles(profile("ST\tgene one\tadk")), "'gene one'")
  expect_error(scheme_files$read_mlst_profiles(profile("ST\t../x\tadk")), "'../x'")
  expect_error(scheme_files$read_mlst_profiles(profile("ST\ta_b\ta-b")), "'a-b' collides")
  expect_error(scheme_files$read_mlst_profiles(profile("ST\tadk\tADK")), "'ADK' collides")
  expect_error(scheme_files$read_mlst_profiles(profile("adk\tfumC\tgyrB")), "no ST column")

  # A row-number column without a header is no locus.
  rownum <- file.path(dir, "rownum.csv")
  writeLines(c('"","ST","adk"', '"1",1,1'), rownum)
  expect_identical(scheme_files$read_mlst_profiles(rownum)$candidates, "adk")
})

test_that("MLST alleles and STs follow claMLST's uniqueness rules", {
  dir <- local_tempdir()
  profiles <- file.path(dir, "profiles.tsv")
  writeLines(c("ST\tadk", "1\t1", "01\t2", "9999999999\t1"), profiles)
  alleles <- file.path(dir, "alleles.fasta")
  writeLines(
    c(">adk_1", cds(4), ">adk_2", cds(4), ">adk_3", cds(5), ">adk_99999999999", cds(6)),
    alleles
  )
  prof <- scheme_files$read_mlst_profiles(profiles)
  read <- scheme_files$read_mlst_alleles(alleles, prof$candidates)
  scheme <- scheme_files$assemble_mlst_scheme(prof, read)

  expect_identical(read$alleles$allele, c(1L, 3L))
  expect_match(read$issues$problem, "same sequence", all = FALSE)
  expect_match(read$issues$problem, "too large", all = FALSE)
  expect_identical(unique(scheme$profiles$st), 1L)
  expect_match(scheme$issues$problem, "2 profile row", all = FALSE)
})

test_that("files are recognised by content when a download lost its extension", {
  dir <- write_loci(local_tempdir(), list(A_1 = cds(20), B_2 = cds(21, c("CTG"))))
  archive <- file.path(local_tempdir(), "null")
  run("zip", c("-q", paste0(archive, ".zip"), list.files(dir)), wd = dir)
  file.rename(paste0(archive, ".zip"), archive)
  table <- file.path(local_tempdir(), "null (1)")
  writeLines(c("Locus\tGene", "A_1\tabcA"), table)

  expect_identical(scheme_files$sniff_file(archive), "zip")
  expect_identical(scheme_files$sniff_file(table), "text")
  expect_identical(scheme_files$sniff_file(file.path(dir, "A_1.fasta")), "fasta")
  expect_identical(scheme_files$read_cg_scheme(archive)$loci$locus, c("A_1", "B_2"))
  expect_identical(scheme_files$read_scheme_table(table)$Gene, "abcA")
  expect_error(scheme_files$read_scheme_table(archive), "ZIP archive")
})
