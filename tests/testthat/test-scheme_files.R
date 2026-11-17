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
  built <- scheme_files$build_targets(c("LOC_1", "LOC_2"), table, mapping)

  expect_identical(mapping$locus, "Locus")
  expect_identical(mapping$gene, "Gene")
  expect_identical(built$annotated, 1L)
  expect_identical(built$targets$Locus, c("LOC_1", "LOC_2"))
  expect_identical(built$targets$Gene, c("abcA", NA))
  expect_identical(built$targets$Product, c("some protein", NA))
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
