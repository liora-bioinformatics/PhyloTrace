box::use(
  shiny[reactive, testServer],
  testthat[expect_false, expect_match, expect_true, test_that],
  withr[local_tempdir],
)
box::use(
  app / logic / mlst_scheme,
  app / view / database_mlst,
)

# TRUE when the Scheme Setup import form is hidden by its gate.
form_hidden <- function(output) {
  grepl("display: none", paste(unlist(output$scheme_form_gate), collapse = ""), fixed = TRUE)
}

toy_scheme <- function() {
  list(
    loci = "adk",
    profiles = data.frame(st = 1L, locus = "adk", allele = 1L),
    alleles = data.frame(locus = "adk", allele = 1L, sequence = "ATGAAATAA")
  )
}

test_that("without a stored scheme the import form is open", {
  path <- file.path(local_tempdir(), "db.db")
  build_db(path, default_local())

  testServer(database_mlst$server, args = list(db_path = reactive(path)), {
    session$flushReact()
    expect_false(form_hidden(output))
    expect_match(paste(unlist(output$stored_scheme), collapse = ""), "carries no classical MLST")
  })
})

test_that("a stored scheme is shown in full and the form opens only on replace", {
  path <- file.path(local_tempdir(), "db.db")
  build_db(path, default_local())
  mlst_scheme$store_mlst_scheme(
    path,
    toy_scheme(),
    list(name = "Toy MLST", source = "own", source_label = "Own / in-house scheme")
  )

  testServer(database_mlst$server, args = list(db_path = reactive(path)), {
    session$flushReact()
    shown <- paste(unlist(output$stored_scheme), collapse = "")
    expect_match(shown, "Toy MLST", fixed = TRUE)
    expect_match(shown, "Replace scheme", fixed = TRUE)
    expect_true(form_hidden(output))

    session$setInputs(replace_scheme = 1)
    expect_false(form_hidden(output))

    session$setInputs(cancel_replace_scheme = 1)
    expect_true(form_hidden(output))
  })
})
