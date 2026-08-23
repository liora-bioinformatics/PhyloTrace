box::use(
  shiny[reactive, reactiveVal, testServer],
  testthat[
    expect_false,
    expect_identical,
    expect_null,
    expect_s3_class,
    expect_true,
    test_that
  ],
  withr[local_tempfile],
)
box::use(
  app / logic / field_profile,
  app / view / visualization_amr,
)

# A database carrying an AMR screen for three of its four isolates. ISO-4 was
# never screened, which is what a freshly imported isolate looks like.
amr_db <- function(env = parent.frame()) {
  path <- local_tempfile(fileext = ".db", .local_envir = env)
  build_db(
    path,
    list(
      ref = c(acsA = "ACGT"),
      `ISO-1` = c(acsA = "ACGT"),
      `ISO-2` = c(acsA = "ACGA"),
      `ISO-3` = c(acsA = "ACGC"),
      `ISO-4` = c(acsA = "ACGG")
    ),
    metadata = data.frame(
      isolate = paste0("ISO-", 1:4),
      geo_loc_name_country = c("Germany", "France", "Germany", "France"),
      stringsAsFactors = FALSE
    )
  )
  seed_results(path, paste0("ISO-", 1:3), classical = FALSE, amr = TRUE)

  # seed_results gives every isolate the same single gene, which cannot produce
  # a heatmap worth clustering — add a second gene and a virulence hit so the
  # element-type split and the drug-class strip have something to work with.
  con <- DBI::dbConnect(RSQLite::SQLite(), path)
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  extra <- list(
    list("ISO-1", "gyrA", "AMR", "QUINOLONE"),
    list("ISO-2", "gyrA", "AMR", "QUINOLONE"),
    list("ISO-1", "fimH", "VIRULENCE", NA_character_),
    list("ISO-3", "fimH", "VIRULENCE", NA_character_)
  )
  for (row in extra) {
    DBI::dbExecute(
      con,
      "INSERT INTO amr_results
         (isolate, gene_symbol, element_type, class, method, pct_identity,
          pct_coverage, called_at)
       VALUES (?, ?, ?, ?, 'EXACTX', 99, 100, '2026-01-01')",
      row
    )
  }
  DBI::dbExecute(
    con,
    "INSERT INTO amr_summary (isolate, section, drug_class, genes, called_at)
     VALUES ('ISO-2', 'partials', 'Quinolone', 'gyrA', '2026-01-01')"
  )
  path
}

meta_fixture <- function() {
  data.frame(
    isolate = paste0("ISO-", 1:4),
    geo_loc_name_country = c("Germany", "France", "Germany", "France"),
    stringsAsFactors = FALSE
  )
}

# The controls the plot reads, as a freshly loaded sidebar declares them. The
# gene and annotation pickers are server-rendered, so they have no value until
# the browser reports one — which is exactly the state a test starts in.
set_default_inputs <- function(session) {
  session$setInputs(
    amr_mode = "heatmap",
    amr_elements = c("AMR", "VIRULENCE", "STRESS"),
    amr_sections = c("matches", "partials", "virulence"),
    amr_min_identity = 0,
    amr_min_coverage = 0,
    amr_column_grouping = "class",
    amr_cluster_rows = TRUE,
    amr_cluster_distance = "binary",
    amr_cluster_method = "ward.D2",
    amr_col_cluster_distance = "binary",
    amr_col_cluster_method = "ward.D2",
    amr_dend_size = 1.5,
    amr_show_row_names = FALSE,
    amr_show_class_anno = TRUE,
    amr_level = "gene",
    amr_top_n = 30
  )
}

anno_meta_fixture <- function() {
  meta <- meta_fixture()
  # Two isolates share a day deliberately: a date with one distinct value per
  # isolate groups nothing and the mapping engine refuses it outright, which is
  # a different case from the one these tests are about.
  meta$enrollment_date <- c(
    "2026-01-05",
    "2026-01-05",
    "2026-01-20",
    "2026-02-21"
  )
  meta
}

anno_profiles_fixture <- function(meta) {
  types <- c(isolate = "text", geo_loc_name_country = "category")
  if ("enrollment_date" %in% names(meta)) {
    types <- c(types, enrollment_date = "date")
  }
  field_profile$field_profiles(meta, types = types)
}

test_that("Generate builds the matrix only while AMR is the active engine", {
  path <- amr_db()
  generate <- reactiveVal(0L)

  testServer(
    visualization_amr$server,
    args = list(
      db_path = reactive(path),
      viz_metadata = reactive(meta_fixture()),
      generate = generate,
      plot_type = reactiveVal("AMR")
    ),
    {
      set_default_inputs(session)
      generate(1L)
      session$flushReact()

      expect_true(generated())
      # Every isolate is a row, including the one that was never screened.
      expect_identical(nrow(presence_mat()), 4L)
      expect_identical(
        sort(colnames(presence_mat())),
        c("blaTEST", "fimH", "gyrA")
      )
    }
  )
})

test_that("a sibling engine's Generate leaves this engine alone", {
  # The Generate button is shared by every plot tab's engine, so the guard in
  # the observer is the only thing stopping another type's press landing here.
  path <- amr_db()
  generate <- reactiveVal(0L)
  plot_type <- reactiveVal("MST")

  testServer(
    visualization_amr$server,
    args = list(
      db_path = reactive(path),
      viz_metadata = reactive(meta_fixture()),
      generate = generate,
      plot_type = plot_type
    ),
    {
      set_default_inputs(session)
      generate(1L)
      session$flushReact()

      expect_false(generated())
    }
  )
})

test_that("a database with no screening reports it instead of drawing nothing", {
  path <- local_tempfile(fileext = ".db")
  build_db(
    path,
    list(ref = c(acsA = "ACGT"), `ISO-1` = c(acsA = "ACGT")),
    metadata = data.frame(isolate = "ISO-1", stringsAsFactors = FALSE)
  )
  generate <- reactiveVal(0L)

  testServer(
    visualization_amr$server,
    args = list(
      db_path = reactive(path),
      viz_metadata = reactive(data.frame(
        isolate = "ISO-1",
        stringsAsFactors = FALSE
      )),
      generate = generate,
      plot_type = reactiveVal("AMR")
    ),
    {
      set_default_inputs(session)
      generate(1L)
      session$flushReact()

      expect_false(generated())
    }
  )
})

test_that("the element filter narrows the heatmap without another Generate", {
  # Filtering a matrix is microseconds, so the controls track live once a plot
  # exists — the same rule the Epi curve follows for its interval.
  path <- amr_db()
  generate <- reactiveVal(0L)

  testServer(
    visualization_amr$server,
    args = list(
      db_path = reactive(path),
      viz_metadata = reactive(meta_fixture()),
      generate = generate,
      plot_type = reactiveVal("AMR")
    ),
    {
      set_default_inputs(session)
      generate(1L)
      session$flushReact()
      expect_identical(ncol(presence_mat()), 3L)

      session$setInputs(amr_elements = "VIRULENCE")
      expect_identical(colnames(presence_mat()), "fimH")
    }
  )
})

test_that("the identity floor drops hits it was set above", {
  path <- amr_db()
  generate <- reactiveVal(0L)

  testServer(
    visualization_amr$server,
    args = list(
      db_path = reactive(path),
      viz_metadata = reactive(meta_fixture()),
      generate = generate,
      plot_type = reactiveVal("AMR")
    ),
    {
      set_default_inputs(session)
      generate(1L)
      session$flushReact()

      # seed_results records no percentages for blaTEST (like a point mutation),
      # so it survives any floor; gyrA and fimH are 99% and do not.
      session$setInputs(amr_min_identity = 100)
      expect_identical(colnames(presence_mat()), "blaTEST")
    }
  )
})

test_that("the drug-class view carries abritamr's partial/confident split", {
  path <- amr_db()
  generate <- reactiveVal(0L)

  testServer(
    visualization_amr$server,
    args = list(
      db_path = reactive(path),
      viz_metadata = reactive(meta_fixture()),
      generate = generate,
      plot_type = reactiveVal("AMR")
    ),
    {
      set_default_inputs(session)
      session$setInputs(amr_mode = "classes")
      generate(1L)
      session$flushReact()

      mat <- class_mat()
      expect_true(generated())
      expect_identical(mat["ISO-1", "Beta-lactam"], 2L)
      expect_identical(mat["ISO-2", "Quinolone"], 1L)
      # ISO-4 was never screened, so it has a row and nothing in it.
      expect_identical(unname(sum(mat["ISO-4", ])), 0L)

      session$setInputs(amr_sections = "matches")
      expect_identical(colnames(class_mat()), "Beta-lactam")
    }
  )
})

test_that("each view renders to a ggplot the export path can take", {
  path <- amr_db()
  generate <- reactiveVal(0L)

  testServer(
    visualization_amr$server,
    args = list(
      db_path = reactive(path),
      viz_metadata = reactive(meta_fixture()),
      generate = generate,
      plot_type = reactiveVal("AMR")
    ),
    {
      set_default_inputs(session)
      generate(1L)
      session$flushReact()

      for (view in c("heatmap", "classes", "prevalence")) {
        session$setInputs(amr_mode = view)
        expect_s3_class(amr_ggplot(), "ggplot")
      }
    }
  )
})

test_that("mapping a variable adds an annotation strip keyed by isolate", {
  path <- amr_db()
  meta <- meta_fixture()
  generate <- reactiveVal(0L)

  testServer(
    visualization_amr$server,
    args = list(
      db_path = reactive(path),
      viz_metadata = reactive(meta),
      field_profiles = reactive(anno_profiles_fixture(meta)),
      generate = generate,
      plot_type = reactiveVal("AMR")
    ),
    {
      set_default_inputs(session)
      generate(1L)
      session$flushReact()

      expect_identical(anno_layers(), list())

      session$setInputs(amr_layer_add = "geo_loc_name_country")
      session$flushReact()

      layers <- anno_layers()
      expect_identical(length(layers), 1L)
      expect_identical(layers[[1]]$label, "Country")
      expect_identical(layers[[1]]$values[["ISO-1"]], "Germany")
      expect_s3_class(amr_ggplot(), "ggplot")
    }
  )
})

test_that("the same variable cannot be mapped twice", {
  path <- amr_db()
  meta <- meta_fixture()

  testServer(
    visualization_amr$server,
    args = list(
      db_path = reactive(path),
      viz_metadata = reactive(meta),
      field_profiles = reactive(anno_profiles_fixture(meta)),
      generate = reactiveVal(0L),
      plot_type = reactiveVal("AMR")
    ),
    {
      set_default_inputs(session)
      session$setInputs(amr_layer_add = "geo_loc_name_country")
      session$flushReact()
      id <- amr_layers()[[1]]$id

      # The picker clears itself after an add, so re-picking the same field is
      # what a second click looks like from here.
      session$setInputs(amr_layer_add = "geo_loc_name_country")
      session$flushReact()
      expect_identical(length(amr_layers()), 1L)

      session$setInputs(amr_layer_delete = id)
      session$flushReact()
      expect_identical(length(amr_layers()), 0L)
    }
  )
})

test_that("prevalence counts distinct isolates and honours the top-n cap", {
  path <- amr_db()
  generate <- reactiveVal(0L)

  testServer(
    visualization_amr$server,
    args = list(
      db_path = reactive(path),
      viz_metadata = reactive(meta_fixture()),
      generate = generate,
      plot_type = reactiveVal("AMR")
    ),
    {
      set_default_inputs(session)
      session$setInputs(amr_mode = "prevalence")
      generate(1L)
      session$flushReact()

      df <- prevalence_df()
      expect_identical(df$item[1], "blaTEST")
      expect_identical(df$n[1], 3L)

      session$setInputs(amr_top_n = 1)
      expect_identical(nrow(prevalence_df()), 1L)

      session$setInputs(amr_level = "class")
      expect_identical(prevalence_df()$item, "Beta-lactam")
    }
  )
})

test_that("the snapshot carries the amr_ controls and restore accepts it", {
  path <- amr_db()
  generate <- reactiveVal(0L)

  testServer(
    visualization_amr$server,
    args = list(
      db_path = reactive(path),
      viz_metadata = reactive(meta_fixture()),
      generate = generate,
      plot_type = reactiveVal("AMR")
    ),
    {
      set_default_inputs(session)
      session$setInputs(amr_mode = "classes", amr_top_n = 15)
      generate(1L)
      session$flushReact()

      snap <- snapshot()
      expect_identical(snap$amr_mode, "classes")
      expect_identical(snap$amr_top_n, 15)
      # `.layers` is the annotation strips, which are reactiveVal state rather
      # than an input; everything else is an amr_ control.
      expect_true(all(startsWith(setdiff(names(snap), ".layers"), "amr_")))

      # A snapshot taken before a control existed must restore everything else
      # cleanly rather than erroring on the missing id.
      expect_null(restore(list(amr_mode = "heatmap")))
    }
  )
})

# --- restore(): saved annotation strips ---------------------------------------
# The strips are reactiveVal state rather than inputs, so they travel in the
# snapshot's `.layers` key and come back through normalize_layers(). A snapshot
# saved before the rewrite carried one strip in flat amr_anno_* keys instead;
# migrate_legacy_annotation() is what stops those plots losing it on reopen.

test_that("the snapshot carries the strips and restore brings them back", {
  path <- amr_db()
  meta <- anno_meta_fixture()

  testServer(
    visualization_amr$server,
    args = list(
      db_path = reactive(path),
      viz_metadata = reactive(meta),
      field_profiles = reactive(anno_profiles_fixture(meta)),
      generate = reactiveVal(0L),
      plot_type = reactiveVal("AMR")
    ),
    {
      set_default_inputs(session)
      session$setInputs(amr_layer_add = "geo_loc_name_country")
      session$flushReact()

      saved <- snapshot()$.layers
      expect_identical(length(saved), 1L)

      amr_layers(list())
      restore(list(.layers = saved))
      session$flushReact()

      expect_identical(length(amr_layers()), 1L)
      expect_identical(amr_layers()[[1]]$field, "geo_loc_name_country")
    }
  )
})

test_that("a saved strip naming a missing column draws nothing", {
  path <- amr_db()
  meta <- anno_meta_fixture()

  testServer(
    visualization_amr$server,
    args = list(
      db_path = reactive(path),
      viz_metadata = reactive(meta),
      field_profiles = reactive(anno_profiles_fixture(meta)),
      generate = reactiveVal(0L),
      plot_type = reactiveVal("AMR")
    ),
    {
      set_default_inputs(session)
      restore(list(.layers = list(
        list(
          id = "L1",
          field = "gone_from_this_database",
          title = "Gone",
          aesthetic = "annotation",
          palette = "Set1",
          n_levels = 2L,
          auto = TRUE
        )
      )))
      session$flushReact()

      # The record survives the restore — dropping it would lose the mapping if
      # the column comes back — but nothing reaches the builder for it.
      expect_identical(length(amr_layers()), 1L)
      expect_identical(anno_layers(), list())
    }
  )
})

test_that("a pre-rewrite snapshot's single annotation field becomes a strip", {
  path <- amr_db()
  meta <- anno_meta_fixture()

  testServer(
    visualization_amr$server,
    args = list(
      db_path = reactive(path),
      viz_metadata = reactive(meta),
      field_profiles = reactive(anno_profiles_fixture(meta)),
      generate = reactiveVal(0L),
      plot_type = reactiveVal("AMR")
    ),
    {
      set_default_inputs(session)
      restore(list(
        amr_anno_field = "enrollment_date",
        amr_anno_granularity = "month",
        amr_anno_scale = "Set3"
      ))
      session$flushReact()

      expect_identical(length(amr_layers()), 1L)
      l <- amr_layers()[[1]]
      expect_identical(l$field, "enrollment_date")
      expect_identical(l$granularity, "month")
      expect_identical(l$palette, "Set3")
    }
  )
})

test_that("a saved 'Element type' grouping restores as one the picker offers", {
  path <- amr_db()
  meta <- meta_fixture()

  testServer(
    visualization_amr$server,
    args = list(
      db_path = reactive(path),
      viz_metadata = reactive(meta),
      field_profiles = reactive(anno_profiles_fixture(meta)),
      generate = reactiveVal(0L),
      plot_type = reactiveVal("AMR")
    ),
    {
      set_default_inputs(session)
      # Element type is structural now, so the grouping it named is gone from
      # the control and the value has to be translated rather than passed on.
      session$setInputs(amr_column_grouping = "element")
      expect_identical(grouping(), "none")
    }
  )
})
