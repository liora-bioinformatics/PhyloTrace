box::use(
  shiny[reactive, testServer],
  withr[local_tempdir],
)
box::use(
  app / logic / custom_fields[custom_col],
  app / view / database_custom,
)

# A database with isolates A and B and two custom variables to edit.
.values_db <- function() {
  path <- file.path(local_tempdir(.local_envir = parent.frame()), "db.sqlite")
  build_db(path, default_local(), metadata = meta_df(c("A", "B")))
  seed_custom(path, list(
    age = list(type = "integer", values = c(A = "3")),
    risk = list(type = "category", levels = '["High","Low"]')
  ))
  path
}

.col_idx <- function(state, column) match(column, names(state$data)) - 1L

test_that("a pasted block is validated per variable type and buffered", {
  path <- .values_db()

  testServer(database_custom$server, args = list(db_path = reactive(path)), {
    session$flushReact()
    age <- .col_idx(State, custom_col("age"))
    risk <- .col_idx(State, custom_col("risk"))

    session$setInputs(values_table_sheet = list(
      op = "edit",
      isolate = c("A", "B", "A", "B"),
      col = c(age, age, risk, risk),
      value = c("7", "seven", "low", "High"),
      skipped = 0L
    ))
    session$flushReact()

    expect_identical(State$data[[custom_col("age")]], c(7, NA))
    expect_identical(State$data[[custom_col("risk")]], c("Low", "High"))
    expect_identical(nrow(State$dirty), 3L)
    expect_true(State$pending)
  })
})

test_that("the isolate column is never written", {
  path <- .values_db()

  testServer(database_custom$server, args = list(db_path = reactive(path)), {
    session$flushReact()
    session$setInputs(values_table_sheet = list(
      op = "edit", isolate = "A", col = 0L, value = "Z", skipped = 0L
    ))
    session$flushReact()

    expect_identical(State$data$isolate, c("A", "B"))
    expect_null(State$dirty)
  })
})

test_that("undo and redo cover single-cell edits", {
  path <- .values_db()

  testServer(database_custom$server, args = list(db_path = reactive(path)), {
    session$flushReact()
    age <- .col_idx(State, custom_col("age"))

    session$setInputs(values_table_cell_edit = list(row = 1, col = age, value = "40"))
    session$flushReact()
    expect_identical(State$data[[custom_col("age")]][[1]], 40)

    session$setInputs(values_table_sheet = list(op = "undo"))
    session$flushReact()
    expect_identical(State$data[[custom_col("age")]][[1]], 3)
    # Back at the saved value: nothing left to save.
    expect_null(State$dirty)
    expect_false(State$pending)

    session$setInputs(values_table_sheet = list(op = "redo"))
    session$flushReact()
    expect_identical(State$data[[custom_col("age")]][[1]], 40)
  })
})
