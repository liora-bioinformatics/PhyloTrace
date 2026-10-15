# A report bundles metadata, screenshot and redacted logs into the outbox.
# Building never raises, and uploading never blocks or raises: a report that
# cannot be sent stays in the outbox and is retried, never lost.

box::use(
  httpuv[randomPort, startServer, stopServer],
  jsonlite[fromJSON],
  later[run_now],
  testthat[expect_false, expect_identical, expect_lt, expect_null, expect_true, test_that],
  withr[defer, local_envvar, local_tempdir, local_tempfile],
  zip[unzip, zip_list],
)
box::use(
  app / logic / issue_report,
)

# 1x1 white JPEG
PIXEL <- paste0(
  "data:image/jpeg;base64,/9j/4AAQSkZJRgABAQEASABIAAD/2wBDAP//////////////////",
  "////////////////////////////////////////////////////////////////////wgALCAAB",
  "AAEBAREA/8QAFBABAAAAAAAAAAAAAAAAAAAAAP/aAAgBAQABPxA="
)

# Run the later event loop until `done()` is TRUE or `secs` pass.
wait_for <- function(done, secs = 15) {
  deadline <- Sys.time() + secs
  while (!done() && Sys.time() < deadline) run_now(0.1)
  done()
}

# A local receiver answering every request with 200; returns its URL.
local_receiver <- function(env = parent.frame()) {
  port <- randomPort()
  server <- startServer("127.0.0.1", port, list(call = function(req) {
    req$rook.input$read() # drain the upload
    list(status = 200L, headers = list("Content-Type" = "application/json"), body = "{\"ok\":true}")
  }))
  defer(stopServer(server), envir = env)
  sprintf("http://127.0.0.1:%d/api/phylotrace-report", port)
}

inquiry <- function(dir) {
  issue_report$build_report("inquiry", comment = "hi", include_logs = FALSE, dir = dir)
}

test_that("a report holds metadata, screenshot and home-redacted logs", {
  dir <- local_tempdir()
  writeLines(
    c("start", paste("opened", path.expand("~/data/db.sqlite"))),
    file.path(dir, "last_session.log")
  )
  path <- issue_report$build_report(
    "error", "boom", "it broke", "a@b.c",
    screenshot = PIXEL, dir = dir
  )
  expect_identical(dirname(path), issue_report$outbox(dir))
  files <- zip_list(path)$filename
  expect_true(all(c("report.json", "screenshot.jpg", "logs/last_session.log") %in% files))

  out <- local_tempdir()
  unzip(path, exdir = out)
  meta <- fromJSON(file.path(out, "report.json"))
  expect_identical(meta$message, "boom")
  log <- readLines(file.path(out, "logs", "last_session.log"))
  expect_identical(log[2], "opened ~/data/db.sqlite")
})

test_that("an inquiry without logs or screenshot holds only report.json", {
  path <- inquiry(local_tempdir())
  expect_identical(setdiff(zip_list(path)$filename, "logs/"), "report.json")
})

test_that("a report that cannot be built returns NULL instead of raising", {
  not_a_dir <- local_tempfile()
  writeLines("x", not_a_dir)
  expect_null(inquiry(not_a_dir))
})

test_that("an unreachable server keeps the report without blocking", {
  path <- inquiry(local_tempdir())
  local_envvar(PHYLOTRACE_REPORT_URL = "http://127.0.0.1:9/")
  result <- NULL
  started <- Sys.time()
  issue_report$upload_report(path, function(sent) result <<- sent)
  expect_lt(as.numeric(Sys.time() - started, units = "secs"), 1)
  expect_true(wait_for(function() !is.null(result)))
  expect_false(result)
  expect_true(file.exists(path))
})

test_that("a sent report leaves the outbox, and a flush sends the rest", {
  dir <- local_tempdir()
  local_envvar(PHYLOTRACE_REPORT_URL = local_receiver())

  path <- inquiry(dir)
  result <- NULL
  issue_report$upload_report(path, function(sent) result <<- sent)
  expect_true(wait_for(function() !is.null(result)))
  expect_true(result)
  expect_false(file.exists(path))

  # two queued reports (distinct names: they are stamped to the second)
  file.copy(inquiry(dir), file.path(issue_report$outbox(dir), "phylotrace_inquiry_1.zip"))
  inquiry(dir)
  queued <- function() list.files(issue_report$outbox(dir), "\\.zip$")
  expect_identical(length(queued()), 2L)
  issue_report$flush_outbox(dir)
  expect_true(wait_for(function() length(queued()) == 0))
})
