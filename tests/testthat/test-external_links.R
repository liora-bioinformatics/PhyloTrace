# tests/testthat/test-external_links.R
#
# Checks that every external URL the app links to or fetches from still works: the addresses of
# app/logic/urls.R, every URL in the app's source and data files, the licence deeds and NCBI
# Taxonomy pages built for species, the basemap tiles and their attribution links, and the
# geocoder. The offline tests keep URLs in their central place (app/logic/urls.R, or
# scheme_sources.R for the scheme providers). The online tests hit third-party servers, so they
# run only when PHYLOTRACE_LINK_CHECK=true, which the weekly workflow
# .github/workflows/link_check.yml sets; locally:
#   PHYLOTRACE_LINK_CHECK=true Rscript -e 'rhino::test_r()'
# Pages a host's robots.txt closes to automated clients are not opened, only listed as not
# checked: the scheme pages of cgMLST.org (/ncs/schema/) and PubMLST (/bigsdb), whose addresses
# the weekly scheme index run (dev/scheme_index) reads from the providers' listings instead.
# The geocoder and NCBI E-utilities are APIs, called under their own usage policies.

box::use(
  curl[handle_data, has_internet, multi_add, multi_run, new_handle, new_pool],
  jsonlite[fromJSON],
  leaflet.providers[providers_default],
  rlang[`%||%`],
  testthat[expect, expect_true, skip_if, test_that],
  utils[getParseData, modifyList, read.csv],
)
box::use(
  app / logic / geocoder[nominatim_lookup],
  app / logic / scheme_sources[safe_url],
  app / logic / species,
  app / logic / urls,
)

# Exported functions of urls.R that build a URL, each with an example of a working one. A new
# one fails the offline test until it is added here.
URL_BUILDERS <- list(
  taxonomy_url = function() urls$taxonomy_url(470L),
  cc_license_url = function() c(urls$cc_license_url("by-sa", "4.0"), urls$cc_license_url("zero")),
  nominatim_search_url = function() urls$nominatim_search_url("Berlin, Germany"),
  chewie_ns_api = function() urls$chewie_ns_api("/species/list")
)

# Constants of urls.R that are tile templates rather than pages.
TILE_TEMPLATES <- "MAP_LABEL_TILES"

# Scripts that may hold URL literals themselves: the central module, and the scheme providers'
# registry, which keeps each address next to its terms and citation.
LITERAL_HOMES <- c("logic/urls.R", "logic/scheme_sources.R")

APP_DIR <- "../../app"

USER_AGENT <- "PhyloTrace-link-check (+https://github.com/liora-bioinformatics/PhyloTrace)"

# Product token of USER_AGENT that robots.txt groups are matched against (RFC 9309).
ROBOTS_AGENT <- "phylotrace-link-check"

# robots.txt rules per origin, fetched once per run.
robots_cache <- new.env()

# Files the sweep leaves out: binaries, the bundles built from app/js and app/styles (both swept
# as sources), a vendored library, and local downloads .gitignore keeps out of the app.
SKIP_FILE <- paste(
  "\\.(png|jpe?g|gif|ico|webp|woff2?|ttf|otf|eot|fasta|gz|zip|pyc|map|rds)$",
  "^static/js/app\\.min\\.js$",
  "^static/css/app\\.min\\.css$",
  "^static/js/html2canvas\\.min\\.js$",
  "^logic/data/assembly_summary\\.txt$",
  "^logic/data/.*_log\\.txt$",
  "^logic/urls\\.R$",
  sep = "|"
)

# URL templates and string fragments in app/ that are no link on their own, each with what
# covers the URLs built from it. A new one fails the inventory test until it is listed here (and
# covered by a check below); one that disappears from the app fails it as well.
FRAGMENTS <- c(
  "https://" = "placeholder of the database wizard's scheme URL input",
  "https://pubmed.ncbi.nlm.nih.gov/{pmid}/" = "output of a data-preparation script; not shown",
  "https://www.ncbi.nlm.nih.gov/bioproject/{row[" = "output of a data-preparation script; not shown"
)

# Pages some providers serve with HTTP 200 although what was asked for does not exist. Each rule
# returns a problem description for such a page, NULL for a real one.
CONTENT_RULES <- list(
  list(
    pattern = "^https://www\\.cgmlst\\.org/ncs/schema/",
    check = function(url, body) {
      if (grepl("INSERT TITLE HERE", body, fixed = TRUE)) "unknown scheme (blank page template)"
    }
  ),
  list(
    pattern = "^https://enterobase\\.[^/]+/species/index/",
    check = function(url, body) {
      db <- sub("^.*/species/index/([^/?#]+).*$", "\\1", url)
      if (!grepl(paste0("species/", db, "/"), body, fixed = TRUE)) {
        "unknown database (generic page without the database's links)"
      }
    }
  )
)

# Skips the online tests unless PHYLOTRACE_LINK_CHECK=true, and without network access.
skip_unless_online <- function() {
  skip_if(
    !identical(tolower(Sys.getenv("PHYLOTRACE_LINK_CHECK")), "true"),
    "online link check runs only with PHYLOTRACE_LINK_CHECK=true"
  )
  skip_if(!has_internet(), "no network access")
}

# Inventory entries: the URL, where it comes from and what a working answer looks like ("page",
# "image").
entries <- function(url, where, kind = "page") {
  data.frame(url = url, where = where, kind = kind, stringsAsFactors = FALSE)
}

# URLs in a piece of text. A whole string that is one URL is taken as it is (data values such as
# Wikimedia file pages hold commas and parentheses); in running text a URL ends at whitespace,
# quotes or angle brackets, and loses trailing punctuation and unbalanced closing parentheses.
text_urls <- function(text) {
  text <- text[!is.na(text)]
  whole <- grepl("^https?://[^[:space:]]+$", text)
  prose <- text[!whole]
  found <- unlist(regmatches(prose, gregexpr("https?://[^[:space:]\"'<>`]*", prose)))
  found <- sub("[.,;:!?]+$", "", found)
  count <- function(u, char) nchar(gsub(paste0("[^", char, "]"), "", u))
  found <- vapply(found, function(u) {
    while (endsWith(u, ")") && count(u, "(") < count(u, ")")) {
      u <- substr(u, 1L, nchar(u) - 1L)
    }
    u
  }, character(1), USE.NAMES = FALSE)
  c(text[whole], found)
}

# Every URL in the files under app/: data files are read as data (each JSON string and CSV cell),
# everything else as text. XML namespace and DOCTYPE identifiers in SVGs are names, not links.
app_urls <- function() {
  files <- list.files(APP_DIR, recursive = TRUE)
  files <- files[!grepl(SKIP_FILE, files) & !grepl("(^|/)__pycache__/", files)]
  rows <- lapply(files, function(file) {
    path <- file.path(APP_DIR, file)
    where <- file.path("app", file)
    if (grepl("\\.json$", file)) {
      leaves <- unlist(fromJSON(path, simplifyVector = FALSE), use.names = FALSE)
      urls <- text_urls(as.character(leaves))
    } else if (grepl("\\.csv$", file)) {
      cells <- as.matrix(read.csv(path, colClasses = "character", na.strings = ""))
      urls <- character(0)
      wheres <- character(0)
      for (i in seq_len(nrow(cells))) {
        found <- text_urls(cells[i, ])
        urls <- c(urls, found)
        wheres <- c(wheres, rep(sprintf("%s (row %d)", where, i + 1L), length(found)))
      }
      return(if (length(urls)) entries(urls, wheres))
    } else {
      text <- paste(readLines(path, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
      text <- gsub("xmlns(:[[:alnum:]_-]+)?=\"[^\"]*\"", "", text)
      text <- gsub("<!DOCTYPE[^>]*>", "", text)
      urls <- text_urls(text)
    }
    if (length(urls)) entries(urls, where)
  })
  out <- do.call(rbind, rows)
  out[!duplicated(out[c("url", "where")]), , drop = FALSE]
}

# TRUE for a template or a fragment of a URL rather than a link.
is_fragment <- function(url) {
  grepl("[{}]|%[0-9.]*[sd]|[=?&]$", url) | grepl("^https?://$", url) | url %in% names(FRAGMENTS)
}

# Tile URL of a leaflet.providers basemap name such as "Esri.WorldImagery", with its attribution
# HTML resolved the way leaflet-providers resolves `{attribution.<Provider>}`.
provider_tile <- function(name) {
  details <- providers_default()$providers_details
  parts <- strsplit(name, ".", fixed = TRUE)[[1]]
  base <- details[[parts[1]]]
  if (is.null(base)) {
    return(list(url = NA_character_, attribution = ""))
  }
  variant <- if (length(parts) > 1L) base$variants[[parts[2]]]
  if (length(parts) > 1L && is.null(variant)) {
    return(list(url = NA_character_, attribution = ""))
  }
  if (is.character(variant)) {
    variant <- list(options = list(variant = variant))
  }
  options <- modifyList(base$options %||% list(), variant$options %||% list())
  attribution <- options$attribution %||% ""
  while (grepl("\\{attribution\\.[[:alnum:]]+\\}", attribution)) {
    ref <- sub("^.*?\\{attribution\\.([[:alnum:]]+)\\}.*$", "\\1", attribution, perl = TRUE)
    attribution <- sub(
      paste0("{attribution.", ref, "}"),
      details[[ref]]$options$attribution %||% "",
      attribution,
      fixed = TRUE
    )
  }
  list(url = fill_tile(variant$url %||% base$url, options), attribution = attribution)
}

# A tile template with its placeholders filled for the single tile of zoom level 0.
fill_tile <- function(template, options = list()) {
  subdomains <- paste(options$subdomains %||% "abc", collapse = "")
  values <- list(
    s = substr(subdomains, 1L, 1L),
    z = "0", x = "0", y = "0", r = "",
    variant = options$variant %||% "",
    ext = options$ext %||% "png"
  )
  for (key in names(values)) {
    template <- gsub(paste0("{", key, "}"), values[[key]], template, fixed = TRUE)
  }
  template
}

# Fetches URLs concurrently across hosts but one at a time per host, to stay within the
# providers' rate limits (PubMLST alone is asked about 130 pages). Returns, per URL, the HTTP
# status, content type, text body (empty for binary answers) and the transport error, if any.
fetch_urls <- function(urls, host_con = 1L, connect_timeout = 30L) {
  out <- list()
  pool <- new_pool(total_con = 12L, host_con = host_con)
  for (url in unique(urls)) {
    local({
      u <- url
      h <- new_handle(
        url = u,
        followlocation = TRUE,
        maxredirs = 10L,
        connecttimeout = connect_timeout,
        timeout = 120L,
        useragent = USER_AGENT,
        # Large downloads (such as NCBI's assembly summary) stop after the headers.
        maxfilesize = 2e7
      )
      multi_add(
        h,
        pool = pool,
        done = function(res) {
          type <- res$type %||% ""
          type <- if (is.na(type)) "" else type
          # Some servers (cgMLST.org) send no content type, so only declared binaries are skipped.
          binary <- grepl("^(image|audio|video|font)/|octet-stream|zip|pdf", type)
          body <- if (binary) "" else tryCatch(rawToChar(res$content), error = function(e) "")
          out[[u]] <<- list(status = res$status_code, type = type, body = body, error = NA)
        },
        fail = function(msg) {
          status <- tryCatch(handle_data(h)$status_code, error = function(e) 0L)
          out[[u]] <<- list(status = status, type = "", body = "", error = msg)
        }
      )
    })
  }
  multi_run(pool = pool)
  out
}

# Allow and Disallow rules of a robots.txt that apply to the link check (RFC 9309): those of the
# groups naming its product token, otherwise those of the `*` groups. Consecutive user-agent
# lines share a group; a user-agent line after a rule starts a new one.
robots_rules <- function(text) {
  lines <- trimws(sub("#.*$", "", strsplit(text, "\r?\n|\r")[[1]]))
  lines <- lines[grepl("^[A-Za-z-]+[[:space:]]*:", lines)]
  keys <- tolower(trimws(sub(":.*$", "", lines)))
  values <- trimws(sub("^[^:]*:", "", lines))
  groups <- list()
  for (i in seq_along(keys)) {
    if (keys[i] == "user-agent") {
      n <- length(groups)
      if (!n || groups[[n]]$closed) {
        groups[[n + 1L]] <- list(
          agents = character(0), type = character(0), path = character(0), closed = FALSE
        )
        n <- n + 1L
      }
      groups[[n]]$agents <- c(groups[[n]]$agents, tolower(values[i]))
    } else if (keys[i] %in% c("allow", "disallow") && length(groups)) {
      n <- length(groups)
      groups[[n]]$closed <- TRUE
      # An empty Disallow allows everything, the same as no rule.
      if (nzchar(values[i])) {
        groups[[n]]$type <- c(groups[[n]]$type, keys[i])
        groups[[n]]$path <- c(groups[[n]]$path, values[i])
      }
    }
  }
  chosen <- Filter(function(g) ROBOTS_AGENT %in% g$agents, groups)
  if (!length(chosen)) {
    chosen <- Filter(function(g) "*" %in% g$agents, groups)
  }
  data.frame(
    type = as.character(unlist(lapply(chosen, `[[`, "type"))),
    path = as.character(unlist(lapply(chosen, `[[`, "path"))),
    stringsAsFactors = FALSE
  )
}

# Regular expression of a robots.txt path pattern: `*` matches any characters, a final `$` ends
# the path, everything else is literal and matched from the start of the path.
robots_regex <- function(pattern) {
  anchored <- endsWith(pattern, "$")
  if (anchored) {
    pattern <- substr(pattern, 1L, nchar(pattern) - 1L)
  }
  literal <- gsub("([\\[\\].\\\\|(){}^$*+?])", "\\\\\\1", pattern, perl = TRUE)
  paste0("^", gsub("\\*", ".*", literal, fixed = TRUE), if (anchored) "$")
}

# TRUE when robots.txt rules let the link check open a URL: the longest matching rule decides
# and Allow wins a tie; a URL no rule matches is allowed. NULL rules (robots.txt unreachable)
# allow nothing.
robots_allows <- function(rules, url) {
  if (is.null(rules)) {
    return(FALSE)
  }
  target <- sub("#.*$", "", sub("^https?://[^/?#]+", "", url))
  if (!startsWith(target, "/")) {
    target <- paste0("/", target)
  }
  hit <- vapply(rules$path, function(p) grepl(robots_regex(p), target, perl = TRUE), logical(1))
  if (!any(hit)) {
    return(TRUE)
  }
  len <- nchar(rules$path[hit])
  any(rules$type[hit][len == max(len)] == "allow")
}

# For each URL, whether its host's robots.txt lets the link check open it. A missing robots.txt
# (HTTP 4xx) allows everything; an unreachable one (transport error, HTTP 5xx) nothing, as
# RFC 9309 asks.
robots_permitted <- function(urls) {
  origins <- sub("^(https?://[^/?#]+).*$", "\\1", urls)
  todo <- setdiff(unique(origins), ls(robots_cache))
  if (length(todo)) {
    answers <- fetch_urls(paste0(todo, "/robots.txt"))
    for (origin in todo) {
      res <- answers[[paste0(origin, "/robots.txt")]]
      robots_cache[[origin]] <- if (!is.na(res$error) || res$status >= 500L) {
        NULL
      } else if (res$status >= 400L) {
        robots_rules("")
      } else {
        robots_rules(res$body)
      }
    }
  }
  vapply(seq_along(urls), function(i) {
    robots_allows(robots_cache[[origins[i]]], urls[i])
  }, logical(1))
}

# What is wrong with an answer, NA when it is a working one of the expected kind.
problem <- function(url, kind, res) {
  ok_status <- isTRUE(res$status >= 200L && res$status < 300L)
  if (!is.na(res$error)) {
    if (ok_status && grepl("file size", res$error, ignore.case = TRUE)) {
      return(NA_character_)
    }
    return(res$error)
  }
  if (!ok_status) {
    return(sprintf("HTTP %d", res$status))
  }
  if (kind == "image" && !startsWith(res$type, "image/")) {
    return(sprintf("not an image (%s)", res$type))
  }
  for (rule in CONTENT_RULES) {
    if (grepl(rule$pattern, url)) {
      found <- rule$check(url, res$body)
      if (!is.null(found)) {
        return(found)
      }
    }
  }
  NA_character_
}

# Checks inventory entries online and reports every failing one with where it comes from.
# Rate-limited, server-side and transport failures get up to three slower attempts. Entries
# robots.txt closes to the link check are not opened, only listed.
expect_urls_work <- function(inv) {
  inv <- inv[!is.na(inv$url), , drop = FALSE]
  allowed <- robots_permitted(inv$url)
  if (any(!allowed)) {
    closed <- unique(inv$url[!allowed])
    message(sprintf(
      "Not checked, robots.txt closes them to automated clients (%d):\n%s",
      length(closed),
      paste0("  ", closed, collapse = "\n")
    ))
  }
  inv <- inv[allowed, , drop = FALSE]
  results <- fetch_urls(inv$url)
  transient <- function(r) !is.na(r$error) || r$status == 429L || r$status >= 500L
  for (wait in c(10, 30, 60)) {
    retry <- names(results)[vapply(results, transient, logical(1))]
    if (!length(retry)) {
      break
    }
    Sys.sleep(wait)
    results[retry] <- fetch_urls(retry, connect_timeout = 90L)[retry]
  }
  issues <- vapply(seq_len(nrow(inv)), function(i) {
    problem(inv$url[i], inv$kind[i], results[[inv$url[i]]])
  }, character(1))
  bad <- !is.na(issues)
  expect(
    !any(bad),
    sprintf(
      "%d of %d URLs do not work:\n%s",
      sum(bad),
      nrow(inv),
      paste0("  ", inv$where[bad], ": ", inv$url[bad], "\n    -> ", issues[bad], collapse = "\n")
    )
  )
}

# The basemaps offered on the map, by leaflet.providers name.
basemap_names <- function() {
  unique(unname(urls$MAP_BASEMAPS))
}

# URL constants of urls.R: name to URL.
url_constants <- function() {
  values <- mget(ls(urls), envir = as.environment(urls))
  values[vapply(values, function(v) is.character(v) && length(v) == 1L, logical(1))]
}

# Strings that are URLs in the R scripts of app/, per script (relative to app/).
r_url_literals <- function() {
  files <- list.files(APP_DIR, pattern = "\\.R$", recursive = TRUE)
  found <- lapply(files, function(file) {
    data <- getParseData(parse(file.path(APP_DIR, file), keep.source = TRUE))
    strings <- data$text[data$token == "STR_CONST"]
    strings <- substr(strings, 2L, nchar(strings) - 1L)
    strings[grepl("^https?://.", strings)]
  })
  names(found) <- files
  found[lengths(found) > 0L]
}

test_that("every URL fragment in app/ is listed and every other URL is well-formed", {
  urls <- app_urls()
  expect_true(nrow(urls) > 0L)
  fragments <- unique(urls$url[is_fragment(urls$url)])
  unlisted <- setdiff(fragments, names(FRAGMENTS))
  expect(
    !length(unlisted),
    paste(
      "URL templates or fragments not covered by a link check; add them to FRAGMENTS and check",
      "the URLs built from them:",
      paste(unlisted, collapse = ", ")
    )
  )
  gone <- setdiff(names(FRAGMENTS), fragments)
  expect(!length(gone), paste("FRAGMENTS lists URLs no longer in app/:", toString(gone)))
  links <- unique(urls$url[!is_fragment(urls$url)])
  malformed <- links[!vapply(links, safe_url, logical(1))]
  expect(!length(malformed), paste("Malformed URLs:", toString(malformed)))
})

test_that("URLs live in app/logic/urls.R, scheme_sources.R or the data files only", {
  literals <- r_url_literals()
  stray <- literals[!names(literals) %in% LITERAL_HOMES]
  expect(
    !length(stray),
    paste(
      "URLs written into scripts; move them to app/logic/urls.R so the link check covers them:",
      paste0(names(stray), ": ", vapply(stray, toString, character(1)), collapse = "; ")
    )
  )
  constants <- url_constants()
  expect_true(length(constants) > 0L)
  plain <- constants[!names(constants) %in% TILE_TEMPLATES]
  expect(
    all(vapply(plain, safe_url, logical(1))) && !any(grepl("[{}]", plain)),
    "A constant of urls.R is no plain URL; tile templates go in TILE_TEMPLATES"
  )
  functions <- ls(urls)[vapply(ls(urls), function(n) is.function(urls[[n]]), logical(1))]
  expect(
    setequal(functions, names(URL_BUILDERS)),
    paste(
      "URL builders of urls.R and URL_BUILDERS differ:",
      toString(c(setdiff(functions, names(URL_BUILDERS)), setdiff(names(URL_BUILDERS), functions)))
    )
  )
  built <- unlist(lapply(URL_BUILDERS, function(f) f()))
  expect(all(vapply(built, safe_url, logical(1))), "A URL builder returns no plain URL")
})

test_that("every basemap resolves to a tile URL without placeholders", {
  for (name in basemap_names()) {
    tile <- provider_tile(name)
    expect(!is.na(tile$url), sprintf("leaflet.providers does not know basemap %s", name))
    expect(!grepl("[{}]", tile$url), sprintf("%s keeps a placeholder: %s", name, tile$url))
  }
  for (name in TILE_TEMPLATES) {
    expect_true(!grepl("[{}]", fill_tile(urls[[name]])))
  }
})

test_that("robots.txt rules are applied as RFC 9309 describes", {
  robots <- paste(
    "User-agent: SomeBot",
    "Disallow: /",
    "",
    "User-agent: *",
    "Disallow: /ncs/schema/ # schemes",
    "Disallow: /bigsdb",
    "Allow: /bigsdb/open$",
    "Disallow: /*.pdf$",
    "Disallow:",
    sep = "\n"
  )
  rules <- robots_rules(robots)
  expect_true(robots_allows(rules, "https://www.cgmlst.org/ncs/"))
  expect_true(!robots_allows(rules, "https://www.cgmlst.org/ncs/schema/Abaumannii/"))
  expect_true(!robots_allows(rules, "https://pubmlst.org/bigsdb?db=pubmlst_neisseria_seqdef"))
  expect_true(robots_allows(rules, "https://pubmlst.org/bigsdb/open"))
  expect_true(!robots_allows(rules, "https://pubmlst.org/bigsdb/open/more"))
  expect_true(!robots_allows(rules, "https://x.org/a/b.pdf"))
  expect_true(robots_allows(rules, "https://x.org/a/b.pdf?page=2"))
  expect_true(robots_allows(rules, "https://x.org"))
  own_group <- robots_rules("User-agent: phylotrace-link-check\nDisallow: /")
  expect_true(!robots_allows(own_group, "https://x.org/"))
  expect_true(robots_allows(robots_rules("<!doctype html><html></html>"), "https://x.org/a"))
  expect_true(!robots_allows(NULL, "https://x.org/a"))
})

test_that("every URL of urls.R works", {
  skip_unless_online()
  constants <- url_constants()
  plain <- constants[!names(constants) %in% TILE_TEMPLATES]
  tiles <- constants[names(constants) %in% TILE_TEMPLATES]
  expect_urls_work(rbind(
    entries(unname(unlist(plain)), paste("urls$", names(plain), sep = "")),
    entries(unname(vapply(tiles, fill_tile, character(1))), paste("urls$", names(tiles), sep = ""),
      kind = "image"
    ),
    entries(unname(unlist(lapply(URL_BUILDERS, function(f) f()))), "urls.R builder")
  ))
})

test_that("every URL in the app's source and data files works", {
  skip_unless_online()
  urls <- app_urls()
  expect_urls_work(urls[!is_fragment(urls$url), , drop = FALSE])
})

test_that("the licence deeds and NCBI Taxonomy pages linked for species work", {
  skip_unless_online()
  catalog <- species$species_catalog()
  licenses <- unlist(lapply(catalog$label, function(label) {
    species$image_credit(label)$license_url
  }))
  licenses <- unique(licenses[!is.na(licenses)])
  taxa <- catalog[!is.na(catalog$taxid), , drop = FALSE]
  # NCBI answers scripts with a browser check (HTTP 200) instead of the page, so one page shows
  # that the address still works and E-utilities confirms each species' taxon exists.
  expect_urls_work(rbind(
    entries(licenses, "species picture licence (species$image_credit)"),
    entries(urls$taxonomy_url(taxa$taxid[1]), "NCBI Taxonomy page (urls$taxonomy_url)")
  ))
  summary_url <- paste0(
    "https://eutils.ncbi.nlm.nih.gov/entrez/eutils/esummary.fcgi?db=taxonomy&retmode=json&id=",
    paste(unique(taxa$taxid), collapse = ",")
  )
  summary <- fromJSON(summary_url, simplifyVector = FALSE)$result
  missing <- taxa$label[!vapply(as.character(taxa$taxid), function(id) {
    identical(summary[[id]]$status, "active")
  }, logical(1))]
  expect(
    !length(missing),
    paste("Species whose NCBI taxon id is unknown or no longer active:", toString(missing))
  )
})

test_that("every basemap serves tiles and its attribution links work", {
  skip_unless_online()
  tiles <- lapply(basemap_names(), provider_tile)
  hrefs <- unlist(lapply(seq_along(tiles), function(i) {
    found <- regmatches(tiles[[i]]$attribution, gregexpr("href=\"[^\"]+\"", tiles[[i]]$attribution))
    sub("^href=\"(.*)\"$", "\\1", found[[1]])
  }))
  expect_urls_work(rbind(
    entries(
      vapply(tiles, `[[`, character(1), "url"),
      paste("basemap", basemap_names()),
      kind = "image"
    ),
    entries(unique(hrefs), "basemap attribution link")
  ))
})

test_that("the geocoder places a known address", {
  skip_unless_online()
  # The map's own request, so endpoint, User-Agent and answer format are all exercised.
  hit <- nominatim_lookup("Berlin, Germany")
  expect(
    identical(hit$outcome, "found"),
    paste("Nominatim lookup failed:", hit$outcome, hit$message)
  )
})
