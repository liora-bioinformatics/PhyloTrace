# app/logic/scheme_guide.R
#
# Step-by-step instructions for getting a published scheme from its provider
# and importing it, shown in a dialog opened from the setup's scheme lists.
# Commands are offered only where the provider's terms allow a tool to enable
# downloads; they run on the user's machine, and PhyloTrace itself never
# contacts a provider.

box::use(
  rlang[`%||%`],
  shiny,
)

box::use(
  app / logic / scheme_sources[safe_url, scheme_source],
  app / logic / setup_ui[plan_list, setup_note],
  app / logic / urls[
    CHEWIE_NS_SCHEMAS,
    ENTEROBASE_SCHEMES,
    PASTEUR_ACCOUNT,
    PASTEUR_API_KEY_HELP,
    PUBMLST_API,
    chewie_ns_api,
  ],
)

# Folders of EnteroBase's public scheme directory, by the scheme's id in
# dev/scheme_index/curation.csv (checked against the directory on 2026-11-23).
# The DSMZ-hosted databases (Enterococcus, Mycobacterium) have no such folder
# and get the website route instead.
ENTEROBASE_FOLDERS <- c(
  "enterobase:clostridium:cgMLST_V1" = "clostridium.cgMLSTv1",
  "enterobase:clostridium:wgMLST" = "clostridium.wgMLST",
  "enterobase:ecoli:Achtman_MLST" = "Escherichia.Achtman7GeneMLST",
  "enterobase:ecoli:cgMLST_V1" = "Escherichia.cgMLSTv1",
  "enterobase:ecoli:wgMLST" = "Escherichia.wgMLST",
  "enterobase:mcatarrhalis:Achtman_MLST" = "Moraxella.Achtman7GeneMLST",
  "enterobase:senterica:Achtman_MLST" = "Salmonella.Achtman7GeneMLST",
  "enterobase:senterica:cgMLST_V2" = "Salmonella.cgMLSTv2",
  "enterobase:senterica:wgMLST" = "Salmonella.wgMLST",
  "enterobase:streptococcus:cgMLST_V1" = "Streptococcus.cgMLSTv1",
  "enterobase:vibrio:cgMLST" = "VIBwgMLST.cgMLSTv1",
  "enterobase:yersinia:Achtman_MLST" = "Yersinia.Achtman7GeneMLST",
  "enterobase:yersinia:cgMLST_V1" = "Yersinia.cgMLSTv1",
  "enterobase:yersinia:McNally_MLST" = "Yersinia.McNally",
  "enterobase:yersinia:wgMLST" = "Yersinia.wgMLST"
)

# Copies the commands next to the button and briefly marks the button as done.
# The clipboard API exists only on secure origins (https, localhost); served
# over plain http from another machine, the selection is copied instead. The
# helper text area sits inside the dialog, which keeps the focus to itself.
COPY_JS <- paste(
  "var b = this, t = b.parentNode.querySelector('code').innerText;",
  "var done = function () { b.classList.add('is-copied');",
  "setTimeout(function () { b.classList.remove('is-copied'); }, 1500); };",
  "var legacy = function () { var a = document.createElement('textarea');",
  "a.value = t; b.parentNode.appendChild(a); a.select();",
  "if (document.execCommand('copy')) { done(); } a.remove(); };",
  "if (navigator.clipboard) { navigator.clipboard.writeText(t).then(done, legacy); }",
  "else { legacy(); }"
)

# A link that opens outside the app window. Inline tags here keep the markup
# from putting a space between them and the punctuation that follows.
external_link <- function(href, label) {
  shiny$tags$a(
    href = href,
    target = "_blank",
    rel = "noopener noreferrer",
    label,
    .noWS = "outside"
  )
}

# Inline code, e.g. a folder or file name.
code_tag <- function(text) shiny$tags$code(text, .noWS = "outside")

# Emphasised name of a control or menu entry.
em_tag <- function(text) shiny$em(text, .noWS = "outside")

# Shell commands with a copy button.
command_block <- function(lines) {
  shiny$div(
    class = "scheme-guide_code",
    shiny$tags$pre(code_tag(paste(lines, collapse = "\n")), .noWS = "inside"),
    shiny$tags$button(
      type = "button",
      class = "btn btn-sm btn-outline-secondary scheme-guide_copy",
      title = "Copy the commands",
      onclick = COPY_JS,
      shiny$span(class = "scheme-guide_copy-idle", shiny$icon("copy"), " Copy"),
      shiny$span(class = "scheme-guide_copy-done", shiny$icon("check"), " Copied")
    )
  )
}

# Folder name for a scheme's files: its name reduced to characters that need no
# quoting in a shell and are valid on every file system.
folder_name <- function(scheme) {
  name <- gsub("^_+|_+$", "", gsub("[^A-Za-z0-9.-]+", "_", scheme))
  if (nzchar(name)) substr(name, 1L, 60L) else "scheme"
}

# Database and scheme id of a BIGSdb index row ("pubmlst_x_seqdef/3"), NULL
# when the id is not of that plain form; it ends up in a shell command.
bigsdb_ids <- function(source_id) {
  if (is.na(source_id) || !grepl("^[a-z0-9_]+_seqdef/[0-9]+$", source_id)) {
    return(NULL)
  }
  parts <- strsplit(source_id, "/", fixed = TRUE)[[1]]
  list(db = parts[1], scheme = parts[2])
}

# Species and schema id of a Chewie-NS index row ("18/1"), NULL otherwise.
chewie_ids <- function(source_id) {
  if (is.na(source_id) || !grepl("^[0-9]+/[0-9]+$", source_id)) {
    return(NULL)
  }
  parts <- strsplit(source_id, "/", fixed = TRUE)[[1]]
  list(species = parts[1], schema = parts[2])
}

# How long a sequential download of `loci` files roughly takes, as words.
duration_hint <- function(loci, seconds_per_locus) {
  if (is.na(loci)) {
    return("")
  }
  minutes <- ceiling(loci * seconds_per_locus / 60)
  if (minutes <= 2) {
    ""
  } else {
    sprintf(" For %s loci this takes about %d minutes.", format(loci, big.mark = ","), minutes)
  }
}

# Final step: where the downloaded files go in PhyloTrace.
import_step <- function(type, what) {
  if (identical(type, "MLST")) {
    list(
      title = "Import the files",
      detail = shiny$tagList(
        "In the task ", em_tag("Scheme files"), ", pick ", what$profiles,
        " as the ", em_tag("Profile table"), ". For ", em_tag("Allele files"),
        ", ", what$alleles
      )
    )
  } else {
    list(
      title = "Import the files",
      detail = shiny$tagList(
        "In the cgMLST scheme step, task ", em_tag("Allele files"), ", click ",
        em_tag("Select allele files"), " and ", what$alleles
      )
    )
  }
}

# "open the folder X and choose 'Use this folder'", as tags.
pick_folder <- function(dir) {
  shiny$tagList(
    "open the folder ", code_tag(dir), " and choose ", em_tag("Use this folder"), "."
  )
}

# Steps for PubMLST. Its restriction on software tools that enable downloading
# (terms, section 4) covers only data submitted from 2025 on; earlier data is
# unrestricted. The commands therefore never take an API key: without one,
# PubMLST serves only data submitted up to 2024. A cgMLST scheme needs only the
# first allele of each locus (the reference PhyloTrace keeps), so the commands
# fetch that alone into one FASTA file instead of every allele - minutes and a
# few megabytes instead of hours and gigabytes.
pubmlst_steps <- function(row) {
  ids <- bigsdb_ids(row$source_id)
  if (is.null(ids)) {
    return(NULL)
  }
  api <- paste0(PUBMLST_API, "/", ids$db)
  dir <- folder_name(row$scheme)
  loci_list <- c(
    sprintf('curl -fsS "$API/schemes/%s/loci" | grep -o "$API/loci/[^\\"]*" \\', ids$scheme)
  )

  if (identical(row$type, "MLST")) {
    commands <- c(
      sprintf('DIR="%s"', dir),
      sprintf('API="%s"', api),
      'mkdir -p "$DIR" && cd "$DIR"',
      sprintf('curl -fsS -o profiles.tsv "$API/schemes/%s/profiles_csv"', ids$scheme),
      loci_list,
      '  | while read -r url; do curl -fsS -o "${url##*/}.fasta" "$url/alleles_fasta"; done',
      'echo "Done: files are in $PWD"'
    )
    return(list(
      list(
        title = "Download the scheme",
        detail = shiny$tagList(
          shiny$p(
            "Paste these commands into a terminal. They save the profile table and one",
            " FASTA file per locus in a new folder ", code_tag(dir), "."
          ),
          command_block(commands),
          shiny$p(
            class = "help-note",
            "Without logging in, PubMLST serves the sequence types and alleles",
            " submitted up to the end of 2024. Newer ones are available to logged-in",
            " users on the PubMLST website (", em_tag("Download page"), ")."
          )
        )
      ),
      import_step("MLST", list(
        profiles = code_tag(paste0(dir, "/profiles.tsv")),
        alleles = pick_folder(dir)
      ))
    ))
  }

  file <- paste0(dir, "_references.fasta")
  commands <- c(
    sprintf('API="%s"', api),
    sprintf('FILE="%s"', file),
    loci_list,
    "  | sed 's|$|/alleles/1|' | xargs curl -fsS --retry 3 -w '\\n' \\",
    "  | awk '/\"sequence\"/ {",
    "      match($0, /\\/loci\\/[^\"]+/); print \">\" substr($0, RSTART + 6, RLENGTH - 6)",
    "      match($0, /\"sequence\":\"[A-Z]+/); print substr($0, RSTART + 12, RLENGTH - 12)",
    "    }' > \"$FILE\"",
    'echo "$(grep -c ">" "$FILE") loci saved in $PWD/$FILE"'
  )
  list(
    list(
      title = "Download the reference alleles",
      detail = shiny$tagList(
        shiny$p(
          "PhyloTrace needs only the first allele of each locus as its reference.",
          " Paste these commands into a terminal; they save those alleles in one file, ",
          code_tag(file), ".", duration_hint(row$loci, 0.25), " No account is needed."
        ),
        command_block(commands),
        shiny$p(
          class = "help-note",
          "The last line reports how many loci were saved. Far fewer than the scheme's ",
          "loci means the scheme is newer than 2024: PubMLST then offers its alleles to ",
          "logged-in users on its website."
        )
      )
    ),
    import_step(row$type, list(
      alleles = shiny$tagList("pick ", code_tag(file), ".")
    ))
  )
}

# Steps for Institut Pasteur. Its policy requires a licence contract from the
# publishers of commercial software tools that enable downloading its data, of
# any date and with or without the user's own API key, so the guide offers no
# commands: it points to the scheme's pages on the Pasteur website.
pasteur_steps <- function(row) {
  ids <- bigsdb_ids(row$source_id)
  if (is.null(ids)) {
    return(NULL)
  }
  page <- function(name) {
    sprintf("%s?db=%s&page=%s&scheme_id=%s", PASTEUR_ACCOUNT, ids$db, name, ids$scheme)
  }
  alleles_link <- external_link(page("downloadAlleles"), "allele download page")
  terms_note <- shiny$p(
    class = "help-note",
    "Institut Pasteur's terms require a licence for software that downloads its data,",
    " so PhyloTrace shows no download commands for it."
  )

  if (identical(row$type, "MLST")) {
    return(list(
      list(
        title = "Download the profile table",
        detail = shiny$tagList(
          "Open the scheme's ",
          external_link(page("downloadProfiles"), "profile download page"),
          " and save the table in a new folder."
        )
      ),
      list(
        title = "Download the allele files",
        detail = shiny$tagList(
          shiny$p(
            "On the ", alleles_link, ", download the FASTA file of every locus into the",
            " same folder."
          ),
          terms_note
        )
      ),
      import_step("MLST", list(
        profiles = "the profile table",
        alleles = shiny$tagList(
          "open the folder and choose ", em_tag("Use this folder"), "."
        )
      ))
    ))
  }

  list(
    list(
      title = "Download the allele files",
      detail = shiny$tagList(
        shiny$p(
          "The scheme's ", alleles_link, " offers one FASTA file per locus. For a",
          " scheme of many loci, Institut Pasteur gives registered users API access",
          " instead (", external_link(PASTEUR_API_KEY_HELP, "Requesting an API key"),
          "). Keep the files together in one folder."
        ),
        terms_note,
        shiny$p(
          class = "help-note",
          "Other providers listed for this species may offer a scheme as a single download."
        )
      )
    ),
    import_step(row$type, list(
      alleles = shiny$tagList(
        "open the folder with the locus files and choose ", em_tag("Use this folder"), "."
      )
    ))
  )
}

# Steps for Chewie-NS: every schema is offered as one compressed download, no
# account needed. PhyloTrace reads the ZIP as it is.
chewie_steps <- function(row) {
  ids <- chewie_ids(row$source_id)
  if (is.null(ids)) {
    return(NULL)
  }
  file <- paste0(folder_name(row$scheme), ".zip")
  url <- chewie_ns_api(sprintf("/species/%s/schemas/%s/zip", ids$species, ids$schema))
  list(
    list(
      title = "Download the schema",
      detail = shiny$tagList(
        shiny$p(
          "Paste these commands into a terminal. They save the compressed schema as ",
          code_tag(file), ". No account is needed."
        ),
        command_block(c(
          sprintf('FILE="%s"', file),
          sprintf('URL="%s"', url),
          'curl -fL --retry 3 -o "$FILE" "$URL?request_type=download"'
        )),
        shiny$p(
          class = "help-note",
          "In the browser, the same file is offered on the Chewie-NS ",
          external_link(CHEWIE_NS_SCHEMAS, "Schemas page"),
          " under ", em_tag("Schema details"), ". If you work with chewBBACA, ",
          code_tag(sprintf(
            "chewBBACA.py DownloadSchema -sp %s -sc %s -o %s",
            ids$species,
            ids$schema,
            folder_name(row$scheme)
          )),
          " gives the same files as a folder."
        )
      )
    ),
    import_step(row$type, list(
      alleles = shiny$tagList(
        "pick ", code_tag(file),
        ". The schema's configuration files and its ", code_tag("short"),
        " folder are skipped."
      )
    ))
  )
}

# Steps for EnteroBase: its public scheme directory holds one gzipped FASTA per
# locus and, for MLST, the profile table. Schemes without a known folder get
# the route through the database's website.
enterobase_steps <- function(row) {
  folder <- if (is.na(row$source_id)) NA else unname(ENTEROBASE_FOLDERS[row$source_id])
  mlst <- identical(row$type, "MLST")
  if (is.na(folder)) {
    website <- list(
      list(
        title = "Open the scheme download page",
        detail = shiny$tagList(
          "Open the database page with ", em_tag("Download page"), ". In the ",
          em_tag("Tasks"), " menu on the left, choose ", em_tag("Download Schemes"),
          " and select ", em_tag(row$scheme), ". Log in or register if asked."
        )
      ),
      list(
        title = "Download the files",
        detail = shiny$tagList(
          if (mlst) {
            shiny$tagList(
              "Click ", em_tag("Download ST profiles for scheme"), " for the profile ",
              "table. Then "
            )
          } else {
            "Click "
          },
          em_tag("Filter/Clear Filters"), ", choose the scheme, tick all loci and click ",
          em_tag("Download alleles for selected loci"),
          ". You get one gzipped FASTA file per locus; keep them together in one folder."
        )
      )
    )
    return(c(website, list(import_step(row$type, list(
      profiles = "the unpacked profile table",
      alleles = shiny$tagList(
        "open the folder with the locus files and choose ", em_tag("Use this folder"), "."
      )
    )))))
  }

  dir <- folder_name(row$scheme)
  commands <- c(
    sprintf('DIR="%s"', dir),
    sprintf('URL="%s%s"', ENTEROBASE_SCHEMES, folder),
    'mkdir -p "$DIR" && cd "$DIR"',
    if (mlst) 'curl -fsS "$URL/profiles.list.gz" | gunzip > profiles.tsv',
    "curl -fsS \"$URL/\" | grep -o 'href=\"[^\"/]*\\.fasta\\.gz\"' | cut -d'\"' -f2 \\",
    "  | sed \"s|^|$URL/|\" | xargs curl -fsS --retry 3 --remote-name-all",
    'echo "Done: $(ls *.fasta.gz | wc -l) loci saved in $PWD"'
  )
  list(
    list(
      title = if (mlst) "Download the scheme" else "Download the allele files",
      detail = shiny$tagList(
        shiny$p(
          "Paste these commands into a terminal. They save ",
          if (mlst) "the profile table and ",
          "one gzipped FASTA file per locus from EnteroBase's public ",
          external_link(ENTEROBASE_SCHEMES, "scheme directory"),
          " in a new folder ", code_tag(dir), ".",
          " A large scheme takes a few minutes. No account is needed."
        ),
        command_block(commands),
        shiny$p(
          class = "help-note",
          "In the browser, the same files are offered on the database page under ",
          em_tag("Tasks"), " > ", em_tag("Download Schemes"), "."
        )
      )
    ),
    import_step(row$type, list(
      profiles = code_tag(paste0(dir, "/profiles.tsv")),
      alleles = pick_folder(dir)
    ))
  )
}

# Steps for cgMLST.org: each scheme page offers its allele files as one ZIP
# archive. Its robots rules exclude scheme pages from automated access, so the
# guide keeps to the browser.
cgmlst_org_steps <- function(row) {
  list(
    list(
      title = "Download the allele files",
      detail = shiny$tagList(
        "Open the scheme page with ", em_tag("Download page"), ". In its download ",
        "section, download the allele files: one ZIP archive with a FASTA file per ",
        "locus. No account is needed."
      )
    ),
    list(
      title = "Optional: download the locus table",
      detail = shiny$tagList(
        "The same section offers the scheme's locus (target) table as CSV. Import it in ",
        "the task ", em_tag("Locus annotation"), " to show gene names and products in ",
        "the cgMLST overview."
      )
    ),
    import_step(row$type, list(
      alleles = shiny$tagList("pick the ZIP archive as it is, without unpacking it.")
    ))
  )
}

# Steps when the provider or the scheme's id is not one the guide knows.
generic_steps <- function(row) {
  list(
    list(
      title = "Download the scheme files",
      detail = shiny$tagList(
        "Open the provider's page with ", em_tag("Download page"), " and download ",
        if (identical(row$type, "MLST")) {
          "the profile table and one FASTA file per locus."
        } else {
          "one FASTA file per locus, ideally as one ZIP archive."
        }
      )
    ),
    import_step(row$type, list(
      profiles = "the profile table",
      alleles = shiny$tagList("pick the ZIP archive, the files or their folder.")
    ))
  )
}

#' Download Steps for a Published Scheme
#'
#' @description The numbered steps that get a scheme from its provider into
#'   PhyloTrace, with ready-to-run commands where the provider's terms allow
#'   them.
#'
#' @param row One-row data frame from `scheme_index$schemes_for_species()`.
#'
#' @return List of lists with `title` and `detail`, as `setup_ui$plan_list()`
#'   takes them.
#' @export
guide_steps <- function(row) {
  steps <- switch(
    row$provider,
    pubmlst = pubmlst_steps(row),
    pasteur = pasteur_steps(row),
    chewie_ns = chewie_steps(row),
    enterobase = enterobase_steps(row),
    cgmlst_org = cgmlst_org_steps(row),
    NULL
  )
  steps %||% generic_steps(row)
}

#' Scheme Download Guide Dialog
#'
#' @description How to obtain a published scheme from its provider: the
#'   provider's terms first, then numbered steps with copyable commands.
#'
#' @param row One-row data frame from `scheme_index$schemes_for_species()`.
#'
#' @return A `shiny$modalDialog()` wrapped in its styling container.
#' @export
guide_modal <- function(row) {
  src <- scheme_source(row$provider)
  provider <- if (is.null(src)) row$provider else src$label
  steps <- guide_steps(row)
  scripted <- any(vapply(
    steps,
    function(st) grepl("scheme-guide_code", as.character(st$detail), fixed = TRUE),
    logical(1)
  ))
  meta <- paste(c(
    provider,
    if (!is.na(row$loci)) sprintf("%s loci", format(row$loci, big.mark = ","))
  ), collapse = " · ")

  shiny$div(
    class = "help-modal about-modal scheme-guide-modal",
    shiny$modalDialog(
      title = NULL,
      easyClose = TRUE,
      shiny$div(
        class = "scheme-guide help-page",
        shiny$div(
          class = "scheme-guide_head",
          shiny$span(class = "setup-scheme_type", row$type),
          shiny$tags$h2(class = "scheme-guide_title", row$scheme)
        ),
        shiny$p(class = "help-lead", meta),
        shiny$p(
          "PhyloTrace imports a scheme from files you download yourself. These steps ",
          "show the usual way to get them",
          if (scripted) {
            shiny$tagList(
              "; the commands run in a terminal on this computer and need ",
              code_tag("curl"), "."
            )
          } else {
            "."
          }
        ),
        if (!is.null(src) && !is.na(src$commercial_use)) {
          setup_note(shiny$tagList(
            src$commercial_use, " ",
            if (safe_url(src$terms_url)) external_link(src$terms_url, "Terms of use")
          ), "warn")
        },
        plan_list(steps)
      ),
      footer = shiny$tagList(
        if (safe_url(row$url)) {
          shiny$tags$a(
            href = row$url,
            target = "_blank",
            rel = "noopener noreferrer",
            class = "btn btn-outline-secondary",
            shiny$icon("arrow-up-right-from-square"),
            " Download page"
          )
        },
        shiny$modalButton("Close")
      )
    )
  )
}
