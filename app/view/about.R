# app/view/about.R
#
# "About PhyloTrace" dialog: what the platform is, its version, licence, how to
# cite it, who develops it and whose species pictures it shows. Opened from the
# navbar and from the start screen.

box::use(
  shiny,
)

box::use(
  app / logic / app_meta[APP_VERSION],
  app / logic / species[image_credits],
)

URLS <- list(
  website = "https://liora-bioinformatics.com/phylotrace",
  github = "https://github.com/liora-bioinformatics/PhyloTrace",
  doi = "https://doi.org/10.5281/zenodo.10996423",
  license = "https://www.gnu.org/licenses/gpl-3.0"
)

# A link that opens outside the app window.
external_link <- function(href, label) {
  shiny$tags$a(href = href, target = "_blank", rel = "noopener noreferrer", label)
}

#' Liora Bioinformatics Logo
#'
#' @description Theme-aware logo: the black and the white variant are both
#'   emitted and CSS shows the one matching `data-bs-theme` (see `.brand-logo`).
#'
#' @param class Optional extra class of both images.
#' @return A tag list of two `img` tags.
#' @export
brand_logo <- function(class = NULL) {
  shiny$tagList(
    shiny$tags$img(
      class = c("brand-logo", "brand-logo--light", class),
      src = "static/images/liora_bioinformatics_black.png",
      alt = "Liora Bioinformatics",
      width = 1200,
      height = 165
    ),
    shiny$tags$img(
      class = c("brand-logo", "brand-logo--dark", class),
      src = "static/images/liora_bioinformatics_white.png",
      alt = "Liora Bioinformatics",
      width = 1200,
      height = 163
    )
  )
}

# Collapsed list of who made each species picture and under which licence.
image_credit_list <- function() {
  credits <- image_credits()
  if (is.null(credits) || !nrow(credits)) {
    return(NULL)
  }
  shiny$tags$details(
    class = "about_credits",
    shiny$tags$summary("Species image credits"),
    shiny$tags$ul(lapply(seq_len(nrow(credits)), function(i) {
      cr <- credits[i, ]
      # .noWS keeps the rendered markup from putting a space before ":" and ",".
      link <- function(href, label) {
        shiny$tags$a(
          href = href,
          target = "_blank",
          rel = "noopener noreferrer",
          label,
          .noWS = "outside"
        )
      }
      shiny$tags$li(
        shiny$tags$em(cr$label, .noWS = "outside"),
        ": ",
        link(cr$source, cr$author),
        ", ",
        if (is.na(cr$license_url)) cr$license else link(cr$license_url, cr$license)
      )
    }))
  )
}

# Body of the About dialog.
about_page <- function() {
  points <- list(
    c(
      "Genomes and epidemiology together",
      "Typing, resistance findings and your own metadata, analysed side by side."
    ),
    c(
      "Ongoing and retrospective",
      "Monitor continuously as isolates arrive, or revisit existing collections at any time."
    ),
    c(
      "Comparable without a central server",
      "Allele fingerprints give partner laboratories identical results without coordinating."
    ),
    c(
      "Verifiable and under your control",
      "Every result keeps its provenance, and you choose what leaves your lab."
    )
  )

  facts <- list(
    list("Version", paste0("v", APP_VERSION)),
    list("Licence", external_link(URLS$license, "GNU GPL v3.0")),
    list("Source code", external_link(URLS$github, "github.com/liora-bioinformatics/PhyloTrace")),
    list("Website", external_link(URLS$website, "liora-bioinformatics.com/phylotrace")),
    list("Contact", "info@liora-bioinformatics.com"),
    list(
      "Cite as",
      shiny$tagList(
        "Freisleben, M. & Paskali, F. (2024). PhyloTrace. Zenodo. ",
        external_link(URLS$doi, "doi:10.5281/zenodo.10996423")
      )
    )
  )

  shiny$div(
    class = "about",
    shiny$div(
      class = "about_head",
      shiny$tags$img(
        class = "about_logo",
        src = "static/images/PhyloTrace_flat_256.png",
        alt = "",
        width = 256,
        height = 256
      ),
      shiny$div(
        shiny$tags$h2(class = "about_title", "PhyloTrace"),
        shiny$p(
          class = "about_tagline",
          "Decentralised pathogen surveillance — under your control."
        )
      )
    ),
    shiny$p(
      class = "about_lead",
      "PhyloTrace is an open-source platform for bacterial pathogen surveillance. It integrates ",
      "cgMLST and MLST typing and antimicrobial-resistance screening with your epidemiological ",
      "metadata — in one local, portable database, on your own hardware and without writing code."
    ),
    shiny$tags$ul(
      class = "about_points",
      lapply(points, function(p) {
        shiny$tags$li(shiny$tags$strong(p[[1]]), shiny$tags$span(p[[2]]))
      })
    ),
    shiny$tags$dl(
      class = "about_facts",
      lapply(facts, function(f) {
        shiny$tagList(shiny$tags$dt(f[[1]]), shiny$tags$dd(f[[2]]))
      })
    ),
    shiny$div(
      class = "about_maker",
      external_link(URLS$website, brand_logo("about_maker-logo")),
      shiny$p(
        "Developed by Liora Bioinformatics in collaboration with Hochschule Furtwangen ",
        "University (HFU) and the Medical University of Graz (MUG). Made in Germany."
      )
    ),
    image_credit_list(),
    shiny$p(
      class = "about_disclaimer",
      shiny$tags$strong("For research use only."),
      " PhyloTrace is not a medical device and is not intended for diagnostic procedures, ",
      "for guiding treatment decisions, or for the management of individual patients."
    )
  )
}

#' @export
ui <- function(id, variant = c("icon", "link")) {
  ns <- shiny$NS(id)
  variant <- match.arg(variant)

  if (variant == "icon") {
    shiny$actionButton(
      ns("open"),
      label = NULL,
      icon = shiny$icon("info"),
      title = "About PhyloTrace",
      `aria-label` = "About PhyloTrace"
    )
  } else {
    shiny$actionLink(ns("open"), "About PhyloTrace", class = "about-link")
  }
}

#' @export
server <- function(id) {
  shiny$moduleServer(id, function(input, output, session) {
    # Shows the About dialog.
    shiny$observeEvent(input$open, {
      # The help dialog's sheet (.help-modal), sized to its content.
      shiny$showModal(shiny$div(
        class = "help-modal about-modal",
        shiny$modalDialog(
          about_page(),
          title = NULL,
          easyClose = TRUE,
          footer = shiny$modalButton("Close")
        )
      ))
    })
  })
}
