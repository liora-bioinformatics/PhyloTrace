# app/view/database_scheme_info.R
#
# "Scheme Info" interface of the Database menu. UI and backend live here so
# the panel computes its own state independently of the other menu entries.

box::use(
  shiny[
    NS,
    moduleServer,
    reactive,
    observeEvent,
    req,
    div,
    span,
    em,
    a,
    p,
    imageOutput,
    renderImage,
    uiOutput,
    renderUI,
    tagList
  ],
  bslib[
    as_fill_carrier,
    as_fill_item,
    card,
    card_header,
    card_body
  ],
  DT[datatable, renderDT, DTOutput]
)

box::use(
  app / logic / database_functions[load_db_scheme_overview, load_db_species],
  app / logic / functions[render_info],
  app / logic / scheme_browser[get_species_img, get_species_details]
)

#' @export
ui <- function(id) {
  ns <- NS(id)

  as_fill_carrier(
    # Flex row: the metadata card fills all remaining space, the species
    # aside stays at its fixed (photo) width on the right.
    div(
      class = "scheme-info-layout",
      # Left: scheme overview, fills the available width
      as_fill_carrier(
        div(
          class = "scheme-info-main",
          as_fill_item(
            card(
              fill = TRUE,
              full_screen = TRUE,
              card_header(
                class = "bg-dark",
                "Scheme Metadata"
              ),
              card_body(DTOutput(ns("local_scheme_table"), fill = TRUE))
            )
          )
        )
      ),
      # Right: species photo (relocated out of Details) + details card.
      # The aside is fixed at the photo's width so the Details card below
      # lines up to the same width as the image.
      as_fill_carrier(
        div(
          class = "scheme-aside",
          div(
            class = "species-photo",
            imageOutput(ns("species_img"), height = "auto"),
            uiOutput(ns("species_caption"))
          ),
          as_fill_item(
            card(
              fill = TRUE,
              full_screen = TRUE,
              card_header(
                class = "bg-dark",
                "Details"
              ),
              card_body(
                div(
                  class = "species-card_article",
                  uiOutput(ns("species_details")),
                  uiOutput(ns("species_summary"))
                )
              )
            )
          )
        )
      )
    )
  )
}

#' @export
server <- function(
  id,
  db_path = shiny::reactive(NULL),
  session_reset = shiny::reactive(0L)
) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Reset module state when the user returns to the landing screen.
    # No local reactive state to clear yet; placeholder for future scheme-info UI.
    observeEvent(session_reset(), {}, ignoreInit = TRUE)

    # Load the scheme overview of the current database
    scheme_overview <- reactive({
      req(db_path())
      load_db_scheme_overview(db_path())
    })

    # Authoritative species of the loaded scheme, read from the database's
    # `mlst_type` table (written at typing time).
    scheme_species <- reactive({
      req(db_path())
      load_db_species(db_path())
    })

    # Render scheme info table
    output$local_scheme_table <- renderDT({
      overview <- scheme_overview()

      if (is.null(overview) || isFALSE(is.data.frame(overview))) {
        overview <- data.frame(
          " " = "No 'Scheme Overview' table found. <br> Try rebuilding the schema in the <strong>Create Scheme</strong> module",
          check.names = FALSE
        )
      }

      render_info("output$local_scheme_table")

      datatable(
        overview,
        class = 'stripe row-border order-column',
        colnames = rep("", ncol(overview)),
        rownames = FALSE,
        escape = FALSE,
        selection = "none",
        options = list(dom = "t", ordering = FALSE, paging = FALSE)
      )
    })

    # Render species img
    output$species_img <- renderImage(
      {
        species <- scheme_species()
        req(species)

        render_info("output$species_img")

        list(src = get_species_img(species))
      },
      deleteFile = FALSE
    )

    # Enriched species metadata (taxonomy + description)
    species_record <- reactive({
      species <- scheme_species()
      req(species)

      get_species_details(species)
    })

    # Overlays on the photo: rank + NCBI badges in the top corner (kept off the
    # name row so they never wrap onto a second line at narrow widths), species
    # name across the bottom.
    output$species_caption <- renderUI({
      render_info("output$species_caption")

      species <- scheme_species()
      req(species)

      details <- species_record()

      tagList(
        div(
          class = "species-photo_badges",
          if (!is.null(details) && !is.null(details$rank)) {
            span(details$rank, class = "species-details_rank")
          },
          if (!is.null(details) && !is.null(details$ncbi_taxid)) {
            a(
              href = paste0(
                "https://www.ncbi.nlm.nih.gov/Taxonomy/Browser/wwwtax.cgi?id=",
                details$ncbi_taxid
              ),
              target = "_blank",
              class = "species-details_taxid",
              paste0("NCBI:txid", details$ncbi_taxid)
            )
          }
        ),
        div(
          class = "species-photo_caption",
          span(em(species), class = "species-photo_name")
        )
      )
    })

    # Render taxonomy ladder
    output$species_details <- renderUI({
      render_info("output$species_details")

      req(scheme_species())

      details <- species_record()

      if (is.null(details)) {
        return(div(
          class = "species-details_empty",
          "No metadata available for this species."
        ))
      }

      # Taxonomy ladder: one chip per known rank (skips missing ranks)
      ranks <- c("phylum", "class", "order", "family", "genus")
      chips <- lapply(ranks, function(rank) {
        value <- details$lineage[[rank]]
        if (is.null(value)) {
          return(NULL)
        }
        div(
          class = "species-details_chip",
          span(toupper(rank), class = "species-details_chip-rank"),
          span(value, class = "species-details_chip-value")
        )
      })

      div(
        class = "species-details",
        # Taxonomy ladder
        div(class = "species-details_lineage", chips)
      )
    })

    # Render description in its own full-width row below the title/image
    output$species_summary <- renderUI({
      render_info("output$species_summary")

      details <- species_record()
      req(!is.null(details), !is.null(details$summary))

      p(details$summary, class = "species-details_summary")
    })
  })
}
