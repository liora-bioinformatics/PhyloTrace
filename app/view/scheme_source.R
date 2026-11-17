# app/view/scheme_source.R
#
# Provider picker for an imported typing scheme. Shows the provider's terms of
# use and the acknowledgement it asks for, and collects the user's confirmation
# that they obtained the files themselves and may use them.

box::use(
  rlang[`%||%`],
  shiny,
)

box::use(
  app / logic / scheme_sources[scheme_source, source_choices],
)

#' @export
ui <- function(id, types = NULL, label = "Scheme provider") {
  ns <- shiny$NS(id)
  shiny$tagList(
    shiny$selectInput(
      ns("provider"),
      label,
      choices = c("Select the provider ..." = "", source_choices(types)),
      width = "100%"
    ),
    shiny$uiOutput(ns("notice"))
  )
}

#' @export
server <- function(id) {
  shiny$moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Details of the selected provider, NULL until one is picked.
    provider <- shiny$reactive(scheme_source(input$provider))

    # Terms, acknowledgement and the confirmation the import depends on.
    output$notice <- shiny$renderUI({
      src <- provider()
      shiny$req(src)
      shiny$tagList(
        if (identical(src$id, "other")) {
          shiny$div(
            class = "allow-free-text",
            shiny$textInput(ns("other_name"), "Provider name", width = "100%")
          )
        },
        shiny$div(
          class = "scheme-terms",
          if (!is.na(src$terms)) {
            shiny$div(
              class = "scheme-terms_text",
              shiny$icon("scale-balanced"),
              shiny$span(src$terms),
              if (!is.na(src$terms_url)) {
                shiny$a(
                  href = src$terms_url,
                  target = "_blank",
                  rel = "noopener",
                  "Read the full terms"
                )
              }
            )
          },
          # Acknowledgement and citation matter when publishing, not while
          # importing, so they are folded away behind one line.
          if (!is.na(src$acknowledgement) || !is.na(src$citation)) {
            shiny$tags$details(
              class = "scheme-terms_more",
              shiny$tags$summary("Acknowledgement & citation for publications"),
              if (!is.na(src$acknowledgement)) {
                shiny$div(
                  class = "scheme-terms_ack",
                  shiny$tags$strong("Required acknowledgement: "),
                  src$acknowledgement
                )
              },
              if (!is.na(src$citation)) {
                shiny$div(class = "scheme-terms_ack", shiny$tags$strong("Cite: "), src$citation)
              }
            )
          },
          shiny$checkboxInput(
            ns("confirmed"),
            if (identical(src$id, "own")) {
              "This scheme is my own or I hold the rights to use it."
            } else {
              paste(
                "I obtained these files from the provider and my use of them",
                "complies with its terms."
              )
            },
            value = FALSE,
            width = "100%"
          )
        )
      )
    })

    # The provider as it will be recorded, with `confirmed` and a display label.
    shiny$reactive({
      src <- provider()
      if (is.null(src)) {
        return(NULL)
      }
      if (identical(src$id, "other")) {
        name <- trimws(input$other_name %||% "")
        src$label <- if (nzchar(name)) substr(name, 1L, 100L) else NA_character_
      }
      src$confirmed <- isTRUE(input$confirmed) && !is.na(src$label)
      src
    })
  })
}
