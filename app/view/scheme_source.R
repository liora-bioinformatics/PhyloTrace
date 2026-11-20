# app/view/scheme_source.R
#
# Provider picker for an imported typing scheme. Shows the provider's rule for
# commercial use, links its terms of use and collects the user's confirmation
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
    # The dropdown is attached to the body so the card around it cannot clip it.
    shiny$selectizeInput(
      ns("provider"),
      label,
      choices = c("Select the provider ..." = "", source_choices(types)),
      width = "100%",
      options = list(dropdownParent = "body")
    ),
    shiny$uiOutput(ns("notice"))
  )
}

#' @export
server <- function(id, reset = NULL) {
  shiny$moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Starts over without a provider; the terms confirmation goes with it.
    if (!is.null(reset)) {
      shiny$observeEvent(reset(), {
        shiny$updateSelectInput(session, "provider", selected = "")
        shiny$updateCheckboxInput(session, "confirmed", value = FALSE)
      }, ignoreInit = TRUE)
    }

    # Details of the selected provider, NULL until one is picked.
    provider <- shiny$reactive(scheme_source(input$provider))

    # The confirmation the import depends on, next to the provider's terms.
    output$notice <- shiny$renderUI({
      src <- provider()
      shiny$req(src)
      own <- identical(src$id, "own")
      shiny$tagList(
        if (identical(src$id, "other")) {
          shiny$div(
            class = "allow-free-text",
            shiny$textInput(ns("other_name"), "Provider name", width = "100%")
          )
        },
        shiny$div(
          class = "scheme-terms",
          # The provider's commercial-use rule sits above the checkbox, so it is read before
          # the terms are confirmed; most providers exclude commercial use.
          if (!is.na(src$commercial_use)) {
            shiny$p(class = "scheme-terms_rule", shiny$icon("circle-info"), " ", src$commercial_use)
          },
          shiny$checkboxInput(
            ns("confirmed"),
            if (own) {
              "This scheme is my own, or I hold the rights to use it."
            } else {
              "I obtained these files from the provider and use them under its terms."
            },
            value = FALSE
          ),
          # Outside the checkbox label, so following the link does not tick it.
          if (!is.na(src$terms_url)) {
            shiny$a(
              href = src$terms_url,
              target = "_blank",
              rel = "noopener",
              class = "scheme-terms_link",
              shiny$icon("scale-balanced"),
              " Terms of use"
            )
          }
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
