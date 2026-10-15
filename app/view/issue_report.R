# app/view/issue_report.R
#
# "Report issue / Contact" navbar button and the modal behind it. The modal is
# also opened from the Report button app/js/report-issue.js adds to every error
# notification; the screenshot is taken in JS before the modal covers the page.

box::use(
  bslib[nav_item],
  later[later],
  shiny[
    actionButton, checkboxInput, div, icon, modalButton, modalDialog,
    moduleServer, NS, observeEvent, p, reactiveVal, removeModal, showModal,
    showNotification, tagList, tags, textAreaInput, textInput
  ],
)
box::use(
  app / logic / issue_report[build_report, flush_outbox, upload_report],
)

#' @export
ui <- function(id) {
  ns <- NS(id)
  nav_item(
    tags$button(
      id = "report-issue",
      type = "button",
      class = "btn btn-default",
      title = "Report issue / Contact",
      # report-issue.js posts {kind, message, screenshot} to this input
      `data-report-input` = ns("report_issue"),
      icon("life-ring")
    )
  )
}

#' @export
server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    pending <- reactiveVal(NULL)

    # Retry reports a previous session could not send, once startup has settled.
    later(function() tryCatch(flush_outbox(), error = function(e) NULL), 10)

    observeEvent(input$report_issue, {
      r <- input$report_issue
      pending(r)
      is_error <- identical(r$kind, "error")
      has_shot <- length(r$screenshot) == 1 && nzchar(r$screenshot)
      showModal(modalDialog(
        title = if (is_error) "Report this error" else "Contact PhyloTrace support",
        easyClose = FALSE,
        if (is_error) {
          div(class = "alert alert-danger py-2", r$message)
        },
        textAreaInput(
          ns("comment"),
          if (is_error) "What were you doing when this happened?" else "Your message",
          width = "100%",
          rows = 5
        ),
        div(
          class = "allow-free-text",
          textInput(ns("email"), "Your email (optional, for a reply)", width = "100%")
        ),
        if (has_shot) {
          tagList(
            checkboxInput(ns("include_shot"), "Include screenshot", TRUE),
            tags$img(
              src = r$screenshot,
              style = "max-width:100%;max-height:180px;border:1px solid var(--bs-border-color);"
            )
          )
        },
        checkboxInput(ns("include_logs"), "Include recent logs", TRUE),
        p(
          class = "text-muted small mb-0",
          "Sent to Liora Bioinformatics only when you click Send. Logs contain ",
          "file names and app events but no sequence data; your home folder is masked. ",
          "If you are offline, the report is kept and sent the next time PhyloTrace starts."
        ),
        footer = tagList(
          modalButton("Cancel"),
          actionButton(ns("send"), "Send", class = "btn-primary", icon = icon("paper-plane"))
        )
      ))
    })

    observeEvent(input$send, {
      r <- pending()
      req_comment <- trimws(input$comment)
      if (!identical(r$kind, "error") && !nzchar(req_comment)) {
        showNotification("Please write a message first.", type = "warning")
        return()
      }
      removeModal()
      zip <- build_report(
        kind = r$kind,
        message = r$message,
        comment = req_comment,
        email = trimws(input$email),
        screenshot = if (isTRUE(input$include_shot)) r$screenshot,
        include_logs = isTRUE(input$include_logs)
      )
      if (is.null(zip)) {
        # A warning, not an error: report-issue.js would offer to report this toast.
        showNotification(
          paste(
            "Could not create the report. Please try again or email",
            "info@liora-bioinformatics.com."
          ),
          type = "warning",
          duration = 10
        )
      } else {
        # Background upload; a failure stays silent and is retried at next start.
        upload_report(zip)
        showNotification("Thank you - your report is being sent.", type = "message")
      }
      pending(NULL)
    })
  })
}
