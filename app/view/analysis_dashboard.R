# app/view/analysis_dashboard.R
# Tier 3: Main analysis dashboard — manages the group list and navigation.

box::use(
  shiny[
    NS,
    actionButton,
    div,
    hr,
    icon,
    modalButton,
    modalDialog,
    moduleServer,
    observeEvent,
    outputOptions,
    p,
    reactiveVal,
    reactiveValues,
    removeModal,
    renderUI,
    req,
    selectInput,
    showModal,
    textInput,
    uiOutput,
  ],
  bslib[
    page_sidebar,
    sidebar,
  ],
  app / view / analysis_dashboard / group,
)

#' @export
ui <- function(id) {
  ns <- NS(id)

  page_sidebar(
    fillable = TRUE,
    sidebar = sidebar(
      title = "Analysis Dashboard",
      actionButton(
        ns("trigger_group_modal"),
        "Add New Group",
        icon = icon("folder-plus"),
        class = "btn-success w-100 mb-3"
      ),
      hr(),
      uiOutput(ns("sidebar_navigation"))
    ),
    div(
      class = "ad-group-stack",
      uiOutput(ns("groups_vertical_stack"))
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

    group_counter <- reactiveVal(1)
    active_groups <- reactiveVal(1)

    group_names_map <- reactiveValues()
    group_names_map[["1"]] <- "Analysis Group"

    current_view <- reactiveVal("all")

    # Reset the group list to just the initial group and return to the "all"
    # view. Dynamic group servers remain alive but hidden; they reset via their
    # own observer that receives the same session_reset signal.
    observeEvent(session_reset(), {
      for (i in seq_len(group_counter())[-1]) {
        group_names_map[[as.character(i)]] <- NULL
      }
      group_names_map[["1"]] <- "Analysis Group"
      group_counter(1)
      active_groups(1)
      current_view("all")
    }, ignoreInit = TRUE)

    group$server(
      id = "group_instance_1",
      assigned_name = "Analysis Group",
      remove_group_callback = function() {
        active_groups(setdiff(active_groups(), 1))
        if (current_view() == "1") current_view("all")
      },
      session_reset = session_reset
    )

    observeEvent(input$trigger_group_modal, {
      next_num <- group_counter() + 1
      showModal(modalDialog(
        title = "📁 Setup group environment",
        textInput(
          ns("modal_group_name"),
          "Assign Group Name",
          value = paste("Analysis Group", next_num)
        ),
        footer = list(
          actionButton(
            ns("submit_new_group"),
            "Initialize Group",
            class = "btn-primary"
          ),
          modalButton("Cancel")
        ),
        easyClose = FALSE
      ))
    })

    observeEvent(input$submit_new_group, {
      req(input$modal_group_name)
      removeModal()

      new_id <- group_counter() + 1
      group_counter(new_id)

      id_str <- as.character(new_id)
      chosen_name <- input$modal_group_name

      group_names_map[[id_str]] <- chosen_name

      local({
        current_g_id <- new_id
        g_id_str <- id_str
        group$server(
          id = paste0("group_instance_", g_id_str),
          assigned_name = chosen_name,
          remove_group_callback = function() {
            active_groups(setdiff(active_groups(), current_g_id))
            if (current_view() == g_id_str) current_view("all")
          },
          session_reset = session_reset
        )
      })

      active_groups(c(active_groups(), new_id))
      current_view(id_str)
    })

    output$sidebar_navigation <- renderUI({
      g_ids <- active_groups()
      choices_list <- c("Show All Groups" = "all")

      if (length(g_ids) > 0) {
        for (g_id in g_ids) {
          id_str <- as.character(g_id)
          choices_list[group_names_map[[id_str]]] <- id_str
        }
      }

      selectInput(
        ns("selected_group_view"),
        label = "Navigate Groups:",
        choices = choices_list,
        selected = current_view()
      )
    })

    observeEvent(input$selected_group_view, {
      current_view(input$selected_group_view)
    })

    output$groups_vertical_stack <- renderUI({
      g_ids <- active_groups()

      if (length(g_ids) == 0) {
        return(p(
          class = "ad-empty ad-empty-block",
          paste(
            "No dynamic groups generated yet. Click 'Add New Group' to",
            "initialize new analysis groups."
          )
        ))
      }

      visible_ids <- if (current_view() == "all") {
        g_ids
      } else {
        intersect(g_ids, as.numeric(current_view()))
      }

      if (length(visible_ids) == 0) {
        return(p(class = "ad-empty", "The selected group is no longer active."))
      }

      lapply(visible_ids, function(g_id) {
        group$ui(ns(paste0("group_instance_", g_id)))
      })
    })

    # Keep all rendered outputs reactive while the analysis dashboard panel is
    # absent from the DOM (removed by nav_remove on session reset) so reset-
    # triggered reactive changes propagate before the panel is re-inserted.
    outputOptions(output, "sidebar_navigation", suspendWhenHidden = FALSE)
    outputOptions(output, "groups_vertical_stack", suspendWhenHidden = FALSE)
  })
}
