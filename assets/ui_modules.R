# UI Modules
mst_control_box <- box(
  solidHeader = TRUE,
  status = "primary",
  width = "100%",
  title = "Controls",
  fluidRow(
    column(
      width = 10,
      column(
        width = 2,
        align = "center",
        div(
          class = "plot-control-buttons",
          tipify(
            actionBttn(
              "mst_label_menu",
              label = "",
              color = "default",
              size = "sm",
              style = "material-flat",
              icon = icon("tags")
            ),
            title = "Labeling",
            options = list("delay': 400, 'foo" = "foo")
          )
        )
      ),
      column(
        width = 2,
        align = "center",
        div(
          class = "plot-control-buttons",
          tipify(
            actionBttn(
              "mst_variable_menu",
              label = "",
              color = "default",
              size = "sm",
              style = "material-flat",
              icon = icon("map-pin")
            ),
            title = "Variable Mapping",
            options = list("delay': 400, 'foo" = "foo")
          )
        )
      ),
      column(
        width = 2,
        align = "center",
        div(
          class = "plot-control-buttons",
          tipify(
            actionBttn(
              "mst_color_menu",
              label = "",
              color = "default",
              size = "sm",
              style = "material-flat",
              icon = icon("palette")
            ),
            title = "Colors",
            options = list("delay': 400, 'foo" = "foo")
          )
        )
      ),
      column(
        width = 2,
        align = "center",
        div(
          class = "plot-control-buttons",
          tipify(
            actionBttn(
              "mst_size_menu",
              label = "",
              color = "default",
              size = "sm",
              style = "material-flat",
              icon = icon("up-right-and-down-left-from-center")
            ),
            title = "Sizing",
            options = list("delay': 400, 'foo" = "foo")
          )
        )
      ),
      column(
        width = 2,
        align = "center",
        div(
          class = "plot-control-buttons",
          tipify(
            actionBttn(
              "mst_misc_menu",
              label = "",
              color = "default",
              size = "sm",
              style = "material-flat",
              icon = icon("ellipsis")
            ),
            title = "Other Settings",
            options = list("delay': 400, 'foo" = "foo")
          )
        )
      ),
      column(
        width = 2,
        align = "center",
        div(
          class = "plot-control-buttons",
          tipify(
            actionBttn(
              "mst_download_menu",
              label = "",
              color = "default",
              size = "sm",
              style = "material-flat",
              icon = icon("download")
            ),
            title = "Export",
            options = list("delay': 400, 'foo" = "foo")
          )
        )
      )
    ),
    column(
      width = 2,
      align = "center",
      div(
        class = "plot-control-reset",
        tipify(
          actionBttn(
            "mst_reset",
            label = "",
            color = "default",
            size = "sm",
            style = "material-flat",
            icon = icon("rotate-right")
          ),
          title = "Reset",
          options = list("delay': 400, 'foo" = "foo")
        )
      )
    )
  )
)

nj_control_box <- box(
  solidHeader = TRUE,
  status = "primary",
  width = "100%",
  title = "Controls",
  fluidRow(
    column(
      width = 10,
      column(
        width = 2,
        align = "center",
        div(
          class = "plot-control-buttons",
          tipify(
            actionBttn(
              "nj_label_menu",
              label = "",
              color = "default",
              size = "sm",
              style = "material-flat",
              icon = icon("tags")
            ),
            title = "Labeling",
            options = list("delay': 400, 'foo" = "foo")
          )
        )
      ),
      column(
        width = 2,
        align = "center",
        div(
          class = "plot-control-buttons",
          tipify(
            actionBttn(
              "nj_variable_menu",
              label = "",
              color = "default",
              size = "sm",
              style = "material-flat",
              icon = icon("map-pin")
            ),
            title = "Variable Mapping",
            options = list("delay': 400, 'foo" = "foo")
          )
        )
      ),
      column(
        width = 2,
        align = "center",
        div(
          class = "plot-control-buttons",
          tipify(
            actionBttn(
              "nj_color_menu",
              label = "",
              color = "default",
              size = "sm",
              style = "material-flat",
              icon = icon("palette")
            ),
            title = "Coloring",
            options = list("delay': 400, 'foo" = "foo")
          )
        )
      ),
      column(
        width = 2,
        align = "center",
        div(
          class = "plot-control-buttons",
          tipify(
            actionBttn(
              "nj_elements_menu",
              label = "",
              color = "default",
              size = "sm",
              style = "material-flat",
              icon = icon("diagram-project")
            ),
            title = "Other Elements",
            options = list("delay': 400, 'foo" = "foo")
          )
        )
      ),
      column(
        width = 2,
        align = "center",
        div(
          class = "plot-control-buttons",
          tipify(
            actionBttn(
              "nj_misc_menu",
              label = "",
              color = "default",
              size = "sm",
              style = "material-flat",
              icon = icon("ellipsis")
            ),
            title = "Other Settings",
            options = list("delay': 400, 'foo" = "foo")
          )
        )
      ),
      column(
        width = 2,
        align = "center",
        div(
          class = "plot-control-buttons",
          tipify(
            actionBttn(
              "nj_download_menu",
              label = "",
              color = "default",
              size = "sm",
              style = "material-flat",
              icon = icon("download")
            ),
            title = "Export",
            options = list("delay': 400, 'foo" = "foo")
          )
        )
      )
    ),
    column(
      width = 2,
      align = "center",
      div(
        class = "plot-control-reset",
        tipify(
          actionBttn(
            "nj_reset",
            label = "",
            color = "default",
            size = "sm",
            style = "material-flat",
            icon = icon("rotate-right")
          ),
          title = "Reset",
          options = list("delay': 400, 'foo" = "foo")
        )
      ),
      materialSwitch(
        "toggle_style",
        "View"
      ),
    )
  )
)

# Locus Screening Menu
screening_menu_available <- sidebarMenu(
  menuItem(
    text = "AMR Profile",
    tabName = "gene_screening",
    icon = icon("dna"),
    startExpanded = TRUE,
    menuSubItem(
      text = "Browse Results",
      tabName = "gs_profile"
    ),
    menuSubItem(
      text = "Screening",
      tabName = "gs_screening"
    ),
    menuSubItem(
      text = "Visualization",
      tabName = "gs_visualization"
    )
  )
)


initiate_multi_typing_ui <- renderUI({
  column(
    width = 12,
    fluidRow(
      column(
        width = 3,
        align = "center",
        br(),
        br(),
        fluidRow(
          column(1),
          column(
            width = 11,
            align = "left",
            h3(
              p("Assembly Selection"),
              style = "color:white; margin-left: 40px"
            ),
          )
        ),
        br(),
        br(),
        fluidRow(
          column(1),
          column(
            width = 11,
            align = "center",
            fluidRow(
              column(
                width = 6,
                align = "center",
                shinyFilesButton(
                  "assembly_files",
                  "Select File(s)",
                  icon = icon("file"),
                  title = "Select one or multiple assembly file(s)",
                  multiple = TRUE,
                  buttonType = "default",
                  class = NULL,
                  width = "120px",
                  root = path_home()
                )
              ),
              column(
                width = 6,
                align = "left",
                uiOutput("multi_file_sel_info")
              )
            ),
            br(),
            fluidRow(
              column(
                width = 6,
                align = "center",
                shinyDirButton(
                  "assembly_folder",
                  "Select Folder",
                  icon = icon("folder-open"),
                  title = "Select folder containing assembly file(s)",
                  buttonType = "default",
                  root = path_home()
                )
              ),
              column(
                width = 6,
                align = "left",
                uiOutput("multi_folder_sel_info")
              )
            ),
            br(),
            br(),
            fluidRow(
              column(1),
              uiOutput("metadata_multi_box")
            )
          )
        )
      ),
      column(1),
      column(
        width = 7,
        br(),
        br(),
        fluidRow(
          column(
            width = 10,
            uiOutput("multi_select_tab_ctrls"),
          )
        ),
        fluidRow(
          column(
            width = 12,
            rHandsontableOutput("multi_select_table")
          )
        )
      )
    )
  )
})
