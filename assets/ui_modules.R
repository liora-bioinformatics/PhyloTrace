# UI Modules
block_ui <-
  'document.getElementById("blocking-overlay").style.display = "block";'
unblock_ui <-
  'document.getElementById("blocking-overlay").style.display = "none";'

species_data_ui <- function(species_data, fetch) {
  if (is.null(species_data$Image)) {
    species_image <- HTML(
      '<i class="fa-solid fa-bacteria" style="font-size:150px;color:white;margin-right:25px;" ></i>'
    )
  } else {
    response <- httr::GET(species_data$Image)
    if (response$status_code == 200) {
      species_image <- tags$img(
        class = "species-image",
        src = species_data$Image
      )
    } else {
      species_image <- HTML(
        '<i class="fa-solid fa-bacteria" style="font-size:150px;color:white;margin-right:25px;" ></i>'
      )
    }
  }

  column(
    width = 12,
    fluidRow(
      br(),
      column(
        width = 7,
        p(
          HTML(
            paste0(
              '<i class="fa-solid fa-bacterium" style="font-size:20px;color:white; margin-right: 10px;"></i>',
              '<span style="color: white; font-size: 22px; ">',
              species_data$Name$name,
              '</span>'
            )
          )
        ),
        p(
          HTML(
            paste0(
              '<span style="color: white; font-size: 12px;">',
              species_data$Name$authority,
              '</span>'
            )
          )
        ),
        br(),
        p(
          HTML(
            paste0(
              '<span style="color: white; font-size: 15px;">',
              'URL: ',
              '<a href="https://www.ncbi.nlm.nih.gov/datasets/taxonomy/',
              species_data$ID,
              '/" target="_blank" style="color:#008edb; text-decoration:none;">',
              species_data$Name$name,
              ' NCBI',
              '</a>',
              '</span>'
            )
          )
        ),
        br(),
        fluidRow(
          column(
            width = 12,
            p(
              HTML(
                paste0(
                  '<span style="color: white; font-size: 20px;">',
                  'Lineage',
                  '</span>'
                )
              )
            ),
            fluidRow(
              column(
                width = 6,
                p(
                  HTML(
                    paste0(
                      '<span style="color: white; font-size: 15px;">',
                      '<a href="https://www.ncbi.nlm.nih.gov/datasets/taxonomy/',
                      species_data$Classification$domain$id,
                      '/" target="_blank" style="color:#008edb; text-decoration:none;">',
                      species_data$Classification$domain$name,
                      '</a>',
                      '</span>'
                    )
                  )
                )
              ),
              column(
                width = 6,
                align = "left",
                p(
                  HTML(
                    paste0(
                      '<span style="color: white; font-size: 12px;">',
                      'Domain',
                      '</span>'
                    )
                  )
                )
              )
            ),
            fluidRow(
              column(
                width = 6,
                p(
                  HTML(
                    paste0(
                      '<span style="color: white; font-size: 15px;">',
                      '<a href="https://www.ncbi.nlm.nih.gov/datasets/taxonomy/',
                      species_data$Classification$kingdom$id,
                      '/" target="_blank" style="color:#008edb; text-decoration:none;">',
                      species_data$Classification$kingdom$name,
                      '</a>',
                      '</span>'
                    )
                  )
                )
              ),
              column(
                width = 6,
                align = "left",
                p(
                  HTML(
                    paste0(
                      '<span style="color: white; font-size: 12px;">',
                      'Kingdom',
                      '</span>'
                    )
                  )
                )
              )
            ),
            fluidRow(
              column(
                width = 6,
                p(
                  HTML(
                    paste0(
                      '<span style="color: white; font-size: 15px;">',
                      '<a href="https://www.ncbi.nlm.nih.gov/datasets/taxonomy/',
                      species_data$Classification$phylum$id,
                      '/" target="_blank" style="color:#008edb; text-decoration:none;">',
                      species_data$Classification$phylum$name,
                      '</a>',
                      '</span>'
                    )
                  )
                )
              ),
              column(
                width = 6,
                p(
                  HTML(
                    paste0(
                      '<span style="color: white; font-size: 12px;">',
                      'Phylum',
                      '</span>'
                    )
                  )
                )
              )
            ),
            fluidRow(
              column(
                width = 6,
                p(
                  HTML(
                    paste0(
                      '<span style="color: white; font-size: 15px;">',
                      '<a href="https://www.ncbi.nlm.nih.gov/datasets/taxonomy/',
                      species_data$Classification$class$id,
                      '/" target="_blank" style="color:#008edb; text-decoration:none;">',
                      species_data$Classification$class$name,
                      '</a>',
                      '</span>'
                    )
                  )
                )
              ),
              column(
                width = 6,
                align = "left",
                p(
                  HTML(
                    paste0(
                      '<span style="color: white; font-size: 12px;">',
                      'Class',
                      '</span>'
                    )
                  )
                )
              )
            ),
            fluidRow(
              column(
                width = 6,
                p(
                  HTML(
                    paste0(
                      '<span style="color: white; font-size: 15px;">',
                      '<a href="https://www.ncbi.nlm.nih.gov/datasets/taxonomy/',
                      species_data$Classification$order$id,
                      '/" target="_blank" style="color:#008edb; text-decoration:none;">',
                      species_data$Classification$order$name,
                      '</a>',
                      '</span>'
                    )
                  )
                )
              ),
              column(
                width = 6,
                align = "left",
                p(
                  HTML(
                    paste0(
                      '<span style="color: white; font-size: 12px;">',
                      'Order',
                      '</span>'
                    )
                  )
                )
              )
            ),
            fluidRow(
              column(
                width = 6,
                p(
                  HTML(
                    paste0(
                      '<span style="color: white; font-size: 15px;">',
                      '<a href="https://www.ncbi.nlm.nih.gov/datasets/taxonomy/',
                      species_data$Classification$family$id,
                      '/" target="_blank" style="color:#008edb; text-decoration:none;">',
                      species_data$Classification$family$name,
                      '</a>',
                      '</span>'
                    )
                  )
                )
              ),
              column(
                width = 6,
                align = "left",
                p(
                  HTML(
                    paste0(
                      '<span style="color: white; font-size: 12px;">',
                      'Family',
                      '</span>'
                    )
                  )
                )
              )
            ),
            fluidRow(
              column(
                width = 6,
                p(
                  HTML(
                    paste0(
                      '<span style="color: white; font-size: 15px;">',
                      '<a href="https://www.ncbi.nlm.nih.gov/datasets/taxonomy/',
                      species_data$Classification$genus$id,
                      '/" target="_blank" style="color:#008edb; text-decoration:none;">',
                      species_data$Classification$genus$name,
                      '</a>',
                      '</span>'
                    )
                  )
                )
              ),
              column(
                width = 6,
                align = "left",
                p(
                  HTML(
                    paste0(
                      '<span style="color: white; font-size: 12px;">',
                      'Genus',
                      '</span>'
                    )
                  )
                )
              )
            )
          )
        )
      ),
      column(
        width = 5,
        align = "right",
        species_image
      )
    )
  )
}

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
      )
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
