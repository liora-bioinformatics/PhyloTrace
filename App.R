######## PhyloTrace #########

options(ignore.negative.edge = TRUE)
options(shiny.error = browser)
Sys.setlocale("LC_TIME", "C")

# _______________________ ####
# CRAN Packages
library(shiny)
library(shinyjs)
library(R.utils)
library(igraph)
suppressPackageStartupMessages(library(ComplexHeatmap))
library(shinyWidgets)
library(shinydashboard)
library(dashboardthemes)
library(ggplot2)
library(ggnewscale)
library(ggplotify)
library(grid)
library(gridExtra)
library(ape)
library(tidyverse)
library(rlang)
library(tidytree)
library(shinyFiles)
library(dplyr)
library(downloader)
library(rvest)
library(rmarkdown)
library(knitr)
library(kableExtra)
library(fs)
library(data.table)
library(zoo)
library(ggnetwork)
library(rhandsontable)
library(visNetwork)
library(proxy)
library(phangorn)
library(cowplot)
library(waiter)
library(viridis)
library(RColorBrewer)
library(bslib)
library(bsicons)
library(DT)
library(shinyBS)
library(openssl)
library(logr)
# Bioconductor Packages
library(treeio)
library(ggtree)
library(ggtreeExtra)

source("./assets/constants.R")
source("./assets/functions.R")
source("./assets/ui_modules.R")
source("./assets/shinyDirChoose.R")
source("./assets/schemes.R")

# User Interface ----

ui <- dashboardPage(
  title = paste("PhyloTrace", phylotraceVersion),

  # Title
  dashboardHeader(
    title = span(
      div(
        class = "img_logo",
        a(
          href = "https://www.liora-bioinformatics.com/phylotrace",
          target = "_blank",
          img(
            src = "PhyloTrace_BW.png",
            width = 190
          )
        )
      )
    ),
    uiOutput("loaded_scheme"),
    uiOutput("statustext"),
    uiOutput("databasetext"),
    dropdownMenuOutput("notificationMenu"),
    tags$li(
      class = "dropdown",
      fluidRow(
        actionBttn(
          "support_menu",
          label = "",
          color = "default",
          size = "sm",
          style = "material-flat",
          icon = icon("circle-question")
        )
      )
    ),
    tags$li(
      class = "dropdown",
      fluidRow(
        actionBttn(
          "settings_menu",
          label = "",
          color = "default",
          size = "sm",
          style = "material-flat",
          icon = icon("gear")
        )
      )
    ),
    tags$li(
      class = "dropdown",
      fluidRow(
        actionBttn(
          "shutdown",
          label = "",
          color = "default",
          size = "sm",
          style = "material-flat",
          icon = icon("xmark")
        )
      )
    ),
    tags$li(
      class = "dropdown",
      tags$span(id = "currentTime", style = "color:black; font-weight:bold;")
    ),
    disable = FALSE
  ),

  ## Sidebar ----
  dashboardSidebar(
    tags$head(includeCSS("www/head.css")),
    tags$style(includeCSS("www/body.css")),
    tags$style(HTML(
      "@keyframes pulsate {
       0% { transform: scale(1); }
        50% { transform: scale(1.1); }
        100% { transform: scale(1); }
       }
       .pulsating-button {
        animation: pulsate 1s ease infinite;
        width: 40px !important;
       }
       .pulsating-button:hover {
        animation: none;
       }
      "
    )),
    br(),
    sidebarMenu(
      id = "tabs",
      br(),
      uiOutput("menu_header_typing"),
      br(),
      sidebarMenuOutput("menu_typing"),
      br(),
      br(),
      uiOutput("menu_header_screening"),
      br(),
      sidebarMenuOutput("menu_screening")
    )
  ),

  dashboardBody(
    div(id = "blocking-overlay"),
    tags$head(tags$link(rel = "shortcut icon", href = "pt_small_icon.png")),
    useShinyjs(),
    shinyDashboardThemeDIY(
      ### general
      appFontFamily = "Liberation Sans",
      appFontColor = "#000000",
      primaryFontColor = "#ffffff",
      infoFontColor = "rgb(0,0,0)",
      successFontColor = "rgb(0,0,0)",
      warningFontColor = "rgb(0,0,0)",
      dangerFontColor = "rgb(0,0,0)",
      bodyBackColor = cssGradientThreeColors(
        direction = "down",
        colorStart = "#282f38",
        colorMiddle = "#384454",
        colorEnd = "#495d78",
        colorStartPos = 0,
        colorMiddlePos = 50,
        colorEndPos = 100
      ),
      ### header
      logoBackColor = "#282f38",
      headerButtonBackColor = "#282f38",
      headerButtonIconColor = "#18ece1",
      headerButtonBackColorHover = "#282f38",
      headerButtonIconColorHover = "#ffffff",
      headerBackColor = "#282f38",
      headerBoxShadowColor = "#aaaaaa",
      headerBoxShadowSize = "0px 0px 0px",
      ### sidebar
      sidebarBackColor = cssGradientThreeColors(
        direction = "down",
        colorStart = "#282f38",
        colorMiddle = "#384454",
        colorEnd = "#495d78",
        colorStartPos = 0,
        colorMiddlePos = 50,
        colorEndPos = 100
      ),
      sidebarPadding = 0,
      sidebarMenuBackColor = "transparent",
      sidebarMenuPadding = 0,
      sidebarMenuBorderRadius = 0,
      sidebarShadowRadius = "5px 5px 5px",
      sidebarShadowColor = "#282f38",
      sidebarUserTextColor = "#ffffff",
      sidebarSearchBackColor = "rgb(55,72,80)",
      sidebarSearchIconColor = "rgb(153,153,153)",
      sidebarSearchBorderColor = "rgb(55,72,80)",
      sidebarTabTextColor = "rgb(255,255,255)",
      sidebarTabTextSize = 15,
      sidebarTabBorderStyle = "none none solid none",
      sidebarTabBorderColor = "rgb(35,106,135)",
      sidebarTabBorderWidth = 0,
      sidebarTabBackColorSelected = cssGradientThreeColors(
        direction = "right",
        colorStart = "rgba(44,222,235,1)",
        colorMiddle = "rgba(44,222,235,1)",
        colorEnd = "rgba(0,255,213,1)",
        colorStartPos = 0,
        colorMiddlePos = 30,
        colorEndPos = 100
      ),
      sidebarTabTextColorSelected = "rgb(0,0,0)",
      sidebarTabRadiusSelected = "0px 0px 0px 0px",
      sidebarTabBackColorHover = cssGradientThreeColors(
        direction = "right",
        colorStart = "rgba(44,222,235,1)",
        colorMiddle = "rgba(44,222,235,1)",
        colorEnd = "rgba(0,255,213,1)",
        colorStartPos = 0,
        colorMiddlePos = 30,
        colorEndPos = 100
      ),
      sidebarTabTextColorHover = "rgb(50,50,50)",
      sidebarTabBorderStyleHover = "none none solid none",
      sidebarTabBorderColorHover = "rgb(75,126,151)",
      sidebarTabBorderWidthHover = 0,
      sidebarTabRadiusHover = "0px 0px 0px 0px",
      ### boxes
      boxBackColor = "#ffffff",
      boxBorderRadius = 7,
      boxShadowSize = "0px 0px 0px",
      boxShadowColor = "#ffffff",
      boxTitleSize = 20,
      boxDefaultColor = "#00a65a",
      boxPrimaryColor = "#ffffff",
      boxInfoColor = "#00a65a",
      boxSuccessColor = "#00a65a",
      boxWarningColor = "#ffffff",
      boxDangerColor = "#ffffff",
      tabBoxTabColor = "#ffffff",
      tabBoxTabTextSize = 14,
      tabBoxTabTextColor = "rgb(0,0,0)",
      tabBoxTabTextColorSelected = "rgb(0,0,0)",
      tabBoxBackColor = "#ffffff",
      tabBoxHighlightColor = "#ffffff",
      tabBoxBorderRadius = 5,
      ### inputs
      buttonBackColor = "#282F38",
      buttonTextColor = "#ffffff",
      buttonBorderColor = "#282F38",
      buttonBorderRadius = 5,
      buttonBackColorHover = cssGradientThreeColors(
        direction = "right",
        colorStart = "rgba(44,222,235,1)",
        colorMiddle = "rgba(44,222,235,1)",
        colorEnd = "rgba(0,255,213,1)",
        colorStartPos = 0,
        colorMiddlePos = 30,
        colorEndPos = 100
      ),
      buttonTextColorHover = "#000000",
      buttonBorderColorHover = "transparent",
      textboxBackColor = "#ffffff",
      textboxBorderColor = "#ffffff",
      textboxBorderRadius = 5,
      textboxBackColorSelect = "#ffffff",
      textboxBorderColorSelect = "#000000",
      ### tables
      tableBackColor = "rgb(255,255,255)",
      tableBorderColor = "rgb(240,240,240)",
      tableBorderTopSize = 1,
      tableBorderRowSize = 1
    ),

    # Render start page message
    uiOutput("start_message"),

    tabItems(
      ## Tab Database ----

      ### Tab Browse Entries ----

      tabItem(
        tabName = "db_browse_entries",
        fluidRow(
          column(
            width = 3,
            align = "center",
            h2(p("Browse Entries"), style = "color:white")
          ),
          column(
            width = 7,
            align = "left",
            p(
              HTML(
                paste0(
                  '<span style="color: white; font-size: 15px; position:relati',
                  've; top:25px;"> Browse and manage the local database with e',
                  'ntries comprising isolates with their metadata, variables a',
                  'nd results of cgMLST Typing.</span>'
                )
              )
            )
          )
        ),
        hr(),
        br(),
        fluidRow(
          column(1),
          div(
            class = "db-table-column",
            column(
              width = 8,
              fluidRow(
                column(
                  width = 12,
                  uiOutput("no_scheme_entries"),
                  uiOutput("db_no_entries"),
                  uiOutput("entry_table_controls")
                )
              ),
              fluidRow(
                div(
                  class = "db-table-column",
                  column(
                    width = 12,
                    br(),
                    uiOutput("db_entries_table")
                  )
                )
              )
            )
          ),
          div(
            class = "db-controls-column",
            column(
              width = 3,
              align = "left",
              uiOutput("custom_var_box"),
              uiOutput("delete_box"),
              uiOutput("compare_allele_box")
            )
          )
        )
      ),

      ### Tab Scheme Info  ----

      tabItem(
        tabName = "db_schemeinfo",
        fluidRow(
          column(
            width = 3,
            align = "center",
            h2(p("Scheme Info"), style = "color:white")
          ),
          column(
            width = 1,
            align = "left",
            uiOutput("download_scheme_info")
          ),
          column(
            width = 7,
            align = "left",
            p(
              HTML(
                paste0(
                  '<span style="color: white; font-size: 15px; position:relati',
                  've; top:25px;">Information about the currently selected sch',
                  'eme and the respective species.</span>'
                )
              )
            )
          )
        ),
        hr(),
        br(),
        uiOutput("no_scheme_info"),
        fluidRow(
          column(1),
          column(
            width = 5,
            tableOutput("scheme_info")
          ),
          column(
            width = 5,
            uiOutput("species_info_select_saved"),
            uiOutput("species_info_saved")
          )
        )
      ),

      ### Tab Loci Info  ----

      tabItem(
        tabName = "db_loci_info",
        fluidRow(
          column(
            width = 3,
            align = "center",
            h2(p("Loci Info"), style = "color:white")
          ),
          column(
            width = 1,
            uiOutput("download_loci")
          ),
          column(
            width = 7,
            align = "left",
            uiOutput("loci_info_text"),
            uiOutput("db_loci_no")
          )
        ),
        hr(),
        fluidRow(
          column(1),
          div(
            class = "loci-info-column",
            column(
              width = 7,
              align = "center",
              br(),
              div(class = "loci-table", dataTableOutput("db_loci"))
            )
          ),
          div(
            class = "loci-controls-column",
            column(
              width = 4,
              br(),
              uiOutput("sequence_selector"),
              uiOutput("loci_sequences")
            )
          )
        )
      ),

      ### Tab Distance Matrix  ----

      tabItem(
        tabName = "db_distmatrix",
        fluidRow(
          column(
            width = 3,
            align = "center",
            h2(p("Distance Matrix"), style = "color:white")
          ),
          uiOutput("distance_matrix_info")
        ),
        hr(),
        br(),
        uiOutput("no_scheme_distancematrix"),
        uiOutput("distancematrix_no_entries"),
        uiOutput("distmatrix_show"),
        br()
      ),

      ### Tab Missing Values ----

      tabItem(
        tabName = "db_missing_values",
        fluidRow(
          column(
            width = 3,
            align = "center",
            h2(p("Missing Values"), style = "color:white")
          ),
          column(
            width = 1,
            align = "left",
            downloadBttn(
              "download_na_matrix",
              style = "simple",
              label = "",
              size = "sm",
              icon = icon("download")
            )
          ),
          column(
            width = 7,
            align = "left",
            p(
              HTML(
                paste0(
                  '<span style="color: white; font-size: 15px; position:relati',
                  've; top:25px;">Overview of isolates whose allelic profile c',
                  'ontains missing values (NA). This occurs if no allele could',
                  ' be assigned for a locus.</span>'
                )
              )
            )
          )
        ),
        hr(),
        br(),
        fluidRow(
          column(1),
          div(
            class = "miss-val-column",
            column(
              width = 3,
              uiOutput("missing_values")
            )
          ),
          div(
            class = "miss-val-table-col",
            column(
              width = 7,
              rHandsontableOutput("table_missing_values"),
              br(),
              fluidRow(
                column(
                  width = 2,
                  div(
                    class = "rectangle-red-space"
                  )
                ),
                column(
                  width = 10,
                  align = "left",
                  p(
                    HTML(
                      paste(
                        tags$span(
                          style = paste0(
                            "color: white; font-size: 15px; posit",
                            "ion: relative; bottom: -12px"
                          ),
                          "≥ 5% of loci missing"
                        )
                      )
                    )
                  )
                )
              )
            )
          )
        )
      ),

      ## Tab Manage Schemes  ----

      tabItem(
        tabName = "init",
        fluidRow(
          column(
            width = 3,
            align = "center",
            h2(p("Manage Schemes"), style = "color:white")
          ),
          column(
            width = 7,
            align = "left",
            p(
              HTML(
                paste0(
                  '<span style="color: white; font-size: 15px; position:relati',
                  've; top:25px;">Browse remote cgMLST schemes available on ',
                  ' <a href="https://www.cgmlst.org/ncs/" target="_blank" styl',
                  'e="color:#008edb; text-decoration:none;"> cgMLST.org Nomenc',
                  'lature Server (h25) </a> and <a href="https://pubmlst.org/',
                  '" target="_blank" style="color:#008edb; text-decoration:non',
                  'e;"> pubMLST </a> databases.</span>'
                )
              )
            )
          )
        ),
        hr(),
        br(),
        fluidRow(
          column(1),
          column(
            width = 5,
            div(
              class = "scheme-sel-box",
              box(
                solidHeader = TRUE,
                status = "primary",
                width = "100%",
                title = "Scheme Selection",
                fluidRow(
                  column(
                    width = 4,
                    p(
                      HTML(
                        paste0(
                          '<span style="color: white; font-size: 15px; positio',
                          'n:relative; top: 15px; left: 30px;"><i class="fas f',
                          'a-layer-group" style="color:white; font-size: 15px"',
                          '></i>Select Scheme'
                        )
                      )
                    )
                  ),
                  column(
                    width = 8,
                    align = "left",
                    uiOutput("scheme_selector")
                  )
                )
              )
            )
          ),
          column(1),
          column(
            width = 5,
            div(
              class = "manage-schemes-actions-box",
              box(
                solidHeader = TRUE,
                status = "primary",
                width = "100%",
                title = "Fetch Data from Scheme Database",
                fluidRow(
                  column(1),
                  column(
                    width = 5,
                    disabled(
                      actionButton(
                        "download_cgMLST",
                        label = "Download",
                        icon = icon("download")
                      )
                    ),
                    hidden(
                      div(
                        id = "downloading",
                        HTML(
                          paste0(
                            "<span style='color: white; font-size: 15px; posit",
                            "ion: relative;top: 5px;'>Downloading scheme",
                            '<i class="fa fa-spinner fa-spin fa-fw fa-2x" styl',
                            'e="color:white; margin-left: 15px; position: rela',
                            'tive; top: 6px; margin-bottom: 26px;"></i>'
                          )
                        )
                      )
                    ),
                    hidden(
                      div(
                        id = "hashing",
                        HTML(
                          paste0(
                            "<span style='color: white; font-size: 15px; posit",
                            "ion: relative;top: 5px;'>Hashing scheme",
                            '<i class="fa fa-spinner fa-spin fa-fw fa-2x" styl',
                            'e="color:white; margin-left: 15px; position: rela',
                            'tive; top: 6px; margin-bottom: 26px;"></i>'
                          )
                        )
                      )
                    )
                  ),
                  column(
                    width = 6,
                    uiOutput("scheme_update_info")
                  )
                )
              )
            )
          )
        ),
        fluidRow(
          column(1),
          column(
            width = 5,
            align = "left",
            uiOutput("cgmlst_scheme_table")
          ),
          column(
            width = 5,
            uiOutput("species_info_select"),
            uiOutput("species_info")
          )
        )
      ),

      ## Tab Allelic Typing ----------------------------------------------

      tabItem(
        tabName = "typing",
        fluidRow(
          column(
            width = 3,
            align = "center",
            h2(p("Generate Allelic Profile"), style = "color:white")
          )
        ),
        hr(),
        uiOutput("typing_no_db"),
        fluidRow(
          uiOutput("initiate_multi_typing_ui"),
          uiOutput("multi_stop"),
          column(1),
          uiOutput("start_multi_typing_ui")
        ),
        fluidRow(
          column(
            width = 6,
            uiOutput("pending_typing")
          ),
          column(
            width = 6,
            uiOutput("multi_typing_results")
          )
        )
      ),

      ## Tab Visualization -----------------------------------------------------

      tabItem(
        tabName = "visualization",
        fluidRow(
          column(
            width = 5,
            br(),
            div(
              class = "vis-control-box",
              uiOutput("generate_plot_ui")
            )
          ),
          column(
            width = 7,
            br(),
            conditionalPanel(
              "input.tree_type=='MST'",
              div(
                class = "vis-control-box-2",
                mst_control_box
              )
            ),
            conditionalPanel(
              "input.tree_type=='Tree'",
              div(
                class = "vis-control-box-2",
                nj_control_box
              )
            )
          )
        ),
        fluidRow(
          tags$script(src = "javascript_functions.js"),
          column(
            width = 2,
            conditionalPanel(
              "input.tree_type=='MST'",
              div(
                class = "vis-control-box-3",
                uiOutput("mst_controls")
              )
            ),
            conditionalPanel(
              "input.tree_type=='Tree'",
              div(
                class = "vis-control-box-3",
                uiOutput("tree_controls")
              )
            )
          ),
          column(
            width = 9,
            align = "left",
            br(),
            conditionalPanel(
              "input.tree_type=='MST'",
              uiOutput("mst_field")
            ),
            conditionalPanel(
              "input.tree_type=='Tree'",
              uiOutput("tree_field")
            )
          )
        )
      ),

      ## Tab Screening ---------------------------------------------------------

      tabItem(
        tabName = "gs_screening",
        fluidRow(
          column(
            width = 3,
            align = "center",
            h2(p("Screening"), style = "color:white; margin-bottom: -20px;")
          ),
          column(
            width = 7,
            align = "left",
            uiOutput("gene_screening_info")
          )
        ),
        br(),
        hr(),
        fluidRow(
          uiOutput("screening_interface")
        )
      ),

      ## Tab Resistance Profile ------------------------------------------------

      tabItem(
        tabName = "gs_profile",
        fluidRow(
          column(
            width = 3,
            align = "center",
            h2(p("Browse Entries"), style = "color:white; margin-bottom: -20px")
          ),
          column(
            width = 7,
            align = "left",
            uiOutput("gene_resistance_info")
          )
        ),
        br(),
        hr(),
        br(),
        br(),
        fluidRow(
          column(1),
          uiOutput("gs_profile_display")
        )
      ),

      ## Tab GS Visualization --------------------------------------------------

      tabItem(
        tabName = "gs_visualization",
        uiOutput("gs_visualization_ui")
      )
    ) # End tabItems
  ) # End dashboardPage
) # end UI

# _______________________ ####

# Server ----

server <- function(input, output, session) {
  ## Reactives ----

  # default plot control input values

  fruit_width <- function(
    nj_layout = nj_layout_val(),
    nj_max_x = Vis$nj_max_x
  ) {
    if (!is.null(nj_layout)) {
      if (!is.null(nj_max_x)) {
        if (round(ceiling(nj_max_x) * 0.033, 1) < 0.1) {
          width <- 0.1
          ifelse(
            nj_layout == "circular" || nj_layout == "inward",
            width <- 1.5,
            width <- 0.5
          )
        } else {
          width <- round(ceiling(nj_max_x) * 0.033, 1)
          if (nj_layout == "circular" || nj_layout == "inward") {
            width <- width * 4
          }
        }
      } else {
        ifelse(
          nj_layout == "circular" || nj_layout == "inward",
          width <- 1.5,
          width <- 0.5
        )
      }
    } else {
      width <- 1
    }
    return(width)
  }

  heatmap_width <- reactive({
    if (!is.null(nj_layout_val())) {
      if (!is.null(nj_heatmap_select_val())) {
        length_input <- length(nj_heatmap_select_val())
        if (
          nj_layout_val() != "circular" &&
            nj_layout_val() != "inward"
        ) {
          if (length_input < 3) {
            width <- 0.1
          } else {
            if (length_input >= 3 && length_input <= 50) {
              width <- min(0.15 + 0.05 * floor((length_input - 3) / 2), 1.5)
            } else {
              width <- 1.5
            }
          }
        } else {
          if (length_input < 3) {
            width <- 0.3
          } else if (length_input >= 3 && length_input <= 27) {
            width <- min(0.6 + 0.2 * floor((length_input - 3) / 2), 1.5)
          } else {
            width <- 3
          }
        }
      } else {
        width <- 0.1
      }
    } else {
      width <- 0.1
    }

    return(width)
  })

  clade_highlight_color <- reactive({
    selected_nodes <- nj_parentnode_val()
    if (
      length(
        selected_nodes
      ) ==
        1 &&
        selected_nodes == ""
    ) {
      selected_nodes <- NULL
    }

    if (length(selected_nodes) > 0) {
      if (length(selected_nodes) == 1) {
        if (!is.null(nj_clade_scale_val())) {
          if (!startsWith(nj_clade_scale_val(), "#")) {
            "#D0F221"
          } else {
            nj_clade_scale_val()
          }
        } else {
          "#D0F221"
        }
      } else {
        "Set1"
      }
    } else {
      "#D0F221"
    }
  })

  nj_tippoint_scale_val_default <- reactive({
    determine_scale(
      variable_val = nj_tipcolor_mapping_val(),
      meta_nj = Vis$meta_nj,
      numeric_scale = "viridis",
      long_scale = "turbo",
      short_scale = "Set1"
    )
  })

  nj_tiplab_scale_val_default <- reactive({
    determine_scale(
      variable_val = nj_color_mapping_val(),
      meta_nj = Vis$meta_nj,
      numeric_scale = "viridis",
      long_scale = "turbo",
      short_scale = "Set1"
    )
  })

  nj_tiles_scale_1_val_default <- reactive({
    determine_scale(
      variable_val = nj_fruit_variable_val(),
      meta_nj = Vis$meta_nj,
      numeric_scale = "viridis",
      long_scale = "turbo",
      short_scale = "Accent"
    )
  })

  nj_tiles_scale_2_val_default <- reactive({
    determine_scale(
      variable_val = nj_fruit_variable_2_val(),
      meta_nj = Vis$meta_nj,
      numeric_scale = "viridis",
      long_scale = "turbo",
      short_scale = "Accent"
    )
  })

  nj_tiles_scale_3_val_default <- reactive({
    determine_scale(
      variable_val = nj_fruit_variable_3_val(),
      meta_nj = Vis$meta_nj,
      numeric_scale = "viridis",
      long_scale = "turbo",
      short_scale = "Accent"
    )
  })

  nj_tiles_scale_4_val_default <- reactive({
    determine_scale(
      variable_val = nj_fruit_variable_4_val(),
      meta_nj = Vis$meta_nj,
      numeric_scale = "viridis",
      long_scale = "turbo",
      short_scale = "Accent"
    )
  })

  nj_tiles_scale_5_val_default <- reactive({
    determine_scale(
      variable_val = nj_fruit_variable_5_val(),
      meta_nj = Vis$meta_nj,
      numeric_scale = "viridis",
      long_scale = "turbo",
      short_scale = "Accent"
    )
  })

  nj_heatmap_scale_val_default <- reactive({
    determine_scale(
      variable_val = nj_heatmap_select_val(),
      meta_nj = Vis$meta_nj,
      numeric_scale = "viridis",
      long_scale = "turbo",
      short_scale = "Dark2"
    )
  })

  # Change UI values reactively

  tree.type.info <- reactive({
    mst_text <- paste0(
      "A minimum-spanning tree (MST) connects samples using",
      "the shortest total genetic distances without assuming",
      "an evolutionary model, making it ideal for visualizing",
      "practical relationships, such as population structure",
      "or transmission pathways. It prioritizes simplicity and",
      "may not reflect ancestry or divergence patterns."
    )
    tree_text <- paste0(
      "A hierarchical tree assumes evolutionary relationships and organizes sa",
      "mples into a bifurcating structure, illustrating divergence from common",
      " ancestors. It is better suited for visualizing broader evolutionary tr",
      "ends but relies on model assumptions that may oversimplify local geneti",
      "c relationships."
    )
    if (!is.null(input$tree_type)) {
      if (input$tree_type == "MST") {
        mst_text
      } else {
        tree_text
      }
    } else {
      mst_text
    }
  })

  plot.control <- reactive({
    if (!is.null(input$tree_type)) {
      if (input$tree_type == "MST") {
        "Generate Minimum-Spanning Tree"
      } else {
        "Generate Hierarchical Tree"
      }
    } else {
      "Generate Minimum-Spanning Tree"
    }
  })

  # Get rhandsontable
  get.entry.table.meta <- reactive({
    table <- hot_to_r(input$db_entries)
    if (!is.null(table)) {
      tbl_no_screened <- select(table, -13)
      num_meta_cols <- 1:(13 + nrow(DB$cust_var))

      if (ncol(tbl_no_screened) >= length(num_meta_cols)) {
        select(tbl_no_screened, all_of(num_meta_cols))
      }
    }
  })

  entry_table_height <- reactive({
    if (!is.null(DB$data)) {
      if (nrow(DB$data) > 24) {
        height <- 600
      } else {
        height <- NULL
      }
    } else {
      height <- NULL
    }
  })

  # Function to check for duplicate isolate IDs for multi typing start
  dupl_mult_id <- reactive({
    req(Typing$multi_sel_table)
    if (!is.null(DB$data)) {
      selection <- Typing$multi_sel_table[
        which(
          unlist(Typing$multi_sel_table$Files) %in%
            unlist(DB$data["Assembly ID"])
        ),
      ]
      selection$Files
    } else {
      ""
    }
  })

  # Function to check single typing log file
  check_new_entry <- reactive({
    invalidateLater(5000, session)

    if (!is.null(Startup$database) && !is.null(DB$scheme)) {
      typing_file <- file.path(
        Startup$database,
        gsub(" ", "_", DB$scheme),
        "Typing.rds"
      )

      if (file_exists(typing_file)) {
        # Initialize Database to NULL in case of failure
        Database <- tryCatch(
          {
            readRDS(typing_file)
          },
          error = function(e) {
            log_print(paste(
              "Error in check_new_entry():",
              "Typing.rds could not be read."
            ))
            return(NULL)
          }
        )

        # Stop further execution if readRDS failed
        if (is.null(Database)) {
          return(FALSE)
        }

        if (is.null(DB$data)) {
          if (nrow(Database[["Typing"]]) >= 1) {
            return(TRUE)
          } else {
            return(FALSE)
          }
        } else {
          if (nrow(DB$data) < nrow(Database[["Typing"]])) {
            return(TRUE)
          } else {
            return(FALSE)
          }
        }
      } else {
        return(FALSE)
      }
    } else {
      return(FALSE)
    }
  })

  # Render Entry Table Highlights

  pinned_entries_highlight <- reactive({
    req(DB$meta)
    pinned_entries <- grep(":ext", DB$meta$`Assembly ID`)
    if (length(pinned_entries)) pinned_entries else return(NULL)
  })

  diff_allele <- reactive({
    if (
      !is.null(DB$data) &&
        !is.null(input$compare_select) &&
        !is.null(DB$cust_var)
    ) {
      data <- select(DB$data, any_of(input$compare_select))

      if (length(data)) {
        var_alleles(data) + (14 + nrow(DB$cust_var))
      }
    }
  })

  err_thresh <- reactive({
    if (!is.null(DB$data) && !is.null(DB$number_loci)) {
      which(as.numeric(DB$data[["Errors"]]) >= (DB$number_loci * 0.05))
    }
  })

  err_thresh_na <- reactive({
    if (!is.null(DB$na_table) && !is.null(DB$number_loci)) {
      which(as.numeric(DB$na_table[["Errors"]]) >= (DB$number_loci * 0.05))
    }
  })

  true_rows <- reactive({
    if (!is.null(DB$data)) {
      which(DB$data$Include == TRUE)
    }
  })

  duplicated_names <- reactive({
    if (!is.null(DB$meta)) {
      which(
        duplicated(DB$meta$`Assembly Name`) |
          duplicated(DB$meta$`Assembly Name`, fromLast = TRUE)
      )
    }
  })

  duplicated_ids <- reactive({
    if (!is.null(DB$meta)) {
      which(
        duplicated(DB$meta$`Assembly ID`) |
          duplicated(DB$meta$`Assembly ID`, fromLast = TRUE)
      )
    }
  })

  # _______________________ ####

  ## Startup ----

  ### Miscellaneous ----

  # Waiting screen config
  waiting_screen <- tagList(
    spin_flower(),
    h4("Loading Database", style = "color: white; cursor: wait")
  )

  # Waiter confid
  w <- Waiter$new(
    id = "db_entries",
    html = spin_3(),
    color = transparent(.5)
  )

  #TODO Enable this, or leave disabled
  # Kill server on session end
  session$onSessionEnded(function() {
    system(
      paste("bash", shQuote(paste0(getwd(), "/bin/kill_multi.sh"))),
      wait = TRUE
    )
    stopApp()
  })

  ### Logging ----
  # Set log paths
  app_local_share_path <- file.path(
    path_home(),
    ".local",
    "share",
    "phylotrace"
  )
  logdir <- file.path(app_local_share_path, "logs")
  logfile <- file.path(logdir, "phylotrace.log")

  # Initiate logging
  # Create logs dir in user local share
  if (!dir_exists(logdir)) {
    dir_create(logdir, recurse = TRUE)
  }

  # Initiate logging
  logfile <- file.path(file.path(logdir, "phylotrace.log"))
  log <- log_open(logfile, logdir = FALSE)
  log_print("Session started")

  ### Reactive variables ----

  # reactive variables related to startup process
  Startup <- reactiveValues(sidebar = TRUE, block_db = FALSE)

  # reactive variables related to local database
  DB <- reactiveValues()

  # reactive variables related to typing process
  Typing <- reactiveValues()

  # Null typing progress trackers
  writeLines("0", file.path(logdir, "script_log.txt"))
  saveRDS(list(), file.path(app_local_share_path, "event_list.rds"))

  # reactive variables related to gene screening
  Screening <- reactiveValues()

  # reactive variables related to visualization
  Vis <- reactiveValues()

  # reactive variables related to report functions
  Report <- reactiveValues()

  # reactive variables related to scheme functions
  Scheme <- reactiveValues()

  ### Set up environment ----

  # Get available scheme urls
  tryCatch(
    {
      cgmlstorg_schemes$url <- get_latest_url(cgmlstorg_schemes$abb)
    },
    error = function(e) {
      DB$failCon <- TRUE
      show_toast(
        title = "Could not retrieve data. Check internet connection.",
        type = "error",
        position = "bottom-end",
        timer = 6000
      )
      warning("Could not retrieve data. Check internet connection.")
      return(NULL)
    }
  )

  schemes <- dplyr::arrange(
    dplyr::add_row(pubmlst_schemes, cgmlstorg_schemes),
    species
  )

  #### Screening environment ----

  # Clear screening file
  if (
    file.exists(file.path(app_local_share_path, "screening", "output_file.tsv"))
  ) {
    file.remove(file.path(app_local_share_path, "screening", "output_file.tsv"))
  }

  if (file.exists(file.path(app_local_share_path, "screening", "error.txt"))) {
    file.remove(file.path(app_local_share_path, "screening", "error.txt"))
  }

  #### Database environment ----

  # Load last used database if possible
  if (
    file.path(app_local_share_path, "last_db.rds") %in%
      dir_ls(file.path(app_local_share_path))
  ) {
    DB$last_db <- TRUE
  }

  observe({
    runjs(block_ui)

    shinyDirChoose(
      input,
      "db_location",
      roots = c(Home = path_home(), Root = "/"),
      defaultRoot = "Home",
      session = session
    )

    if (!is.null(Startup$select_new)) {
      if (isFALSE(Startup$select_new)) {
        if (isFALSE(Startup$block_db)) {
          req(input$db_location)

          Startup$database <- as.character(
            parseDirPath(
              roots = c(Home = path_home(), Root = "/"),
              input$db_location
            )
          )

          # Logical any local database present
          DB$exist <- (length(dir_ls(Startup$database)) == 0)

          # List of local schemes available
          available <- gsub("_", " ", basename(dir_ls(Startup$database)))
          DB$available <- available[
            available %in%
              gsub("_", " ", schemes$species)
          ]
        }
      } else if (isTRUE(Startup$select_new)) {
        if (length(DB$new_database)) {
          Startup$database <- paste0(DB$new_database, "/Database")
        } else {
          Startup$database <- ""
        }
      }
    } else {
      if (
        !is.null(DB$last_db) &&
          file.exists(file.path(app_local_share_path, "last_db.rds"))
      ) {
        last_db <- readRDS(file.path(app_local_share_path, "last_db.rds"))

        if (!is.null(last_db) && dir_exists(last_db)) {
          Startup$database <- last_db
          # Logical any local database present
          DB$exist <- (length(dir_ls(Startup$database)) == 0)

          # List of local schemes available
          available <- gsub("_", " ", basename(dir_ls(Startup$database)))
          DB$available <- available[
            available %in%
              gsub("_", " ", schemes$species)
          ]
        }
      }
    }

    runjs(unblock_ui)
  })

  ### Landing page UI ----

  # Hide sidebar on startup
  addClass(selector = "body", class = "sidebar-collapse")
  removeClass(selector = "body", class = "sidebar-toggle")

  # Show system time
  output$messageMenu <- renderText({
    HTML(format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z"))
  })

  observe({
    if (isFALSE(Startup$sidebar)) {
      removeClass(selector = "body", class = "sidebar-collapse")
      addClass(selector = "body", class = "sidebar-toggle")
    }
  })

  output$start_message <- renderUI(
    column(
      width = 12,
      align = "center",
      br(),
      br(),
      br(),
      br(),
      br(),
      br(),
      div(
        class = "image",
        imageOutput("imageOutput")
      ),
      br(),
      br(),
      br(),
      p(
        HTML(
          paste(tags$span(
            style = 'color: white; font-size: 16px;',
            paste0(
              'Proceed by loading a compatible local databa',
              'se or create a new one.'
            )
          ))
        )
      ),
      br(),
      fluidRow(
        column(
          width = 6,
          align = "right",
          shinyDirButton(
            "db_location",
            "Browse",
            icon = icon("folder-open"),
            title = "Locate the database folder",
            buttonType = "default",
            root = path_home()
          )
        ),
        column(
          width = 6,
          align = "left",
          shinyDirButton(
            "create_new_db",
            "Create New",
            icon = icon("plus"),
            title = "Choose location for new PhyloTrace database",
            buttonType = "default",
            root = path_home()
          )
        )
      ),
      br(),
      br(),
      fluidRow(
        column(
          width = 12,
          align = "center",
          useWaiter(),
          uiOutput("load_db"),
          br(),
          br(),
          br(),
          br(),
          br(),
          br(),
          br()
        )
      )
    )
  )

  # Load db & scheme selection UI
  output$load_db <- renderUI(
    if (!is.null(Startup$select_new)) {
      if (length(DB$new_database) > 0 && Startup$select_new) {
        column(
          width = 12,
          p(
            tags$span(
              style = 'color: white; font-size: 15px;',
              HTML(
                paste(
                  'New database will be created in',
                  DB$new_database
                )
              )
            )
          ),
          uiOutput("new_db_name_ui"),
          br(),
          br(),
          actionButton("load", "Create", class = "load-start")
        )
      } else if (length(DB$available) > 0 && !(Startup$select_new)) {
        if (
          sum(
            gsub(" ", "_", gsub(" (PM|CM)", "", DB$available)) %in%
              gsub("_(PM|CM)", "", schemes$species)
          ) ==
            0
        ) {
          column(
            width = 12,
            p(
              tags$span(
                style = 'color: white; font-size: 15px; font-style: italic;',
                HTML(
                  paste('Selected directory:', Startup$database)
                )
              )
            ),
            br(),
            p(
              HTML(
                paste(
                  tags$span(
                    style = paste0(
                      'color: #E18B00; font-size: 13px; font-styl',
                      'e: italic;'
                    ),
                    'Warning: Directory contains no valid elements'
                  )
                )
              )
            ),
            p(
              HTML(
                paste(
                  tags$span(
                    style = paste0(
                      'color: #E18B00; font-size: 13px; font-styl',
                      'e: italic;'
                    ),
                    paste0(
                      'Select a database directory containing compatible ',
                      'scheme folders.'
                    )
                  )
                )
              )
            ),
            br()
          )
        } else if (
          any(
            !(gsub(" ", "_", gsub(" (PM|CM)", "", DB$available)) %in%
              gsub("_(PM|CM)", "", schemes$species))
          )
        ) {
          column(
            width = 12,
            p(
              tags$span(
                style = 'color: white; font-size: 15px; font-style: italic;',
                HTML(
                  paste('Selected:', Startup$database)
                )
              )
            ),
            uiOutput("scheme_db"),
            br(),
            p(
              HTML(
                paste(
                  tags$span(
                    style = paste0(
                      'color: #E18B00; font-size: 13px; f',
                      'ont-style: italic;'
                    ),
                    'Warning: Folder contains invalid elements.'
                  )
                )
              )
            ),
            br(),
            actionButton(
              "load",
              "Load",
              class = "load-start"
            )
          )
        } else {
          column(
            width = 12,
            p(
              tags$span(
                style = 'color: white; font-size: 15px; font-style: italic;',
                HTML(
                  paste('Selected:', Startup$database)
                )
              )
            ),
            uiOutput("scheme_db"),
            br(),
            br(),
            actionButton(
              "load",
              "Load",
              class = "load-start"
            )
          )
        }
      } else {
        column(
          width = 12,
          p(
            tags$span(
              style = 'color: white; font-size: 15px; font-style: italic;',
              HTML(
                paste('Selected directory:', Startup$database)
              )
            )
          ),
          br(),
          p(
            HTML(
              paste(
                tags$span(
                  style = paste0(
                    'color: #E18B00; font-size: 13px; fon',
                    't-style: italic;'
                  ),
                  'Warning: Directory contains no valid elements'
                )
              )
            )
          ),
          p(
            HTML(
              paste(
                tags$span(
                  style = paste0(
                    'color: #E18B00; font-size: 13px; fon',
                    't-style: italic;'
                  ),
                  paste0(
                    'Select a database directory containing compa',
                    'tible scheme folders.'
                  )
                )
              )
            )
          ),
          br()
        )
      }
    } else if ((!is.null(DB$last_db)) && (!is.null(DB$available))) {
      if (isTRUE(DB$last_db) && (length(DB$available) > 0)) {
        if (
          sum(
            gsub(" ", "_", gsub(" (PM|CM)", "", DB$available)) %in%
              gsub("_(PM|CM)", "", schemes$species)
          ) ==
            0
        ) {
          column(
            width = 12,
            p(
              tags$span(
                style = 'color: white; font-size: 15px; font-style: italic;',
                HTML(
                  paste('Selected directory:', Startup$database)
                )
              )
            ),
            br(),
            p(
              HTML(
                paste(
                  tags$span(
                    style = paste0(
                      'color: #E18B00; font-size: 13px; f',
                      'ont-style: italic;'
                    ),
                    'Warning: Directory contains no valid elements'
                  )
                )
              )
            ),
            p(
              HTML(
                paste(
                  tags$span(
                    style = paste0(
                      'color: #E18B00; font-size: 13px; f',
                      'ont-style: italic;'
                    ),
                    paste0(
                      'Select a database containing compatible sc',
                      'heme folders.'
                    )
                  )
                )
              )
            ),
            br()
          )
        } else if (
          any(
            !(gsub(" ", "_", gsub(" (PM|CM)", "", DB$available)) %in%
              gsub("_(PM|CM)", "", schemes$species))
          )
        ) {
          column(
            width = 12,
            p(
              tags$span(
                style = 'color: white; font-size: 15px; font-style: italic;',
                HTML(
                  paste('Selected directory:', Startup$database)
                )
              )
            ),
            uiOutput("scheme_db"),
            br(),
            p(
              HTML(
                paste(
                  tags$span(
                    style = paste0(
                      'color: #E18B00; font-size: 13px; f',
                      'ont-style: italic;'
                    ),
                    'Warning: Folder contains invalid elements.'
                  )
                )
              )
            ),
            br(),
            actionButton(
              "load",
              "Load",
              class = "load-start"
            )
          )
        } else {
          column(
            width = 12,
            p(
              tags$span(
                style = 'color: white; font-size: 15px; font-style: italic;',
                HTML(
                  paste('Selected directory:', Startup$database)
                )
              )
            ),
            uiOutput("scheme_db"),
            br(),
            br(),
            actionButton(
              "load",
              "Load",
              class = "load-start"
            )
          )
        }
      } else if (isTRUE(DB$last_db) && (length(DB$available) == 0)) {
        column(
          width = 12,
          p(
            tags$span(
              style = 'color: white; font-size: 15px; font-style: italic;',
              HTML(
                paste('Selected directory:', Startup$database)
              )
            )
          ),
          br(),
          actionButton(
            "load",
            "Load",
            class = "load-start"
          )
        )
      }
    }
  )

  output$imageOutput <- renderImage(
    {
      image_path <- paste0(getwd(), "/www/PhyloTrace.png")
      list(src = image_path, height = 180)
    },
    deleteFile = FALSE
  )

  ### Landing page events ----

  # User selection new db or load db
  observeEvent(input$create_new_db, {
    log_print("Input create_new_db")
    Startup$select_new <- TRUE

    output$new_db_name_ui <- renderUI(
      textInput(
        "new_db_name",
        "",
        placeholder = "New database name"
      )
    )
  })

  observeEvent(input$db_location, {
    log_print("Input db_location")
    Startup$select_new <- FALSE
    output$new_db_name_ui <- NULL
  })

  ### Load app event ----

  observeEvent(input$load, {
    if (tail(readLogFile(), 1) != "0") {
      show_toast(
        title = "Pending Multi Typing",
        type = "warning",
        position = "bottom-end",
        timer = 6000
      )
    } else if (!is.null(Screening$status) && Screening$status == "started") {
      show_toast(
        title = "Pending Gene Screening",
        type = "warning",
        position = "bottom-end",
        timer = 6000
      )
    } else if (
      isTRUE(Startup$select_new) &&
        !is.null(input$new_db_name) &&
        nchar(input$new_db_name) < 1
    ) {
      show_toast(
        title = "Database name empty",
        type = "warning",
        position = "bottom-end",
        timer = 6000
      )
    } else if (
      isTRUE(Startup$select_new) &&
        !is.null(input$new_db_name) &&
        nchar(input$new_db_name) > 0 &&
        dir_exists(file.path(DB$new_database, input$new_db_name))
    ) {
      show_toast(
        title = "Name already exists in directory",
        type = "warning",
        position = "bottom-end",
        timer = 6000
      )
    } else {
      runjs(block_ui)

      output$start_message <- NULL
      output$load_db <- NULL

      addClass(selector = "body", class = "sidebar-collapse")
      removeClass(selector = "body", class = "sidebar-toggle")

      waiter_show(html = waiting_screen, color = "#384555")

      #### Reset reactive variables ----

      # reactive variables related to local database
      DB$data <- NULL
      DB$load_selected <- TRUE
      DB$no_na_switch <- FALSE
      DB$first_look <- FALSE
      DB$scheme <- NULL
      DB$check_new_entries <- NULL
      DB$meta_gs <- NULL
      DB$meta <- NULL
      DB$meta_true <- NULL
      DB$allelic_profile <- NULL
      DB$allelic_profile_trunc <- NULL
      DB$allelic_profile_true <- NULL
      # DB$scheme_new <- NULL
      DB$loci_info <- NULL
      DB$schemeinfo <- NULL
      DB$scheme_db <- NULL
      DB$scheme_link <- NULL
      DB$number_loci <- NULL
      DB$cluster_thresh <- NULL
      DB$cust_var <- data.frame()
      DB$change <- NULL
      DB$matrix_min <- NULL
      DB$matrix_max <- NULL
      DB$na_table <- NULL
      DB$url_link <- NULL
      if (is.null(DB$failCon)) {
        DB$failCon <- NULL
      }
      DB$inhibit_change <- NULL
      DB$count <- 0
      DB$deleted_entries <- NULL
      DB$remove_iso <- NULL
      DB$ham_matrix <- NULL
      DB$loci <- NULL
      DB$seq <- NULL

      # reactive variables related to typing process
      Typing$reload <- NULL
      Typing$multi_sel_table <- NULL
      Typing$pending <- FALSE
      Typing$multi_started <- FALSE
      Typing$multi_help <- FALSE
      Typing$last_success <- "0"
      Typing$last_failure <- "0"
      Typing$last_scheme <- NULL
      Typing$assembly_folder_path <- NULL
      Typing$files_filtered <- NULL
      Typing$assembly_files_path <- NULL
      Typing$result_list <- NULL
      Typing$multi_table_length <- NULL
      Typing$multi_result_status <- NULL
      Typing$last_scheme <- NULL
      Typing$status <- ""
      Typing$file_selection <- ""
      # Null typing progress trackers
      writeLines("0", file.path(logdir, "script_log.txt"))
      saveRDS(list(), file.path(app_local_share_path, "event_list.rds"))

      # reactive variables related to gene screening
      Screening$status <- "idle"
      Screening$picker_status <- TRUE
      Screening$first_result <- NULL
      Screening$status_df <- NULL
      Screening$choices <- NULL
      Screening$amr_results <- NULL
      Screening$amr_class <- NULL
      Screening$vir_class <- NULL
      Screening$hm_meta <- NULL
      Screening$res_profile <- NULL
      Screening$picker_choices <- NULL
      Screening$picker_selected <- NULL
      Screening$meta_df <- NULL

      # reactive variables related to visualization
      Vis$custom_label_nj <- data.frame()
      Vis$nj_label_pos_y <- list()
      Vis$nj_label_pos_x <- list()
      Vis$nj_label_size <- list()
      Vis$nj <- NULL
      Vis$mst_pre <- NULL
      Vis$title <- NULL
      Vis$nj_max_x <- NULL
      Vis$meta_nj <- NULL
      Vis$nj_parentnodes <- NULL
      Vis$branch_size_nj <- NULL
      Vis$tiplab_padding_nj <- NULL
      Vis$nodepointsize_nj <- NULL
      Vis$tippointsize_nj <- NULL
      Vis$labelsize_nj <- NULL
      Vis$nj_min_x <- NULL
      Vis$amr_nj <- NULL
      Vis$var_cols <- NULL
      Vis$meta_mst <- NULL
      Vis$unique_meta <- NULL
      Vis$nj_heatmap_select <- NULL
      Vis$nj_plot <- NULL
      Vis$nj_tree <- NULL

      # Reactive variables related to plot controls
      if (isTRUE(Vis$mst_true)) {
        mst_node_label_reactive("Assembly Name")
        mst_title_reactive("")
        mst_subtitle_reactive("")
        mst_color_var_reactive(FALSE)
        mst_col_var_reactive("Isolation Date")
        mst_col_scale_reactive("Viridis")
        mst_text_color_reactive("#000000")
        mst_color_node_reactive("#B2FACA")
        mst_color_edge_reactive("#000000")
        mst_edge_font_color_reactive("#000000")
        mst_background_color_reactive("#ffffff")
        mst_background_transparent_reactive(FALSE)
        mst_scale_nodes_reactive(TRUE)
        mst_node_scale_reactive(c(20, 40))
        mst_node_size_reactive(30)
        mst_scale_edges_reactive(TRUE)
        mst_edge_length_scale_reactive(15)
        mst_edge_length_reactive(35)
        mst_edge_font_size_reactive(18)
        mst_node_label_fontsize_reactive(14)
        mst_title_size_reactive(35)
        mst_subtitle_size_reactive(20)
        mst_ratio_reactive(16 / 10)
        mst_scale_reactive(600)
        mst_shadow_reactive(TRUE)
        mst_node_shape_reactive("dot")
        mst_show_clusters_reactive(FALSE)
        mst_cluster_col_scale_reactive("Viridis")
        mst_cluster_type_reactive("Area")
        mst_cluster_width_reactive(24)
        ifelse(
          !is.null(DB$cluster_thresh),
          mst_cluster_threshold_reactive(DB$cluster_thresh),
          mst_cluster_threshold_reactive(10)
        )
        mst_legend_ori_reactive("left")
        mst_font_size_reactive(18)
        mst_symbol_size_reactive(20)
      }

      if (isTRUE(Vis$nj_true)) {
        # Tip label
        Vis$nj_tiplab_val_reset <- TRUE
        nj_tiplab_val("Assembly Name")
        nj_tiplab_show_val(TRUE)
        Vis$nj_align_reset <- TRUE
        nj_align_val(FALSE)
        ifelse(
          !is.null(Vis$labelsize_nj),
          nj_tiplab_size_val(Vis$labelsize_nj),
          nj_tiplab_size_val(4)
        )
        nj_tiplab_fontface_val("plain")
        nj_tiplab_alpha_val(1)
        nj_tiplab_position_val(0)
        Vis$nj_tiplab_angle_reset <- TRUE
        nj_tiplab_angle_val(0)

        # Label panels
        nj_geom_val(FALSE)
        nj_tiplab_labelradius_val(0.2)
        ifelse(
          !is.null(Vis$tiplab_padding_nj),
          nj_tiplab_padding_val(Vis$tiplab_padding_nj),
          nj_tiplab_padding_val(0.2)
        )

        # Branch labels
        nj_show_branch_label_val(FALSE)
        ifelse(
          !is.null(Vis$branch_size_nj),
          nj_branch_size_val(Vis$branch_size_nj),
          nj_branch_size_val(4)
        )
        Vis$nj_branch_label_val_reset <- TRUE
        nj_branch_label_val("Host")
        nj_branchlab_alpha_val(0.65)
        nj_branch_x_val(0)
        nj_branchlab_fontface_val("plain")
        nj_branch_labelradius_val(0.5)

        # Titles
        nj_title_val(NULL)
        nj_title_size_val(30)
        nj_subtitle_val(NULL)
        nj_subtitle_size_val(30)

        # Custom label
        Vis$nj_label_pos_x <- list()
        Vis$nj_label_pos_y <- list()
        Vis$nj_label_size <- list()
        Vis$custom_label_nj <- data.frame()

        # Tip label mapping
        nj_mapping_show_val(FALSE)
        Vis$nj_color_mapping_val_reset <- TRUE
        nj_color_mapping_val("Country")
        Vis$nj_tiplab_scale_reset <- TRUE
        nj_tiplab_scale_val(nj_tiplab_scale_val_default())
        Vis$nj_color_mapping_div_mid_reset <- TRUE
        nj_color_mapping_div_mid_val("Mean")

        # Tip points mapping
        nj_tipcolor_mapping_show_val(FALSE)
        Vis$nj_tipcolor_mapping_val_reset <- TRUE
        nj_tipcolor_mapping_val("Country")
        Vis$nj_tippoint_scale_val_reset <- TRUE
        nj_tippoint_scale_val(nj_tippoint_scale_val_default())
        Vis$nj_tipcolor_mapping_div_mid_reset <- TRUE
        nj_tipcolor_mapping_div_mid_val("Mean")

        # Tip shape mapping
        nj_tipshape_mapping_show_val(FALSE)
        Vis$nj_tipshape_mapping_val_reset <- TRUE
        nj_tipshape_mapping_val("Host")

        # Tiles mapping
        nj_tiles_show_1_val(FALSE)
        nj_tiles_show_2_val(FALSE)
        nj_tiles_show_3_val(FALSE)
        nj_tiles_show_4_val(FALSE)
        nj_tiles_show_5_val(FALSE)
        Vis$nj_fruit_variable_val_reset <- TRUE
        nj_fruit_variable_val("Isolation Date")
        Vis$nj_fruit_variable_2_val_reset <- TRUE
        nj_fruit_variable_2_val("Isolation Date")
        Vis$nj_fruit_variable_3_val_reset <- TRUE
        nj_fruit_variable_3_val("Isolation Date")
        Vis$nj_fruit_variable_4_val_reset <- TRUE
        nj_fruit_variable_4_val("Isolation Date")
        Vis$nj_fruit_variable_5_val_reset <- TRUE
        nj_fruit_variable_5_val("Isolation Date")
        Vis$nj_tiles_scale_1_reset <- TRUE
        nj_tiles_scale_1_val(nj_tiles_scale_1_val_default())
        Vis$nj_tiles_scale_2_reset <- TRUE
        nj_tiles_scale_2_val(nj_tiles_scale_2_val_default())
        Vis$nj_tiles_scale_3_reset <- TRUE
        nj_tiles_scale_3_val(nj_tiles_scale_3_val_default())
        Vis$nj_tiles_scale_4_reset <- TRUE
        nj_tiles_scale_4_val(nj_tiles_scale_4_val_default())
        Vis$nj_tiles_scale_5_reset <- TRUE
        nj_tiles_scale_5_val(nj_tiles_scale_5_val_default())
        Vis$nj_tiles_mapping_div_mid_1_reset <- TRUE
        nj_tiles_mapping_div_mid_1_val("Mean")
        Vis$nj_tiles_mapping_div_mid_2_reset <- TRUE
        nj_tiles_mapping_div_mid_2_val("Mean")
        Vis$nj_tiles_mapping_div_mid_3_reset <- TRUE
        nj_tiles_mapping_div_mid_3_val("Mean")
        Vis$nj_tiles_mapping_div_mid_4_reset <- TRUE
        nj_tiles_mapping_div_mid_4_val("Mean")
        Vis$nj_tiles_mapping_div_mid_5_reset <- TRUE
        nj_tiles_mapping_div_mid_5_val("Mean")

        nj_heatmap_show_val(FALSE)
        Vis$nj_heatmap_select_val_reset <- TRUE
        nj_heatmap_select_val(NULL)
        Vis$nj_heatmap_scale_reset <- TRUE
        nj_heatmap_scale_val(nj_heatmap_scale_val_default())
        Vis$nj_heatmap_div_mid_val_reset <- TRUE
        nj_heatmap_div_mid_val("Mean")

        # Color values
        nj_color_val("#000000")
        nj_bg_val("#ffffff")
        nj_title_color_val("#000000")
        nj_tiplab_color_val("#000000")
        nj_tiplab_fill_val("#84D9A0")
        nj_branch_label_color_val("#FFB7B7")
        nj_tippoint_color_val("#3A4657")
        nj_nodepoint_color_val("#3A4657")

        # Tip points
        nj_tippoint_show_val(FALSE)
        Vis$nj_tippoint_shape_reset <- TRUE
        nj_tippoint_shape_val("circle")
        nj_tippoint_alpha_val(0.5)
        ifelse(
          !is.null(Vis$tippointsize_nj),
          nj_tippoint_size_val(Vis$tippointsize_nj),
          nj_tippoint_size_val(4)
        )

        # Node points
        nj_nodepoint_show_val(FALSE)
        nj_nodepoint_shape_val("circle")
        nj_nodepoint_alpha_val(1)
        ifelse(
          !is.null(Vis$nodepointsize_nj),
          nj_nodepoint_size_val(Vis$nodepointsize_nj),
          nj_nodepoint_size_val(2.5)
        )

        # Tiles
        nj_tile_number_val(1)
        nj_fruit_alpha_val(1)
        nj_fruit_alpha_2_val(1)
        nj_fruit_alpha_3_val(1)
        nj_fruit_alpha_4_val(1)
        nj_fruit_alpha_5_val(1)
        Vis$nj_fruit_width_circ_val_reset <- TRUE
        nj_fruit_width_circ_val(fruit_width())
        Vis$nj_fruit_width_circ_2_val_reset <- TRUE
        nj_fruit_width_circ_2_val(fruit_width())
        Vis$nj_fruit_width_circ_3_val_reset <- TRUE
        nj_fruit_width_circ_3_val(fruit_width())
        Vis$nj_fruit_width_circ_4_val_reset <- TRUE
        nj_fruit_width_circ_4_val(fruit_width())
        Vis$nj_fruit_width_circ_5_val_reset <- TRUE
        nj_fruit_width_circ_5_val(fruit_width())
        Vis$nj_fruit_offset_circ_reset <- TRUE
        nj_fruit_offset_circ_val(0.05)
        Vis$nj_fruit_offset_circ_2_reset <- TRUE
        nj_fruit_offset_circ_2_val(0.05)
        Vis$nj_fruit_offset_circ_3_reset <- TRUE
        nj_fruit_offset_circ_3_val(0.05)
        Vis$nj_fruit_offset_circ_4_reset <- TRUE
        nj_fruit_offset_circ_4_val(0.05)
        Vis$nj_fruit_offset_circ_5_reset <- TRUE
        nj_fruit_offset_circ_5_val(0.05)

        # Heatmap
        nj_heatmap_title_val("Heatmap")
        nj_colnames_angle_val(-90)
        Vis$nj_colnames_y_val_reset <- TRUE
        nj_colnames_y_val(-1)
        nj_heatmap_width_val(heatmap_width())
        Vis$nj_heatmap_offset_val_reset <- TRUE
        nj_heatmap_offset_val(0)

        # Clade highlights
        nj_nodelabel_show_val(FALSE)
        Vis$nj_parentnode_val_reset <- TRUE
        nj_parentnode_val("")
        nj_clade_scale_val(clade_highlight_color())
        nj_clade_type_val("roundrect")

        # Dimensions
        nj_ratio_val(c("16:10" = (16 / 10)))
        nj_v_val(0)
        nj_h_val(-0.05)
        nj_scale_val(670)
        nj_zoom_val(0.95)

        # Root tree
        nj_root_isolate_val("Automatic")

        # Layout
        nj_layout_val("rectangular")
        nj_rootedge_show_val(FALSE)
        Vis$nj_rootedge_length_val_reset <- TRUE
        ifelse(
          !is.null(Vis$nj_max_x),
          nj_rootedge_length_val(round(ceiling(Vis$nj_max_x) * 0.05)),
          nj_rootedge_length_val(2)
        )
        nj_rootedge_line_val("solid")
        Vis$nj_xlim_val_reset <- TRUE
        nj_xlim_val(-10)
        Vis$nj_xlim_inw_val_reset <- TRUE
        nj_xlim_inw_val(50)
        nj_treescale_show_val(FALSE)
        Vis$nj_treescale_width_val_reset <- TRUE
        ifelse(
          !is.null(Vis$nj_max_x),
          nj_treescale_width_val(round(ceiling(Vis$nj_max_x) * 0.1, 0)),
          nj_treescale_width_val(2)
        )
        Vis$nj_treescale_x_val_reset <- TRUE
        ifelse(
          !is.null(Vis$nj_max_x),
          nj_treescale_x_val(round(ceiling(Vis$nj_max_x) * 0.2, 0)),
          nj_treescale_x_val(2)
        )
        Vis$nj_treescale_y_val_reset <- TRUE
        nj_treescale_y_val(0)
        nj_ladder_val(TRUE)

        # Legend
        nj_legend_orientation_val("vertical")
        nj_legend_size_val(10)
        nj_legend_x_val(0.9)
        nj_legend_y_val(0.2)
      }

      # reactive variables related to report functions
      Report$report_list_mst <- NULL
      Report$report_list_nj <- NULL
      Report$report_df <- NULL

      # reactive variables related to scheme functions
      Scheme$link_scheme <- NULL
      Scheme$link_cgmlst <- NULL
      Scheme$link_targets <- NULL
      Scheme$folder_name <- NULL
      Scheme$species_data <- NULL

      ### Reset UI elements ----

      # Reset database tab UI elements
      output$hash_feedback <- NULL
      output$hashing_status <- NULL
      output$hash_import_button <- NULL
      output$hash_folderpath <- NULL
      output$hash_dir <- NULL
      output$import_metadata_sel <- NULL
      output$import_id_sel <- NULL
      output$import_feedback <- NULL
      output$table_missing_values <- NULL

      # Reset reactive screening variables
      output$screening_start <- NULL
      output$screening_result_sel <- NULL
      output$screening_result <- NULL
      output$screening_fail <- NULL
      output$multi_select_table <- NULL
      output$multi_select_tab_ctrls <- NULL
      Screening$status_df <- NULL
      Screening$choices <- NULL
      Screening$picker_status <- TRUE
      Screening$status <- "idle"
      Screening$first_result <- NULL
      if (!is.null(input$screening_select)) {
        if (!is.null(DB$data)) {
          updatePickerInput(
            session,
            "screening_select",
            selected = character(0)
          )
        }
      }

      # Empty tree plot fields
      output$tree_field <- NULL
      output$mst_field <- NULL
      log_print("Input load")

      # set typing start control variable
      Typing$reload <- TRUE

      ### Render status bar ----
      observe({
        req(DB$scheme)

        if (is.null(input$scheme_position)) {
          output$loaded_scheme <- renderUI({
            fluidRow(
              tags$li(
                class = "dropdown",
                tags$span(
                  HTML(
                    paste(
                      '<i class="fa-solid fa-layer-group"></i>',
                      "Selected scheme:&nbsp;&nbsp;&nbsp;<i>",
                      DB$scheme,
                      "</i>"
                    )
                  ),
                  style = "color:black;"
                )
              )
            )
          })
        }

        if (!is.null(input$scheme_position)) {
          output$loaded_scheme <- renderUI({
            fluidRow(
              tags$li(
                class = "dropdown",
                tags$span(
                  HTML(
                    paste(
                      '<i class="fa-solid fa-layer-group"></i>',
                      "Selected scheme:&nbsp;&nbsp;&nbsp;<i>",
                      DB$scheme,
                      "</i>"
                    )
                  ),
                  style = "color:black;"
                ),
                div(
                  class = "reload-bttn",
                  style = paste0(
                    "margin-left:",
                    30 + input$scheme_position,
                    "px; position: relative; top: -24px;"
                  ),
                  tipify(
                    actionButton(
                      "reload_db",
                      label = "",
                      icon = icon("rotate")
                    ),
                    title = "Change scheme",
                    options = list("delay': 400, 'foo" = "foo")
                  )
                )
              )
            )
          })
        }
      })

      observe({
        if (!is.null(Startup$database) && length(Startup$database) > 0) {
          if (nchar(Startup$database) > 35) {
            database <- paste0(
              substring(Startup$database, first = 1, last = 35),
              "..."
            )
          } else {
            database <- Startup$database
          }
          output$databasetext <- renderUI({
            fluidRow(
              tags$li(
                class = "dropdown",
                tags$span(
                  HTML(
                    paste(
                      '<i class="fa-solid fa-folder-open"></i>',
                      "Database:&nbsp;&nbsp;&nbsp;<i>",
                      database,
                      "</i>"
                    )
                  ),
                  style = "color:black;"
                )
              ),
              if (nchar(database) > 35) {
                bsTooltip(
                  "databasetext",
                  HTML(Startup$database),
                  placement = "bottom",
                  trigger = "hover"
                )
              }
            )
          })
        }
      })

      observe({
        if (!is.null(Startup$database)) {
          if (Typing$status == "Finalized") {
            output$statustext <- renderUI(
              fluidRow(
                tags$li(
                  class = "dropdown",
                  tags$span(
                    HTML(
                      paste0(
                        '<i class="fa-solid fa-circle-dot" style="color:lig',
                        'htgreen !important;"></i> Status:&nbsp;&nbsp;&nbsp',
                        '; <i>typing finalized</i>'
                      )
                    ),
                    style = "color:black;"
                  )
                )
              )
            )
          } else if (Typing$status == "Attaching") {
            output$statustext <- renderUI(
              fluidRow(
                tags$li(
                  class = "dropdown",
                  tags$span(
                    HTML(
                      paste0(
                        '<i class="fa-solid fa-circle-dot" style="color:ora',
                        'nge !important;"></i> Status:&nbsp;&nbsp;&nbsp; <i',
                        '>evaluating typing results</i>'
                      )
                    ),
                    style = "color:black;"
                  )
                )
              )
            )
          } else if (Typing$status == "Processing") {
            output$statustext <- renderUI(
              fluidRow(
                tags$li(
                  class = "dropdown",
                  tags$span(
                    HTML(
                      paste0(
                        '<i class="fa-solid fa-circle-dot" style="color:ora',
                        'nge !important;"></i> Status:&nbsp;&nbsp;&nbsp;<i> ',
                        'pending typing</i>'
                      )
                    ),
                    style = "color:black;"
                  )
                )
              )
            )
          } else if (Screening$status == "started") {
            output$statustext <- renderUI(
              fluidRow(
                tags$li(
                  class = "dropdown",
                  tags$span(
                    HTML(
                      paste0(
                        '<i class="fa-solid fa-circle-dot" style="color:ora',
                        'nge !important;"></i> Status:&nbsp;&nbsp;&nbsp;<i> ',
                        'pending gene screening</i>'
                      )
                    ),
                    style = "color:black;"
                  )
                )
              )
            )
          } else if (Screening$status == "finished") {
            output$statustext <- renderUI(
              fluidRow(
                tags$li(
                  class = "dropdown",
                  tags$span(
                    HTML(
                      paste0(
                        '<i class="fa-solid fa-circle-dot" style="color:lig',
                        'htgreen !important;"></i> Status:&nbsp;&nbsp;&nbsp;',
                        '<i> gene screening finalized</i>'
                      )
                    ),
                    style = "color:black;"
                  )
                )
              )
            )
          } else {
            output$statustext <- renderUI(
              fluidRow(
                tags$li(
                  class = "dropdown",
                  tags$span(
                    HTML(
                      paste0(
                        '<i class="fa-solid fa-circle-dot" style="color:lig',
                        'htgreen !important;"></i> Status:&nbsp;&nbsp;&nbsp;',
                        '<i>ready</i>'
                      )
                    ),
                    style = "color:black;"
                  )
                )
              )
            )
          }
        }
      })

      runjs(
        paste0(
          'if(document.querySelector("#loaded_scheme > div > li > span") ',
          '!== null) {',
          '// Select the span element',
          'let spanElement = document.querySelector("#loaded_scheme',
          ' > div > li > span");',
          '// Get the bounding rectangle of the span element',
          'let rect = spanElement.getBoundingClientRect();',
          '// Extract the width',
          'let width = rect.width;',
          'Shiny.setInputValue("scheme_position", width);',
          '}'
        )
      )

      # Load app elements based on database availability and missing value presence
      if (isTRUE(Startup$select_new) && !is.null(DB$new_database)) {
        if (
          Startup$select_new ||
            (isFALSE(Startup$select_new) && is.null(input$scheme_db))
        ) {
          log_print(paste0("New database created in ", DB$new_database))

          DB$check_new_entries <- TRUE
          DB$data <- NULL
          DB$meta_gs <- NULL
          DB$meta <- NULL
          DB$meta_true <- NULL
          DB$allelic_profile <- NULL
          DB$allelic_profile_trunc <- NULL
          DB$allelic_profile_true <- NULL

          # null Distance matrix, entry table and plots
          output$db_distancematrix <- NULL
          output$db_entries_table <- NULL
          output$tree_mst <- NULL
          output$tree_plot <- NULL

          # null report values
          Report$report_list_mst <- list()
          Report$report_list_nj <- list()

          # null plots
          Vis$nj <- NULL
          Vis$mst_pre <- NULL

          removeModal()

          ### Render Menu Items ----

          Startup$sidebar <- FALSE

          output$menu_header_typing <- NULL
          output$menu_header_screening <- NULL

          # Hide start message
          output$start_message <- NULL

          DB$load_selected <- FALSE

          # Declare database path
          Startup$database <- file.path(DB$new_database, input$new_db_name)

          # Set database availability screening variables to present database
          Startup$block_db <- TRUE
          Startup$select_new <- FALSE

          # Render menu with Manage Schemes as start tab and no Missing values tab
          output$menu_typing <- renderMenu(
            sidebarMenu(
              menuItem(
                text = "Schemes",
                tabName = "init",
                icon = icon("layer-group"),
                selected = TRUE
              )
            )
          )

          # Show message that loci files are missing
          showModal(
            div(
              class = "start-modal",
              modalDialog(
                fluidRow(
                  br(),
                  column(
                    width = 11,
                    p(
                      HTML(
                        paste0(
                          '<span style="color: white; display: block; font-s',
                          'ize: 15px; margin-left: 15px; display: block;">',
                          "Download a cgMLST scheme to add a new folder in t",
                          "he database directory. Multiple schemes can be do",
                          "wnloaded and included in one database.",
                          '</span>'
                        )
                      )
                    )
                  ),
                  br()
                ),
                title = paste("Set Up New Database"),
                fade = TRUE,
                easyClose = TRUE,
                footer = tagList(
                  modalButton("Okay")
                )
              )
            )
          )

          # Dont render these elements
          output$db_no_entries <- NULL
          output$distancematrix_no_entries <- NULL
          output$db_entries <- NULL
          output$edit_index <- NULL
          output$edit_scheme_d <- NULL
          output$edit_entries <- NULL
          output$compare_select <- NULL
          output$delete_select <- NULL
          output$del_bttn <- NULL
          output$compare_allele_box <- NULL
          output$download_entries <- NULL
          output$missing_values <- NULL
          output$custom_var_box <- NULL
          output$delete_box <- NULL
          output$missing_values_sidebar <- NULL
          output$download_scheme_info <- NULL
          output$download_loci <- NULL
          output$entry_table_controls <- NULL
          output$multi_stop <- NULL
          output$metadata_multi_box <- NULL
          output$start_multi_typing_ui <- NULL
          output$pending_typing <- NULL
          output$multi_typing_results <- NULL
          output$single_typing_progress <- NULL
          output$metadata_single_box <- NULL
          output$start_typing_ui <- NULL
        }
      } else {
        log_print(paste0(
          "Loading existing ",
          input$scheme_db,
          " database from ",
          Startup$database
        ))
      }

      if (isTRUE(DB$load_selected)) {
        #Check if selected scheme valid
        if (
          gsub(" ", "_", gsub(" (PM|CM)", "", input$scheme_db)) %in%
            gsub("_(PM|CM)", "", schemes$species)
        ) {
          # Save database path for next start
          saveRDS(
            Startup$database,
            file.path(app_local_share_path, "last_db.rds")
          )

          DB$check_new_entries <- TRUE
          DB$data <- NULL
          DB$meta_gs <- NULL
          DB$meta <- NULL
          DB$meta_true <- NULL
          DB$allelic_profile <- NULL
          DB$allelic_profile_trunc <- NULL
          DB$allelic_profile_true <- NULL

          if (!identical(DB$scheme, input$scheme_db)) {
            DB$scheme <- input$scheme_db
            DB$scheme_new <- TRUE
          }

          # Load AMR profile
          profile_path <- file.path(
            Startup$database,
            gsub(" ", "_", DB$scheme),
            "AMR_Profile.rds"
          )
          if (file.exists(profile_path)) {
            amr_profile <- readRDS(profile_path)
            Screening$amr_results <- amr_profile$results
            Screening$amr_class <- amr_profile$AMR_classification
            Screening$vir_class <- amr_profile$virulence_classification
          }

          # null Distance matrix, entry table and plots
          output$db_distancematrix <- NULL
          output$db_entries_table <- NULL
          output$tree_mst <- NULL
          output$tree_plot <- NULL

          # null typing initiation UI
          output$multi_stop <- NULL
          output$metadata_multi_box <- NULL
          output$start_multi_typing_ui <- NULL
          output$pending_typing <- NULL
          output$multi_typing_results <- NULL
          output$single_typing_progress <- NULL
          output$metadata_single_box <- NULL
          output$start_typing_ui <- NULL

          # null report values
          Report$report_list_mst <- list()
          Report$report_list_nj <- list()

          # null plots
          Vis$nj <- NULL
          Vis$mst_pre <- NULL

          removeModal()

          Startup$sidebar <- FALSE

          output$menu_header_typing <- renderUI(
            div(
              class = "menu-header-typing",
              HTML(
                paste(
                  tags$span(
                    style = paste0(
                      "color: white; font-size: 14px; pos",
                      "ition: relative; top: 8px; margin-",
                      "left: 10px;"
                    ),
                    "cgMLST Typing"
                  )
                )
              )
            )
          )

          output$menu_header_screening <- renderUI(
            div(
              class = "menu-header-screening",
              HTML(
                paste(
                  tags$span(
                    style = paste0(
                      "color: white; font-size: 14px; position: r",
                      "elative; top: 8px; margin-left: 10px;"
                    ),
                    "Locus Screening"
                  )
                )
              )
            )
          )

          # Hide start message
          output$start_message <- NULL

          if (any(grepl(gsub(" ", "_", DB$scheme), dir_ls(Startup$database)))) {
            if (
              !any(grepl(
                "alleles",
                dir_ls(paste0(
                  Startup$database,
                  "/",
                  gsub(" ", "_", DB$scheme)
                ))
              ))
            ) {
              log_print("Missing loci files")

              # Show message that loci files are missing
              showModal(
                div(
                  class = "start-modal",
                  modalDialog(
                    fluidRow(
                      br(),
                      column(
                        width = 11,
                        p(
                          HTML(
                            paste0(
                              '<span style="color: white; font-size: 15px; mar',
                              'gin-left: 15px; display: block;">',
                              "No loci files are present in the local ",
                              DB$scheme,
                              " folder. Download the scheme again (no influenc",
                              "e on already typed assemblies).",
                              '</span>'
                            )
                          )
                        )
                      ),
                      br()
                    ),
                    title = "Local Database Error",
                    fade = TRUE,
                    easyClose = TRUE,
                    footer = tagList(
                      modalButton("Okay")
                    )
                  )
                )
              )

              # Render menu with Manage Schemes as start tab
              output$menu_typing <- renderMenu(
                sidebarMenu(
                  menuItem(
                    text = "Database",
                    tabName = "database",
                    icon = icon("hard-drive"),
                    startExpanded = TRUE,
                    menuSubItem(
                      text = "Browse Entries",
                      tabName = "db_browse_entries"
                    ),
                    menuSubItem(
                      text = "Scheme Info",
                      tabName = "db_schemeinfo"
                    ),
                    menuSubItem(
                      text = "Loci Info",
                      tabName = "db_loci_info"
                    ),
                    menuSubItem(
                      text = "Distance Matrix",
                      tabName = "db_distmatrix"
                    ),
                    if (!is.null(DB$allelic_profile)) {
                      if (anyNA(DB$allelic_profile)) {
                        menuSubItem(
                          text = "Missing Values",
                          tabName = "db_missing_values",
                          icon = icon("triangle-exclamation")
                        )
                      }
                    }
                  ),
                  menuItem(
                    text = "Schemes",
                    tabName = "init",
                    icon = icon("layer-group"),
                    selected = TRUE
                  ),
                  menuItem(
                    text = "Allelic Typing",
                    tabName = "typing",
                    icon = icon("gears")
                  ),
                  menuItem(
                    text = "Visualization",
                    tabName = "visualization",
                    icon = icon("circle-nodes")
                  )
                )
              )

              if (!is.null(DB$scheme)) {
                amrfinder_available <- check.amrfinder.available(
                  selected_scheme = DB$scheme,
                  amrfinder_species = amrfinder_species
                )

                if (!isFALSE(amrfinder_available)) {
                  output$menu_screening <- renderMenu(screening_menu_available)
                } else {
                  output$menu_screening <- renderMenu(screening_menu_available)
                }
              }
            } else if (
              !file.exists(file.path(
                Startup$database,
                gsub(" ", "_", DB$scheme),
                "scheme_info.rds"
              ))
            ) {
              output$download_scheme_info <- NULL

              log_print("Scheme info file missing")

              # Show message that scheme info is missing
              showModal(
                div(
                  class = "start-modal",
                  modalDialog(
                    fluidRow(
                      br(),
                      column(
                        width = 11,
                        p(
                          HTML(
                            paste0(
                              '<span style="color: white; font-size: 15px; dis',
                              'play: block; margin-left: 15px;">',
                              "Scheme info of the local ",
                              DB$scheme,
                              " database is missing. Download the scheme again",
                              " (no influence on already typed assemblies).",
                              '</span>'
                            )
                          )
                        )
                      ),
                      br()
                    ),
                    title = "Local Database Error",
                    fade = TRUE,
                    easyClose = TRUE,
                    footer = tagList(
                      modalButton("Okay")
                    )
                  )
                )
              )

              # Render menu with Manage Schemes as start tab
              output$menu_typing <- renderMenu(
                sidebarMenu(
                  menuItem(
                    text = "Database",
                    tabName = "database",
                    icon = icon("hard-drive"),
                    startExpanded = TRUE,
                    menuSubItem(
                      text = "Browse Entries",
                      tabName = "db_browse_entries"
                    ),
                    menuSubItem(
                      text = "Scheme Info",
                      tabName = "db_schemeinfo"
                    ),
                    menuSubItem(
                      text = "Loci Info",
                      tabName = "db_loci_info"
                    ),
                    menuSubItem(
                      text = "Distance Matrix",
                      tabName = "db_distmatrix"
                    ),
                    if (!is.null(DB$allelic_profile)) {
                      if (anyNA(DB$allelic_profile)) {
                        menuSubItem(
                          text = "Missing Values",
                          tabName = "db_missing_values",
                          icon = icon("triangle-exclamation")
                        )
                      }
                    }
                  ),
                  menuItem(
                    text = "Schemes",
                    tabName = "init",
                    icon = icon("layer-group"),
                    selected = TRUE
                  ),
                  menuItem(
                    text = "Allelic Typing",
                    tabName = "typing",
                    icon = icon("gears")
                  ),
                  menuItem(
                    text = "Visualization",
                    tabName = "visualization",
                    icon = icon("circle-nodes")
                  )
                )
              )

              if (!is.null(DB$scheme)) {
                amrfinder_available <- check.amrfinder.available(
                  selected_scheme = DB$scheme,
                  amrfinder_species = amrfinder_species
                )

                if (!isFALSE(amrfinder_available)) {
                  output$menu_screening <- renderMenu(screening_menu_available)
                } else {
                  output$menu_screening <- renderMenu(screening_menu_available)
                }
              }
            } else {
              # Produce Loci Info Table
              if (
                file.exists(file.path(
                  Startup$database,
                  gsub(" ", "_", DB$scheme),
                  "targets.csv"
                ))
              ) {
                DB$loci_info <- read.csv(
                  file.path(
                    Startup$database,
                    gsub(" ", "_", DB$scheme),
                    "targets.csv"
                  ),
                  header = TRUE,
                  sep = "\t",
                  row.names = NULL,
                  colClasses = c(
                    "NULL",
                    "character",
                    "character",
                    "integer",
                    "integer",
                    "character",
                    "integer",
                    "NULL"
                  )
                )
              } else {
                DB$loci_info <- NULL
              }

              # Produce Scheme Info Table
              if (
                file.exists(file.path(
                  Startup$database,
                  gsub(" ", "_", DB$scheme),
                  "scheme_info.rds"
                ))
              ) {
                DB$schemeinfo <- readRDS(file.path(
                  Startup$database,
                  gsub(" ", "_", DB$scheme),
                  "scheme_info.rds"
                ))

                # Get scheme database link
                DB$scheme_db <-
                  DB$schemeinfo[, 2][DB$schemeinfo[, 1] == "Database"]

                # Get scheme database link
                DB$scheme_link <- str_extract(
                  DB$schemeinfo[, 2][DB$schemeinfo[, 1] == "URL"],
                  '(?<=href=")[^"]+'
                )

                # Get locus count
                number_loci <-
                  DB$schemeinfo[, 2][DB$schemeinfo[, 1] == "Locus Count"]

                if (DB$scheme_db == "cgMLST.org Nomenclature Server (h25)") {
                  # Locus count
                  DB$number_loci <- as.numeric(gsub(",", "", number_loci))

                  # Cluster threshold
                  DB$cluster_thresh <-
                    DB$schemeinfo[, 2][
                      DB$schemeinfo[, 1] == "Complex Type Distance"
                    ]
                } else if (DB$scheme_db == "pubMLST") {
                  # Locus count
                  DB$number_loci <- as.numeric(number_loci)

                  # Cluster threshold
                  DB$cluster_thresh <- 10
                }
              } else {
                log_print(paste0(
                  "Scheme info file missing in the local ",
                  DB$scheme,
                  " folder"
                ))

                # Show message that loci files are missing
                showModal(
                  div(
                    class = "start-modal",
                    modalDialog(
                      fluidRow(
                        br(),
                        column(
                          width = 11,
                          p(
                            HTML(
                              paste0(
                                '<span style="color: white; display: block; fo',
                                'nt-size: 15px; margin-left: 15px;">',
                                "Scheme info file is missing in the local ",
                                DB$scheme,
                                " folder. Download the scheme again (no influe",
                                "nce on already typed assemblies).",
                                '</span>'
                              )
                            )
                          )
                        ),
                        br()
                      ),
                      title = "Local Database Error",
                      fade = TRUE,
                      easyClose = TRUE,
                      footer = tagList(
                        modalButton("Okay")
                      )
                    )
                  )
                )
              }

              # Check if number of loci/fastq-files of alleles is coherent with number of targets in scheme
              if (
                DB$number_loci >
                  length(
                    dir_ls(paste0(
                      Startup$database,
                      "/",
                      gsub(" ", "_", DB$scheme),
                      "/",
                      gsub(" ", "_", DB$scheme),
                      "_alleles"
                    ))
                  )
              ) {
                log_print(paste0(
                  "Loci files are missing in the local ",
                  DB$scheme,
                  " folder"
                ))

                # Show message that loci files are missing
                showModal(
                  div(
                    class = "start-modal",
                    modalDialog(
                      fluidRow(
                        br(),
                        column(
                          width = 11,
                          p(
                            HTML(
                              paste0(
                                '<span style="color: white; display: block; fo',
                                'nt-size: 15px; margin-left: 15px;">',
                                "Some loci files are missing in the local ",
                                DB$scheme,
                                " folder. Download the scheme again (no influe",
                                "nce on already typed assemblies).",
                                '</span>'
                              )
                            )
                          )
                        ),
                        br()
                      ),
                      title = "Local Database Error",
                      fade = TRUE,
                      easyClose = TRUE,
                      footer = tagList(
                        modalButton("Okay")
                      )
                    )
                  )
                )

                # Render menu with Manage Schemes as start tab
                output$menu_typing <- renderMenu(
                  sidebarMenu(
                    menuItem(
                      text = "Database",
                      tabName = "database",
                      icon = icon("hard-drive"),
                      startExpanded = TRUE,
                      menuSubItem(
                        text = "Browse Entries",
                        tabName = "db_browse_entries"
                      ),
                      menuSubItem(
                        text = "Scheme Info",
                        tabName = "db_schemeinfo"
                      ),
                      menuSubItem(
                        text = "Loci Info",
                        tabName = "db_loci_info"
                      ),
                      menuSubItem(
                        text = "Distance Matrix",
                        tabName = "db_distmatrix"
                      ),
                      if (!is.null(DB$allelic_profile)) {
                        if (anyNA(DB$allelic_profile)) {
                          menuSubItem(
                            text = "Missing Values",
                            tabName = "db_missing_values",
                            icon = icon("triangle-exclamation")
                          )
                        }
                      }
                    ),
                    menuItem(
                      text = "Schemes",
                      tabName = "init",
                      icon = icon("layer-group"),
                      selected = TRUE
                    ),
                    menuItem(
                      text = "Allelic Typing",
                      tabName = "typing",
                      icon = icon("gears")
                    ),
                    menuItem(
                      text = "Visualization",
                      tabName = "visualization",
                      icon = icon("circle-nodes")
                    )
                  )
                )

                if (!is.null(DB$scheme)) {
                  amrfinder_available <- check.amrfinder.available(
                    selected_scheme = DB$scheme,
                    amrfinder_species = amrfinder_species
                  )

                  if (!isFALSE(amrfinder_available)) {
                    output$menu_screening <- renderMenu(
                      screening_menu_available
                    )
                  } else {
                    output$menu_screening <- renderMenu(
                      screening_menu_available
                    )
                  }
                }
              } else {
                ###### All checks passed - load database
                # If typed entries present
                if (
                  any(grepl(
                    "Typing.rds",
                    dir_ls(paste0(
                      Startup$database,
                      "/",
                      gsub(" ", "_", DB$scheme)
                    ))
                  ))
                ) {
                  # Load database from files
                  Database <- readRDS(file.path(
                    Startup$database,
                    gsub(" ", "_", DB$scheme),
                    "Typing.rds"
                  ))

                  # Databases produced with version < 1.6.1 receive extra column
                  if (
                    !any(colnames(Database[["Typing"]])[1:14] == "Database")
                  ) {
                    Database[["Typing"]] <- add_column(
                      Database[["Typing"]],
                      Database = basename(Startup$database),
                      .before = "Scheme"
                    )
                  }

                  # Renaming of 'Typing Date' column to 'Entry Date'
                  if (
                    any(
                      colnames(Database[["Typing"]])[1:14] == "Typing Date"
                    )
                  ) {
                    date_col <- which(
                      colnames(Database[["Typing"]])[1:14] == "Typing Date"
                    )
                    colnames(Database[["Typing"]])[date_col] <- "Entry Date"
                  }

                  # Save changes
                  saveRDS(
                    Database,
                    file.path(
                      Startup$database,
                      gsub(" ", "_", DB$scheme),
                      "Typing.rds"
                    )
                  )

                  DB$data <- Database[["Typing"]]

                  if ((ncol(DB$data) - 14) != DB$number_loci) {
                    cust_var <- select(
                      DB$data,
                      15:(ncol(DB$data) - DB$number_loci)
                    )
                    DB$cust_var <- data.frame(
                      Variable = names(cust_var),
                      Type = column_classes(cust_var)
                    )
                  } else {
                    DB$cust_var <- data.frame()
                  }

                  DB$change <- FALSE
                  DB$meta_gs <- select(DB$data, c(1, 3:14))
                  DB$meta <- select(DB$data, 1:(14 + nrow(DB$cust_var)))
                  DB$meta_true <- DB$meta[which(DB$data$Include == TRUE), ]
                  DB$allelic_profile <- select(
                    DB$data,
                    -(1:(14 + nrow(DB$cust_var)))
                  )
                  DB$allelic_profile_trunc <- as.data.frame(
                    lapply(DB$allelic_profile, function(x) sapply(x, truncHash))
                  )
                  DB$allelic_profile_true <- DB$allelic_profile[
                    which(
                      DB$data$Include == TRUE
                    ),
                  ]

                  # Reset other reactive typing variables
                  output$single_typing_progress <- NULL
                  output$typing_fin <- NULL
                  output$single_typing_results <- NULL
                  output$typing_formatting <- NULL

                  # Check need for new missing value display
                  if (isTRUE(Startup$first_look)) {
                    if (sum(apply(DB$data, 1, anyNA)) >= 1) {
                      DB$no_na_switch <- TRUE
                    } else {
                      DB$no_na_switch <- FALSE
                    }
                  }

                  Startup$first_look <- TRUE

                  output$initiate_typing_ui <- renderUI({
                    column(
                      width = 3,
                      align = "center",
                      br(),
                      br(),
                      h3(
                        p("Initiate Typing"),
                        style = "color:white; margin-left: 15px"
                      ),
                      br(),
                      br(),
                      p(
                        HTML(
                          paste(
                            tags$span(
                              style = paste0(
                                'color: white; font-size: 15px; ',
                                'margin-bottom: 0px; margin-left: 15px'
                              ),
                              'Select Assembly File'
                            )
                          )
                        )
                      ),
                      fluidRow(
                        column(1),
                        column(
                          width = 11,
                          align = "center",
                          shinyFilesButton(
                            "genome_file",
                            "Browse",
                            icon = icon("file"),
                            title = paste0(
                              "Select the assembly in .fasta/.fna",
                              "/.fa format:"
                            ),
                            multiple = FALSE,
                            buttonType = "default",
                            class = NULL,
                            root = path_home()
                          ),
                          br(),
                          br(),
                          uiOutput("genome_path"),
                          br()
                        )
                      )
                    )
                  })

                  output$initiate_multi_typing_ui <- initiate_multi_typing_ui

                  if (!anyNA(DB$allelic_profile)) {
                    # no NA's -> dont render missing values sidebar elements
                    output$missing_values_sidebar <- NULL

                    # Render menu if no NA's present
                    output$menu_typing <- renderMenu(
                      sidebarMenu(
                        menuItem(
                          text = "Database",
                          tabName = "database",
                          icon = icon("hard-drive"),
                          startExpanded = TRUE,
                          menuSubItem(
                            text = "Browse Entries",
                            tabName = "db_browse_entries"
                          ),
                          menuSubItem(
                            text = "Scheme Info",
                            tabName = "db_schemeinfo"
                          ),
                          menuSubItem(
                            text = "Loci Info",
                            tabName = "db_loci_info"
                          ),
                          menuSubItem(
                            text = "Distance Matrix",
                            tabName = "db_distmatrix"
                          )
                        ),
                        menuItem(
                          text = "Schemes",
                          tabName = "init",
                          icon = icon("layer-group")
                        ),
                        menuItem(
                          text = "Allelic Typing",
                          tabName = "typing",
                          icon = icon("gears")
                        ),
                        menuItem(
                          text = "Visualization",
                          tabName = "visualization",
                          icon = icon("circle-nodes")
                        )
                      )
                    )

                    if (!is.null(DB$scheme)) {
                      amrfinder_available <- check.amrfinder.available(
                        selected_scheme = DB$scheme,
                        amrfinder_species = amrfinder_species
                      )

                      if (!isFALSE(amrfinder_available)) {
                        output$menu_screening <- renderMenu(
                          screening_menu_available
                        )
                      } else {
                        output$menu_screening <- renderMenu(
                          screening_menu_available
                        )
                      }
                    }
                  } else {
                    output$menu_typing <- renderMenu(
                      sidebarMenu(
                        menuItem(
                          text = "Database",
                          tabName = "database",
                          icon = icon("hard-drive"),
                          startExpanded = TRUE,
                          menuSubItem(
                            text = "Browse Entries",
                            tabName = "db_browse_entries",
                            selected = TRUE
                          ),
                          menuSubItem(
                            text = "Scheme Info",
                            tabName = "db_schemeinfo"
                          ),
                          menuSubItem(
                            text = "Loci Info",
                            tabName = "db_loci_info"
                          ),
                          menuSubItem(
                            text = "Distance Matrix",
                            tabName = "db_distmatrix"
                          ),
                          menuSubItem(
                            text = "Missing Values",
                            tabName = "db_missing_values",
                            icon = icon("triangle-exclamation")
                          )
                        ),
                        menuItem(
                          text = "Schemes",
                          tabName = "init",
                          icon = icon("layer-group")
                        ),
                        menuItem(
                          text = "Allelic Typing",
                          tabName = "typing",
                          icon = icon("gears")
                        ),
                        menuItem(
                          text = "Visualization",
                          tabName = "visualization",
                          icon = icon("circle-nodes")
                        )
                      )
                    )

                    if (!is.null(DB$scheme)) {
                      amrfinder_available <- check.amrfinder.available(
                        selected_scheme = DB$scheme,
                        amrfinder_species = amrfinder_species
                      )

                      if (!isFALSE(amrfinder_available)) {
                        output$menu_screening <- renderMenu(
                          screening_menu_available
                        )
                      } else {
                        output$menu_screening <- renderMenu(
                          screening_menu_available
                        )
                      }
                    }
                  }

                  # Render custom variable display
                  output$show_cust_var <- renderDataTable(
                    DB$cust_var,
                    selection = "single",
                    rownames = FALSE,
                    options = list(
                      scrollY = TRUE,
                      pageLength = 10,
                      columnDefs = list(list(
                        searchable = TRUE,
                        targets = "_all"
                      ))
                    )
                  )

                  # Render missing values sidebar elements
                  output$missing_values_sidebar <- renderUI({
                    column(
                      width = 12,
                      fluidRow(
                        column(
                          width = 12,
                          br(),
                          materialSwitch(
                            "miss_val_height",
                            h5(
                              p("Show Full Table"),
                              style = paste0(
                                "color:white; padding-left: 0px;",
                                " position: relative; top: -4px;",
                                " right: -5px;"
                              )
                            ),
                            value = FALSE,
                            right = TRUE
                          )
                        ),
                        br()
                      )
                    )
                  })

                  # Render scheme info download button
                  output$download_loci <- renderUI({
                    if (!is.null(DB$loci_info)) {
                      column(
                        12,
                        downloadBttn(
                          "download_loci_info",
                          style = "simple",
                          label = "",
                          size = "sm",
                          icon = icon("download"),
                          color = "primary"
                        ),
                        bsTooltip(
                          "download_loci_info_bttn",
                          HTML(
                            "Save loci information <br> (without sequence)"
                          ),
                          placement = "bottom",
                          trigger = "hover"
                        )
                      )
                    } else {
                      NULL
                    }
                  })

                  # Render scheme info download button
                  output$download_scheme_info <- renderUI({
                    column(
                      12,
                      downloadBttn(
                        "download_schemeinfo",
                        style = "simple",
                        label = "",
                        size = "sm",
                        icon = icon("download"),
                        color = "primary"
                      ),
                      bsTooltip(
                        "download_schemeinfo_bttn",
                        HTML("Save scheme information"),
                        placement = "bottom",
                        trigger = "hover"
                      )
                    )
                  })

                  # Render select input to choose displayed loci
                  output$compare_select <- renderUI({
                    if (nrow(DB$data) == 1) {
                      HTML(
                        paste(
                          tags$span(
                            style = 'color: white; font-size: 15px;',
                            "Type at least two assemblies to compare"
                          )
                        )
                      )
                    } else {
                      if (!is.null(input$compare_difference)) {
                        if (isFALSE(input$compare_difference)) {
                          div(
                            class = "compare-select",
                            pickerInput(
                              inputId = "compare_select",
                              label = "",
                              width = "85%",
                              choices = names(DB$allelic_profile),
                              selected = names(DB$allelic_profile)[1:20],
                              options = list(
                                `live-search` = TRUE,
                                `actions-box` = TRUE,
                                size = 10,
                                style = paste0(
                                  "background-color: white; borde",
                                  "r-radius: 5px;"
                                )
                              ),
                              multiple = TRUE
                            )
                          )
                        } else {
                          div(
                            class = "compare-select",
                            pickerInput(
                              inputId = "compare_select",
                              label = "",
                              width = "85%",
                              choices = names(DB$allelic_profile),
                              selected = names(
                                DB$allelic_profile
                              )[var_alleles(
                                DB$allelic_profile
                              )],
                              options = list(
                                `live-search` = TRUE,
                                `actions-box` = TRUE,
                                size = 10,
                                style = paste0(
                                  "background-color: white; borde",
                                  "r-radius: 5px;"
                                )
                              ),
                              multiple = TRUE
                            )
                          )
                        }
                      }
                    }
                  })

                  ##### Render Entry Data Table ----
                  output$db_entries_table <- renderUI({
                    if (!is.null(DB$data)) {
                      if (between(nrow(DB$data), 1, 30)) {
                        fluidRow(
                          column(
                            width = 12,
                            rHandsontableOutput("db_entries")
                          ),
                          br(),
                          br(),
                          column(
                            width = 3,
                            br(),
                            fluidRow(
                              column(
                                width = 3,
                                div(
                                  class = "rectangle-blue"
                                )
                              ),
                              column(
                                width = 7,
                                p(
                                  HTML(
                                    paste(
                                      tags$span(
                                        style = paste0(
                                          "color: white; font-size: 12px; posi",
                                          "tion: relative; bottom: -10px"
                                        ),
                                        " = included for analyses"
                                      )
                                    )
                                  )
                                )
                              )
                            )
                          ),
                          column(
                            width = 3,
                            br(),
                            fluidRow(
                              column(
                                width = 3,
                                div(
                                  class = "rectangle-orange"
                                )
                              ),
                              column(
                                width = 7,
                                p(
                                  HTML(
                                    paste(
                                      tags$span(
                                        style = paste0(
                                          "color: white; font-size: 12px; posi",
                                          "tion: relative; bottom: -10px; marg",
                                          "in-left: -45px"
                                        ),
                                        " =  duplicated assembly name"
                                      )
                                    )
                                  )
                                )
                              )
                            )
                          ),
                          column(
                            width = 3,
                            br(),
                            fluidRow(
                              column(
                                width = 3,
                                div(
                                  class = "rectangle-red"
                                )
                              ),
                              column(
                                width = 7,
                                p(
                                  HTML(
                                    paste(
                                      tags$span(
                                        style = paste0(
                                          "color: white; font-size: 12px; posi",
                                          "tion: relative; bottom: -10px; marg",
                                          "in-left: -75px"
                                        ),
                                        " =  ≥ 5% of loci missing"
                                      )
                                    )
                                  )
                                )
                              )
                            )
                          ),
                          column(
                            width = 3,
                            br(),
                            fluidRow(
                              column(
                                width = 3,
                                div(
                                  class = "rectangle-green"
                                )
                              ),
                              column(
                                width = 9,
                                p(
                                  HTML(
                                    paste(
                                      tags$span(
                                        style = paste0(
                                          "color: white; font-size: 12px; posi",
                                          "tion: relative; bottom: -10px; marg",
                                          "in-left: -105px"
                                        ),
                                        " =  locus contains multiple variants"
                                      )
                                    )
                                  )
                                )
                              )
                            )
                          )
                        )
                      } else {
                        fluidRow(
                          column(
                            width = 12,
                            rHandsontableOutput("db_entries")
                          ),
                          br(),
                          column(
                            width = 3,
                            br(),
                            fluidRow(
                              column(
                                width = 3,
                                div(
                                  class = "rectangle-blue"
                                )
                              ),
                              column(
                                width = 7,
                                p(
                                  HTML(
                                    paste(
                                      tags$span(
                                        style = paste0(
                                          "color: white; font-size: 12px; posi",
                                          "tion: relative; bottom: -10px"
                                        ),
                                        " = included for analyses"
                                      )
                                    )
                                  )
                                )
                              )
                            )
                          ),
                          column(
                            width = 3,
                            br(),
                            fluidRow(
                              column(
                                width = 3,
                                div(
                                  class = "rectangle-orange"
                                )
                              ),
                              column(
                                width = 7,
                                p(
                                  HTML(
                                    paste(
                                      tags$span(
                                        style = paste0(
                                          "color: white; font-size: 12px; posi",
                                          "tion: relative; bottom: -10px; marg",
                                          "in-left: -45px"
                                        ),
                                        " =  duplicated assembly name"
                                      )
                                    )
                                  )
                                )
                              )
                            )
                          ),
                          column(
                            width = 3,
                            br(),
                            fluidRow(
                              column(
                                width = 3,
                                div(
                                  class = "rectangle-red"
                                )
                              ),
                              column(
                                width = 7,
                                p(
                                  HTML(
                                    paste(
                                      tags$span(
                                        style = paste0(
                                          "color: white; font-size: 12px; posi",
                                          "tion: relative; bottom: -10px; marg",
                                          "in-left: -75px"
                                        ),
                                        " =  ≥ 5% of loci missing"
                                      )
                                    )
                                  )
                                )
                              )
                            )
                          ),
                          column(
                            width = 3,
                            br(),
                            fluidRow(
                              column(
                                width = 3,
                                div(
                                  class = "rectangle-green"
                                )
                              ),
                              column(
                                width = 9,
                                p(
                                  HTML(
                                    paste(
                                      tags$span(
                                        style = paste0(
                                          "color: white; font-size: 12px; posi",
                                          "tion: relative; bottom: -10px; marg",
                                          "in-left: -105px"
                                        ),
                                        " =  locus contains multiple variants"
                                      )
                                    )
                                  )
                                )
                              )
                            )
                          )
                        )
                      }
                    }
                  })

                  if (!is.null(DB$data)) {
                    observe({
                      output$db_entries <- renderRHandsontable({
                        runjs(block_ui)
                        w$show()

                        tab <- generate_rhandsontable(
                          data = DB$data,
                          cust_var = DB$cust_var,
                          compare_select = input$compare_select,
                          allelic_profile = DB$allelic_profile,
                          allelic_profile_trunc = DB$allelic_profile_trunc,
                          entry_table_height = entry_table_height(),
                          country_names = country_names,
                          diff_allele = diff_allele(),
                          true_rows = true_rows(),
                          duplicated_names = duplicated_names(),
                          duplicated_ids = duplicated_ids(),
                          err_thresh = err_thresh(),
                          pinned_entries_highlight = pinned_entries_highlight()
                        )

                        runjs(unblock_ui)

                        tab
                      })

                      # Dynamic save button when rhandsontable changes or new entries
                      output$edit_entry_table <- renderUI({
                        check_new_entry <- check_new_entry()

                        if (
                          !is.null(check_new_entry) &&
                            !is.null(DB$check_new_entries) &&
                            !is.null(DB$meta)
                        ) {
                          if (!is.null(DB$meta)) {
                            ifelse(
                              isTRUE(all.equal(
                                get.entry.table.meta(),
                                select(DB$meta, -13),
                                check.attributes = FALSE
                              )),
                              new_meta <- FALSE,
                              new_meta <- TRUE
                            )
                          } else {
                            new_meta <- FALSE
                          }

                          if (check_new_entry && DB$check_new_entries) {
                            disable("import_menu")
                            disable("export_menu")
                            disable("del_button")
                            disable("add_new_variable")
                            disable("delete_new_variable")

                            Typing$reload <- FALSE
                            fluidRow(
                              column(
                                width = 8,
                                align = "left",
                                HTML(
                                  paste(
                                    tags$span(
                                      style = paste0(
                                        'color: white; font-size: 14px; positi',
                                        'on: absolute; bottom: -28px; right: -',
                                        '5px'
                                      ),
                                      'New entries - reload database'
                                    )
                                  )
                                )
                              ),
                              column(
                                width = 4,
                                actionButton(
                                  "load",
                                  "",
                                  icon = icon("rotate"),
                                  class = "pulsating-button",
                                  width = "40px"
                                )
                              )
                            )
                          } else if (Typing$status == "Attaching") {
                            disable("import_menu")
                            disable("export_menu")
                            disable("del_button")
                            disable("add_new_variable")
                            disable("delete_new_variable")
                            disable("import_menu")
                            removeModal()

                            fluidRow(
                              column(
                                width = 11,
                                align = "left",
                                HTML(
                                  paste(
                                    tags$span(
                                      style = paste0(
                                        'color: white; font-size: 14px; positi',
                                        'on: absolute; bottom: -28px; right: -',
                                        '5px'
                                      ),
                                      paste0(
                                        'No database changes possible - p',
                                        'ending entry addition'
                                      )
                                    )
                                  )
                                )
                              ),
                              column(
                                width = 1,
                                HTML(paste(
                                  '<i class="fa fa-spinner fa-spin" style="fon',
                                  't-size:20px; color:white; margin-top: 10px"',
                                  '></i>'
                                ))
                              )
                            )
                          } else if (isTRUE(DB$change) || new_meta) {
                            disable("import_menu")
                            disable("export_menu")

                            if (!is.null(input$db_entries)) {
                              fluidRow(
                                column(
                                  width = 5,
                                  HTML(
                                    paste(
                                      tags$span(
                                        style = paste0(
                                          'color: white; font-size: 16px; posi',
                                          'tion: absolute; bottom: -30px; righ',
                                          't: -5px'
                                        ),
                                        'Confirm changes'
                                      )
                                    )
                                  )
                                ),
                                column(
                                  width = 3,
                                  actionButton(
                                    "edit_button",
                                    "",
                                    icon = icon("bookmark"),
                                    class = "pulsating-button"
                                  )
                                ),
                                column(
                                  width = 4,
                                  actionButton(
                                    "undo_changes",
                                    "Undo",
                                    icon = icon("repeat")
                                  )
                                )
                              )
                            }
                          } else {
                            if (Typing$status != "Processing") {
                              enable("import_menu")
                            } else {
                              disable("import_menu")
                            }
                            enable("export_menu")
                            enable("del_button")
                            enable("add_new_variable")
                            enable("delete_new_variable")
                          }
                        } else {
                          if (Typing$status != "Processing") {
                            enable("import_menu")
                          } else {
                            disable("import_menu")
                          }
                          enable("export_menu")
                          enable("del_button")
                          enable("add_new_variable")
                          enable("delete_new_variable")
                        }
                      })
                    })

                    # Hide no entry message
                    output$db_no_entries <- NULL
                    output$distancematrix_no_entries <- NULL
                  } else {
                    # If database loading not successful dont show entry table
                    output$db_entries_table <- NULL
                    output$entry_table_controls <- NULL
                  }

                  # Render Entry table controls
                  output$entry_table_controls <- renderUI({
                    fluidRow(
                      column(
                        width = 5,
                        align = "center",
                        fluidRow(
                          column(
                            width = 3,
                            align = "right",
                            actionButton(
                              "sel_all_entries",
                              "Select All",
                              icon = icon("check")
                            )
                          ),
                          column(
                            width = 3,
                            align = "left",
                            actionButton(
                              "desel_all_entries",
                              "Deselect All",
                              icon = icon("xmark")
                            )
                          ),
                          column(
                            width = 3,
                            align = "right",
                            actionButton(
                              "export_menu",
                              "Export",
                              icon = icon("download")
                            )
                          ),
                          column(
                            width = 3,
                            align = "left",
                            actionButton(
                              "import_menu",
                              "Import",
                              icon = icon("file-import")
                            )
                          )
                        )
                      ),
                      column(
                        width = 5,
                        uiOutput("edit_entry_table")
                      )
                    )
                  })

                  ## Render Distance Matrix ----
                  observe({
                    if (!is.null(DB$data)) {
                      runjs(block_ui)

                      if (
                        any(duplicated(DB$meta$`Assembly Name`)) ||
                          any(duplicated(DB$meta$`Assembly ID`))
                      ) {
                        output$db_distancematrix <- NULL
                        output$distancematrix_duplicated <- renderUI({
                          column(
                            width = 12,
                            tags$span(
                              style = "font-size: 15; color: white",
                              paste0(
                                "Change duplicated entry names to",
                                " display distance matrix."
                              )
                            ),
                            br(),
                            br(),
                            br(),
                            actionButton(
                              "change_entries",
                              "Go to Entry Table",
                              class = "btn btn-default"
                            )
                          )
                        })
                      } else {
                        output$distancematrix_duplicated <- NULL

                        if (
                          !is.null(DB$data) &&
                            !is.null(DB$allelic_profile) &&
                            !is.null(DB$allelic_profile_true) &&
                            !is.null(DB$cust_var) &&
                            !is.null(input$distmatrix_label) &&
                            !is.null(input$distmatrix_diag) &&
                            !is.null(input$distmatrix_triangle) &&
                            nrow(DB$data) > 2
                        ) {
                          dist_matrix <- hamming_df()

                          output$db_distancematrix <- renderRHandsontable({
                            if (nrow(dist_matrix) > 28) {
                              height <- 700
                            } else {
                              height <- NULL
                            }

                            rhandsontable(
                              dist_matrix,
                              digits = 1,
                              readOnly = TRUE,
                              contextMenu = FALSE,
                              highlightCol = TRUE,
                              highlightRow = TRUE,
                              height = height,
                              rowHeaders = NULL
                            ) %>%
                              hot_heatmap(
                                renderer = paste0(
                                  "function (instance, td, row, col, prop, value, ",
                                  "cellProperties) {",
                                  "Handsontable.renderers.TextRenderer.apply(this,",
                                  " arguments);",
                                  "heatmapScale  = chroma.scale(['#17F556', '#ED6D",
                                  "47']);",
                                  "if (instance.heatmap[col]) {",
                                  "mn = ",
                                  DB$matrix_min,
                                  ";",
                                  "mx = ",
                                  DB$matrix_max,
                                  ";",
                                  "pt = (parseInt(value, 10) - mn) / (mx - mn);",
                                  "td.style.backgroundColor = heatmapScale(pt).hex",
                                  "();",
                                  "}",
                                  "}"
                                )
                              ) %>%
                              hot_rows(fixedRowsTop = 0) %>%
                              hot_cols(fixedColumnsLeft = 1) %>%
                              hot_col(
                                1:(dim(dist_matrix)[1] + 1),
                                halign = "htCenter",
                                valign = "htMiddle"
                              ) %>%
                              hot_col(
                                1,
                                renderer = paste0(
                                  "function(instance, td, row, col, prop, value, c",
                                  "ellProperties) {",
                                  "Handsontable.renderers.NumericRenderer.apply(th",
                                  "is, arguments);",
                                  "td.style.background = '#F0F0F0'",
                                  "}"
                                )
                              )
                          })
                        }
                      }

                      # Render Distance Matrix UI

                      output$distmatrix_show <- renderUI({
                        if (!is.null(DB$data)) {
                          if (nrow(DB$data) > 1) {
                            if (!is.null(input$distmatrix_label)) {
                              distmatrix_label_selected <-
                                input$distmatrix_label
                            } else {
                              distmatrix_label_selected <- c("Assembly Name")
                            }

                            if (!is.null(input$distmatrix_true)) {
                              distmatrix_true_selected <- input$distmatrix_true
                            } else {
                              distmatrix_true_selected <- FALSE
                            }

                            if (!is.null(input$distmatrix_triangle)) {
                              distmatrix_triangle_selected <-
                                input$distmatrix_triangle
                            } else {
                              distmatrix_triangle_selected <- FALSE
                            }

                            if (!is.null(input$distmatrix_diag)) {
                              distmatrix_diag_selected <- input$distmatrix_diag
                            } else {
                              distmatrix_diag_selected <- TRUE
                            }

                            fluidRow(
                              column(1),
                              div(
                                class = "distancematrix-options",
                                column(
                                  width = 2,
                                  box(
                                    solidHeader = TRUE,
                                    status = "primary",
                                    width = "100%",
                                    title = "Options",
                                    column(
                                      width = 12,
                                      br(),
                                      br(),
                                      fluidRow(
                                        column(
                                          width = 3,
                                          HTML(
                                            paste(
                                              tags$span(
                                                style = paste0(
                                                  'color: white; font-size: 14',
                                                  'px;'
                                                ),
                                                'Label'
                                              )
                                            )
                                          )
                                        ),
                                        column(
                                          width = 9,
                                          div(
                                            class = "distmatrix-label",
                                            selectInput(
                                              "distmatrix_label",
                                              label = "",
                                              choices = c(
                                                "Index",
                                                "Assembly Name",
                                                "Assembly ID"
                                              ),
                                              selected = distmatrix_label_selected,
                                              width = "100%"
                                            )
                                          )
                                        )
                                      ),
                                      br(),
                                      br(),
                                      div(
                                        class = "mat-switch-dmatrix",
                                        materialSwitch(
                                          "distmatrix_true",
                                          h5(
                                            p("Only Included Entries"),
                                            style = paste0(
                                              "color:white; padding-left: 0px",
                                              "; position: relative; top: -4p",
                                              "x; right: -5px;"
                                            )
                                          ),
                                          value = distmatrix_true_selected,
                                          right = TRUE
                                        )
                                      ),
                                      br(),
                                      div(
                                        class = "mat-switch-dmatrix",
                                        materialSwitch(
                                          "distmatrix_triangle",
                                          h5(
                                            p("Show Upper Triangle"),
                                            style = paste0(
                                              "color:white; padding-left: 0px",
                                              "; position: relative; top: -4p",
                                              "x; right: -5px;"
                                            )
                                          ),
                                          value = distmatrix_triangle_selected,
                                          right = TRUE
                                        )
                                      ),
                                      br(),
                                      div(
                                        class = "mat-switch-dmatrix",
                                        materialSwitch(
                                          "distmatrix_diag",
                                          h5(
                                            p("Show Diagonal"),
                                            style = paste0(
                                              "color:white; padding-left: 0px",
                                              "; position: relative; top: -4p",
                                              "x; right: -5px;"
                                            )
                                          ),
                                          value = distmatrix_diag_selected,
                                          right = TRUE
                                        )
                                      ),
                                      br(),
                                      br(),
                                      br(),
                                      br(),
                                      fluidRow(
                                        column(
                                          width = 8,
                                          HTML(
                                            paste(
                                              tags$span(
                                                style = paste0(
                                                  'color: white; font-size: 14',
                                                  'px;'
                                                ),
                                                'Download CSV'
                                              )
                                            )
                                          )
                                        ),
                                        column(
                                          width = 4,
                                          downloadBttn(
                                            "download_distmatrix",
                                            style = "simple",
                                            label = "",
                                            size = "sm",
                                            icon = icon("download")
                                          )
                                        )
                                      )
                                    )
                                  )
                                )
                              ),
                              column(
                                width = 9,
                                uiOutput("distancematrix_duplicated"),
                                rHandsontableOutput("db_distancematrix")
                              )
                            )
                          } else {
                            column(
                              width = 9,
                              align = "left",
                              p(
                                HTML(
                                  paste(
                                    tags$span(
                                      style = 'color: white; font-size: 15px;',
                                      paste0(
                                        "Type at least two assemblies to ",
                                        "display a distance matrix."
                                      )
                                    )
                                  )
                                )
                              ),
                              br(),
                              br()
                            )
                          }
                        }
                      })
                      runjs(unblock_ui)
                    }
                  })

                  # render custom variables box UI
                  output$custom_var_box <- renderUI({
                    custom_var_button <- actionButton(
                      "custom_var_table",
                      "Browse ",
                      icon = icon("table-list")
                    )

                    if (nrow(DB$cust_var) == 0) {
                      custom_var_button <- disabled(custom_var_button)
                    }

                    del_which_var_input <- selectInput(
                      inputId = "del_which_var",
                      label = "",
                      choices = DB$cust_var$Variable
                    )

                    if (
                      is.null(DB$cust_var$Variable) || nrow(DB$cust_var) == 0
                    ) {
                      del_which_var_input <- shinyjs::disabled(
                        del_which_var_input
                      )
                    }

                    box(
                      solidHeader = TRUE,
                      status = "primary",
                      width = "100%",
                      title = "Custom Variables",
                      fluidRow(
                        column(1),
                        column(
                          width = 7,
                          fluidRow(
                            column(
                              width = 9,
                              align = "center",
                              textInput(
                                "new_var_name",
                                label = "",
                                placeholder = "New Variable"
                              )
                            ),
                            column(
                              width = 2,
                              actionButton(
                                "add_new_variable",
                                "",
                                icon = icon("plus")
                              )
                            )
                          ),
                          fluidRow(
                            column(
                              width = 9,
                              align = "center",
                              del_which_var_input
                            ),
                            column(
                              width = 2,
                              align = "left",
                              actionButton(
                                "delete_new_variable",
                                "",
                                icon = icon("minus")
                              )
                            )
                          )
                        ),
                        column(
                          width = 2,
                          custom_var_button
                        )
                      )
                    )
                  }) |>
                    shiny::bindEvent(
                      DB$cust_var
                    )

                  # Render delete entry box UI
                  output$delete_box <- renderUI({
                    box(
                      solidHeader = TRUE,
                      status = "primary",
                      width = "100%",
                      title = "Delete Entries",
                      fluidRow(
                        column(width = 1),
                        column(
                          width = 2,
                          h5("Index", style = "color: white; margin-top: 5vh")
                        ),
                        column(
                          width = 6,
                          align = "left",
                          uiOutput("delete_select")
                        ),
                        column(
                          width = 2,
                          align = "center",
                          br(),
                          uiOutput("del_bttn")
                        )
                      ),
                      br()
                    )
                  })

                  # Render loci comparison box UI
                  output$compare_allele_box <- renderUI({
                    box(
                      solidHeader = TRUE,
                      status = "primary",
                      width = "100%",
                      title = "Compare Loci",
                      column(
                        width = 12,
                        br(),
                        fluidRow(
                          column(1),
                          column(
                            width = 10,
                            align = "left",
                            uiOutput("compare_select")
                          )
                        ),
                        br(),
                        fluidRow(
                          column(1),
                          column(
                            width = 10,
                            align = "left",
                            uiOutput("compare_difference_box")
                          )
                        )
                      ),
                      br()
                    )
                  })

                  # Render entry deletion select input
                  output$delete_select <- renderUI({
                    pickerInput(
                      "select_delete",
                      label = "",
                      choices = DB$data[, "Index"],
                      options = list(
                        `live-search` = TRUE,
                        `actions-box` = TRUE,
                        size = 10,
                        style = paste0(
                          "background-color: white; bor",
                          "der-radius: 5px;"
                        )
                      ),
                      multiple = TRUE
                    )
                  })

                  # Render delete entry button
                  output$del_bttn <- renderUI({
                    actionBttn(
                      "del_button",
                      label = "",
                      color = "danger",
                      size = "sm",
                      style = "material-circle",
                      icon = icon("xmark")
                    )
                  })

                  # Missing Values UI ----

                  # Missing values calculations and table
                  observe({
                    req(DB$allelic_profile, DB$meta)
                    NA_table <- DB$allelic_profile[,
                      colSums(
                        is.na(DB$allelic_profile)
                      ) !=
                        0
                    ]

                    NA_table <- NA_table[rowSums(is.na(NA_table)) != 0, ]

                    NA_table[is.na(NA_table)] <- "NA"

                    NA_table <- NA_table %>%
                      cbind(
                        "Assembly Name" = DB$meta[
                          rownames(
                            NA_table
                          ),
                        ]$`Assembly Name`
                      ) %>%
                      cbind(
                        "Errors" = DB$meta[
                          rownames(
                            NA_table
                          ),
                        ]$Errors
                      ) %>%
                      relocate("Assembly Name", "Errors")

                    DB$na_table <- NA_table

                    output$table_missing_values <- renderRHandsontable({
                      if (nrow(DB$na_table) > 26) {
                        height <- 650
                      } else {
                        height <- NULL
                      }

                      rhandsontable(
                        DB$na_table,
                        readOnly = TRUE,
                        rowHeaders = NULL,
                        contextMenu = FALSE,
                        height = height,
                        highlightCol = TRUE,
                        highlightRow = TRUE,
                        error_highlight = err_thresh_na() - 1
                      ) %>%
                        hot_cols(fixedColumnsLeft = 1) %>%
                        hot_rows(fixedRowsTop = 0) %>%
                        hot_col(
                          1:ncol(DB$na_table),
                          valign = "htMiddle",
                          halign = "htLeft"
                        ) %>%
                        hot_col(
                          c(1, 2),
                          renderer = "
                            function (instance, td, row, col, 
                            prop, value, cellProperties) {
                              Handsontable.renderers.TextRenderer.apply(this, 
                              arguments);
                              if (instance.params) {
                              hrows = instance.params.error_highlight
                              hrows = hrows instanceof Array ? hrows : [hrows]
                                if (hrows.includes(row)) { 
                                td.style.backgroundColor = 'rgbA(255, 80, 1, 0.8)' 
                                }
                              }
                            }"
                        ) %>%
                        hot_col(
                          3:ncol(DB$na_table),
                          renderer = htmlwidgets::JS(
                            "function(instance, td, row, col, prop, value, 
                            cellProperties) {
                              if (value.length > 8) {
                                value = value.slice(0, 4) + '...' + 
                                value.slice(value.length - 4);
                              }
                              td.innerHTML = value;
                              td.style.textAlign = 'center';
                              return td;
                            }"
                          )
                        )
                    })
                  })

                  # Render missing value informatiojn box UI
                  output$missing_values <- renderUI({
                    box(
                      solidHeader = TRUE,
                      status = "primary",
                      width = "100%",
                      title = "Missing Value Handling",
                      fluidRow(
                        div(
                          class = "white",
                          column(
                            width = 12,
                            align = "left",
                            br(),
                            HTML(
                              paste0(
                                "There are ",
                                strong(as.character(sum(is.na(DB$data)))),
                                " unsuccessful allele allocations (NA). ",
                                strong(sum(sapply(DB$allelic_profile, anyNA))),
                                " out of ",
                                strong(ncol(DB$allelic_profile)),
                                " total loci in this scheme contain NA's (",
                                strong(round(
                                  (sum(sapply(DB$allelic_profile, anyNA)) /
                                    ncol(DB$allelic_profile) *
                                    100),
                                  1
                                )),
                                " %). ",
                                "<br><br><br>Decide how these missing values should be treated:"
                              )
                            ),
                            br()
                          )
                        )
                      ),
                      fluidRow(
                        column(
                          width = 12,
                          align = "left",
                          br(),
                          div(
                            class = "na-handling",
                            prettyRadioButtons(
                              "na_handling",
                              "",
                              choiceNames = c(
                                "Ignore missing values for pairwise comparison",
                                "Omit loci with missing values for all assemblies",
                                "Treat missing values as allele variant"
                              ),
                              choiceValues = c("ignore_na", "omit", "category"),
                              shape = "curve",
                              selected = c("ignore_na")
                            )
                          ),
                          br()
                        )
                      )
                    )
                  })
                } else {
                  #if no typed assemblies present

                  # null underlying database

                  DB$data <- NULL
                  DB$meta <- NULL
                  DB$meta_gs <- NULL
                  DB$meta_true <- NULL
                  DB$allelic_profile <- NULL
                  DB$allelic_profile_trunc <- NULL
                  DB$allelic_profile_true <- NULL

                  # Render menu without missing values tab
                  output$menu_typing <- renderMenu(
                    sidebarMenu(
                      menuItem(
                        text = "Database",
                        tabName = "database",
                        icon = icon("hard-drive"),
                        startExpanded = TRUE,
                        selected = TRUE,
                        menuSubItem(
                          text = "Browse Entries",
                          tabName = "db_browse_entries"
                        ),
                        menuSubItem(
                          text = "Scheme Info",
                          tabName = "db_schemeinfo"
                        ),
                        menuSubItem(
                          text = "Loci Info",
                          tabName = "db_loci_info"
                        ),
                        menuSubItem(
                          text = "Distance Matrix",
                          tabName = "db_distmatrix"
                        )
                      ),
                      menuItem(
                        text = "Schemes",
                        tabName = "init",
                        icon = icon("layer-group")
                      ),
                      menuItem(
                        text = "Allelic Typing",
                        tabName = "typing",
                        icon = icon("gears")
                      ),
                      menuItem(
                        text = "Visualization",
                        tabName = "visualization",
                        icon = icon("circle-nodes")
                      )
                    )
                  )

                  if (!is.null(DB$scheme)) {
                    amrfinder_available <- check.amrfinder.available(
                      selected_scheme = DB$scheme,
                      amrfinder_species = amrfinder_species
                    )

                    if (!isFALSE(amrfinder_available)) {
                      output$menu_screening <- renderMenu(
                        screening_menu_available
                      )
                    } else {
                      output$menu_screening <- renderMenu(
                        screening_menu_available
                      )
                    }
                  }

                  observe({
                    if (is.null(DB$data)) {
                      if (check_new_entry()) {
                        output$db_no_entries <- renderUI(
                          column(
                            width = 12,
                            fluidRow(
                              column(1),
                              column(
                                width = 3,
                                align = "left",
                                HTML(
                                  paste(
                                    tags$span(
                                      style = 'color: white; font-size: 15px; position: absolute; bottom: -30px; right: -5px',
                                      'New entries - reload database'
                                    )
                                  )
                                )
                              ),
                              column(
                                width = 4,
                                actionButton(
                                  "load",
                                  "",
                                  icon = icon("rotate"),
                                  class = "pulsating-button",
                                  width = "40px"
                                )
                              )
                            )
                          )
                        )
                      } else {
                        output$db_no_entries <- renderUI(
                          column(
                            width = 12,
                            fluidRow(
                              column(1),
                              column(
                                width = 11,
                                align = "left",
                                HTML(
                                  paste(
                                    "<span style='color: white;'>",
                                    "No Entries for this scheme available.\n",
                                    "Type a genome in the section <strong>Allelic Typing</strong> and add the result to the local database.",
                                    sep = '<br/>'
                                  )
                                ),
                                br(),
                                br(),
                                HTML(
                                  paste0(
                                    "<span style='color: white;'>",
                                    "Alternatively import a dataset of isolate",
                                    "s which were typed using the same ",
                                    DB$scheme,
                                    " cgMLST nomenclature."
                                  )
                                ),
                                br(),
                                div(
                                  id = "import-menu",
                                  actionButton(
                                    "import_menu",
                                    "Import",
                                    icon = icon("file-import")
                                  )
                                )
                              )
                            )
                          )
                        )
                      }
                    }
                  })

                  # Render scheme info download button
                  output$download_scheme_info <- renderUI({
                    column(
                      12,
                      downloadBttn(
                        "download_schemeinfo",
                        style = "simple",
                        label = "",
                        size = "sm",
                        icon = icon("download"),
                        color = "primary"
                      ),
                      bsTooltip(
                        "download_schemeinfo_bttn",
                        HTML("Save scheme information"),
                        placement = "bottom",
                        trigger = "hover"
                      )
                    )
                  })

                  # Render scheme info download button
                  output$download_loci <- renderUI({
                    if (!is.null(DB$loci_info)) {
                      column(
                        12,
                        downloadBttn(
                          "download_loci_info",
                          style = "simple",
                          label = "",
                          size = "sm",
                          icon = icon("download"),
                          color = "primary"
                        ),
                        bsTooltip(
                          "download_loci_info_bttn",
                          HTML("Save loci information <br> (without sequence)"),
                          placement = "bottom",
                          trigger = "hover"
                        )
                      )
                    } else {
                      NULL
                    }
                  })

                  output$distancematrix_no_entries <- renderUI(
                    fluidRow(
                      column(1),
                      column(
                        width = 11,
                        align = "left",
                        HTML(paste(
                          "<span style='color: white;'>",
                          "No Entries for this scheme available.",
                          "Type an assembly in the section <strong>Allelic Typing</strong> and add the result to the local database.",
                          sep = '<br/>'
                        ))
                      )
                    )
                  )

                  output$db_entries <- NULL
                  output$edit_index <- NULL
                  output$edit_scheme_d <- NULL
                  output$edit_entries <- NULL
                  output$compare_select <- NULL
                  output$delete_select <- NULL
                  output$del_bttn <- NULL
                  output$compare_allele_box <- NULL
                  output$download_entries <- NULL
                  output$missing_values <- NULL
                  output$custom_var_box <- NULL
                  output$delete_box <- NULL
                  output$entry_table_controls <- NULL
                  output$multi_stop <- NULL
                  output$metadata_multi_box <- NULL
                  output$start_multi_typing_ui <- NULL
                  output$pending_typing <- NULL
                  output$multi_typing_results <- NULL
                  output$single_typing_progress <- NULL
                  output$metadata_single_box <- NULL
                  output$start_typing_ui <- NULL

                  output$initiate_typing_ui <- renderUI({
                    column(
                      width = 4,
                      align = "center",
                      br(),
                      br(),
                      h3(
                        p("Initiate Typing"),
                        style = "color:white; margin-left: 15px"
                      ),
                      br(),
                      br(),
                      p(
                        HTML(
                          paste(
                            tags$span(
                              style = 'color: white; font-size: 15px; margin-bottom: 0px; margin-left: 15px',
                              'Select Assembly File'
                            )
                          )
                        )
                      ),
                      fluidRow(
                        column(1),
                        column(
                          width = 11,
                          align = "center",
                          shinyFilesButton(
                            "genome_file",
                            "Browse",
                            icon = icon("file"),
                            title = "Select the assembly in .fasta/.fna/.fa format:",
                            multiple = FALSE,
                            buttonType = "default",
                            class = NULL,
                            root = path_home()
                          ),
                          br(),
                          br(),
                          uiOutput("genome_path"),
                          br()
                        )
                      )
                    )
                  })

                  output$initiate_typing_ui <- renderUI({
                    column(
                      width = 4,
                      align = "center",
                      br(),
                      br(),
                      h3(
                        p("Initiate Typing"),
                        style = "color:white; margin-left: 15px"
                      ),
                      br(),
                      br(),
                      p(
                        HTML(
                          paste(
                            tags$span(
                              style = 'color: white; font-size: 15px; margin-bottom: 0px; margin-left: 15px',
                              'Select Assembly File'
                            )
                          )
                        )
                      ),
                      fluidRow(
                        column(1),
                        column(
                          width = 11,
                          align = "center",
                          shinyFilesButton(
                            "genome_file",
                            "Browse",
                            icon = icon("file"),
                            title = "Select the assembly in .fasta/.fna/.fa format:",
                            multiple = FALSE,
                            buttonType = "default",
                            class = NULL,
                            root = path_home()
                          ),
                          br(),
                          br(),
                          uiOutput("genome_path"),
                          br()
                        )
                      )
                    )
                  })

                  output$initiate_multi_typing_ui <- initiate_multi_typing_ui
                }
              }
            }
          }
        } else {
          log_print("Invalid scheme folder")
          show_toast(
            title = "Invalid scheme folder",
            type = "warning",
            position = "bottom-end",
            timer = 4000
          )
        }
      }

      ### Check if scheme update available
      if (!is.null(DB$scheme_db) && isTRUE(DB$scheme_new)) {
        # Query remote scheme
        if (DB$scheme_db == "cgMLST.org Nomenclature Server (h25)") {
          db_spec <- schemes[schemes[, "database"] == "cgMLST.org", ]
          DB$url_link <- db_spec[, "url"][
            db_spec[, "species"] == gsub(" ", "_", DB$scheme)
          ]

          remote <- tryCatch(
            {
              read_html(DB$url_link)
            },
            error = function(e) {
              DB$failCon <- TRUE
              show_toast(
                title = "Could not retrieve data. Check internet connection.",
                type = "error",
                position = "bottom-end",
                timer = 6000
              )
              warning("Could not retrieve data. Check internet connection.")
              return(NULL)
            }
          )

          if (is.null(remote)) {
            last_scheme_change <- NULL
          } else {
            DB$failCon <- FALSE
            remote_scheme <- remote %>%
              html_table(header = FALSE) %>%
              as.data.frame(stringsAsFactors = FALSE)

            last_scheme_change <- strptime(
              remote_scheme[, 2][remote_scheme[, 1] == "Last Change"],
              format = "%B %d, %Y, %H:%M %p"
            )
          }
        } else if (DB$scheme_db == "pubMLST") {
          db_spec <- schemes[schemes[, "database"] == "pubMLST", ]
          DB$url_link <- db_spec[, "url"][
            db_spec[, "species"] == gsub(" ", "_", DB$scheme)
          ]

          remote_scheme <- get.schemeinfo(url_link = DB$url_link)

          if (is.null(remote_scheme)) {
            last_scheme_change <- NULL
          } else {
            last_scheme_change <- remote_scheme[["last_updated"]]
          }
        }

        if (
          !is.null(last_scheme_change) &&
            length(last_scheme_change) &&
            !is.na(last_scheme_change)
        ) {
          last_file_change <- format(
            file.info(file.path(
              Startup$database,
              ".downloaded_schemes",
              paste0(gsub(" ", "_", DB$scheme), ".zip")
            ))$mtime,
            "%Y-%m-%d %H:%M %p"
          )

          if (!is.null(last_file_change) && !is.na(last_file_change)) {
            if (
              length(last_file_change) > 0 && length(last_scheme_change) > 0
            ) {
              if (last_file_change < last_scheme_change) {
                showModal(
                  div(
                    class = "start-modal",
                    modalDialog(
                      fluidRow(
                        br(),
                        column(
                          width = 11,
                          p(
                            HTML(
                              paste0(
                                '<span style="color: white; display: block; font-size: 15px; margin-left: 15px;">',
                                "The ",
                                DB$scheme,
                                " scheme was updated at ",
                                last_scheme_change,
                                " on the ",
                                DB$schemeinfo[, 2][
                                  DB$schemeinfo[, 1] == "Database"
                                ],
                                " database. To fetch the changes download the scheme.",
                                '</span>'
                              )
                            )
                          )
                        ),
                        br()
                      ),
                      title = HTML(paste0(
                        '<i class="fa-solid fa-circle-exclamation" style="font-size:17px;color:white"></i>',
                        " &nbsp;&nbsp; Scheme update available"
                      )),
                      fade = TRUE,
                      easyClose = TRUE,
                      footer = tagList(
                        modalButton("Dismiss"),
                        actionButton(
                          "update_scheme",
                          "Update Scheme",
                          icon = icon("layer-group"),
                          class = "btn btn-default"
                        )
                      )
                    )
                  )
                )
              }
            }
          }
        }
      }

      DB$scheme_new <- FALSE

      removeClass(selector = "body", class = "sidebar-collapse")
      addClass(selector = "body", class = "sidebar-toggle")
      runjs(unblock_ui)

      waiter_hide()
    }
  })

  # _______________________ ####

  ## Database ----

  ### Conditional UI Elements rendering ----

  # Render the notification dropdown menu
  output$notificationMenu <- renderMenu({
    if (isTRUE(DB$failCon)) {
      dropdownMenu(
        type = "notifications",
        notificationItem(
          text = "Network connectivity unavailable",
          icon = icon("wifi"),
          status = "danger"
        )
      )
    } else {
      dropdownMenu(type = "notifications")
    }
  })

  #### Scheme Info ----

  output$distance_matrix_info <- renderUI({
    req(DB$scheme)

    if (!is.null(DB$allelic_profile)) {
      if (anyNA(DB$allelic_profile)) {
        any_na <- TRUE
      } else {
        any_na <- FALSE
      }
    } else {
      any_na <- FALSE
    }

    if (!is.null(input$na_handling)) {
      na_handling <- input$na_handling
    } else {
      na_handling <- "default"
    }
    column(
      width = 7,
      align = "left",
      p(
        HTML(
          paste(
            if (isTRUE(any_na)) {
              '<span style="color: white; font-size: 13px; position:relative; top:15px;">'
            } else {
              '<span style="color: white; font-size: 14px; position:relative; top:27px;">'
            },
            'Allelic distance according to',
            '<span style="font-style: italic;">',
            DB$scheme,
            '</span>',
            'scheme.',
            '</span>'
          )
        )
      ),
      if (isTRUE(any_na)) {
        p(
          HTML(
            paste(
              '<i class="fa-solid fa-circle-exclamation" style="font-size:13px; color:white; position:relative; top:10px; margin-right: 10px;"></i>',
              '<span style="color:white; font-size:13px; position:relative; top:10px;">',
              'Missing value handling:',
              '</span>',
              '<span style="color:white; font-style:italic; font-size:13px; position:relative; top:10px;">',
              if (na_handling == "ignore_na") {
                "&nbsp;&nbsp;Ignore missing values for pairwise comparison"
              } else if (na_handling == "omit") {
                "&nbsp;&nbsp;Omit loci with missing values for all assemblies"
              } else if (na_handling == "category") {
                "&nbsp;&nbsp;Treat missing values as allele variant"
              } else {
                "&nbsp;&nbsp;Ignore missing values for pairwise comparison"
              },
              '</span>'
            )
          )
        )
      }
    )
  })

  # Species info selector UI
  observe({
    req(DB$scheme, Startup$database)

    scheme_path <- file.path(
      Startup$database,
      gsub(" ", "_", DB$scheme),
      gsub(" ", "_", DB$scheme)
    )

    if (file.exists(paste0(scheme_path, ".rds"))) {
      species_data <- readRDS(paste0(scheme_path, ".rds"))
      if (!is.null(species_data)) {
        if (length(species_data) > 1) {
          choices <- gsub(
            " ",
            "_",
            unlist(lapply(species_data, function(x) x$Name$name))
          )
          names(choices) <- gsub(
            "_",
            " ",
            unlist(lapply(species_data, function(x) x$Name$name))
          )
          output$species_info_select_saved <- renderUI({
            fluidRow(
              column(1),
              column(
                width = 3,
                p(
                  HTML(
                    paste0(
                      '<span style="color: white; font-size: 15px; position: relative; top: 26px;">',
                      'Species Complex',
                      '</span>'
                    )
                  )
                )
              ),
              column(
                width = 1,
                bslib::tooltip(
                  bsicons::bs_icon(
                    "question-circle",
                    title = "Scheme comprises multiple species.",
                    color = "white",
                    height = "14px",
                    width = "14px",
                    position = "relative",
                    top = "9px",
                    right = "28px"
                  ),
                  "Text shown in the tooltip.",
                  show = FALSE,
                  id = "schemeinfo_tooltip_saved"
                )
              ),
              column(
                width = 6,
                selectInput(
                  "selected_species_saved",
                  "",
                  choices = choices
                )
              )
            )
          })
        } else {
          output$species_info_select_saved <- NULL
        }
      } else {
        output$species_info_select_saved <- NULL
      }
    }
  })

  output$species_info_saved <- renderUI({
    req(Startup$database, DB$scheme)

    species_data_path <- file.path(
      Startup$database,
      gsub(" ", "_", DB$scheme),
      paste0(gsub(" ", "_", DB$scheme), ".rds")
    )

    if (file.exists(species_data_path)) {
      species_data <- readRDS(species_data_path)
      if (length(species_data) > 1) {
        if (!is.null(input$selected_species_saved)) {
          image_path <- file.path(
            Startup$database,
            gsub(" ", "_", DB$scheme),
            paste0(
              names(species_data)[
                names(species_data) == input$selected_species_saved
              ],
              ".jpg"
            )
          )

          if (file.exists(image_path)) {
            output$species_no_img_saved <- NULL
            output$species_img_saved <- renderImage(
              {
                list(src = image_path, height = 180)
              },
              deleteFile = FALSE
            )
          } else {
            output$species_no_img_saved <- renderUI(
              HTML(
                '<i class="fa-solid fa-bacteria" style="font-size:150px;color:white;margin-right:25px;" ></i>'
              )
            )
            output$species_img_saved <- NULL
          }
        } else {
          output$species_no_img_saved <- renderUI(
            HTML(
              '<i class="fa-solid fa-bacteria" style="font-size:150px;color:white;margin-right:25px;" ></i>'
            )
          )
          output$species_img_saved <- NULL
        }
      } else if (length(species_data) > 0) {
        image_path <- file.path(
          Startup$database,
          gsub(" ", "_", DB$scheme),
          paste0(gsub("_(PM|CM)", "", gsub(" ", "_", DB$scheme)), ".jpg")
        )

        if (file.exists(image_path)) {
          output$species_no_img_saved <- NULL
          output$species_img_saved <- renderImage(
            {
              list(src = image_path, height = 180)
            },
            deleteFile = FALSE
          )
        } else {
          output$species_no_img_saved <- renderUI(
            HTML(
              '<i class="fa-solid fa-bacteria" style="font-size:150px;color:white;margin-right:25px;" ></i>'
            )
          )
          output$species_img_saved <- NULL
        }
      }

      multiple <- length(species_data) > 1 &&
        !is.null(input$selected_species_saved)
      if (multiple) {
        species_data <- species_data[[input$selected_species_saved]]
      } else {
        species_data <- species_data[[1]]
      }

      if (!is.null(species_data)) {
        box(
          solidHeader = TRUE,
          status = "primary",
          width = "100%",
          title = HTML(
            paste0(
              '<span style="color: white; font-size: 15px;">',
              'Species Information - Data Fetched from NCBI&nbsp&nbsp&nbsp ',
              ' <a href="',
              'https://www.ncbi.nlm.nih.gov/',
              '" target="_blank" style="color:#008edb; font-style:normal; text-decoration:none;"> https://www.ncbi.nlm.nih.gov/ </a>',
              '</span>'
            )
          ),
          addSpinner(
            species_data_ui(species_data, fetch = FALSE),
            spin = "dots",
            color = "#ffffff"
          )
        )
      } else {
        NULL
      }
    }
  })

  # Scheme selector UI
  output$scheme_selector <- renderUI({
    if (!is.null(DB$scheme)) {
      scheme_names <- schemes$species
      names(scheme_names) <- ifelse(
        scheme_names %in% gsub(" ", "_", DB$available),
        paste(scheme_names, "\U1F5BF"),
        scheme_names
      )

      div(
        class = "select-cgmlst",
        pickerInput(
          inputId = "select_cgmlst",
          label = NULL,
          choices = scheme_names,
          choicesOpt = list(
            subtext = paste("Database", schemes$database, sep = ": ")
          ),
          width = "auto",
          selected = gsub(" ", "_", DB$scheme),
          options = list(
            `live-search` = TRUE,
            `actions-box` = TRUE,
            size = 10,
            style = "background-color: white; border-radius: 5px;"
          ),
          multiple = FALSE
        )
      )
    } else {
      pickerInput(
        inputId = "select_cgmlst",
        label = NULL,
        choices = schemes$species,
        choicesOpt = list(
          subtext = paste("Database", schemes$database, sep = ": ")
        ),
        width = "300px",
        selected = NULL,
        options = list(
          `live-search` = TRUE,
          `actions-box` = TRUE,
          size = 10,
          style = "background-color: white; border-radius: 5px;"
        ),
        multiple = FALSE
      )
    }
  })

  # Control custom variables table
  output$cust_var_select <- renderUI({
    if (nrow(DB$cust_var) > 5) {
      selectInput(
        "cust_var_select",
        "",
        choices = 1:ceiling(nrow(DB$cust_var) / 5)
      )
    }
  })

  output$cust_var_info <- renderUI({
    if ((!is.null(DB$cust_var)) && (!is.null(input$cust_var_select))) {
      if (nrow(DB$cust_var) > 5) {
        low <- -4
        high <- 0
        for (i in 1:input$cust_var_select) {
          low <- low + 5
          if ((nrow(DB$cust_var) %% 5) != 0) {
            if (i == ceiling(nrow(DB$cust_var) / 5)) {
              high <- high + nrow(DB$cust_var) %% 5
            } else {
              high <- high + 5
            }
          } else {
            high <- high + 5
          }
        }
        h5(
          paste0(
            "Showing ",
            low,
            " to ",
            high,
            " of ",
            nrow(DB$cust_var),
            " variables"
          ),
          style = "color: white; font-size: 10px;"
        )
      }
    }
  })

  # Message on Database tabs if no scheme available yet
  observe({
    if (!is.null(DB$exist)) {
      if (DB$exist) {
        # Message for tab Browse Entries
        output$no_scheme_entries <- renderUI({
          fluidRow(
            column(1),
            column(
              width = 4,
              align = "left",
              p(
                HTML(
                  paste(
                    tags$span(
                      style = 'color: white; font-size: 15px; ',
                      'No scheme available.'
                    )
                  )
                )
              ),
              p(
                HTML(
                  paste(
                    tags$span(
                      style = 'color: white; font-size: 15px; ',
                      'Download a scheme first and type assemblies in the section Allelic Typing.'
                    )
                  )
                )
              )
            )
          )
        })

        # Message for Tab Scheme Info
        output$no_scheme_info <- renderUI({
          fluidRow(
            column(1),
            column(
              width = 10,
              align = "left",
              p(
                HTML(
                  paste(
                    tags$span(
                      style = 'color: white; font-size: 15px; ',
                      'No scheme available.'
                    )
                  )
                )
              ),
              p(
                HTML(
                  paste(
                    tags$span(
                      style = 'color: white; font-size: 15px; ',
                      'Download a scheme first and type assemblies in the section Allelic Typing.'
                    )
                  )
                )
              )
            )
          )
        })

        # Message for Tab Distance Matrix
        output$no_scheme_distancematrix <- renderUI({
          fluidRow(
            column(1),
            column(
              width = 10,
              align = "left",
              p(
                HTML(
                  paste(
                    tags$span(
                      style = 'color: white; font-size: 15px; ',
                      'No scheme available.'
                    )
                  )
                )
              ),
              p(
                HTML(
                  paste(
                    tags$span(
                      style = 'color: white; font-size: 15px; ',
                      'Download a scheme first and type assemblies in the section Allelic Typing.'
                    )
                  )
                )
              )
            )
          )
        })
      } else {
        output$no_scheme_entries <- NULL
        output$no_scheme_info <- NULL
        output$no_scheme_distancematrix <- NULL
      }
    }
  })

  observe({
    # Conditional Missing Values Tab
    if (!is.null(DB$allelic_profile)) {
      if (anyNA(DB$allelic_profile)) {
        if (isFALSE(DB$no_na_switch)) {
          output$menu_typing <- renderMenu(
            sidebarMenu(
              menuItem(
                text = "Database",
                tabName = "database",
                icon = icon("hard-drive"),
                startExpanded = TRUE,
                menuSubItem(
                  text = "Browse Entries",
                  tabName = "db_browse_entries"
                ),
                menuSubItem(
                  text = "Scheme Info",
                  tabName = "db_schemeinfo"
                ),
                menuSubItem(
                  text = "Loci Info",
                  tabName = "db_loci_info"
                ),
                menuSubItem(
                  text = "Distance Matrix",
                  tabName = "db_distmatrix"
                ),
                menuSubItem(
                  text = "Missing Values",
                  tabName = "db_missing_values",
                  selected = TRUE,
                  icon = icon("triangle-exclamation")
                )
              ),
              menuItem(
                text = "Schemes",
                tabName = "init",
                icon = icon("layer-group")
              ),
              menuItem(
                text = "Allelic Typing",
                tabName = "typing",
                icon = icon("gears")
              ),
              menuItem(
                text = "Visualization",
                tabName = "visualization",
                icon = icon("circle-nodes")
              )
            )
          )

          if (!is.null(DB$scheme)) {
            amrfinder_available <- check.amrfinder.available(
              selected_scheme = DB$scheme,
              amrfinder_species = amrfinder_species
            )

            if (!isFALSE(amrfinder_available)) {
              output$menu_screening <- renderMenu(screening_menu_available)
            } else {
              output$menu_screening <- renderMenu(screening_menu_available)
            }
          }
        }
      } else {
        output$menu_typing <- renderMenu(
          sidebarMenu(
            menuItem(
              text = "Database",
              tabName = "database",
              icon = icon("hard-drive"),
              startExpanded = TRUE,
              menuSubItem(
                text = "Browse Entries",
                tabName = "db_browse_entries"
              ),
              menuSubItem(
                text = "Scheme Info",
                tabName = "db_schemeinfo"
              ),
              menuSubItem(
                text = "Loci Info",
                tabName = "db_loci_info"
              ),
              menuSubItem(
                text = "Distance Matrix",
                tabName = "db_distmatrix"
              )
            ),
            menuItem(
              text = "Schemes",
              tabName = "init",
              icon = icon("layer-group")
            ),
            menuItem(
              text = "Allelic Typing",
              tabName = "typing",
              icon = icon("gears")
            ),
            menuItem(
              text = "Visualization",
              tabName = "visualization",
              icon = icon("circle-nodes")
            )
          )
        )

        if (!is.null(DB$scheme)) {
          amrfinder_available <- check.amrfinder.available(
            selected_scheme = DB$scheme,
            amrfinder_species = amrfinder_species
          )

          if (!isFALSE(amrfinder_available)) {
            output$menu_screening <- renderMenu(screening_menu_available)
          } else {
            output$menu_screening <- renderMenu(screening_menu_available)
          }
        }
      }
    }
  })

  observe({
    if (!is.null(DB$available)) {
      output$scheme_db <- renderUI({
        if (length(DB$available) > 3) {
          selectInput(
            "scheme_db",
            label = "",
            choices = if (!is.null(Typing$last_scheme)) {
              Typing$last_scheme
            } else {
              DB$available
            },
            selected = if (!is.null(Typing$last_scheme)) {
              Typing$last_scheme
            } else {
              if (!is.null(DB$scheme)) {
                DB$scheme
              } else {
                DB$available[1]
              }
            }
          )
        } else {
          prettyRadioButtons(
            "scheme_db",
            label = "",
            choices = if (!is.null(Typing$last_scheme)) {
              Typing$last_scheme
            } else {
              DB$available
            },
            selected = if (!is.null(Typing$last_scheme)) {
              Typing$last_scheme
            } else {
              if (!is.null(DB$scheme)) {
                DB$scheme
              } else {
                DB$available[1]
              }
            }
          )
        }
      })

      if (!is.null(DB$schemeinfo)) {
        output$scheme_info <- renderTable(
          {
            DB$schemeinfo
          },
          sanitize.text.function = function(x) x
        )
      } else {
        output$scheme_info <- NULL
      }

      if (!is.null(DB$loci_info)) {
        loci_info <- DB$loci_info
        names(loci_info)[6] <- "Allele Count"

        output$db_loci <- renderDataTable(
          loci_info,
          selection = "single",
          options = list(
            pageLength = 15,
            scrollY = TRUE,
            columnDefs = list(list(
              searchable = TRUE,
              targets = "_all",
              className = "dt-left"
            ))
          )
        )

        output$db_loci_no <- NULL
        output$loci_info_text <- renderUI(
          p(
            HTML(
              paste0(
                '<span style="color: white; font-size: 15px; position:relative; top:25px;">',
                'Information about loci included in the scheme and their respective alleles.',
                '</span>'
              )
            )
          )
        )
      } else {
        if (
          !is.null(DB$scheme_db) &&
            !is.null(DB$scheme) &&
            !is.null(DB$scheme_link)
        ) {
          if (
            !is.null(DB$scheme_db) &&
              !is.null(DB$scheme) &&
              !is.null(DB$scheme_link)
          ) {
            output$db_loci_no <- renderUI(
              p(
                HTML(
                  paste0(
                    '<i class="fa-solid fa-xmark" style="font-size:20px;color:#ff0000; position:relative; top:27px;margin-right: 10px;"></i>',
                    '<span style="color: white; font-size: 15px; position:relative; top:25px;">',
                    'Locus information not available for ',
                    DB$scheme,
                    if (DB$scheme_db == "pubMLST") {
                      paste0(
                        '. Check database online: ',
                        ' <a href="',
                        DB$scheme_link,
                        '" target="_blank" style="color:#008edb; text-decoration:none;">',
                        DB$scheme_db,
                        '</a>.'
                      )
                    },
                    '</span>'
                  )
                )
              )
            )
            output$loci_info_text <- NULL
          }
        }
        output$db_loci <- NULL
      }
    }
  })

  # If only one entry available disable varying loci checkbox

  output$compare_difference_box <- renderUI({
    if (!is.null(DB$data)) {
      if (nrow(DB$data) > 1) {
        div(
          class = "mat-switch-loci",
          materialSwitch(
            "compare_difference",
            h5(
              p("Only Varying Loci"),
              style = "color:white; padding-left: 0px; position: relative; top: -4px; right: -5px;"
            ),
            value = FALSE,
            right = TRUE
          )
        )
      }
    }
  })

  ### Database Events ----

  #### Import & Export dataset ----

  # Pin import
  observeEvent(input$pin_import, {
    req(DB$import, DB$scheme)

    runjs(block_ui)
    show_toast(
      title = "Import of external dataset started",
      type = "info",
      position = "bottom-end",
      timer = 6000
    )

    if (
      !is.null(DB$data) && !is.null(DB$meta) && !is.null(DB$allelic_profile)
    ) {
      # first merge external allelic profile with local allelic profile
      external_allelic_profile <- DB$import[, colnames(DB$allelic_profile)]
      merged_allelic_profile <- add_row(
        DB$allelic_profile,
        external_allelic_profile
      )

      # selection of "Sample ID" as ID column
      id_column <- DB$import[[input$import_id_selector]]

      # selection of meta data to be imported
      if (length(input$import_metadata_sel)) {
        external_meta <- DB$import[,
          !colnames(DB$import) %in%
            colnames(DB$allelic_profile)
        ]
        import_meta <- external_meta[, input$import_metadata_sel]
        if (any(colnames(import_meta) == input$import_id_selector)) {
          import_meta <- select(import_meta, -c(input$import_id_selector))
        }
        merged_meta <- merge_and_fix_types(DB$meta, import_meta)

        # append new external custom metadata
        external_custom_vars <- which(
          !colnames(import_meta) %in%
            colnames(DB$meta)
        )

        if (length(external_custom_vars)) {
          import_meta_ex_cust <- import_meta[, external_custom_vars]

          for (variable in seq_along(colnames(import_meta_ex_cust))) {
            class <- class(import_meta_ex_cust[, variable])
            ifelse(class == "numeric", type <- "cont", type <- "categ")

            DB$cust_var <- add_row(
              DB$cust_var,
              Variable = colnames(import_meta_ex_cust)[variable],
              Type = type
            )

            rownames(DB$cust_var) <- DB$cust_var$Variable
          }
        }
      } else {
        # no metadata to be imported introduce dummy column
        merged_meta <- merge_and_fix_types(
          DB$meta,
          as.data.frame(id_column, nm = "dummy")
        )
        merged_meta <- select(merged_meta, -c("dummy"))
      }

      # Get number of added entries
      nrow_diff <- nrow(merged_meta) - nrow(DB$meta)

      # Set index column
      merged_meta$Index <- as.character(1:nrow(merged_meta))

      # Set include column of external data to TRUE
      merged_meta$Include[
        (nrow(DB$meta) + 1):(nrow(DB$meta) + nrow_diff)
      ] <- TRUE

      # Set Assembly ID column with external id and optional suffix
      ifelse(
        is.null(input$imp_id_suffix),
        suffix <- "",
        suffix <- input$imp_id_suffix
      )

      merged_meta$`Assembly ID`[
        (nrow(DB$meta) + 1):(nrow(DB$meta) + nrow_diff)
      ] <- paste0(id_column, suffix)

      # Set Assembly Name column with external id and optional suffix
      if (
        any(is.na(merged_meta$`Assembly Name`)) ||
          any(merged_meta$`Assembly Name` == "") ||
          any(duplicated(merged_meta$`Assembly Name`))
      ) {
        merged_meta$`Assembly Name`[
          (nrow(DB$meta) + 1):(nrow(DB$meta) + nrow_diff)
        ] <- paste0(id_column, suffix)

        if (any(duplicated(merged_meta$`Assembly Name`))) {
          merged_meta$`Assembly Name`[which(
            duplicated(merged_meta$`Assembly Name`)
          )] <- paste0(
            merged_meta$`Assembly Name`[which(
              duplicated(merged_meta$`Assembly Name`)
            )],
            "_dup"
          )
        }
      }

      # Set database column
      merged_meta$Database[
        (nrow(DB$meta) + 1):(nrow(
          DB$meta
        ) +
          nrow_diff)
      ] <- input$import_new_name

      # Set Scheme column of external data to current scheme
      merged_meta$Scheme[
        (nrow(DB$meta) + 1):(nrow(DB$meta) + nrow_diff)
      ] <- gsub(" ", "_", DB$scheme)
      # Set Entry Date column to current cate
      merged_meta$`Entry Date`[
        (nrow(DB$meta) + 1):(nrow(DB$meta) + nrow_diff)
      ] <- format(Sys.Date())

      # Set Successes & error columns
      errors <- rowSums(is.na(external_allelic_profile))
      successes <- DB$number_loci - rowSums(is.na(external_allelic_profile))
      merged_meta$Errors[
        (nrow(DB$meta) + 1):(nrow(DB$meta) + nrow_diff)
      ] <- errors
      merged_meta$Successes[
        (nrow(DB$meta) + 1):(nrow(DB$meta) + nrow_diff)
      ] <- successes

      # Set Screened to no
      merged_meta$Screened[
        (nrow(DB$meta) + 1):(nrow(DB$meta) + nrow_diff)
      ] <- "NA"

      # Combine merged metadata and allelic profile
      DB$data <- cbind(merged_meta, merged_allelic_profile)

      DB$meta_gs <- select(DB$data, c(1, 3:14))
      DB$meta <- select(DB$data, 1:(14 + nrow(DB$cust_var)))
      DB$meta_true <- DB$meta[which(DB$data$Include == TRUE), ]
      DB$allelic_profile <- select(DB$data, -(1:(14 + nrow(DB$cust_var))))
      DB$allelic_profile_trunc <- as.data.frame(
        lapply(DB$allelic_profile, function(x) sapply(x, truncHash))
      )
      DB$allelic_profile_true <- DB$allelic_profile[
        which(
          DB$data$Include == TRUE
        ),
      ]

      # Deactivate switching to missing values tabs
      DB$no_na_switch <- TRUE

      # Activate database save button
      DB$change <- TRUE

      # UI changes & feedback
      show_toast(
        title = "Import of external dataset successful",
        type = "success",
        position = "bottom-end",
        timer = 6000
      )

      removeModal()
      disable("export_menu")
      disable("import_menu")
      runjs(unblock_ui)
      delay(2000, runjs("highlight_pin();"))
    } else {
      alleles <- list.files(
        file.path(
          Startup$database,
          gsub(" ", "_", DB$scheme),
          paste0(gsub(" ", "_", DB$scheme), "_alleles")
        ),
        full.names = FALSE
      )

      external_allelic_profile <- DB$import[, gsub(
        ".fasta|.fna|.fa",
        "",
        alleles
      )]

      # selection of "Sample ID" as ID column
      id_column <- DB$import[[input$import_id_selector]]

      # selection of meta data to be imported
      dummy_meta <- data.frame(
        Index = 1,
        Include = TRUE,
        `Assembly ID` = "Dummy_ID",
        `Assembly Name` = "Dummy_ID",
        Database = "Dummy_DB",
        Scheme = "Dummy_Scheme",
        `Isolation Date` = format(Sys.Date()),
        Host = "Dummy_Host",
        Country = "Dummy_Country",
        City = "Dummy_City",
        `Entry Date` = format(Sys.Date()),
        Successes = 1L,
        Errors = 1L,
        Screened = "NA"
      )

      meta_names <- c(
        "Index",
        "Include",
        "Assembly ID",
        "Assembly Name",
        "Database",
        "Scheme",
        "Isolation Date",
        "Host",
        "Country",
        "City",
        "Entry Date",
        "Successes",
        "Errors",
        "Screened"
      )

      colnames(dummy_meta) <- meta_names

      if (length(input$import_metadata_sel)) {
        external_meta <- DB$import[,
          !colnames(DB$import) %in%
            gsub(".fasta|.fna|.fa", "", alleles)
        ]

        import_meta <- external_meta[, input$import_metadata_sel]

        if (any(colnames(import_meta) == input$import_id_selector)) {
          import_meta <- select(import_meta, -c(input$import_id_selector))
        }

        merged_meta <- merge_and_fix_types(dummy_meta, import_meta)

        # append new external custom metadata
        external_custom_vars <- which(!colnames(import_meta) %in% meta_names)

        if (length(external_custom_vars)) {
          import_meta_ex_cust <- import_meta[, external_custom_vars]

          for (variable in seq_along(colnames(import_meta_ex_cust))) {
            class <- class(import_meta_ex_cust[, variable])
            ifelse(class == "numeric", type <- "cont", type <- "categ")
            cust_var <- data.frame(Variable = character(), Type = character())
            DB$cust_var <- add_row(
              cust_var,
              Variable = colnames(import_meta_ex_cust)[variable],
              Type = type
            )

            rownames(DB$cust_var) <- DB$cust_var$Variable
          }
        }
      } else {
        # no metadata to be imported introduce dummy column
        merged_meta <- merge_and_fix_types(
          dummy_meta,
          as.data.frame(id_column, nm = "dummy")
        )

        merged_meta <- select(merged_meta, -c("dummy"))
      }

      # Remove dummy entry
      merged_meta <- merged_meta[-1, ]

      # Set index column
      merged_meta$Index <- as.character(1:nrow(merged_meta))

      # Set include column of external data to TRUE
      merged_meta$Include <- TRUE

      merged_meta$`Assembly ID` <- id_column

      # Set Assembly Name column with external id and optional suffix
      if (
        any(is.na(merged_meta$`Assembly Name`)) ||
          any(merged_meta$`Assembly Name` == "") ||
          any(duplicated(merged_meta$`Assembly Name`))
      ) {
        merged_meta$`Assembly Name` <- id_column

        if (any(duplicated(merged_meta$`Assembly Name`))) {
          merged_meta$`Assembly Name`[which(
            duplicated(merged_meta$`Assembly Name`)
          )] <- paste0(
            merged_meta$`Assembly Name`[which(
              duplicated(merged_meta$`Assembly Name`)
            )],
            "_dup"
          )
        }
      }

      # Set database column
      merged_meta$Database <- input$import_new_name

      # Set Scheme column of external data to current scheme
      merged_meta$Scheme <- gsub(" ", "_", DB$scheme)

      # Set Entry Date column to current cate
      merged_meta$`Entry Date` <- format(Sys.Date())

      # Set Successes & error columns
      errors <- rowSums(is.na(external_allelic_profile))
      successes <- DB$number_loci - rowSums(is.na(external_allelic_profile))
      merged_meta$Errors <- as.integer(errors)
      merged_meta$Successes <- as.integer(successes)

      # Set Screened to NA
      merged_meta$Screened <- "NA"

      # Combine merged metadata and allelic profile
      data <- list()
      data[["Typing"]] <- cbind(merged_meta, external_allelic_profile)

      saveRDS(
        data,
        file.path(Startup$database, gsub(" ", "_", DB$scheme), "Typing.rds")
      )
      # UI changes & feedback
      show_toast(
        title = "Import of external dataset successful",
        type = "success",
        position = "bottom-end",
        timer = 6000
      )

      # Deactivate switching to missing values tabs
      DB$no_na_switch <- TRUE

      disable("export_menu")
      disable("import_menu")

      delay(5000, runjs(unblock_ui))
      removeModal()
    }
  })

  # Foreign dataset file upload observer
  observe({
    shinyFileChoose(
      input,
      "import_files",
      roots = c(Home = path_home(), Root = "/"),
      defaultRoot = "Home",
      session = session,
      filetypes = c("csv", "tsv", "txt", "dat", "tab")
    )
  })

  # Render import new name preview
  output$import_new_name_feedback_ui <- renderUI({
    if (!is.null(input$import_files) && length(input$import_files) > 1) {
      if (is.null(input$import_new_name) || nchar(input$import_new_name) < 1) {
        disable("pin_import")
        HTML(
          paste0(
            '&nbsp;&nbsp; <i class="fa-solid fa-circle-xmark" style="font-',
            'size:15px; color:#ff0000; position:relative;"></i> &nbsp;',
            "<b>Database name can't be empty</b>"
          )
        )
      } else if (
        !is.null(DB$data) &&
          any(DB$data$Database == input$import_new_name)
      ) {
        disable("pin_import")
        HTML(
          paste0(
            '&nbsp;&nbsp; <i class="fa-solid fa-circle-xmark" style="font-',
            'size:15px; color:#ff0000; position:relative;"></i> &nbsp;',
            "<b>Database name already present</b>"
          )
        )
      } else {
        enable("pin_import")
        HTML(
          paste0(
            '&nbsp&nbsp; <i class="fa-solid fa-circle-check" style="font-',
            'size:15px; color:#90EE90; position:relative;"></i> &nbsp;',
            "<b>Database name compatible</b>"
          )
        )
      }
    }
  })

  # Render ID column selection preview
  output$id_preview <- renderUI({
    req(input$import_id_selector, DB$import)

    check_pos_ui <- fluidRow(
      column(
        width = 12,
        HTML(
          paste0(
            '&nbsp <i class="fa-solid fa-circle-check" style="font-',
            'size:15px; color:#90EE90; position:relative;"></i> &nbsp;',
            "IDs are compatible<br>",
            "Assembly IDs can be imported."
          )
        )
      )
    )

    if (!is.null(input$import_files) && length(input$import_files) > 1) {
      # get selected ID column
      id_column <- DB$import[[input$import_id_selector]]

      # check if ID's duplicated
      if (any(duplicated(id_column))) {
        disable("pin_import")

        fluidRow(
          column(
            width = 12,
            HTML(
              paste0(
                '&nbsp; <i class="fa-solid fa-circle-xmark" style="font-',
                'size:15px; color:#ff0000; position:relative;"></i> &nbsp;',
                "IDs contain duplicate(s)<br>",
                "All IDs must be unique."
              )
            )
          )
        )

        # check if ID's already present in local
      } else if (!is.null(DB$data)) {
        merged_ids <- c(DB$data$`Assembly ID`, id_column)
        if (any(duplicated(merged_ids))) {
          fluidRow(
            column(
              width = 12,
              uiOutput("imp_id_dup_info"),
              textInput("imp_id_suffix", "", placeholder = "_ext")
            )
          )
        } else {
          enable("pin_import")
          check_pos_ui
        }
      } else {
        enable("pin_import")
        check_pos_ui
      }
    }
  })

  output$imp_id_dup_info <- renderUI({
    req(DB$import, DB$data)

    if (!is.null(input$import_files) && length(input$import_files) > 1) {
      # get selected ID column
      id_column <- DB$import[[input$import_id_selector]]
      merged_ids <- c(
        DB$data$`Assembly ID`,
        paste0(id_column, input$imp_id_suffix)
      )

      if (any(duplicated(merged_ids))) {
        disable("pin_import")

        HTML(
          paste0(
            '&nbsp; <i class="fa-solid fa-circle-exclamation" style=',
            '"font-size:15px; color:orange; position:relative;"></i> &nbsp;',
            sum(duplicated(merged_ids)),
            " ID(s) already exist in database<br><br>",
            "Change ID column or append a suffix:"
          )
        )
      } else {
        enable("pin_import")

        HTML(
          paste0(
            '&nbsp; <i class="fa-solid fa-circle-check" style=',
            '"font-size:15px; color:#90EE90; position:relative;"></i> &nbsp;',
            'Suffix "',
            input$imp_id_suffix,
            '" appended'
          )
        )
      }
    }
  })

  # Render metadata selection preview
  output$metadata_preview <- renderUI({
    req(input$import_metadata_sel, DB$import)

    if (!is.null(input$import_files) && length(input$import_files) > 1) {
      HTML(paste0(
        length(input$import_metadata_sel),
        " metadata variables selected for import"
      ))
    }
  })

  # Construction of import UI elements & checking of uploaded dataset
  observe({
    if (!is.null(input$import_files) && length(input$import_files) > 1) {
      # adjusting UI element highlighting
      runjs(paste0(
        "document.getElementById('import_files').style.ani",
        "mation = 'none';"
      ))
      runjs(block_ui)

      # getting selected file
      import_path <- parseFilePaths(
        roots = c(Home = path_home(), Root = "/"),
        input$import_files
      )
      import_filepath <- truncate_start(basename(import_path$datapath))
      import <- read_delim(
        import_path$datapath,
        delim = detect_delimiter(import_path$datapath),
        show_col_types = FALSE
      )
      DB$import <- import

      # check compatibe loci in imported dataset
      alleles <- list.files(
        file.path(
          Startup$database,
          gsub(" ", "_", DB$scheme),
          paste0(gsub(" ", "_", DB$scheme), "_alleles")
        ),
        full.names = FALSE
      )

      shared_loci <- gsub(".fasta|.fna|.fa", "", alleles) %in% colnames(import)

      # check if import dataset has ID column
      check_id_col <- DB$number_loci != ncol(import)

      if (!any(shared_loci == FALSE) && check_id_col) {
        DB$import_all_meta <- colnames(import)[which(
          !colnames(import) %in% gsub(".fasta|.fna|.fa", "", alleles)
        )]

        ### Preload UI elements
        # Import dataset feedback
        feedback <- HTML(
          paste0(
            '&nbsp;&nbsp; <i class="fa-solid fa-circle-check" style="font',
            '-size:15px; color:#90EE90; position:relative;"></i> &nbsp; <b>',
            'Nomenclature is compatible</b> <br> All loci are matching the ',
            DB$scheme,
            ' scheme.'
          )
        )
        # Delimiter
        delim <- hr()
        # Import database name text input
        placeholder <- NULL
        if (
          any(colnames(import) == "Database") &&
            is.character(import$Database) &&
            length(unique(import$Database)) == 1
        ) {
          import_new_name_val <- import$Database[1]
        } else {
          import_new_name_val <- gsub(
            "\\.(csv|tsv|txt|dat|tab)$",
            "",
            basename(import_path$datapath)
          )

          if (
            !is.null(DB$data) && any(DB$data$Database == import_new_name_val)
          ) {
            import_new_name_val <- ""
            placeholder <- "Database Name"
          }
        }
        import_new_name <- textInput(
          "import_new_name",
          h5(
            "Import database name",
            style = "font-size: 15px; color: white; margin-bottom: 0px;"
          ),
          value = import_new_name_val,
          placeholder = placeholder
        )
        # Import dataset id column selector
        ifelse(
          any("Assembly ID" == DB$import_all_meta),
          id_col_selected <- "Assembly ID",
          id_col_selected <- DB$import_all_meta[1]
        )
        id_sel_ui <- div(
          class = "import-id-sel",
          selectInput(
            "import_id_selector",
            h5(
              "Select ID Column",
              style = "font-size: 15px; color: white; margin-bottom: 0px;"
            ),
            choices = isolate(DB$import_all_meta),
            selected = id_col_selected,
            width = "100%"
          )
        )
        # Metadata import selector
        metadata_sel_ui <- uiOutput("metadata_sel_ui")
        # Start hashing button
        hash_button_ui <- actionButton(
          "import_start_hash",
          "Hash Dataset",
          icon = icon("play")
        )
        # Allele library directory button
        hash_dir_ui <- shinyDirButton(
          "hash_dir_button",
          "Select Alleles",
          icon = icon("folder-open"),
          title = "Select folder containing allele library",
          buttonType = "default",
          root = path_home()
        )

        # check if import allele indexes are hashed
        hash_check <- import[, which(
          colnames(import) %in%
            gsub(".fasta|.fna|.fa", "", alleles)
        )]

        # check which processing necessary
        if (
          (sum(
            (class(unlist(hash_check)) == "character" ||
              is.na(unlist(hash_check))) ==
              FALSE
          ) ==
            0) &&
            !any(
              nchar(
                unlist(hash_check)[which(!is.na(unlist(hash_check)))]
              ) !=
                64
            )
        ) {
          # no processing needed - user feedback
          hash_feedback <- HTML(
            paste0(
              '&nbsp;&nbsp; <i class="fa-solid fa-circle-check" style="fo',
              'nt-size:15px; color:#90EE90; position:relative;"></i> &nbs',
              'p; <b>Alleles are hashed</b> <br> Allele indexes are alrea',
              'dy hashed. No action necessary.'
            )
          )
          hash_button <- disabled(hash_button_ui)
          output$hash_dir <- renderUI(disabled(hash_dir_ui))
          runjs(
            paste0(
              "document.getElementById('pin_import').style.animation =",
              "'pulsate-shadow 2s infinite linear';"
            )
          )
          enable("pin_import")
        } else {
          # check if hashing can be performed on dataset
          if (is_integer_vector(unlist(hash_check))) {
            # hashing processing possible - user feedback
            hash_feedback <- HTML(
              paste0(
                '&nbsp;&nbsp; <i class="fa-solid fa-circle-exclamation" s',
                'tyle="font-size:15px; color:orange; position:relative;">',
                '</i> &nbsp; <b>Allele indexes not hashed</b> <br>To perf',
                'orm hashing upload allele library containing the sequenc',
                'es for each allele variant of every locus.'
              )
            )
            hash_button <- disabled(hash_button_ui)
            output$hash_dir <- renderUI(hash_dir_ui)
            disable("pin_import")
            enable("import_start_hash")
            delay(
              1000,
              runjs(
                paste0(
                  "document.getElementById('hash_dir_button').style.animati",
                  "on = 'pulsate-shadow 2s infinite linear';"
                )
              )
            )
            delay(
              1000,
              runjs(
                paste0(
                  "document.getElementById('import_start_hash').style.anima",
                  "tion = 'none';"
                )
              )
            )
          } else {
            # hashing processing not possible - user feedback
            hash_feedback <- HTML(
              paste0(
                '&nbsp;&nbsp; <i class="fa-solid fa-circle-xmark" style="',
                'font-size:15px; color:#ff0000; position:relative;"></i> ',
                '&nbsp; <b>Allele indexes are faulty</b> <br> Only whole ',
                'numbers are allowed. Import not possible.'
              )
            )
            hash_button <- disabled(hash_button_ui)
            output$hash_dir <- renderUI(disabled(hash_dir_ui))
            disable("pin_import")
            runjs(
              paste0(
                "document.getElementById('import_files').style.animation ",
                "= 'pulsate-shadow 2s infinite linear';"
              )
            )
          }
        }
      } else {
        # No import possible - adjust element rendering

        # Import section
        runjs(
          paste0(
            "document.getElementById('import_files').style.animation = 'p",
            "ulsate-shadow 2s infinite linear';"
          )
        )
        runjs(paste0(
          "document.getElementById('pin_import').style.",
          "animation = 'none';"
        ))

        if (any(shared_loci == FALSE)) {
          feedback <- HTML(
            paste0(
              '&nbsp;&nbsp; <i class="fa-solid fa-circle-xmark" style="fo',
              'nt-size:15px; color:#ff0000; position:relative;"></i> &nbs',
              'p; <b>Nomenclature not compatible</b><br>The loci of the s',
              'elected dataset do not match the currently selected ',
              DB$scheme,
              ' scheme.'
            )
          )
        } else {
          feedback <- HTML(
            paste0(
              '&nbsp;&nbsp; <i class="fa-solid fa-circle-xmark" style="fo',
              'nt-size:15px; color:#ff0000; position:relative;"></i> &nbs',
              'p; <b>No isolate ID detected</b><br> Ensure that the exter',
              'nal data set contains a variable for unique isolate identi',
              'fiers.'
            )
          )
        }
        delim <- NULL
        import_new_name <- NULL

        # Metadata section
        id_sel_ui <- NULL
        metadata_sel_ui <- NULL
        DB$import <- NULL

        # Hashing section
        output$hash_import_button <- NULL
        output$hash_dir <- NULL
        hash_feedback <- NULL
        hash_button <- NULL
        output$hashing_status <- NULL
      }

      runjs(unblock_ui)
    } else {
      # Nothing imported yet - adjust element rendering

      # Import section
      runjs(paste0(
        "document.getElementById('import_files').style.ani",
        "mation = 'pulsate-shadow 2s infinite linear';"
      ))
      runjs(paste0(
        "document.getElementById('pin_import').style.an",
        "imation = 'none';"
      ))
      import_filepath <- "Select file"
      import_new_name <- NULL
      feedback <- HTML("No dataset imported")
      delim <- NULL

      # Metadata section
      id_sel_ui <- NULL
      metadata_sel_ui <- NULL

      # Hashing section
      output$hash_dir <- NULL
      hash_feedback <- NULL
      hash_button <- NULL
      output$hashing_status <- NULL
    }

    # Render elements

    # Import section
    output$import_path <- renderText(import_filepath)
    output$import_new_name_ui <- renderUI(import_new_name)
    output$import_feedback <- renderUI(feedback)
    output$delim <- renderUI(delim)

    # Metadata section
    output$import_id_sel <- renderUI(id_sel_ui)
    output$import_metadata_sel <- renderUI(metadata_sel_ui)
    output$delim2 <- renderUI(delim)

    # Hashing section
    output$hash_feedback <- renderUI(hash_feedback)
    output$hash_import_button <- renderUI(hash_button)
  })

  # Foreign allele library directory selection
  observeEvent(input$hash_dir_button, {
    output$hashing_status <- NULL

    shinyDirChoose_mod(
      input,
      "hash_dir_button",
      roots = c(Home = path_home(), Root = "/"),
      defaultRoot = "Home",
      session = session
    )

    if (length(input$hash_dir_button) <= 1) {
      return({})
    }

    hash_dir <- as.character(
      parseDirPath(
        roots = c(Home = path_home(), Root = "/"),
        input$hash_dir_button
      )
    )

    output$hash_folderpath <- renderUI(HTML(truncate_start(hash_dir)))

    DB$hash_dir <- hash_dir

    # element highlighting adjustment
    runjs(paste0(
      "document.getElementById('hash_dir_button').style.an",
      "imation = 'none';"
    ))
    enable("hash_import_button")
    runjs(paste0(
      "document.getElementById('import_start_hash').style.",
      "animation = 'pulsate-shadow 2s infinite linear';"
    ))
  })

  # Reset hashing UI elements on new dataset upload
  observeEvent(input$import_files, {
    DB$hash_dir <- NULL
    output$hash_folderpath <- NULL
  })

  # Render metadata selection
  output$metadata_sel_ui <- renderUI({
    req(input$import_id_selector, DB$import_all_meta)

    pickerInput(
      "import_metadata_sel",
      h5(
        "Select Metadata",
        style = "font-size: 15px; color: white; margin-bottom: 0px;"
      ),
      multiple = TRUE,
      options = list(
        "live-search" = TRUE,
        "actions-box" = TRUE,
        size = 10,
        style = "background-color: white; border-radius: 5px;"
      ),
      choices = DB$import_all_meta[
        DB$import_all_meta != input$import_id_selector
      ],
      width = "100%"
    )
  })

  # Hash imported allele library
  observeEvent(input$import_start_hash, {
    req(DB$hash_dir, DB$import)
    log_print("Start to hash imported database")

    # check if directory contains complete loci file list
    dir_content <- gsub(
      ".fasta|.fna|.fa",
      "",
      basename(list.files(DB$hash_dir, full.names = FALSE))
    )
    alleles <- list.files(
      file.path(
        Startup$database,
        gsub(" ", "_", DB$scheme),
        paste0(gsub(" ", "_", DB$scheme), "_alleles")
      ),
      full.names = FALSE
    )

    filtered_selection <- which(
      dir_content %in% gsub(".fasta|.fna|.fa", "", alleles)
    )

    if (
      any(
        !gsub(".fasta|.fna|.fa", "", alleles) %in%
          dir_content[filtered_selection]
      )
    ) {
      # status feedback and element highlighting adjustment
      log_print("Allele library has missing loci")
      show_toast(
        title = "Allele library has missing loci",
        type = "error",
        position = "bottom-end",
        timer = 6000
      )
      output$hashing_status <- renderUI(HTML(
        paste0(
          '&nbsp;&nbsp; <i class="fa-solid fa-circle-xmark" style="font',
          '-size:15px; color:#ff0000; position:relative;"></i> &nbsp; <',
          'b>Hashing failed</b><br>Allele library has missing loci'
        )
      ))
      runjs(paste0(
        "document.getElementById('import_start_hash').styl",
        "e.animation = 'none';"
      ))
      runjs(paste0(
        "document.getElementById('hash_dir_button').style.",
        "animation = 'pulsate-shadow 2s infinite linear';"
      ))
      disable("import_start_hash")
    } else {
      # adjust element highlighting
      runjs(block_ui)
      runjs(paste0(
        "document.getElementById('import_start_hash').styl",
        "e.animation = 'none';"
      ))

      tryCatch(
        {
          # hash allele library
          hash_database(DB$hash_dir, filtered = filtered_selection)

          # assign hashes to integer allele indexes
          DB$import <- hash_allele_profile(
            allele_profile = DB$import,
            hashed_loci_folder = DB$hash_dir
          )

          # status feedback and element highlighting adjustment
          log_print("Successful hashing of imported database")
          show_toast(
            title = "Hashing successful!",
            type = "success",
            position = "bottom-end",
            timer = 6000
          )

          output$hashing_status <- renderUI(HTML(
            paste0(
              '&nbsp;&nbsp; <i class="fa-solid fa-circle-check" style="font',
              '-size:15px; color:#90EE90; position:relative;"></i> &nbsp; <',
              'b>Hashing successful</b><br>Proceed to import the dataset'
            )
          ))
          enable("pin_import")
          disable("import_start_hash")
          disable("hash_dir_button")
          runjs(paste0(
            "document.getElementById('pin_import').style.animation = 'pulsate",
            "-shadow 2s infinite linear';"
          ))
        },
        error = function(e) {
          paste("Error: ", e$message)
          log_print("Hashing of imported database failed")
          show_toast(
            title = "Hashing failed",
            type = "error",
            position = "bottom-end",
            timer = 6000
          )

          # status feedback and element highlighting adjustment
          enable("hash_dir_button")
          disable("import_start_hash")
          runjs(paste0(
            "document.getElementById('hash_dir_button').style.animation = 'pulsa",
            "te-shadow 2s infinite linear';"
          ))
          output$hashing_status <- renderUI(HTML(
            paste0(
              '&nbsp;&nbsp; <i class="fa-solid fa-circle-xmark" style="font',
              '-size:15px; color:#ff0000; position:relative;"></i> &nbsp; <',
              'b>Hashing failed</b><br>Dataset can not be imported.'
            )
          ))
        }
      )
    }

    runjs(unblock_ui)
  })

  # Launch import menu
  observeEvent(input$import_menu, {
    # Reset previous UI elements
    output$hash_folderpath <- NULL

    showModal(
      div(
        class = "start-modal",
        modalDialog(
          fluidRow(
            br(),
            column(
              width = 12,
              fluidRow(
                column(
                  width = 6,
                  shinyFilesButton(
                    "import_files",
                    "Select Dataset",
                    icon = icon("file"),
                    title = "Select allelic profile",
                    multiple = TRUE,
                    buttonType = "default",
                    class = NULL,
                    width = "120px",
                    root = path_home()
                  ),
                  textOutput("import_path"),
                  br(),
                  uiOutput("import_new_name_ui")
                ),
                column(
                  width = 6,
                  uiOutput("import_feedback"),
                  br(),
                  br(),
                  uiOutput("import_new_name_feedback_ui")
                )
              ),
              uiOutput("delim"),
              fluidRow(
                column(
                  width = 6,
                  uiOutput("hash_dir"),
                  uiOutput("hash_folderpath"),
                  br(),
                  uiOutput("hash_import_button"),
                  uiOutput("hashing_status")
                ),
                column(
                  width = 6,
                  br(),
                  uiOutput("hash_feedback")
                )
              ),
              uiOutput("delim2"),
              fluidRow(
                column(
                  width = 6,
                  uiOutput("import_id_sel"),
                  uiOutput("id_preview")
                ),
                column(
                  width = 6,
                  uiOutput("import_metadata_sel"),
                  uiOutput("metadata_preview"),
                  br()
                )
              )
            )
          ),
          title = "Import Menu",
          easyClose = TRUE,
          footer = tagList(
            modalButton("Dismiss"),
            disabled(
              actionButton(
                "pin_import",
                "Pin Import",
                icon = icon("thumbtack"),
                class = "btn btn-default"
              )
            )
          )
        )
      )
    )
  })

  # Launch export menu
  observeEvent(input$export_menu, {
    req(DB$meta, DB$allelic_profile)

    showModal(
      div(
        class = "start-modal",
        modalDialog(
          fluidRow(
            br(),
            column(
              width = 12,
              fluidRow(
                column(1),
                column(
                  width = 4,
                  align = "left",
                  HTML(
                    paste0(
                      '<span style="color: white; display: block; font-si',
                      'ze: 16px; margin-left: -15px;"> Only Included Entr',
                      'ies</span>'
                    )
                  ),
                  HTML(
                    paste0(
                      '<span style="margin-top: 31px; color: white; display: b',
                      'lock; font-size: 16px; margin-left: -15px;"> Select Loc',
                      'i </span>'
                    )
                  ),
                  HTML(
                    paste0(
                      '<span style="margin-top: 31px; color: white; display: b',
                      'lock; font-size: 16px; margin-left: -15px;"> Select Met',
                      'adata </span>'
                    )
                  )
                ),
                column(
                  width = 6,
                  align = "left",
                  div(
                    class = "exp-menu-switch2",
                    materialSwitch(
                      "download_table_include",
                      "",
                      value = FALSE
                    )
                  ),
                  div(
                    class = "exp-menu-picker",
                    pickerInput(
                      "exp_loci_select",
                      choices = colnames(DB$allelic_profile),
                      selected = colnames(DB$allelic_profile),
                      multiple = TRUE,
                      options = list(
                        "live-search" = TRUE,
                        "actions-box" = TRUE,
                        size = 10,
                        style = "background-color: white; border-radius: 5px;"
                      )
                    )
                  ),
                  div(
                    class = "exp-menu-picker",
                    pickerInput(
                      "exp_metadata_select",
                      choices = colnames(DB$meta)[-c(2, 3, 12, 13, 14)],
                      multiple = TRUE,
                      selected = c(
                        "Database",
                        "Scheme",
                        "Isolation Date",
                        "Host",
                        "Country",
                        "City",
                        "Entry Date"
                      ),
                      options = list(
                        "live-search" = TRUE,
                        "actions-box" = TRUE,
                        size = 10,
                        style = "background-color: white; border-radius: 5px;"
                      )
                    )
                  ),
                  HTML(
                    paste0(
                      '<span style="margin-top: 12px; font-style: italic; colo',
                      'r: white; display: block; font-size: 13px;"> Assembly I',
                      'D always included</span>'
                    )
                  ),
                  br()
                )
              )
            )
          ),
          title = "Export Menu",
          easyClose = TRUE,
          footer = tagList(
            modalButton("Dismiss"),
            downloadBttn(
              "download_entry_table",
              style = "simple",
              label = "",
              size = "sm",
              icon = icon("download"),
              color = "primary"
            )
          )
        )
      )
    )
  })

  # Shutdown
  observeEvent(input$shutdown, {
    showModal(
      div(
        class = "start-modal",
        modalDialog(
          fluidRow(
            br(),
            column(
              width = 11,
              p(
                HTML(
                  paste0(
                    '<span style="color: white; display: block; font-size: 15px; margin-left: 15px;">',
                    'Are you sure you want to stop the application?',
                    '</span>'
                  )
                )
              )
            ),
            br()
          ),
          title = "Close PhyloTrace",
          easyClose = TRUE,
          footer = tagList(
            modalButton("Dismiss"),
            actionButton(
              "conf_shutdown",
              "Close",
              class = "load-db",
              width = "100px"
            )
          )
        )
      )
    )
  })

  observeEvent(input$conf_shutdown, {
    system(
      paste("bash", shQuote(paste0(getwd(), "/bin/kill_multi.sh"))),
      wait = TRUE
    )
    runjs("window.close();")
    stopApp()
  })

  # Support menu
  observeEvent(input$support_menu, {
    showModal(
      div(
        class = "start-modal",
        modalDialog(
          fluidRow(
            br(),
            column(
              width = 11,
              p(
                HTML(
                  paste0(
                    '<span style="color: white; display: block; font-size: 15px; margin-left: 15px;">',
                    'For help refer to the ',
                    '<a href="https://www.phylotrace.com/user-manual" target="_blank">User Manual</a>',
                    '.',
                    '</span>'
                  )
                )
              ),
              br(),
              p(
                HTML(
                  paste0(
                    '<span style="color: white; display: block; font-size: 15px; margin-left: 15px; display: block">',
                    'If issues persist or for any other inquiries contact us at <a href="mailto:info@phylotrace.com">info@phylotrace.com</a>.',
                    '</span>'
                  )
                )
              )
            ),
            br()
          ),
          title = "Help",
          easyClose = TRUE,
          footer = tagList(
            modalButton("Dismiss")
          )
        )
      )
    )
  })

  # Settings menu
  observeEvent(input$settings_menu, {
    showModal(
      div(
        class = "start-modal",
        modalDialog(
          fluidRow(
            br(),
            column(
              width = 11,
              p(
                HTML(
                  paste0(
                    '<span style="color: white; display: block; font-size: 15px; margin-left: 15px;">',
                    'Coming soon ... ',
                    '</span>'
                  )
                )
              )
            ),
            br()
          ),
          title = "Settings",
          easyClose = TRUE,
          footer = tagList(
            modalButton("Dismiss")
          )
        )
      )
    )
  })

  # Show custom variable table on button input
  observeEvent(input$custom_var_table, {
    showModal(
      div(
        class = "start-modal",
        modalDialog(
          column(
            width = 12,
            div(class = "var-table", dataTableOutput("show_cust_var")),
            br(),
            br()
          ),
          title = "Custom Variables Overview",
          easyClose = TRUE,
          footer = tagList(
            actionButton(
              "dismiss_cust_var_table",
              "Dismiss",
              class = "load-db",
              width = "100px"
            )
          )
        )
      )
    )
  })

  observeEvent(input$dismiss_cust_var_table, {
    removeModal()
  })

  # Invalid entries table input
  observeEvent(input$invalid_date | input$empty_name | input$empty_id, {
    req(DB$data, input$db_entries)

    if (isTRUE(input$invalid_date)) {
      show_toast(
        title = "Invalid date",
        type = "warning",
        position = "bottom-end",
        timer = 6000
      )
      DB$inhibit_change <- TRUE
    } else if (isTRUE(input$empty_name)) {
      show_toast(
        title = "Empty name",
        type = "warning",
        position = "bottom-end",
        timer = 6000
      )
      DB$inhibit_change <- TRUE
    } else if (isTRUE(input$empty_id)) {
      show_toast(
        title = "Empty ID",
        type = "warning",
        position = "bottom-end",
        timer = 6000
      )
      DB$inhibit_change <- TRUE
    } else {
      DB$inhibit_change <- FALSE
    }
  })

  # Change scheme
  observeEvent(input$reload_db, {
    log_print("Input reload_db")

    if (tail(readLines(file.path(logdir, "script_log.txt")), 1) != "0") {
      show_toast(
        title = "Pending Multi Typing",
        type = "warning",
        position = "bottom-end",
        timer = 6000
      )
    } else if (Screening$status == "started") {
      show_toast(
        title = "Pending Screening",
        type = "warning",
        position = "bottom-end",
        timer = 6000
      )
    } else {
      showModal(
        div(
          class = "start-modal",
          modalDialog(
            fluidRow(
              br(),
              column(
                width = 11,
                p(
                  HTML(
                    paste0(
                      '<span style="color: white; display: block; font-size: 15px; margin-left: 15px;">',
                      'Select available scheme',
                      '</span>'
                    )
                  )
                ),
                div(
                  class = "modal-element",
                  selectInput(
                    "scheme_db",
                    label = "",
                    choices = DB$available,
                    selected = DB$scheme
                  )
                ),
                br()
              ),
              br()
            ),
            title = "Load Database",
            easyClose = TRUE,
            footer = tagList(
              modalButton("Dismiss"),
              actionButton("load", "Load", class = "load-db", width = "100px")
            )
          )
        )
      )
    }
  })

  # Create new database
  observe({
    shinyDirChoose(
      input,
      "create_new_db",
      roots = c(Home = path_home(), Root = "/"),
      defaultRoot = "Home",
      session = session
    )

    if (!is.null(input$create_new_db)) {
      DB$new_database <- as.character(
        parseDirPath(
          roots = c(Home = path_home(), Root = "/"),
          input$create_new_db
        )
      )
    }
  })

  # Undo db changes
  observeEvent(input$undo_changes, {
    runjs(block_ui)
    log_print("Input undo_changes")

    DB$inhibit_change <- FALSE

    Data <- readRDS(paste0(
      Startup$database,
      "/",
      gsub(" ", "_", DB$scheme),
      "/Typing.rds"
    ))

    DB$data <- Data[["Typing"]]

    if ((ncol(DB$data) - 14) != DB$number_loci) {
      cust_var <- select(DB$data, 15:(ncol(DB$data) - DB$number_loci))
      DB$cust_var <- data.frame(
        Variable = names(cust_var),
        Type = column_classes(cust_var)
      )
    } else {
      DB$cust_var <- data.frame()
    }

    DB$change <- FALSE
    DB$count <- 0
    DB$no_na_switch <- TRUE
    DB$meta_gs <- select(DB$data, c(1, 3:14))
    DB$meta <- select(DB$data, 1:(14 + nrow(DB$cust_var)))
    DB$meta_true <- DB$meta[which(DB$data$Include == TRUE), ]
    DB$allelic_profile <- select(DB$data, -(1:(14 + nrow(DB$cust_var))))
    DB$allelic_profile_trunc <- as.data.frame(
      lapply(DB$allelic_profile, function(x) sapply(x, truncHash))
    )
    DB$allelic_profile_true <- DB$allelic_profile[
      which(
        DB$data$Include == TRUE
      ),
    ]
    DB$deleted_entries <- character(0)

    observe({
      req(DB$data)

      output$db_entries <- renderRHandsontable({
        w$show()

        tab <- generate_rhandsontable(
          data = DB$data,
          cust_var = DB$cust_var,
          compare_select = input$compare_select,
          allelic_profile = DB$allelic_profile,
          allelic_profile_trunc = DB$allelic_profile_trunc,
          entry_table_height = entry_table_height(),
          country_names = country_names,
          diff_allele = diff_allele(),
          true_rows = true_rows(),
          duplicated_names = duplicated_names(),
          duplicated_ids = duplicated_ids(),
          err_thresh = err_thresh(),
          pinned_entries_highlight = pinned_entries_highlight()
        )

        tab
      })
    })

    delay(1000, runjs("unhighlight_pin();"))
    runjs(unblock_ui)
  })

  observeEvent(input$add_new_variable, {
    log_print("Input add_new_variable")

    if (nchar(input$new_var_name) > 12) {
      log_print("Add variable; max. 10 character")
      show_toast(
        title = "Max. 10 characters",
        type = "warning",
        position = "bottom-end",
        timer = 6000
      )
    } else {
      if (input$new_var_name == "") {
        log_print("Add variable; min. 1 character")
        show_toast(
          title = "Min. 1 character",
          type = "error",
          position = "bottom-end",
          timer = 6000
        )
      } else {
        if (trimws(input$new_var_name) %in% names(DB$meta)) {
          log_print("Add variable; name already existing")
          show_toast(
            title = "Variable name already existing",
            type = "warning",
            position = "bottom-end",
            timer = 6000
          )
        } else {
          showModal(
            div(
              class = "start-modal",
              modalDialog(
                fluidRow(
                  br(),
                  column(
                    width = 11,
                    p(
                      HTML(
                        paste0(
                          '<span style="color: white; display: block; font-size: 15px; margin-left: 15px;">',
                          'Choose variable type',
                          '</span>'
                        )
                      )
                    ),
                    div(
                      class = "modal-element",
                      selectInput(
                        "new_var_type",
                        label = "",
                        choices = c(
                          "Categorical (character)",
                          "Continous (numeric)"
                        )
                      )
                    )
                  ),
                  br()
                ),
                title = "Select Data Type",
                easyClose = TRUE,
                footer = tagList(
                  modalButton("Cancel"),
                  actionButton(
                    "conf_new_var",
                    "Confirm",
                    class = "btn btn-default"
                  )
                )
              )
            )
          )
        }
      }
    }
  })

  observeEvent(input$conf_new_var, {
    runjs(block_ui)
    log_print("Input conf_new_var")

    # User feedback variables
    removeModal()
    DB$count <- DB$count + 1
    DB$change <- TRUE

    # Format variable name
    name <- trimws(input$new_var_name)

    if (input$new_var_type == "Categorical (character)") {
      DB$data <- DB$data %>%
        mutate("{name}" := character(nrow(DB$data)), .after = 14)

      DB$cust_var <- rbind(
        DB$cust_var,
        data.frame(Variable = name, Type = "categ")
      )
    } else {
      DB$data <- DB$data %>%
        mutate("{name}" := numeric(nrow(DB$data)), .after = 14)

      DB$cust_var <- rbind(
        DB$cust_var,
        data.frame(Variable = name, Type = "cont")
      )
    }

    DB$meta_gs <- select(DB$data, c(1, 3:14))
    DB$meta <- select(DB$data, 1:(14 + nrow(DB$cust_var)))
    DB$meta_true <- DB$meta[which(DB$data$Include == TRUE), ]
    DB$allelic_profile <- select(DB$data, -(1:(14 + nrow(DB$cust_var))))
    DB$allelic_profile_trunc <- as.data.frame(
      lapply(DB$allelic_profile, function(x) sapply(x, truncHash))
    )
    DB$allelic_profile_true <- DB$allelic_profile[
      which(
        DB$data$Include == TRUE
      ),
    ]

    log_print(paste0("New custom variable added: ", input$new_var_name))

    show_toast(
      title = paste0("Variable ", trimws(input$new_var_name), " added"),
      type = "success",
      position = "bottom-end",
      timer = 6000
    )

    runjs(unblock_ui)
  })

  observeEvent(input$delete_new_variable, {
    log_print("Input delete_new_variable")

    if (input$del_which_var == "") {
      log_print("Delete custom variables; no custom variable")
      show_toast(
        title = "No custom variables",
        type = "error",
        position = "bottom-end",
        timer = 6000
      )
    } else {
      showModal(
        div(
          class = "start-modal",
          modalDialog(
            fluidRow(
              br(),
              column(
                width = 11,
                p(
                  HTML(
                    paste0(
                      '<span style="color: white; display: block; font-size: 15px; margin-left: 15px;">',
                      "Confirmation will lead to irreversible deletion of the custom ",
                      input$del_which_var,
                      " variable. Continue?",
                      '</span>'
                    )
                  )
                )
              ),
              br()
            ),
            title = "Delete custom variables",
            fade = TRUE,
            easyClose = TRUE,
            footer = tagList(
              modalButton("Cancel"),
              actionButton("conf_var_del", "Delete", class = "btn btn-danger")
            )
          )
        )
      )
    }
  })

  observeEvent(input$conf_var_del, {
    runjs(block_ui)
    log_print("Input conf_var_del")

    DB$change <- TRUE

    removeModal()

    if (!is.null(DB$count) && DB$count >= 1) {
      DB$count <- DB$count - 1
    }

    show_toast(
      title = paste0("Variable ", input$del_which_var, " removed"),
      type = "success",
      position = "bottom-end",
      timer = 6000
    )

    log_print(paste0("Variable ", input$del_which_var, " removed"))

    DB$cust_var <- DB$cust_var[
      -which(
        DB$cust_var$Variable == input$del_which_var
      ),
    ]
    DB$data <- select(DB$data, -(input$del_which_var))
    DB$meta_gs <- select(DB$data, c(1, 3:14))
    DB$meta <- select(DB$data, 1:(14 + nrow(DB$cust_var)))
    DB$meta_true <- DB$meta[which(DB$data$Include == TRUE), ]
    DB$allelic_profile <- select(DB$data, -(1:(14 + nrow(DB$cust_var))))
    DB$allelic_profile_trunc <- as.data.frame(
      lapply(DB$allelic_profile, function(x) sapply(x, truncHash))
    )
    DB$allelic_profile_true <- DB$allelic_profile[
      which(
        DB$data$Include == TRUE
      ),
    ]

    runjs(unblock_ui)
  })

  # Select all button

  observeEvent(input$sel_all_entries, {
    log_print("Input sel_all_entries")

    DB$data$Include <- TRUE
  })

  observeEvent(input$desel_all_entries, {
    log_print("Input desel_all_entries")

    DB$data$Include <- FALSE
  })

  # Switch to entry table

  observeEvent(input$change_entries, {
    log_print("Input change_entries")

    removeModal()
    updateTabItems(session, "tabs", selected = "db_browse_entries")
  })

  #### Save Missing Value as CSV ----

  output$download_na_matrix <- downloadHandler(
    filename = function() {
      log_print(paste0(
        "Save missing values table ",
        paste0(
          Sys.Date(),
          "_",
          gsub(" ", "_", DB$scheme),
          "_Missing_Values.csv"
        )
      ))
      paste0(Sys.Date(), "_", gsub(" ", "_", DB$scheme), "_Missing_Values.csv")
    },
    content = function(file) {
      download_matrix <- hot_to_r(input$table_missing_values)
      write.csv(
        download_matrix,
        file,
        sep = ",",
        row.names = FALSE,
        quote = FALSE
      )
    }
  )

  #### Save scheme info table as CSV ----

  output$download_schemeinfo <- downloadHandler(
    filename = function() {
      log_print(paste0(
        "Save scheme info table ",
        paste0(gsub(" ", "_", DB$scheme), "_scheme.csv")
      ))

      paste0(gsub(" ", "_", DB$scheme), "_Scheme_Info.csv")
    },
    content = function(file) {
      if (any(DB$schemeinfo[, 1] == "Publications")) {
        pub_index <- which(DB$schemeinfo[, 1] == "Publications")
        write.table(
          DB$schemeinfo[1:(pub_index - 1), ],
          file,
          sep = ";",
          row.names = FALSE,
          quote = FALSE
        )
      } else {
        write.table(
          DB$schemeinfo,
          file,
          sep = ";",
          row.names = FALSE,
          quote = FALSE
        )
      }
    }
  )

  #### Save Loci info table as CSV ----

  output$download_loci_info <- downloadHandler(
    filename = function() {
      log_print(paste0(
        "Save loci info table ",
        paste0(gsub(" ", "_", DB$scheme), "_Loci.csv")
      ))

      paste0(gsub(" ", "_", DB$scheme), "_Loci.csv")
    },
    content = function(file) {
      write.table(
        DB$loci_info,
        file,
        sep = ";",
        row.names = FALSE,
        quote = FALSE
      )
    }
  )

  #### Save entry table as CSV ----

  output$download_entry_table <- downloadHandler(
    filename = function() {
      log_print(paste0(
        "Save entry table ",
        paste0(Sys.Date(), "_", gsub(" ", "_", DB$scheme), "_Entries.csv")
      ))

      paste0(Sys.Date(), "_", gsub(" ", "_", DB$scheme), "_Entries.csv")
    },
    content = function(file) {
      runjs(block_ui)

      if (isTRUE(input$download_table_include)) {
        export <- DB$data[which(DB$data$Include == TRUE), ]
      } else {
        export <- DB$data
      }

      export <- select(
        export,
        -colnames(DB$allelic_profile)[
          !colnames(
            DB$allelic_profile
          ) %in%
            input$exp_loci_select
        ]
      )

      export <- select(
        export,
        -colnames(
          DB$meta
        )[-3][!colnames(DB$meta)[-3] %in% input$exp_metadata_select]
      )

      write.csv(export, file, row.names = FALSE, quote = FALSE)

      runjs(unblock_ui)

      removeModal()
    }
  )

  # Save Edits Button

  observeEvent(input$edit_button, {
    if (nrow(hot_to_r(input$db_entries)) > nrow(DB$data)) {
      show_toast(
        title = "Invalid rows entered. Saving not possible.",
        type = "error",
        position = "bottom-end",
        timer = 6000
      )
    } else {
      if (!isTRUE(DB$inhibit_change)) {
        log_print("Input edit_button")

        showModal(
          div(
            class = "start-modal",
            modalDialog(
              if (length(DB$deleted_entries > 0)) {
                fluidRow(
                  br(),
                  column(
                    width = 11,
                    p(
                      HTML(
                        paste0(
                          '<span style="color: white; display: block; font-size: 15px; margin-left: 15px;">',
                          "Overwriting previous metadata of local ",
                          DB$scheme,
                          " database. Deleted entries will be irreversibly removed. Continue?",
                          '</span>'
                        )
                      )
                    )
                  ),
                  br()
                )
              } else {
                fluidRow(
                  br(),
                  column(
                    width = 11,
                    p(
                      HTML(
                        paste0(
                          '<span style="color: white; display: block; font-size: 15px; margin-left: 15px;">',
                          "Overwriting previous metadata of local ",
                          DB$scheme,
                          " database. Continue?",
                          '</span>'
                        )
                      )
                    )
                  ),
                  br()
                )
              },
              title = "Save Database",
              fade = TRUE,
              easyClose = TRUE,
              footer = tagList(
                modalButton("Cancel"),
                actionButton("conf_db_save", "Save", class = "btn btn-default")
              )
            )
          )
        )
      } else {
        log_print("Input edit_button, invalid values.")
        show_toast(
          title = "Invalid values entered. Saving not possible.",
          type = "error",
          position = "bottom-end",
          timer = 6000
        )
      }
    }
  })

  observeEvent(input$Cancel, {
    log_print("Input Cancel")
    removeModal()
  })

  observeEvent(input$conf_db_save, {
    runjs(block_ui)
    log_print("Input conf_db_save")

    # Remove isolate assembly file if present
    if (!is.null(DB$remove_iso)) {
      if (length(DB$remove_iso) > 0) {
        lapply(
          DB$remove_iso,
          unlink,
          recursive = TRUE,
          force = FALSE,
          expand = TRUE
        )
      }
    }
    DB$remove_iso <- NULL

    # Load currently saved entry table
    Data <- readRDS(file.path(
      Startup$database,
      gsub(" ", "_", DB$scheme),
      "Typing.rds"
    ))

    meta_hot <- hot_to_r(input$db_entries) %>%
      select(1:(14 + nrow(DB$cust_var))) %>%
      mutate(Index = as.character(1:nrow(DB$data)))

    Data[["Typing"]] <- mutate(
      DB$allelic_profile,
      meta_hot,
      .before = 1
    )

    # Ensure correct logical data type & index
    Data[["Typing"]][["Include"]] <- as.logical(Data[["Typing"]][["Include"]])
    rownames(Data[["Typing"]]) <- Data[["Typing"]]$Index

    saveRDS(
      Data,
      file.path(Startup$database, gsub(" ", "_", DB$scheme), "Typing.rds")
    )

    DB$data <- Data[["Typing"]]

    if ((ncol(DB$data) - 14) != DB$number_loci) {
      cust_var <- select(DB$data, 15:(ncol(DB$data) - DB$number_loci))
      DB$cust_var <- data.frame(
        Variable = names(cust_var),
        Type = column_classes(cust_var)
      )
    } else {
      DB$cust_var <- data.frame()
    }

    DB$change <- FALSE
    DB$count <- 0
    DB$no_na_switch <- TRUE
    DB$meta_gs <- select(DB$data, c(1, 3:14))
    DB$meta <- select(DB$data, 1:(14 + nrow(DB$cust_var)))
    DB$meta_true <- DB$meta[which(DB$data$Include == TRUE), ]
    DB$allelic_profile <- select(DB$data, -(1:(14 + nrow(DB$cust_var))))
    DB$allelic_profile_trunc <- as.data.frame(
      lapply(DB$allelic_profile, function(x) sapply(x, truncHash))
    )
    DB$allelic_profile_true <- DB$allelic_profile[
      which(
        DB$data$Include == TRUE
      ),
    ]
    DB$deleted_entries <- character(0)

    removeModal()
    show_toast(
      title = "Database successfully saved",
      type = "success",
      position = "bottom-end",
      timer = 4000
    )

    delay(1000, runjs("unhighlight_pin();"))
    runjs(unblock_ui)
  })

  observeEvent(input$del_button, {
    log_print("Input del_button")

    if (length(input$select_delete) < 1) {
      log_print("Delete entries; no entry selected")
      show_toast(
        title = "No entry selected",
        type = "warning",
        position = "bottom-end",
        timer = 4000
      )
    } else {
      if ((length(input$select_delete) - nrow(DB$data)) == 0) {
        showModal(
          div(
            class = "start-modal",
            modalDialog(
              fluidRow(
                br(),
                column(
                  width = 11,
                  p(
                    HTML(
                      paste0(
                        '<span style="color: white; display: block; font-size: 15px; margin-left: 15px;">',
                        "Deleting will lead to removal of <strong>ALL entries</strong> and assemblies from local ",
                        DB$scheme,
                        " database. The data can not be recovered afterwards. Continue?",
                        '</span>'
                      )
                    )
                  )
                ),
                br()
              ),
              easyClose = TRUE,
              title = "Deleting Entries",
              footer = tagList(
                modalButton("Cancel"),
                actionButton("conf_delete_all", "Delete")
              )
            )
          )
        )
      } else {
        showModal(
          div(
            class = "start-modal",
            modalDialog(
              fluidRow(
                br(),
                column(
                  width = 11,
                  p(
                    HTML(
                      paste0(
                        '<span style="color: white; display: block; font-size: 15px; margin-left: 15px;">',
                        "Confirmation will lead to irreversible removal of selected entries and the respectively saved assembly. Continue?",
                        '</span>'
                      )
                    )
                  )
                ),
                br()
              ),
              title = "Deleting Entries",
              fade = TRUE,
              easyClose = TRUE,
              footer = tagList(
                modalButton("Cancel"),
                actionButton(
                  "conf_delete",
                  "Delete"
                )
              )
            )
          )
        )
      }
    }
  })

  observeEvent(input$conf_delete_all, {
    runjs(block_ui)
    log_print("Input conf_delete_all")

    # remove file with typing data
    file.remove(file.path(
      Startup$database,
      gsub(" ", "_", DB$scheme),
      "Typing.rds"
    ))
    unlink(
      file.path(Startup$database, gsub(" ", "_", DB$scheme), "Isolates"),
      recursive = TRUE,
      force = FALSE,
      expand = TRUE
    )

    showModal(
      div(
        class = "start-modal",
        modalDialog(
          fluidRow(
            br(),
            column(
              width = 11,
              p(
                HTML(
                  paste0(
                    '<span style="color: white; display: block; font-size: 15px; margin-left: 15px;">',
                    'All entries have been removed. Select a local database to load.',
                    '</span>'
                  )
                )
              ),
              div(
                class = "modal-element",
                selectInput(
                  "scheme_db",
                  label = "",
                  choices = if (!is.null(Typing$last_scheme)) {
                    Typing$last_scheme
                  } else {
                    DB$available
                  },
                  selected = if (!is.null(Typing$last_scheme)) {
                    Typing$last_scheme
                  } else {
                    if (!is.null(DB$scheme)) {
                      DB$scheme
                    } else {
                      DB$available[1]
                    }
                  }
                )
              )
            ),
            br()
          ),
          title = "Load Database",
          footer = tagList(
            actionButton("load", "Load", class = "load-db", width = "100px")
          )
        )
      )
    )

    runjs(unblock_ui)
  })

  DB$deleted_entries <- character(0)

  observeEvent(input$conf_delete, {
    runjs(block_ui)
    log_print("Input conf_delete")

    # Get isolates selected for deletion
    DB$deleted_entries <- append(
      DB$deleted_entries,
      DB$data$Index[as.numeric(input$select_delete)]
    )

    # Set reactive status variables
    DB$no_na_switch <- TRUE
    DB$change <- TRUE
    DB$check_new_entries <- FALSE

    # Set isolate directory deletion variables
    isopath <- dir_ls(file.path(
      Startup$database,
      gsub(" ", "_", DB$scheme),
      "Isolates"
    ))

    DB$remove_iso <- isopath[which(
      basename(isopath) %in%
        DB$data$`Assembly ID`[as.numeric(
          input$select_delete
        )]
    )]

    # Reload updated database reactive variables
    DB$data <- DB$data[!(DB$data$Index %in% as.numeric(input$select_delete)), ]
    DB$meta_gs <- select(DB$data, c(1, 3:14))
    DB$meta <- select(DB$data, 1:(14 + nrow(DB$cust_var)))
    DB$meta_true <- DB$meta[which(DB$data$Include == TRUE), ]
    DB$allelic_profile <- select(DB$data, -(1:(14 + nrow(DB$cust_var))))
    DB$allelic_profile_trunc <- as.data.frame(
      lapply(DB$allelic_profile, function(x) sapply(x, truncHash))
    )
    DB$allelic_profile_true <- DB$allelic_profile[
      which(
        DB$data$Include == TRUE
      ),
    ]

    # User feedback
    removeModal()

    if (length(input$select_delete) > 1) {
      show_toast(
        title = "Entries deleted",
        type = "success",
        position = "bottom-end",
        timer = 4000
      )
    } else {
      show_toast(
        title = "Entry deleted",
        type = "success",
        position = "bottom-end",
        timer = 4000
      )
    }

    runjs(unblock_ui)
  })

  ### Distance Matrix ----

  hamming_df <- reactive({
    req(DB$data, DB$allelic_profile)

    if (isTRUE(input$distmatrix_true)) {
      if (isTRUE(any(DB$data$Include == TRUE))) {
        if (anyNA(DB$allelic_profile)) {
          if (input$na_handling == "omit") {
            allelic_profile_noNA <- DB$allelic_profile[,
              colSums(
                is.na(DB$allelic_profile)
              ) ==
                0
            ]

            allelic_profile_noNA_true <- allelic_profile_noNA[
              which(
                DB$data$Include == TRUE
              ),
            ]

            hamming_mat <- compute.distMatrix(
              allelic_profile_noNA_true,
              hamming.dist
            )
          } else if (input$na_handling == "ignore_na") {
            hamming_mat <- compute.distMatrix(
              DB$allelic_profile_true,
              hamming.distIgnore
            )
          } else {
            hamming_mat <- compute.distMatrix(
              DB$allelic_profile_true,
              hamming.distCategory
            )
          }
        } else {
          hamming_mat <- compute.distMatrix(
            DB$allelic_profile_true,
            hamming.dist
          )
        }
      } else {
        show_toast(
          title = "No isolates selected",
          type = "warning",
          position = "bottom-end",
          timer = 2000
        )
        return(NULL)
      }
    } else {
      if (anyNA(DB$allelic_profile)) {
        if (input$na_handling == "omit") {
          allelic_profile_noNA <- DB$allelic_profile[,
            colSums(
              is.na(DB$allelic_profile)
            ) ==
              0
          ]
          hamming_mat <- compute.distMatrix(allelic_profile_noNA, hamming.dist)
        } else if (input$na_handling == "ignore_na") {
          hamming_mat <- compute.distMatrix(
            DB$allelic_profile,
            hamming.distIgnore
          )
        } else {
          hamming_mat <- compute.distMatrix(
            DB$allelic_profile,
            hamming.distCategory
          )
        }
      } else {
        hamming_mat <- compute.distMatrix(DB$allelic_profile, hamming.dist)
      }

      # Extreme values for distance matrix heatmap display
      DB$matrix_min <- min(hamming_mat, na.rm = TRUE)
      DB$matrix_max <- max(hamming_mat, na.rm = TRUE)

      if (isFALSE(input$distmatrix_triangle)) {
        hamming_mat[upper.tri(hamming_mat, diag = !input$distmatrix_diag)] <- NA
      }

      # Row- and colnames change
      if (isTRUE(input$distmatrix_true)) {
        rownames(hamming_mat) <- unlist(
          DB$data[input$distmatrix_label][which(DB$data$Include == TRUE), ]
        )
      } else {
        rownames(hamming_mat) <- unlist(DB$data[input$distmatrix_label])
      }
      colnames(hamming_mat) <- rownames(hamming_mat)

      mode(hamming_mat) <- "integer"

      DB$ham_matrix <- hamming_mat %>%
        as.data.frame() %>%
        mutate(Index = colnames(hamming_mat)) %>%
        relocate(Index)

      return(DB$ham_matrix)
    }
  })

  output$download_distmatrix <- downloadHandler(
    filename = function() {
      paste0(Sys.Date(), "_", gsub(" ", "_", DB$scheme), "_Distance_Matrix.csv")
    },
    content = function(file) {
      download_matrix <- hot_to_r(input$db_distancematrix)
      download_matrix[is.na(download_matrix)] <- ""
      write.csv(download_matrix, file, row.names = FALSE, quote = FALSE)
    }
  )

  # _______________________ ####

  ## Locus sequences ----

  observe({
    if (!is.null(Startup$database) && !is.null(DB$scheme)) {
      DB$loci <- list.files(
        path = paste0(
          Startup$database,
          "/",
          gsub(" ", "_", DB$scheme),
          "/",
          gsub(" ", "_", DB$scheme),
          "_alleles"
        ),
        pattern = "\\.(fasta|fa|fna)$",
        full.names = TRUE
      )
    }
  })

  observe({
    req(DB$data)
    if (!is.null(DB$loci_info)) {
      loci_info <- DB$loci_info
      names(loci_info)[6] <- "Allele Count"

      output$db_loci <- renderDataTable(
        loci_info,
        selection = "single",
        options = list(
          pageLength = 15,
          scrollY = TRUE,
          columnDefs = list(list(searchable = TRUE, targets = "_all"))
        )
      )

      output$db_loci_no <- NULL
      output$loci_info_text <- renderUI(
        p(
          HTML(
            paste0(
              '<span style="color: white; font-size: 15px; position:relative; top:25px;">',
              'Information about loci included in the scheme and their respective alleles.',
              '</span>'
            )
          )
        )
      )
    } else {
      if (
        !is.null(DB$scheme_db) &&
          !is.null(DB$scheme) &&
          !is.null(DB$scheme_link)
      ) {
        output$db_loci_no <- renderUI(
          p(
            HTML(
              paste0(
                '<i class="fa-solid fa-xmark" style="font-size:20px;color:#ff0000; position:relative; top:27px;margin-right: 10px;"></i>',
                '<span style="color: white; font-size: 15px; position:relative; top:25px;">',
                'Locus information not available for ',
                DB$scheme,
                '. Check database online: ',
                ' <a href="',
                DB$scheme_link,
                '" target="_blank" style="color:#008edb; text-decoration:none;">',
                DB$scheme_db,
                '</a>.',
                '</span>'
              )
            )
          )
        )

        output$loci_info_text <- NULL
      }
      output$db_loci <- NULL
    }
  })

  output$loci_sequences <- renderUI({
    if (!is.null(DB$loci_info)) {
      req(
        input$db_loci_rows_selected,
        Startup$database,
        DB$scheme,
        input$seq_sel
      )

      DB$loci <- list.files(
        path = paste0(
          Startup$database,
          "/",
          gsub(" ", "_", DB$scheme),
          "/",
          gsub(" ", "_", DB$scheme),
          "_alleles"
        ),
        pattern = "\\.(fasta|fa|fna)$",
        full.names = TRUE
      )

      fasta <- format_fasta(DB$loci[input$db_loci_rows_selected])
      select <- which(
        fasta ==
          paste0(">", gsub("Allele ", "", sub(" -.*", "", input$seq_sel)))
      ) +
        1

      if (length(select) > 0) {
        seq <- fasta[[
          which(
            fasta ==
              paste0(">", gsub("Allele ", "", sub(" -.*", "", input$seq_sel)))
          ) +
            1
        ]]

        DB$seq <- seq

        div(
          class = "loci-sequence-column",
          column(
            width = 12,
            br(),
            br(),
            HTML(
              paste(
                tags$span(
                  style = 'color: white; font-size: 13px; position:relative; left: 3px',
                  sub(" -.*", "", input$seq_sel)
                )
              )
            ),
            tags$pre(HTML(color_sequence(seq)), class = "sequence")
          )
        )
      } else {
        NULL
      }
    } else {
      NULL
    }
  })

  output$sequence_selector <- renderUI({
    if (!is.null(input$db_loci_rows_selected) && !is.null(DB$loci_info)) {
      req(input$db_loci_rows_selected, Startup$database, DB$scheme)

      DB$loci <- list.files(
        path = paste0(
          Startup$database,
          "/",
          gsub(" ", "_", DB$scheme),
          "/",
          gsub(" ", "_", DB$scheme),
          "_alleles"
        ),
        pattern = "\\.(fasta|fa|fna)$",
        full.names = TRUE
      )

      fasta <- format_fasta(DB$loci[input$db_loci_rows_selected])

      seq_names <- c()
      for (i in seq_along(fasta)) {
        if (startsWith(fasta[[i]], ">")) {
          name <- sub(">", "", fasta[[i]])
          seq_names <- c(seq_names, name)
        }
      }

      var_count <- table(
        DB$allelic_profile[gsub(
          ".fasta",
          "",
          (basename(DB$loci[input$db_loci_rows_selected]))
        )]
      )

      vec <- prop.table(var_count)

      perc <- sapply(unname(vec), scales::percent, accuracy = 0.1)

      names(perc) <- names(vec)

      choices <- seq_names

      present <- which(choices %in% names(vec))
      absent <- which(!(choices %in% names(vec)))

      choices[present] <- paste0(
        "Allele ",
        choices[present],
        " - ",
        unname(var_count),
        " times in DB (",
        unname(perc),
        ")"
      )

      choices[absent] <- paste0("Allele ", choices[absent], " - not present")

      choices <- c(choices[present], choices[absent])

      names(choices) <- sapply(choices, function(x) {
        x <- strsplit(x, " ")[[1]]
        x[2] <- paste0(
          substr(x[2], 1, 4),
          "...",
          substr(x[2], nchar(x[2]) - 3, nchar(x[2]))
        )
        paste(x, collapse = " ")
      })

      box(
        solidHeader = TRUE,
        status = "primary",
        width = "100%",
        title = "Select Allele",
        column(
          width = 12,
          br(),
          fluidRow(
            column(1),
            column(
              width = 10,
              align = "left",
              selectInput(
                "seq_sel",
                "",
                choices = choices,
                width = "85%"
              )
            )
          ),
          br(),
          br(),
          fluidRow(
            column(1),
            column(
              width = 3,
              align = "left",
              actionButton("copy_seq", "Sequence", icon = icon("copy")),
              bsTooltip(
                "copy_seq",
                "Copy the allele sequence <br> to clipboard",
                placement = "bottom",
                trigger = "hover"
              )
            ),
            column(
              width = 3,
              align = "left",
              actionButton("copy_hash", "Hash", icon = icon("copy")),
              bsTooltip(
                "copy_hash",
                "Copy the allele<br>hash to clipboard",
                placement = "bottom",
                trigger = "hover"
              )
            ),
            column(
              width = 3,
              align = "left",
              downloadBttn(
                "get_locus",
                style = "simple",
                label = "Locus",
                size = "sm",
                icon = icon("download")
              ),
              bsTooltip(
                "get_locus_bttn",
                "Save locus file as FASTA",
                placement = "bottom",
                trigger = "hover"
              )
            )
          ),
          br()
        )
      )
    }
  })

  observeEvent(input$copy_seq, {
    if (!is.null(DB$seq)) {
      session$sendCustomMessage("txt", DB$seq)
    }
    show_toast(
      title = "Copied sequence",
      type = "success",
      position = "bottom-end",
      timer = 2000
    )
  })

  observeEvent(input$copy_hash, {
    if (!is.null(input$seq_sel)) {
      session$sendCustomMessage(
        "txt",
        gsub("Allele ", "", sub(" -.*", "", input$seq_sel))
      )
    }
    show_toast(
      title = "Copied hash",
      type = "success",
      position = "bottom-end",
      timer = 2000
    )
  })

  output$get_locus <- downloadHandler(
    filename = function() {
      fname <- basename(DB$loci[input$db_loci_rows_selected])
      log_print(paste0("Get locus fasta ", fname))
      fname
    },
    content = function(file) {
      cont <- readLines(DB$loci[input$db_loci_rows_selected])
      writeLines(cont, file)
    }
  )

  # _______________________ ####

  ## Download cgMLST ----

  ### Manage Schemes UI Elements ----

  ### Update scheme links based on selection ----
  observe({
    req(input$select_cgmlst)

    if (grepl("_PM", input$select_cgmlst)) {
      Scheme$link_scheme <- schemes$url[schemes$species == input$select_cgmlst]
      Scheme$link_cgmlst <- schemes$url[schemes$species == input$select_cgmlst]
      Scheme$link_targets <- NA
      Scheme$folder_name <-
        schemes$species[schemes$species == input$select_cgmlst]
    } else if (grepl("_CM", input$select_cgmlst)) {
      Scheme$link_scheme <- paste0(
        schemes$url[schemes$species == input$select_cgmlst],
        "/"
      )
      Scheme$link_cgmlst <- paste0(
        schemes$url[schemes$species == input$select_cgmlst],
        "/alleles/"
      )
      Scheme$link_targets <- paste0(
        schemes$url[schemes$species == input$select_cgmlst],
        "/locus/?content-type=csv"
      )
      Scheme$folder_name <-
        schemes$species[schemes$species == input$select_cgmlst]
    }
  })

  ### Download Scheme ----
  observeEvent(input$download_cgMLST, {
    runjs(block_ui)
    log_print(paste0("Started download of scheme for ", Scheme$folder_name))

    show_toast(
      title = paste("Download of", input$select_cgmlst, "started"),
      type = "info",
      position = "bottom-start",
      timer = 5000
    )

    shinyjs::hide("download_cgMLST")
    shinyjs::show("downloading")

    # Disable pickerInput
    runjs("$('#select_cgmlst').prop('disabled', true);")
    runjs("$('#select_cgmlst').selectpicker('refresh');")

    if (length(DB$available) == 0) {
      saveRDS(DB$new_database, file.path(app_local_share_path, "new_db.rds"))
      dir.create(
        file.path(
          readRDS(file.path(app_local_share_path, "new_db.rds")),
          "Database"
        ),
        recursive = TRUE
      )
    }

    DB$load_selected <- TRUE

    # Check if .downloaded_schemes folder exists and if not create it
    if (!dir.exists(file.path(Startup$database, ".downloaded_schemes"))) {
      dir.create(
        file.path(Startup$database, ".downloaded_schemes"),
        recursive = TRUE
      )
    }

    # Check if remains of old temporary folder exists and remove them
    if (
      dir.exists(file.path(
        Startup$database,
        Scheme$folder_name,
        paste0(Scheme$folder_name, ".tmp")
      ))
    ) {
      unlink(
        file.path(
          Startup$database,
          Scheme$folder_name,
          paste0(Scheme$folder_name, ".tmp")
        ),
        recursive = TRUE
      )
    }

    ### Download Loci Fasta Files

    options(timeout = 600)

    # Create a Progress object
    progress <- shiny::Progress$new()
    on.exit(progress$close())

    progress$set(message = "Download Scheme", value = 0)

    tryCatch(
      {
        if (grepl("_PM", input$select_cgmlst)) {
          download.alleles.PM(
            url_link = schemes$url[schemes$species == input$select_cgmlst],
            database = Startup$database,
            folder_name = Scheme$folder_name,
            progress = progress
          )
          "Download successful!"
        } else if (grepl("_CM", input$select_cgmlst)) {
          download.alleles.CM(
            url_link = schemes$url[schemes$species == input$select_cgmlst],
            database = Startup$database,
            folder_name = Scheme$folder_name,
            progress = progress
          )
          "Download successful!"
        }
      },
      error = function(e) {
        paste("Error: ", e$message)
      }
    )

    # Unzip the scheme in temporary folder
    unzip(
      zipfile = file.path(
        Startup$database,
        ".downloaded_schemes",
        paste0(Scheme$folder_name, ".zip")
      ),
      exdir = file.path(
        Startup$database,
        Scheme$folder_name,
        paste0(Scheme$folder_name, ".tmp")
      )
    )

    # Download species info & image
    if (!is.null(Scheme$species_data)) {
      if (length(Scheme$species_data) > 0) {
        saveRDS(
          Scheme$species_data,
          file.path(
            Startup$database,
            schemes$species[schemes$species == input$select_cgmlst],
            paste0(
              schemes$species[schemes$species == input$select_cgmlst],
              ".rds"
            )
          )
        )

        if (length(Scheme$species_data) > 1) {
          for (i in seq_along(Scheme$species_data)) {
            # Download image
            destination_file <- file.path(
              Startup$database,
              input$select_cgmlst,
              paste0(
                gsub(" ", "_", Scheme$species_data[[i]]$Name$name),
                ".jpg"
              )
            )
            response_download <- httr::GET(Scheme$species_data[[i]]$Image)

            if (response_download$status_code == 200) {
              writeBin(
                httr::content(response_download, "raw"),
                destination_file
              )
              response_download <- NULL
              log_print("Image downloaded successfully!")
            } else {
              log_print("Failed to download image.")
            }
          }
        } else {
          destination_file <- file.path(
            Startup$database,
            input$select_cgmlst,
            paste0(gsub(" ", "_", Scheme$species_data[[1]]$Name$name), ".jpg")
          )

          response_download <- httr::GET(Scheme$species_data[[1]]$Image)

          if (response_download$status_code == 200) {
            writeBin(httr::content(response_download, "raw"), destination_file)
            log_print("Image downloaded successfully!")
          } else {
            log_print("Failed to download image.")
          }
        }
      } else {
        log_print("Failed to download species data from NCBI.")
      }
    } else {
      log_print("Failed to download species data from NCBI.")
    }

    log_print("Hashing downloaded database")
    shinyjs::show("hashing")
    shinyjs::hide("downloading")

    show_toast(
      title = paste("Hashing of", input$select_cgmlst, "started"),
      type = "info",
      position = "bottom-start",
      timer = 5000
    )

    # Hash temporary folder
    hash_database(
      file.path(
        Startup$database,
        Scheme$folder_name,
        paste0(Scheme$folder_name, ".tmp")
      ),
      progress = progress
    )

    # Get list from local database
    local_db_filelist <- list.files(
      file.path(
        Startup$database,
        Scheme$folder_name,
        paste0(Scheme$folder_name, "_alleles")
      )
    )

    if (!is_empty(local_db_filelist)) {
      # Get list from temporary database
      tmp_db_filelist <- list.files(
        file.path(
          Startup$database,
          Scheme$folder_name,
          paste0(Scheme$folder_name, ".tmp")
        )
      )

      # Find the difference (extra files in local database)
      local_db_extra <- setdiff(local_db_filelist, tmp_db_filelist)

      # Copy extra files to temporary folder
      file.copy(
        file.path(
          Startup$database,
          Scheme$folder_name,
          paste0(Scheme$folder_name, "_alleles"),
          local_db_extra
        ),
        file.path(
          Startup$database,
          Scheme$folder_name,
          paste0(Scheme$folder_name, ".tmp")
        )
      )

      # Check differences in file pairs
      local_db_hashes <- tools::md5sum(
        file.path(
          Startup$database,
          Scheme$folder_name,
          paste0(Scheme$folder_name, "_alleles"),
          local_db_filelist
        )
      )
      tmp_db_hashes <- tools::md5sum(
        file.path(
          Startup$database,
          Scheme$folder_name,
          paste0(Scheme$folder_name, ".tmp"),
          local_db_filelist
        )
      )

      diff_files <- local_db_hashes %in% tmp_db_hashes
      diff_loci <- names(local_db_hashes)[diff_files == FALSE]
      diff_loci <- sapply(strsplit(diff_loci, "/"), function(x) x[length(x)])

      # Check locus hashes
      for (locus in diff_loci) {
        local_db_hashes <- get_locus_hashes(
          file.path(
            Startup$database,
            Scheme$folder_name,
            paste0(Scheme$folder_name, "_alleles"),
            locus
          )
        )
        tmp_db_hashes <- get_locus_hashes(
          file.path(
            Startup$database,
            Scheme$folder_name,
            paste0(Scheme$folder_name, ".tmp"),
            locus
          )
        )
        diff_hashes <- setdiff(local_db_hashes, tmp_db_hashes)

        sequences <- extract_seq(
          file.path(
            Startup$database,
            Scheme$folder_name,
            paste0(Scheme$folder_name, "_alleles"),
            locus
          ),
          diff_hashes
        )
        if (
          !is_empty(sequences$idx) &&
            !is_empty(sequences$seq) &&
            length(sequences$idx) == length(sequences$seq)
        ) {
          add_new_sequences(
            file.path(
              Startup$database,
              Scheme$folder_name,
              paste0(Scheme$folder_name, ".tmp"),
              locus
            ),
            sequences
          )
        }
      }
    }

    unlink(
      file.path(
        Startup$database,
        Scheme$folder_name,
        paste0(Scheme$folder_name, "_alleles")
      ),
      recursive = TRUE
    )

    file.rename(
      file.path(
        Startup$database,
        Scheme$folder_name,
        paste0(Scheme$folder_name, ".tmp")
      ),
      file.path(
        Startup$database,
        Scheme$folder_name,
        paste0(Scheme$folder_name, "_alleles")
      )
    )

    # Download Scheme Info (pubMLST already downloaded)
    if (grepl("_CM", input$select_cgmlst)) {
      tryCatch(
        {
          scheme_overview <- read_html(Scheme$link_scheme) %>%
            html_table(header = FALSE) %>%
            as.data.frame(stringsAsFactors = FALSE)

          names(scheme_overview) <- c("X1", "X2")

          scheme_overview <- add_row(
            scheme_overview,
            data.frame(
              X1 = c("URL", "Database"),
              X2 = c(
                paste0(
                  '<a href="',
                  schemes$url[schemes$species == input$select_cgmlst],
                  '/" target="_blank">',
                  schemes$url[schemes$species == input$select_cgmlst],
                  '</a>'
                ),
                "cgMLST.org Nomenclature Server (h25)"
              )
            ),
            .after = 1
          )

          names(scheme_overview) <- NULL

          saveRDS(
            scheme_overview,
            file.path(
              Startup$database,
              Scheme$folder_name,
              "scheme_info.rds"
            )
          )

          message("Scheme info downloaded")
        },
        error = function(e) {
          stop("Failed to download scheme info: ", "\nError: ", e$message)
        }
      )
    }

    available <- gsub("_", " ", basename(dir_ls(Startup$database)))
    DB$available <- available[available %in% gsub("_", " ", schemes$species)]
    DB$exist <- length(dir_ls(Startup$database)) == 0

    shinyjs::show("download_cgMLST")
    shinyjs::hide("hashing")

    output$statustext <- renderUI(
      fluidRow(
        tags$li(
          class = "dropdown",
          tags$span(
            HTML(
              paste(
                '<i class="fa-solid fa-circle-dot" style="color:lightgreen !important;"></i>',
                "Status:&nbsp;&nbsp;&nbsp; <i>ready</i>"
              )
            ),
            style = "color:white;"
          )
        )
      )
    )

    log_print("Download successful")

    showModal(
      div(
        class = "load-modal",
        modalDialog(
          fluidRow(
            br(),
            column(
              width = 11,
              p(
                HTML(
                  paste0(
                    '<span style="color: white; display: block; font-size: 15px; margin-left: 15px;">',
                    'Scheme successfully downloaded.',
                    '</span>'
                  )
                )
              ),
              div(
                class = "modal-element",
                selectInput(
                  "scheme_db",
                  label = "",
                  choices = if (!is.null(Typing$last_scheme)) {
                    Typing$last_scheme
                  } else {
                    DB$available
                  },
                  selected = if (!is.null(Typing$last_scheme)) {
                    Typing$last_scheme
                  } else {
                    if (!is.null(DB$scheme)) {
                      gsub("_", " ", input$select_cgmlst)
                    } else {
                      DB$available[1]
                    }
                  }
                )
              )
            ),
            br()
          ),
          title = "Load Database",
          footer = tagList(
            actionButton("load", "Load", class = "load-db", width = "100px")
          )
        )
      )
    )

    # Disable pickerInput
    runjs("$('#select_cgmlst').prop('disabled', false);")
    runjs("$('#select_cgmlst').selectpicker('refresh');")
    runjs(unblock_ui)
  })

  ### Fetch Scheme Info (CSV Table) ----
  observe({
    req(input$select_cgmlst, Scheme$link_scheme)

    runjs(block_ui)

    input$download_cgMLST

    ifelse(
      length(DB$new_database) > 0,
      database <- DB$new_database,
      database <- Startup$database
    )

    if (dir.exists(file.path(database, input$select_cgmlst))) {
      updateActionButton(session, "download_cgMLST", label = "Fetch Update")
    } else {
      updateActionButton(session, "download_cgMLST", label = "Download")
    }

    if (grepl("_CM", input$select_cgmlst)) {
      scheme_overview <- tryCatch(
        {
          read_html(Scheme$link_scheme)
        },
        error = function(e) {
          DB$failCon <- TRUE
          show_toast(
            title = "Could not retrieve data. Check internet connection.",
            type = "error",
            position = "bottom-end",
            timer = 6000
          )
          warning("Could not retrieve data. Check internet connection.")
          return(NULL)
        }
      )

      if (!is.null(scheme_overview)) {
        DB$failCon <- FALSE
        scheme_overview <- scheme_overview %>%
          html_table(header = FALSE) %>%
          as.data.frame(stringsAsFactors = FALSE)

        names(scheme_overview) <- c("X1", "X2")

        scheme_overview <- add_row(
          scheme_overview,
          data.frame(
            X1 = c("URL", "Database"),
            X2 = c(
              paste0(
                '<a href="',
                schemes$url[schemes$species == input$select_cgmlst],
                '/" target="_blank">',
                schemes$url[schemes$species == input$select_cgmlst],
                '</a>'
              ),
              "cgMLST.org Nomenclature Server (h25)"
            )
          ),
          .after = 1
        )

        last_scheme_change <- strptime(
          scheme_overview$X2[scheme_overview$X1 == "Last Change"],
          format = "%B %d, %Y, %H:%M %p"
        )
        names(scheme_overview) <- NULL

        last_file_change <- format(
          file.info(file.path(
            Startup$database,
            ".downloaded_schemes",
            paste0(Scheme$folder_name, ".zip")
          ))$mtime,
          "%Y-%m-%d %H:%M %p"
        )
      }
    } else if (grepl("_PM", input$select_cgmlst)) {
      scheme_overview <- tryCatch(
        {
          get.schemeinfo(
            url_link = schemes$url[schemes$species == input$select_cgmlst]
          )
        },
        error = function(e) {
          DB$failCon <- TRUE

          show_toast(
            title = "Could not retrieve data. Check internet connection.",
            type = "error",
            position = "bottom-end",
            timer = 6000
          )
          warning("Could not retrieve data. Check internet connection.")
          return(NULL)
        }
      )

      if (!is.null(scheme_overview)) {
        DB$failCon <- FALSE

        scheme_overview <- parse.schemeinfo(
          scheme_info = scheme_overview,
          repo = "PM",
          database = Startup$database,
          folder_name = Scheme$folder_name,
          url_link = schemes$url[schemes$species == input$select_cgmlst]
        )

        # Retrieve last changes
        if (!is.null(scheme_overview[["last_updated"]])) {
          last_scheme_change <- scheme_overview[["last_updated"]]
          last_file_change <- format(
            file.info(file.path(
              database,
              ".downloaded_schemes",
              paste0(folder_name, ".zip")
            ))$mtime,
            "%Y-%m-%d %H:%M %p"
          )
        } else {
          last_scheme_change <- "Not Available"
          last_file_change <- NULL
        }
      }
    }

    if (!is.null(scheme_overview)) {
      enable("download_cgMLST")

      output$cgmlst_scheme_table <- renderUI(
        addSpinner(
          tableOutput("cgmlst_scheme"),
          spin = "dots",
          color = "#ffffff"
        )
      )

      # Render scheme info table
      output$cgmlst_scheme <- renderTable(
        {
          scheme_overview
        },
        sanitize.text.function = function(x) x,
        width = "90%"
      )

      # Render scheme update availability info
      output$scheme_update_info <- renderUI({
        req(last_file_change, last_scheme_change)

        if (
          length(last_file_change) &&
            length(last_scheme_change) &&
            !is.na(last_file_change) &&
            !is.na(last_scheme_change)
        ) {
          new_scheme <- last_file_change < last_scheme_change
          if (length(new_scheme) != 0) {
            if (new_scheme) {
              HTML(
                paste0(
                  '<i class="fa-solid fa-circle-exclamation" style="font-size:20px;color:orange; position:relative; top: 17px; left: -10px;"></i>',
                  '<span style="color: white; font-size: 15px; position:relative; top: 15px;">',
                  "Updated scheme available",
                  '</span>'
                )
              )
            } else {
              HTML(
                paste0(
                  '<i class="fa-solid fa-check" style="font-size:20px;color:lightgreen; position:relative; top: 17px; left: -10px;"></i>',
                  '<span style="color: white; font-size: 15px; position:relative; top: 15px;">',
                  "Scheme is up-to-date",
                  '</span>'
                )
              )
            }
          }
        }
      })
    } else {
      disable("download_cgMLST")

      output$cgmlst_scheme <- NULL
      output$scheme_update_info <- NULL
    }

    ### Fetch species info ----
    selected_species <- gsub(
      "_",
      " ",
      gsub(
        "_(PM|CM)",
        "",
        schemes$species[schemes$species == input$select_cgmlst]
      )
    )

    tryCatch(
      {
        Scheme$species_data <- fetch.species.data(species = selected_species)
      },
      error = function(e) {
        DB$failCon <- TRUE
        show_toast(
          title = "Could not retrieve data. Check internet connection.",
          type = "error",
          position = "bottom-end",
          timer = 6000
        )
        warning("Could not retrieve data. Check internet connection.")
        Scheme$species_data <- NULL
      }
    )

    runjs(unblock_ui)
  })

  ### Render species info & image ----
  observe({
    req(Scheme$species_data, input$select_cgmlst)

    runjs(block_ui)

    DB$failCon <- FALSE

    multiple <- length(Scheme$species_data) > 1 &&
      !is.null(input$selected_species)
    if (multiple) {
      species_data <- Scheme$species_data[[input$selected_species]]
    } else {
      species_data <- Scheme$species_data[[1]]
    }

    if (!is.null(species_data)) {
      # Download and render species image
      if (!is.null(input$selected_species) && length(Scheme$species_data) > 0) {
        # Download image
        destination_file <- file.path(
          Startup$database,
          schemes$species[schemes$species == input$select_cgmlst],
          paste0(input$selected_species, ".jpg")
        )
        if (!dir.exists(dirname(destination_file))) {
          if (
            file.exists(
              file.path(tempdir(), paste0(input$selected_species, ".jpg"))
            )
          ) {
            destination_file <- file.path(
              tempdir(),
              paste0(input$selected_species, ".jpg")
            )
            output$species_no_img <- NULL
            output$species_img <- renderImage(
              {
                list(src = destination_file, height = 180)
              },
              deleteFile = FALSE
            )
          } else {
            if (!is.null(Scheme$species_data[[input$selected_species]])) {
              response <- httr::GET(
                Scheme$species_data[[input$selected_species]]$Image
              )
              destination_file <- file.path(
                tempdir(),
                paste0(input$selected_species, ".jpg")
              )
              if (response$status_code == 200) {
                writeBin(httr::content(response, "raw"), destination_file)
                response <- NULL
                output$species_no_img <- NULL
                output$species_img <- renderImage(
                  {
                    list(src = destination_file, height = 180)
                  },
                  deleteFile = FALSE
                )
                print("Image downloaded successfully!")
              } else {
                output$species_img <- NULL
                output$species_no_img <- renderUI(
                  HTML(
                    '<i class="fa-solid fa-bacteria" style="font-size:150px;color:white;margin-right:25px;" ></i>'
                  )
                )
                print("Failed to download image.")
              }
            }
          }
        } else {
          if (!file.exists(destination_file)) {
            if (!is.null(Scheme$species_data[[input$selected_species]])) {
              response <- httr::GET(
                Scheme$species_data[[input$selected_species]]$Image
              )
              destination_file <- file.path(
                tempdir(),
                paste0(input$selected_species, ".jpg")
              )

              if (response$status_code == 200) {
                writeBin(httr::content(response, "raw"), destination_file)
                response <- NULL
                print("Image downloaded successfully!")
                output$species_no_img <- NULL
                output$species_img <- renderImage(
                  {
                    list(src = destination_file, height = 180)
                  },
                  deleteFile = FALSE
                )
              } else {
                output$species_no_img <- renderUI(
                  HTML(
                    '<i class="fa-solid fa-bacteria" style="font-size:150px;color:white;margin-right:25px;" ></i>'
                  )
                )
                output$species_img <- NULL
                print("Failed to download image.")
              }
            }
          } else {
            output$species_no_img <- NULL
            output$species_img <- renderImage(
              {
                list(src = destination_file, height = 180)
              },
              deleteFile = FALSE
            )
          }
        }
      } else if (length(Scheme$species_data) > 0) {
        # Download image
        destination_file <- file.path(
          Startup$database,
          schemes$species[schemes$species == input$select_cgmlst],
          paste0(
            schemes$species[schemes$species == input$select_cgmlst],
            ".jpg"
          )
        )

        if (dir.exists(dirname(destination_file))) {
          if (!file.exists(destination_file)) {
            response <- httr::GET(Scheme$species_data[[1]]$Image)

            if (response$status_code == 200) {
              writeBin(httr::content(response, "raw"), destination_file)
              print("Image downloaded successfully!")
              output$species_no_img <- NULL
              output$species_img <- renderImage(
                {
                  list(src = destination_file, height = 180)
                },
                deleteFile = FALSE
              )
            } else {
              output$species_img <- NULL
              output$species_no_img <- renderUI(
                HTML(
                  '<i class="fa-solid fa-bacteria" style="font-size:150px;color:white;margin-right:25px;" ></i>'
                )
              )
              print("Failed to download image.")
            }
          } else {
            output$species_no_img <- NULL
            output$species_img <- renderImage(
              {
                list(src = destination_file, height = 180)
              },
              deleteFile = FALSE
            )
          }
        } else {
          if (
            file.exists(
              file.path(
                tempdir(),
                paste0(
                  schemes$species[schemes$species == input$select_cgmlst],
                  ".jpg"
                )
              )
            )
          ) {
            destination_file <- file.path(
              tempdir(),
              paste0(
                schemes$species[schemes$species == input$select_cgmlst],
                ".jpg"
              )
            )
            output$species_no_img <- NULL
            output$species_img <- renderImage(
              {
                list(src = destination_file, height = 180)
              },
              deleteFile = FALSE
            )
          } else {
            destination_file <- file.path(
              tempdir(),
              paste0(
                schemes$species[schemes$species == input$select_cgmlst],
                ".jpg"
              )
            )
            response <- httr::GET(Scheme$species_data[[1]]$Image)
            if (response$status_code == 200) {
              writeBin(httr::content(response, "raw"), destination_file)
              output$species_no_img <- NULL
              output$species_img <- renderImage(
                {
                  list(src = destination_file, height = 180)
                },
                deleteFile = FALSE
              )
              print("Image downloaded successfully!")
            } else {
              output$species_img <- NULL
              output$species_no_img <- renderUI(
                HTML(
                  '<i class="fa-solid fa-bacteria" style="font-size:150px;color:white;margin-right:25px;" ></i>'
                )
              )
              print("Failed to download image.")
            }
          }
        }
      }

      # Render species info
      output$species_info <- renderUI({
        addSpinner(
          box(
            solidHeader = TRUE,
            status = "primary",
            width = "100%",
            title = HTML(
              paste0(
                '<span style="color: white; font-size: 15px;">',
                'Species Information - Data Fetched from NCBI&nbsp&nbsp&nbsp ',
                ' <a href="',
                'https://www.ncbi.nlm.nih.gov/',
                '" target="_blank" style="color:#008edb; font-style:normal; text-decoration:none;"> https://www.ncbi.nlm.nih.gov/ </a>',
                '</span>'
              )
            ),
            species_data_ui(species_data, fetch = TRUE)
          ),
          spin = "dots",
          color = "#ffffff"
        )
      })
    } else {
      output$species_info <- NULL
      output$species_img <- NULL
    }

    runjs(unblock_ui)
  })

  # Species info selector UI
  observe({
    if (!is.null(Scheme$species_data)) {
      if (length(Scheme$species_data) > 1) {
        choices <- gsub(
          " ",
          "_",
          unlist(lapply(Scheme$species_data, function(x) x$Name$name))
        )
        names(choices) <- gsub(
          "_",
          " ",
          unlist(lapply(Scheme$species_data, function(x) x$Name$name))
        )
        output$species_info_select <- renderUI({
          fluidRow(
            column(1),
            column(
              width = 3,
              p(
                HTML(
                  paste0(
                    '<span style="color: white; font-size: 15px; position: relative; top: 5px;">',
                    'Species Complex',
                    '</span>'
                  )
                )
              )
            ),
            column(
              width = 1,
              bslib::tooltip(
                bsicons::bs_icon(
                  "question-circle",
                  title = "Scheme comprises multiple species.",
                  color = "white",
                  height = "14px",
                  width = "14px",
                  position = "relative",
                  top = "9px",
                  right = "28px"
                ),
                "Text shown in the tooltip.",
                show = FALSE,
                id = "schemeinfo_tooltip"
              )
            ),
            column(
              width = 6,
              div(
                class = "selected-species",
                selectInput(
                  "selected_species",
                  "",
                  choices = choices
                )
              )
            )
          )
        })
      } else {
        output$species_info_select <- NULL
      }
    } else {
      output$species_info_select <- NULL
    }
  })

  # Switch to Manage Schemes tab if user wants to update scheme
  observeEvent(input$update_scheme, {
    removeModal()
    updateTabItems(session, "tabs", selected = "init")
  })

  # _______________________ ####

  ## Visualization ----

  # Render placeholder image

  output$placeholder <- renderImage(
    {
      # Path to your PNG image with a transparent background
      image_path <- paste0(getwd(), "/www/PhyloTrace.png")

      # Use HTML to display the image with the <img> tag
      list(src = image_path, height = 180)
    },
    deleteFile = FALSE
  )

  ### Plot Controls ----

  tree_type_reactive <- reactiveVal()

  observe({
    if (!is.null(input$tree_type)) {
      tree_type_reactive(input$tree_type)
    } else {
      tree_type_reactive("MST")
    }
  })

  output$generate_plot_ui <- renderUI({
    box(
      solidHeader = TRUE,
      status = "primary",
      width = "100%",
      title = plot.control(),
      fluidRow(
        column(
          width = 8,
          fluidRow(
            column(
              width = 6,
              div(
                class = "tree-info",
                popify(
                  div(
                    id = "tree_info_icon",
                    HTML(
                      '<i class="fa-regular fa-circle-question" style="color: white !important; top: -38px; position: absolute; font-size: 16px; padding: 5px; left: 74px"></i>'
                    )
                  ),
                  gsub("Generate ", "", plot.control()),
                  tree.type.info(),
                  options = list(container = "body")
                )
              ),
              radioGroupButtons(
                inputId = "tree_type",
                label = "",
                choices = c("MST", "Tree"),
                selected = tree_type_reactive(),
                justified = TRUE
              )
            ),
            uiOutput("tree_algo_lines"),
            column(
              width = 4,
              uiOutput("tree_algo_ui")
            )
          )
        ),
        column(
          width = 4,
          tags$div(
            id = "button-wrapper",
            actionButton(
              "create_tree",
              h5(
                "Generate",
                style = "position: relative; left: 15px; color: white; font-size: 15px;"
              ),
              width = "100%"
            ),
            tags$img(
              src = "phylo.png",
              alt = "icon",
              class = "icon"
            )
          )
        )
      )
    )
  })

  output$tree_algo_ui <- renderUI({
    tree_algo <- prettyRadioButtons(
      inputId = "tree_algo",
      "",
      choices = c("Neighbour-Joining", "UPGMA"),
      inline = FALSE
    )

    if (tree_type_reactive() == "MST") {
      disabled(tree_algo)
    } else {
      tree_algo
    }
  })

  output$tree_algo_lines <- renderUI({
    if (tree_type_reactive() == "MST") {
      div(
        div(class = "lower-line-inactive"),
        div(class = "upper-line-inactive")
      )
    } else {
      div(
        div(class = "lower-line-active"),
        div(class = "upper-line-active")
      )
    }
  })

  #### Tree controls ----

  # Initially shown label menu

  session$sendCustomMessage('nj_highlight', "nj_label_menu")

  output$tree_controls <- renderUI(
    div(
      class = "control-box",
      box(
        solidHeader = TRUE,
        status = "primary",
        width = "100%",
        title = "Labels",
        fluidRow(
          div(
            class = "nj-label-control-col",
            column(
              width = 12,
              align = "left",
              br(),
              fluidRow(
                column(
                  width = 6,
                  align = "left",
                  h4(
                    p("Isolate Label"),
                    style = "color:white; position: relative; right: -15px; "
                  ),
                ),
                column(
                  width = 6,
                  align = "left",
                  div(
                    class = "mat-switch-lab",
                    materialSwitch(
                      "nj_tiplab_show",
                      "",
                      value = TRUE
                    )
                  )
                )
              ),
              fluidRow(
                column(
                  width = 9,
                  align = "left",
                  div(
                    class = "nj-label-sel",
                    uiOutput("nj_tiplab_ui")
                  )
                ),
                column(
                  width = 3,
                  align = "center",
                  dropMenu(
                    actionBttn(
                      "nj_labeltext_menu",
                      label = "",
                      color = "default",
                      size = "sm",
                      style = "material-flat",
                      icon = icon("sliders")
                    ),
                    placement = "right",
                    theme = "translucent",
                    fluidRow(
                      column(
                        width = 6,
                        align = "center",
                        uiOutput("nj_align_ui"),
                        br(),
                        sliderInput(
                          "nj_tiplab_size",
                          label = h5(
                            "Label size",
                            style = "color:white; margin-bottom: 0px"
                          ),
                          min = 1,
                          max = 10,
                          step = 0.5,
                          value = nj_tiplab_size_val(),
                          width = "150px",
                          ticks = FALSE
                        ),
                        br(),
                        sliderInput(
                          "nj_tiplab_alpha",
                          label = h5(
                            "Opacity",
                            style = "color:white; margin-bottom: 0px"
                          ),
                          min = 0.1,
                          max = 1,
                          step = 0.05,
                          value = 1,
                          width = "150px",
                          ticks = FALSE
                        )
                      ),
                      column(
                        width = 6,
                        align = "center",
                        selectInput(
                          "nj_tiplab_fontface",
                          label = h5(
                            "Fontface",
                            style = "color:white; margin-bottom: 5px; margin-top: 16px"
                          ),
                          width = "250px",
                          choices = c(
                            Plain = "plain",
                            Bold = "bold",
                            Italic = "italic",
                            `B & I` = "bold.italic"
                          ),
                          selected = "plain"
                        ),
                        br(),
                        sliderInput(
                          inputId = "nj_tiplab_position",
                          label = h5(
                            "Position",
                            style = "color:white; margin-bottom: 0px"
                          ),
                          min = -3,
                          max = 3,
                          step = 0.05,
                          value = 0,
                          width = "150px",
                          ticks = FALSE
                        ),
                        br(),
                        uiOutput("nj_tiplab_angle_ui")
                      )
                    )
                  )
                )
              )
            )
          )
        ),
        fluidRow(
          column(
            width = 8,
            div(
              class = "mat-switch-geom",
              materialSwitch(
                "nj_geom",
                h5(
                  p("Panels"),
                  style = "color:white; padding-left: 5px; position: relative; top: -4px; right: 5px;"
                ),
                value = FALSE,
                right = FALSE
              )
            )
          ),
          column(
            width = 4,
            dropMenu(
              actionBttn(
                "nj_labelformat_menu",
                label = "",
                color = "default",
                size = "sm",
                style = "material-flat",
                icon = icon("sliders")
              ),
              placement = "right",
              theme = "translucent",
              fluidRow(
                column(
                  width = 12,
                  align = "center",
                  sliderInput(
                    inputId = "nj_tiplab_padding",
                    label = h5(
                      "Size",
                      style = "color:white; margin-bottom: 0px"
                    ),
                    min = 0.05,
                    max = 1,
                    value = nj_tiplab_padding_val(),
                    step = 0.05,
                    width = "150px",
                    ticks = FALSE
                  ),
                  br(),
                  sliderInput(
                    inputId = "nj_tiplab_labelradius",
                    label = h5(
                      "Smooth edge",
                      style = "color:white; margin-bottom: 0px"
                    ),
                    min = 0,
                    step = 0.05,
                    max = 0.5,
                    value = 0.2,
                    width = "150px",
                    ticks = FALSE
                  )
                )
              )
            )
          )
        ),
        hr(),
        fluidRow(
          div(
            class = "nj-label-control-col",
            column(
              width = 12,
              align = "left",
              fluidRow(
                column(
                  width = 6,
                  align = "left",
                  h4(
                    p("Branches"),
                    style = "color:white; position: relative; right: -15px;"
                  ),
                ),
                column(
                  width = 6,
                  align = "left",
                  div(
                    class = "mat-switch-lab",
                    materialSwitch(
                      "nj_show_branch_label",
                      "",
                      value = FALSE
                    )
                  )
                )
              ),
              fluidRow(
                column(
                  width = 9,
                  align = "left",
                  div(
                    class = "nj-label-sel",
                    uiOutput("nj_branch_label")
                  )
                ),
                column(
                  width = 3,
                  align = "center",
                  dropMenu(
                    actionBttn(
                      "nj_branch_label_menu",
                      label = "",
                      color = "default",
                      size = "sm",
                      style = "material-flat",
                      icon = icon("sliders")
                    ),
                    placement = "right",
                    theme = "translucent",
                    fluidRow(
                      column(
                        width = 6,
                        align = "center",
                        selectInput(
                          "nj_branchlab_fontface",
                          label = h5(
                            "Fontface",
                            style = "color:white; margin-bottom: 0px;"
                          ),
                          width = "250px",
                          choices = c(
                            Plain = "plain",
                            Bold = "bold",
                            Italic = "italic",
                            `B & I` = "bold.italic"
                          )
                        ),
                        br(),
                        sliderInput(
                          inputId = "nj_branch_x",
                          label = h5(
                            "X Position",
                            style = "color:white; margin-bottom: 0px"
                          ),
                          min = -3,
                          max = 3,
                          value = 0,
                          width = "250px",
                          ticks = FALSE
                        ),
                        br(),
                        sliderInput(
                          "nj_branchlab_alpha",
                          label = h5(
                            "Opacity",
                            style = "color:white; margin-bottom: 0px"
                          ),
                          min = 0.1,
                          max = 1,
                          step = 0.05,
                          value = 0.65,
                          width = "250px",
                          ticks = FALSE
                        )
                      ),
                      column(
                        width = 6,
                        align = "center",
                        sliderInput(
                          "nj_branch_size",
                          label = h5(
                            "Size",
                            style = "color:white; margin-bottom: 0px"
                          ),
                          min = 2,
                          max = 10,
                          step = 0.5,
                          value = nj_branch_size_val(),
                          width = "150px",
                          ticks = FALSE
                        ),
                        br(),
                        sliderInput(
                          "nj_branch_labelradius",
                          label = h5(
                            "Smooth edge",
                            style = "color:white; margin-bottom: 0px"
                          ),
                          min = 0,
                          max = 0.5,
                          step = 0.05,
                          value = 0.5,
                          width = "250px",
                          ticks = FALSE
                        )
                      )
                    )
                  )
                )
              )
            )
          )
        ),
        hr(),
        fluidRow(
          column(
            width = 12,
            align = "left",
            fluidRow(
              column(
                width = 8,
                textInput(
                  "nj_title",
                  label = "",
                  width = "100%",
                  placeholder = "Plot Title"
                )
              ),
              column(
                width = 4,
                dropMenu(
                  actionBttn(
                    "nj_title_menu",
                    label = "",
                    color = "default",
                    size = "sm",
                    style = "material-flat",
                    icon = icon("sliders")
                  ),
                  placement = "right",
                  theme = "translucent",
                  fluidRow(
                    column(
                      width = 12,
                      align = "center",
                      sliderInput(
                        "nj_title_size",
                        label = h5(
                          "Title Size",
                          style = "color:white; margin-bottom: 0px"
                        ),
                        value = 30,
                        min = 15,
                        max = 40,
                        step = 1,
                        width = "150px",
                        ticks = FALSE
                      )
                    )
                  )
                )
              )
            )
          )
        ),
        br(),
        fluidRow(
          column(
            width = 12,
            align = "left",
            fluidRow(
              column(
                width = 8,
                textInput(
                  "nj_subtitle",
                  label = "",
                  width = "100%",
                  placeholder = "Plot Subtitle"
                )
              ),
              column(
                width = 4,
                dropMenu(
                  actionBttn(
                    "nj_subtitle_menu",
                    label = "",
                    color = "default",
                    size = "sm",
                    style = "material-flat",
                    icon = icon("sliders")
                  ),
                  placement = "right",
                  theme = "translucent",
                  fluidRow(
                    column(
                      width = 12,
                      align = "center",
                      sliderInput(
                        "nj_subtitle_size",
                        label = h5(
                          "Subtitle Size",
                          style = "color:white; margin-bottom: 0px"
                        ),
                        value = 30,
                        min = 15,
                        max = 40,
                        step = 1,
                        width = "150px",
                        ticks = FALSE
                      )
                    )
                  )
                )
              )
            )
          )
        ),
        hr(),
        fluidRow(
          div(
            class = "nj-label-control-col",
            column(
              width = 12,
              align = "left",
              h4(
                p("Custom Label"),
                style = "color:white; position: relative; right: -15px;"
              ),
              column(
                width = 12,
                align = "center",
                fluidRow(
                  column(
                    width = 7,
                    textInput(
                      "nj_new_label_name",
                      "",
                      placeholder = "New Label"
                    )
                  ),
                  column(
                    width = 3,
                    actionButton(
                      "nj_add_new_label",
                      "",
                      icon = icon("plus")
                    )
                  ),
                  column(
                    width = 2,
                    align = "right",
                    dropMenu(
                      actionBttn(
                        "nj_custom_label_menu",
                        label = "",
                        color = "default",
                        size = "sm",
                        style = "material-flat",
                        icon = icon("sliders")
                      ),
                      placement = "right",
                      theme = "translucent",
                      fluidRow(
                        div(
                          class = "nj-custom-label-menu-col",
                          column(
                            width = 12,
                            align = "center",
                            fluidRow(
                              column(
                                width = 3,
                                align = "left",
                                HTML(
                                  paste(
                                    tags$span(
                                      style = 'color: white; font-size: 14px; position: relative; top: 7px;',
                                      'Size'
                                    )
                                  )
                                )
                              ),
                              column(
                                width = 9,
                                align = "right",
                                div(
                                  class = "nj-label-slider",
                                  uiOutput("nj_custom_labelsize")
                                )
                              )
                            ),
                            br(),
                            fluidRow(
                              column(
                                width = 3,
                                align = "left",
                                HTML(
                                  paste(
                                    tags$span(
                                      style = 'color: white; font-size: 14px; position: relative; top: 7px;',
                                      'Vertical'
                                    )
                                  )
                                )
                              ),
                              column(
                                width = 9,
                                align = "right",
                                div(
                                  class = "nj-label-slider",
                                  uiOutput("nj_sliderInput_y")
                                )
                              )
                            ),
                            br(),
                            fluidRow(
                              column(
                                width = 3,
                                align = "left",
                                HTML(
                                  paste(
                                    tags$span(
                                      style = 'color: white; font-size: 14px; position: relative; top: 7px;',
                                      'Horizontal'
                                    )
                                  )
                                )
                              ),
                              column(
                                width = 9,
                                align = "right",
                                div(
                                  class = "nj-label-slider",
                                  uiOutput("nj_sliderInput_x")
                                )
                              )
                            )
                          )
                        )
                      )
                    )
                  )
                ),
                fluidRow(
                  column(
                    width = 7,
                    uiOutput("nj_custom_label_select")
                  ),
                  column(
                    width = 4,
                    actionButton(
                      "nj_del_label",
                      "",
                      icon = icon("minus")
                    )
                  )
                ),
                fluidRow(
                  column(
                    width = 12,
                    align = "center",
                    actionButton(
                      "nj_cust_label_save",
                      "Apply"
                    )
                  )
                )
              )
            )
          )
        )
      )
    )
  )

  ##### Label Menu ----

  ###### Label Control Values ----

  # Tip label
  nj_tiplab_reactive <- reactive({
    ifelse(!is.null(input$nj_tiplab), input$nj_tiplab, "Assembly Name")
  }) |>
    debounce(100)
  nj_tiplab_val <- reactiveVal()
  nj_tiplab_show_val <- reactiveVal()
  nj_align_reactive <- reactive({
    ifelse(!is.null(input$nj_align), input$nj_align, FALSE)
  }) |>
    debounce(100)
  nj_align_val <- reactiveVal()
  nj_tiplab_size_val <- reactiveVal()
  nj_tiplab_fontface_val <- reactiveVal()
  nj_tiplab_alpha_val <- reactiveVal()
  nj_tiplab_position_val <- reactiveVal()
  nj_tiplab_angle_val <- reactiveVal()

  observe({
    nj_tiplab_val(nj_tiplab_reactive())

    ifelse(
      !is.null(input$nj_tiplab_show),
      nj_tiplab_show_val(input$nj_tiplab_show),
      nj_tiplab_show_val(TRUE)
    )

    ifelse(
      !is.null(Vis$tree_algo),
      ifelse(
        Vis$tree_algo == "UPGMA",
        nj_align_val(TRUE),
        nj_align_val(nj_align_reactive())
      ),
      nj_align_val(nj_align_reactive())
    )

    ifelse(
      !is.null(input$nj_tiplab_size),
      nj_tiplab_size_val(input$nj_tiplab_size),
      ifelse(
        !is.null(Vis$labelsize_nj),
        nj_tiplab_size_val(Vis$labelsize_nj),
        nj_tiplab_size_val(4)
      )
    )

    ifelse(
      !is.null(input$nj_tiplab_fontface),
      nj_tiplab_fontface_val(input$nj_tiplab_fontface),
      nj_tiplab_fontface_val("plain")
    )

    ifelse(
      !is.null(input$nj_tiplab_alpha),
      nj_tiplab_alpha_val(input$nj_tiplab_alpha),
      nj_tiplab_alpha_val(1)
    )

    ifelse(
      !is.null(input$nj_tiplab_position),
      nj_tiplab_position_val(input$nj_tiplab_position),
      ifelse(
        !is.null(nj_layout_val()),
        ifelse(
          nj_layout_val() != "inward" &&
            nj_layout_val() != "circular",
          nj_tiplab_position_val(0),
          ifelse(
            nj_layout_val() == "inward",
            nj_tiplab_position_val(1.1),
            nj_tiplab_position_val(-0.05)
          )
        ),
        nj_tiplab_position_val(0)
      )
    )

    ifelse(
      !is.null(input$nj_tiplab_angle),
      nj_tiplab_angle_val(input$nj_tiplab_angle),
      nj_tiplab_angle_val(0)
    )
  })

  # Label panels
  nj_geom_val <- reactiveVal()
  nj_tiplab_labelradius_val <- reactiveVal()
  nj_tiplab_padding_val <- reactiveVal()

  observe({
    ifelse(
      !is.null(input$nj_geom),
      nj_geom_val(input$nj_geom),
      nj_geom_val(FALSE)
    )

    ifelse(
      !is.null(input$nj_tiplab_labelradius),
      nj_tiplab_labelradius_val(input$nj_tiplab_labelradius),
      nj_tiplab_labelradius_val(0.2)
    )

    ifelse(
      !is.null(input$nj_tiplab_padding),
      nj_tiplab_padding_val(input$nj_tiplab_padding),
      ifelse(
        !is.null(Vis$tiplab_padding_nj),
        nj_tiplab_padding_val(Vis$tiplab_padding_nj),
        nj_tiplab_padding_val(0.2)
      )
    )
  })

  # Branch labels
  nj_show_branch_label_val <- reactiveVal()
  nj_branch_size_val <- reactiveVal()
  nj_branch_label_reactive <- reactive({
    ifelse(!is.null(input$nj_branch_label), input$nj_branch_label, "Host")
  }) |>
    debounce(100)
  nj_branch_label_val <- reactiveVal()
  nj_branchlab_alpha_val <- reactiveVal()
  nj_branch_x_val <- reactiveVal()
  nj_branchlab_fontface_val <- reactiveVal()
  nj_branch_labelradius_val <- reactiveVal()

  observe({
    ifelse(
      !is.null(input$nj_show_branch_label),
      nj_show_branch_label_val(input$nj_show_branch_label),
      nj_show_branch_label_val(FALSE)
    )

    ifelse(
      !is.null(input$nj_branch_size),
      nj_branch_size_val(input$nj_branch_size),
      ifelse(
        !is.null(Vis$branch_size_nj),
        nj_branch_size_val(Vis$branch_size_nj),
        nj_branch_size_val(4)
      )
    )

    nj_branch_label_val(nj_branch_label_reactive())

    ifelse(
      !is.null(input$nj_branchlab_alpha),
      nj_branchlab_alpha_val(input$nj_branchlab_alpha),
      nj_branchlab_alpha_val(0.65)
    )

    ifelse(
      !is.null(input$nj_branch_x),
      nj_branch_x_val(input$nj_branch_x),
      nj_branch_x_val(0)
    )

    ifelse(
      !is.null(input$nj_branchlab_fontface),
      nj_branchlab_fontface_val(input$nj_branchlab_fontface),
      nj_branchlab_fontface_val("plain")
    )

    ifelse(
      !is.null(input$nj_branch_labelradius),
      nj_branch_labelradius_val(input$nj_branch_labelradius),
      nj_branch_labelradius_val(0.5)
    )
  })

  # Titles
  nj_title_val <- reactiveVal()
  nj_title_size_val <- reactiveVal()
  nj_subtitle_val <- reactiveVal()
  nj_subtitle_size_val <- reactiveVal()

  observe({
    ifelse(
      !is.null(input$nj_title),
      nj_title_val(input$nj_title),
      nj_title_val(NULL)
    )

    ifelse(
      !is.null(input$nj_title_size),
      nj_title_size_val(input$nj_title_size),
      nj_title_size_val(30)
    )

    ifelse(
      !is.null(input$nj_subtitle),
      nj_subtitle_val(input$nj_subtitle),
      nj_subtitle_val(NULL)
    )

    ifelse(
      !is.null(input$nj_subtitle_size),
      nj_subtitle_size_val(input$nj_subtitle_size),
      nj_subtitle_size_val(30)
    )
  })

  ###### Label Interface ----

  observeEvent(input$nj_label_menu, {
    runjs(block_ui)

    session$sendCustomMessage('nj_reset_style', "")
    session$sendCustomMessage('nj_highlight', "nj_label_menu")

    output$tree_controls <- renderUI(
      div(
        class = "control-box",
        box(
          solidHeader = TRUE,
          status = "primary",
          width = "100%",
          title = "Labels",
          fluidRow(
            div(
              class = "nj-label-control-col",
              column(
                width = 12,
                align = "left",
                br(),
                fluidRow(
                  column(
                    width = 6,
                    align = "left",
                    h4(
                      p("Isolate Label"),
                      style = "color:white; position: relative; right: -15px;"
                    )
                  ),
                  column(
                    width = 6,
                    align = "left",
                    div(
                      class = "mat-switch-lab",
                      materialSwitch(
                        "nj_tiplab_show",
                        "",
                        value = isolate(nj_tiplab_show_val())
                      )
                    )
                  )
                ),
                fluidRow(
                  column(
                    width = 9,
                    align = "left",
                    div(
                      class = "nj-label-sel",
                      uiOutput("nj_tiplab_ui")
                    )
                  ),
                  column(
                    width = 3,
                    align = "center",
                    dropMenu(
                      actionBttn(
                        "nj_labeltext_menu",
                        label = "",
                        color = "default",
                        size = "sm",
                        style = "material-flat",
                        icon = icon("sliders")
                      ),
                      placement = "right",
                      theme = "translucent",
                      fluidRow(
                        column(
                          width = 6,
                          align = "center",
                          uiOutput("nj_align_ui"),
                          br(),
                          sliderInput(
                            "nj_tiplab_size",
                            label = h5(
                              "Label size",
                              style = "color:white; margin-bottom: 0px"
                            ),
                            min = 1,
                            max = 10,
                            step = 0.5,
                            value = isolate(nj_tiplab_size_val()),
                            width = "150px",
                            ticks = FALSE
                          ),
                          br(),
                          sliderInput(
                            "nj_tiplab_alpha",
                            label = h5(
                              "Opacity",
                              style = "color:white; margin-bottom: 0px"
                            ),
                            min = 0.1,
                            max = 1,
                            step = 0.05,
                            value = isolate(nj_tiplab_alpha_val()),
                            width = "150px",
                            ticks = FALSE
                          )
                        ),
                        column(
                          width = 6,
                          align = "center",
                          selectInput(
                            "nj_tiplab_fontface",
                            label = h5(
                              "Fontface",
                              style = "color:white; margin-bottom: 5px; margin-top: 16px"
                            ),
                            width = "250px",
                            choices = c(
                              Plain = "plain",
                              Bold = "bold",
                              Italic = "italic",
                              `B & I` = "bold.italic"
                            ),
                            selected = isolate(nj_tiplab_fontface_val())
                          ),
                          br(),
                          sliderInput(
                            inputId = "nj_tiplab_position",
                            label = h5(
                              "Position",
                              style = "color:white; margin-bottom: 0px"
                            ),
                            min = -3,
                            max = 3,
                            step = 0.05,
                            value = isolate(nj_tiplab_position_val()),
                            width = "150px",
                            ticks = FALSE
                          ),
                          br(),
                          uiOutput("nj_tiplab_angle_ui")
                        )
                      )
                    )
                  )
                )
              )
            )
          ),
          fluidRow(
            column(
              width = 8,
              div(
                class = "mat-switch-geom",
                materialSwitch(
                  "nj_geom",
                  h5(
                    p("Panels"),
                    style = "color:white; padding-left: 5px; position: relative; top: -4px; right: 5px;"
                  ),
                  value = isolate(nj_geom_val()),
                  right = FALSE
                )
              )
            ),
            column(
              width = 4,
              dropMenu(
                actionBttn(
                  "nj_labelformat_menu",
                  label = "",
                  color = "default",
                  size = "sm",
                  style = "material-flat",
                  icon = icon("sliders")
                ),
                placement = "right",
                theme = "translucent",
                fluidRow(
                  column(
                    width = 12,
                    align = "center",
                    sliderInput(
                      inputId = "nj_tiplab_padding",
                      label = h5(
                        "Size",
                        style = "color:white; margin-bottom: 0px"
                      ),
                      min = 0.05,
                      max = 1,
                      value = isolate(nj_tiplab_padding_val()),
                      step = 0.05,
                      width = "150px",
                      ticks = FALSE
                    ),
                    br(),
                    sliderInput(
                      inputId = "nj_tiplab_labelradius",
                      label = h5(
                        "Smooth edge",
                        style = "color:white; margin-bottom: 0px"
                      ),
                      min = 0,
                      step = 0.05,
                      max = 0.5,
                      value = isolate(nj_tiplab_labelradius_val()),
                      width = "150px",
                      ticks = FALSE
                    )
                  )
                )
              )
            )
          ),
          hr(),
          fluidRow(
            div(
              class = "nj-label-control-col",
              column(
                width = 12,
                align = "left",
                fluidRow(
                  column(
                    width = 6,
                    align = "left",
                    h4(
                      p("Branches"),
                      style = "color:white; position: relative; right: -15px;"
                    )
                  ),
                  column(
                    width = 6,
                    align = "left",
                    div(
                      class = "mat-switch-lab",
                      materialSwitch(
                        "nj_show_branch_label",
                        "",
                        value = isolate(nj_show_branch_label_val())
                      )
                    )
                  )
                ),
                fluidRow(
                  column(
                    width = 9,
                    align = "left",
                    div(
                      class = "nj-label-sel",
                      uiOutput("nj_branch_label")
                    )
                  ),
                  column(
                    width = 3,
                    align = "center",
                    dropMenu(
                      actionBttn(
                        "nj_branch_label_menu",
                        label = "",
                        color = "default",
                        size = "sm",
                        style = "material-flat",
                        icon = icon("sliders")
                      ),
                      placement = "right",
                      theme = "translucent",
                      fluidRow(
                        column(
                          width = 6,
                          align = "center",
                          selectInput(
                            "nj_branchlab_fontface",
                            label = h5(
                              "Fontface",
                              style = "color:white; margin-bottom: 0px;"
                            ),
                            width = "250px",
                            choices = c(
                              Plain = "plain",
                              Bold = "bold",
                              Italic = "italic",
                              `B & I` = "bold.italic"
                            ),
                            selected = isolate(nj_branchlab_fontface_val())
                          ),
                          br(),
                          sliderInput(
                            inputId = "nj_branch_x",
                            label = h5(
                              "X Position",
                              style = "color:white; margin-bottom: 0px"
                            ),
                            min = -3,
                            max = 3,
                            value = isolate(nj_branch_x_val()),
                            width = "250px",
                            ticks = FALSE
                          ),
                          br(),
                          sliderInput(
                            "nj_branchlab_alpha",
                            label = h5(
                              "Opacity",
                              style = "color:white; margin-bottom: 0px"
                            ),
                            min = 0.1,
                            max = 1,
                            step = 0.05,
                            value = isolate(nj_branchlab_alpha_val()),
                            width = "250px",
                            ticks = FALSE
                          )
                        ),
                        column(
                          width = 6,
                          align = "center",
                          sliderInput(
                            "nj_branch_size",
                            label = h5(
                              "Size",
                              style = "color:white; margin-bottom: 0px"
                            ),
                            min = 2,
                            max = 10,
                            step = 0.5,
                            value = isolate(nj_branch_size_val()),
                            width = "150px",
                            ticks = FALSE
                          ),
                          br(),
                          sliderInput(
                            "nj_branch_labelradius",
                            label = h5(
                              "Smooth edge",
                              style = "color:white; margin-bottom: 0px"
                            ),
                            min = 0,
                            max = 0.5,
                            step = 0.05,
                            value = isolate(nj_branch_labelradius_val()),
                            width = "250px",
                            ticks = FALSE
                          )
                        )
                      )
                    )
                  )
                )
              )
            )
          ),
          hr(),
          fluidRow(
            column(
              width = 12,
              align = "left",
              fluidRow(
                column(
                  width = 8,
                  textInput(
                    "nj_title",
                    label = "",
                    width = "100%",
                    placeholder = "Plot Title",
                    value = isolate(nj_title_val())
                  )
                ),
                column(
                  width = 4,
                  dropMenu(
                    actionBttn(
                      "nj_title_menu",
                      label = "",
                      color = "default",
                      size = "sm",
                      style = "material-flat",
                      icon = icon("sliders")
                    ),
                    placement = "right",
                    theme = "translucent",
                    fluidRow(
                      column(
                        width = 12,
                        align = "center",
                        sliderInput(
                          "nj_title_size",
                          label = h5(
                            "Title Size",
                            style = "color:white; margin-bottom: 0px"
                          ),
                          value = isolate(nj_title_size_val()),
                          min = 15,
                          max = 40,
                          step = 1,
                          width = "150px",
                          ticks = FALSE
                        )
                      )
                    )
                  )
                )
              )
            )
          ),
          br(),
          fluidRow(
            column(
              width = 12,
              align = "left",
              fluidRow(
                column(
                  width = 8,
                  textInput(
                    "nj_subtitle",
                    label = "",
                    width = "100%",
                    placeholder = "Plot Subtitle",
                    value = isolate(nj_subtitle_val())
                  )
                ),
                column(
                  width = 4,
                  dropMenu(
                    actionBttn(
                      "nj_subtitle_menu",
                      label = "",
                      color = "default",
                      size = "sm",
                      style = "material-flat",
                      icon = icon("sliders")
                    ),
                    placement = "right",
                    theme = "translucent",
                    fluidRow(
                      column(
                        width = 12,
                        align = "center",
                        sliderInput(
                          "nj_subtitle_size",
                          label = h5(
                            "Subtitle Size",
                            style = "color:white; margin-bottom: 0px"
                          ),
                          value = isolate(nj_subtitle_size_val()),
                          min = 15,
                          max = 40,
                          step = 1,
                          width = "150px",
                          ticks = FALSE
                        )
                      )
                    )
                  )
                )
              )
            )
          ),
          hr(),
          fluidRow(
            div(
              class = "nj-label-control-col",
              column(
                width = 12,
                align = "left",
                h4(
                  p("Custom Label"),
                  style = "color:white; position: relative; right: -15px;"
                ),
                column(
                  width = 12,
                  align = "center",
                  fluidRow(
                    column(
                      width = 7,
                      textInput(
                        "nj_new_label_name",
                        "",
                        placeholder = "New Label"
                      )
                    ),
                    column(
                      width = 3,
                      actionButton(
                        "nj_add_new_label",
                        "",
                        icon = icon("plus")
                      )
                    ),
                    column(
                      width = 2,
                      align = "right",
                      dropMenu(
                        actionBttn(
                          "nj_custom_label_menu",
                          label = "",
                          color = "default",
                          size = "sm",
                          style = "material-flat",
                          icon = icon("sliders")
                        ),
                        placement = "right",
                        theme = "translucent",
                        fluidRow(
                          div(
                            class = "nj-custom-label-menu-col",
                            column(
                              width = 12,
                              align = "center",
                              fluidRow(
                                column(
                                  width = 3,
                                  align = "left",
                                  HTML(
                                    paste(
                                      tags$span(
                                        style = 'color: white; font-size: 14px; position: relative; top: 7px;',
                                        'Size'
                                      )
                                    )
                                  )
                                ),
                                column(
                                  width = 9,
                                  align = "right",
                                  div(
                                    class = "nj-label-slider",
                                    uiOutput("nj_custom_labelsize")
                                  )
                                )
                              ),
                              br(),
                              fluidRow(
                                column(
                                  width = 3,
                                  align = "left",
                                  HTML(
                                    paste(
                                      tags$span(
                                        style = 'color: white; font-size: 14px; position: relative; top: 7px;',
                                        'Vertical'
                                      )
                                    )
                                  )
                                ),
                                column(
                                  width = 9,
                                  align = "right",
                                  div(
                                    class = "nj-label-slider",
                                    uiOutput("nj_sliderInput_y")
                                  )
                                )
                              ),
                              br(),
                              fluidRow(
                                column(
                                  width = 3,
                                  align = "left",
                                  HTML(
                                    paste(
                                      tags$span(
                                        style = 'color: white; font-size: 14px; position: relative; top: 7px;',
                                        'Horizontal'
                                      )
                                    )
                                  )
                                ),
                                column(
                                  width = 9,
                                  align = "right",
                                  div(
                                    class = "nj-label-slider",
                                    uiOutput("nj_sliderInput_x")
                                  )
                                )
                              )
                            )
                          )
                        )
                      )
                    )
                  ),
                  fluidRow(
                    column(
                      width = 7,
                      uiOutput("nj_custom_label_select")
                    ),
                    column(
                      width = 4,
                      actionButton(
                        "nj_del_label",
                        "",
                        icon = icon("minus")
                      )
                    )
                  ),
                  fluidRow(
                    column(
                      width = 12,
                      align = "center",
                      actionButton(
                        "nj_cust_label_save",
                        "Apply"
                      )
                    )
                  )
                )
              )
            )
          )
        )
      )
    )

    runjs(unblock_ui)
  })

  ###### Label Inputs ----

  # Tip label
  output$nj_tiplab_ui <- renderUI({
    if (!is.null(Vis$meta_nj)) {
      choices <- colnames(Vis$meta_nj)[-c(1, 2, 6, 12, 13, 14)]
    } else {
      choices <- colnames(DB$meta[, -2])[-c(1, 2, 6, 12, 13, 14)]
    }

    output <- render_plot_control(
      input_id = "nj_tiplab",
      input_type = "selectInput",
      choices = choices,
      reactive_value = nj_tiplab_val(),
      default_value = "Assembly Name",
      reset = isolate(Vis$nj_tiplab_val_reset)
    )

    isolate(Vis$nj_tiplab_val_reset <- FALSE)

    output
  })

  output$nj_align_ui <- renderUI({
    output <- render_plot_control(
      input_id = "nj_align",
      input_type = "materialSwitch",
      label = h5(
        p("Align"),
        style = "color:white; padding-left: 0px; position: relative; top: -4px; right: -5px;"
      ),
      reactive_value = nj_align_val(),
      default_value = FALSE,
      div_class = "mat-switch-align",
      reset = isolate(Vis$nj_align_reset),
      right = TRUE,
      show_condition = !is.null(Vis$tree_algo) && Vis$tree_algo == "NJ"
    )

    isolate(Vis$nj_align_reset <- FALSE)

    output
  })

  output$nj_tiplab_angle_ui <- renderUI({
    output <- render_plot_control(
      input_id = "nj_tiplab_angle",
      input_type = "sliderInput",
      label = h5("Angle", style = "color:white; margin-bottom: 0px"),
      min = -90,
      max = 90,
      width = "150px",
      reactive_value = nj_tiplab_angle_val(),
      default_value = 0,
      reset = isolate(Vis$nj_tiplab_angle_reset),
      right = TRUE,
      show_condition = nj_layout_val() != "inward" &&
        nj_layout_val() != "circular"
    )

    isolate(Vis$nj_tiplab_angle_reset <- FALSE)

    output
  })

  # Branch label
  output$nj_branch_label <- renderUI({
    if (!is.null(Vis$meta_nj)) {
      choices <- colnames(Vis$meta_nj)[-c(1:4, 6, 12:14)]
    } else {
      choices <- colnames(DB$meta[, -2])[-c(1:4, 6, 12:14)]
    }

    output <- render_plot_control(
      input_id = "nj_branch_label",
      input_type = "selectInput",
      choices = choices,
      reactive_value = nj_branch_label_val(),
      default_value = "Host",
      reset = isolate(Vis$nj_branch_label_val_reset)
    )

    isolate(Vis$nj_branch_label_val_reset <- FALSE)

    output
  })

  # Custom label
  output$nj_custom_label_select <- renderUI({
    if (!is.null(Vis$custom_label_nj)) {
      if (nrow(Vis$custom_label_nj) > 0) {
        choices <- Vis$custom_label_nj[, 1]
      } else {
        choices <- NULL
      }
    } else {
      choices <- NULL
    }

    div(
      class = "nj-custom-labe-sel",
      selectInput(
        "nj_custom_label_sel",
        "",
        choices = choices
      )
    )
  })

  output$nj_custom_labelsize <- renderUI({
    if (length(Vis$custom_label_nj) > 0) {
      if (!is.null(Vis$nj_label_size[[input$nj_custom_label_sel]])) {
        sliderInput(
          inputId = paste0("nj_slider_", input$nj_custom_label_sel, "_size"),
          label = "",
          min = 0,
          max = 10,
          step = 0.5,
          ticks = F,
          value = Vis$nj_label_size[[input$nj_custom_label_sel]],
          width = "150px"
        )
      } else {
        sliderInput(
          inputId = paste0("nj_slider_", input$nj_custom_label_sel, "_size"),
          label = "",
          min = 0,
          max = 10,
          step = 0.5,
          ticks = F,
          value = 5,
          width = "150px"
        )
      }
    } else {
      sliderInput(
        inputId = paste0("nj_slider_size"),
        label = "",
        min = 0,
        max = 10,
        step = 0.5,
        ticks = F,
        value = 5,
        width = "150px"
      )
    }
  })

  output$nj_sliderInput_y <- renderUI({
    if (!is.null(Vis$custom_label_nj)) {
      if (length(Vis$custom_label_nj) > 0) {
        inputId <- paste0("nj_slider_", input$nj_custom_label_sel, "_y")
        max <- sum(DB$data$Include)

        if (!is.null(Vis$nj_label_pos_y[[input$nj_custom_label_sel]])) {
          value <- Vis$nj_label_pos_y[[input$nj_custom_label_sel]]
        } else {
          value <- round(sum(DB$data$Include) / 2, 0)
        }
      } else {
        inputId <- "nj_slider_y"
        max <- 10
        value <- 5
      }
    } else {
      inputId <- "nj_slider_y"
      max <- 10
      value <- 5
    }

    sliderInput(
      inputId = inputId,
      label = "",
      min = 0,
      max = max,
      value = value,
      width = "150px",
      ticks = FALSE
    )
  })

  output$nj_sliderInput_x <- renderUI({
    if (!is.null(Vis$custom_label_nj)) {
      if (length(Vis$custom_label_nj) > 0) {
        inputId <- paste0("nj_slider_", input$nj_custom_label_sel, "_x")

        if (!is.null(Vis$nj_label_pos_x[[input$nj_custom_label_sel]])) {
          max <- 50
          value <- Vis$nj_label_pos_x[[input$nj_custom_label_sel]]
        } else {
          if (!is.null(Vis$nj_max_x)) {
            max <- round(Vis$nj_max_x, 0)
            value <- round(Vis$nj_max_x / 2, 0)
          } else {
            max <- 10
            value <- 1
          }
        }
      } else {
        inputId <- "nj_slider_x"
        max <- 10
        value <- 5
      }
    } else {
      inputId <- "nj_slider_x"
      max <- 10
      value <- 5
    }

    sliderInput(
      inputId = inputId,
      label = "",
      min = 0,
      max = max,
      value = value,
      step = 1,
      ticks = FALSE,
      width = "100%"
    )
  })

  ##### Variable Menu ----

  ###### Variable Control Values ----

  # Tip label mapping
  nj_mapping_show_val <- reactiveVal()
  nj_color_mapping_reactive <- reactive({
    ifelse(!is.null(input$nj_color_mapping), input$nj_color_mapping, "Country")
  }) |>
    debounce(100)
  nj_color_mapping_val <- reactiveVal()
  nj_tiplab_scale_reactive <- reactive({
    ifelse(
      !is.null(input$nj_tiplab_scale),
      input$nj_tiplab_scale,
      nj_tiplab_scale_val_default()
    )
  }) |>
    debounce(100)
  nj_tiplab_scale_val <- reactiveVal()
  nj_color_mapping_div_mid_reactive <- reactive({
    ifelse(
      !is.null(input$nj_color_mapping_div_mid),
      input$nj_color_mapping_div_mid,
      "Mean"
    )
  }) |>
    debounce(100)
  nj_color_mapping_div_mid_val <- reactiveVal()

  observe({
    ifelse(
      !is.null(input$nj_mapping_show),
      nj_mapping_show_val(input$nj_mapping_show),
      nj_mapping_show_val(FALSE)
    )

    nj_color_mapping_val(nj_color_mapping_reactive())

    nj_tiplab_scale_val(nj_tiplab_scale_reactive())

    nj_color_mapping_div_mid_val(nj_color_mapping_div_mid_reactive())
  })

  # Tip points mapping
  nj_tipcolor_mapping_show_val <- reactiveVal()
  nj_tipcolor_mapping_reactive <- reactive({
    ifelse(
      !is.null(input$nj_tipcolor_mapping),
      input$nj_tipcolor_mapping,
      "Country"
    )
  }) |>
    debounce(100)
  nj_tipcolor_mapping_val <- reactiveVal()
  nj_tippoint_scale_reactive <- reactive({
    ifelse(
      !is.null(input$nj_tippoint_scale),
      input$nj_tippoint_scale,
      nj_tippoint_scale_val_default()
    )
  }) |>
    debounce(100)
  nj_tippoint_scale_val <- reactiveVal()
  nj_tipcolor_mapping_div_mid_reactive <- reactive({
    ifelse(
      !is.null(input$nj_tipcolor_mapping_div_mid),
      input$nj_tipcolor_mapping_div_mid,
      "Mean"
    )
  })
  nj_tipcolor_mapping_div_mid_val <- reactiveVal()

  observe({
    ifelse(
      !is.null(input$nj_tipcolor_mapping_show),
      nj_tipcolor_mapping_show_val(input$nj_tipcolor_mapping_show),
      nj_tipcolor_mapping_show_val(FALSE)
    )

    nj_tipcolor_mapping_val(nj_tipcolor_mapping_reactive())

    nj_tippoint_scale_val(nj_tippoint_scale_reactive())

    nj_tipcolor_mapping_div_mid_val(nj_tipcolor_mapping_div_mid_reactive())
  })

  # Tip shape mapping
  nj_tipshape_mapping_show_val <- reactiveVal()
  nj_tipshape_mapping_reactive <- reactive({
    ifelse(
      !is.null(input$nj_tipshape_mapping),
      input$nj_tipshape_mapping,
      "Host"
    )
  }) |>
    debounce(100)
  nj_tipshape_mapping_val <- reactiveVal()

  observe({
    ifelse(
      !is.null(input$nj_tipshape_mapping_show),
      nj_tipshape_mapping_show_val(input$nj_tipshape_mapping_show),
      nj_tipshape_mapping_show_val(FALSE)
    )

    nj_tipshape_mapping_val(nj_tipshape_mapping_reactive())
  })

  # Tiles mapping
  nj_tiles_show_1_val <- reactiveVal()
  nj_tiles_show_2_val <- reactiveVal()
  nj_tiles_show_3_val <- reactiveVal()
  nj_tiles_show_4_val <- reactiveVal()
  nj_tiles_show_5_val <- reactiveVal()
  nj_fruit_variable_reactive <- reactive({
    ifelse(
      !is.null(input$nj_fruit_variable),
      input$nj_fruit_variable,
      "Isolation Date"
    )
  }) |>
    debounce(100)
  nj_fruit_variable_val <- reactiveVal()
  nj_fruit_variable_2_reactive <- reactive({
    ifelse(
      !is.null(input$nj_fruit_variable_2),
      input$nj_fruit_variable_2,
      "Isolation Date"
    )
  }) |>
    debounce(100)
  nj_fruit_variable_2_val <- reactiveVal()
  nj_fruit_variable_3_reactive <- reactive({
    ifelse(
      !is.null(input$nj_fruit_variable_3),
      input$nj_fruit_variable_3,
      "Isolation Date"
    )
  }) |>
    debounce(100)
  nj_fruit_variable_3_val <- reactiveVal()
  nj_fruit_variable_4_reactive <- reactive({
    ifelse(
      !is.null(input$nj_fruit_variable_4),
      input$nj_fruit_variable_4,
      "Isolation Date"
    )
  }) |>
    debounce(100)
  nj_fruit_variable_4_val <- reactiveVal()
  nj_fruit_variable_5_reactive <- reactive({
    ifelse(
      !is.null(input$nj_fruit_variable_5),
      input$nj_fruit_variable_5,
      "Isolation Date"
    )
  }) |>
    debounce(100)
  nj_fruit_variable_5_val <- reactiveVal()
  nj_tiles_scale_1_reactive <- reactive({
    ifelse(
      !is.null(input$nj_tiles_scale_1),
      input$nj_tiles_scale_1,
      nj_tiles_scale_1_val_default()
    )
  }) |>
    debounce(100)
  nj_tiles_scale_1_val <- reactiveVal()
  nj_tiles_scale_2_reactive <- reactive({
    ifelse(
      !is.null(input$nj_tiles_scale_2),
      input$nj_tiles_scale_2,
      nj_tiles_scale_2_val_default()
    )
  }) |>
    debounce(100)
  nj_tiles_scale_2_val <- reactiveVal()
  nj_tiles_scale_3_reactive <- reactive({
    ifelse(
      !is.null(input$nj_tiles_scale_3),
      input$nj_tiles_scale_3,
      nj_tiles_scale_3_val_default()
    )
  }) |>
    debounce(100)
  nj_tiles_scale_3_val <- reactiveVal()
  nj_tiles_scale_4_reactive <- reactive({
    ifelse(
      !is.null(input$nj_tiles_scale_4),
      input$nj_tiles_scale_4,
      nj_tiles_scale_4_val_default()
    )
  }) |>
    debounce(100)
  nj_tiles_scale_4_val <- reactiveVal()
  nj_tiles_scale_5_reactive <- reactive({
    ifelse(
      !is.null(input$nj_tiles_scale_5),
      input$nj_tiles_scale_5,
      nj_tiles_scale_5_val_default()
    )
  }) |>
    debounce(100)
  nj_tiles_scale_5_val <- reactiveVal()
  nj_tiles_mapping_div_mid_1_reactive <- reactive({
    ifelse(
      !is.null(input$nj_tiles_mapping_div_mid_1),
      input$nj_tiles_mapping_div_mid_1,
      "Mean"
    )
  }) |>
    debounce(100)
  nj_tiles_mapping_div_mid_1_val <- reactiveVal()
  nj_tiles_mapping_div_mid_2_reactive <- reactive({
    ifelse(
      !is.null(input$nj_tiles_mapping_div_mid_2),
      input$nj_tiles_mapping_div_mid_2,
      "Mean"
    )
  }) |>
    debounce(100)
  nj_tiles_mapping_div_mid_2_val <- reactiveVal()
  nj_tiles_mapping_div_mid_3_reactive <- reactive({
    ifelse(
      !is.null(input$nj_tiles_mapping_div_mid_3),
      input$nj_tiles_mapping_div_mid_3,
      "Mean"
    )
  }) |>
    debounce(100)
  nj_tiles_mapping_div_mid_3_val <- reactiveVal()
  nj_tiles_mapping_div_mid_4_reactive <- reactive({
    ifelse(
      !is.null(input$nj_tiles_mapping_div_mid_4),
      input$nj_tiles_mapping_div_mid_4,
      "Mean"
    )
  }) |>
    debounce(100)
  nj_tiles_mapping_div_mid_4_val <- reactiveVal()
  nj_tiles_mapping_div_mid_5_reactive <- reactive({
    ifelse(
      !is.null(input$nj_tiles_mapping_div_mid_5),
      input$nj_tiles_mapping_div_mid_5,
      "Mean"
    )
  }) |>
    debounce(100)
  nj_tiles_mapping_div_mid_5_val <- reactiveVal()

  observe({
    ifelse(
      !is.null(input$nj_tiles_show_1),
      nj_tiles_show_1_val(input$nj_tiles_show_1),
      nj_tiles_show_1_val(FALSE)
    )

    ifelse(
      !is.null(input$nj_tiles_show_2),
      nj_tiles_show_2_val(input$nj_tiles_show_2),
      nj_tiles_show_2_val(FALSE)
    )

    ifelse(
      !is.null(input$nj_tiles_show_3),
      nj_tiles_show_3_val(input$nj_tiles_show_3),
      nj_tiles_show_3_val(FALSE)
    )

    ifelse(
      !is.null(input$nj_tiles_show_4),
      nj_tiles_show_4_val(input$nj_tiles_show_4),
      nj_tiles_show_4_val(FALSE)
    )

    ifelse(
      !is.null(input$nj_tiles_show_5),
      nj_tiles_show_5_val(input$nj_tiles_show_5),
      nj_tiles_show_5_val(FALSE)
    )

    nj_fruit_variable_val(nj_fruit_variable_reactive())
    nj_fruit_variable_2_val(nj_fruit_variable_2_reactive())
    nj_fruit_variable_3_val(nj_fruit_variable_3_reactive())
    nj_fruit_variable_4_val(nj_fruit_variable_4_reactive())
    nj_fruit_variable_5_val(nj_fruit_variable_5_reactive())

    nj_tiles_scale_1_val(nj_tiles_scale_1_reactive())
    nj_tiles_scale_2_val(nj_tiles_scale_2_reactive())
    nj_tiles_scale_3_val(nj_tiles_scale_3_reactive())
    nj_tiles_scale_4_val(nj_tiles_scale_4_reactive())
    nj_tiles_scale_5_val(nj_tiles_scale_5_reactive())

    nj_tiles_mapping_div_mid_1_val(nj_tiles_mapping_div_mid_1_reactive())
    nj_tiles_mapping_div_mid_2_val(nj_tiles_mapping_div_mid_2_reactive())
    nj_tiles_mapping_div_mid_3_val(nj_tiles_mapping_div_mid_3_reactive())
    nj_tiles_mapping_div_mid_4_val(nj_tiles_mapping_div_mid_4_reactive())
    nj_tiles_mapping_div_mid_5_val(nj_tiles_mapping_div_mid_5_reactive())
  })

  # Heatmap mapping
  nj_heatmap_show_val <- reactiveVal()
  nj_heatmap_select_val <- reactiveVal()
  nj_heatmap_scale_val <- reactiveVal()
  nj_heatmap_div_mid_val <- reactiveVal()

  observe({
    ifelse(
      !is.null(input$nj_heatmap_show),
      nj_heatmap_show_val(input$nj_heatmap_show),
      nj_heatmap_show_val(FALSE)
    )

    nj_heatmap_select_val(input$nj_heatmap_select)

    ifelse(
      !is.null(input$nj_heatmap_scale),
      nj_heatmap_scale_val(input$nj_heatmap_scale),
      nj_heatmap_scale_val(nj_heatmap_scale_val_default())
    )

    ifelse(
      !is.null(input$nj_heatmap_div_mid),
      nj_heatmap_div_mid_val(input$nj_heatmap_div_mid),
      nj_heatmap_div_mid_val("Mean")
    )
  })

  ###### Variable Interface ----

  observeEvent(input$nj_variable_menu, {
    runjs(block_ui)

    session$sendCustomMessage('nj_reset_style', "")
    session$sendCustomMessage('nj_highlight', "nj_variable_menu")

    output$tree_controls <- renderUI(
      div(
        class = "control-box",
        box(
          solidHeader = TRUE,
          status = "primary",
          width = "100%",
          title = "Variable Mapping",
          fluidRow(
            column(
              width = 12,
              align = "left",
              br(),
              fluidRow(
                div(
                  class = "nj-label-control-col",
                  column(
                    width = 8,
                    align = "left",
                    h4(
                      p("Tip Label Color"),
                      style = "color:white; position: relative; right: -15px; "
                    )
                  )
                ),
                div(
                  class = "nj-label-control-col",
                  column(
                    width = 4,
                    align = "center",
                    div(
                      class = "mat-switch-v",
                      materialSwitch(
                        "nj_mapping_show",
                        "",
                        value = isolate(nj_mapping_show_val())
                      )
                    )
                  )
                )
              ),
              fluidRow(
                column(
                  width = 8,
                  uiOutput("nj_color_mapping")
                ),
                column(
                  width = 3,
                  align = "center",
                  dropMenu(
                    actionBttn(
                      "nj_variable_tiplab_menu",
                      label = "",
                      color = "default",
                      size = "sm",
                      style = "material-flat",
                      icon = icon("sliders")
                    ),
                    placement = "right",
                    theme = "translucent",
                    fluidRow(
                      column(
                        width = 12,
                        align = "center",
                        uiOutput("nj_tiplab_scale"),
                        br(),
                        uiOutput("nj_tiplab_mid_scale")
                      )
                    )
                  )
                )
              ),
              fluidRow(
                column(
                  width = 12,
                  uiOutput("nj_tiplab_mapping_info")
                )
              ),
              hr(),
              fluidRow(
                div(
                  class = "nj-label-control-col",
                  column(
                    width = 8,
                    align = "left",
                    h4(
                      p("Tip Point Color"),
                      style = "color:white; position: relative; right: -15px; "
                    )
                  )
                ),
                div(
                  class = "nj-label-control-col",
                  column(
                    width = 4,
                    align = "center",
                    div(
                      class = "mat-switch-v",
                      materialSwitch(
                        "nj_tipcolor_mapping_show",
                        "",
                        value = isolate(nj_tipcolor_mapping_show_val())
                      )
                    )
                  )
                )
              ),
              fluidRow(
                column(
                  width = 8,
                  uiOutput("nj_tipcolor_mapping")
                ),
                column(
                  width = 3,
                  align = "center",
                  dropMenu(
                    actionBttn(
                      "nj_variable_tipcolor_menu",
                      label = "",
                      color = "default",
                      size = "sm",
                      style = "material-flat",
                      icon = icon("sliders")
                    ),
                    placement = "right",
                    theme = "translucent",
                    fluidRow(
                      column(
                        width = 12,
                        align = "center",
                        uiOutput("nj_tippoint_scale"),
                        br(),
                        uiOutput("nj_tippoint_mid_scale")
                      )
                    )
                  )
                )
              ),
              fluidRow(
                column(
                  width = 12,
                  uiOutput("nj_tipcolor_mapping_info")
                )
              ),
              hr(),
              fluidRow(
                div(
                  class = "nj-label-control-col",
                  column(
                    width = 8,
                    align = "left",
                    h4(
                      p("Tip Point Shape"),
                      style = "color:white; position: relative; right: -15px; "
                    )
                  )
                ),
                div(
                  class = "nj-label-control-col",
                  column(
                    width = 4,
                    align = "center",
                    div(
                      class = "mat-switch-v",
                      materialSwitch(
                        "nj_tipshape_mapping_show",
                        "",
                        value = isolate(nj_tipshape_mapping_show_val())
                      )
                    )
                  )
                )
              ),
              fluidRow(
                column(
                  width = 8,
                  uiOutput("nj_tipshape_mapping")
                )
              ),
              fluidRow(
                column(
                  width = 12,
                  uiOutput("nj_tipshape_mapping_info")
                )
              ),
              hr(),
              fluidRow(
                div(
                  class = "nj-label-control-col",
                  column(
                    width = 4,
                    align = "left",
                    h4(
                      p("Tiles"),
                      style = "color:white; position: relative; right: -15px; "
                    )
                  )
                ),
                div(
                  class = "nj-label-control-col",
                  column(
                    width = 4,
                    div(
                      class = "tile-sel",
                      selectInput(
                        "nj_tile_num",
                        "",
                        choices = 1:5,
                        width = "50px"
                      )
                    )
                  )
                ),
                column(
                  width = 4,
                  align = "center",
                  conditionalPanel(
                    "input.nj_tile_num == 1",
                    div(
                      class = "mat-switch-v",
                      materialSwitch(
                        "nj_tiles_show_1",
                        "",
                        value = isolate(nj_tiles_show_1_val())
                      )
                    )
                  ),
                  conditionalPanel(
                    "input.nj_tile_num == 2",
                    div(
                      class = "mat-switch-v",
                      materialSwitch(
                        "nj_tiles_show_2",
                        "",
                        value = isolate(nj_tiles_show_2_val())
                      )
                    )
                  ),
                  conditionalPanel(
                    "input.nj_tile_num == 3",
                    div(
                      class = "mat-switch-v",
                      materialSwitch(
                        "nj_tiles_show_3",
                        "",
                        value = isolate(nj_tiles_show_3_val())
                      )
                    )
                  ),
                  conditionalPanel(
                    "input.nj_tile_num == 4",
                    div(
                      class = "mat-switch-v",
                      materialSwitch(
                        "nj_tiles_show_4",
                        "",
                        value = isolate(nj_tiles_show_4_val())
                      )
                    )
                  ),
                  conditionalPanel(
                    "input.nj_tile_num == 5",
                    div(
                      class = "mat-switch-v",
                      materialSwitch(
                        "nj_tiles_show_5",
                        "",
                        value = isolate(nj_tiles_show_5_val())
                      )
                    )
                  )
                )
              ),
              fluidRow(
                column(
                  width = 8,
                  conditionalPanel(
                    "input.nj_tile_num == 1",
                    uiOutput("nj_fruit_variable")
                  ),
                  conditionalPanel(
                    "input.nj_tile_num == 2",
                    uiOutput("nj_fruit_variable_2")
                  ),
                  conditionalPanel(
                    "input.nj_tile_num == 3",
                    uiOutput("nj_fruit_variable_3")
                  ),
                  conditionalPanel(
                    "input.nj_tile_num == 4",
                    uiOutput("nj_fruit_variable_4")
                  ),
                  conditionalPanel(
                    "input.nj_tile_num == 5",
                    uiOutput("nj_fruit_variable_5")
                  )
                ),
                column(
                  width = 3,
                  align = "center",
                  dropMenu(
                    actionBttn(
                      "nj_variable_tiles_menu",
                      label = "",
                      color = "default",
                      size = "sm",
                      style = "material-flat",
                      icon = icon("sliders")
                    ),
                    placement = "right",
                    theme = "translucent",
                    fluidRow(
                      column(
                        width = 12,
                        align = "center",
                        conditionalPanel(
                          "input.nj_tile_num == 1",
                          uiOutput("nj_tiles_scale_1"),
                          br(),
                          uiOutput("nj_tiles_mid_scale_1")
                        ),
                        conditionalPanel(
                          "input.nj_tile_num == 2",
                          uiOutput("nj_tiles_scale_2"),
                          br(),
                          uiOutput("nj_tiles_mid_scale_2")
                        ),
                        conditionalPanel(
                          "input.nj_tile_num == 3",
                          uiOutput("nj_tiles_scale_3"),
                          br(),
                          uiOutput("nj_tiles_mid_scale_3")
                        ),
                        conditionalPanel(
                          "input.nj_tile_num == 4",
                          uiOutput("nj_tiles_scale_4"),
                          br(),
                          uiOutput("nj_tiles_mid_scale_4")
                        ),
                        conditionalPanel(
                          "input.nj_tile_num == 5",
                          uiOutput("nj_tiles_scale_5"),
                          br(),
                          uiOutput("nj_tiles_mid_scale_5")
                        )
                      )
                    )
                  )
                )
              ),
              fluidRow(
                column(
                  width = 12,
                  uiOutput("nj_fruit_mapping_info")
                )
              ),
              hr(),
              fluidRow(
                div(
                  class = "nj-label-control-col",
                  column(
                    width = 8,
                    align = "left",
                    h4(
                      p("Heatmap"),
                      style = "color:grey; position: relative; right: -15px; "
                    )
                  )
                ),
                div(
                  class = "nj-label-control-col",
                  column(
                    width = 4,
                    align = "center",
                    div(
                      class = "mat-switch-v",
                      disabled(
                        materialSwitch(
                          "nj_heatmap_show",
                          "",
                          value = isolate(nj_heatmap_show_val())
                        )
                      )
                    )
                  )
                )
              ),
              fluidRow(
                column(
                  width = 8,
                  disabled(
                    actionButton("nj_heatmap_button", "Select Variables")
                  )
                ),
                column(1),
                column(
                  width = 3,
                  align = "center",
                  dropMenu(
                    disabled(
                      actionBttn(
                        "nj_variable_heatmap_menu",
                        label = "",
                        color = "default",
                        size = "sm",
                        style = "material-flat",
                        icon = icon("sliders")
                      )
                    ),
                    placement = "right",
                    theme = "translucent",
                    fluidRow(
                      column(
                        width = 12,
                        align = "center",
                        uiOutput("nj_heatmap_scale"),
                        br(),
                        uiOutput("nj_heatmap_mid_scale")
                      )
                    )
                  )
                )
              ),
              fluidRow(
                column(
                  width = 12,
                  uiOutput("nj_heatmap_mapping_info")
                )
              ),
              br()
            )
          )
        )
      )
    )

    runjs(unblock_ui)
  })

  ###### Variable Inputs ----

  # Tip label variable mapping

  output$nj_color_mapping <- renderUI({
    if (!is.null(Vis$meta_nj)) {
      choices <- colnames(Vis$meta_nj)[-c(1:4, 6, 12:14)]
    } else {
      choices <- colnames(DB$meta[, -2])[-c(1:4, 6, 12:14)]
    }

    output <- render_plot_control(
      input_id = "nj_color_mapping",
      input_type = "selectInput",
      choices = choices,
      reactive_value = nj_color_mapping_val(),
      default_value = "Country",
      reset = isolate(Vis$nj_color_mapping_val_reset),
      show_condition = isTRUE(nj_mapping_show_val())
    )

    isolate(Vis$nj_color_mapping_val_reset <- FALSE)

    output
  })

  output$nj_tiplab_scale <- renderUI({
    if (!is.null(Vis$meta_nj)) {
      meta_nj <- Vis$meta_nj
    } else {
      meta_nj <- DB$meta[, -2]
    }

    output <- render_plot_control(
      input_id = "nj_tiplab_scale",
      input_type = "selectInput",
      label = h5("Color Scale", style = "color: white; margin-bottom: 0px;"),
      width = "150px",
      choices = determine_scale_choices(
        meta_nj = meta_nj,
        variable_val = nj_color_mapping_val(),
        gradient_scales = gradient_scales,
        diverging_scales = diverging_scales,
        qualitative_scales = qualitative_scales,
        sequential_scales = sequential_scales
      ),
      reactive_value = nj_tiplab_scale_val(),
      default_value = nj_tiplab_scale_val_default(),
      reset = isolate(Vis$nj_tiplab_scale_reset),
      show_condition = nj_mapping_show_val()
    )

    isolate(Vis$nj_tiplab_scale_val_reset <- FALSE)

    output
  })

  output$nj_tiplab_mid_scale <- renderUI({
    req(Vis$meta_nj)

    output <- render_plot_control(
      input_id = "nj_color_mapping_div_mid",
      input_type = "selectInput",
      label = h5("Midpoint", style = "color: white; margin-bottom: 0px;"),
      choices = c("Zero", "Mean", "Median"),
      reactive_value = nj_color_mapping_div_mid_val(),
      default_value = "Mean",
      reset = isolate(Vis$nj_color_mapping_div_mid_reset),
      show_condition = isTRUE(nj_mapping_show_val()) &&
        !is.null(Vis$meta_nj) &&
        is.numeric(unlist(Vis$meta_nj[nj_color_mapping_val()])) &&
        nj_tiplab_scale_val() %in% unlist(diverging_scales)
    )

    isolate(Vis$nj_color_mapping_div_mid_reset <- FALSE)

    output
  })

  output$nj_tiplab_mapping_info <- renderUI({
    req(Vis$meta_nj)

    if (is.numeric(unlist(Vis$meta_nj[nj_color_mapping_val()]))) {
      fluidRow(
        h5(
          "Continous values",
          style = "color: white; font-size: 12px; margin-top: 5px; margin-left: 35px; margin-bottom: -12px;"
        )
      )
    } else {
      if (length(unique(unlist(Vis$meta_nj[nj_color_mapping_val()]))) > 7) {
        fluidRow(
          h5(
            paste0(
              "> 7 (",
              length(unique(unlist(Vis$meta_nj[nj_color_mapping_val()]))),
              ") categorical values"
            ),
            style = "color: #E18B00; font-size: 12px; margin-top: 5px; margin-left: 35px; margin-bottom: -12px;"
          )
        )
      } else {
        fluidRow(
          h5(
            paste0(
              length(unique(unlist(Vis$meta_nj[nj_color_mapping_val()]))),
              paste0(" categorical values")
            ),
            style = "color: white; font-size: 12px; margin-top: 5px; margin-left: 35px; margin-bottom: -12px;"
          )
        )
      }
    }
  })

  # Tip point variable mapping

  output$nj_tipcolor_mapping <- renderUI({
    if (!is.null(Vis$meta_nj)) {
      choices <- colnames(Vis$meta_nj)[-c(1:4, 6, 12:14)]
    } else {
      choices <- colnames(DB$meta[, -2])[-c(1:4, 6, 12:14)]
    }

    output <- render_plot_control(
      input_id = "nj_tipcolor_mapping",
      input_type = "selectInput",
      choices = choices,
      reactive_value = nj_tipcolor_mapping_val(),
      default_value = "Country",
      reset = isolate(Vis$nj_tipcolor_mapping_val_reset),
      show_condition = isTRUE(nj_tipcolor_mapping_show_val())
    )

    isolate(Vis$nj_tipcolor_mapping_val_reset <- FALSE)

    output
  })

  output$nj_tippoint_scale <- renderUI({
    if (!is.null(Vis$meta_nj)) {
      meta_nj <- Vis$meta_nj
    } else {
      meta_nj <- DB$meta[, -2]
    }

    output <- render_plot_control(
      input_id = "nj_tippoint_scale",
      input_type = "selectInput",
      label = h5("Color Scale", style = "color: white; margin-bottom: 0px;"),
      width = "150px",
      choices = determine_scale_choices(
        meta_nj = meta_nj,
        variable_val = nj_tipcolor_mapping_val(),
        gradient_scales = gradient_scales,
        diverging_scales = diverging_scales,
        qualitative_scales = qualitative_scales,
        sequential_scales = sequential_scales
      ),
      reactive_value = nj_tippoint_scale_val(),
      default_value = nj_tippoint_scale_val_default(),
      reset = isolate(Vis$nj_tippoint_scale_reset),
      show_condition = nj_tipcolor_mapping_show_val()
    )

    isolate(Vis$nj_tippoint_scale_reset <- FALSE)

    output
  })

  output$nj_tippoint_mid_scale <- renderUI({
    req(Vis$meta_nj)

    output <- render_plot_control(
      input_id = "nj_tipcolor_mapping_div_mid",
      input_type = "selectInput",
      label = h5("Midpoint", style = "color: white; margin-bottom: 0px;"),
      choices = c("Zero", "Mean", "Median"),
      reactive_value = nj_tipcolor_mapping_div_mid_val(),
      default_value = "Mean",
      reset = isolate(Vis$nj_tipcolor_mapping_div_mid_reset),
      show_condition = isTRUE(nj_tipcolor_mapping_show_val()) &&
        !is.null(Vis$meta_nj) &&
        is.numeric(unlist(Vis$meta_nj[nj_tipcolor_mapping_val()])) &&
        nj_tippoint_scale_val() %in% unlist(diverging_scales)
    )

    isolate(Vis$nj_tipcolor_mapping_div_mid_reset <- FALSE)

    output
  })

  output$nj_tipcolor_mapping_info <- renderUI({
    req(Vis$meta_nj)

    if (is.numeric(unlist(Vis$meta_nj[nj_tipcolor_mapping_val()]))) {
      fluidRow(
        h5(
          "Continous values",
          style = "color: white; font-size: 12px; margin-top: 5px; margin-left: 35px; margin-bottom: -12px;"
        )
      )
    } else {
      if (length(unique(unlist(Vis$meta_nj[nj_tipcolor_mapping_val()]))) > 7) {
        fluidRow(
          h5(
            paste0(
              "> 7 (",
              length(unique(unlist(Vis$meta_nj[nj_tipcolor_mapping_val()]))),
              ") categorical values"
            ),
            style = "color: #E18B00; font-size: 12px; margin-top: 5px; margin-left: 35px; margin-bottom: -12px;"
          )
        )
      } else {
        fluidRow(
          h5(
            paste0(
              length(unique(unlist(Vis$meta_nj[nj_tipcolor_mapping_val()]))),
              paste0(" categorical values")
            ),
            style = "color: white; font-size: 12px; margin-top: 5px; margin-left: 35px; margin-bottom: -12px;"
          )
        )
      }
    }
  })

  # Tip shape variable mapping

  output$nj_tipshape_mapping <- renderUI({
    if (!is.null(Vis$meta_nj)) {
      choices <- names(Vis$meta_nj)[-c(1:4, 6, 12:14)]

      if (!is.null(DB$cust_var) && length(DB$cust_var) > 0) {
        categ_vars <- DB$cust_var$Variable[which(DB$cust_var$Type == "categ")]
        choices <- choices[-which(choices %in% categ_vars)]
        choices <- c(choices, categ_vars)
      }
    } else {
      choices = c(
        Database = "Database",
        `Isolation Date` = "Isolation Date",
        Host = "Host",
        Country = "Country",
        City = "City"
      )
    }

    output <- render_plot_control(
      input_id = "nj_tipshape_mapping",
      input_type = "selectInput",
      choices = choices,
      reactive_value = nj_tipshape_mapping_val(),
      default_value = "Host",
      reset = isolate(Vis$nj_tipshape_mapping_val_reset),
      show_condition = isTRUE(nj_tipshape_mapping_show_val())
    )

    isolate(Vis$nj_tipshape_mapping_val_reset <- FALSE)

    output
  })

  output$nj_tipshape_mapping_info <- renderUI({
    req(Vis$meta_nj)

    if (is.numeric(unlist(Vis$meta_nj[nj_tipshape_mapping_val()]))) {
      fluidRow(
        h5(
          "Mapping continous variables to shape not possible",
          style = "color: #E18B00; font-size: 12px; margin-top: 5px; margin-left: 35px; margin-bottom: -12px;"
        )
      )
    } else {
      if (length(unique(unlist(Vis$meta_nj[nj_tipshape_mapping_val()]))) > 6) {
        fluidRow(
          h5(
            "Mapping > 6 variables to shape not possible",
            style = "color: #E18B00; font-size: 12px; margin-top: 5px; margin-left: 35px; margin-bottom: -12px;"
          )
        )
      } else {
        fluidRow(
          h5(
            paste0(
              length(unique(unlist(Vis$meta_nj[nj_tipshape_mapping_val()]))),
              paste0(" categorical values")
            ),
            style = "color: white; font-size: 12px; margin-top: 5px; margin-left: 35px; margin-bottom: -12px;"
          )
        )
      }
    }
  })

  # Tiles variables mapping

  output$nj_fruit_variable <- renderUI({
    if (!is.null(Vis$meta_nj)) {
      choices <- colnames(Vis$meta_nj)[-c(1:4, 6, 12:14)]
    } else {
      choices <- colnames(DB$meta[, -2])[-c(1:4, 6, 12:14)]
    }

    output <- render_plot_control(
      input_id = "nj_fruit_variable",
      input_type = "selectInput",
      choices = choices,
      div_class = "nj-fruit-variable",
      reactive_value = nj_fruit_variable_val(),
      default_value = "Isolation Date",
      reset = isolate(Vis$nj_fruit_variable_val_reset),
      show_condition = isTRUE(nj_tiles_show_1_val())
    )

    isolate(Vis$nj_fruit_variable_val_reset <- FALSE)

    output
  })

  output$nj_fruit_variable_2 <- renderUI({
    if (!is.null(Vis$meta_nj)) {
      choices <- colnames(Vis$meta_nj)[-c(1:4, 6, 12:14)]
    } else {
      choices <- colnames(DB$meta[, -2])[-c(1:4, 6, 12:14)]
    }

    output <- render_plot_control(
      input_id = "nj_fruit_variable_2",
      input_type = "selectInput",
      choices = choices,
      div_class = "nj-fruit-variable",
      reactive_value = nj_fruit_variable_2_val(),
      default_value = "Isolation Date",
      reset = isolate(Vis$nj_fruit_variable_2_val_reset),
      show_condition = isTRUE(nj_tiles_show_2_val())
    )

    isolate(Vis$nj_fruit_variable_2_val_reset <- FALSE)

    output
  })

  output$nj_fruit_variable_3 <- renderUI({
    if (!is.null(Vis$meta_nj)) {
      choices <- colnames(Vis$meta_nj)[-c(1:4, 6, 12:14)]
    } else {
      choices <- colnames(DB$meta[, -2])[-c(1:4, 6, 12:14)]
    }

    output <- render_plot_control(
      input_id = "nj_fruit_variable_3",
      input_type = "selectInput",
      choices = choices,
      div_class = "nj-fruit-variable",
      reactive_value = nj_fruit_variable_3_val(),
      default_value = "Isolation Date",
      reset = isolate(Vis$nj_fruit_variable_3_val_reset),
      show_condition = isTRUE(nj_tiles_show_3_val())
    )

    isolate(Vis$nj_fruit_variable_3_val_reset <- FALSE)

    output
  })

  output$nj_fruit_variable_4 <- renderUI({
    if (!is.null(Vis$meta_nj)) {
      choices <- colnames(Vis$meta_nj)[-c(1:4, 6, 12:14)]
    } else {
      choices <- colnames(DB$meta[, -2])[-c(1:4, 6, 12:14)]
    }

    output <- render_plot_control(
      input_id = "nj_fruit_variable_4",
      input_type = "selectInput",
      choices = choices,
      div_class = "nj-fruit-variable",
      reactive_value = nj_fruit_variable_4_val(),
      default_value = "Isolation Date",
      reset = isolate(Vis$nj_fruit_variable_4_val_reset),
      show_condition = isTRUE(nj_tiles_show_4_val())
    )

    isolate(Vis$nj_fruit_variable_4_val_reset <- FALSE)

    output
  })

  output$nj_fruit_variable_5 <- renderUI({
    if (!is.null(Vis$meta_nj)) {
      choices <- colnames(Vis$meta_nj)[-c(1:4, 6, 12:14)]
    } else {
      choices <- colnames(DB$meta[, -2])[-c(1:4, 6, 12:14)]
    }

    output <- render_plot_control(
      input_id = "nj_fruit_variable_5",
      input_type = "selectInput",
      choices = choices,
      div_class = "nj-fruit-variable",
      reactive_value = nj_fruit_variable_5_val(),
      default_value = "Isolation Date",
      reset = isolate(Vis$nj_fruit_variable_5_val_reset),
      show_condition = isTRUE(nj_tiles_show_5_val())
    )

    isolate(Vis$nj_fruit_variable_5_val_reset <- FALSE)

    output
  })

  output$nj_tiles_scale_1 <- renderUI({
    if (!is.null(Vis$meta_nj)) {
      meta_nj <- Vis$meta_nj
    } else {
      meta_nj <- DB$meta[, -2]
    }

    output <- render_plot_control(
      input_id = "nj_tiles_scale_1",
      input_type = "selectInput",
      label = h5("Color Scale", style = "color: white; margin-bottom: 0px;"),
      width = "150px",
      choices = determine_scale_choices(
        meta_nj = meta_nj,
        variable_val = nj_fruit_variable_val(),
        gradient_scales = gradient_scales,
        diverging_scales = diverging_scales,
        qualitative_scales = qualitative_scales,
        sequential_scales = sequential_scales
      ),
      reactive_value = nj_tiles_scale_1_val(),
      default_value = nj_tiles_scale_1_val_default(),
      div_class = "nj-tiles-scale",
      reset = isolate(Vis$nj_tiles_scale_1_reset),
      show_condition = !is.null(Vis$meta_nj) && nj_tiles_show_1_val()
    )

    isolate(Vis$nj_tiles_scale_1_reset <- FALSE)

    output
  })

  output$nj_tiles_scale_2 <- renderUI({
    if (!is.null(Vis$meta_nj)) {
      meta_nj <- Vis$meta_nj
    } else {
      meta_nj <- DB$meta[, -2]
    }

    output <- render_plot_control(
      input_id = "nj_tiles_scale_2",
      input_type = "selectInput",
      label = h5("Color Scale", style = "color: white; margin-bottom: 0px;"),
      width = "150px",
      choices = determine_scale_choices(
        meta_nj = meta_nj,
        variable_val = nj_fruit_variable_2_val(),
        gradient_scales = gradient_scales,
        diverging_scales = diverging_scales,
        qualitative_scales = qualitative_scales,
        sequential_scales = sequential_scales
      ),
      reactive_value = nj_tiles_scale_2_val(),
      default_value = nj_tiles_scale_2_val_default(),
      div_class = "nj-tiles-scale",
      reset = isolate(Vis$nj_tiles_scale_2_reset),
      show_condition = !is.null(Vis$meta_nj) && nj_tiles_show_2_val()
    )

    isolate(Vis$nj_tiles_scale_2_reset <- FALSE)

    output
  })

  output$nj_tiles_scale_3 <- renderUI({
    if (!is.null(Vis$meta_nj)) {
      meta_nj <- Vis$meta_nj
    } else {
      meta_nj <- DB$meta[, -2]
    }

    output <- render_plot_control(
      input_id = "nj_tiles_scale_3",
      input_type = "selectInput",
      label = h5("Color Scale", style = "color: white; margin-bottom: 0px;"),
      width = "150px",
      choices = determine_scale_choices(
        meta_nj = meta_nj,
        variable_val = nj_fruit_variable_3_val(),
        gradient_scales = gradient_scales,
        diverging_scales = diverging_scales,
        qualitative_scales = qualitative_scales,
        sequential_scales = sequential_scales
      ),
      reactive_value = nj_tiles_scale_3_val(),
      default_value = nj_tiles_scale_3_val_default(),
      div_class = "nj-tiles-scale",
      reset = isolate(Vis$nj_tiles_scale_3_reset),
      show_condition = !is.null(Vis$meta_nj) && nj_tiles_show_3_val()
    )

    isolate(Vis$nj_tiles_scale_3_reset <- FALSE)

    output
  })

  output$nj_tiles_scale_4 <- renderUI({
    if (!is.null(Vis$meta_nj)) {
      meta_nj <- Vis$meta_nj
    } else {
      meta_nj <- DB$meta[, -2]
    }

    output <- render_plot_control(
      input_id = "nj_tiles_scale_4",
      input_type = "selectInput",
      label = h5("Color Scale", style = "color: white; margin-bottom: 0px;"),
      width = "150px",
      choices = determine_scale_choices(
        meta_nj = meta_nj,
        variable_val = nj_fruit_variable_4_val(),
        gradient_scales = gradient_scales,
        diverging_scales = diverging_scales,
        qualitative_scales = qualitative_scales,
        sequential_scales = sequential_scales
      ),
      reactive_value = nj_tiles_scale_4_val(),
      default_value = nj_tiles_scale_4_val_default(),
      div_class = "nj-tiles-scale",
      reset = isolate(Vis$nj_tiles_scale_4_reset),
      show_condition = !is.null(Vis$meta_nj) && nj_tiles_show_4_val()
    )

    isolate(Vis$nj_tiles_scale_4_reset <- FALSE)

    output
  })

  output$nj_tiles_scale_5 <- renderUI({
    if (!is.null(Vis$meta_nj)) {
      meta_nj <- Vis$meta_nj
    } else {
      meta_nj <- DB$meta[, -2]
    }

    output <- render_plot_control(
      input_id = "nj_tiles_scale_5",
      input_type = "selectInput",
      label = h5("Color Scale", style = "color: white; margin-bottom: 0px;"),
      width = "150px",
      choices = determine_scale_choices(
        meta_nj = meta_nj,
        variable_val = nj_fruit_variable_5_val(),
        gradient_scales = gradient_scales,
        diverging_scales = diverging_scales,
        qualitative_scales = qualitative_scales,
        sequential_scales = sequential_scales
      ),
      reactive_value = nj_tiles_scale_5_val(),
      default_value = nj_tiles_scale_5_val_default(),
      div_class = "nj-tiles-scale",
      reset = isolate(Vis$nj_tiles_scale_5_reset),
      show_condition = !is.null(Vis$meta_nj) && nj_tiles_show_5_val()
    )

    isolate(Vis$nj_tiles_scale_5_reset <- FALSE)

    output
  })

  output$nj_tiles_mid_scale_1 <- renderUI({
    req(Vis$meta_nj)

    output <- render_plot_control(
      input_id = "nj_tiles_mapping_div_mid_1",
      input_type = "selectInput",
      label = h5("Midpoint", style = "color: white; margin-bottom: 0px;"),
      choices = c("Zero", "Mean", "Median"),
      reactive_value = nj_tiles_mapping_div_mid_1_val(),
      default_value = "Mean",
      div_class = "nj-tiles-scale",
      reset = isolate(Vis$nj_tiles_mapping_div_mid_1_reset),
      show_condition = nj_tiles_show_1_val() &&
        !is.null(Vis$meta_nj) &&
        is.numeric(unlist(Vis$meta_nj[nj_fruit_variable_val()])) &&
        nj_tiles_scale_1_val() %in% unlist(diverging_scales)
    )

    isolate(Vis$nj_tiles_mapping_div_mid_1_reset <- FALSE)

    output
  })

  output$nj_tiles_mid_scale_2 <- renderUI({
    req(Vis$meta_nj)

    output <- render_plot_control(
      input_id = "nj_tiles_mapping_div_mid_2",
      input_type = "selectInput",
      label = h5("Midpoint", style = "color: white; margin-bottom: 0px;"),
      choices = c("Zero", "Mean", "Median"),
      reactive_value = nj_tiles_mapping_div_mid_2_val(),
      default_value = "Mean",
      div_class = "nj-tiles-scale",
      reset = isolate(Vis$nj_tiles_mapping_div_mid_2_reset),
      show_condition = nj_tiles_show_2_val() &&
        !is.null(Vis$meta_nj) &&
        is.numeric(unlist(Vis$meta_nj[nj_fruit_variable_2_val()])) &&
        nj_tiles_scale_2_val() %in% unlist(diverging_scales)
    )

    isolate(Vis$nj_tiles_mapping_div_mid_2_reset <- FALSE)

    output
  })

  output$nj_tiles_mid_scale_3 <- renderUI({
    req(Vis$meta_nj)

    output <- render_plot_control(
      input_id = "nj_tiles_mapping_div_mid_3",
      input_type = "selectInput",
      label = h5("Midpoint", style = "color: white; margin-bottom: 0px;"),
      choices = c("Zero", "Mean", "Median"),
      reactive_value = nj_tiles_mapping_div_mid_3_val(),
      default_value = "Mean",
      div_class = "nj-tiles-scale",
      reset = isolate(Vis$nj_tiles_mapping_div_mid_3_reset),
      show_condition = nj_tiles_show_3_val() &&
        !is.null(Vis$meta_nj) &&
        is.numeric(unlist(Vis$meta_nj[nj_fruit_variable_3_val()])) &&
        nj_tiles_scale_3_val() %in% unlist(diverging_scales)
    )

    isolate(Vis$nj_tiles_mapping_div_mid_3_reset <- FALSE)

    output
  })

  output$nj_tiles_mid_scale_4 <- renderUI({
    req(Vis$meta_nj)

    output <- render_plot_control(
      input_id = "nj_tiles_mapping_div_mid_4",
      input_type = "selectInput",
      label = h5("Midpoint", style = "color: white; margin-bottom: 0px;"),
      choices = c("Zero", "Mean", "Median"),
      reactive_value = nj_tiles_mapping_div_mid_4_val(),
      default_value = "Mean",
      div_class = "nj-tiles-scale",
      reset = isolate(Vis$nj_tiles_mapping_div_mid_4_reset),
      show_condition = nj_tiles_show_4_val() &&
        !is.null(Vis$meta_nj) &&
        is.numeric(unlist(Vis$meta_nj[nj_fruit_variable_4_val()])) &&
        nj_tiles_scale_4_val() %in% unlist(diverging_scales)
    )

    isolate(Vis$nj_tiles_mapping_div_mid_4_reset <- FALSE)

    output
  })

  output$nj_tiles_mid_scale_5 <- renderUI({
    req(Vis$meta_nj)

    output <- render_plot_control(
      input_id = "nj_tiles_mapping_div_mid_5",
      input_type = "selectInput",
      label = h5("Midpoint", style = "color: white; margin-bottom: 0px;"),
      choices = c("Zero", "Mean", "Median"),
      reactive_value = nj_tiles_mapping_div_mid_5_val(),
      default_value = "Mean",
      reset = isolate(Vis$nj_tiles_mapping_div_mid_5_reset),
      show_condition = nj_tiles_show_5_val() &&
        !is.null(Vis$meta_nj) &&
        is.numeric(unlist(Vis$meta_nj[nj_fruit_variable_5_val()])) &&
        nj_tiles_scale_5_val() %in% unlist(diverging_scales)
    )

    isolate(Vis$nj_tiles_mapping_div_mid_5_reset <- FALSE)

    output
  })

  output$nj_fruit_mapping_info <- renderUI({
    req(input$nj_tile_num, Vis$meta_nj)

    if (input$nj_tile_num == 1) {
      if (is.numeric(unlist(Vis$meta_nj[nj_fruit_variable_val()]))) {
        fluidRow(
          h5(
            "Continous values",
            style = "color: white; font-size: 12px; margin-top: 5px; margin-left: 35px; margin-bottom: -12px;"
          )
        )
      } else {
        if (length(unique(unlist(Vis$meta_nj[nj_fruit_variable_val()]))) > 7) {
          fluidRow(
            h5(
              paste0(
                "> 7 (",
                length(unique(unlist(Vis$meta_nj[nj_fruit_variable_val()]))),
                ") categorical values"
              ),
              style = "color: #E18B00; font-size: 12px; margin-top: 5px; margin-left: 35px; margin-bottom: -12px;"
            )
          )
        } else {
          fluidRow(
            h5(
              paste0(
                length(unique(unlist(Vis$meta_nj[nj_fruit_variable_val()]))),
                paste0(" categorical values")
              ),
              style = "color: white; font-size: 12px; margin-top: 5px; margin-left: 35px; margin-bottom: -12px;"
            )
          )
        }
      }
    } else if (input$nj_tile_num == 2) {
      if (is.numeric(unlist(Vis$meta_nj[nj_fruit_variable_2_val()]))) {
        fluidRow(
          h5(
            "Continous values",
            style = "color: white; font-size: 12px; margin-top: 5px; margin-left: 35px; margin-bottom: -12px;"
          )
        )
      } else {
        if (
          length(unique(unlist(Vis$meta_nj[nj_fruit_variable_2_val()]))) > 7
        ) {
          fluidRow(
            h5(
              paste0(
                "> 7 (",
                length(unique(unlist(Vis$meta_nj[nj_fruit_variable_2_val()]))),
                ") categorical values"
              ),
              style = "color: #E18B00; font-size: 12px; margin-top: 5px; margin-left: 35px; margin-bottom: -12px;"
            )
          )
        } else {
          fluidRow(
            h5(
              paste0(
                length(unique(unlist(Vis$meta_nj[nj_fruit_variable_2_val()]))),
                paste0(" categorical values")
              ),
              style = "color: white; font-size: 12px; margin-top: 5px; margin-left: 35px; margin-bottom: -12px;"
            )
          )
        }
      }
    } else if (input$nj_tile_num == 3) {
      if (is.numeric(unlist(Vis$meta_nj[nj_fruit_variable_3_val()]))) {
        fluidRow(
          h5(
            "Continous values",
            style = "color: white; font-size: 12px; margin-top: 5px; margin-left: 35px; margin-bottom: -12px;"
          )
        )
      } else {
        if (
          length(unique(unlist(Vis$meta_nj[nj_fruit_variable_3_val()]))) > 7
        ) {
          fluidRow(
            h5(
              paste0(
                "> 7 (",
                length(unique(unlist(Vis$meta_nj[nj_fruit_variable_3_val()]))),
                ") categorical values"
              ),
              style = "color: #E18B00; font-size: 12px; margin-top: 5px; margin-left: 35px; margin-bottom: -12px;"
            )
          )
        } else {
          fluidRow(
            h5(
              paste0(
                length(unique(unlist(Vis$meta_nj[nj_fruit_variable_3_val()]))),
                paste0(" categorical values")
              ),
              style = "color: white; font-size: 12px; margin-top: 5px; margin-left: 35px; margin-bottom: -12px;"
            )
          )
        }
      }
    } else if (input$nj_tile_num == 4) {
      if (is.numeric(unlist(Vis$meta_nj[nj_fruit_variable_4_val()]))) {
        fluidRow(
          h5(
            "Continous values",
            style = "color: white; font-size: 12px; margin-top: 5px; margin-left: 35px; margin-bottom: -12px;"
          )
        )
      } else {
        if (
          length(unique(unlist(Vis$meta_nj[nj_fruit_variable_4_val()]))) > 7
        ) {
          fluidRow(
            h5(
              paste0(
                "> 7 (",
                length(unique(unlist(Vis$meta_nj[nj_fruit_variable_4_val()]))),
                ") categorical values"
              ),
              style = "color: #E18B00; font-size: 12px; margin-top: 5px; margin-left: 35px; margin-bottom: -12px;"
            )
          )
        } else {
          fluidRow(
            h5(
              paste0(
                length(unique(unlist(Vis$meta_nj[nj_fruit_variable_4_val()]))),
                paste0(" categorical values")
              ),
              style = "color: white; font-size: 12px; margin-top: 5px; margin-left: 35px; margin-bottom: -12px;"
            )
          )
        }
      }
    } else if (input$nj_tile_num == 5) {
      if (is.numeric(unlist(Vis$meta_nj[nj_fruit_variable_5_val()]))) {
        fluidRow(
          h5(
            "Continous values",
            style = "color: white; font-size: 12px; margin-top: 5px; margin-left: 35px; margin-bottom: -12px;"
          )
        )
      } else {
        if (
          length(unique(unlist(Vis$meta_nj[nj_fruit_variable_5_val()]))) > 7
        ) {
          fluidRow(
            h5(
              paste0(
                "> 7 (",
                length(unique(unlist(Vis$meta_nj[nj_fruit_variable_5_val()]))),
                ") categorical values"
              ),
              style = "color: #E18B00; font-size: 12px; margin-top: 5px; margin-left: 35px; margin-bottom: -12px;"
            )
          )
        } else {
          fluidRow(
            h5(
              paste0(
                length(unique(unlist(Vis$meta_nj[nj_fruit_variable_5_val()]))),
                paste0(" categorical values")
              ),
              style = "color: white; font-size: 12px; margin-top: 5px; margin-left: 35px; margin-bottom: -12px;"
            )
          )
        }
      }
    }
  })

  # Heatmap variables mapping

  output$nj_heatmap_sel <- renderUI({
    req(Vis$meta_nj)

    no_screened <- FALSE

    if (!is.null(Vis$meta_nj)) {
      meta <- select(
        Vis$meta_nj,
        -c(
          taxa,
          Index,
          `Assembly ID`,
          `Assembly Name`,
          Scheme,
          `Entry Date`,
          Successes,
          Errors,
          Screened
        )
      )

      if (input$nj_heatmap_map == "Variables") {
        meta <- select(meta, -colnames(Vis$amr_nj))

        # Identify numeric columns
        numeric_columns <- sapply(meta, is.numeric)
        numeric_column_names <- names(meta[numeric_columns])
        non_numeric_column_names <- names(meta)[!numeric_columns]
        non_numeric_column_names <- non_numeric_column_names[!numeric_columns]

        choices <- list()

        # Add Continuous list only if there are numeric columns
        if (length(numeric_column_names) > 0) {
          choices$Continuous <- as.list(setNames(
            numeric_column_names,
            numeric_column_names
          ))
        }

        # Add Diverging list
        choices$Categorical <- setNames(
          non_numeric_column_names,
          non_numeric_column_names
        )
      } else if (input$nj_heatmap_map == "AMR Profile") {
        if (
          !isTRUE(Screening$available) ||
            sum(Vis$meta_nj$Screened == "Yes") == 0
        ) {
          choices <- NULL
          no_screened <- TRUE
        } else {
          choices <- as.list(colnames(Vis$amr_nj))
        }
      }
    } else {
      if (input$nj_heatmap_map == "AMR Profile") {
        choices <- NULL
        if (!isTRUE(Screening$available)) {
          no_screened <- TRUE
        }
      }
      choices <- c(
        `Isolation Date` = "Isolation Date",
        Host = "Host",
        Country = "Country",
        City = "City",
        Database = "Database"
      )
    }

    output <- render_plot_control(
      input_id = "nj_heatmap_select",
      input_type = "pickerInput",
      multiple = TRUE,
      choices = choices,
      options = list(
        `live-search` = TRUE,
        `actions-box` = TRUE,
        size = 10,
        style = "background-color: white; border-radius: 5px;"
      ),
      reactive_value = nj_heatmap_select_val(),
      default_value = NULL,
      reset = isolate(Vis$nj_heatmap_select_val_reset),
      show_condition = isFALSE(no_screened)
    )

    isolate(Vis$nj_heatmap_select_val_reset <- FALSE)

    output
  })

  output$nj_heatmap_scale <- renderUI({
    req(Vis$meta_nj)

    if (!is.null(Vis$meta_nj)) {
      if (class(unlist(Vis$meta_nj[nj_heatmap_select_val()])) == "numeric") {
        choices <- list(
          Continous = gradient_scales,
          Diverging = diverging_scales
        )
      } else {
        if (length(unique(unlist(Vis$meta_nj[nj_heatmap_select_val()]))) > 7) {
          choices <- list(Gradient = gradient_scales)
        } else {
          choices <- list(
            Qualitative = qualitative_scales,
            Sequential = sequential_scales
          )
        }
      }
    } else {
      choices <- list(
        Qualitative = qualitative_scales,
        Sequential = sequential_scales
      )
    }

    output <- render_plot_control(
      input_id = "nj_heatmap_scale",
      input_type = "selectInput",
      label = h5("Color Scale", style = "color: white; margin-bottom: 0px;"),
      width = "150px",
      choices = choices,
      reactive_value = nj_heatmap_scale_val(),
      default_value = nj_heatmap_scale_val_default(),
      div_class = "nj-heatmap-scale",
      reset = isolate(Vis$nj_heatmap_scale_reset),
      show_condition = !is.null(Vis$meta_nj) && nj_heatmap_show_val()
    )

    isolate(Vis$nj_heatmap_scale_reset <- FALSE)

    output
  })

  output$nj_heatmap_mid_scale <- renderUI({
    req(Vis$meta_nj)

    output <- render_plot_control(
      input_id = "nj_heatmap_div_mid",
      input_type = "selectInput",
      label = h5("Midpoint", style = "color: white; margin-bottom: 0px;"),
      choices = c("Zero", "Mean", "Median"),
      reactive_value = nj_heatmap_div_mid_val(),
      default_value = "Mean",
      div_class = "nj-heatmap-div-mid",
      reset = isolate(Vis$nj_heatmap_div_mid_val_reset),
      show_condition = isTRUE(nj_heatmap_show_val()) &&
        !is.null(Vis$meta_nj) &&
        any(sapply(Vis$meta_nj[nj_heatmap_select_val()], is.numeric)) &&
        nj_heatmap_scale_val() %in% unlist(diverging_scales)
    )

    isolate(Vis$nj_heatmap_div_mid_val_reset <- FALSE)

    output
  })

  output$nj_heatmap_mapping_info <- renderUI({
    req(Vis$meta_nj)

    if (!is.null(nj_heatmap_select_val())) {
      if (
        !(any(sapply(Vis$meta_nj[nj_heatmap_select_val()], is.numeric)) &&
          any(!sapply(Vis$meta_nj[nj_heatmap_select_val()], is.numeric)))
      ) {
        if (any(sapply(Vis$meta_nj[nj_heatmap_select_val()], is.numeric))) {
          fluidRow(
            h5(
              "Continous values",
              style = "color: white; font-size: 12px; margin-top: 10px; margin-left: 35px; margin-bottom: -12px;"
            )
          )
        } else {
          if (
            length(unique(unlist(Vis$meta_nj[nj_heatmap_select_val()]))) > 7
          ) {
            fluidRow(
              h5(
                paste0("> 7 categorical values"),
                style = "color: #E18B00; font-size: 12px; margin-top: 10px; margin-left: 35px; margin-bottom: -12px;"
              )
            )
          } else {
            fluidRow(
              h5(
                "Categorical values",
                style = "color: white; font-size: 12px; margin-top: 10px; margin-left: 35px; margin-bottom: -12px;"
              )
            )
          }
        }
      } else {
        NULL
      }
    } else {
      NULL
    }
  })

  output$nj_heatmap_var_warning <- renderUI({
    req(Vis$meta_nj)

    if (
      !is.null(Vis$meta_nj) &&
        any(sapply(Vis$meta_nj[nj_heatmap_select_val()], is.numeric)) &&
        any(!sapply(Vis$meta_nj[nj_heatmap_select_val()], is.numeric))
    ) {
      h5(
        "Heatmap with both categorical and continous values not possible",
        style = "color: #E18B00; font-size: 12px; font-style: italic; margin-left: 20px"
      )
    } else {
      h5("")
    }
  })

  ##### Color Menu ----

  ###### Color Control Values ----

  nj_color_val <- reactiveVal()
  nj_bg_val <- reactiveVal()
  nj_title_color_val <- reactiveVal()
  nj_tiplab_color_val <- reactiveVal()
  nj_tiplab_fill_val <- reactiveVal()
  nj_branch_label_color_val <- reactiveVal()
  nj_tippoint_color_val <- reactiveVal()
  nj_nodepoint_color_val <- reactiveVal()

  observe({
    ifelse(
      !is.null(input$nj_color),
      nj_color_val(input$nj_color),
      nj_color_val("#000000")
    )

    ifelse(!is.null(input$nj_bg), nj_bg_val(input$nj_bg), nj_bg_val("#ffffff"))

    ifelse(
      !is.null(input$nj_title_color),
      nj_title_color_val(input$nj_title_color),
      nj_title_color_val("#000000")
    )

    ifelse(
      !is.null(input$nj_tiplab_color),
      nj_tiplab_color_val(input$nj_tiplab_color),
      nj_tiplab_color_val("#000000")
    )

    ifelse(
      !is.null(input$nj_tiplab_fill),
      nj_tiplab_fill_val(input$nj_tiplab_fill),
      nj_tiplab_fill_val("#84D9A0")
    )

    ifelse(
      !is.null(input$nj_branch_label_color),
      nj_branch_label_color_val(input$nj_branch_label_color),
      nj_branch_label_color_val("#FFB7B7")
    )

    ifelse(
      !is.null(input$nj_tippoint_color),
      nj_tippoint_color_val(input$nj_tippoint_color),
      nj_tippoint_color_val("#3A4657")
    )

    ifelse(
      !is.null(input$nj_nodepoint_color),
      nj_nodepoint_color_val(input$nj_nodepoint_color),
      nj_nodepoint_color_val("#3A4657")
    )
  })

  ###### Color Interface ----

  observeEvent(input$nj_color_menu, {
    runjs(block_ui)

    session$sendCustomMessage('nj_reset_style', "")
    session$sendCustomMessage('nj_highlight', "nj_color_menu")

    output$tree_controls <- renderUI(
      box(
        solidHeader = TRUE,
        status = "primary",
        width = "100%",
        title = "Color Menu",
        column(
          width = 12,
          br(),
          fluidRow(
            column(
              width = 6,
              align = "left",
              HTML(
                paste(
                  tags$span(
                    style = 'color: white; font-size: 14px; position: relative; top: 10px;',
                    'Lines/Text'
                  )
                )
              )
            ),
            column(
              width = 6,
              align = "center",
              div(
                class = "control-color",
                colorPickr(
                  inputId = "nj_color",
                  selected = isolate(nj_color_val()),
                  label = "",
                  update = "changestop",
                  interaction = list(clear = FALSE, save = FALSE),
                  position = "right-start",
                  width = "100%"
                )
              )
            )
          ),
          br(),
          fluidRow(
            column(
              width = 6,
              align = "left",
              HTML(
                paste(
                  tags$span(
                    style = 'color: white; font-size: 14px; position: relative; top: 10px;',
                    'Background'
                  )
                )
              )
            ),
            column(
              width = 6,
              align = "center",
              div(
                class = "control-color",
                colorPickr(
                  inputId = "nj_bg",
                  selected = isolate(nj_bg_val()),
                  label = "",
                  update = "changestop",
                  interaction = list(clear = FALSE, save = FALSE),
                  position = "right-start",
                  width = "100%"
                )
              )
            )
          ),
          br(),
          fluidRow(
            column(
              width = 6,
              align = "left",
              HTML(
                paste(
                  tags$span(
                    style = 'color: white; font-size: 14px; position: relative; top: 10px;',
                    'Title'
                  )
                )
              )
            ),
            column(
              width = 6,
              align = "center",
              div(
                class = "control-color",
                colorPickr(
                  inputId = "nj_title_color",
                  selected = isolate(nj_title_color_val()),
                  label = "",
                  update = "changestop",
                  interaction = list(clear = FALSE, save = FALSE),
                  position = "right-start",
                  width = "100%"
                )
              )
            )
          ),
          br(),
          fluidRow(
            column(
              width = 6,
              align = "left",
              HTML(
                paste(
                  tags$span(
                    style = 'color: white; font-size: 14px; position: relative; top: 10px;',
                    'Tip Label'
                  )
                )
              )
            ),
            column(
              width = 6,
              align = "center",
              div(
                class = "control-color",
                colorPickr(
                  inputId = "nj_tiplab_color",
                  selected = isolate(nj_tiplab_color_val()),
                  label = "",
                  update = "changestop",
                  interaction = list(clear = FALSE, save = FALSE),
                  position = "right-start",
                  width = "100%"
                )
              )
            )
          ),
          br(),
          fluidRow(
            column(
              width = 6,
              align = "left",
              HTML(
                paste(
                  tags$span(
                    style = 'color: white; font-size: 14px; position: relative; top: 10px;',
                    'Label Panel'
                  )
                )
              )
            ),
            column(
              width = 6,
              align = "center",
              div(
                class = "control-color",
                colorPickr(
                  inputId = "nj_tiplab_fill",
                  selected = isolate(nj_tiplab_fill_val()),
                  label = "",
                  update = "changestop",
                  interaction = list(clear = FALSE, save = FALSE),
                  position = "right-start",
                  width = "100%"
                )
              )
            )
          ),
          br(),
          fluidRow(
            column(
              width = 6,
              align = "left",
              HTML(
                paste(
                  tags$span(
                    style = 'color: white; font-size: 13px; position: relative; top: 10px;',
                    'Branch Label'
                  )
                )
              )
            ),
            column(
              width = 6,
              align = "center",
              div(
                class = "control-color",
                colorPickr(
                  inputId = "nj_branch_label_color",
                  selected = isolate(nj_branch_label_color_val()),
                  label = "",
                  update = "changestop",
                  interaction = list(clear = FALSE, save = FALSE),
                  position = "right-start",
                  width = "100%"
                )
              )
            )
          ),
          br(),
          fluidRow(
            column(
              width = 6,
              align = "left",
              HTML(
                paste(
                  tags$span(
                    style = 'color: white; font-size: 14px; position: relative; top: 10px;',
                    'Tip Point'
                  )
                )
              )
            ),
            column(
              width = 6,
              align = "center",
              div(
                class = "control-color",
                colorPickr(
                  inputId = "nj_tippoint_color",
                  selected = isolate(nj_tippoint_color_val()),
                  label = "",
                  update = "changestop",
                  interaction = list(clear = FALSE, save = FALSE),
                  position = "right-start",
                  width = "100%"
                )
              )
            )
          ),
          br(),
          fluidRow(
            column(
              width = 6,
              align = "left",
              HTML(
                paste(
                  tags$span(
                    style = 'color: white; font-size: 14px; position: relative; top: 10px;',
                    'Node Point'
                  )
                )
              )
            ),
            column(
              width = 6,
              align = "center",
              div(
                class = "control-color",
                colorPickr(
                  inputId = "nj_nodepoint_color",
                  selected = isolate(nj_nodepoint_color_val()),
                  label = "",
                  update = "changestop",
                  interaction = list(clear = FALSE, save = FALSE),
                  position = "right-start",
                  width = "100%"
                )
              )
            )
          ),
          br()
        )
      )
    )

    runjs(unblock_ui)
  })

  ##### Elements Menu ----

  ###### Elements Control Values ----

  # Tip points
  nj_tippoint_show_val <- reactiveVal()
  nj_tippoint_shape_reactive <- reactive({
    ifelse(!is.null(input$nj_tippoint_shape), input$nj_tippoint_shape, "circle")
  }) |>
    debounce(100)
  nj_tippoint_shape_val <- reactiveVal()
  nj_tippoint_alpha_val <- reactiveVal()
  nj_tippoint_size_val <- reactiveVal()

  observe({
    ifelse(
      !is.null(input$nj_tippoint_show),
      nj_tippoint_show_val(input$nj_tippoint_show),
      nj_tippoint_show_val(FALSE)
    )

    nj_tippoint_shape_val(nj_tippoint_shape_reactive())

    ifelse(
      !is.null(input$nj_tippoint_alpha),
      nj_tippoint_alpha_val(input$nj_tippoint_alpha),
      nj_tippoint_alpha_val(0.5)
    )

    ifelse(
      !is.null(input$nj_tippoint_size),
      nj_tippoint_size_val(input$nj_tippoint_size),
      ifelse(
        !is.null(Vis$tippointsize_nj),
        nj_tippoint_size_val(Vis$tippointsize_nj),
        nj_tippoint_size_val(4)
      )
    )
  })

  # Node points
  nj_nodepoint_show_val <- reactiveVal()
  nj_nodepoint_shape_val <- reactiveVal()
  nj_nodepoint_alpha_val <- reactiveVal()
  nj_nodepoint_size_val <- reactiveVal()

  observe({
    ifelse(
      !is.null(input$nj_nodepoint_show),
      nj_nodepoint_show_val(input$nj_nodepoint_show),
      nj_nodepoint_show_val(FALSE)
    )

    ifelse(
      !is.null(input$nj_nodepoint_shape),
      nj_nodepoint_shape_val(input$nj_nodepoint_shape),
      nj_nodepoint_shape_val("circle")
    )

    ifelse(
      !is.null(input$nj_nodepoint_alpha),
      nj_nodepoint_alpha_val(input$nj_nodepoint_alpha),
      nj_nodepoint_alpha_val(1)
    )

    ifelse(
      !is.null(input$nj_nodepoint_size),
      nj_nodepoint_size_val(input$nj_nodepoint_size),
      ifelse(
        !is.null(Vis$nodepointsize_nj),
        nj_nodepoint_size_val(Vis$nodepointsize_nj),
        nj_nodepoint_size_val(2.5)
      )
    )
  })

  # Tiles
  nj_tile_number_val <- reactiveVal()
  nj_fruit_alpha_val <- reactiveVal()
  nj_fruit_alpha_2_val <- reactiveVal()
  nj_fruit_alpha_3_val <- reactiveVal()
  nj_fruit_alpha_4_val <- reactiveVal()
  nj_fruit_alpha_5_val <- reactiveVal()
  nj_fruit_width_circ_reactive <- reactive({
    ifelse(
      !is.null(input$nj_fruit_width_circ),
      input$nj_fruit_width_circ,
      fruit_width()
    )
  }) |>
    debounce(100)
  nj_fruit_width_circ_val <- reactiveVal()
  nj_fruit_width_circ_2_reactive <- reactive({
    ifelse(
      !is.null(input$nj_fruit_width_circ_2),
      input$nj_fruit_width_circ_2,
      fruit_width()
    )
  }) |>
    debounce(100)
  nj_fruit_width_circ_2_val <- reactiveVal()
  nj_fruit_width_circ_3_reactive <- reactive({
    ifelse(
      !is.null(input$nj_fruit_width_circ_3),
      input$nj_fruit_width_circ_3,
      fruit_width()
    )
  }) |>
    debounce(100)
  nj_fruit_width_circ_3_val <- reactiveVal()
  nj_fruit_width_circ_4_reactive <- reactive({
    ifelse(
      !is.null(input$nj_fruit_width_circ_4),
      input$nj_fruit_width_circ_4,
      fruit_width()
    )
  }) |>
    debounce(100)
  nj_fruit_width_circ_4_val <- reactiveVal()
  nj_fruit_width_circ_5_reactive <- reactive({
    ifelse(
      !is.null(input$nj_fruit_width_circ_5),
      input$nj_fruit_width_circ_5,
      fruit_width()
    )
  }) |>
    debounce(100)
  nj_fruit_width_circ_5_val <- reactiveVal()
  nj_fruit_offset_circ_val <- reactiveVal()
  nj_fruit_offset_circ_2_val <- reactiveVal()
  nj_fruit_offset_circ_3_val <- reactiveVal()
  nj_fruit_offset_circ_4_val <- reactiveVal()
  nj_fruit_offset_circ_5_val <- reactiveVal()

  observe({
    ifelse(
      !is.null(input$nj_tile_number),
      nj_tile_number_val(input$nj_tile_number),
      nj_tile_number_val(1)
    )

    ifelse(
      !is.null(input$nj_fruit_alpha),
      nj_fruit_alpha_val(input$nj_fruit_alpha),
      nj_fruit_alpha_val(1)
    )

    ifelse(
      !is.null(input$nj_fruit_alpha_2),
      nj_fruit_alpha_2_val(input$nj_fruit_alpha_2),
      nj_fruit_alpha_2_val(1)
    )

    ifelse(
      !is.null(input$nj_fruit_alpha_3),
      nj_fruit_alpha_3_val(input$nj_fruit_alpha_3),
      nj_fruit_alpha_3_val(1)
    )

    ifelse(
      !is.null(input$nj_fruit_alpha_4),
      nj_fruit_alpha_4_val(input$nj_fruit_alpha_4),
      nj_fruit_alpha_4_val(1)
    )

    ifelse(
      !is.null(input$nj_fruit_alpha_5),
      nj_fruit_alpha_5_val(input$nj_fruit_alpha_5),
      nj_fruit_alpha_5_val(1)
    )

    nj_fruit_width_circ_val(nj_fruit_width_circ_reactive())
    nj_fruit_width_circ_2_val(nj_fruit_width_circ_2_reactive())
    nj_fruit_width_circ_3_val(nj_fruit_width_circ_3_reactive())
    nj_fruit_width_circ_4_val(nj_fruit_width_circ_4_reactive())
    nj_fruit_width_circ_5_val(nj_fruit_width_circ_5_reactive())

    ifelse(
      !is.null(input$nj_fruit_offset_circ),
      nj_fruit_offset_circ_val(input$nj_fruit_offset_circ),
      ifelse(
        nj_layout_val() == "circular" ||
          nj_layout_val() == "inward",
        nj_fruit_offset_circ_val(0.15),
        nj_fruit_offset_circ_val(0.05)
      )
    )

    ifelse(
      !is.null(input$nj_fruit_offset_circ_2),
      nj_fruit_offset_circ_2_val(input$nj_fruit_offset_circ_2),
      ifelse(
        nj_layout_val() == "circular" ||
          nj_layout_val() == "inward",
        nj_fruit_offset_circ_2_val(0.15),
        nj_fruit_offset_circ_2_val(0.05)
      )
    )

    ifelse(
      !is.null(input$nj_fruit_offset_circ_3),
      nj_fruit_offset_circ_3_val(input$nj_fruit_offset_circ_3),
      ifelse(
        nj_layout_val() == "circular" ||
          nj_layout_val() == "inward",
        nj_fruit_offset_circ_3_val(0.15),
        nj_fruit_offset_circ_3_val(0.05)
      )
    )

    ifelse(
      !is.null(input$nj_fruit_offset_circ_4),
      nj_fruit_offset_circ_4_val(input$nj_fruit_offset_circ_4),
      ifelse(
        nj_layout_val() == "circular" ||
          nj_layout_val() == "inward",
        nj_fruit_offset_circ_4_val(0.15),
        nj_fruit_offset_circ_4_val(0.05)
      )
    )

    ifelse(
      !is.null(input$nj_fruit_offset_circ_5),
      nj_fruit_offset_circ_5_val(input$nj_fruit_offset_circ_5),
      ifelse(
        nj_layout_val() == "circular" ||
          nj_layout_val() == "inward",
        nj_fruit_offset_circ_5_val(0.15),
        nj_fruit_offset_circ_5_val(0.05)
      )
    )
  })

  # Heatmap
  nj_heatmap_title_val <- reactiveVal()
  nj_colnames_angle_val <- reactiveVal()
  nj_colnames_y_val <- reactiveVal()
  nj_heatmap_width_val <- reactiveVal()
  nj_heatmap_offset_reactive <- reactive({
    ifelse(!is.null(input$nj_heatmap_offset), input$nj_heatmap_offset, 0)
  }) |>
    debounce(100)
  nj_heatmap_offset_val <- reactiveVal()

  observe({
    ifelse(
      !is.null(input$nj_heatmap_title),
      nj_heatmap_title_val(input$nj_heatmap_title),
      nj_heatmap_title_val("Heatmap")
    )

    ifelse(
      !is.null(input$nj_colnames_angle),
      nj_colnames_angle_val(input$nj_colnames_angle),
      ifelse(
        !is.null(nj_layout_val()),
        ifelse(
          nj_layout_val() == "circular" ||
            nj_layout_val() == "inward",
          nj_colnames_angle_val(90),
          nj_colnames_angle_val(-90)
        ),
        nj_colnames_angle_val(-90)
      )
    )

    ifelse(
      !is.null(input$nj_colnames_y),
      nj_colnames_y_val(input$nj_colnames_y),
      ifelse(
        nj_layout_val() == "circular" ||
          nj_layout_val() == "inward",
        nj_colnames_y_val(0),
        nj_colnames_y_val(-1)
      )
    )

    ifelse(
      !is.null(input$nj_heatmap_width),
      nj_heatmap_width_val(input$nj_heatmap_width),
      nj_heatmap_width_val(heatmap_width())
    )

    nj_heatmap_offset_val(nj_heatmap_offset_reactive())
  })

  # Clade highlights
  nj_nodelabel_show_val <- reactiveVal()
  nj_parentnode_reactive <- reactive({
    if (!is.null(input$nj_parentnode)) {
      input$nj_parentnode
    } else {
      ""
    }
  }) |>
    debounce(100)
  nj_parentnode_val <- reactiveVal()
  nj_clade_scale_val <- reactiveVal()
  nj_clade_type_val <- reactiveVal()

  observe({
    ifelse(
      !is.null(input$nj_nodelabel_show),
      nj_nodelabel_show_val(input$nj_nodelabel_show),
      nj_nodelabel_show_val(FALSE)
    )

    nj_parentnode_val(nj_parentnode_reactive())

    ifelse(
      !is.null(input$nj_clade_scale),
      nj_clade_scale_val(input$nj_clade_scale),
      nj_clade_scale_val(clade_highlight_color())
    )

    ifelse(
      !is.null(input$nj_clade_type),
      nj_clade_type_val(input$nj_clade_type),
      nj_clade_type_val("roundrect")
    )
  })

  ###### Elements Interface ----

  observeEvent(input$nj_elements_menu, {
    runjs(block_ui)

    session$sendCustomMessage('nj_reset_style', "")
    session$sendCustomMessage('nj_highlight', "nj_elements_menu")

    output$tree_controls <- renderUI(
      div(
        class = "control-box",
        box(
          solidHeader = TRUE,
          status = "primary",
          width = "100%",
          title = "Elements Menu",
          fluidRow(
            column(
              width = 12,
              align = "left",
              br(),
              fluidRow(
                div(
                  class = "nj-elements-control-col",
                  column(
                    width = 8,
                    h4(
                      p("Tip Points"),
                      style = "color:white; position: relative; top: 0px; margin-bottom: -10px; right: -15px"
                    )
                  )
                ),
                div(
                  class = "nj-elements-control-col",
                  column(
                    width = 4,
                    align = "center",
                    div(
                      class = "mat-switch-v",
                      materialSwitch(
                        "nj_tippoint_show",
                        "",
                        value = isolate(nj_tippoint_show_val())
                      )
                    )
                  )
                )
              ),
              fluidRow(
                column(
                  width = 3,
                  h5(
                    p("Shape"),
                    style = "color:white; position: relative; top: 19px; margin-bottom: -10px; right: -15px"
                  )
                ),
                column(
                  width = 6,
                  uiOutput("nj_tippoint_shape_ui")
                ),
                column(
                  width = 3,
                  dropMenu(
                    actionBttn(
                      "nj_tippoint_menu",
                      label = "",
                      color = "default",
                      size = "sm",
                      style = "material-flat",
                      icon = icon("sliders")
                    ),
                    placement = "right",
                    theme = "translucent",
                    fluidRow(
                      column(
                        width = 12,
                        align = "center",
                        sliderInput(
                          "nj_tippoint_alpha",
                          label = h5(
                            "Opacity",
                            style = "color:white; margin-bottom: 0px"
                          ),
                          value = isolate(nj_tippoint_alpha_val()),
                          min = 0.1,
                          step = 0.05,
                          max = 1,
                          width = "150px",
                          ticks = FALSE
                        ),
                        br(),
                        sliderInput(
                          inputId = "nj_tippoint_size",
                          label = h5(
                            "Size",
                            style = "color:white; margin-bottom: 0px"
                          ),
                          min = 1,
                          max = 20,
                          step = 0.5,
                          value = isolate(nj_tippoint_size_val()),
                          width = "150px",
                          ticks = FALSE
                        )
                      )
                    )
                  )
                )
              ),
              div(class = "nj-elements-hr", hr()),
              fluidRow(
                div(
                  class = "nj-elements-control-col",
                  column(
                    width = 8,
                    h4(
                      p("Node Points"),
                      style = "color:white; position: relative; top: 0px; margin-bottom: -10px; right: -15px"
                    )
                  )
                ),
                div(
                  class = "nj-elements-control-col",
                  column(
                    width = 4,
                    align = "center",
                    div(
                      class = "mat-switch-v",
                      materialSwitch(
                        "nj_nodepoint_show",
                        "",
                        value = isolate(nj_nodepoint_show_val())
                      )
                    )
                  )
                )
              ),
              fluidRow(
                column(
                  width = 3,
                  h5(
                    p("Shape"),
                    style = "color:white; position: relative; top: 19px; margin-bottom: -10px; right: -15px"
                  )
                ),
                column(
                  width = 6,
                  selectInput(
                    "nj_nodepoint_shape",
                    "",
                    choices = c(
                      Circle = "circle",
                      Square = "square",
                      Diamond = "diamond",
                      Triangle = "triangle",
                      Cross = "cross",
                      Asterisk = "asterisk"
                    ),
                    selected = isolate(nj_nodepoint_shape_val())
                  )
                ),
                column(
                  width = 3,
                  dropMenu(
                    actionBttn(
                      "nj_nodepoint_menu",
                      label = "",
                      color = "default",
                      size = "sm",
                      style = "material-flat",
                      icon = icon("sliders")
                    ),
                    placement = "right",
                    theme = "translucent",
                    fluidRow(
                      column(
                        width = 12,
                        align = "center",
                        sliderInput(
                          "nj_nodepoint_alpha",
                          label = h5(
                            "Opacity",
                            style = "color:white; margin-bottom: 0px"
                          ),
                          value = isolate(nj_nodepoint_alpha_val()),
                          min = 0.1,
                          step = 0.05,
                          max = 1,
                          width = "150px",
                          ticks = FALSE
                        ),
                        br(),
                        sliderInput(
                          inputId = "nj_nodepoint_size",
                          label = h5(
                            "Size",
                            style = "color:white; margin-bottom: 0px"
                          ),
                          min = 1,
                          max = 20,
                          value = isolate(nj_nodepoint_size_val()),
                          step = 0.5,
                          width = "150px",
                          ticks = FALSE
                        )
                      )
                    )
                  )
                )
              ),
              div(class = "nj-elements-hr", hr()),
              fluidRow(
                div(
                  class = "nj-elements-control-tiles",
                  column(
                    width = 4,
                    h4(
                      p("Tiles"),
                      style = "color:white; position: relative; top: 0px; margin-bottom: -10px; right: -15px"
                    )
                  )
                ),
                div(
                  class = "nj-elements-control-col",
                  column(
                    width = 5,
                    align = "left",
                    div(
                      class = "sel-tile-number",
                      selectInput(
                        "nj_tile_number",
                        "",
                        choices = 1:5,
                        width = "70px",
                        selected = isolate(nj_tile_number_val())
                      )
                    )
                  )
                ),
                div(
                  class = "nj-elements-control-col",
                  column(
                    width = 3,
                    dropMenu(
                      actionBttn(
                        "nj_tile_menu",
                        label = "",
                        color = "default",
                        size = "sm",
                        style = "material-flat",
                        icon = icon("sliders")
                      ),
                      placement = "right",
                      theme = "translucent",
                      fluidRow(
                        column(
                          width = 12,
                          align = "center",
                          conditionalPanel(
                            "input.nj_tile_number == 1",
                            sliderInput(
                              "nj_fruit_alpha",
                              label = h5(
                                "Opacity",
                                style = "color:white; margin-bottom: 0px"
                              ),
                              min = 0.1,
                              max = 1,
                              value = isolate(nj_fruit_alpha_val()),
                              step = 0.05,
                              width = "150px",
                              ticks = FALSE
                            )
                          ),
                          conditionalPanel(
                            "input.nj_tile_number == 2",
                            sliderInput(
                              "nj_fruit_alpha_2",
                              label = h5(
                                "Opacity",
                                style = "color:white; margin-bottom: 0px"
                              ),
                              min = 0.1,
                              max = 1,
                              value = isolate(nj_fruit_alpha_2_val()),
                              step = 0.05,
                              width = "150px",
                              ticks = FALSE
                            )
                          ),
                          conditionalPanel(
                            "input.nj_tile_number == 3",
                            sliderInput(
                              "nj_fruit_alpha_3",
                              label = h5(
                                "Opacity",
                                style = "color:white; margin-bottom: 0px"
                              ),
                              min = 0.1,
                              max = 1,
                              value = isolate(nj_fruit_alpha_3_val()),
                              step = 0.05,
                              width = "150px",
                              ticks = FALSE
                            )
                          ),
                          conditionalPanel(
                            "input.nj_tile_number == 4",
                            sliderInput(
                              "nj_fruit_alpha_4",
                              label = h5(
                                "Opacity",
                                style = "color:white; margin-bottom: 0px"
                              ),
                              min = 0.1,
                              max = 1,
                              value = isolate(nj_fruit_alpha_4_val()),
                              step = 0.05,
                              width = "150px",
                              ticks = FALSE
                            )
                          ),
                          conditionalPanel(
                            "input.nj_tile_number == 5",
                            sliderInput(
                              "nj_fruit_alpha_5",
                              label = h5(
                                "Opacity",
                                style = "color:white; margin-bottom: 0px"
                              ),
                              min = 0.1,
                              max = 1,
                              value = isolate(nj_fruit_alpha_5_val()),
                              step = 0.05,
                              width = "150px",
                              ticks = FALSE
                            )
                          )
                        )
                      )
                    )
                  )
                )
              ),
              conditionalPanel(
                "input.nj_tile_number == 1",
                fluidRow(
                  column(
                    width = 5,
                    h5(
                      "Width",
                      style = "color:white; margin-left: 15px; margin-top: 27px;"
                    )
                  ),
                  column(
                    width = 7,
                    align = "center",
                    uiOutput("nj_fruit_width")
                  )
                ),
                fluidRow(
                  column(
                    width = 5,
                    h5(
                      "Position",
                      style = "color:white; margin-left: 15px; margin-top: 32px;"
                    )
                  ),
                  column(
                    width = 7,
                    align = "center",
                    uiOutput("nj_fruit_offset_circ")
                  )
                )
              ),
              conditionalPanel(
                "input.nj_tile_number == 2",
                fluidRow(
                  column(
                    width = 5,
                    h5(
                      "Width",
                      style = "color:white; margin-left: 15px; margin-top: 27px;"
                    )
                  ),
                  column(
                    width = 7,
                    align = "center",
                    uiOutput("nj_fruit_width2")
                  )
                ),
                fluidRow(
                  column(
                    width = 5,
                    h5(
                      "Position",
                      style = "color:white; margin-left: 15px; margin-top: 32px; "
                    )
                  ),
                  column(
                    width = 7,
                    align = "center",
                    uiOutput("nj_fruit_offset_circ_2")
                  )
                )
              ),
              conditionalPanel(
                "input.nj_tile_number == 3",
                fluidRow(
                  column(
                    width = 5,
                    h5(
                      "Width",
                      style = "color:white; margin-left: 15px; margin-top: 27px;"
                    )
                  ),
                  column(
                    width = 7,
                    align = "center",
                    uiOutput("nj_fruit_width3")
                  )
                ),
                fluidRow(
                  column(
                    width = 5,
                    h5(
                      "Position",
                      style = "color:white; margin-left: 15px; margin-top: 32px; "
                    )
                  ),
                  column(
                    width = 7,
                    align = "center",
                    uiOutput("nj_fruit_offset_circ_3")
                  )
                )
              ),
              conditionalPanel(
                "input.nj_tile_num == 4",
                fluidRow(
                  column(
                    width = 5,
                    h5(
                      "Width",
                      style = "color:white; margin-left: 15px; margin-top: 27px;"
                    )
                  ),
                  column(
                    width = 7,
                    align = "center",
                    uiOutput("nj_fruit_width4")
                  )
                ),
                fluidRow(
                  column(
                    width = 5,
                    h5(
                      "Position",
                      style = "color:white; margin-left: 15px; margin-top: 32px;"
                    )
                  ),
                  column(
                    width = 7,
                    align = "center",
                    uiOutput("nj_fruit_offset_circ_4")
                  )
                )
              ),
              conditionalPanel(
                "input.nj_tile_number == 5",
                fluidRow(
                  column(
                    width = 5,
                    h5(
                      "Width",
                      style = "color:white; margin-left: 15px; margin-top: 27px;"
                    )
                  ),
                  column(
                    width = 7,
                    align = "center",
                    uiOutput("nj_fruit_width5")
                  )
                ),
                fluidRow(
                  column(
                    width = 5,
                    h5(
                      "Position",
                      style = "color:white; margin-left: 15px; margin-top: 32px;"
                    )
                  ),
                  column(
                    width = 7,
                    align = "center",
                    uiOutput("nj_fruit_offset_circ_5"),
                  )
                )
              ),
              div(class = "nj-elements-hr", hr()),
              fluidRow(
                div(
                  class = "nj-elements-control-col",
                  column(
                    width = 12,
                    h4(
                      p("Heatmap"),
                      style = "color:grey; position: relative; right: -15px"
                    )
                  )
                )
              ),
              fluidRow(
                column(
                  width = 3,
                  h5(
                    "Title",
                    style = "color:white; margin-left: 15px; margin-top: 15px;"
                  )
                ),
                column(
                  width = 6,
                  align = "left",
                  disabled(
                    textInput(
                      "nj_heatmap_title",
                      label = "",
                      value = isolate(nj_heatmap_title_val()),
                      placeholder = "Heatmap"
                    )
                  )
                ),
                column(
                  width = 3,
                  dropMenu(
                    disabled(
                      actionBttn(
                        "nj_heatmap_menu",
                        label = "",
                        color = "default",
                        size = "sm",
                        style = "material-flat",
                        icon = icon("sliders")
                      )
                    ),
                    placement = "right",
                    theme = "translucent",
                    fluidRow(
                      column(
                        width = 12,
                        align = "center",
                        fluidRow(
                          column(
                            width = 2,
                            align = "left",
                            HTML(
                              paste(
                                tags$span(
                                  style = 'color: white; font-size: 14px; position: relative; top: 7px;',
                                  'Angle'
                                )
                              )
                            )
                          ),
                          column(
                            width = 9,
                            align = "right",
                            div(
                              class = "nj-label-slider",
                              sliderInput(
                                "nj_colnames_angle",
                                label = "",
                                min = -90,
                                max = 90,
                                value = isolate(nj_colnames_angle_val()),
                                width = "150px",
                                ticks = FALSE
                              )
                            )
                          )
                        ),
                        br(),
                        fluidRow(
                          column(
                            width = 2,
                            align = "left",
                            HTML(
                              paste(
                                tags$span(
                                  style = 'color: white; font-size: 14px; position: relative; top: 7px;',
                                  'Position'
                                )
                              )
                            )
                          ),
                          column(
                            width = 9,
                            align = "right",
                            div(
                              class = "nj-label-slider",
                              uiOutput("nj_colnames_y")
                            )
                          )
                        ),
                        br(),
                        fluidRow(
                          column(
                            width = 2,
                            align = "left",
                            HTML(
                              paste(
                                tags$span(
                                  style = 'color: white; font-size: 14px; position: relative; top: 7px;',
                                  'Width'
                                )
                              )
                            )
                          ),
                          column(
                            width = 9,
                            align = "right",
                            div(
                              class = "nj-label-slider",
                              sliderInput(
                                "nj_heatmap_width",
                                label = "",
                                min = 0.05,
                                max = 1.5,
                                value = isolate(nj_heatmap_width_val()),
                                step = 0.05,
                                width = "150px",
                                ticks = FALSE
                              )
                            )
                          )
                        ),
                        br(),
                        fluidRow(
                          column(
                            width = 2,
                            align = "left",
                            HTML(
                              paste(
                                tags$span(
                                  style = 'color: white; font-size: 14px; position: relative; top: 7px;',
                                  'Offset'
                                )
                              )
                            )
                          ),
                          column(
                            width = 9,
                            align = "right",
                            div(
                              class = "nj-label-slider",
                              uiOutput("nj_heatmap_offset")
                            )
                          )
                        )
                      )
                    )
                  )
                )
              ),
              div(class = "nj-elements-hr", hr()),
              fluidRow(
                div(
                  class = "nj-elements-control-col",
                  column(
                    width = 12,
                    h4(
                      p("Clade Highlight"),
                      style = "color:white; position: relative; right: -15px"
                    )
                  )
                )
              ),
              fluidRow(
                column(
                  width = 12,
                  div(
                    class = "control-nodelabel-switch",
                    materialSwitch(
                      "nj_nodelabel_show",
                      h5(
                        p("Toggle Node View"),
                        style = "color:white; padding-left: 5px; position: relative; top: -4px; right: -5px;"
                      ),
                      value = isolate(nj_nodelabel_show_val()),
                      right = TRUE
                    )
                  )
                )
              ),
              fluidRow(
                column(
                  width = 3,
                  h5(
                    p("Nodes"),
                    style = "color:white; position: relative; right: -15px; margin-top: 14px"
                  )
                ),
                column(
                  width = 6,
                  uiOutput("nj_parentnode")
                ),
                column(
                  width = 3,
                  dropMenu(
                    actionBttn(
                      "nj_clade_menu",
                      label = "",
                      color = "default",
                      size = "sm",
                      style = "material-flat",
                      icon = icon("sliders")
                    ),
                    placement = "right-start",
                    theme = "translucent",
                    uiOutput("nj_clade_scale"),
                    fluidRow(
                      column(
                        width = 3,
                        h5(
                          p("Form"),
                          style = "color:white; position: relative; margin-top: 27px"
                        )
                      ),
                      column(
                        width = 9,
                        div(
                          class = "sel-clade",
                          selectInput(
                            "nj_clade_type",
                            "",
                            choices = c("Rect" = "rect", "Round" = "roundrect"),
                            selected = isolate(nj_clade_type_val()),
                            width = "150px"
                          )
                        )
                      )
                    )
                  )
                )
              )
            )
          )
        )
      )
    )

    runjs(unblock_ui)
  })

  ###### Elements Inputs ----

  # Tip points
  output$nj_tippoint_shape_ui <- renderUI({
    output <- render_plot_control(
      input_id = "nj_tippoint_shape",
      input_type = "selectInput",
      choices = c(
        Circle = "circle",
        Square = "square",
        Diamond = "diamond",
        Triangle = "triangle",
        Cross = "cross",
        Asterisk = "asterisk"
      ),
      reactive_value = nj_tippoint_shape_val(),
      default_value = "circle",
      reset = isolate(Vis$nj_tippoint_shape_reset),
      show_condition = isFALSE(nj_tipshape_mapping_show_val())
    )

    isolate(Vis$nj_tippoint_shape_reset <- FALSE)

    output
  })

  # Heatmap settings
  output$nj_heatmap_offset <- renderUI({
    ifelse(
      !is.null(Vis$nj_max_x),
      max <- round(ceiling(Vis$nj_max_x) * 1.5, 0),
      max <- 10
    )

    output <- render_plot_control(
      input_id = "nj_heatmap_offset",
      input_type = "sliderInput",
      width = "150px",
      min = 0,
      max = max,
      step = 1,
      reactive_value = nj_heatmap_offset_val(),
      default_value = 0,
      reset = isolate(Vis$nj_heatmap_offset_val_reset)
    )

    isolate(Vis$nj_heatmap_offset_val_reset <- FALSE)

    output
  })

  # Clade highlighting
  output$nj_parentnode <- renderUI({
    ifelse(
      !is.null(Vis$nj_parentnodes),
      choices <- sort(unique(as.numeric(Vis$nj_parentnodes))),
      choices <- ""
    )

    output <- render_plot_control(
      input_id = "nj_parentnode",
      input_type = "pickerInput",
      multiple = TRUE,
      width = "99%",
      options = list(
        `live-search` = TRUE,
        size = 10,
        style = "background-color: white; border-radius: 5px;"
      ),
      choices = choices,
      reactive_value = nj_parentnode_val(),
      default_value = "",
      reset = isolate(Vis$nj_parentnode_val_reset)
    )

    isolate(Vis$nj_parentnode_val_reset <- FALSE)

    output
  })

  output$nj_clade_scale <- renderUI({
    selected_nodes <- nj_parentnode_val()
    if (
      length(selected_nodes) == 1 &&
        selected_nodes == ""
    ) {
      selected_nodes <- NULL
    }

    if (length(selected_nodes) < 2) {
      label <- "Color"

      ifelse(
        length(selected_nodes == 1),
        selected <- nj_clade_scale_val(),
        selected <- "#D0F221"
      )

      nj_clade_scale <- colorPickr(
        inputId = "nj_clade_scale",
        selected = selected,
        label = "",
        update = "changestop",
        interaction = list(clear = FALSE, save = FALSE),
        position = "right-start",
        width = "150px"
      )
    } else {
      label <- "Scale"
      nj_clade_scale <- div(
        class = "sel-clade-scale",
        selectInput(
          "nj_clade_scale",
          "",
          choices = list(
            Qualitative = qualitative_scales,
            Sequential = sequential_scales
          ),
          selected = nj_clade_scale_val(),
          width = "150px"
        )
      )
    }

    fluidRow(
      column(
        width = 3,
        h5(label, style = "color:white; position: relative; margin-top: 20px")
      ),
      column(
        width = 9,
        align = "center",
        nj_clade_scale
      )
    )
  })

  # Heatmap control
  output$nj_colnames_y <- renderUI({
    req(DB$data)

    disable <- FALSE
    if (
      nj_layout_val() == "inward" ||
        nj_layout_val() == "circular"
    ) {
      min <- 0
      disable <- TRUE
    } else {
      ifelse(
        (sum(DB$data$Include) * -0.1) > -2,
        min <- -2,
        min <- round(sum(DB$data$Include) * -0.1, 0)
      )
    }

    output <- render_plot_control(
      input_id = "nj_colnames_y",
      input_type = "sliderInput",
      width = "150px",
      step = 1,
      min = min,
      max = sum(DB$data$Include),
      reactive_value = nj_colnames_y_val(),
      default_value = -1,
      reset = isolate(Vis$nj_colnames_y_val_reset),
      show_condition = !disable
    )

    isolate(Vis$nj_colnames_y_val_reset <- FALSE)

    output
  })

  # Tiles settings

  output$nj_fruit_width <- renderUI({
    fruit_width <- fruit_width()
    min_val <- max(round(fruit_width / 5, 1), 0.1)
    step_val <- max(round(fruit_width / 10, 1), 0.1)

    ifelse(
      !is.null(Vis$nj_max_x),
      max <- round(ceiling(Vis$nj_max_x) * 0.5, 0),
      max <- 10
    )

    output <- render_plot_control(
      input_id = "nj_fruit_width_circ",
      input_type = "sliderInput",
      width = "150px",
      min = min_val,
      step = step_val,
      max = max,
      reactive_value = isolate(nj_fruit_width_circ_val()),
      default_value = fruit_width,
      reset = isolate(Vis$nj_fruit_width_circ_val_reset)
    )

    isolate(Vis$nj_fruit_width_circ_val_reset <- FALSE)

    output
  })

  output$nj_fruit_width2 <- renderUI({
    ifelse(
      !is.null(Vis$nj_max_x),
      max <- round(ceiling(Vis$nj_max_x) * 0.5, 0),
      max <- 10
    )

    output <- render_plot_control(
      input_id = "nj_fruit_width_circ_2",
      input_type = "selectInput",
      width = "150px",
      min = 1,
      max = max,
      reactive_value = nj_fruit_width_circ_2_val(),
      default_value = fruit_width(),
      reset = isolate(Vis$nj_fruit_width_circ_2_val_reset)
    )

    isolate(Vis$nj_fruit_width_circ_2_val_reset <- FALSE)

    output
  })

  output$nj_fruit_width3 <- renderUI({
    ifelse(
      !is.null(Vis$nj_max_x),
      max <- round(ceiling(Vis$nj_max_x) * 0.5, 0),
      max <- 10
    )

    output <- render_plot_control(
      input_id = "nj_fruit_width_circ_3",
      input_type = "selectInput",
      width = "150px",
      min = 1,
      max = max,
      reactive_value = nj_fruit_width_circ_3_val(),
      default_value = fruit_width(),
      reset = isolate(Vis$nj_fruit_width_circ_3_val_reset)
    )

    isolate(Vis$nj_fruit_width_circ_3_val_reset <- FALSE)

    output
  })

  output$nj_fruit_width4 <- renderUI({
    ifelse(
      !is.null(Vis$nj_max_x),
      max <- round(ceiling(Vis$nj_max_x) * 0.5, 0),
      max <- 10
    )

    output <- render_plot_control(
      input_id = "nj_fruit_width_circ_4",
      input_type = "selectInput",
      width = "150px",
      min = 1,
      max = max,
      reactive_value = nj_fruit_width_circ_4_val(),
      default_value = fruit_width(),
      reset = isolate(Vis$nj_fruit_width_circ_4_val_reset)
    )

    isolate(Vis$nj_fruit_width_circ_4_val_reset <- FALSE)

    output
  })

  output$nj_fruit_width5 <- renderUI({
    ifelse(
      !is.null(Vis$nj_max_x),
      max <- round(ceiling(Vis$nj_max_x) * 0.5, 0),
      max <- 10
    )

    output <- render_plot_control(
      input_id = "nj_fruit_width_circ_5",
      input_type = "selectInput",
      width = "150px",
      min = 1,
      max = max,
      reactive_value = nj_fruit_width_circ_5_val(),
      default_value = fruit_width(),
      reset = isolate(Vis$nj_fruit_width_circ_5_val_reset)
    )

    isolate(Vis$nj_fruit_width_circ_5_val_reset <- FALSE)

    output
  })

  output$nj_fruit_offset_circ <- renderUI({
    if (
      !is.null(nj_layout_val()) &&
        (nj_layout_val() == "circular" || nj_layout_val() == "inward")
    ) {
      step <- 0.03
      min <- -0.6
      max <- 0.6
    } else {
      step <- 0.01
      min <- -0.2
      max <- 0.2
    }

    output <- render_plot_control(
      input_id = "nj_fruit_offset_circ",
      input_type = "sliderInput",
      width = "150px",
      step = step,
      min = min,
      max = max,
      reactive_value = isolate(nj_fruit_offset_circ_val()),
      default_value = 0.05,
      reset = isolate(Vis$nj_fruit_offset_circ_reset)
    )

    isolate(Vis$nj_fruit_offset_circ_reset <- FALSE)

    output
  })

  output$nj_fruit_offset_circ_2 <- renderUI({
    if (
      !is.null(nj_layout_val()) &&
        (nj_layout_val() == "circular" || nj_layout_val() == "inward")
    ) {
      step <- 0.03
      min <- -0.6
      max <- 0.6
    } else {
      step <- 0.01
      min <- -0.2
      max <- 0.2
    }

    output <- render_plot_control(
      input_id = "nj_fruit_offset_circ_2",
      input_type = "sliderInput",
      width = "150px",
      step = step,
      min = min,
      max = max,
      reactive_value = nj_fruit_offset_circ_2_val(),
      default_value = 0.05,
      reset = isolate(Vis$nj_fruit_offset_circ_2_reset)
    )

    isolate(Vis$nj_fruit_offset_circ_2_reset <- FALSE)

    output
  })

  output$nj_fruit_offset_circ_3 <- renderUI({
    if (
      !is.null(nj_layout_val()) &&
        (nj_layout_val() == "circular" || nj_layout_val() == "inward")
    ) {
      step <- 0.03
      min <- -0.6
      max <- 0.6
    } else {
      step <- 0.01
      min <- -0.2
      max <- 0.2
    }

    output <- render_plot_control(
      input_id = "nj_fruit_offset_circ_3",
      input_type = "sliderInput",
      width = "150px",
      step = step,
      min = min,
      max = max,
      reactive_value = nj_fruit_offset_circ_3_val(),
      default_value = 0.05,
      reset = isolate(Vis$nj_fruit_offset_circ_3_reset)
    )

    isolate(Vis$nj_fruit_offset_circ_3_reset <- FALSE)

    output
  })

  output$nj_fruit_offset_circ_4 <- renderUI({
    if (
      !is.null(nj_layout_val()) &&
        (nj_layout_val() == "circular" || nj_layout_val() == "inward")
    ) {
      step <- 0.03
      min <- -0.6
      max <- 0.6
    } else {
      step <- 0.01
      min <- -0.2
      max <- 0.2
    }

    output <- render_plot_control(
      input_id = "nj_fruit_offset_circ_4",
      input_type = "sliderInput",
      width = "150px",
      step = step,
      min = min,
      max = max,
      reactive_value = nj_fruit_offset_circ_4_val(),
      default_value = 0.05,
      reset = isolate(Vis$nj_fruit_offset_circ_4_reset)
    )

    isolate(Vis$nj_fruit_offset_circ_4_reset <- FALSE)

    output
  })

  output$nj_fruit_offset_circ_5 <- renderUI({
    if (
      !is.null(nj_layout_val()) &&
        (nj_layout_val() == "circular" || nj_layout_val() == "inward")
    ) {
      step <- 0.03
      min <- -0.6
      max <- 0.6
    } else {
      step <- 0.01
      min <- -0.2
      max <- 0.2
    }

    output <- render_plot_control(
      input_id = "nj_fruit_offset_circ_5",
      input_type = "sliderInput",
      width = "150px",
      step = step,
      min = min,
      max = max,
      reactive_value = nj_fruit_offset_circ_5_val(),
      default_value = 0.05,
      reset = isolate(Vis$nj_fruit_offset_circ_5_reset)
    )

    isolate(Vis$nj_fruit_offset_circ_5_reset <- FALSE)

    output
  })

  ##### Other Menu ----

  ###### Other Control Values ----

  # Dimensions
  nj_ratio_val <- reactiveVal()
  nj_v_val <- reactiveVal()
  nj_h_val <- reactiveVal()
  nj_scale_val <- reactiveVal()
  nj_zoom_val <- reactiveVal()

  observe({
    ifelse(
      !is.null(input$nj_ratio),
      nj_ratio_val(input$nj_ratio),
      nj_ratio_val(c("16:10" = (16 / 10)))
    )

    ifelse(!is.null(input$nj_v), nj_v_val(input$nj_v), nj_v_val(0))

    ifelse(
      !is.null(input$nj_h),
      nj_h_val(input$nj_h),
      ifelse(
        nj_layout_val() != "circular" && nj_layout_val() != "inward",
        nj_h_val(-0.05),
        nj_h_val(0)
      )
    )

    ifelse(
      !is.null(input$nj_scale),
      nj_scale_val(input$nj_scale),
      nj_scale_val(670)
    )

    ifelse(
      !is.null(input$nj_zoom),
      nj_zoom_val(input$nj_zoom),
      nj_zoom_val(0.95)
    )
  })

  # Root Tree
  nj_root_isolate_val <- reactiveVal()

  observe({
    ifelse(
      !is.null(input$nj_root_isolate),
      nj_root_isolate_val(input$nj_root_isolate),
      nj_root_isolate_val("Automatic")
    )
  })

  # Layout
  nj_layout_val <- reactiveVal()
  nj_rootedge_show_val <- reactiveVal()
  nj_rootedge_length_reactive <- reactive({
    ifelse(
      !is.null(input$nj_rootedge_length),
      input$nj_rootedge_length,
      ifelse(!is.null(Vis$nj_max_x), round(ceiling(Vis$nj_max_x) * 0.05), 2)
    )
  }) |>
    debounce(100)
  nj_rootedge_length_val <- reactiveVal()
  nj_rootedge_line_val <- reactiveVal()
  nj_xlim_reactive <- reactive({
    ifelse(!is.null(input$nj_xlim), input$nj_xlim, -10)
  }) |>
    debounce(100)
  nj_xlim_val <- reactiveVal()
  nj_xlim_inw_reactive <- reactive({
    ifelse(!is.null(input$nj_xlim_inw), input$nj_xlim_inw, 50)
  }) |>
    debounce(100)
  nj_xlim_inw_val <- reactiveVal()
  nj_treescale_show_val <- reactiveVal()
  nj_treescale_width_reactive <- reactive({
    ifelse(
      !is.null(input$nj_treescale_width),
      input$nj_treescale_width,
      ifelse(!is.null(Vis$nj_max_x), round(ceiling(Vis$nj_max_x) * 0.1, 0), 2)
    )
  }) |>
    debounce(100)
  nj_treescale_width_val <- reactiveVal()
  nj_treescale_x_reactive <- reactive({
    ifelse(
      !is.null(input$nj_treescale_x),
      input$nj_treescale_x,
      ifelse(!is.null(Vis$nj_max_x), round(ceiling(Vis$nj_max_x) * 0.2, 0), 2)
    )
  }) |>
    debounce(100)
  nj_treescale_x_val <- reactiveVal()
  nj_treescale_y_reactive <- reactive({
    ifelse(!is.null(input$nj_treescale_y), input$nj_treescale_y, 0)
  }) |>
    debounce(100)
  nj_ladder_val <- reactiveVal()
  nj_treescale_y_val <- reactiveVal()

  observe({
    ifelse(
      !is.null(input$nj_layout),
      nj_layout_val(input$nj_layout),
      nj_layout_val("rectangular")
    )

    ifelse(
      !is.null(input$nj_rootedge_show),
      nj_rootedge_show_val(input$nj_rootedge_show),
      nj_rootedge_show_val(FALSE)
    )

    nj_rootedge_length_val(nj_rootedge_length_reactive())

    ifelse(
      !is.null(input$nj_rootedge_line),
      nj_rootedge_line_val(input$nj_rootedge_line),
      nj_rootedge_line_val("solid")
    )

    nj_xlim_val(nj_xlim_reactive())

    nj_xlim_inw_val(nj_xlim_inw_reactive())

    ifelse(
      !is.null(input$nj_treescale_show),
      nj_treescale_show_val(input$nj_treescale_show),
      nj_treescale_show_val(FALSE)
    )

    nj_treescale_width_val(nj_treescale_width_reactive())

    nj_treescale_x_val(nj_treescale_x_reactive())

    nj_treescale_y_val(nj_treescale_y_reactive())

    ifelse(
      !is.null(input$nj_ladder),
      nj_ladder_val(input$nj_ladder),
      nj_ladder_val(TRUE)
    )
  })

  # Legend
  nj_legend_orientation_val <- reactiveVal()
  nj_legend_size_val <- reactiveVal()
  nj_legend_x_val <- reactiveVal()
  nj_legend_y_val <- reactiveVal()

  observe({
    ifelse(
      !is.null(input$nj_legend_orientation),
      nj_legend_orientation_val(input$nj_legend_orientation),
      nj_legend_orientation_val("vertical")
    )

    ifelse(
      !is.null(input$nj_legend_size),
      nj_legend_size_val(input$nj_legend_size),
      nj_legend_size_val(10)
    )

    ifelse(
      !is.null(input$nj_legend_x),
      nj_legend_x_val(input$nj_legend_x),
      nj_legend_x_val(0.9)
    )

    ifelse(
      !is.null(input$nj_legend_y),
      nj_legend_y_val(input$nj_legend_y),
      nj_legend_y_val(0.2)
    )
  })

  ###### Other Interface ----

  observeEvent(input$nj_misc_menu, {
    runjs(block_ui)

    session$sendCustomMessage('nj_reset_style', "")
    session$sendCustomMessage('nj_highlight', "nj_misc_menu")

    output$tree_controls <- renderUI({
      div(
        class = "control-box",
        box(
          solidHeader = TRUE,
          status = "primary",
          width = "100%",
          title = "Miscellaneous",
          fluidRow(
            column(
              width = 12,
              align = "left",
              br(),
              fluidRow(
                div(
                  class = "nj-label-control-col",
                  column(
                    width = 8,
                    h4(
                      p("Dimensions"),
                      style = "color:white; position: relative; top: 0px; margin-bottom: -10px; right: -15px"
                    )
                  )
                )
              ),
              fluidRow(
                column(
                  width = 4,
                  h5(
                    p("Ratio"),
                    style = "color:white; position: relative; top: 19px; margin-bottom: -10px; right: -15px"
                  )
                ),
                column(
                  width = 4,
                  align = "center",
                  div(
                    class = "nj-control-ratio",
                    selectInput(
                      "nj_ratio",
                      "",
                      choices = c(
                        "16:10" = (16 / 10),
                        "16:9" = (16 / 9),
                        "4:3" = (4 / 3)
                      ),
                      selected = isolate(nj_ratio_val())
                    )
                  )
                ),
                column(
                  width = 3,
                  dropMenu(
                    actionBttn(
                      "nj_dim_menu",
                      label = "",
                      color = "default",
                      size = "sm",
                      style = "material-flat",
                      icon = icon("sliders")
                    ),
                    placement = "right",
                    theme = "translucent",
                    fluidRow(
                      column(
                        width = 12,
                        align = "center",
                        fluidRow(
                          column(
                            width = 2,
                            align = "left",
                            HTML(
                              paste(
                                tags$span(
                                  style = 'color: white; font-size: 14px; position: relative; right: 10px; top: 7px;',
                                  'Vertical'
                                )
                              )
                            )
                          ),
                          column(
                            width = 9,
                            align = "right",
                            div(
                              class = "nj-label-slider",
                              sliderInput(
                                "nj_v",
                                label = "",
                                min = -0.5,
                                max = 0.5,
                                step = 0.01,
                                value = isolate(nj_v_val()),
                                width = "150px",
                                ticks = FALSE
                              )
                            )
                          )
                        ),
                        br(),
                        fluidRow(
                          column(
                            width = 2,
                            align = "left",
                            HTML(
                              paste(
                                tags$span(
                                  style = 'color: white; font-size: 14px; position: relative; right: 10px; top: 7px;',
                                  'Horizontal'
                                )
                              )
                            )
                          ),
                          column(
                            width = 9,
                            align = "right",
                            div(
                              class = "nj-label-slider",
                              sliderInput(
                                "nj_h",
                                label = "",
                                min = -0.5,
                                max = 0.5,
                                step = 0.01,
                                value = isolate(nj_h_val()),
                                width = "150px",
                                ticks = FALSE
                              )
                            )
                          )
                        ),
                        br(),
                        fluidRow(
                          column(
                            width = 2,
                            align = "left",
                            HTML(
                              paste(
                                tags$span(
                                  style = 'color: white; font-size: 14px; position: relative; right: 10px; top: 7px;',
                                  'Horizontal'
                                )
                              )
                            )
                          ),
                          column(
                            width = 9,
                            align = "right",
                            div(
                              class = "nj-label-slider",
                              sliderInput(
                                "nj_scale",
                                "",
                                min = 450,
                                max = 670,
                                step = 5,
                                value = isolate(nj_scale_val()),
                                width = "150px",
                                ticks = FALSE
                              )
                            )
                          )
                        ),
                        br(),
                        fluidRow(
                          column(
                            width = 2,
                            align = "left",
                            HTML(
                              paste(
                                tags$span(
                                  style = 'color: white; font-size: 14px; position: relative; right: 10px; top: 7px;',
                                  'Zoom'
                                )
                              )
                            )
                          ),
                          column(
                            width = 9,
                            align = "right",
                            div(
                              class = "nj-label-slider",
                              sliderInput(
                                "nj_zoom",
                                "",
                                min = 0.5,
                                max = 1.5,
                                step = 0.05,
                                value = isolate(nj_zoom_val()),
                                width = "150px",
                                ticks = FALSE
                              )
                            )
                          )
                        )
                      )
                    )
                  )
                )
              )
            )
          ),
          hr(),
          fluidRow(
            div(
              class = "nj-label-control-col",
              column(
                width = 4,
                h4(
                  p("Root"),
                  style = paste0(
                    "color:white; position: relative; top: 0px;",
                    " margin-bottom: -10px; right: -15px"
                  )
                )
              )
            ),
            div(
              class = "nj-elements-control-col",
              column(
                width = 5,
                div(
                  class = "nj-root-select",
                  selectInput(
                    "nj_root_isolate",
                    "",
                    choices = c(
                      "Automatic",
                      DB$data$`Assembly Name`[which(DB$data$Include == TRUE)]
                    ),
                    selected = isolate(nj_root_isolate_val()),
                    width = "90%"
                  )
                )
              )
            ),
            column(
              width = 1,
              actionButton(
                "table_view",
                "",
                icon = icon("table-list")
              ),
              bsTooltip(
                "table_view",
                HTML("Select from table"),
                placement = "bottom",
                trigger = "hover"
              )
            )
          ),
          hr(),
          fluidRow(
            column(
              width = 12,
              fluidRow(
                div(
                  class = "nj-label-control-col",
                  column(
                    width = 5,
                    h4(
                      p("Layout"),
                      style = "color:white; position: relative; top: 0px; margin-bottom: -10px; right: -15px"
                    )
                  )
                ),
                div(
                  class = "nj-label-control-col",
                  column(
                    width = 7,
                    div(
                      class = "nj-layout-select",
                      selectInput(
                        inputId = "nj_layout",
                        label = "",
                        choices = list(
                          Linear = list(
                            "Rectangular" = "rectangular",
                            "Roundrect" = "roundrect",
                            "Slanted" = "slanted",
                            "Ellipse" = "ellipse"
                          ),
                          Circular = list(
                            "Circular" = "circular",
                            "Inward" = "inward"
                          )
                        ),
                        selected = isolate(nj_layout_val()),
                        width = "90%"
                      )
                    )
                  )
                )
              ),
              fluidRow(
                column(
                  width = 8,
                  div(
                    class = "mat-switch-layout3",
                    materialSwitch(
                      "nj_ladder",
                      h5(
                        p("Ladderize"),
                        style = "color:white; padding-left: 0px; position: relative; top: -4px; right: -5px;"
                      ),
                      value = isolate(nj_ladder_val()),
                      right = TRUE
                    )
                  )
                )
              ),
              fluidRow(
                column(
                  width = 8,
                  div(
                    class = "mat-switch-layout",
                    materialSwitch(
                      "nj_rootedge_show",
                      h5(
                        p("Rootedge"),
                        style = "color:white; padding-left: 0px; position: relative; top: -4px; right: -5px;"
                      ),
                      value = isolate(nj_rootedge_show_val()),
                      right = TRUE
                    )
                  )
                ),
                column(
                  width = 4,
                  align = "right",
                  dropMenu(
                    actionBttn(
                      "nj_rootedge_menu",
                      label = "",
                      color = "default",
                      size = "sm",
                      style = "material-flat",
                      icon = icon("sliders")
                    ),
                    placement = "right",
                    theme = "translucent",
                    fluidRow(
                      column(
                        width = 12,
                        align = "center",
                        fluidRow(
                          column(
                            width = 2,
                            align = "left",
                            HTML(
                              paste(
                                tags$span(
                                  style = 'color: white; font-size: 14px; position: relative; top: 7px;',
                                  'Length'
                                )
                              )
                            )
                          ),
                          column(
                            width = 9,
                            align = "right",
                            uiOutput("nj_rootedge_length")
                          )
                        ),
                        br(),
                        fluidRow(
                          column(
                            width = 3,
                            align = "left",
                            HTML(
                              paste(
                                tags$span(
                                  style = 'color: white; font-size: 14px; position: relative; top: 7px;',
                                  'Line'
                                )
                              )
                            )
                          ),
                          column(
                            width = 9,
                            align = "center",
                            div(
                              class = "nj-rootedge-line",
                              selectInput(
                                "nj_rootedge_line",
                                label = "",
                                choices = c(
                                  Solid = "solid",
                                  Dashed = "dashed",
                                  Dotted = "dotted"
                                ),
                                selected = isolate(nj_rootedge_line_val()),
                                width = "100px"
                              )
                            )
                          )
                        )
                      )
                    )
                  )
                )
              ),
              fluidRow(
                column(
                  width = 8,
                  align = "left",
                  div(
                    class = "mat-switch-layout2",
                    materialSwitch(
                      "nj_treescale_show",
                      h5(
                        p("Tree Scale"),
                        style = "color:white; padding-left: 0px; position: relative; top: -4px; right: -5px;"
                      ),
                      value = isolate(nj_treescale_show_val()),
                      right = TRUE
                    )
                  )
                ),
                column(
                  width = 4,
                  align = "right",
                  dropMenu(
                    actionBttn(
                      "nj_treescale_menu",
                      label = "",
                      color = "default",
                      size = "sm",
                      style = "material-flat",
                      icon = icon("sliders")
                    ),
                    placement = "right",
                    theme = "translucent",
                    fluidRow(
                      column(
                        width = 12,
                        align = "center",
                        fluidRow(
                          column(
                            width = 2,
                            align = "left",
                            HTML(
                              paste(
                                tags$span(
                                  style = 'color: white; font-size: 14px; position: relative; right: 10px; top: 7px;',
                                  'Length'
                                )
                              )
                            )
                          ),
                          column(
                            width = 9,
                            align = "right",
                            uiOutput("nj_treescale_width")
                          )
                        ),
                        br(),
                        fluidRow(
                          column(
                            width = 2,
                            align = "left",
                            HTML(
                              paste(
                                tags$span(
                                  style = 'color: white; font-size: 14px; position: relative; right: 10px; top: 7px;',
                                  'Horizontal'
                                )
                              )
                            )
                          ),
                          column(
                            width = 9,
                            align = "right",
                            uiOutput("nj_treescale_x")
                          )
                        ),
                        br(),
                        fluidRow(
                          column(
                            width = 2,
                            align = "left",
                            HTML(
                              paste(
                                tags$span(
                                  style = 'color: white; font-size: 14px; position: relative; right: 10px; top: 7px;',
                                  'Vertical'
                                )
                              )
                            )
                          ),
                          column(
                            width = 9,
                            align = "right",
                            uiOutput("nj_treescale_y")
                          )
                        )
                      )
                    )
                  )
                )
              ),
              fluidRow(
                fluidRow(
                  column(
                    width = 6,
                    h5(
                      "Circular Space",
                      style = "color:white; margin-left: 29px; margin-top: 30px;"
                    )
                  ),
                  column(
                    width = 5,
                    align = "center",
                    uiOutput("nj_xlim_ui")
                  )
                )
              ),
              hr(),
              fluidRow(
                column(
                  width = 12,
                  align = "left",
                  fluidRow(
                    div(
                      class = "nj-label-control-col",
                      column(
                        width = 8,
                        h4(
                          p("Legend"),
                          style = "color:white; position: relative; top: 0px; margin-bottom: -10px; right: -15px"
                        )
                      )
                    )
                  ),
                  fluidRow(
                    column(
                      width = 7,
                      align = "left",
                      prettyRadioButtons(
                        "nj_legend_orientation",
                        "",
                        choices = c(
                          Horizontal = "horizontal",
                          Vertical = "vertical"
                        ),
                        selected = isolate(nj_legend_orientation_val()),
                        inline = FALSE
                      )
                    ),
                    column(
                      width = 5,
                      align = "right",
                      dropMenu(
                        actionBttn(
                          "nj_legend_menu",
                          label = "",
                          color = "default",
                          size = "sm",
                          style = "material-flat",
                          icon = icon("sliders")
                        ),
                        placement = "right",
                        theme = "translucent",
                        fluidRow(
                          column(
                            width = 12,
                            align = "center",
                            fluidRow(
                              column(
                                width = 2,
                                align = "left",
                                HTML(
                                  paste(
                                    tags$span(
                                      style = 'color: white; font-size: 14px; position: relative; right: 10px; top: 7px;',
                                      'Size'
                                    )
                                  )
                                )
                              ),
                              column(
                                width = 9,
                                align = "right",
                                div(
                                  class = "nj-label-slider",
                                  sliderInput(
                                    "nj_legend_size",
                                    label = "",
                                    min = 5,
                                    max = 25,
                                    step = 1,
                                    value = isolate(nj_legend_size_val()),
                                    width = "150px",
                                    ticks = FALSE
                                  )
                                )
                              )
                            ),
                            br(),
                            fluidRow(
                              column(
                                width = 2,
                                align = "left",
                                HTML(
                                  paste(
                                    tags$span(
                                      style = 'color: white; font-size: 14px; position: relative; right: 10px; top: 7px;',
                                      'Horizontal'
                                    )
                                  )
                                )
                              ),
                              column(
                                width = 9,
                                align = "right",
                                div(
                                  class = "nj-label-slider",
                                  sliderInput(
                                    "nj_legend_x",
                                    label = "",
                                    value = isolate(nj_legend_x_val()),
                                    min = -0.9,
                                    max = 1.9,
                                    step = 0.2,
                                    width = "150px",
                                    ticks = FALSE
                                  )
                                )
                              )
                            ),
                            br(),
                            fluidRow(
                              column(
                                width = 2,
                                align = "left",
                                HTML(
                                  paste(
                                    tags$span(
                                      style = 'color: white; font-size: 14px; position: relative; right: 10px; top: 7px;',
                                      'Vertical'
                                    )
                                  )
                                )
                              ),
                              column(
                                width = 9,
                                align = "right",
                                div(
                                  class = "nj-label-slider",
                                  sliderInput(
                                    "nj_legend_y",
                                    label = "",
                                    value = isolate(nj_legend_y_val()),
                                    min = -1.5,
                                    max = 1.5,
                                    step = 0.1,
                                    width = "150px",
                                    ticks = FALSE
                                  )
                                )
                              )
                            )
                          )
                        )
                      )
                    )
                  )
                )
              )
            )
          )
        )
      )
    })

    runjs(unblock_ui)
  })

  ###### Other Inputs ----

  # Layout settings
  output$nj_xlim_ui <- renderUI({
    if (nj_layout_val() == "inward") {
      input_id <- "nj_xlim_inw"
      min <- 30
      max <- 120
      value <- nj_xlim_inw_val()
      default <- 50
      reset <- Vis$nj_xlim_inw_val_reset
    } else {
      input_id <- "nj_xlim"
      min <- -50
      max <- 0
      value <- nj_xlim_val()
      default <- -10
      reset <- Vis$nj_xlim_val_reset
    }

    output <- render_plot_control(
      input_id = input_id,
      input_type = "sliderInput",
      div_class = "nj-xlim",
      width = "150px",
      min = min,
      max = max,
      reactive_value = value,
      default_value = default,
      reset = reset,
      show_condition = nj_layout_val() == "inward" ||
        nj_layout_val() == "circular"
    )

    isolate(Vis$nj_xlim_inw_val_reset <- Vis$nj_xlim_val_reset <- FALSE)

    output
  })

  # Treescale
  output$nj_treescale_width <- renderUI({
    ifelse(
      !is.null(Vis$nj_max_x),
      max <- round(floor(Vis$nj_max_x) * 0.5, 0),
      max <- 10
    )

    output <- render_plot_control(
      input_id = "nj_treescale_width",
      input_type = "sliderInput",
      div_class = "nj-label-slider",
      width = "150px",
      min = 1,
      max = max,
      reactive_value = nj_treescale_width_val(),
      default_value = ifelse(
        !is.null(Vis$nj_max_x),
        round(ceiling(Vis$nj_max_x) * 0.1, 0),
        2
      ),
      reset = isolate(Vis$nj_treescale_width_val_reset)
    )

    isolate(Vis$nj_treescale_width_val_reset <- FALSE)

    output
  })

  output$nj_treescale_x <- renderUI({
    if ((!is.null(Vis$nj_min_x)) && (!is.null(Vis$nj_max_x))) {
      ifelse(ceiling(Vis$nj_min_x) < 1, min <- 1, min <- ceiling(Vis$nj_min_x))

      max <- round(floor(Vis$nj_max_x))
    } else {
      min <- 1
      max <- 10
    }

    output <- render_plot_control(
      input_id = "nj_treescale_x",
      input_type = "sliderInput",
      div_class = "nj-label-slider",
      width = "150px",
      min = min,
      max = max,
      reactive_value = nj_treescale_x_val(),
      default_value = ifelse(
        !is.null(Vis$nj_max_x),
        round(ceiling(Vis$nj_max_x) * 0.2, 0),
        2
      ),
      reset = isolate(Vis$nj_treescale_x_val_reset)
    )

    isolate(Vis$nj_treescale_x_val_reset <- FALSE)

    output
  })

  output$nj_treescale_y <- renderUI({
    ifelse(
      !is.null(sum(DB$data$Include)),
      max <- sum(DB$data$Include),
      max <- 10
    )

    output <- render_plot_control(
      input_id = "nj_treescale_y",
      input_type = "sliderInput",
      div_class = "nj-label-slider",
      width = "150px",
      min = 0,
      max = max,
      reactive_value = nj_treescale_y_val(),
      default_value = 0,
      reset = isolate(Vis$nj_treescale_y_val_reset)
    )

    isolate(Vis$nj_treescale_y_val_reset <- FALSE)

    output
  })

  # Rootedge
  output$nj_rootedge_length <- renderUI({
    if (!is.null(Vis$nj_max_x)) {
      if (round(ceiling(Vis$nj_max_x) * 0.02, 0) < 1) {
        min <- 1
      } else {
        min <- round(ceiling(Vis$nj_max_x) * 0.02, 0)
      }
      max <- round(ceiling(Vis$nj_max_x) * 0.2, 0)
    } else {
      min <- 1
      max <- 10
    }

    output <- render_plot_control(
      input_id = "nj_rootedge_length",
      input_type = "sliderInput",
      div_class = "nj-label-slider",
      width = "150px",
      min = min,
      max = max,
      reactive_value = nj_rootedge_length_val(),
      default_value = ifelse(
        !is.null(Vis$nj_max_x),
        round(ceiling(Vis$nj_max_x) * 0.05),
        2
      ),
      reset = isolate(Vis$nj_rootedge_length_val_reset)
    )

    isolate(Vis$nj_rootedge_length_val_reset <- FALSE)

    output
  })

  ##### Export Menu ----

  observeEvent(input$nj_download_menu, {
    session$sendCustomMessage('nj_reset_style', "")
    session$sendCustomMessage('nj_highlight', "nj_download_menu")

    output$tree_controls <- renderUI(
      div(
        class = "control-box",
        box(
          solidHeader = TRUE,
          status = "primary",
          width = "100%",
          title = "Export",
          fluidRow(
            column(
              width = 12,
              align = "center",
              br(),
              fluidRow(
                br(),
                column(
                  width = 6,
                  align = "left",
                  HTML(
                    paste(
                      tags$span(
                        style = 'color: white; font-size: 14px; position: relative; left: 15px;',
                        'Save plot as'
                      )
                    )
                  )
                ),
                column(
                  width = 6,
                  align = "center",
                  div(
                    class = "filetype-mst",
                    selectInput(
                      inputId = "filetype_nj",
                      label = "",
                      choices = c("png", "jpeg", "bmp", "svg")
                    )
                  )
                )
              ),
              fluidRow(
                column(6),
                column(
                  width = 6,
                  downloadBttn(
                    "download_nj",
                    style = "simple",
                    label = "",
                    size = "sm",
                    icon = icon("download"),
                    color = "primary"
                  )
                )
              ),
              hr(),
              fluidRow(
                column(
                  width = 6,
                  align = "left",
                  HTML(
                    paste(
                      tags$span(
                        style = 'color: white; font-size: 14px; position: relative; left: 15px; top: 17px',
                        'Print Report'
                      )
                    )
                  )
                ),
                column(
                  width = 6,
                  align = "center",
                  actionButton(
                    "create_rep",
                    "",
                    icon = icon("file-waveform")
                  )
                )
              ),
              br(),
              br()
            )
          )
        )
      )
    )
  })

  ##### Tree Control Events ----

  # Reset all control inputs
  observeEvent(input$nj_reset, {
    showModal(
      div(
        class = "load-modal",
        modalDialog(
          fluidRow(
            br(),
            column(
              width = 11,
              p(
                HTML(
                  paste0(
                    '<span style="color: white; display: block; font-size: 15px; margin-left: 15px;">',
                    'Resetting all plot control inputs will lead to loss of all current settings. Continue?',
                    '</span>'
                  )
                )
              )
            ),
            br()
          ),
          title = "Reset Plot Settings",
          footer = tagList(
            modalButton("Cancel"),
            actionButton("nj_reset_confirm", "Reset", class = "btn btn-default")
          )
        )
      )
    )
  })

  # Reset control values
  observeEvent(input$nj_reset_confirm, {
    runjs(block_ui)

    removeModal()

    # Tip label
    Vis$nj_tiplab_val_reset <- TRUE
    nj_tiplab_val("Assembly Name")
    nj_tiplab_show_val(TRUE)
    Vis$nj_align_reset <- TRUE
    nj_align_val(FALSE)
    ifelse(
      !is.null(Vis$labelsize_nj),
      nj_tiplab_size_val(Vis$labelsize_nj),
      nj_tiplab_size_val(4)
    )
    nj_tiplab_fontface_val("plain")
    nj_tiplab_alpha_val(1)
    nj_tiplab_position_val(0)
    Vis$nj_tiplab_angle_reset <- TRUE
    nj_tiplab_angle_val(0)

    # Label panels
    nj_geom_val(FALSE)
    nj_tiplab_labelradius_val(0.2)
    ifelse(
      !is.null(Vis$tiplab_padding_nj),
      nj_tiplab_padding_val(Vis$tiplab_padding_nj),
      nj_tiplab_padding_val(0.2)
    )

    # Branch labels
    nj_show_branch_label_val(FALSE)
    ifelse(
      !is.null(Vis$branch_size_nj),
      nj_branch_size_val(Vis$branch_size_nj),
      nj_branch_size_val(4)
    )
    Vis$nj_branch_label_val_reset <- TRUE
    nj_branch_label_val("Host")
    nj_branchlab_alpha_val(0.65)
    nj_branch_x_val(0)
    nj_branchlab_fontface_val("plain")
    nj_branch_labelradius_val(0.5)

    # Titles
    nj_title_val(NULL)
    nj_title_size_val(30)
    nj_subtitle_val(NULL)
    nj_subtitle_size_val(30)

    # Custom label
    Vis$nj_label_pos_x <- list()
    Vis$nj_label_pos_y <- list()
    Vis$nj_label_size <- list()
    Vis$custom_label_nj <- data.frame()

    # Tip label mapping
    nj_mapping_show_val(FALSE)
    Vis$nj_color_mapping_val_reset <- TRUE
    nj_color_mapping_val("Country")
    Vis$nj_tiplab_scale_reset <- TRUE
    nj_tiplab_scale_val(nj_tiplab_scale_val_default())
    Vis$nj_color_mapping_div_mid_reset <- TRUE
    nj_color_mapping_div_mid_val("Mean")

    # Tip points mapping
    nj_tipcolor_mapping_show_val(FALSE)
    Vis$nj_tipcolor_mapping_val_reset <- TRUE
    nj_tipcolor_mapping_val("Country")
    Vis$nj_tippoint_scale_val_reset <- TRUE
    nj_tippoint_scale_val(nj_tippoint_scale_val_default())
    Vis$nj_tipcolor_mapping_div_mid_reset <- TRUE
    nj_tipcolor_mapping_div_mid_val("Mean")

    # Tip shape mapping
    nj_tipshape_mapping_show_val(FALSE)
    Vis$nj_tipshape_mapping_val_reset <- TRUE
    nj_tipshape_mapping_val("Host")

    # Tiles mapping
    nj_tiles_show_1_val(FALSE)
    nj_tiles_show_2_val(FALSE)
    nj_tiles_show_3_val(FALSE)
    nj_tiles_show_4_val(FALSE)
    nj_tiles_show_5_val(FALSE)
    Vis$nj_fruit_variable_val_reset <- TRUE
    nj_fruit_variable_val("Isolation Date")
    Vis$nj_fruit_variable_2_val_reset <- TRUE
    nj_fruit_variable_2_val("Isolation Date")
    Vis$nj_fruit_variable_3_val_reset <- TRUE
    nj_fruit_variable_3_val("Isolation Date")
    Vis$nj_fruit_variable_4_val_reset <- TRUE
    nj_fruit_variable_4_val("Isolation Date")
    Vis$nj_fruit_variable_5_val_reset <- TRUE
    nj_fruit_variable_5_val("Isolation Date")
    Vis$nj_tiles_scale_1_reset <- TRUE
    nj_tiles_scale_1_val(nj_tiles_scale_1_val_default())
    Vis$nj_tiles_scale_2_reset <- TRUE
    nj_tiles_scale_2_val(nj_tiles_scale_2_val_default())
    Vis$nj_tiles_scale_3_reset <- TRUE
    nj_tiles_scale_3_val(nj_tiles_scale_3_val_default())
    Vis$nj_tiles_scale_4_reset <- TRUE
    nj_tiles_scale_4_val(nj_tiles_scale_4_val_default())
    Vis$nj_tiles_scale_5_reset <- TRUE
    nj_tiles_scale_5_val(nj_tiles_scale_5_val_default())
    Vis$nj_tiles_mapping_div_mid_1_reset <- TRUE
    nj_tiles_mapping_div_mid_1_val("Mean")
    Vis$nj_tiles_mapping_div_mid_2_reset <- TRUE
    nj_tiles_mapping_div_mid_2_val("Mean")
    Vis$nj_tiles_mapping_div_mid_3_reset <- TRUE
    nj_tiles_mapping_div_mid_3_val("Mean")
    Vis$nj_tiles_mapping_div_mid_4_reset <- TRUE
    nj_tiles_mapping_div_mid_4_val("Mean")
    Vis$nj_tiles_mapping_div_mid_5_reset <- TRUE
    nj_tiles_mapping_div_mid_5_val("Mean")

    nj_heatmap_show_val(FALSE)
    Vis$nj_heatmap_select_val_reset <- TRUE
    nj_heatmap_select_val(NULL)
    Vis$nj_heatmap_scale_reset <- TRUE
    nj_heatmap_scale_val(nj_heatmap_scale_val_default())
    Vis$nj_heatmap_div_mid_val_reset <- TRUE
    nj_heatmap_div_mid_val("Mean")

    # Color values
    nj_color_val("#000000")
    nj_bg_val("#ffffff")
    nj_title_color_val("#000000")
    nj_tiplab_color_val("#000000")
    nj_tiplab_fill_val("#84D9A0")
    nj_branch_label_color_val("#FFB7B7")
    nj_tippoint_color_val("#3A4657")
    nj_nodepoint_color_val("#3A4657")

    # Tip points
    nj_tippoint_show_val(FALSE)
    Vis$nj_tippoint_shape_reset <- TRUE
    nj_tippoint_shape_val("circle")
    nj_tippoint_alpha_val(0.5)
    ifelse(
      !is.null(Vis$tippointsize_nj),
      nj_tippoint_size_val(Vis$tippointsize_nj),
      nj_tippoint_size_val(4)
    )

    # Node points
    nj_nodepoint_show_val(FALSE)
    nj_nodepoint_shape_val("circle")
    nj_nodepoint_alpha_val(1)
    ifelse(
      !is.null(Vis$nodepointsize_nj),
      nj_nodepoint_size_val(Vis$nodepointsize_nj),
      nj_nodepoint_size_val(2.5)
    )

    # Tiles
    nj_tile_number_val(1)
    nj_fruit_alpha_val(1)
    nj_fruit_alpha_2_val(1)
    nj_fruit_alpha_3_val(1)
    nj_fruit_alpha_4_val(1)
    nj_fruit_alpha_5_val(1)
    Vis$nj_fruit_width_circ_val_reset <- TRUE
    nj_fruit_width_circ_val(fruit_width())
    Vis$nj_fruit_width_circ_2_val_reset <- TRUE
    nj_fruit_width_circ_2_val(fruit_width())
    Vis$nj_fruit_width_circ_3_val_reset <- TRUE
    nj_fruit_width_circ_3_val(fruit_width())
    Vis$nj_fruit_width_circ_4_val_reset <- TRUE
    nj_fruit_width_circ_4_val(fruit_width())
    Vis$nj_fruit_width_circ_5_val_reset <- TRUE
    nj_fruit_width_circ_5_val(fruit_width())
    Vis$nj_fruit_offset_circ_reset <- TRUE
    nj_fruit_offset_circ_val(0.05)
    Vis$nj_fruit_offset_circ_2_reset <- TRUE
    nj_fruit_offset_circ_2_val(0.05)
    Vis$nj_fruit_offset_circ_3_reset <- TRUE
    nj_fruit_offset_circ_3_val(0.05)
    Vis$nj_fruit_offset_circ_4_reset <- TRUE
    nj_fruit_offset_circ_4_val(0.05)
    Vis$nj_fruit_offset_circ_5_reset <- TRUE
    nj_fruit_offset_circ_5_val(0.05)

    # Heatmap
    nj_heatmap_title_val("Heatmap")
    nj_colnames_angle_val(-90)
    Vis$nj_colnames_y_val_reset <- TRUE
    nj_colnames_y_val(-1)
    nj_heatmap_width_val(heatmap_width())
    Vis$nj_heatmap_offset_val_reset <- TRUE
    nj_heatmap_offset_val(0)

    # Clade highlights
    nj_nodelabel_show_val(FALSE)
    Vis$nj_parentnode_val_reset <- TRUE
    nj_parentnode_val("")
    nj_clade_scale_val(clade_highlight_color())
    nj_clade_type_val("roundrect")

    # Dimensions
    nj_ratio_val(c("16:10" = (16 / 10)))
    nj_v_val(0)
    nj_h_val(-0.05)
    nj_scale_val(670)
    nj_zoom_val(0.95)

    # Root Tree
    nj_root_isolate_val("Automatic")

    # Layout
    nj_layout_val("rectangular")
    nj_rootedge_show_val(FALSE)
    Vis$nj_rootedge_length_val_reset <- TRUE
    ifelse(
      !is.null(Vis$nj_max_x),
      nj_rootedge_length_val(round(ceiling(Vis$nj_max_x) * 0.05)),
      nj_rootedge_length_val(2)
    )
    nj_rootedge_line_val("solid")
    Vis$nj_xlim_val_reset <- TRUE
    nj_xlim_val(-10)
    Vis$nj_xlim_inw_val_reset <- TRUE
    nj_xlim_inw_val(50)
    nj_treescale_show_val(FALSE)
    Vis$nj_treescale_width_val_reset <- TRUE
    ifelse(
      !is.null(Vis$nj_max_x),
      nj_treescale_width_val(round(ceiling(Vis$nj_max_x) * 0.1, 0)),
      nj_treescale_width_val(2)
    )
    Vis$nj_treescale_x_val_reset <- TRUE
    ifelse(
      !is.null(Vis$nj_max_x),
      nj_treescale_x_val(round(ceiling(Vis$nj_max_x) * 0.2, 0)),
      nj_treescale_x_val(2)
    )
    Vis$nj_treescale_y_val_reset <- TRUE
    nj_treescale_y_val(0)
    nj_ladder_val(TRUE)

    # Legend
    nj_legend_orientation_val("vertical")
    nj_legend_size_val(10)
    nj_legend_x_val(0.9)
    nj_legend_y_val(0.2)

    output$tree_controls <- NULL
    runjs("document.querySelector('#nj_label_menu').click();")
    runjs(unblock_ui)
  })

  observeEvent(input$nj_heatmap_button, {
    showModal(
      div(
        class = "start-modal",
        modalDialog(
          conditionalPanel(
            "input.nj_heatmap_map==='Variables'",
            fluidRow(
              br(),
              column(
                width = 11,
                p(
                  HTML(
                    paste0(
                      '<span style="color: white; display: block; font-size: 15px; margin-left: 15px;">',
                      'Mapping metadata and custom variables.',
                      '</span>'
                    )
                  )
                )
              ),
              br()
            )
          ),
          conditionalPanel(
            "input.nj_heatmap_map==='AMR Profile'",
            fluidRow(
              br(),
              column(
                width = 11,
                p(
                  HTML(
                    if (isTRUE(Screening$available)) {
                      paste0(
                        '<span style="color: white; display: block; font-size: 15px; margin-left: 15px;">',
                        'Mapping <b>resistance, virulence and other genes </b> resulting from AMR Screening.',
                        '</span>'
                      )
                    } else {
                      paste0(
                        '<span style="color: white; display: block; font-size: 15px; margin-left: 15px;">',
                        'Antimicrobial resistance screening not available for ',
                        gsub("CM|PM", "", DB$scheme),
                        ".",
                        '</span>'
                      )
                    }
                  )
                )
              ),
              br()
            )
          ),
          br(),
          fluidRow(
            column(
              width = 11,
              fluidRow(
                column(
                  width = 6,
                  uiOutput("nj_heatmap_sel")
                ),
                column(
                  width = 6,
                  conditionalPanel(
                    "input.nj_heatmap_map==='AMR Profile'",
                    if (isTRUE(Screening$available)) {
                      div(
                        class = "switch-heatmap_cluster",
                        materialSwitch(
                          "nj_heatmap_cluster",
                          h5(
                            p("Cluster Genes"),
                            style = "color:white; padding-left: 0px; position: relative; top: -3px; right: -20px;"
                          ),
                          value = TRUE,
                          right = TRUE
                        )
                      )
                    }
                  )
                )
              ),
              fluidRow(
                column(
                  width = 12,
                  br(),
                  uiOutput("nj_heatmap_var_warning")
                )
              )
            )
          ),
          title = fluidRow(
            column(
              width = 5,
              p(
                HTML(
                  paste0(
                    '<span style="color: white; font-size: 20px; position: relative; top: 5px; margin-left: 10px;">',
                    'Heatmap',
                    '</span>'
                  )
                )
              )
            ),
            column(
              width = 5,
              radioGroupButtons(
                inputId = "nj_heatmap_map",
                label = "",
                choices = c("Variables", "AMR Profile"),
                justified = TRUE
              )
            )
          ),
          footer = tagList(
            modalButton("Cancel"),
            actionButton(
              "nj_heatmap_confirm",
              "Apply",
              class = "btn btn-default"
            )
          )
        )
      )
    )
  })

  # Size scaling NJ
  observe({
    if (equals(nj_ratio_val(), "1.6")) {
      updateSliderInput(
        session,
        "nj_scale",
        step = 5,
        value = 670,
        min = 450,
        max = 670
      )
    } else if (equals(nj_ratio_val(), "1.77777777777778")) {
      updateSliderInput(
        session,
        "nj_scale",
        step = 9,
        value = 657,
        min = 450,
        max = 666
      )
    } else if (equals(nj_ratio_val(), "1.33333333333333")) {
      updateSliderInput(
        session,
        "nj_scale",
        step = 3,
        value = 654,
        min = 450,
        max = 669
      )
    }
  })

  ### Custom Labels
  # Add custom label
  observeEvent(input$nj_add_new_label, {
    if (nchar(input$nj_new_label_name) > 0) {
      if (!(input$nj_new_label_name %in% Vis$custom_label_nj)) {
        Vis$custom_label_nj <- rbind(
          Vis$custom_label_nj,
          input$nj_new_label_name
        )
        if (nrow(Vis$custom_label_nj) != 1) {
          updateSelectInput(
            session,
            "nj_custom_label_sel",
            selected = input$nj_new_label_name
          )
        }
      } else {
        show_toast(
          title = "Label already exists",
          type = "error",
          position = "bottom-end",
          timer = 6000
        )
      }
    } else {
      show_toast(
        title = "Min. 1 character",
        type = "error",
        position = "bottom-end",
        timer = 6000
      )
    }
  })

  # Delete custom label
  observeEvent(input$nj_del_label, {
    req(Vis$custom_label_nj)
    if (nrow(Vis$custom_label_nj) > 1) {
      Vis$custom_label_nj <- Vis$custom_label_nj[
        -which(
          Vis$custom_label_nj[, 1] == input$nj_custom_label_sel
        ),
        ,
        drop = FALSE
      ]
    } else if (nrow(Vis$custom_label_nj) == 1) {
      Vis$nj_label_pos_x <- list()
      Vis$nj_label_pos_y <- list()
      Vis$nj_label_size <- list()
      Vis$custom_label_nj <- data.frame()
    }
  })

  # Apply custom label changes
  observeEvent(input$nj_cust_label_save, {
    req(
      Vis$nj_label_pos_y,
      Vis$nj_label_pos_x,
      Vis$nj_label_size,
      input$nj_custom_label_sel
    )

    Vis$nj_label_pos_y[[input$nj_custom_label_sel]] <- input[[paste0(
      "nj_slider_",
      input$nj_custom_label_sel,
      "_y"
    )]]
    Vis$nj_label_pos_x[[input$nj_custom_label_sel]] <- input[[paste0(
      "nj_slider_",
      input$nj_custom_label_sel,
      "_x"
    )]]
    Vis$nj_label_size[[input$nj_custom_label_sel]] <- input[[paste0(
      "nj_slider_",
      input$nj_custom_label_sel,
      "_size"
    )]]
  })

  # Update value if new variables added
  observeEvent(input$nj_heatmap_select, {
    req(nj_layout_val())

    length_input <- length(input$nj_heatmap_select)
    if (
      nj_layout_val() != "circular" &&
        nj_layout_val() != "inward"
    ) {
      if (length_input < 3) {
        width <- 0.1
      } else {
        if (length_input >= 3 && length_input <= 50) {
          width <- min(0.15 + 0.05 * floor((length_input - 3) / 2), 1.5)
        } else {
          width <- 1.5
        }
      }
    } else {
      if (length_input < 3) {
        width <- 0.3
      } else if (length_input >= 3 && length_input <= 27) {
        width <- min(0.6 + 0.2 * floor((length_input - 3) / 2), 1.5)
      } else {
        width <- 3
      }
    }
    updateSliderInput(session, "nj_heatmap_width", value = width)
  })

  # Changes when tree layout changes
  observeEvent(input$nj_layout, {
    if (!is.null(Vis$nj_max_x)) {
      fruit_width <- fruit_width(nj_layout = input$nj_layout)

      nj_fruit_width_circ_val(fruit_width)
      nj_fruit_width_circ_2_val(fruit_width)
      nj_fruit_width_circ_3_val(fruit_width)
      nj_fruit_width_circ_4_val(fruit_width)
      nj_fruit_width_circ_5_val(fruit_width)

      updateSliderInput(session, "nj_fruit_width_circ", value = fruit_width)
      updateSliderInput(session, "nj_fruit_width_circ_2", value = fruit_width)
      updateSliderInput(session, "nj_fruit_width_circ_3", value = fruit_width)
      updateSliderInput(session, "nj_fruit_width_circ_4", value = fruit_width)
      updateSliderInput(session, "nj_fruit_width_circ_5", value = fruit_width)
    }

    # if(input$nj_layout == "circular" || input$nj_layout == "inward") {
    #   offset <- 0.05
    #   step <- 0.01
    #   min <- -0.2
    #   max <- 0.2
    # } else {
    #   offset <- 0.15
    #   step <- 0.03
    #   min <- -0.6
    #   max <- 0.6
    # }
    #
    # updateSliderInput(session, "nj_fruit_offset_circ", min = min, step = step, max = max)
    # updateSliderInput(session, "nj_fruit_offset_circ_2", min = min, step = step, max = max, value = offset)
    # updateSliderInput(session, "nj_fruit_offset_circ_3", min = min, step = step, max = max, value = offset)
    # updateSliderInput(session, "nj_fruit_offset_circ_4", min = min, step = step, max = max, value = offset)
    # updateSliderInput(session, "nj_fruit_offset_circ_5", min = min, step = step, max = max, value = offset)

    #

    if (input$nj_layout != "inward" && input$nj_layout != "circular") {
      # nj_colnames_angle_val(-90)
      # updateSliderInput(session, "nj_colnames_angle", value = -90)
      # nj_colnames_y_val(-1)

      if (!is.null(Vis$tree_algo) && Vis$tree_algo == "NJ") {
        nj_align_val(FALSE)
      }

      nj_h_val(-0.05)
      updateSliderInput(session, "nj_h", value = -0.05)

      nj_v_val(0)
      updateSliderInput(session, "nj_v", value = 0)

      nj_zoom_val(0.95)
      updateSliderInput(session, "nj_zoom", value = 0.95)
    } else {
      # nj_colnames_angle_val(90)
      # updateSliderInput(session, "nj_colnames_angle", value = 90)
      # nj_colnames_y_val(0)

      nj_align_val(TRUE)

      nj_h_val(0)
      updateSliderInput(session, "nj_h", value = 0)

      nj_v_val(0.04)
      updateSliderInput(session, "nj_v", value = 0.04)
    }

    # Adapt heatmap control inputs

    # length_input <- length(nj_heatmap_select_val())
    #
    # if(input$nj_layout != "circular" & input$nj_layout != "inward") {
    #   if(length_input < 3) {
    #     width <- 0.1
    #   } else {
    #     if (length_input >= 3 && length_input <= 50) {
    #       width <- min(0.15 + 0.05 * floor((length_input - 3) / 2), 1.5)
    #     } else {
    #       width <- 1.5
    #     }
    #   }
    # } else {
    #   if(length_input < 3) {
    #     width <- 0.3
    #   } else if (length_input >= 3 && length_input <= 27) {
    #     width <- min(0.6 + 0.2 * floor((length_input - 3) / 2), 1.5)
    #   } else {
    #     width <- 3
    #   }
    # }
    #
    # nj_heatmap_width_val(width)
  })

  # Tile number selector update each other
  observeEvent(input$nj_tile_num, {
    updateSelectInput(session, "nj_tile_number", selected = input$nj_tile_num)
  })

  observeEvent(input$nj_tile_number, {
    updateSelectInput(session, "nj_tile_num", selected = input$nj_tile_number)
  })

  #### MST controls ----

  session$sendCustomMessage('mst_reset_style', "")
  session$sendCustomMessage('mst_highlight', "mst_label_menu")

  output$mst_controls <- renderUI(
    box(
      solidHeader = TRUE,
      status = "primary",
      width = "100%",
      title = "Labels",
      fluidRow(
        column(
          width = 12,
          align = "left",
          h4(
            p("Isolate Label"),
            style = "color:white; position: relative; right: -15px; top: 15px;"
          ),
          column(
            width = 12,
            align = "center",
            div(
              class = "mst-label-sel",
              uiOutput("mst_node_label")
            )
          )
        )
      ),
      br(),
      fluidRow(
        column(
          width = 12,
          align = "left",
          h4(
            p("Title"),
            style = "color:white; position: relative; right: -15px; top: 15px;"
          ),
          column(
            width = 12,
            align = "center",
            textInput(
              "mst_title",
              label = "",
              width = "100%",
              placeholder = "Plot Title"
            )
          )
        )
      ),
      br(),
      fluidRow(
        column(
          width = 12,
          align = "left",
          h4(
            p("Subtitle"),
            style = "color:white; position: relative; right: -15px; top: 15px;"
          ),
          column(
            width = 12,
            align = "center",
            textInput(
              "mst_subtitle",
              label = "",
              width = "100%",
              placeholder = "Plot Subtitle"
            )
          )
        )
      ),
      br(),
      br()
    )
  )

  ##### Label Menu ----

  mst_node_label_reactive <- reactiveVal()
  mst_title_reactive <- reactiveVal()
  mst_subtitle_reactive <- reactiveVal()

  observe({
    ifelse(
      isTRUE(mst_color_var_reactive()),
      mst_node_label_reactive("Assembly Name"),
      ifelse(
        !is.null(input$mst_node_label),
        mst_node_label_reactive(input$mst_node_label),
        mst_node_label_reactive("Assembly Name")
      )
    )

    ifelse(
      !is.null(input$mst_title),
      mst_title_reactive(input$mst_title),
      mst_title_reactive("")
    )

    ifelse(
      !is.null(input$mst_subtitle),
      mst_subtitle_reactive(input$mst_subtitle),
      mst_subtitle_reactive("")
    )
  })

  observeEvent(input$mst_label_menu, {
    runjs(block_ui)

    session$sendCustomMessage('mst_reset_style', "")
    session$sendCustomMessage('mst_highlight', "mst_label_menu")

    output$mst_controls <- renderUI(
      box(
        solidHeader = TRUE,
        status = "primary",
        width = "100%",
        title = "Labels",
        fluidRow(
          column(
            width = 12,
            align = "left",
            h4(
              p("Isolate Label"),
              style = "color:white; position: relative; right: -15px; top: 15px;"
            ),
            column(
              width = 12,
              align = "center",
              div(
                class = "mst-label-sel",
                uiOutput("mst_node_label")
              )
            )
          )
        ),
        br(),
        fluidRow(
          column(
            width = 12,
            align = "left",
            h4(
              p("Title"),
              style = "color:white; position: relative; right: -15px; top: 15px;"
            ),
            column(
              width = 12,
              align = "center",
              textInput(
                "mst_title",
                value = isolate(mst_title_reactive()),
                label = "",
                width = "100%",
                placeholder = "Plot Title"
              )
            )
          )
        ),
        br(),
        fluidRow(
          column(
            width = 12,
            align = "left",
            h4(
              p("Subtitle"),
              style = "color:white; position: relative; right: -15px; top: 15px;"
            ),
            column(
              width = 12,
              align = "center",
              textInput(
                "mst_subtitle",
                value = isolate(mst_subtitle_reactive()),
                label = "",
                width = "100%",
                placeholder = "Plot Subtitle"
              )
            )
          )
        ),
        br(),
        br()
      )
    )

    runjs(unblock_ui)
  })

  ##### Variable Mapping Menu ----

  mst_color_var_reactive <- reactiveVal()
  mst_col_var_reactive <- reactiveVal()
  mst_col_scale_reactive <- reactiveVal()

  observe({
    ifelse(
      !is.null(input$mst_color_var),
      mst_color_var_reactive(input$mst_color_var),
      mst_color_var_reactive(FALSE)
    )

    ifelse(
      !is.null(input$mst_col_var),
      mst_col_var_reactive(input$mst_col_var),
      mst_col_var_reactive("Isolation Date")
    )

    ifelse(
      !is.null(input$mst_col_scale),
      mst_col_scale_reactive(input$mst_col_scale),
      mst_col_scale_reactive("Viridis")
    )
  })

  observeEvent(input$mst_variable_menu, {
    req(mst_col_var_reactive())
    runjs(block_ui)

    session$sendCustomMessage('mst_reset_style', "")
    session$sendCustomMessage('mst_highlight', "mst_variable_menu")

    output$mst_controls <- renderUI(
      div(
        class = "control-box",
        box(
          solidHeader = TRUE,
          status = "primary",
          width = "100%",
          title = "Variable Mapping",
          fluidRow(
            column(
              width = 12,
              align = "left",
              br(),
              fluidRow(
                column(
                  width = 8,
                  align = "left",
                  h4(
                    p("Node Color"),
                    style = "color:white; position: relative; right: -15px; "
                  ),
                  bslib::tooltip(
                    bsicons::bs_icon(
                      "info-circle",
                      title = "Only categorical variables can \nbe mapped to the node color",
                      color = "white",
                      height = "14px",
                      width = "14px",
                      position = "relative",
                      right = "-120px"
                    ),
                    "Text shown in the tooltip.",
                    show = FALSE,
                    id = "mst_node_col_info"
                  )
                ),
                column(
                  width = 4,
                  align = "center",
                  div(
                    class = "mat-switch-v",
                    materialSwitch(
                      "mst_color_var",
                      "",
                      value = isolate(mst_color_var_reactive())
                    )
                  )
                )
              ),
              fluidRow(
                column(
                  width = 7,
                  div(
                    class = "mst-color-mapping",
                    selectInput(
                      "mst_col_var",
                      "",
                      choices = colnames(Vis$meta_mst)[
                        -c(1, 2, 3, 4, 6, 12, 13, 14)
                      ],
                      selected = isolate(mst_col_var_reactive()),
                      width = "100%"
                    )
                  )
                ),
                column(
                  width = 5,
                  div(
                    class = "mst-color-scale",
                    selectInput(
                      "mst_col_scale",
                      "",
                      choices = c("Viridis", "Rainbow"),
                      selected = isolate(mst_col_scale_reactive()),
                      width = "100%"
                    )
                  )
                )
              ),
              br()
            )
          )
        )
      )
    )

    runjs(unblock_ui)
  })

  ##### Color Menu ----

  mst_text_color_reactive <- reactiveVal()
  mst_color_node_reactive <- reactiveVal()
  mst_color_edge_reactive <- reactiveVal()
  mst_edge_font_color_reactive <- reactiveVal()
  mst_background_color_reactive <- reactiveVal()
  mst_background_transparent_reactive <- reactiveVal()

  observe({
    ifelse(
      !is.null(input$mst_text_color),
      mst_text_color_reactive(input$mst_text_color),
      mst_text_color_reactive("#000000")
    )

    ifelse(
      !is.null(input$mst_color_node),
      mst_color_node_reactive(input$mst_color_node),
      mst_color_node_reactive("#B2FACA")
    )

    ifelse(
      !is.null(input$mst_color_edge),
      mst_color_edge_reactive(input$mst_color_edge),
      mst_color_edge_reactive("#000000")
    )

    ifelse(
      !is.null(input$mst_edge_font_color),
      mst_edge_font_color_reactive(input$mst_edge_font_color),
      mst_edge_font_color_reactive("#000000")
    )

    ifelse(
      !is.null(input$mst_background_color),
      mst_background_color_reactive(input$mst_background_color),
      mst_background_color_reactive("#ffffff")
    )

    ifelse(
      !is.null(input$mst_background_transparent),
      mst_background_transparent_reactive(
        input$mst_background_transparent
      ),
      mst_background_transparent_reactive(FALSE)
    )
  })

  observeEvent(input$mst_color_menu, {
    runjs(block_ui)

    session$sendCustomMessage('mst_reset_style', "")
    session$sendCustomMessage('mst_highlight', "mst_color_menu")

    output$mst_controls <- renderUI(
      box(
        solidHeader = TRUE,
        status = "primary",
        width = "100%",
        title = "Color Menu",
        column(
          width = 12,
          br(),
          fluidRow(
            column(
              width = 6,
              align = "left",
              HTML(
                paste(
                  tags$span(
                    style = 'color: white; font-size: 14px; position: relative; top: 10px;',
                    'Text'
                  )
                )
              )
            ),
            column(
              width = 6,
              align = "center",
              colorPickr(
                inputId = "mst_text_color",
                selected = isolate(mst_text_color_reactive()),
                label = "",
                update = "changestop",
                interaction = list(clear = FALSE, save = FALSE),
                position = "right-start",
                width = "100%"
              )
            )
          ),
          br(),
          fluidRow(
            column(
              width = 6,
              align = "left",
              HTML(
                paste(
                  tags$span(
                    style = 'color: white; font-size: 14px; position: relative; top: 10px;',
                    'Nodes'
                  )
                )
              )
            ),
            column(
              width = 6,
              align = "center",
              uiOutput("mst_color_mapping")
            )
          ),
          br(),
          fluidRow(
            column(
              width = 6,
              align = "left",
              HTML(
                paste(
                  tags$span(
                    style = 'color: white; font-size: 14px; position: relative; top: 10px;',
                    'Edges'
                  )
                )
              )
            ),
            column(
              width = 6,
              align = "center",
              colorPickr(
                inputId = "mst_color_edge",
                width = "100%",
                selected = isolate(mst_color_edge_reactive()),
                label = "",
                update = "changestop",
                interaction = list(clear = FALSE, save = FALSE),
                position = "right-start"
              )
            )
          ),
          br(),
          fluidRow(
            column(
              width = 6,
              align = "left",
              HTML(
                paste(
                  tags$span(
                    style = 'color: white; font-size: 14px; position: relative; top: 10px;',
                    'Edge Font'
                  )
                )
              )
            ),
            column(
              width = 6,
              align = "center",
              colorPickr(
                inputId = "mst_edge_font_color",
                width = "100%",
                selected = isolate(mst_edge_font_color_reactive()),
                label = "",
                update = "changestop",
                interaction = list(clear = FALSE, save = FALSE),
                position = "right-start"
              )
            )
          ),
          br(),
          fluidRow(
            column(
              width = 6,
              align = "left",
              HTML(
                paste(
                  tags$span(
                    style = 'color: white; font-size: 14px; position: relative; top: 10px;',
                    'Background'
                  )
                )
              )
            ),
            column(
              width = 6,
              align = "center",
              colorPickr(
                inputId = "mst_background_color",
                width = "100%",
                selected = isolate(mst_background_color_reactive()),
                label = "",
                update = "changestop",
                interaction = list(clear = FALSE, save = FALSE),
                position = "right-start"
              )
            )
          ),
          fluidRow(
            column(1),
            column(
              width = 11,
              div(
                class = "switch-mst-transparent",
                materialSwitch(
                  "mst_background_transparent",
                  h5(
                    p("Transparent"),
                    style = "color:white; padding-left: 0px; position: relative; top: -4px; right: -5px;"
                  ),
                  value = isolate(mst_background_transparent_reactive()),
                  right = TRUE
                )
              )
            )
          ),
          br()
        )
      )
    )

    runjs(unblock_ui)
  })

  ##### Sizing Menu ----

  mst_scale_nodes_reactive <- reactiveVal()
  mst_node_scale_reactive <- reactiveVal()
  mst_node_size_reactive <- reactiveVal()
  mst_scale_edges_reactive <- reactiveVal()
  mst_edge_length_scale_reactive <- reactiveVal()
  mst_edge_length_reactive <- reactiveVal()
  mst_edge_font_size_reactive <- reactiveVal()
  mst_title_size_reactive <- reactiveVal()
  mst_subtitle_size_reactive <- reactiveVal()
  mst_node_label_fontsize_reactive <- reactiveVal()

  observe({
    ifelse(
      !is.null(input$mst_scale_nodes),
      mst_scale_nodes_reactive(input$mst_scale_nodes),
      mst_scale_nodes_reactive(TRUE)
    )

    ifelse(
      !is.null(input$mst_node_scale),
      mst_node_scale_reactive(input$mst_node_scale),
      mst_node_scale_reactive(c(20, 40))
    )

    ifelse(
      !is.null(input$mst_node_size),
      mst_node_size_reactive(input$mst_node_size),
      mst_node_size_reactive(30)
    )

    ifelse(
      !is.null(input$mst_scale_edges),
      mst_scale_edges_reactive(input$mst_scale_edges),
      mst_scale_edges_reactive(TRUE)
    )

    ifelse(
      !is.null(input$mst_edge_length_scale),
      mst_edge_length_scale_reactive(input$mst_edge_length_scale),
      mst_edge_length_scale_reactive(15)
    )

    ifelse(
      !is.null(input$mst_edge_length),
      mst_edge_length_reactive(input$mst_edge_length),
      mst_edge_length_reactive(35)
    )

    ifelse(
      !is.null(input$mst_edge_font_size),
      mst_edge_font_size_reactive(input$mst_edge_font_size),
      mst_edge_font_size_reactive(18)
    )

    ifelse(
      !is.null(input$mst_node_label_fontsize),
      mst_node_label_fontsize_reactive(input$mst_node_label_fontsize),
      mst_node_label_fontsize_reactive(14)
    )

    ifelse(
      !is.null(input$mst_title_size),
      mst_title_size_reactive(input$mst_title_size),
      mst_title_size_reactive(35)
    )

    ifelse(
      !is.null(input$mst_subtitle_size),
      mst_subtitle_size_reactive(input$mst_subtitle_size),
      mst_subtitle_size_reactive(20)
    )
  })

  observeEvent(input$mst_size_menu, {
    runjs(block_ui)

    session$sendCustomMessage('mst_reset_style', "")
    session$sendCustomMessage('mst_highlight', "mst_size_menu")

    output$mst_controls <- renderUI(
      div(
        class = "control-box",
        box(
          solidHeader = TRUE,
          status = "primary",
          width = "100%",
          title = "Size Settings",
          fluidRow(
            column(
              width = 12,
              align = "left",
              br(),
              fluidRow(
                div(
                  class = "nj-label-control-col",
                  column(
                    width = 8,
                    h4(
                      p("Nodes"),
                      style = "color:white; position: relative; top: 0px; margin-bottom: -10px; right: -15px"
                    )
                  )
                )
              ),
              fluidRow(
                column(
                  width = 12,
                  align = "left",
                  div(
                    class = "switch-mst-edges",
                    materialSwitch(
                      "mst_scale_nodes",
                      h5(
                        p("Scale by Duplicates"),
                        style = "color:white; padding-left: 0px; position: relative; top: -4px; right: -5px;"
                      ),
                      value = isolate(mst_scale_nodes_reactive()),
                      right = TRUE
                    )
                  )
                )
              ),
              fluidRow(
                column(
                  width = 12,
                  align = "left",
                  fluidRow(
                    column(
                      width = 4,
                      conditionalPanel(
                        "input.mst_scale_nodes==true",
                        HTML(
                          paste(
                            tags$span(
                              style = 'color: white; font-size: 14px; position: relative; bottom: -17px; margin-left: 15px ',
                              'Range'
                            )
                          )
                        )
                      ),
                      conditionalPanel(
                        "input.mst_scale_nodes==false",
                        HTML(
                          paste(
                            tags$span(
                              style = 'color: white; font-size: 14px; position: relative; bottom: -17px; margin-left: 15px ',
                              'Size'
                            )
                          )
                        )
                      )
                    ),
                    column(
                      width = 8,
                      align = "center",
                      conditionalPanel(
                        "input.mst_scale_nodes==true",
                        div(
                          class = "mst_scale_slider",
                          sliderInput(
                            "mst_node_scale",
                            label = "",
                            min = 1,
                            max = 80,
                            value = isolate(mst_node_scale_reactive()),
                            ticks = FALSE
                          )
                        )
                      ),
                      conditionalPanel(
                        "input.mst_scale_nodes==false",
                        div(
                          class = "mst_scale_slider",
                          sliderInput(
                            inputId = "mst_node_size",
                            label = "",
                            min = 1,
                            max = 100,
                            value = isolate(mst_node_size_reactive()),
                            ticks = FALSE
                          )
                        )
                      )
                    )
                  ),
                  br()
                )
              )
            )
          ),
          hr(),
          fluidRow(
            column(
              width = 12,
              align = "left",
              fluidRow(
                div(
                  class = "nj-label-control-col",
                  column(
                    width = 8,
                    h4(
                      p("Edges"),
                      style = "color:white; position: relative; top: 0px; margin-bottom: -10px; right: -15px"
                    )
                  )
                )
              ),
              fluidRow(
                column(
                  width = 12,
                  div(
                    class = "switch-mst-edges",
                    materialSwitch(
                      "mst_scale_edges",
                      h5(
                        p("Scale Allelic Distance"),
                        style = "color:white; padding-left: 0px; position: relative; top: -4px; right: -5px;"
                      ),
                      value = isolate(mst_scale_edges_reactive()),
                      right = TRUE
                    )
                  )
                )
              ),
              fluidRow(
                column(
                  width = 4,
                  conditionalPanel(
                    "input.mst_scale_edges==true",
                    HTML(
                      paste(
                        tags$span(
                          style = 'color: white; font-size: 14px; position: relative; bottom: -17px; margin-left: 15px ',
                          'Multiplier'
                        )
                      )
                    )
                  ),
                  conditionalPanel(
                    "input.mst_scale_edges==false",
                    HTML(
                      paste(
                        tags$span(
                          style = 'color: white; font-size: 14px; position: relative; bottom: -17px; margin-left: 15px ',
                          'Length'
                        )
                      )
                    )
                  )
                ),
                column(
                  width = 8,
                  align = "center",
                  conditionalPanel(
                    "input.mst_scale_edges==true",
                    div(
                      class = "slider_edge",
                      sliderInput(
                        inputId = "mst_edge_length_scale",
                        label = NULL,
                        min = 1,
                        max = 40,
                        value = isolate(mst_edge_length_scale_reactive()),
                        ticks = FALSE
                      )
                    )
                  ),
                  conditionalPanel(
                    "input.mst_scale_edges==false",
                    div(
                      class = "slider_edge",
                      sliderTextInput(
                        inputId = "mst_edge_length",
                        label = NULL,
                        choices = append(seq(0.1, 1, 0.1), 2:100),
                        selected = isolate(mst_edge_length_reactive()),
                        hide_min_max = FALSE
                      )
                    )
                  )
                )
              ),
              fluidRow(
                column(
                  width = 4,
                  HTML(
                    paste(
                      tags$span(
                        style = 'color: white; font-size: 14px; position: relative; top: 7px; margin-left: 15px;',
                        'Font'
                      )
                    )
                  )
                ),
                column(
                  width = 8,
                  align = "center",
                  div(
                    class = "mst-edge-slider",
                    sliderInput(
                      "mst_edge_font_size",
                      "",
                      value = isolate(mst_edge_font_size_reactive()),
                      step = 1,
                      min = 8,
                      max = 30,
                      ticks = FALSE
                    )
                  )
                )
              )
            )
          ),
          hr(),
          fluidRow(
            column(
              width = 12,
              align = "left",
              fluidRow(
                column(
                  width = 4,
                  HTML(
                    paste(
                      tags$span(
                        style = 'color: white; font-size: 14px; position: relative; top: 15px; margin-left: 15px;',
                        'Label'
                      )
                    )
                  )
                ),
                column(
                  width = 8,
                  align = "center",
                  div(
                    class = "mst-size-slider",
                    sliderInput(
                      "mst_node_label_fontsize",
                      "",
                      value = isolate(mst_node_label_fontsize_reactive()),
                      min = 8,
                      max = 30,
                      step = 1,
                      ticks = FALSE
                    )
                  )
                )
              ),
              fluidRow(
                column(
                  width = 4,
                  HTML(
                    paste(
                      tags$span(
                        style = 'color: white; font-size: 14px; position: relative; top: 15px; margin-left: 15px;',
                        'Title'
                      )
                    )
                  )
                ),
                column(
                  width = 8,
                  align = "center",
                  div(
                    class = "mst-size-slider",
                    sliderInput(
                      "mst_title_size",
                      "",
                      value = isolate(mst_title_size_reactive()),
                      min = 15,
                      max = 50,
                      step = 1,
                      ticks = FALSE
                    )
                  )
                )
              ),
              fluidRow(
                column(
                  width = 4,
                  HTML(
                    paste(
                      tags$span(
                        style = 'color: white; font-size: 14px; position: relative; top: 15px; margin-left: 15px;',
                        'Subtitle'
                      )
                    )
                  )
                ),
                column(
                  width = 8,
                  align = "center",
                  div(
                    class = "mst-size-slider2",
                    sliderInput(
                      "mst_subtitle_size",
                      "",
                      value = isolate(mst_subtitle_size_reactive()),
                      min = 15,
                      max = 40,
                      step = 1,
                      ticks = FALSE
                    )
                  )
                )
              )
            )
          )
        )
      )
    )

    runjs(unblock_ui)
  })

  ##### Other Menu ----

  mst_ratio_reactive <- reactiveVal()
  mst_scale_reactive <- reactiveVal()
  mst_shadow_reactive <- reactiveVal()
  mst_node_shape_reactive <- reactiveVal()
  mst_show_clusters_reactive <- reactiveVal()
  mst_cluster_col_scale_reactive <- reactiveVal()
  mst_cluster_type_reactive <- reactiveVal()
  mst_cluster_width_reactive <- reactiveVal()
  mst_cluster_threshold_reactive <- reactiveVal()
  mst_legend_ori_reactive <- reactiveVal()
  mst_font_size_reactive <- reactiveVal()
  mst_symbol_size_reactive <- reactiveVal()

  observe({
    ifelse(
      !is.null(input$mst_ratio),
      mst_ratio_reactive(input$mst_ratio),
      mst_ratio_reactive(16 / 10)
    )

    ifelse(
      !is.null(input$mst_scale),
      mst_scale_reactive(input$mst_scale),
      mst_scale_reactive(600)
    )

    ifelse(
      !is.null(input$mst_shadow),
      mst_shadow_reactive(input$mst_shadow),
      mst_shadow_reactive(TRUE)
    )

    ifelse(
      isTRUE(mst_color_var_reactive()),
      mst_node_shape_reactive("custom"),
      ifelse(
        !is.null(input$mst_node_shape),
        mst_node_shape_reactive(input$mst_node_shape),
        mst_node_shape_reactive("dot")
      )
    )

    ifelse(
      !is.null(input$mst_show_clusters),
      mst_show_clusters_reactive(input$mst_show_clusters),
      mst_show_clusters_reactive(FALSE)
    )

    ifelse(
      !is.null(input$mst_cluster_col_scale),
      mst_cluster_col_scale_reactive(input$mst_cluster_col_scale),
      mst_cluster_col_scale_reactive("Viridis")
    )

    ifelse(
      !is.null(input$mst_cluster_type),
      mst_cluster_type_reactive(input$mst_cluster_type),
      mst_cluster_type_reactive("Area")
    )

    ifelse(
      !is.null(input$mst_cluster_width),
      mst_cluster_width_reactive(input$mst_cluster_width),
      mst_cluster_width_reactive(24)
    )

    ifelse(
      !is.null(input$mst_cluster_threshold),
      mst_cluster_threshold_reactive(input$mst_cluster_threshold),
      ifelse(
        !is.null(DB$cluster_thresh),
        mst_cluster_threshold_reactive(DB$cluster_thresh),
        mst_cluster_threshold_reactive(10)
      )
    )

    ifelse(
      !is.null(input$mst_legend_ori),
      mst_legend_ori_reactive(input$mst_legend_ori),
      mst_legend_ori_reactive("left")
    )

    ifelse(
      !is.null(input$mst_font_size),
      mst_font_size_reactive(input$mst_font_size),
      mst_font_size_reactive(18)
    )

    ifelse(
      !is.null(input$mst_symbol_size),
      mst_symbol_size_reactive(input$mst_symbol_size),
      mst_symbol_size_reactive(20)
    )
  })

  observeEvent(input$mst_misc_menu, {
    runjs(block_ui)

    session$sendCustomMessage('mst_reset_style', "")
    session$sendCustomMessage('mst_highlight', "mst_misc_menu")

    output$mst_controls <- renderUI(
      div(
        class = "control-box",
        box(
          solidHeader = TRUE,
          status = "primary",
          width = "100%",
          title = "Miscellaneous",
          fluidRow(
            column(
              width = 12,
              align = "left",
              br(),
              fluidRow(
                div(
                  class = "nj-label-control-col",
                  column(
                    width = 12,
                    align = "left",
                    h4(
                      p("Dimensions"),
                      style = "color:white; position: relative; top: 0px; margin-bottom: -10px; right: -15px"
                    )
                  )
                )
              ),
              fluidRow(
                column(
                  width = 5,
                  align = "left",
                  HTML(
                    paste(
                      tags$span(
                        style = 'color: white; font-size: 14px; margin-left: 15px; position: relative; top: 27px',
                        'Ratio'
                      )
                    )
                  )
                ),
                column(
                  width = 6,
                  align = "center",
                  div(
                    class = "mst-control-ratio",
                    selectInput(
                      "mst_ratio",
                      "",
                      choices = c(
                        "16:10" = (16 / 10),
                        "16:9" = (16 / 9),
                        "4:3" = (4 / 3)
                      ),
                      selected = isolate(mst_ratio_reactive())
                    )
                  )
                )
              ),
              br(),
              fluidRow(
                column(
                  width = 4,
                  HTML(
                    paste(
                      tags$span(
                        style = 'color: white; margin-left: 15px; font-size: 14px; position: relative;',
                        'Scale'
                      )
                    )
                  )
                ),
                column(
                  width = 8,
                  align = "center",
                  div(
                    class = "mst-scale-slider",
                    sliderInput(
                      "mst_scale",
                      "",
                      min = 450,
                      max = 670,
                      step = 5,
                      value = isolate(mst_scale_reactive()),
                      ticks = FALSE
                    )
                  )
                )
              )
            )
          ),
          hr(),
          fluidRow(
            column(
              width = 12,
              align = "left",
              fluidRow(
                div(
                  class = "nj-label-control-col",
                  column(
                    width = 12,
                    align = "left",
                    h4(
                      p("Node Shapes"),
                      style = "color:white; position: relative; top: 0px; margin-bottom: -10px; right: -15px"
                    )
                  )
                )
              ),
              fluidRow(
                column(
                  width = 12,
                  div(
                    class = "switch-mst-edges",
                    materialSwitch(
                      "mst_shadow",
                      h5(
                        p("Show Shadow"),
                        style = "color:white; padding-left: 0px; position: relative; top: -4px; right: -5px;"
                      ),
                      value = isolate(mst_shadow_reactive()),
                      right = TRUE
                    )
                  )
                )
              ),
              fluidRow(
                column(
                  width = 5,
                  align = "left",
                  HTML(
                    paste(
                      tags$span(
                        style = 'color: white; margin-left: 15px; font-size: 14px; position: relative; top: 22px',
                        'Shape'
                      )
                    )
                  )
                ),
                column(
                  width = 6,
                  align = "center",
                  div(
                    class = "mst-shape",
                    uiOutput("mst_node_shape")
                  )
                )
              )
            )
          ),
          hr(),
          fluidRow(
            column(
              width = 12,
              align = "left",
              fluidRow(
                div(
                  class = "nj-label-control-col",
                  column(
                    width = 12,
                    align = "left",
                    h4(
                      p("Clustering"),
                      style = "color:white; position: relative; top: 0px; margin-bottom: -10px; right: -15px;"
                    ),
                    bslib::tooltip(
                      bsicons::bs_icon(
                        "info-circle",
                        title = "Cluster threshold according to species-specific\nComplex Type Distance (cgMLST.org)",
                        color = "white",
                        height = "15px",
                        width = "15px",
                        position = "relative",
                        top = "-21px",
                        right = "-115px"
                      ),
                      "Text shown in the tooltip.",
                      show = FALSE,
                      id = "mst_cluster_info"
                    )
                  )
                )
              ),
              fluidRow(
                column(
                  width = 8,
                  div(
                    class = "mst-cluster-switch",
                    materialSwitch(
                      "mst_show_clusters",
                      h5(
                        p("Show"),
                        style = "color:white; padding-left: 0px; position: relative; top: -4px; right: -5px;"
                      ),
                      value = isolate(mst_show_clusters_reactive()),
                      right = TRUE
                    )
                  )
                ),
                column(
                  width = 3,
                  align = "right",
                  dropMenu(
                    actionBttn(
                      "mst_cluster_col_menu",
                      label = "",
                      color = "default",
                      size = "sm",
                      style = "material-flat",
                      icon = icon("sliders")
                    ),
                    placement = "top-start",
                    theme = "translucent",
                    width = 5,
                    fluidRow(
                      column(
                        width = 12,
                        align = "center",
                        selectInput(
                          "mst_cluster_col_scale",
                          label = h5(
                            "Color Scale",
                            style = "color:white; margin-bottom: 0px;"
                          ),
                          choices = c("Viridis", "Rainbow"),
                          selected = isolate(mst_cluster_col_scale_reactive()),
                          width = "150px"
                        ),
                        br(),
                        selectInput(
                          "mst_cluster_type",
                          label = h5(
                            "Cluster Type",
                            style = "color:white; margin-bottom: 0px;"
                          ),
                          choices = c("Area", "Skeleton"),
                          selected = isolate(mst_cluster_type_reactive()),
                          width = "150px"
                        ),
                        br(),
                        conditionalPanel(
                          "input.mst_cluster_type=='Skeleton'",
                          sliderInput(
                            "mst_cluster_width",
                            label = h5(
                              "Skeleton Width",
                              style = "color:white; margin-bottom: 0px;"
                            ),
                            value = isolate(mst_cluster_width_reactive()),
                            step = 1,
                            min = 1,
                            max = 50,
                            ticks = FALSE,
                            width = "150px"
                          )
                        )
                      )
                    )
                  )
                )
              ),
              fluidRow(
                column(
                  width = 4,
                  HTML(
                    paste(
                      tags$span(
                        style = 'color: white; margin-left: 15px; text-align: left; font-size: 14px;',
                        'Threshold'
                      )
                    )
                  )
                ),
                column(
                  width = 5,
                  uiOutput("mst_cluster")
                ),
                column(
                  width = 2,
                  align = "right",
                  div(
                    id = "mst_cluster_reset_div",
                    actionButton(
                      "mst_cluster_reset",
                      label = "",
                      icon = icon("rotate")
                    ),
                    addTooltip(
                      session,
                      id = 'mst_cluster_reset',
                      title = "Labels",
                      placement = "bottom",
                      trigger = "hover",
                      options = list(delay = list(show = 400, hide = 300))
                    )
                  )
                )
              )
            )
          ),
          hr(),
          fluidRow(
            column(
              width = 12,
              align = "left",
              fluidRow(
                div(
                  class = "nj-label-control-col",
                  column(
                    width = 12,
                    align = "left",
                    h4(
                      p("Legend"),
                      style = "color:white; position: relative; top: 0px; margin-bottom: -10px; right: -15px"
                    )
                  )
                )
              ),
              fluidRow(
                column(
                  width = 8,
                  div(
                    class = "mst-legend-ori",
                    selectInput(
                      "mst_legend_ori",
                      label = "",
                      width = "90%",
                      choices = c("Left" = "left", "Right" = "right"),
                      selected = isolate(mst_legend_ori_reactive())
                    )
                  )
                ),
                column(2),
                column(
                  width = 3,
                  align = "right",
                  dropMenu(
                    actionBttn(
                      "mst_legend_menu",
                      label = "",
                      color = "default",
                      size = "sm",
                      style = "material-flat",
                      icon = icon("sliders")
                    ),
                    placement = "top-start",
                    theme = "translucent",
                    fluidRow(
                      column(
                        width = 11,
                        sliderInput(
                          "mst_font_size",
                          label = h5(
                            "Font Size",
                            style = "color:white; margin-bottom: 0px;"
                          ),
                          value = isolate(mst_font_size_reactive()),
                          min = 15,
                          max = 30,
                          step = 1,
                          ticks = FALSE,
                          width = "180px"
                        )
                      ),
                      column(1)
                    ),
                    br(),
                    fluidRow(
                      column(
                        width = 11,
                        sliderInput(
                          "mst_symbol_size",
                          label = h5(
                            "Key Size",
                            style = "color:white; margin-bottom: 0px;"
                          ),
                          value = isolate(mst_symbol_size_reactive()),
                          min = 10,
                          max = 30,
                          step = 1,
                          ticks = FALSE,
                          width = "180px"
                        )
                      ),
                      column(1)
                    )
                  )
                )
              )
            )
          )
        )
      )
    )

    runjs(unblock_ui)
  })

  ##### Export Menu ----

  observeEvent(input$mst_download_menu, {
    runjs(block_ui)

    session$sendCustomMessage('mst_reset_style', "")
    session$sendCustomMessage('mst_highlight', "mst_download_menu")

    output$mst_controls <- renderUI(
      div(
        class = "control-box",
        box(
          solidHeader = TRUE,
          status = "primary",
          width = "100%",
          title = "Export",
          fluidRow(
            column(
              width = 12,
              align = "center",
              br(),
              fluidRow(
                br(),
                column(
                  width = 6,
                  align = "left",
                  HTML(
                    paste(
                      tags$span(
                        style = 'color: white; font-size: 14px; position: relative; left: 10px;',
                        'Save plot as'
                      )
                    )
                  )
                ),
                column(
                  width = 6,
                  align = "center",
                  div(
                    class = "filetype-mst",
                    selectInput(
                      inputId = "mst_plot_format",
                      label = "",
                      choices = c("html", "jpeg", "png", "bmp")
                    )
                  )
                )
              ),
              fluidRow(
                column(6),
                column(
                  width = 6,
                  conditionalPanel(
                    "input.mst_plot_format=='jpeg'",
                    actionBttn(
                      "save_plot_jpeg",
                      style = "simple",
                      label = "",
                      size = "sm",
                      icon = icon("download"),
                      color = "primary"
                    )
                  ),
                  conditionalPanel(
                    "input.mst_plot_format=='png'",
                    actionBttn(
                      "save_plot_png",
                      style = "simple",
                      label = "",
                      size = "sm",
                      icon = icon("download"),
                      color = "primary"
                    )
                  ),
                  conditionalPanel(
                    "input.mst_plot_format=='bmp'",
                    actionBttn(
                      "save_plot_bmp",
                      style = "simple",
                      label = "",
                      size = "sm",
                      icon = icon("download"),
                      color = "primary"
                    )
                  ),
                  conditionalPanel(
                    "input.mst_plot_format=='html'",
                    downloadBttn(
                      "save_plot_html",
                      style = "simple",
                      label = "",
                      size = "sm",
                      icon = icon("download"),
                      color = "primary"
                    )
                  )
                )
              ),
              hr(),
              fluidRow(
                column(
                  width = 6,
                  align = "left",
                  HTML(
                    paste(
                      tags$span(
                        style = 'color: white; font-size: 14px; position: relative; left: 10px; top: 17px',
                        'Print Report'
                      )
                    )
                  )
                ),
                column(
                  width = 6,
                  align = "center",
                  actionButton(
                    "create_rep",
                    "",
                    icon = icon("file-waveform")
                  )
                )
              ),
              br(),
              br()
            )
          )
        )
      )
    )

    runjs(unblock_ui)
  })

  ##### Miscellaneous ----

  output$mst_node_shape <- renderUI({
    if (isTRUE(mst_color_var_reactive())) {
      choices <- c("Pie Nodes" = "custom")
      selected <- "custom"
    } else {
      choices <- list(
        `Label inside` = c("Circle" = "circle", "Box" = "box", "Text" = "text"),
        `Label outside` = c(
          "Diamond" = "diamond",
          "Hexagon" = "hexagon",
          "Dot" = "dot",
          "Square" = "square"
        )
      )
      selected <- mst_node_shape_reactive()
    }

    selectInput(
      "mst_node_shape",
      "",
      choices = choices,
      selected = selected
    )
  })

  # Reset all control inputs
  observeEvent(input$mst_reset, {
    showModal(
      div(
        class = "load-modal",
        modalDialog(
          fluidRow(
            br(),
            column(
              width = 11,
              p(
                HTML(
                  paste0(
                    '<span style="color: white; display: block; font-size: 15px; margin-left: 15px;">',
                    'Resetting all plot control inputs will lead to loss of all current settings. Continue?',
                    '</span>'
                  )
                )
              )
            ),
            br()
          ),
          title = "Reset Plot Settings",
          footer = tagList(
            modalButton("Cancel"),
            actionButton(
              "mst_reset_confirm",
              "Reset",
              class = "btn btn-default"
            )
          )
        )
      )
    )
  })

  observeEvent(input$mst_reset_confirm, {
    runjs(block_ui)

    removeModal()

    mst_node_label_reactive("Assembly Name")
    mst_title_reactive("")
    mst_subtitle_reactive("")
    mst_color_var_reactive(FALSE)
    mst_col_var_reactive("Isolation Date")
    mst_col_scale_reactive("Viridis")
    mst_text_color_reactive("#000000")
    mst_color_node_reactive("#B2FACA")
    mst_color_edge_reactive("#000000")
    mst_edge_font_color_reactive("#000000")
    mst_background_color_reactive("#ffffff")
    mst_background_transparent_reactive(FALSE)
    mst_scale_nodes_reactive(TRUE)
    mst_node_scale_reactive(c(20, 40))
    mst_node_size_reactive(30)
    mst_scale_edges_reactive(TRUE)
    mst_edge_length_scale_reactive(15)
    mst_edge_length_reactive(35)
    mst_edge_font_size_reactive(18)
    mst_node_label_fontsize_reactive(14)
    mst_title_size_reactive(35)
    mst_subtitle_size_reactive(20)
    mst_ratio_reactive(16 / 10)
    mst_scale_reactive(600)
    mst_shadow_reactive(TRUE)
    mst_node_shape_reactive("dot")
    mst_show_clusters_reactive(FALSE)
    mst_cluster_col_scale_reactive("Viridis")
    mst_cluster_type_reactive("Area")
    mst_cluster_width_reactive(24)
    ifelse(
      !is.null(DB$cluster_thresh),
      mst_cluster_threshold_reactive(DB$cluster_thresh),
      mst_cluster_threshold_reactive(10)
    )
    mst_legend_ori_reactive("left")
    mst_font_size_reactive(18)
    mst_symbol_size_reactive(20)

    runjs(unblock_ui)
  })

  # Size scaling MST
  observe({
    if (mst_ratio_reactive() == "1.6") {
      updateSliderInput(
        session,
        "mst_scale",
        step = 5,
        value = 600,
        min = 450,
        max = 670
      )
    } else if (mst_ratio_reactive() == "1.77777777777778") {
      updateSliderInput(
        session,
        "mst_scale",
        step = 9,
        value = 657,
        min = 450,
        max = 666
      )
    } else if (mst_ratio_reactive() == "1.33333333333333") {
      updateSliderInput(
        session,
        "mst_scale",
        step = 3,
        value = 654,
        min = 450,
        max = 669
      )
    }
  })

  # Clustering UI
  output$mst_cluster <- renderUI({
    req(DB$schemeinfo)

    div(
      class = "mst-threshold",
      numericInput(
        inputId = "mst_cluster_threshold",
        label = NULL,
        value = mst_cluster_threshold_reactive(),
        min = 1,
        max = 99
      )
    )
  })

  # MST color mapping
  output$mst_color_mapping <- renderUI({
    mst_color_node <- colorPickr(
      inputId = "mst_color_node",
      width = "100%",
      selected = mst_color_node_reactive(),
      label = "",
      update = "changestop",
      interaction = list(clear = FALSE, save = FALSE),
      position = "right-start"
    )

    if (isFALSE(mst_color_var_reactive())) {
      mst_color_node
    } else {
      disabled(mst_color_node)
    }
  })

  observeEvent(input$mst_color_var, {
    if (isTRUE(input$mst_color_var)) {
      updateSelectizeInput(
        session,
        inputId = "mst_node_shape",
        choices = c("Pie Nodes" = "custom")
      )
      updateSelectizeInput(
        session,
        inputId = "mst_node_label",
        choices = c("Assembly Name")
      )
    } else {
      updateSelectizeInput(
        session,
        inputId = "mst_node_shape",
        choices = list(
          `Label inside` = c(
            "Circle" = "circle",
            "Box" = "box",
            "Text" = "text"
          ),
          `Label outside` = c(
            "Diamond" = "diamond",
            "Hexagon" = "hexagon",
            "Dot" = "dot",
            "Square" = "square"
          )
        ),
        selected = c("Dot" = "dot")
      )
      updateSelectizeInput(
        session,
        inputId = "mst_node_label",
        choices = names(Vis$meta_mst)[c(1, 3, 4, 5)],
        selected = "Assembly Name"
      )
    }
  })

  # MST node labels
  output$mst_node_label <- renderUI({
    if (isTRUE(mst_color_var_reactive())) {
      choices <- "Assembly Name"
    } else {
      choices <- names(DB$meta)[c(1, 3, 4, 5)]
    }

    selectInput(
      "mst_node_label",
      label = "",
      choices = choices,
      selected = mst_node_label_reactive(),
      width = "100%"
    )
  })

  ### Plot Function ----

  #### MST ----

  mst_tree <- reactive({
    data <- toVisNetworkData(Vis$mst_pre)
    data$nodes <- mutate(
      data$nodes,
      label = Vis$unique_meta[,
        colnames(Vis$unique_meta) %in%
          mst_node_label_reactive()
      ],
      value = mst_node_scaling()
    )

    Vis$var_cols <- NULL

    # Generate pie charts as nodes
    if (isTRUE(mst_color_var_reactive())) {
      group <- character(nrow(data$nodes))
      for (i in 1:length(unique(Vis$meta_mst[[mst_col_var_reactive()]]))) {
        group[i] <- unique(Vis$meta_mst[[mst_col_var_reactive()]])[i]
      }

      data$nodes <- cbind(
        data$nodes,
        data.frame(metadata = character(nrow(data$nodes)))
      )

      if (length(which(data$nodes$group == "")) != 0) {
        data$nodes$group[which(data$nodes$group == "")] <- data$nodes$group[1]
      }

      if (is.null(mst_col_scale_reactive())) {
        Vis$var_cols <- data.frame(
          value = unique(Vis$meta_mst[[mst_col_var_reactive()]]),
          color = viridis(
            length(unique(Vis$meta_mst[[mst_col_var_reactive()]]))
          )
        )
      } else if (mst_col_scale_reactive() == "Rainbow") {
        Vis$var_cols <- data.frame(
          value = unique(Vis$meta_mst[[mst_col_var_reactive()]]),
          color = rainbow(
            length(unique(Vis$meta_mst[[mst_col_var_reactive()]]))
          )
        )
      } else if (mst_col_scale_reactive() == "Viridis") {
        Vis$var_cols <- data.frame(
          value = unique(Vis$meta_mst[[mst_col_var_reactive()]]),
          color = viridis(
            length(unique(Vis$meta_mst[[mst_col_var_reactive()]]))
          )
        )
      }

      for (i in 1:nrow(data$nodes)) {
        iso_subset <- strsplit(data$nodes$label[i], split = "\n")[[1]]
        variable <- Vis$meta_mst[[mst_col_var_reactive()]]
        values <- variable[which(Vis$meta_mst$`Assembly Name` %in% iso_subset)]

        for (j in 1:length(unique(values))) {
          share <- sum(unique(values)[j] == values) / length(values) * 100
          color <- Vis$var_cols$color[Vis$var_cols$value == unique(values)[j]]

          if (j == 1) {
            pie_vec <- paste0('{"value":', share, ',"color":"', color, '"}')
          } else {
            pie_vec <- paste0(
              pie_vec,
              ',{"value":',
              share,
              ',"color":"',
              color,
              '"}'
            )
          }
        }

        data$nodes$metadata[i] <- paste0('[', pie_vec, ']')
      }
    }

    data$edges <- mutate(
      data$edges,
      length = if (isFALSE(mst_scale_edges_reactive())) {
        mst_edge_length_reactive()
      } else {
        log(data$edges$weight) * mst_edge_length_scale_reactive()
      },
      label = as.character(data$edges$weight)
    )

    if (isTRUE(mst_show_clusters_reactive())) {
      clusters <- compute_clusters(
        data$nodes,
        data$edges,
        mst_cluster_threshold_reactive()
      )
      if (mst_cluster_type_reactive() == "Area") {
        data$nodes$group <- clusters$group
      }
    }

    visNetwork_graph <- visNetwork(
      data$nodes,
      data$edges,
      main = mst_title(),
      background = mst_background_color(),
      submain = mst_subtitle()
    ) %>%
      visNodes(
        size = mst_node_size_reactive(),
        shape = mst_node_shape(),
        shadow = mst_shadow_reactive(),
        color = mst_color_node_reactive(),
        ctxRenderer = ctxRendererJS,
        scaling = list(
          min = mst_node_scale_reactive()[1],
          max = mst_node_scale_reactive()[2]
        ),
        font = list(
          color = mst_text_color_reactive(),
          size = mst_node_label_fontsize_reactive()
        )
      ) %>%
      visEdges(
        color = mst_color_edge_reactive(),
        font = list(
          color = mst_edge_font_color_reactive(),
          size = mst_edge_font_size_reactive(),
          strokeWidth = 4,
          strokeColor = mst_background_color()
        )
      ) %>%
      visOptions(collapse = TRUE) %>%
      visInteraction(hover = TRUE) %>%
      visLayout(randomSeed = 1) %>%
      visLegend(
        useGroups = FALSE,
        zoom = TRUE,
        width = 0.2,
        position = mst_legend_ori_reactive(),
        ncol = legend_col(),
        addNodes = mst_legend()
      )

    if (isTRUE(mst_show_clusters_reactive())) {
      no_color <- length(unique(data$nodes$group[duplicated(data$nodes$group)]))
      no_color_edges <- length(unique(clusters$edge_group))

      if (mst_cluster_col_scale_reactive() == "Viridis") {
        color_palette <- viridis(no_color)
        color_edges <- viridis(no_color_edges)
      } else {
        color_palette <- rainbow(no_color + 1)
        color_edges <- rainbow(no_color_edges)
      }

      j <- 1
      if (mst_cluster_type_reactive() == "Area") {
        for (i in 1:length(unique(data$nodes$group))) {
          # Color only cluster with 2 or more nodes
          if (sum(data$nodes$group == unique(data$nodes$group)[i]) > 1) {
            visNetwork_graph <- visNetwork_graph %>%
              visGroups(
                groupname = unique(data$nodes$group)[i],
                color = color_palette[j]
              )
            j <- j + 1
          } else {
            visNetwork_graph <- visNetwork_graph %>%
              visGroups(
                groupname = unique(data$nodes$group)[i],
                color = mst_color_node_reactive()
              )
          }
        }
      } else {
        thin_edges <- data$edges
        thin_edges$width <- 2
        thin_edges$color <- "black"

        thick_edges <- data$edges
        thick_edges$width <- mst_cluster_width_reactive()
        thick_edges$color <- rep("rgba(0, 0, 0, 0)", length(data$edges$from))

        for (i in 1:length(unique(clusters$edge_group))) {
          if (unique(clusters$edge_group)[i] != "0") {
            edge_color <- paste(col2rgb(color_edges[i]), collapse = ", ")
            thick_edges$color[
              clusters$edge_group ==
                unique(
                  clusters$edge_group
                )[i]
            ] <- paste0("rgba(", edge_color, ", 0.5)")
          }
        }
        merged_edges <- rbind(thick_edges, thin_edges)
        data$edges <- merged_edges
        visNetwork_graph <- visNetwork(
          data$nodes,
          data$edges,
          main = mst_title(),
          background = mst_background_color(),
          submain = mst_subtitle()
        ) %>%
          visNodes(
            size = mst_node_size_reactive(),
            shape = mst_node_shape(),
            shadow = mst_shadow_reactive(),
            color = mst_color_node_reactive(),
            ctxRenderer = ctxRendererJS,
            scaling = list(
              min = mst_node_scale_reactive()[1],
              max = mst_node_scale_reactive()[2]
            ),
            font = list(
              color = mst_text_color_reactive(),
              size = mst_node_label_fontsize_reactive()
            )
          ) %>%
          visEdges(
            color = mst_color_edge_reactive(),
            font = list(
              color = mst_edge_font_color_reactive(),
              size = mst_edge_font_size_reactive(),
              strokeWidth = 4
            ),
            smooth = FALSE,
            physics = FALSE
          ) %>%
          visOptions(collapse = TRUE) %>%
          visInteraction(hover = TRUE) %>%
          visLayout(randomSeed = 1) %>%
          visLegend(
            useGroups = FALSE,
            zoom = TRUE,
            width = 0.2,
            position = mst_legend_ori_reactive(),
            ncol = legend_col(),
            addNodes = mst_legend()
          )
      }
    }
    visNetwork_graph
  })

  # MST legend
  legend_col <- reactive({
    if (!is.null(Vis$var_cols)) {
      if (nrow(Vis$var_cols) > 10) {
        3
      } else if (nrow(Vis$var_cols) > 5) {
        2
      } else {
        1
      }
    } else {
      1
    }
  })

  mst_legend <- reactive({
    if (is.null(Vis$var_cols)) {
      NULL
    } else {
      legend <- Vis$var_cols
      names(legend)[1] <- "label"
      mutate(
        legend,
        shape = "dot",
        font.color = mst_text_color_reactive(),
        size = mst_symbol_size_reactive(),
        font.size = mst_font_size_reactive()
      )
    }
  })

  # Set MST node shape
  mst_node_shape <- reactive({
    if (mst_node_shape_reactive() == "custom") {
      mst_node_shape_reactive()
    } else if (
      mst_node_shape_reactive() %in%
        c("circle", "database", "box", "text")
    ) {
      disable('mst_scale_nodes')
      updateCheckboxInput(session, "mst_scale_nodes", value = FALSE)
      disable('mst_node_size')
      disable('mst_node_scale')
      mst_node_shape_reactive()
    } else {
      enable('mst_scale_nodes')
      enable('mst_node_size')
      enable('mst_node_scale')
      mst_node_shape_reactive()
    }
  })

  # Node Size Scaling
  mst_node_scaling <- reactive({
    if (isTRUE(mst_scale_nodes_reactive())) {
      Vis$unique_meta$size
    } else {
      NULL
    }
  })

  # Set Title
  mst_title <- reactive({
    if (nchar(mst_title_reactive()) < 1) {
      list(
        text = "title",
        style = paste0(
          "font-family:Georgia, Times New Roman, Times, serif;",
          "text-align:center;",
          "font-size: ",
          as.character(mst_title_size_reactive()),
          "px",
          "; color: ",
          as.character(mst_background_color())
        )
      )
    } else {
      list(
        text = mst_title_reactive(),
        style = paste0(
          "font-family:Georgia, Times New Roman, Times, serif;",
          "text-align:center;",
          "font-size: ",
          as.character(mst_title_size_reactive()),
          "px",
          "; color: ",
          as.character(mst_text_color_reactive())
        )
      )
    }
  })

  # Set Subtitle
  mst_subtitle <- reactive({
    list(
      text = mst_subtitle_reactive(),
      style = paste0(
        "font-family:Georgia, Times New Roman, Times, serif;",
        "text-align:center;",
        "font-size: ",
        as.character(mst_subtitle_size_reactive()),
        "px",
        "; color: ",
        as.character(mst_text_color_reactive())
      )
    )
  })

  # Background color

  mst_background_color <- reactive({
    if (isTRUE(mst_background_transparent_reactive())) {
      'rgba(0, 0, 0, 0)'
    } else {
      mst_background_color_reactive()
    }
  })

  #### Hierarchical Tree ----

  make.tree <- reactive({
    Vis_nj <- Vis$nj
    Vis_nj$tip.label <- Vis$meta_nj$Index

    # Rooted tree
    if (nj_root_isolate_val() != "Automatic") {
      Vis_nj <- ape::root(
        Vis_nj,
        outgroup = which(Vis$meta_nj$`Assembly Name` == nj_root_isolate_val()),
        resolve.root = TRUE
      )
    }

    if (isTRUE(nj_nodelabel_show_val())) {
      ggtree(
        Vis_nj,
        alpha = 0.2,
        layout = layout_nj()
      ) +
        geom_nodelab(
          aes(label = node),
          color = "#29303A",
          size = nj_tiplab_size_val() + 1,
          hjust = 0.7
        ) +
        nj_limit() +
        nj_inward()
    } else {
      tree <- ggtree(
        Vis_nj,
        color = nj_color_val(),
        layout = layout_nj(),
        ladderize = nj_ladder_val()
      ) %<+%
        Vis$meta_nj +
        nj_clades() +
        nj_tiplab() +
        nj_tiplab_scale() +
        new_scale_color() +
        nj_limit() +
        nj_inward() +
        nj_label_branch() +
        nj_treescale() +
        nj_nodepoint() +
        nj_tippoint() +
        nj_tippoint_scale() +
        new_scale_color() +
        nj_clip_label() +
        nj_rootedge() +
        ggtitle(label = nj_title_val(), subtitle = nj_subtitle_val()) +
        theme_tree(bgcolor = nj_bg_val()) +
        theme(
          plot.title = element_text(
            colour = nj_title_color_val(),
            size = nj_title_size_val()
          ),
          plot.subtitle = element_text(
            colour = nj_title_color_val(),
            size = nj_subtitle_size_val()
          ),
          legend.background = element_rect(fill = "transparent", colour = NA),
          legend.direction = nj_legend_orientation_val(),
          legend.title = element_text(
            color = nj_color_val(),
            size = nj_legend_size_val() * 1.2
          ),
          legend.title.align = 0.5,
          legend.position = c(nj_legend_x_val(), nj_legend_y_val()),
          legend.text = element_text(
            color = nj_color_val(),
            size = nj_legend_size_val()
          ),
          legend.key = element_rect(fill = nj_bg_val()),
          legend.box.spacing = unit(1.5, "cm"),
          legend.key.size = unit(0.05 * nj_legend_size_val(), 'cm'),
          plot.background = element_rect(
            fill = nj_bg_val(),
            color = nj_bg_val()
          )
        ) +
        new_scale_fill() +
        nj_fruit() +
        nj_gradient() +
        new_scale_fill() +
        nj_fruit2() +
        nj_gradient2() +
        new_scale_fill() +
        nj_fruit3() +
        nj_gradient3() +
        new_scale_fill() +
        nj_fruit4() +
        nj_gradient4() +
        new_scale_fill() +
        nj_fruit5() +
        nj_gradient5() +
        new_scale_fill()

      # Add custom labels
      if (length(Vis$custom_label_nj) > 0) {
        for (i in Vis$custom_label_nj[, 1]) {
          ifelse(
            !is.null(Vis$nj_label_pos_x[[i]]),
            x_pos <- Vis$nj_label_pos_x[[i]],
            x_pos <- round(Vis$nj_max_x / 2, 0)
          )

          ifelse(
            !is.null(Vis$nj_label_pos_y[[i]]),
            y_pos <- Vis$nj_label_pos_y[[i]],
            y_pos <- sum(DB$data$Include) / 2
          )

          ifelse(
            !is.null(Vis$nj_label_size[[i]]),
            size <- Vis$nj_label_size[[i]],
            size <- 5
          )

          tree <- tree +
            annotate("text", x = x_pos, y = y_pos, label = i, size = size)
        }
      }

      # Add heatmap
      if (!is.null(Vis$nj_heatmap_select)) {
        if (
          isTRUE(nj_heatmap_show_val()) && length(Vis$nj_heatmap_select) > 0
        ) {
          if (
            !(any(sapply(Vis$meta_nj[Vis$nj_heatmap_select], is.numeric)) &&
              any(!sapply(Vis$meta_nj[Vis$nj_heatmap_select], is.numeric)))
          ) {
            tree <- gheatmap.mod(
              tree,
              data = select(Vis$meta_nj, Vis$nj_heatmap_select),
              offset = nj_heatmap_offset_val(),
              width = nj_heatmap_width(),
              legend_title = nj_heatmap_title_val(),
              colnames_angle = -nj_colnames_angle(),
              colnames_offset_y = nj_colnames_y(),
              colnames_color = nj_color_val()
            ) +
              nj_heatmap_scale()
          }
        }
      }

      # Sizing control
      Vis$nj_plot <- ggplotify::as.ggplot(
        tree,
        scale = nj_zoom_val(),
        hjust = nj_h_val(),
        vjust = nj_v_val()
      )

      Vis$nj_true <- TRUE

      # Correct background color if zoomed out
      cowplot::ggdraw(Vis$nj_plot) +
        theme(
          plot.background = element_rect(
            fill = nj_bg_val(),
            color = nj_bg_val()
          )
        )
    }
  })

  # Heatmap width
  nj_heatmap_width <- reactive({
    if (!is.null(nj_heatmap_width_val())) {
      nj_heatmap_width_val()
    } else {
      if (!is.null(Vis$nj_heatmap_select)) {
        length_input <- length(Vis$nj_heatmap_select)
        if (
          nj_layout_val() != "circular" &&
            nj_layout_val() != "inward"
        ) {
          if (length_input < 3) {
            0.1
          } else {
            if (length_input >= 3 && length_input <= 50) {
              min(0.15 + 0.05 * floor((length_input - 3) / 2), 1.5)
            } else {
              1.5
            }
          }
        } else {
          if (length_input < 3) {
            0.3
          } else if (length_input >= 3 && length_input <= 27) {
            min(0.6 + 0.2 * floor((length_input - 3) / 2), 1.5)
          } else {
            3
          }
        }
      } else {
        NULL
      }
    }
  })

  # Heatmap column titles position
  nj_colnames_y <- reactive({
    if (!is.null(nj_colnames_y_val())) {
      nj_colnames_y_val()
    } else {
      if (
        nj_layout_val() == "inward" ||
          nj_layout_val() == "circular"
      ) {
        0
      } else {
        -1
      }
    }
  })

  # Heatmap column titles angle
  nj_colnames_angle <- reactive({
    if (!is.null(nj_colnames_angle_val())) {
      nj_colnames_angle_val()
    } else {
      if (
        nj_layout_val() == "inward" ||
          nj_layout_val() == "circular"
      ) {
        90
      } else {
        -90
      }
    }
  })

  # Heatmap scale
  nj_heatmap_scale <- reactive({
    req(Vis$meta_nj, Vis$nj_heatmap_select)

    selected_var <- Vis$meta_nj[Vis$nj_heatmap_select]

    if (
      !(any(sapply(selected_var, is.numeric)) &&
        any(!sapply(selected_var, is.numeric)))
    ) {
      if (
        any(sapply(selected_var, is.numeric)) ||
          length(unique(unlist(selected_var))) > 7
      ) {
        if (nj_heatmap_scale_val() %in% unlist(diverging_scales)) {
          if (nj_heatmap_div_mid_val() == "Zero") {
            midpoint <- 0
          } else if (nj_heatmap_div_mid_val() == "Mean") {
            midpoint <- mean(as.matrix(selected_var), na.rm = TRUE)
          } else {
            midpoint <- median(as.matrix(selected_var), na.rm = TRUE)
          }
          scale_fill_gradient2(
            name = nj_heatmap_title_val(),
            low = brewer.pal(3, nj_heatmap_scale_val())[1],
            mid = brewer.pal(3, nj_heatmap_scale_val())[2],
            high = brewer.pal(3, nj_heatmap_scale_val())[3],
            midpoint = midpoint
          )
        } else {
          ifelse(
            any(sapply(selected_var, is.numeric)),
            discrete <- FALSE,
            discrete <- TRUE
          )

          if (nj_heatmap_scale_val() == "magma") {
            option <- "A"
          } else if (nj_heatmap_scale_val() == "inferno") {
            option <- "B"
          } else if (nj_heatmap_scale_val() == "plasma") {
            option <- "C"
          } else if (nj_heatmap_scale_val() == "viridis") {
            option <- "D"
          } else if (nj_heatmap_scale_val() == "cividis") {
            option <- "E"
          } else if (nj_heatmap_scale_val() == "rocket") {
            option <- "F"
          } else if (nj_heatmap_scale_val() == "mako") {
            option <- "G"
          } else if (nj_heatmap_scale_val() == "turbo") {
            option <- "H"
          } else {
            option <- "D"
          }

          scale_fill_viridis(
            name = nj_heatmap_title_val(),
            discrete = discrete,
            option = option
          )
        }
      } else {
        scale_fill_brewer(
          name = nj_heatmap_title_val(),
          palette = nj_heatmap_scale_val()
        )
      }
    }
  })

  # Tippoint Scale
  nj_tippoint_scale <- reactive({
    req(Vis$meta_nj)

    selected_var <- Vis$meta_nj[nj_tipcolor_mapping_val()]

    if (
      class(unlist(selected_var)) == "numeric" ||
        length(unique(unlist(selected_var))) > 7
    ) {
      if (nj_tippoint_scale_val() %in% unlist(diverging_scales)) {
        if (nj_tipcolor_mapping_div_mid_val() == "Zero") {
          midpoint <- 0
        } else if (nj_tipcolor_mapping_div_mid_val() == "Mean") {
          midpoint <- mean(as.matrix(selected_var), na.rm = TRUE)
        } else {
          midpoint <- median(as.matrix(selected_var), na.rm = TRUE)
        }
        scale_color_gradient2(
          name = nj_tipcolor_mapping_val(),
          low = brewer.pal(3, nj_tippoint_scale_val())[1],
          mid = brewer.pal(3, nj_tippoint_scale_val())[2],
          high = brewer.pal(3, nj_tippoint_scale_val())[3],
          midpoint = midpoint
        )
      } else {
        ifelse(
          class(unlist(selected_var)) == "numeric",
          discrete <- FALSE,
          discrete <- TRUE
        )

        if (nj_tippoint_scale_val() == "magma") {
          option <- "A"
        } else if (nj_tippoint_scale_val() == "inferno") {
          option <- "B"
        } else if (nj_tippoint_scale_val() == "plasma") {
          option <- "C"
        } else if (nj_tippoint_scale_val() == "viridis") {
          option <- "D"
        } else if (nj_tippoint_scale_val() == "cividis") {
          option <- "E"
        } else if (nj_tippoint_scale_val() == "rocket") {
          option <- "F"
        } else if (nj_tippoint_scale_val() == "mako") {
          option <- "G"
        } else if (nj_tippoint_scale_val() == "turbo") {
          option <- "H"
        } else {
          option <- "D"
        }

        scale_color_viridis(
          name = nj_tipcolor_mapping_val(),
          discrete = discrete,
          option = option
        )
      }
    } else {
      scale_color_brewer(
        name = nj_tipcolor_mapping_val(),
        palette = nj_tippoint_scale_val()
      )
    }
  })

  # Tiplab Scale
  nj_tiplab_scale <- reactive({
    req(Vis$meta_nj)

    selected_var <- Vis$meta_nj[nj_color_mapping_val()]

    if (
      class(unlist(selected_var)) == "numeric" ||
        length(unique(unlist(selected_var))) > 7
    ) {
      if (nj_tiplab_scale_val() %in% unlist(diverging_scales)) {
        if (nj_color_mapping_div_mid_val() == "Zero") {
          midpoint <- 0
        } else if (nj_color_mapping_div_mid_val() == "Mean") {
          midpoint <- mean(as.matrix(selected_var), na.rm = TRUE)
        } else {
          midpoint <- median(as.matrix(selected_var), na.rm = TRUE)
        }
        scale_color_gradient2(
          name = nj_color_mapping_val(),
          low = brewer.pal(3, nj_tiplab_scale_val())[1],
          mid = brewer.pal(3, nj_tiplab_scale_val())[2],
          high = brewer.pal(3, nj_tiplab_scale_val())[3],
          midpoint = midpoint
        )
      } else {
        ifelse(
          class(unlist(selected_var)) == "numeric",
          discrete <- FALSE,
          discrete <- TRUE
        )

        if (nj_tiplab_scale_val() == "magma") {
          option <- "A"
        } else if (nj_tiplab_scale_val() == "inferno") {
          option <- "B"
        } else if (nj_tiplab_scale_val() == "plasma") {
          option <- "C"
        } else if (nj_tiplab_scale_val() == "viridis") {
          option <- "D"
        } else if (nj_tiplab_scale_val() == "cividis") {
          option <- "E"
        } else if (nj_tiplab_scale_val() == "rocket") {
          option <- "F"
        } else if (nj_tiplab_scale_val() == "mako") {
          option <- "G"
        } else if (nj_tiplab_scale_val() == "turbo") {
          option <- "H"
        } else {
          option <- "D"
        }

        scale_color_viridis(
          name = nj_color_mapping_val(),
          discrete = discrete,
          option = option
        )
      }
    } else {
      scale_color_brewer(
        name = nj_color_mapping_val(),
        palette = nj_tiplab_scale_val()
      )
    }
  })

  # Clade Highlight
  nj_clades <- reactive({
    selected_nodes <- nj_parentnode_val()
    if (length(selected_nodes) == 1 && selected_nodes == "") {
      selected_nodes <- NULL
    }

    num_clades <- length(selected_nodes)

    if (num_clades != 0) {
      if (num_clades > 9) {
        show_toast(
          title = "Max. 9 clade highlights allowed",
          type = "error",
          position = "bottom-end",
          timer = 6000
        )
        return(NULL)
      } else {
        if (!is.null(nj_clade_scale_val())) {
          nj_clade_scale <- nj_clade_scale_val()
        } else {
          if (num_clades == 1) {
            nj_clade_scale <- "#D0F221"
          } else {
            nj_clade_scale <- "Set1"
          }
        }

        if (!is.null(nj_clade_type_val())) {
          nj_clade_type <- nj_clade_type_val()
        } else {
          nj_clade_type <- "roundrect"
        }

        if (num_clades == 1) {
          if (!startsWith(nj_clade_scale, "#")) {
            fill <- "#D0F221"
          } else {
            fill <- nj_clade_scale
          }
        } else if (num_clades >= 1) {
          ifelse(num_clades < 3, num_cols <- 3, num_cols <- num_clades)

          if (startsWith(nj_clade_scale, "#")) {
            fill <- brewer.pal(num_cols, "Set1")[1:num_clades]
          } else {
            fill <- brewer.pal(num_cols, nj_clade_scale)[1:num_clades]
          }
        }

        geom_hilight(
          node = as.numeric(selected_nodes),
          fill = fill,
          type = nj_clade_type,
          to.bottom = TRUE
        )
      }
    } else {
      NULL
    }
  })

  # Tiles fill fill gradient
  nj_gradient <- reactive({
    req(Vis$meta_nj)

    selected_var <- Vis$meta_nj[nj_fruit_variable_val()]

    if (
      class(unlist(selected_var)) == "numeric" ||
        length(unique(unlist(selected_var))) > 7
    ) {
      if (nj_tiles_scale_1_val() %in% unlist(diverging_scales)) {
        if (nj_tiles_mapping_div_mid_1_val() == "Zero") {
          midpoint <- 0
        } else if (nj_tiles_mapping_div_mid_1_val() == "Mean") {
          midpoint <- mean(as.matrix(selected_var), na.rm = TRUE)
        } else {
          midpoint <- median(as.matrix(selected_var), na.rm = TRUE)
        }
        scale_fill_gradient2(
          name = nj_fruit_variable_val(),
          low = brewer.pal(3, nj_tiles_scale_1_val())[1],
          mid = brewer.pal(3, nj_tiles_scale_1_val())[2],
          high = brewer.pal(3, nj_tiles_scale_1_val())[3],
          midpoint = midpoint
        )
      } else {
        ifelse(
          class(unlist(selected_var)) == "numeric",
          discrete <- FALSE,
          discrete <- TRUE
        )

        if (nj_tiles_scale_1_val() == "magma") {
          option <- "A"
        } else if (nj_tiles_scale_1_val() == "inferno") {
          option <- "B"
        } else if (nj_tiles_scale_1_val() == "plasma") {
          option <- "C"
        } else if (nj_tiles_scale_1_val() == "viridis") {
          option <- "D"
        } else if (nj_tiles_scale_1_val() == "cividis") {
          option <- "E"
        } else if (nj_tiles_scale_1_val() == "rocket") {
          option <- "F"
        } else if (nj_tiles_scale_1_val() == "mako") {
          option <- "G"
        } else if (nj_tiles_scale_1_val() == "turbo") {
          option <- "H"
        } else {
          option <- "D"
        }

        scale_fill_viridis(
          name = nj_fruit_variable_val(),
          discrete = discrete,
          option = option
        )
      }
    } else {
      scale_fill_brewer(
        name = nj_fruit_variable_val(),
        palette = nj_tiles_scale_1_val()
      )
    }
  })

  nj_gradient2 <- reactive({
    req(Vis$meta_nj)

    selected_var <- Vis$meta_nj[nj_fruit_variable_2_val()]

    if (
      class(unlist(selected_var)) == "numeric" ||
        length(unique(unlist(selected_var))) > 7
    ) {
      if (nj_tiles_scale_2_val() %in% unlist(diverging_scales)) {
        if (nj_tiles_mapping_div_mid_2_val() == "Zero") {
          midpoint <- 0
        } else if (nj_tiles_mapping_div_mid_2_val() == "Mean") {
          midpoint <- mean(as.matrix(selected_var), na.rm = TRUE)
        } else {
          midpoint <- median(as.matrix(selected_var), na.rm = TRUE)
        }
        scale_fill_gradient2(
          name = nj_fruit_variable_2_val(),
          low = brewer.pal(3, nj_tiles_scale_2_val())[1],
          mid = brewer.pal(3, nj_tiles_scale_2_val())[2],
          high = brewer.pal(3, nj_tiles_scale_2_val())[3],
          midpoint = midpoint
        )
      } else {
        ifelse(
          class(unlist(selected_var)) == "numeric",
          discrete <- FALSE,
          discrete <- TRUE
        )

        if (nj_tiles_scale_2_val() == "magma") {
          option <- "A"
        } else if (nj_tiles_scale_2_val() == "inferno") {
          option <- "B"
        } else if (nj_tiles_scale_2_val() == "plasma") {
          option <- "C"
        } else if (nj_tiles_scale_2_val() == "viridis") {
          option <- "D"
        } else if (nj_tiles_scale_2_val() == "cividis") {
          option <- "E"
        } else if (nj_tiles_scale_2_val() == "rocket") {
          option <- "F"
        } else if (nj_tiles_scale_2_val() == "mako") {
          option <- "G"
        } else if (nj_tiles_scale_2_val() == "turbo") {
          option <- "H"
        } else {
          option <- "D"
        }

        scale_fill_viridis(
          name = nj_fruit_variable_2_val(),
          discrete = discrete,
          option = option
        )
      }
    } else {
      scale_fill_brewer(
        name = nj_fruit_variable_2_val(),
        palette = nj_tiles_scale_2_val()
      )
    }
  })

  nj_gradient3 <- reactive({
    req(Vis$meta_nj)

    selected_var <- Vis$meta_nj[nj_fruit_variable_3_val()]

    if (
      class(unlist(selected_var)) == "numeric" ||
        length(unique(unlist(selected_var))) > 7
    ) {
      if (nj_tiles_scale_3_val() %in% unlist(diverging_scales)) {
        if (nj_tiles_mapping_div_mid_3_val() == "Zero") {
          midpoint <- 0
        } else if (nj_tiles_mapping_div_mid_3_val() == "Mean") {
          midpoint <- mean(as.matrix(selected_var), na.rm = TRUE)
        } else {
          midpoint <- median(as.matrix(selected_var), na.rm = TRUE)
        }
        scale_fill_gradient2(
          name = nj_fruit_variable_3_val(),
          low = brewer.pal(3, nj_tiles_scale_3_val())[1],
          mid = brewer.pal(3, nj_tiles_scale_3_val())[2],
          high = brewer.pal(3, nj_tiles_scale_3_val())[3],
          midpoint = midpoint
        )
      } else {
        ifelse(
          class(unlist(selected_var)) == "numeric",
          discrete <- FALSE,
          discrete <- TRUE
        )

        if (nj_tiles_scale_3_val() == "magma") {
          option <- "A"
        } else if (nj_tiles_scale_3_val() == "inferno") {
          option <- "B"
        } else if (nj_tiles_scale_3_val() == "plasma") {
          option <- "C"
        } else if (nj_tiles_scale_3_val() == "viridis") {
          option <- "D"
        } else if (nj_tiles_scale_3_val() == "cividis") {
          option <- "E"
        } else if (nj_tiles_scale_3_val() == "rocket") {
          option <- "F"
        } else if (nj_tiles_scale_3_val() == "mako") {
          option <- "G"
        } else if (nj_tiles_scale_3_val() == "turbo") {
          option <- "H"
        } else {
          option <- "D"
        }

        scale_fill_viridis(
          name = nj_fruit_variable_3_val(),
          discrete = discrete,
          option = option
        )
      }
    } else {
      scale_fill_brewer(
        name = nj_fruit_variable_3_val(),
        palette = nj_tiles_scale_3_val()
      )
    }
  })

  nj_gradient4 <- reactive({
    req(Vis$meta_nj)

    selected_var <- Vis$meta_nj[nj_fruit_variable_4_val()]

    if (
      class(unlist(selected_var)) == "numeric" ||
        length(unique(unlist(selected_var))) > 7
    ) {
      if (nj_tiles_scale_4_val() %in% unlist(diverging_scales)) {
        if (nj_tiles_mapping_div_mid_4_val() == "Zero") {
          midpoint <- 0
        } else if (nj_tiles_mapping_div_mid_4_val() == "Mean") {
          midpoint <- mean(as.matrix(selected_var), na.rm = TRUE)
        } else {
          midpoint <- median(as.matrix(selected_var), na.rm = TRUE)
        }
        scale_fill_gradient2(
          name = nj_fruit_variable_4_val(),
          low = brewer.pal(3, nj_tiles_scale_4_val())[1],
          mid = brewer.pal(3, nj_tiles_scale_4_val())[2],
          high = brewer.pal(3, nj_tiles_scale_4_val())[3],
          midpoint = midpoint
        )
      } else {
        ifelse(
          class(unlist(selected_var)) == "numeric",
          discrete <- FALSE,
          discrete <- TRUE
        )

        if (nj_tiles_scale_4_val() == "magma") {
          option <- "A"
        } else if (nj_tiles_scale_4_val() == "inferno") {
          option <- "B"
        } else if (nj_tiles_scale_4_val() == "plasma") {
          option <- "C"
        } else if (nj_tiles_scale_4_val() == "viridis") {
          option <- "D"
        } else if (nj_tiles_scale_4_val() == "cividis") {
          option <- "E"
        } else if (nj_tiles_scale_4_val() == "rocket") {
          option <- "F"
        } else if (nj_tiles_scale_4_val() == "mako") {
          option <- "G"
        } else if (nj_tiles_scale_4_val() == "turbo") {
          option <- "H"
        } else {
          option <- "D"
        }

        scale_fill_viridis(
          name = nj_fruit_variable_4_val(),
          discrete = discrete,
          option = option
        )
      }
    } else {
      scale_fill_brewer(
        name = nj_fruit_variable_4_val(),
        palette = nj_tiles_scale_4_val()
      )
    }
  })

  nj_gradient5 <- reactive({
    req(Vis$meta_nj)

    selected_var <- Vis$meta_nj[nj_fruit_variable_5_val()]

    if (
      class(unlist(selected_var)) == "numeric" ||
        length(unique(unlist(selected_var))) > 7
    ) {
      if (nj_tiles_scale_5_val() %in% unlist(diverging_scales)) {
        if (nj_tiles_mapping_div_mid_5_val() == "Zero") {
          midpoint <- 0
        } else if (nj_tiles_mapping_div_mid_5_val() == "Mean") {
          midpoint <- mean(as.matrix(selected_var), na.rm = TRUE)
        } else {
          midpoint <- median(as.matrix(selected_var), na.rm = TRUE)
        }
        scale_fill_gradient2(
          name = nj_fruit_variable_5_val(),
          low = brewer.pal(3, nj_tiles_scale_5_val())[1],
          mid = brewer.pal(3, nj_tiles_scale_5_val())[2],
          high = brewer.pal(3, nj_tiles_scale_5_val())[3],
          midpoint = midpoint
        )
      } else {
        ifelse(
          class(unlist(selected_var)) == "numeric",
          discrete <- FALSE,
          discrete <- TRUE
        )

        if (nj_tiles_scale_5_val() == "magma") {
          option <- "A"
        } else if (nj_tiles_scale_5_val() == "inferno") {
          option <- "B"
        } else if (nj_tiles_scale_5_val() == "plasma") {
          option <- "C"
        } else if (nj_tiles_scale_5_val() == "viridis") {
          option <- "D"
        } else if (nj_tiles_scale_5_val() == "cividis") {
          option <- "E"
        } else if (nj_tiles_scale_5_val() == "rocket") {
          option <- "F"
        } else if (nj_tiles_scale_5_val() == "mako") {
          option <- "G"
        } else if (nj_tiles_scale_5_val() == "turbo") {
          option <- "H"
        } else {
          option <- "D"
        }

        scale_fill_viridis(
          name = nj_fruit_variable_5_val(),
          discrete = discrete,
          option = option
        )
      }
    } else {
      scale_fill_brewer(
        name = nj_fruit_variable_5_val(),
        palette = nj_tiles_scale_5_val()
      )
    }
  })

  # No label clip off for linear NJ tree
  nj_clip_label <- reactive({
    if (
      nj_layout_val() != "circular" &&
        nj_layout_val() != "inward"
    ) {
      coord_cartesian(clip = "off")
    } else {
      NULL
    }
  })

  # Geom Fruit
  nj_fruit <- reactive({
    if (isTRUE(nj_tiles_show_1_val())) {
      geom_fruit(
        geom = geom_tile,
        mapping = aes(fill = !!sym(nj_fruit_variable_val())),
        offset = nj_fruit_offset_circ_val(),
        width = nj_fruit_width_circ_val(),
        alpha = nj_fruit_alpha_val()
      )
    } else {
      NULL
    }
  })

  # Geom Fruit
  nj_fruit2 <- reactive({
    if (isTRUE(nj_tiles_show_2_val())) {
      geom_fruit(
        geom = geom_tile,
        mapping = aes(fill = !!sym(nj_fruit_variable_2_val())),
        offset = nj_fruit_offset_circ_2_val(),
        width = nj_fruit_width_circ_2_val(),
        alpha = nj_fruit_alpha_2_val()
      )
    } else {
      NULL
    }
  })

  nj_fruit3 <- reactive({
    if (isTRUE(nj_tiles_show_3_val())) {
      geom_fruit(
        geom = geom_tile,
        mapping = aes(fill = !!sym(nj_fruit_variable_3_val())),
        offset = nj_fruit_offset_circ_3_val(),
        width = nj_fruit_width_circ_3_val(),
        alpha = nj_fruit_alpha_3_val()
      )
    } else {
      NULL
    }
  })

  nj_fruit4 <- reactive({
    if (isTRUE(nj_tiles_show_4_val())) {
      geom_fruit(
        geom = geom_tile,
        mapping = aes(fill = !!sym(nj_fruit_variable_4_val())),
        offset = nj_fruit_offset_circ_4_val(),
        width = nj_fruit_width_circ_4_val(),
        alpha = nj_fruit_alpha_4_val()
      )
    } else {
      NULL
    }
  })

  nj_fruit5 <- reactive({
    if (isTRUE(nj_tiles_show_5_val())) {
      geom_fruit(
        geom = geom_tile,
        mapping = aes(fill = !!sym(nj_fruit_variable_5_val())),
        offset = nj_fruit_offset_circ_5_val(),
        width = nj_fruit_width_circ_5_val(),
        alpha = nj_fruit_alpha_5_val()
      )
    } else {
      NULL
    }
  })

  # Xlim
  nj_limit <- reactive({
    if (nj_layout_val() == "circular") {
      xlim(nj_xlim_val(), NA)
    } else {
      NULL
    }
  })

  # NJ inward circular
  nj_inward <- reactive({
    if (nj_layout_val() == "inward") {
      if (between(nj_xlim_inw_val(), 30, 120)) {
        xlim <- nj_xlim_inw_val()
      } else {
        xlim <- 50
      }
      layout_inward_circular(xlim = xlim)
    } else {
      NULL
    }
  })

  # Treescale
  nj_treescale <- reactive({
    if (nj_layout_val() != "circular") {
      if (isTRUE(nj_treescale_show_val())) {
        if (nj_treescale_width_val() < 1) {
          width <- 1
        } else {
          width <- nj_treescale_width_val()
        }
        geom_treescale(
          x = nj_treescale_x_val(),
          y = nj_treescale_y_val(),
          width = width,
          color = nj_color_val(),
          fontsize = 4
        )
      } else {
        NULL
      }
    } else {
      NULL
    }
  })

  # Label branches
  nj_label_branch <- reactive({
    if (
      nj_layout_val() != "circular" ||
        nj_layout_val() != "inward"
    ) {
      if (isTRUE(nj_show_branch_label_val())) {
        geom_label(
          aes(
            x = !!sym("branch"),
            label = !!sym(nj_branch_label_val())
          ),
          fill = nj_branch_label_color_val(),
          size = nj_branch_size_val(),
          label.r = unit(nj_branch_labelradius_val(), "lines"),
          nudge_x = nj_branch_x_val(),
          fontface = nj_branchlab_fontface_val(),
          alpha = nj_branchlab_alpha_val()
        )
      } else {
        NULL
      }
    } else {
      NULL
    }
  })

  # Rootedge
  nj_rootedge <- reactive({
    if (isTRUE(nj_rootedge_show_val())) {
      geom_rootedge(
        rootedge = nj_rootedge_length_val(),
        linetype = nj_rootedge_line_val()
      )
    } else {
      NULL
    }
  })

  # Tippoints
  nj_tippoint <- reactive({
    if (
      !isTRUE(nj_tippoint_show_val()) &&
        !isTRUE(nj_tipcolor_mapping_show_val()) &&
        !isTRUE(nj_tipshape_mapping_show_val())
    ) {
      return(NULL)
    }

    aes_list <- list()

    if (isTRUE(nj_tipcolor_mapping_show_val())) {
      aes_list$color <- sym(nj_tipcolor_mapping_val())
    }
    if (isTRUE(nj_tipshape_mapping_show_val())) {
      aes_list$shape <- sym(nj_tipshape_mapping_val())
    }

    args <- list(
      mapping = do.call(aes, aes_list),
      alpha = nj_tippoint_alpha_val(),
      size = nj_tippoint_size_val()
    )

    if (!isTRUE(nj_tipcolor_mapping_show_val())) {
      args$color <- nj_tippoint_color_val()
      args$fill <- nj_tippoint_color_val()
    }

    if (!isTRUE(nj_tipshape_mapping_show_val())) {
      args$shape <- nj_tippoint_shape_val()
    }

    do.call(geom_tippoint, args)
  })

  # Nodepoints
  nj_nodepoint <- reactive({
    if (isTRUE(nj_nodepoint_show_val())) {
      geom_nodepoint(
        alpha = nj_nodepoint_alpha_val(),
        color = nj_nodepoint_color_val(),
        shape = nj_nodepoint_shape_val(),
        size = nj_nodepoint_size_val()
      )
    } else {
      NULL
    }
  })

  # NJ circularity
  nj_tiplab <- reactive({
    aes <- aes(
      label = !!sym(nj_tiplab_val()),
      color = if (isTRUE(nj_mapping_show_val())) {
        !!sym(
          nj_color_mapping_val()
        )
      } else {
        NULL
      }
    )

    common_params <- list(
      geom = if (isTRUE(nj_geom_val())) "label" else "text",
      size = nj_tiplab_size_val(),
      alpha = nj_tiplab_alpha_val(),
      fontface = nj_tiplab_fontface_val(),
      align = nj_align_val()
    )

    if (nj_layout_val() != "inward" && nj_layout_val() != "circular") {
      common_params$angle <- nj_tiplab_angle_val()
      common_params$nudge_x <- nj_tiplab_position_val()
    } else {
      common_params$hjust <- nj_tiplab_position_val()
    }

    if (isTRUE(nj_geom_val())) {
      common_params <- append(
        common_params,
        list(
          label.padding = unit(nj_tiplab_padding_val(), "lines"),
          label.r = unit(nj_tiplab_labelradius_val(), "lines"),
          fill = nj_tiplab_fill_val()
        )
      )
    }

    if (!isTRUE(nj_mapping_show_val())) {
      common_params <- append(
        common_params,
        list(color = nj_tiplab_color_val())
      )
    }

    if (isTRUE(nj_tiplab_show_val())) {
      do.call(geom_tiplab, c(list(mapping = aes), common_params))
    }
  })

  # NJ Tree Layout
  layout_nj <- reactive({
    if (nj_layout_val() == "inward") {
      "circular"
    } else {
      nj_layout_val()
    }
  })

  ### Save MST Plot ----
  output$save_plot_html <- downloadHandler(
    filename = function() {
      log_print(paste0("Save MST;", paste0("MST_", Sys.Date(), ".html")))
      paste0(Sys.Date(), "_", gsub(" ", "_", DB$scheme), "_MST.html")
    },
    content = function(file) {
      mst_tree() %>% visSave(file = file, background = mst_background_color())
    }
  )

  ### Save NJ Plot ----

  # Define download handler to save the plot

  output$download_nj <- downloadHandler(
    filename = function() {
      log_print(paste0(
        "Save NJ;",
        paste0("NJ_", Sys.Date(), ".", input$filetype_nj)
      ))
      paste0(
        Sys.Date(),
        "_",
        gsub(" ", "_", DB$scheme),
        "_Tree.",
        input$filetype_nj
      )
    },
    content = function(file) {
      if (input$filetype_nj == "png") {
        png(
          file,
          width = (as.numeric(nj_scale_val()) *
            as.numeric(nj_ratio_val())),
          height = as.numeric(nj_scale_val())
        )
        print(make.tree())
        dev.off()
      } else if (input$filetype_nj == "jpeg") {
        jpeg(
          file,
          width = (as.numeric(nj_scale_val()) *
            as.numeric(nj_ratio_val())),
          height = as.numeric(nj_scale_val()),
          quality = 100
        )
        print(make.tree())
        dev.off()
      } else if (input$filetype_nj == "svg") {
        plot <- print(make.tree())
        ggsave(
          file = file,
          plot = plot,
          device = svg(
            width = (as.numeric(nj_scale_val()) *
              as.numeric(nj_ratio_val())) /
              96,
            height = as.numeric(nj_scale_val()) / 96
          )
        )
      } else if (input$filetype_nj == "bmp") {
        bmp(
          file,
          width = (as.numeric(nj_scale_val()) *
            as.numeric(nj_ratio_val())),
          height = as.numeric(nj_scale_val())
        )
        print(make.tree())
        dev.off()
      }
    }
  )

  ### Reactive Events ----

  # Show isolate selection table
  observeEvent(input$table_view, {
    showModal(
      div(
        class = "start-modal",
        div(
          class = "modal-fit-content",
          modalDialog(
            fluidRow(
              fluidRow(
                br(),
                column(
                  width = 11,
                  p(
                    HTML(
                      paste0(
                        '<span style="color: white; display: block; font-size: 15px; margin-left: 15px;">',
                        "Click an isolate from the isolate table and confirm with 'Select'.",
                        '</span>'
                      )
                    )
                  ),
                  uiOutput("gs_table_selected_isolate")
                ),
                br()
              ),
              br(),
              column(
                width = 12,
                div(class = "amr-table", dataTableOutput("isolate_table"))
              ),
              br()
            ),
            title = paste("Select Isolate"),
            fade = TRUE,
            easyClose = TRUE,
            footer = tagList(
              modalButton("Dismiss"),
              actionButton("conf_isolate_select", "Select")
            )
          )
        )
      )
    )
  })

  # Render isolate selection table
  observe({
    req(DB$meta, DB$data)
    output$isolate_table <- renderDataTable(
      DB$meta[, -2],
      selection = "single",
      rownames = FALSE,
      options = list(
        scrollY = TRUE,
        pageLength = 10,
        autoWidth = TRUE,
        columnDefs = list(
          list(searchable = TRUE, targets = "_all", className = "dt-left")
        )
      )
    )
  })

  # Pass from table selected isolate to input
  observeEvent(input$conf_isolate_select, {
    req(DB$meta)

    if (!is.null(input$isolate_table_rows_selected)) {
      isolate <- DB$meta$`Assembly Name`[input$isolate_table_rows_selected]

      if (!is.na(isolate)) {
        updateSelectInput(session, "nj_root_isolate", selected = isolate)
      }

      removeModal()
    } else {
      show_toast(
        title = "No isolate selected",
        type = "warning",
        position = "bottom-end",
        timer = 6000
      )
    }
  })

  observeEvent(input$nj_heatmap_confirm, {
    if (isFALSE(nj_heatmap_show_val())) {
      nj_heatmap_show_val(TRUE)
      updateSwitchInput(session, "nj_heatmap_show", value = TRUE)
    }

    removeModal()

    if (input$nj_heatmap_map == "Variables") {
      Vis$nj_heatmap_select <- input$nj_heatmap_select
    } else if (input$nj_heatmap_map == "AMR Profile") {
      if (isTRUE(input$nj_heatmap_cluster)) {
        df_numeric <- as.data.frame(lapply(
          Vis$amr_nj[, input$nj_heatmap_select],
          as.numeric
        ))

        # Compute distance matrix using Jaccard distance
        dist_matrix <- dist(t(df_numeric), method = "binary")

        # Hierarchical clustering
        hc <- hclust(dist_matrix)

        # Reorder columns based on clustering
        Vis$nj_heatmap_select <- input$nj_heatmap_select[hc$order]
      } else {
        Vis$nj_heatmap_select <- input$nj_heatmap_select
      }
    }
  })

  # MST cluster reset button
  observeEvent(input$mst_cluster_reset, {
    if (!is.null(DB$schemeinfo)) {
      updateNumericInput(
        session,
        "mst_cluster_threshold",
        value = DB$cluster_thresh
      )
    }
  })

  # Conditional disabling of control elemenmts
  observe({
    req(input$nj_layout)

    # Tiles for inward layout
    if (input$nj_layout == "inward") {
      disable('nj_tiles_show')
      disable('nj_tiles_show_2')
      disable('nj_tiles_show_3')
      disable('nj_tiles_show_4')
      disable('nj_tiles_show_5')
      disable('nj_fruit_variable')
      disable('nj_fruit_variable_2')
      disable('nj_fruit_variable_3')
      disable('nj_fruit_variable_4')
      disable('nj_fruit_variable_5')
      disable('nj_fruit_width')
      disable('nj_fruit_width_2')
      disable('nj_fruit_width_3')
      disable('nj_fruit_width_4')
      disable('nj_fruit_width_5')
      disable('nj_fruit_offset')
      disable('nj_fruit_offset_2')
      disable('nj_fruit_offset_3')
      disable('nj_fruit_offset_4')
      disable('nj_fruit_offset_5')
    } else {
      enable('nj_tiles_show')
      enable('nj_tiles_show_2')
      enable('nj_tiles_show_3')
      enable('nj_tiles_show_4')
      enable('nj_tiles_show_5')
      enable('nj_fruit_variable')
      enable('nj_fruit_variable_2')
      enable('nj_fruit_variable_3')
      enable('nj_fruit_variable_4')
      enable('nj_fruit_variable_5')
      enable('nj_fruit_width')
      enable('nj_fruit_width_2')
      enable('nj_fruit_width_3')
      enable('nj_fruit_width_4')
      enable('nj_fruit_width_5')
      enable('nj_fruit_offset')
      enable('nj_fruit_offset_2')
      enable('nj_fruit_offset_3')
      enable('nj_fruit_offset_4')
      enable('nj_fruit_offset_5')
    }

    # Shut off branch labels for circular layout
    if (input$nj_layout == "circular" || input$nj_layout == "inward") {
      disable('nj_show_branch_label')
    } else {
      enable('nj_show_branch_label')
    }
  })

  #### Make Plot ----

  # Get distances
  hamming_dist <- reactive({
    req(DB$data, DB$allelic_profile)

    if (nrow(DB$data) > 2) {
      if (anyNA(DB$allelic_profile)) {
        if (input$na_handling == "omit") {
          allelic_profile_noNA <- DB$allelic_profile[,
            colSums(
              is.na(DB$allelic_profile)
            ) ==
              0
          ]

          allelic_profile_noNA_true <- allelic_profile_noNA[
            which(
              DB$data$Include == TRUE
            ),
          ]

          compute.distMatrix(allelic_profile_noNA_true, hamming.dist)
        } else if (input$na_handling == "ignore_na") {
          compute.distMatrix(DB$allelic_profile_true, hamming.distIgnore)
        } else {
          compute.distMatrix(DB$allelic_profile_true, hamming.distCategory)
        }
      } else {
        compute.distMatrix(DB$allelic_profile_true, hamming.dist)
      }
    }
  })

  hamming_mst <- reactive({
    if (anyNA(DB$allelic_profile)) {
      if (input$na_handling == "omit") {
        allelic_profile_noNA <- DB$allelic_profile[,
          colSums(
            is.na(DB$allelic_profile)
          ) ==
            0
        ]

        allelic_profile_noNA_true <- allelic_profile_noNA[
          which(
            DB$data$Include == TRUE
          ),
        ]

        dist <- compute.distMatrix(allelic_profile_noNA_true, hamming.dist)
      } else if (input$na_handling == "ignore_na") {
        dist <- compute.distMatrix(DB$allelic_profile_true, hamming.distIgnore)
      } else {
        dist <- compute.distMatrix(
          DB$allelic_profile_true,
          hamming.distCategory
        )
      }
    } else {
      dist <- compute.distMatrix(DB$allelic_profile_true, hamming.dist)
    }

    # Find indices of pairs with a distance of 0
    zero_distance_pairs <- as.data.frame(which(
      as.matrix(dist) == 0,
      arr.ind = TRUE
    ))

    zero_distance_pairs <-
      zero_distance_pairs[zero_distance_pairs$row != zero_distance_pairs$col, ]

    if (nrow(zero_distance_pairs) > 0) {
      # Sort each row so that x <= y
      df_sorted <- t(apply(zero_distance_pairs, 1, function(row) sort(row)))

      # Remove duplicate rows
      df_unique <- as.data.frame(unique(df_sorted))

      colnames(df_unique) <- c("col", "row")

      # get metadata in df
      vector_col <- character(0)
      count <- 1
      for (i in df_unique$col) {
        vector_col[count] <- Vis$meta_mst$`Assembly Name`[i]
        count <- count + 1
      }

      vector_row <- character(0)
      count <- 1
      for (i in df_unique$row) {
        vector_row[count] <- Vis$meta_mst$`Assembly Name`[i]
        count <- count + 1
      }

      col_id <- character(0)
      count <- 1
      for (i in df_unique$col) {
        col_id[count] <- Vis$meta_mst$`Assembly ID`[i]
        count <- count + 1
      }

      row_id <- character(0)
      count <- 1
      for (i in df_unique$row) {
        row_id[count] <- Vis$meta_mst$`Assembly ID`[i]
        count <- count + 1
      }

      col_index <- character(0)
      count <- 1
      for (i in df_unique$col) {
        col_index[count] <- Vis$meta_mst$Index[i]
        count <- count + 1
      }

      row_index <- character(0)
      count <- 1
      for (i in df_unique$row) {
        row_index[count] <- Vis$meta_mst$Index[i]
        count <- count + 1
      }

      col_date <- character(0)
      count <- 1
      for (i in df_unique$col) {
        col_date[count] <- Vis$meta_mst$`Isolation Date`[i]
        count <- count + 1
      }

      row_date <- character(0)
      count <- 1
      for (i in df_unique$row) {
        row_date[count] <- Vis$meta_mst$`Isolation Date`[i]
        count <- count + 1
      }

      col_host <- character(0)
      count <- 1
      for (i in df_unique$col) {
        col_host[count] <- Vis$meta_mst$Host[i]
        count <- count + 1
      }

      row_host <- character(0)
      count <- 1
      for (i in df_unique$row) {
        row_host[count] <- Vis$meta_mst$Host[i]
        count <- count + 1
      }

      col_country <- character(0)
      count <- 1
      for (i in df_unique$col) {
        col_country[count] <- Vis$meta_mst$Country[i]
        count <- count + 1
      }

      row_country <- character(0)
      count <- 1
      for (i in df_unique$row) {
        row_country[count] <- Vis$meta_mst$Country[i]
        count <- count + 1
      }

      col_city <- character(0)
      count <- 1
      for (i in df_unique$col) {
        col_city[count] <- Vis$meta_mst$City[i]
        count <- count + 1
      }

      row_city <- character(0)
      count <- 1
      for (i in df_unique$row) {
        row_city[count] <- Vis$meta_mst$City[i]
        count <- count + 1
      }

      df_unique <- cbind(
        df_unique,
        col_name = vector_col,
        row_name = vector_row,
        col_index = col_index,
        row_index = row_index,
        col_id = col_id,
        row_id = row_id,
        col_date = col_date,
        row_date = row_date,
        col_host = col_host,
        row_host = row_host,
        col_country = col_country,
        row_country = row_country,
        col_city = col_city,
        row_city = row_city
      )

      # Add groups
      grouped_df <- df_unique %>%
        group_by(col) %>%
        mutate(group_id = cur_group_id())

      # Merge groups
      name <- character(0)
      index <- character(0)
      id <- character(0)
      count <- 1
      for (i in grouped_df$group_id) {
        name[count] <- paste(
          unique(append(
            grouped_df$col_name[which(grouped_df$group_id == i)],
            grouped_df$row_name[which(grouped_df$group_id == i)]
          )),
          collapse = "\n"
        )

        id[count] <- paste(
          unique(append(
            grouped_df$col_id[which(grouped_df$group_id == i)],
            grouped_df$row_id[which(grouped_df$group_id == i)]
          )),
          collapse = "\n"
        )

        index[count] <- paste(
          unique(append(
            grouped_df$col_index[which(grouped_df$group_id == i)],
            grouped_df$row_index[which(grouped_df$group_id == i)]
          )),
          collapse = "\n"
        )

        count <- count + 1
      }

      merged_names <- cbind(
        grouped_df,
        "Index" = index,
        "Assembly Name" = name,
        "Assembly ID" = id
      )

      # remove duplicate groups

      final <- merged_names[!duplicated(merged_names$group_id), ]

      final_cleaned <- final[!(final$col_name %in% final$row_name), ]

      final_cleaned <- select(final_cleaned, 3, 17:20)

      # adapt metadata
      Date_merged <- character(0)
      for (j in 1:length(final_cleaned$Index)) {
        Date <- character(0)
        for (i in strsplit(final_cleaned$Index, "\n")[[j]]) {
          Date <- append(
            Date,
            Vis$meta_mst$`Isolation Date`[which(Vis$meta_mst$Index == i)]
          )
        }
        Date_merged <- append(Date_merged, paste(Date, collapse = "\n"))
      }

      Host_merged <- character(0)
      for (j in 1:length(final_cleaned$Index)) {
        Host <- character(0)
        for (i in strsplit(final_cleaned$Index, "\n")[[j]]) {
          Host <- append(
            Host,
            Vis$meta_mst$Host[which(Vis$meta_mst$Index == i)]
          )
        }
        Host_merged <- append(Host_merged, paste(Host, collapse = "\n"))
      }

      Country_merged <- character(0)
      for (j in 1:length(final_cleaned$Index)) {
        Country <- character(0)
        for (i in strsplit(final_cleaned$Index, "\n")[[j]]) {
          Country <- append(
            Country,
            Vis$meta_mst$Country[which(Vis$meta_mst$Index == i)]
          )
        }
        Country_merged <- append(
          Country_merged,
          paste(Country, collapse = "\n")
        )
      }

      City_merged <- character(0)
      for (j in 1:length(final_cleaned$Index)) {
        City <- character(0)
        for (i in strsplit(final_cleaned$Index, "\n")[[j]]) {
          City <- append(
            City,
            Vis$meta_mst$City[which(Vis$meta_mst$Index == i)]
          )
        }
        City_merged <- append(City_merged, paste(City, collapse = "\n"))
      }

      final_meta <- cbind(
        final_cleaned,
        "Isolation Date" = Date_merged,
        "Host" = Host_merged,
        "Country" = Country_merged,
        "City" = City_merged
      )

      # Merging with original data frame / allelic profile

      allelic_profile_true <- DB$allelic_profile_true
      meta_true <- Vis$meta_mst

      rownames(allelic_profile_true) <- Vis$meta_mst$`Assembly Name`
      rownames(meta_true) <- Vis$meta_mst$`Assembly Name`

      omit <- unique(append(df_unique$col_name, df_unique$row_name)) %in%
        final_cleaned$col_name

      omit_id <- unique(append(df_unique$col_name, df_unique$row_name))[!omit]

      remove <- !(rownames(allelic_profile_true) %in% omit_id)

      allelic_profile_clean <- allelic_profile_true[remove, ]

      meta_clean <- meta_true[remove, ]

      # substitute meta assembly names with group names
      count <- 1
      for (i in which(rownames(meta_clean) %in% final_meta$col_name)) {
        meta_clean$Index[i] <- final_meta$Index[count]
        meta_clean$`Assembly Name`[i] <- final_meta$`Assembly Name`[count]
        meta_clean$`Assembly ID`[i] <- final_meta$`Assembly ID`[count]
        meta_clean$`Isolation Date`[i] <- final_meta$`Isolation Date`[count]
        meta_clean$Host[i] <- final_meta$Host[count]
        meta_clean$Country[i] <- final_meta$Country[count]
        meta_clean$City[i] <- final_meta$City[count]
        count <- count + 1
      }

      # Metadata completion
      # get group size
      size_vector <- numeric(0)
      for (i in 1:nrow(meta_clean)) {
        if (str_count(meta_clean$`Assembly Name`[i], "\n") == 0) {
          size_vector[i] <- 1
        } else {
          size_vector[i] <- str_count(meta_clean$`Assembly Name`[i], "\n") + 1
        }
      }

      meta_clean <- mutate(meta_clean, size = size_vector)

      # get font size dependent on group size

      font_size <- numeric(nrow(meta_clean))

      for (i in 1:length(font_size)) {
        if (meta_clean$size[i] < 3) {
          font_size[i] <- 12
        } else {
          font_size[i] <- 11
        }
      }

      # get v-align dependent on group size
      valign <- numeric(nrow(meta_clean))

      for (i in 1:length(valign)) {
        if (meta_clean$size[i] == 1) {
          valign[i] <- -30
        } else if (meta_clean$size[i] == 2) {
          valign[i] <- -38
        } else if (meta_clean$size[i] == 3) {
          valign[i] <- -46
        } else if (meta_clean$size[i] == 4) {
          valign[i] <- -54
        } else if (meta_clean$size[i] == 5) {
          valign[i] <- -62
        } else if (meta_clean$size[i] > 5) {
          valign[i] <- -70
        }
      }

      Vis$unique_meta <- meta_clean %>%
        cbind(font_size = font_size, valign = valign)

      # final dist calculation

      if (anyNA(DB$allelic_profile)) {
        if (input$na_handling == "omit") {
          allelic_profile_clean_noNA_names <- allelic_profile_clean[,
            colSums(
              is.na(allelic_profile_clean)
            ) ==
              0
          ]
          compute.distMatrix(allelic_profile_clean_noNA_names, hamming.dist)
        } else if (input$na_handling == "ignore_na") {
          compute.distMatrix(allelic_profile_clean, hamming.distIgnore)
        } else {
          compute.distMatrix(allelic_profile_clean, hamming.distCategory)
        }
      } else {
        compute.distMatrix(allelic_profile_clean, hamming.dist)
      }
    } else {
      font_size <- rep(12, nrow(Vis$meta_mst))
      valign <- rep(-30, nrow(Vis$meta_mst))
      size <- rep(1, nrow(Vis$meta_mst))
      Vis$unique_meta <- Vis$meta_mst %>%
        cbind(size, font_size, valign)

      dist
    }
  })

  observeEvent(input$create_tree, {
    runjs(block_ui)
    log_print("Input create_tree")

    if (is.null(DB$data)) {
      log_print("Missing data")

      show_toast(
        title = "Missing data",
        type = "error",
        position = "bottom-end",
        timer = 6000
      )
    } else if (nrow(DB$allelic_profile_true) < 3) {
      log_print("Min. of 3 entries required for visualization")

      show_toast(
        title = "Min. of 3 entries required for visualization",
        type = "error",
        position = "bottom-end",
        timer = 6000
      )
    } else {
      if (
        any(duplicated(DB$meta$`Assembly Name`)) ||
          any(duplicated(DB$meta$`Assembly ID`))
      ) {
        log_print("Duplicated assemblies present")

        dup_name <- which(duplicated(DB$meta_true$`Assembly Name`))
        dup_id <- which(duplicated(DB$meta_true$`Assembly ID`))

        showModal(
          div(
            class = "start-modal",
            modalDialog(
              fluidRow(
                br(),
                column(
                  width = 11,
                  p(
                    HTML(
                      paste0(
                        '<span style="color: white; display: block; font-size: 15px; margin-left: 15px;">',
                        "Entries contain duplicated name(s). Please assign only unique assembly name(s).",
                        '</span>'
                      )
                    )
                  )
                ),
                br()
              ),
              title = "Duplicate entries",
              fade = TRUE,
              easyClose = TRUE,
              footer = tagList(
                modalButton("Dismiss"),
                actionButton(
                  "change_entries",
                  "Go to Entry Table",
                  class = "btn btn-default"
                )
              )
            )
          )
        )
      } else {
        set.seed(1)

        if (input$tree_type == "Tree") {
          log_print("Rendering tree plot")

          output$tree_field <- renderUI({
            addSpinner(
              plotOutput(
                "tree_plot",
                width = paste0(
                  as.character(
                    as.numeric(nj_scale_val()) *
                      as.numeric(nj_ratio_val())
                  ),
                  "px"
                ),
                height = paste0(as.character(nj_scale_val()), "px")
              ),
              spin = "dots",
              color = "#ffffff"
            )
          })

          meta_nj <- select(DB$meta_true, -2)
          if (
            file.exists(file.path(
              Startup$database,
              gsub(" ", "_", DB$scheme),
              "AMR_Profile.rds"
            ))
          ) {
            amr_profile <- readRDS(file.path(
              Startup$database,
              gsub(" ", "_", DB$scheme),
              "AMR_Profile.rds"
            ))

            if (isFALSE(any(meta_nj$Screened != "Yes"))) {
              Vis$amr_nj <- amr_profile$results[
                rownames(
                  amr_profile$results
                ) %in%
                  meta_nj$`Assembly ID`,
              ]
              meta_nj <- add_column(meta_nj, Vis$amr_nj)
            }
          }

          if (
            length(unique(gsub(" ", "_", colnames(meta_nj)))) <
              length(
                gsub(" ", "_", colnames(meta_nj))
              )
          ) {
            show_toast(
              title = "Conflicting Custom Variable Names",
              type = "warning",
              position = "bottom-end",
              timer = 6000
            )
          } else {
            # Create phylogenetic tree data
            if (input$tree_algo == "Neighbour-Joining") {
              Vis_nj <- ape::nj(hamming_dist())
              Vis$tree_algo <- "NJ"
            } else {
              Vis_nj <- phangorn::upgma(hamming_dist())
              Vis$tree_algo <- "UPGMA"
            }

            # Convert negative edges
            edge_lengths_abs <- abs(Vis_nj[["edge.length"]])
            edge_lengths_log <- log(
              edge_lengths_abs + sqrt(edge_lengths_abs^2 + 1)
            )
            Vis_nj[["edge.length"]] <- edge_lengths_log
            Vis$nj <- Vis_nj

            # Create phylogenetic tree meta data
            Vis$meta_nj <- mutate(meta_nj, taxa = Index) %>%
              relocate(taxa)

            # Get number of included entries calculate start values for tree
            if (nj_layout_val() == "circular" || nj_layout_val() == "inward") {
              if (sum(DB$data$Include) < 21) {
                Vis$labelsize_nj <- 5.5
                Vis$tippointsize_nj <- 5.5
                Vis$nodepointsize_nj <- 4
                Vis$tiplab_padding_nj <- 0.25
                Vis$branch_size_nj <- 4.5
              } else if (between(sum(DB$data$Include), 21, 40)) {
                Vis$labelsize_nj <- 5
                Vis$tippointsize_nj <- 5
                Vis$nodepointsize_nj <- 3.5
                Vis$tiplab_padding_nj <- 0.2
                Vis$branch_size_nj <- 4
              } else if (between(sum(DB$data$Include), 41, 60)) {
                Vis$labelsize_nj <- 4.5
                Vis$tippointsize_nj <- 4.5
                Vis$nodepointsize_nj <- 3
                Vis$tiplab_padding_nj <- 0.15
                Vis$branch_size_nj <- 3.5
              } else if (between(sum(DB$data$Include), 61, 80)) {
                Vis$labelsize_nj <- 4
                Vis$tippointsize_nj <- 4
                Vis$nodepointsize_nj <- 2.5
                Vis$tiplab_padding_nj <- 0.1
                Vis$branch_size_nj <- 3
              } else if (between(sum(DB$data$Include), 81, 100)) {
                Vis$labelsize_nj <- 3.5
                Vis$tippointsize_nj <- 3.5
                Vis$nodepointsize_nj <- 2
                Vis$tiplab_padding_nj <- 0.1
                Vis$branch_size_nj <- 2.5
              } else {
                Vis$labelsize_nj <- 3
                Vis$tippointsize_nj <- 3
                Vis$nodepointsize_nj <- 1.5
                Vis$tiplab_padding_nj <- 0.05
                Vis$branch_size_nj <- 2
              }
            } else {
              if (sum(DB$data$Include) < 21) {
                Vis$labelsize_nj <- 5
                Vis$tippointsize_nj <- 5
                Vis$nodepointsize_nj <- 4
                Vis$tiplab_padding_nj <- 0.25
                Vis$branch_size_nj <- 4.5
              } else if (between(sum(DB$data$Include), 21, 40)) {
                Vis$labelsize_nj <- 4.5
                Vis$tippointsize_nj <- 4.5
                Vis$nodepointsize_nj <- 3.5
                Vis$tiplab_padding_nj <- 0.2
                Vis$branch_size_nj <- 4
              } else if (between(sum(DB$data$Include), 41, 60)) {
                Vis$labelsize_nj <- 4
                Vis$tippointsize_nj <- 4
                Vis$nodepointsize_nj <- 3
                Vis$tiplab_padding_nj <- 0.15
                Vis$branch_size_nj <- 3.5
              } else if (between(sum(DB$data$Include), 61, 80)) {
                Vis$labelsize_nj <- 3.5
                Vis$tippointsize_nj <- 3.5
                Vis$nodepointsize_nj <- 2.5
                Vis$tiplab_padding_nj <- 0.1
                Vis$branch_size_nj <- 3
              } else if (between(sum(DB$data$Include), 81, 100)) {
                Vis$labelsize_nj <- 3
                Vis$tippointsize_nj <- 3
                Vis$nodepointsize_nj <- 2
                Vis$tiplab_padding_nj <- 0.1
                Vis$branch_size_nj <- 2.5
              } else {
                Vis$labelsize_nj <- 2.5
                Vis$tippointsize_nj <- 2.5
                Vis$nodepointsize_nj <- 1.5
                Vis$tiplab_padding_nj <- 0.05
                Vis$branch_size_nj <- 2
              }
            }

            nj_tree <- ggtree(Vis$nj)

            # Get upper and lower end of x range
            Vis$nj_max_x <- max(nj_tree$data$x)
            Vis$nj_min_x <- min(nj_tree$data$x)

            # Get parent node numbers
            Vis$nj_parentnodes <- nj_tree$data$parent

            # Update visualization control inputs
            nj_tiplab_size_val(Vis$labelsize_nj)
            nj_tippoint_size_val(Vis$tippointsize_nj)
            nj_nodepoint_size_val(Vis$nodepointsize_nj)
            nj_tiplab_padding_val(Vis$tiplab_padding_nj)
            nj_branch_size_val(Vis$branch_size_nj)
            nj_treescale_width_val(round(ceiling(Vis$nj_max_x) * 0.1, 0))
            nj_rootedge_length_val(round(ceiling(Vis$nj_max_x) * 0.05, 0))

            output$tree_plot <- renderPlot({
              make.tree()
            })

            Vis$nj_true <- TRUE
          }
        } else {
          log_print("Rendering MST graph")

          output$mst_field <- renderUI({
            if (isTRUE(mst_background_transparent_reactive())) {
              visNetworkOutput(
                "tree_mst",
                width = paste0(
                  mst_scale_reactive() * as.numeric(mst_ratio_reactive()),
                  "px"
                ),
                height = paste0(mst_scale_reactive(), "px")
              )
            } else {
              addSpinner(
                visNetworkOutput(
                  "tree_mst",
                  width = paste0(
                    mst_scale_reactive() * as.numeric(mst_ratio_reactive()),
                    "px"
                  ),
                  height = paste0(mst_scale_reactive(), "px")
                ),
                spin = "dots",
                color = "#ffffff"
              )
            }
          })

          if (nrow(DB$meta_true) > 150) {
            log_print("Over 100 isolates in MST graph")

            show_toast(
              title = "Computation might take a while",
              type = "info",
              position = "bottom-end",
              timer = 10000
            )
          }

          meta_mst <- DB$meta_true
          Vis$meta_mst <- meta_mst

          # prepare igraph object
          Vis$mst_pre <- hamming_mst() |>
            as.matrix() |>
            graph.adjacency(weighted = TRUE) |>
            igraph::mst()

          output$tree_mst <- renderVisNetwork({
            mst_tree()
          })

          Vis$mst_true <- TRUE
        }
      }
    }

    runjs(unblock_ui)
  })

  # _______________________ ####

  ## Report ----

  observe({
    if (!is.null(DB$data)) {
      if (!is.null(input$tree_type)) {
        if (input$tree_type == "MST") {
          disable("rep_plot_report")
          updateCheckboxInput(session, "rep_plot_report", value = FALSE)
        } else {
          enable("rep_plot_report")
        }
      }
    }
  })

  ### Report creation UI ----

  observeEvent(input$create_rep, {
    runjs(block_ui)

    if (
      (input$tree_type == "MST" && isTRUE(Vis$mst_true)) ||
        (input$tree_type == "Tree" && isTRUE(Vis$nj_true))
    ) {
      # Get currently selected missing value handling option
      if (input$na_handling == "ignore_na") {
        na_handling <- "Ignore missing values for pairwise comparison"
      } else if (input$na_handling == "omit") {
        na_handling <- "Omit loci with missing values for all assemblies"
      } else if (input$na_handling == "category") {
        na_handling <- "Treat missing values as allele variant"
      }

      extra_var <- character()
      if (input$tree_type == "MST") {
        runjs("mstReport();")
        if (isTRUE(mst_color_var_reactive())) {
          extra_var <- c(extra_var, mst_col_var_reactive())
        }
      } else if (input$tree_type == "Tree") {
        if (isTRUE(input$nj_mapping_show)) {
          extra_var <- c(extra_var, input$nj_color_mapping)
        }
        if (isTRUE(input$nj_tipcolor_mapping_show)) {
          extra_var <- c(extra_var, input$nj_tipcolor_mapping)
        }
        if (isTRUE(input$nj_tipshape_mapping_show)) {
          extra_var <- c(extra_var, input$nj_tipshape_mapping)
        }
        if (isTRUE(input$nj_tiles_show_1)) {
          extra_var <- c(extra_var, input$nj_fruit_variable)
        }
        if (isTRUE(input$nj_tiles_show_2)) {
          extra_var <- c(extra_var, input$nj_fruit_variable_2)
        }
        if (isTRUE(input$nj_tiles_show_3)) {
          extra_var <- c(extra_var, input$nj_fruit_variable_3)
        }
        if (isTRUE(input$nj_tiles_show_4)) {
          extra_var <- c(extra_var, input$nj_fruit_variable_4)
        }
        if (isTRUE(input$nj_tiles_show_5)) {
          extra_var <- c(extra_var, input$nj_fruit_variable_5)
        }
        if (isTRUE(input$nj_heatmap_show)) {
          extra_var <- c(extra_var, input$nj_heatmap_select)
        }
      }

      showModal(
        div(
          class = "start-modal",
          modalDialog(
            fluidRow(
              column(
                width = 12,
                fluidRow(
                  column(
                    width = 4,
                    align = "left",
                    HTML(
                      paste(
                        tags$span(
                          style = 'color:white; font-size: 15px; font-weight: 900',
                          'General'
                        )
                      )
                    )
                  ),
                  column(
                    width = 3,
                    align = "left",
                    checkboxInput(
                      "rep_general",
                      label = "",
                      value = TRUE
                    )
                  )
                ),
                fluidRow(
                  column(
                    width = 12,
                    align = "left",
                    fluidRow(
                      column(
                        width = 3,
                        checkboxInput(
                          "rep_date_general",
                          label = h5("Date", style = "color:white;"),
                          value = TRUE
                        )
                      ),
                      column(
                        width = 7,
                        dateInput(
                          "mst_date_general_select",
                          "",
                          max = Sys.Date()
                        )
                      )
                    ),
                    fluidRow(
                      column(
                        width = 3,
                        checkboxInput(
                          "rep_operator_general",
                          label = h5("Operator", style = "color:white;"),
                          value = TRUE
                        )
                      ),
                      column(
                        width = 8,
                        textInput(
                          "mst_operator_general_select",
                          ""
                        )
                      )
                    ),
                    fluidRow(
                      column(
                        width = 3,
                        checkboxInput(
                          "rep_institute_general",
                          label = h5("Institute", style = "color:white;"),
                          value = TRUE
                        )
                      ),
                      column(
                        width = 8,
                        textInput(
                          "mst_institute_general_select",
                          ""
                        )
                      )
                    ),
                    fluidRow(
                      column(
                        width = 3,
                        checkboxInput(
                          "rep_comm_general",
                          label = h5("Comment", style = "color:white;")
                        )
                      ),
                      column(
                        width = 8,
                        textAreaInput(
                          inputId = "mst_comm_general_select",
                          label = "",
                          width = "100%",
                          height = "60px",
                          cols = NULL,
                          rows = NULL,
                          placeholder = NULL,
                          resize = "vertical"
                        )
                      )
                    )
                  )
                ),
                hr(),
                fluidRow(
                  column(
                    width = 4,
                    align = "left",
                    HTML(
                      paste(
                        tags$span(
                          style = 'color: white; font-size: 15px; font-weight: 900',
                          'Isolate Table'
                        )
                      )
                    )
                  ),
                  column(
                    width = 3,
                    align = "left",
                    checkboxInput(
                      "rep_entrytable",
                      label = "",
                      value = TRUE
                    )
                  ),
                  column(
                    width = 4,
                    align = "left",
                    HTML(
                      paste(
                        tags$span(
                          style = 'color: white; font-size: 15px; font-weight: 900',
                          'Include Plot'
                        )
                      )
                    )
                  ),
                  column(
                    width = 1,
                    align = "left",
                    checkboxInput(
                      "rep_plot_report",
                      label = "",
                      value = TRUE
                    )
                  )
                ),
                fluidRow(
                  column(
                    width = 6,
                    align = "left",
                    div(
                      class = "rep_tab_sel",
                      pickerInput(
                        "select_rep_tab",
                        label = "",
                        choices = names(DB$meta)[-2],
                        selected = c(
                          "Assembly Name",
                          "Scheme",
                          "Isolation Date",
                          "Host",
                          "Country",
                          "City",
                          extra_var
                        ),
                        options = list(
                          size = 10,
                          `actions-box` = TRUE,
                          style = "background-color: white; border-radius: 5px;"
                        ),
                        multiple = TRUE
                      )
                    )
                  )
                ),
                hr(),
                fluidRow(
                  column(
                    width = 4,
                    align = "left",
                    HTML(
                      paste(
                        tags$span(
                          style = 'color: white; font-size: 15px; font-weight: 900',
                          'Analysis Parameter'
                        )
                      )
                    )
                  ),
                  column(
                    width = 3,
                    align = "left",
                    checkboxInput(
                      "rep_analysis",
                      label = "",
                      value = TRUE
                    )
                  )
                ),
                fluidRow(
                  column(
                    width = 6,
                    align = "left",
                    fluidRow(
                      column(
                        width = 4,
                        checkboxInput(
                          "rep_cgmlst_analysis",
                          label = h5("Scheme", style = "color:white;"),
                          value = TRUE
                        )
                      ),
                      column(
                        width = 8,
                        align = "right",
                        HTML(
                          paste(
                            tags$span(
                              style = 'color:white; position: relative; top: 17px; font-style: italic',
                              DB$scheme
                            )
                          )
                        )
                      )
                    ),
                    fluidRow(
                      column(
                        width = 4,
                        checkboxInput(
                          "rep_tree_analysis",
                          label = h5("Tree", style = "color:white;"),
                          value = TRUE
                        )
                      ),
                      column(
                        width = 8,
                        align = "right",
                        HTML(
                          paste(
                            tags$span(
                              style = 'color:white; position: relative; top: 17px; font-style: italic',
                              input$tree_type
                            )
                          )
                        )
                      )
                    )
                  ),
                  column(
                    width = 6,
                    align = "left",
                    fluidRow(
                      column(2),
                      column(
                        width = 4,
                        checkboxInput(
                          "rep_distance",
                          label = h5("Distance", style = "color:white;"),
                          value = TRUE
                        )
                      ),
                      column(
                        width = 5,
                        align = "right",
                        HTML(
                          paste(
                            tags$span(
                              style = 'color:white; position: relative; top: 17px; font-style: italic',
                              'Hamming'
                            )
                          )
                        )
                      )
                    ),
                    fluidRow(
                      column(2),
                      column(
                        width = 4,
                        checkboxInput(
                          "rep_version",
                          label = h5("Version", style = "color:white;"),
                          value = TRUE
                        )
                      ),
                      column(
                        width = 5,
                        align = "right",
                        HTML(
                          paste(
                            tags$span(
                              style = 'color:white; position: relative; top: 17px; font-style: italic',
                              phylotraceVersion
                            )
                          )
                        )
                      )
                    )
                  )
                ),
                fluidRow(
                  column(
                    width = 3,
                    align = "left",
                    checkboxInput(
                      "rep_missval",
                      label = h5("NA handling", style = "color:white;"),
                      value = TRUE
                    )
                  ),
                  column(
                    width = 7,
                    align = "right",
                    HTML(
                      paste(
                        tags$span(
                          style = 'color:white; position: relative; top: 17px; font-style: italic; right: 35px;',
                          na_handling
                        )
                      )
                    )
                  )
                )
              )
            ),
            title = "cgMLST Report Generation",
            easyClose = TRUE,
            footer = tagList(
              modalButton("Dismiss"),
              downloadBttn(
                "download_report",
                style = "simple",
                label = "Save",
                size = "sm",
                icon = icon("download")
              )
            )
          )
        )
      )
    } else {
      show_toast(
        title = "No tree created",
        type = "error",
        position = "bottom-end",
        timer = 6000
      )
    }

    runjs(unblock_ui)
  })

  observe({
    if (!is.null(input$rep_general)) {
      if (isFALSE(input$rep_general)) {
        disable('rep_date_general')
        disable('rep_operator_general')
        disable('rep_institute_general')
        disable('rep_comm_general')
        disable('mst_date_general_select')
        disable('mst_operator_general_select')
        disable('mst_institute_general_select')
        disable('mst_comm_general_select')
      } else {
        enable('rep_date_general')
        enable('rep_operator_general')
        enable('rep_institute_general')
        enable('rep_comm_general')
        enable('mst_date_general_select')
        enable('mst_operator_general_select')
        enable('mst_institute_general_select')
        enable('mst_comm_general_select')
      }
    }

    if (!is.null(input$rep_analysis)) {
      if (isFALSE(input$rep_analysis)) {
        disable('rep_cgmlst_analysis')
        disable('rep_tree_analysis')
        disable('rep_distance')
        disable('rep_missval')
        disable('rep_version')
      } else {
        enable('rep_cgmlst_analysis')
        enable('rep_tree_analysis')
        enable('rep_distance')
        enable('rep_missval')
        enable('rep_version')
      }
    }

    if (length(input$select_rep_tab) > 0) {
      updateCheckboxInput(session, "rep_entrytable", value = TRUE)
    } else {
      updateCheckboxInput(session, "rep_entrytable", value = FALSE)
    }
  })

  ### Save Report ----

  #### Get Report elements ----

  observe({
    if (!is.null(DB$data)) {
      if (!is.null(input$tree_type)) {
        req(c(
          input$rep_entrytable,
          input$rep_general,
          input$rep_date_general,
          input$rep_operator_general,
          input$rep_institute_general,
          input$rep_comm_general,
          input$rep_analysis,
          input$rep_cgmlst_analysis,
          input$rep_tree_analysis,
          input$rep_distance,
          input$rep_missval,
          input$rep_version,
          input$rep_plot_report,
          input$select_rep_tab
        ))
        Report$report_df <- data.frame(
          Element = c(
            "entry_table",
            "general_show",
            "general_date",
            "operator",
            "institute",
            "comment",
            "analysis_show",
            "scheme",
            "tree",
            "distance",
            "na_handling",
            "version",
            "plot"
          ),
          Include = c(
            input$rep_entrytable,
            input$rep_general,
            input$rep_date_general,
            input$rep_operator_general,
            input$rep_institute_general,
            input$rep_comm_general,
            input$rep_analysis,
            input$rep_cgmlst_analysis,
            input$rep_tree_analysis,
            input$rep_distance,
            input$rep_missval,
            input$rep_version,
            input$rep_plot_report
          )
        )
      }
    }
  })

  #### Get Report values ----

  observeEvent(input$create_tree, {
    runjs(block_ui)

    if (input$tree_type == "MST") {
      Report$report_list_mst <- list(
        entry_table = DB$meta_true,
        scheme = DB$schemeinfo,
        tree = input$tree_type,
        na_handling = if (
          anyNA(
            DB$allelic_profile_true
          )
        ) {
          input$na_handling
        } else {
          NULL
        },
        distance = "Hamming Distances",
        version = c(phylotraceVersion, "2.5.1"),
        plot = "MST"
      )
    } else if (input$tree_type == "Tree") {
      Report$report_list_nj <- list(
        entry_table = DB$meta_true,
        scheme = DB$schemeinfo,
        tree = input$tree_type,
        na_handling = input$na_handling,
        distance = "Hamming Distances",
        version = c(phylotraceVersion, "2.5.1"),
        plot = "NJ"
      )
    }

    runjs(unblock_ui)
  })

  # Save plot for Report
  plot.report <- reactive({
    if (tree_type_reactive() == "Tree") {
      jpeg(
        paste0(getwd(), "/Report/NJ.jpeg"),
        width = (as.numeric(nj_scale_val()) * as.numeric(nj_ratio_val())),
        height = as.numeric(nj_scale_val()),
        quality = 100
      )
      print(make.tree())
      dev.off()
    } else if (tree_type_reactive() == "MST") {
      runjs("mstReport();")
      decoded_data <- base64enc::base64decode(input$canvas_data)
      writeBin(decoded_data, paste0(getwd(), "/Report/MST.jpg"))
    }
  })

  #### Event Save Report ----
  output$download_report <- downloadHandler(
    filename = function() {
      paste0(Sys.Date(), "_", gsub(" ", "_", DB$scheme), "_Report.html")
    },
    content = function(file) {
      runjs(block_ui)
      if (input$tree_type == "MST") {
        plot.report()

        report <- c(
          Report$report_list_mst,
          "general_date" = as.character(input$mst_date_general_select),
          "operator" = input$mst_operator_general_select,
          "institute" = input$mst_institute_general_select,
          "comment" = input$mst_comm_general_select,
          "report_df" = Report$report_df
        )

        report[["table_columns"]] <- input$select_rep_tab

        # Save data to an RDS file if any elements were selected
        if (!is.null(report)) {
          log_print("Creating MST report")

          saveRDS(
            report,
            file = paste0(getwd(), "/Report/selected_elements.rds")
          )

          rmarkdown::render(paste0(getwd(), "/Report/Report.Rmd"))

          file.copy(paste0(getwd(), "/Report/Report.html"), file)
        } else {
          log_print("Creating MST report failed (report is null)")
        }
      } else if (input$tree_type == "Tree") {
        plot.report()
        report <- c(
          Report$report_list_nj,
          "general_date" = as.character(input$mst_date_general_select),
          "operator" = input$mst_operator_general_select,
          "institute" = input$mst_institute_general_select,
          "comment" = input$mst_comm_general_select,
          "report_df" = Report$report_df
        )

        report[["table_columns"]] <- input$select_rep_tab

        # Save data to an RDS file if any elements were selected
        if (!is.null(report)) {
          log_print("Creating NJ report")

          saveRDS(
            report,
            file = paste0(getwd(), "/Report/selected_elements.rds")
          )

          rmarkdown::render(paste0(getwd(), "/Report/Report.Rmd"))

          file.copy(paste0(getwd(), "/Report/Report.html"), file)
        } else {
          log_print("Creating NJ report failed (report is null)")
        }
      }

      removeModal()
      runjs(unblock_ui)
    }
  )

  # _______________________ ####

  ## Gene Screening  ----

  # Check is gene screening available for species
  observe({
    req(DB$scheme)
    amrfinder_available <- check.amrfinder.available(
      selected_scheme = DB$scheme,
      amrfinder_species = amrfinder_species
    )
    if (!isFALSE(amrfinder_available)) {
      Screening$available <- TRUE
    } else {
      Screening$available <- NULL
    }
  })

  ### Reactive Events ----

  # download gs plot
  output$gs_download_plot <- downloadHandler(
    filename = function() {
      log_print(paste0("Save AMR heatmap"))
      paste0(
        Sys.Date(),
        "_",
        gsub(" ", "_", DB$scheme),
        "_AMR_Heatmap.",
        input$filetype_gs
      )
    },
    content = function(file) {
      ht_opt$message <- FALSE

      if (input$filetype_gs == "png") {
        png(
          file,
          width = as.numeric(input_gs_scale()) * as.numeric(input_gs_ratio()),
          height = as.numeric(input_gs_scale())
        )
        print(gs_plot())
        dev.off()
      } else if (input$filetype_gs == "jpeg") {
        jpeg(
          file,
          width = as.numeric(input_gs_scale()) * as.numeric(input_gs_ratio()),
          height = as.numeric(input_gs_scale()),
          quality = 100
        )
        print(gs_plot())
        dev.off()
      } else if (input$filetype_gs == "svg") {
        plot <- print(gs_plot())
        svg(
          file,
          width = (as.numeric(
            input_gs_scale()
          ) *
            as.numeric(input_gs_ratio())) /
            92,
          height = as.numeric(input_gs_scale()) / 92
        )
        print(gs_plot())
        dev.off()
      } else if (input$filetype_gs == "bmp") {
        bmp(
          file,
          width = as.numeric(input_gs_scale()) * as.numeric(input_gs_ratio()),
          height = as.numeric(input_gs_scale())
        )
        print(gs_plot())
        dev.off()
      }
    }
  )

  ### Screening UI Elements ----

  # conditional rendering of gs visualization ui

  output$gs_visualization_ui <- renderUI({
    req(DB$data, DB$scheme, Screening$available, Screening$amr_results)
    amrfinder_available <- check.amrfinder.available(
      selected_scheme = DB$scheme,
      amrfinder_species = amrfinder_species
    )

    if (!isFALSE(amrfinder_available)) {
      fluidRow(
        column(
          width = 2,
          align = "left",
          br(),
          br(),
          div(
            class = "gs-plot-box",
            box(
              solidHeader = TRUE,
              status = "primary",
              width = "100%",
              title = "Plot Controls",
              fluidRow(
                column(
                  width = 6,
                  align = "right",
                  div(
                    id = "gs-control",
                    tipify(
                      actionBttn(
                        "gs_data_menu",
                        label = "",
                        color = "default",
                        size = "sm",
                        style = "material-flat",
                        icon = icon("table-cells")
                      ),
                      title = "Data Mapping",
                      placement = "left",
                      options = list("delay': 400, 'foo" = "foo")
                    )
                  )
                ),
                column(
                  width = 6,
                  align = "left",
                  div(
                    id = "gs-variable-menu",
                    tipify(
                      actionBttn(
                        "gs_variable_menu",
                        label = "",
                        color = "default",
                        size = "sm",
                        style = "material-flat",
                        icon = icon("map-pin")
                      ),
                      title = "Variable Mapping",
                      placement = "right",
                      options = list("delay': 400, 'foo" = "foo")
                    )
                  )
                )
              ),
              fluidRow(
                column(
                  width = 6,
                  align = "right",
                  br(),
                  div(
                    id = "gs-control",
                    tipify(
                      actionBttn(
                        "gs_color_menu",
                        label = "",
                        color = "default",
                        size = "sm",
                        style = "material-flat",
                        icon = icon("palette")
                      ),
                      title = "Colors",
                      placement = "left",
                      options = list("delay': 400, 'foo" = "foo")
                    )
                  )
                ),
                column(
                  width = 6,
                  align = "left",
                  br(),
                  div(
                    id = "gs-size-menu",
                    tipify(
                      actionBttn(
                        "gs_size_menu",
                        label = "",
                        color = "default",
                        size = "sm",
                        style = "material-flat",
                        icon = icon("up-right-and-down-left-from-center")
                      ),
                      title = "Sizing",
                      placement = "right",
                      options = list("delay': 400, 'foo" = "foo")
                    )
                  )
                )
              ),
              fluidRow(
                column(
                  width = 6,
                  align = "right",
                  br(),
                  div(
                    id = "gs-misc-menu",
                    tipify(
                      actionBttn(
                        "gs_misc_menu",
                        label = "",
                        color = "default",
                        size = "sm",
                        style = "material-flat",
                        icon = icon("ellipsis")
                      ),
                      title = "Other Settings",
                      placement = "left",
                      options = list("delay': 400, 'foo" = "foo")
                    )
                  )
                ),
                column(
                  width = 6,
                  align = "left",
                  br(),
                  div(
                    id = "gs-download-menu",
                    tipify(
                      actionBttn(
                        "gs_download_menu",
                        label = "",
                        color = "default",
                        size = "sm",
                        style = "material-flat",
                        icon = icon("download")
                      ),
                      title = "Save Plot",
                      placement = "right",
                      options = list("delay': 400, 'foo" = "foo")
                    )
                  )
                )
              )
            )
          ),
          br(),
          uiOutput("gs_plot_control_ui")
        ),
        column(
          width = 9,
          align = "left",
          br(),
          br(),
          br(),
          br(),
          uiOutput("gs_field")
        )
      )
    } else {
      NULL
    }
  })

  # gs plot control panel
  output$gs_plot_control_ui <- renderUI({
    req(
      Screening$available,
      DB$data,
      Screening$amr_results,
      Screening$vir_class,
      Screening$amr_class
    )
    amr_profile_numeric <- as.data.frame(lapply(
      Screening$amr_results,
      as.numeric
    ))
    rownames(amr_profile_numeric) <- rownames(Screening$amr_results)
    colnames(amr_profile_numeric) <- colnames(Screening$amr_results)
    heatmap_matrix <- as.matrix(amr_profile_numeric)

    if (nrow(Screening$amr_class) > 0) {
      amr_meta <- get.gsMeta(
        gene_class = Screening$amr_class,
        hm_matrix = heatmap_matrix
      )
      amr_classes <- amr_meta$class
      names(amr_classes) <- rownames(amr_meta)
      amr_classes_filtered <- na.omit(amr_classes)
      choices_amr <- list()
      for (i in 1:length(unique(amr_classes_filtered))) {
        group <- names(amr_classes_filtered)[which(
          amr_classes_filtered == unique(amr_classes_filtered)[i]
        )]
        if (length(group) == 1) {
          choices_amr[[unique(amr_classes_filtered)[i]]] <- as.list(group)
        } else {
          choices_amr[[unique(amr_classes_filtered)[i]]] <- group
        }
      }
      choices_amr <- choices_amr[order(names(choices_amr))]
    } else {
      choices_amr <- character(0)
      amr_meta <- data.frame(results = NA)
    }

    if (nrow(Screening$vir_class) > 0) {
      vir_meta <- get.gsMeta(
        gene_class = Screening$vir_class,
        hm_matrix = heatmap_matrix
      )
      vir_classes <- vir_meta$class
      names(vir_classes) <- rownames(amr_meta)
      vir_classes_filtered <- na.omit(vir_classes)
      choices_vir <- list()
      for (i in 1:length(unique(vir_classes_filtered))) {
        group <- names(vir_classes_filtered)[which(
          vir_classes_filtered == unique(vir_classes_filtered)[i]
        )]
        if (length(group) == 1) {
          choices_vir[[unique(vir_classes_filtered)[i]]] <- as.list(group)
        } else {
          choices_vir[[unique(vir_classes_filtered)[i]]] <- group
        }
      }
      choices_vir <- choices_vir[order(names(choices_vir))]
    } else {
      choices_vir <- character(0)
      vir_meta <- data.frame(results = NA)
    }

    if (all(!is.na(vir_meta$class)) && all(!is.na(amr_meta$class))) {
      choices_noclass <- character(0)
    } else {
      choices_noclass <- rownames(amr_meta)[which(
        is.na(vir_meta$class) &
          is.na(amr_meta$class)
      )]
    }

    div(
      class = "gs-plot-box2",
      box(
        solidHeader = TRUE,
        status = "primary",
        width = "100%",
        title = "Heatmap Data",
        column(
          width = 12,
          fluidRow(
            column(
              width = 12,
              align = "center",
              HTML(
                paste(
                  tags$span(
                    style = 'color: white; font-size: 16px; position: relative; top: 10px;',
                    'Isolates'
                  )
                )
              ),
              fluidRow(
                column(
                  width = 9,
                  div(
                    class = "gs-plot-selected-isolate",
                    pickerInput(
                      "gs_plot_selected_isolate",
                      label = "",
                      choices = list(
                        Screened = if (
                          length(DB$data$`Assembly ID`[which(
                            DB$data$Screened == "Yes"
                          )]) ==
                            1
                        ) {
                          as.list(DB$data$`Assembly ID`[which(
                            DB$data$Screened == "Yes"
                          )])
                        } else {
                          DB$data$`Assembly ID`[which(
                            DB$data$Screened == "Yes"
                          )]
                        },
                        Unscreened = if (
                          length(DB$data$`Assembly ID`[which(
                            DB$data$Screened == "No"
                          )]) ==
                            1
                        ) {
                          as.list(DB$data$`Assembly ID`[which(
                            DB$data$Screened == "No"
                          )])
                        } else {
                          DB$data$`Assembly ID`[which(
                            DB$data$Screened == "No"
                          )]
                        },
                        `No Assembly File` = if (
                          sum(
                            DB$data$Screened == "NA"
                          ) ==
                            1
                        ) {
                          as.list(DB$data$`Assembly ID`[which(
                            DB$data$Screened == "NA"
                          )])
                        } else {
                          DB$data$`Assembly ID`[which(
                            DB$data$Screened == "NA"
                          )]
                        }
                      ),
                      choicesOpt = list(
                        disabled = c(
                          rep(
                            FALSE,
                            length(DB$data$`Assembly ID`[which(
                              DB$data$Screened == "Yes"
                            )])
                          ),
                          rep(
                            TRUE,
                            length(DB$data$`Assembly ID`[which(
                              DB$data$Screened == "No"
                            )])
                          ),
                          rep(
                            TRUE,
                            length(DB$data$`Assembly ID`[which(
                              DB$data$Screened == "NA"
                            )])
                          )
                        )
                      ),
                      selected = DB$data$`Assembly ID`[which(
                        DB$data$Screened == "Yes"
                      )],
                      options = list(
                        `live-search` = TRUE,
                        `actions-box` = TRUE,
                        size = 10,
                        style = "background-color: white; border-radius: 5px;"
                      ),
                      multiple = TRUE,
                      width = "96%"
                    )
                  )
                ),
                column(
                  width = 3,
                  align = "center",
                  dropMenu(
                    actionBttn(
                      "gsplot_isolate_label_menu",
                      label = "",
                      color = "default",
                      size = "sm",
                      style = "material-flat",
                      icon = icon("sliders")
                    ),
                    placement = "right-start",
                    theme = "translucent",
                    fluidRow(
                      div(
                        class = "gsplot-isolate-label",
                        selectInput(
                          "gsplot_isolate_label",
                          label = h5("Isolate Label", style = "color:white;"),
                          choices = c("Assembly ID", "Assembly Name"),
                          selected = "Assembly Name",
                          width = "70%"
                        )
                      )
                    )
                  ),
                  bsTooltip(
                    "gsplot_isolate_label_menu",
                    title = "Label",
                    placement = "right"
                  )
                )
              ),
              uiOutput("gs_plot_sel_isolate_info")
            ),
            column(
              width = 12,
              align = "center",
              br(),
              div(
                class = "gs-heatmap-text1",
                HTML(
                  paste(
                    tags$span(
                      style = 'color: white; font-size: 16px; position: relative;',
                      'AMR Genes'
                    )
                  )
                )
              ),
              div(
                class = "gs-plot-selected-isolate",
                pickerInput(
                  "gs_plot_selected_amr",
                  label = "",
                  choices = choices_amr,
                  options = list(
                    `live-search` = TRUE,
                    `actions-box` = TRUE,
                    size = 10,
                    style = "background-color: white; border-radius: 5px;"
                  ),
                  selected = unlist(choices_amr),
                  multiple = TRUE,
                  width = "96%"
                )
              ),
              uiOutput("gs_plot_sel_amr_info")
            ),
            column(
              width = 12,
              align = "center",
              br(),
              div(
                class = "gs-heatmap-text",
                HTML(
                  paste(
                    tags$span(
                      style = 'color: white; font-size: 16px; position: relative;',
                      'Virulence Genes'
                    )
                  )
                )
              ),
              div(
                class = "gs-plot-selected-isolate",
                pickerInput(
                  "gs_plot_selected_vir",
                  label = "",
                  choices = choices_vir,
                  options = list(
                    `live-search` = TRUE,
                    `actions-box` = TRUE,
                    size = 10,
                    style = "background-color: white; border-radius: 5px;"
                  ),
                  selected = unlist(choices_vir),
                  multiple = TRUE,
                  width = "96%"
                )
              ),
              uiOutput("gs_plot_sel_vir_info")
            ),
            column(
              width = 12,
              align = "center",
              br(),
              div(
                class = "gs-heatmap-text",
                HTML(
                  paste(
                    tags$span(
                      style = 'color: white; font-size: 15px; position: relative;',
                      'Unclassifiable Genes'
                    )
                  )
                )
              ),
              div(
                class = "gs-plot-selected-isolate",
                pickerInput(
                  "gs_plot_selected_noclass",
                  label = "",
                  choices = choices_noclass,
                  options = list(
                    `live-search` = TRUE,
                    `actions-box` = TRUE,
                    size = 10,
                    style = "background-color: white; border-radius: 5px;"
                  ),
                  selected = unlist(choices_noclass),
                  multiple = TRUE,
                  width = "96%"
                )
              ),
              uiOutput("gs_plot_sel_noclass_info")
            )
          )
        )
      )
    )
  })

  # gs heatmap data menu
  input_gsplot_isolate_label <- reactive({
    input$gsplot_isolate_label
  }) %>%
    debounce(1000)

  observeEvent(input$gs_data_menu, {
    runjs(block_ui)

    if (!is.null(input_gsplot_isolate_label())) {
      gsplot_isolate_label_selected <- input_gsplot_isolate_label()
    } else {
      gsplot_isolate_label_selected <- "Assembly Name"
    }

    if (!is.null(gs_plot_selected_isolate())) {
      gs_plot_selected_isolate_selected <- gs_plot_selected_isolate()
    } else {
      gs_plot_selected_isolate_selected <- DB$data$`Assembly ID`[which(
        DB$data$Screened == "Yes"
      )]
    }

    amr_profile_numeric <- as.data.frame(lapply(
      Screening$amr_results,
      as.numeric
    ))
    rownames(amr_profile_numeric) <- rownames(Screening$amr_results)
    colnames(amr_profile_numeric) <- colnames(Screening$amr_results)
    heatmap_matrix <- as.matrix(amr_profile_numeric)

    if (!is.null(Screening$amr_class) && nrow(Screening$amr_class) > 0) {
      amr_meta <- get.gsMeta(
        gene_class = Screening$amr_class,
        hm_matrix = heatmap_matrix
      )
      choices_amr <- rownames(amr_meta)[which(!is.na(amr_meta$class))]
      amr_classes <- amr_meta$class
      names(amr_classes) <- rownames(amr_meta)
      amr_classes_filtered <- na.omit(amr_classes)
      choices_amr <- list()
      for (i in 1:length(unique(amr_classes_filtered))) {
        group <- names(amr_classes_filtered)[which(
          amr_classes_filtered == unique(amr_classes_filtered)[i]
        )]
        if (length(group) == 1) {
          choices_amr[[unique(amr_classes_filtered)[i]]] <- as.list(group)
        } else {
          choices_amr[[unique(amr_classes_filtered)[i]]] <- group
        }
      }
    } else {
      choices_amr <- character(0)
      amr_meta <- data.frame(results = NA)
    }

    gs_plot_selected_amr_selected <- gs_plot_selected_amr()

    if (!is.null(Screening$vir_class) && nrow(Screening$vir_class) > 0) {
      vir_meta <- get.gsMeta(
        gene_class = Screening$vir_class,
        hm_matrix = heatmap_matrix
      )
      choices_vir <- rownames(vir_meta)[which(!is.na(vir_meta$class))]
      vir_classes <- vir_meta$class
      names(vir_classes) <- rownames(amr_meta)
      vir_classes_filtered <- na.omit(vir_classes)
      choices_vir <- list()
      for (i in 1:length(unique(vir_classes_filtered))) {
        group <- names(vir_classes_filtered)[which(
          vir_classes_filtered == unique(vir_classes_filtered)[i]
        )]
        if (length(group) == 1) {
          choices_vir[[unique(vir_classes_filtered)[i]]] <- as.list(group)
        } else {
          choices_vir[[unique(vir_classes_filtered)[i]]] <- group
        }
      }
    } else {
      choices_vir <- character(0)
      vir_meta <- data.frame(results = NA)
    }

    gs_plot_selected_vir_selected <- gs_plot_selected_vir()

    if (all(!is.na(vir_meta$class)) && all(!is.na(amr_meta$class))) {
      choices_noclass <- character(0)
    } else {
      choices_noclass <- rownames(amr_meta)[which(
        is.na(vir_meta$class) &
          is.na(amr_meta$class)
      )]
    }

    gs_plot_selected_noclass_selected <- gs_plot_selected_noclass()

    output$gs_plot_control_ui <- renderUI(
      div(
        class = "gs-plot-box2",
        box(
          solidHeader = TRUE,
          status = "primary",
          width = "100%",
          title = "Heatmap Data",
          column(
            width = 12,
            fluidRow(
              column(
                width = 12,
                align = "center",
                HTML(
                  paste(
                    tags$span(
                      style = 'color: white; font-size: 16px; position: relative; top: 10px;',
                      'Isolates'
                    )
                  )
                ),
                fluidRow(
                  column(
                    width = 9,
                    div(
                      class = "gs-plot-selected-isolate",
                      pickerInput(
                        "gs_plot_selected_isolate",
                        label = "",
                        choices = list(
                          Screened = if (
                            length(DB$data$`Assembly ID`[which(
                              DB$data$Screened == "Yes"
                            )]) ==
                              1
                          ) {
                            as.list(DB$data$`Assembly ID`[which(
                              DB$data$Screened == "Yes"
                            )])
                          } else {
                            DB$data$`Assembly ID`[which(
                              DB$data$Screened == "Yes"
                            )]
                          },
                          Unscreened = if (
                            length(DB$data$`Assembly ID`[which(
                              DB$data$Screened == "No"
                            )]) ==
                              1
                          ) {
                            as.list(DB$data$`Assembly ID`[which(
                              DB$data$Screened == "No"
                            )])
                          } else {
                            DB$data$`Assembly ID`[which(
                              DB$data$Screened == "No"
                            )]
                          },
                          `No Assembly File` = if (
                            sum(
                              DB$data$Screened == "NA"
                            ) ==
                              1
                          ) {
                            as.list(DB$data$`Assembly ID`[which(
                              DB$data$Screened == "NA"
                            )])
                          } else {
                            DB$data$`Assembly ID`[which(
                              DB$data$Screened == "NA"
                            )]
                          }
                        ),
                        choicesOpt = list(
                          disabled = c(
                            rep(
                              FALSE,
                              length(DB$data$`Assembly ID`[which(
                                DB$data$Screened == "Yes"
                              )])
                            ),
                            rep(
                              TRUE,
                              length(DB$data$`Assembly ID`[which(
                                DB$data$Screened == "No"
                              )])
                            ),
                            rep(
                              TRUE,
                              length(DB$data$`Assembly ID`[which(
                                DB$data$Screened == "NA"
                              )])
                            )
                          )
                        ),
                        selected = gs_plot_selected_isolate_selected,
                        options = list(
                          `live-search` = TRUE,
                          `actions-box` = TRUE,
                          size = 10,
                          style = "background-color: white; border-radius: 5px;"
                        ),
                        multiple = TRUE,
                        width = "96%"
                      )
                    )
                  ),
                  column(
                    width = 3,
                    align = "center",
                    dropMenu(
                      actionBttn(
                        "gsplot_isolate_label_menu",
                        label = "",
                        color = "default",
                        size = "sm",
                        style = "material-flat",
                        icon = icon("sliders")
                      ),
                      placement = "right-start",
                      theme = "translucent",
                      fluidRow(
                        div(
                          class = "gsplot-isolate-label",
                          selectInput(
                            "gsplot_isolate_label",
                            label = h5("Isolate Label", style = "color:white;"),
                            choices = c("Assembly ID", "Assembly Name"),
                            selected = gsplot_isolate_label_selected,
                            width = "70%"
                          )
                        )
                      )
                    )
                  )
                ),
                uiOutput("gs_plot_sel_isolate_info")
              ),
              column(
                width = 12,
                align = "center",
                br(),
                div(
                  class = "gs-heatmap-text1",
                  HTML(
                    paste(
                      tags$span(
                        style = 'color: white; font-size: 16px; position: relative;',
                        'AMR Genes'
                      )
                    )
                  )
                ),
                div(
                  class = "gs-plot-selected-isolate",
                  pickerInput(
                    "gs_plot_selected_amr",
                    label = "",
                    choices = choices_amr,
                    options = list(
                      `live-search` = TRUE,
                      `actions-box` = TRUE,
                      size = 10,
                      style = "background-color: white; border-radius: 5px;"
                    ),
                    selected = gs_plot_selected_amr_selected,
                    multiple = TRUE,
                    width = "96%"
                  )
                ),
                uiOutput("gs_plot_sel_amr_info")
              ),
              column(
                width = 12,
                align = "center",
                br(),
                div(
                  class = "gs-heatmap-text",
                  HTML(
                    paste(
                      tags$span(
                        style = 'color: white; font-size: 16px; position: relative;',
                        'Virulence Genes'
                      )
                    )
                  )
                ),
                div(
                  class = "gs-plot-selected-isolate",
                  pickerInput(
                    "gs_plot_selected_vir",
                    label = "",
                    choices = choices_vir,
                    options = list(
                      `live-search` = TRUE,
                      `actions-box` = TRUE,
                      size = 10,
                      style = "background-color: white; border-radius: 5px;"
                    ),
                    selected = gs_plot_selected_vir_selected,
                    multiple = TRUE,
                    width = "96%"
                  )
                ),
                uiOutput("gs_plot_sel_vir_info")
              ),
              column(
                width = 12,
                align = "center",
                br(),
                div(
                  class = "gs-heatmap-text",
                  HTML(
                    paste(
                      tags$span(
                        style = 'color: white; font-size: 15px; position: relative;',
                        'Unclassifiable Genes'
                      )
                    )
                  )
                ),
                div(
                  class = "gs-plot-selected-isolate",
                  pickerInput(
                    "gs_plot_selected_noclass",
                    label = "",
                    choices = choices_noclass,
                    options = list(
                      `live-search` = TRUE,
                      `actions-box` = TRUE,
                      size = 10,
                      style = "background-color: white; border-radius: 5px;"
                    ),
                    selected = gs_plot_selected_noclass_selected,
                    multiple = TRUE,
                    width = "96%"
                  )
                ),
                uiOutput("gs_plot_sel_noclass_info")
              )
            )
          )
        )
      )
    )

    runjs(unblock_ui)
  })

  # color menu
  observeEvent(input$gs_color_menu, {
    runjs(block_ui)

    if (!is.null(gsplot_color_text())) {
      gsplot_color_text_selected <- gsplot_color_text()
    } else {
      gsplot_color_text_selected <- "#000000"
    }

    if (!is.null(gsplot_color_dend())) {
      gsplot_color_dend_selected <- gsplot_color_dend()
    } else {
      gsplot_color_dend_selected <- "#000000"
    }

    if (!is.null(gsplot_color_palette1())) {
      gsplot_color_palette1_selected <- gsplot_color_palette1()
    } else {
      gsplot_color_palette1_selected <- "#66C2A5"
    }

    if (!is.null(gsplot_color_palette2())) {
      gsplot_color_palette2_selected <- gsplot_color_palette2()
    } else {
      gsplot_color_palette2_selected <- "#E5C494"
    }

    if (!is.null(gsplot_grid_color())) {
      gsplot_grid_color_selected <- gsplot_grid_color()
    } else {
      gsplot_grid_color_selected <- "#ffffff"
    }

    if (!is.null(gsplot_background())) {
      gsplot_background_selected <- gsplot_background()
    } else {
      gsplot_background_selected <- "#ffffff"
    }

    output$gs_plot_control_ui <- renderUI({
      req(Screening$available)

      div(
        class = "gs-plot-box3",
        box(
          solidHeader = TRUE,
          status = "primary",
          width = "100%",
          title = "Color Menu",
          column(
            width = 12,
            br(),
            fluidRow(
              column(
                width = 6,
                align = "left",
                HTML(
                  paste(
                    tags$span(
                      style = 'color: white; font-size: 14px; position: relative; top: 10px;',
                      'Text'
                    )
                  )
                )
              ),
              column(
                width = 6,
                align = "center",
                colorPickr(
                  inputId = "gsplot_color_text",
                  selected = gsplot_color_text_selected,
                  label = "",
                  update = "changestop",
                  interaction = list(clear = FALSE, save = FALSE),
                  position = "right-start",
                  width = "120px"
                )
              )
            ),
            br(),
            fluidRow(
              column(
                width = 6,
                align = "left",
                HTML(
                  paste(
                    tags$span(
                      style = 'color: white; font-size: 14px; position: relative; top: 10px;',
                      'Dendrogram'
                    )
                  )
                )
              ),
              column(
                width = 6,
                align = "center",
                colorPickr(
                  inputId = "gsplot_color_dend",
                  selected = gsplot_color_dend_selected,
                  label = "",
                  update = "changestop",
                  interaction = list(clear = FALSE, save = FALSE),
                  position = "right-start",
                  width = "120px"
                )
              )
            ),
            br(),
            fluidRow(
              column(
                width = 6,
                align = "left",
                HTML(
                  paste(
                    tags$span(
                      style = 'color: white; font-size: 14px; position: relative; top: 10px;',
                      'Present'
                    )
                  )
                )
              ),
              column(
                width = 6,
                align = "center",
                colorPickr(
                  inputId = "gsplot_color_palette1",
                  selected = gsplot_color_palette1_selected,
                  label = "",
                  update = "changestop",
                  interaction = list(clear = FALSE, save = FALSE),
                  position = "right-start",
                  width = "120px"
                )
              )
            ),
            br(),
            fluidRow(
              column(
                width = 6,
                align = "left",
                HTML(
                  paste(
                    tags$span(
                      style = 'color: white; font-size: 14px; position: relative; top: 10px;',
                      'Absent'
                    )
                  )
                )
              ),
              column(
                width = 6,
                align = "center",
                colorPickr(
                  inputId = "gsplot_color_palette2",
                  selected = gsplot_color_palette2_selected,
                  label = "",
                  update = "changestop",
                  interaction = list(clear = FALSE, save = FALSE),
                  position = "right-start",
                  width = "120px"
                )
              )
            ),
            br(),
            fluidRow(
              column(
                width = 6,
                align = "left",
                HTML(
                  paste(
                    tags$span(
                      style = 'color: white; font-size: 14px; position: relative; top: 10px;',
                      'Grid Color'
                    )
                  )
                )
              ),
              column(
                width = 6,
                align = "center",
                colorPickr(
                  inputId = "gsplot_grid_color",
                  selected = gsplot_grid_color_selected,
                  label = "",
                  update = "changestop",
                  interaction = list(clear = FALSE, save = FALSE),
                  position = "right-start",
                  width = "120px"
                )
              )
            ),
            br(),
            fluidRow(
              column(
                width = 6,
                align = "left",
                HTML(
                  paste(
                    tags$span(
                      style = 'color: white; font-size: 14px; position: relative; top: 10px;',
                      'Background'
                    )
                  )
                )
              ),
              column(
                width = 6,
                align = "center",
                colorPickr(
                  inputId = "gsplot_background",
                  selected = gsplot_background_selected,
                  label = "",
                  update = "changestop",
                  interaction = list(clear = FALSE, save = FALSE),
                  position = "right-start",
                  width = "120px"
                ),
                br()
              )
            )
          )
        )
      )
    })

    runjs(unblock_ui)
  })

  # variable mapping menu
  input_gs_amr_variables <- reactive({
    input$gs_amr_variables
  }) %>%
    debounce(1000)
  input_gs_vir_variables <- reactive({
    input$gs_vir_variables
  }) %>%
    debounce(1000)

  observeEvent(input$gs_variable_menu, {
    runjs(block_ui)

    output$gs_plot_control_ui <- renderUI({
      req(Screening$available)

      if (!is.null(input_gs_amr_variables())) {
        gs_amr_variables_selected <- input_gs_amr_variables()
      } else {
        gs_amr_variables_selected <- "Classification"
      }

      if (!is.null(input_gs_vir_variables())) {
        gs_vir_variables_selected <- input_gs_vir_variables()
      } else {
        gs_vir_variables_selected <- "Classification"
      }

      delay(
        0,
        runjs(
          gsub("#col_scale_id", "#gs_virclass_scale", color_scale_bg_JS)
        )
      )

      delay(
        0,
        runjs(
          gsub("#col_scale_id", "#gs_amrclass_scale", color_scale_bg_JS)
        )
      )

      delay(
        0,
        runjs(
          gsub("#col_scale_id", "#gs_mapping_scale", color_scale_bg_JS)
        )
      )

      div(
        class = "gs-plot-box2",
        box(
          solidHeader = TRUE,
          status = "primary",
          width = "100%",
          title = "Variable Mapping",
          column(
            width = 12,
            fluidRow(
              column(
                width = 12,
                align = "center",
                HTML(
                  paste(
                    tags$span(
                      style = 'color: white; font-size: 16px; position: relative; top: 10px',
                      'Isolate Variables'
                    )
                  )
                )
              )
            ),
            fluidRow(
              column(
                width = 4,
                align = "left",
                HTML(
                  paste(
                    tags$span(
                      style = 'color: white; font-size: 14px; position: relative; left: 10px; top: 37px;',
                      'Variable'
                    )
                  )
                )
              ),
              column(
                width = 8,
                align = "center",
                uiOutput("gs_var_mapping_ui"),
                br()
              )
            ),
            fluidRow(
              column(
                width = 4,
                align = "left",
                HTML(
                  paste(
                    tags$span(
                      style = 'color: white; font-size: 14px; position: relative; left: 10px; top: 9px;',
                      'Scale'
                    )
                  )
                )
              ),
              column(
                width = 8,
                align = "center",
                uiOutput("gs_mapping_scale_ui"),
                br()
              )
            ),
            fluidRow(
              column(
                width = 12,
                align = "center",
                HTML(
                  paste(
                    tags$span(
                      style = 'color: white; font-size: 16px; position: relative;',
                      'AMR Genes'
                    )
                  )
                )
              )
            ),
            fluidRow(
              column(
                width = 4,
                align = "left",
                HTML(
                  paste(
                    tags$span(
                      style = 'color: white; font-size: 14px; position: relative; left: 10px; top: 27px;',
                      'Variable'
                    )
                  )
                )
              ),
              column(
                width = 8,
                align = "center",
                div(
                  class = "gs-var-mapping-sel",
                  selectInput(
                    "gs_amr_variables",
                    "",
                    choices = c("Classification", "None"),
                    selected = gs_amr_variables_selected,
                    width = "100%"
                  )
                ),
                br()
              )
            ),
            fluidRow(
              column(
                width = 4,
                align = "left",
                HTML(
                  paste(
                    tags$span(
                      style = 'color: white; font-size: 14px; position: relative; left: 10px; top: 9px;',
                      'Scale'
                    )
                  )
                )
              ),
              column(
                width = 8,
                align = "center",
                uiOutput("gs_amrclass_scale_ui")
              )
            ),
            fluidRow(
              column(
                width = 12,
                align = "center",
                HTML(
                  paste(
                    tags$span(
                      style = 'color: white; font-size: 16px; position: relative;',
                      'Virulence Genes'
                    )
                  )
                )
              )
            ),
            fluidRow(
              column(
                width = 4,
                align = "left",
                HTML(
                  paste(
                    tags$span(
                      style = 'color: white; font-size: 14px; position: relative; left: 10px; top: 27px;',
                      'Variable'
                    )
                  )
                )
              ),
              column(
                width = 8,
                align = "center",
                div(
                  class = "gs-var-mapping-sel",
                  selectInput(
                    "gs_vir_variables",
                    "",
                    choices = c("Classification", "None"),
                    selected = gs_vir_variables_selected,
                    width = "100%"
                  )
                ),
                br()
              )
            ),
            fluidRow(
              column(
                width = 4,
                align = "left",
                HTML(
                  paste(
                    tags$span(
                      style = 'color: white; font-size: 14px; position: relative; left: 10px; top: 9px;',
                      'Scale'
                    )
                  )
                )
              ),
              column(
                width = 8,
                align = "center",
                uiOutput("gs_virclass_scale_ui")
              )
            )
          )
        )
      )
    })

    runjs(unblock_ui)
  })

  # sizing menu
  input_gsplot_grid_width <- reactive({
    input$gsplot_grid_width
  }) %>%
    debounce(1000)
  input_gsplot_legend_labelsize <- reactive({
    input$gsplot_legend_labelsize
  }) %>%
    debounce(1000)
  input_gsplot_fontsize_title <- reactive({
    input$gsplot_fontsize_title
  }) %>%
    debounce(1000)
  input_gsplot_treeheight_col <- reactive({
    input$gsplot_treeheight_col
  }) %>%
    debounce(1000)
  input_gsplot_treeheight_row <- reactive({
    input$gsplot_treeheight_row
  }) %>%
    debounce(1000)

  observeEvent(input$gs_size_menu, {
    req(Screening$available)
    runjs(block_ui)

    output$gs_plot_control_ui <- renderUI({
      if (!is.null(input_gsplot_grid_width())) {
        gsplot_grid_width_selected <- input_gsplot_grid_width()
      } else {
        gsplot_grid_width_selected <- 1
      }

      if (!is.null(input_gsplot_legend_labelsize())) {
        gsplot_legend_labelsize_selected <- input_gsplot_legend_labelsize()
      } else {
        gsplot_legend_labelsize_selected <- 9
      }

      if (!is.null(input_gsplot_fontsize_title())) {
        gsplot_fontsize_title_selected <- input_gsplot_fontsize_title()
      } else {
        gsplot_fontsize_title_selected <- 16
      }

      if (!is.null(input_gsplot_treeheight_col())) {
        gsplot_treeheight_col_selected <- input_gsplot_treeheight_col()
      } else {
        gsplot_treeheight_col_selected <- 2
      }

      if (!is.null(input_gsplot_treeheight_row())) {
        gsplot_treeheight_row_selected <- input_gsplot_treeheight_row()
      } else {
        gsplot_treeheight_row_selected <- 2
      }

      div(
        class = "gs-plot-box3",
        box(
          solidHeader = TRUE,
          status = "primary",
          width = "100%",
          title = "Sizing Menu",
          column(
            width = 12,
            br(),
            fluidRow(
              column(
                width = 6,
                align = "left",
                HTML(
                  paste(
                    tags$span(
                      style = 'color: white; font-size: 14px; position: relative; top: 12px;',
                      'Legend'
                    )
                  )
                )
              ),
              column(
                width = 6,
                div(
                  class = "gs-slider",
                  sliderInput(
                    "gsplot_legend_labelsize",
                    "",
                    value = gsplot_legend_labelsize_selected,
                    max = 14,
                    min = 6,
                    ticks = FALSE
                  )
                )
              )
            ),
            fluidRow(
              column(
                width = 6,
                align = "left",
                HTML(
                  paste(
                    tags$span(
                      style = 'color: white; font-size: 14px; position: relative; top: 12px;',
                      'Grid Width'
                    )
                  )
                )
              ),
              column(
                width = 6,
                div(
                  class = "gs-slider",
                  sliderInput(
                    "gsplot_grid_width",
                    "",
                    value = gsplot_grid_width_selected,
                    max = 3,
                    min = 0,
                    ticks = FALSE
                  )
                )
              )
            ),
            fluidRow(
              column(
                width = 6,
                align = "left",
                HTML(
                  paste(
                    tags$span(
                      style = 'color: white; font-size: 12px; position: relative; top: 12px;',
                      'Row/Col Titles'
                    )
                  )
                )
              ),
              column(
                width = 6,
                div(
                  class = "gs-slider",
                  sliderInput(
                    "gsplot_fontsize_title",
                    "",
                    value = gsplot_fontsize_title_selected,
                    max = 24,
                    min = 8,
                    ticks = FALSE
                  )
                )
              )
            ),
            fluidRow(
              column(
                width = 6,
                align = "left",
                HTML(
                  paste(
                    tags$span(
                      style = 'color: white; font-size: 14px; position: relative; top: 12px;',
                      'Dendro Cols'
                    )
                  )
                )
              ),
              column(
                width = 6,
                div(
                  class = "gs-slider",
                  sliderInput(
                    "gsplot_treeheight_col",
                    "",
                    value = gsplot_treeheight_col_selected,
                    max = 10,
                    min = 1,
                    ticks = FALSE
                  )
                )
              )
            ),
            fluidRow(
              column(
                width = 6,
                align = "left",
                HTML(
                  paste(
                    tags$span(
                      style = 'color: white; font-size: 13px; position: relative; top: 12px;',
                      'Dendro Rows'
                    )
                  )
                )
              ),
              column(
                width = 6,
                div(
                  class = "gs-slider",
                  sliderInput(
                    "gsplot_treeheight_row",
                    "",
                    value = gsplot_treeheight_row_selected,
                    max = 10,
                    min = 1,
                    ticks = FALSE
                  )
                ),
                br()
              )
            )
          )
        )
      )
    })

    runjs(unblock_ui)
  })

  # download menu
  observeEvent(input$gs_download_menu, {
    runjs(block_ui)

    output$gs_plot_control_ui <- renderUI({
      req(Screening$available)

      div(
        class = "gs-plot-box4",
        box(
          solidHeader = TRUE,
          status = "primary",
          width = "100%",
          title = "Save Heatmap",
          column(
            width = 12,
            align = "center",
            br(),
            fluidRow(
              column(
                width = 6,
                align = "left",
                HTML(
                  paste(
                    tags$span(
                      style = 'color: white; font-size: 14px; position: relative; left: 10px;',
                      'Save As'
                    )
                  )
                )
              ),
              column(
                width = 6,
                align = "center",
                div(
                  class = "filetype-gs",
                  selectInput(
                    inputId = "filetype_gs",
                    label = "",
                    choices = c("png", "jpeg", "bmp", "svg")
                  )
                ),
                br(),
                br()
              )
            ),
            downloadBttn(
              "gs_download_plot",
              style = "simple",
              label = "",
              size = "sm",
              icon = icon("download"),
              color = "primary"
            ),
            br(),
            br()
          )
        )
      )
    })

    runjs(unblock_ui)
  })

  input_gs_ratio <- reactive({
    input$gs_ratio
  }) %>%
    debounce(1000)
  input_gs_scale <- reactive({
    input$gs_scale
  }) %>%
    debounce(1000)
  input_gs_cluster_row <- reactive({
    input$gs_cluster_row
  }) %>%
    debounce(1000)
  input_gs_cluster_col <- reactive({
    input$gs_cluster_col
  }) %>%
    debounce(1000)
  input_gs_cluster_distance_col <- reactive({
    input$gs_cluster_distance_col
  }) %>%
    debounce(1000)
  input_gs_cluster_distance_row <- reactive({
    input$gs_cluster_distance_row
  }) %>%
    debounce(1000)
  input_gs_cluster_method_col <- reactive({
    input$gs_cluster_method_col
  }) %>%
    debounce(1000)
  input_gs_cluster_method_row <- reactive({
    input$gs_cluster_method_row
  }) %>%
    debounce(1000)

  # miscellaneous menu
  observeEvent(input$gs_misc_menu, {
    runjs(block_ui)

    output$gs_plot_control_ui <- renderUI({
      req(Screening$available)

      if (!is.null(input_gs_ratio())) {
        gs_ratio_selected <- input_gs_ratio()
        if (input_gs_ratio() == "1.6") {
          min <- 500
          max <- 1200
          step <- 5
        } else if (input_gs_ratio() == "1.77777777777778") {
          min <- 504
          max <- 1197
          step <- 9
        } else if (input_gs_ratio() == "1.33333333333333") {
          min <- 501
          max <- 1200
          step <- 3
        }
      } else {
        gs_ratio_selected <- c("16:9" = (16 / 9))
        min <- 504
        max <- 1197
        step <- 9
      }

      if (!is.null(input_gs_scale())) {
        gs_scale_selected <- input_gs_scale()
      } else {
        gs_scale_selected <- 702
      }

      if (!is.null(input_gs_cluster_col())) {
        gs_cluster_col_selected <- input_gs_cluster_col()
      } else {
        gs_cluster_col_selected <- TRUE
      }

      if (!is.null(input_gs_cluster_row())) {
        gs_cluster_row_selected <- input_gs_cluster_row()
      } else {
        gs_cluster_row_selected <- TRUE
      }

      if (!is.null(input_gs_cluster_distance_col())) {
        gs_cluster_distance_col_selected <- input_gs_cluster_distance_col()
      } else {
        gs_cluster_distance_col_selected <- "binary"
      }

      if (!is.null(input_gs_cluster_distance_row())) {
        gs_cluster_distance_row_selected <- input_gs_cluster_distance_row()
      } else {
        gs_cluster_distance_row_selected <- "binary"
      }

      if (!is.null(input_gs_cluster_method_col())) {
        gs_cluster_method_col_selected <- input_gs_cluster_method_col()
      } else {
        gs_cluster_method_col_selected <- "average"
      }

      if (!is.null(input_gs_cluster_method_row())) {
        gs_cluster_method_row_selected <- input_gs_cluster_method_row()
      } else {
        gs_cluster_method_row_selected <- "average"
      }

      div(
        class = "gs-plot-box2",
        box(
          solidHeader = TRUE,
          status = "primary",
          width = "100%",
          title = "Miscellaneous",
          column(
            width = 12,
            br(),
            fluidRow(
              column(
                width = 6,
                align = "left",
                HTML(
                  paste(
                    tags$span(
                      style = 'color: white; font-size: 14px; position: relative;',
                      'Ratio'
                    )
                  )
                )
              ),
              column(
                width = 6,
                align = "center",
                div(
                  class = "gs-ratio",
                  selectInput(
                    "gs_ratio",
                    "",
                    choices = c(
                      "16:10" = (16 / 10),
                      "16:9" = (16 / 9),
                      "4:3" = (4 / 3)
                    ),
                    selected = gs_ratio_selected
                  )
                )
              )
            ),
            br(),
            fluidRow(
              column(
                width = 6,
                align = "left",
                HTML(
                  paste(
                    tags$span(
                      style = 'color: white; font-size: 14px; position: relative; top: 7px;',
                      'Scale'
                    )
                  )
                )
              ),
              div(
                class = "gs-slider-col",
                column(
                  width = 6,
                  align = "center",
                  div(
                    class = "gs-slider-scale",
                    sliderInput(
                      "gs_scale",
                      "",
                      min = min,
                      max = max,
                      value = gs_scale_selected,
                      step = step,
                      width = "95%",
                      ticks = FALSE
                    )
                  )
                )
              )
            ),
            hr(),
            fluidRow(
              column(
                width = 8,
                align = "left",
                HTML(
                  paste(
                    tags$span(
                      style = 'color: white; font-size: 15px; position: relative; top: 5px;',
                      'Cluster Genes'
                    )
                  )
                )
              ),
              column(
                width = 4,
                align = "left",
                div(
                  class = "mat-switch-gs-settings",
                  materialSwitch(
                    "gs_cluster_col",
                    "",
                    value = gs_cluster_col_selected,
                    right = TRUE
                  )
                )
              )
            ),
            fluidRow(
              column(
                width = 6,
                align = "left",
                br(),
                HTML(
                  paste(
                    tags$span(
                      style = 'color: white; font-size: 12px; position: relative; top: -12px;',
                      'Distance Algorithm'
                    )
                  )
                )
              ),
              column(
                width = 6,
                align = "center",
                br(),
                div(
                  class = "gs-cluster-sel",
                  selectInput(
                    "gs_cluster_distance_col",
                    "",
                    choices = c(
                      "Binary" = "binary",
                      "Hamming" = "hamming",
                      "MCC" = "mcc"
                    ),
                    selected = gs_cluster_distance_col_selected
                  )
                )
              )
            ),
            fluidRow(
              column(
                width = 6,
                align = "left",
                br(),
                HTML(
                  paste(
                    tags$span(
                      style = 'color: white; font-size: 12px; position: relative; top: -12px;',
                      'Agglom. Method'
                    )
                  )
                )
              ),
              column(
                width = 6,
                align = "center",
                br(),
                div(
                  class = "gs-cluster-sel",
                  selectInput(
                    "gs_cluster_method_col",
                    "",
                    choices = c(
                      "ward.D" = "ward.D",
                      "ward.D2" = "ward.D2",
                      "Single" = "single",
                      "Complete" = "complete",
                      "UPGMA" = "average",
                      "WPGMA" = "mcquitty",
                      "WPGMC" = "median",
                      "UPGMC" = "centroid"
                    ),
                    selected = gs_cluster_method_col_selected
                  )
                )
              )
            ),
            hr(),
            fluidRow(
              column(
                width = 8,
                align = "left",
                HTML(
                  paste(
                    tags$span(
                      style = 'color: white; font-size: 15px; position: relative; top: 4px;',
                      'Cluster Isolates'
                    )
                  )
                )
              ),
              column(
                width = 4,
                align = "left",
                div(
                  class = "mat-switch-gs-settings",
                  materialSwitch(
                    "gs_cluster_row",
                    "",
                    value = gs_cluster_row_selected,
                    right = TRUE
                  )
                )
              )
            ),
            fluidRow(
              column(
                width = 6,
                align = "left",
                br(),
                HTML(
                  paste(
                    tags$span(
                      style = 'color: white; font-size: 12px; position: relative; top: -12px;',
                      'Distance Algorithm'
                    )
                  )
                )
              ),
              column(
                width = 6,
                align = "center",
                br(),
                div(
                  class = "gs-cluster-sel",
                  selectInput(
                    "gs_cluster_distance_row",
                    "",
                    choices = c(
                      "Binary" = "binary",
                      "Hamming" = "hamming",
                      "MCC" = "mcc"
                    ),
                    selected = gs_cluster_distance_row_selected
                  )
                )
              )
            ),
            fluidRow(
              column(
                width = 6,
                align = "left",
                br(),
                HTML(
                  paste(
                    tags$span(
                      style = 'color: white; font-size: 12px; position: relative; top: -12px;',
                      'Agglom. Method'
                    )
                  )
                )
              ),
              column(
                width = 6,
                align = "center",
                br(),
                div(
                  class = "gs-cluster-sel",
                  selectInput(
                    "gs_cluster_method_row",
                    "",
                    selectize = TRUE,
                    choices = c(
                      "ward.D" = "ward.D",
                      "ward.D2" = "ward.D2",
                      "Single" = "single",
                      "Complete" = "complete",
                      "UPGMA" = "average",
                      "WPGMA" = "mcquitty",
                      "WPGMC" = "median",
                      "UPGMC" = "centroid"
                    ),
                    selected = gs_cluster_method_row_selected
                  )
                )
              )
            )
          )
        )
      )
    })

    runjs(unblock_ui)
  })

  # gs classification scale
  output$gs_virclass_scale_ui <- renderUI({
    req(Screening$available)

    if (!is.null(Screening$hm_meta)) {
      if (length(unique(Screening$hm_meta$vir)) > 7) {
        if (!is.null(gs_virclass_scale())) {
          gs_virclass_scale_selected <- gs_virclass_scale()
        } else {
          gs_virclass_scale_selected <- "#turbo"
        }

        gs_virclass_scale <- selectInput(
          "gs_virclass_scale",
          "",
          choices = list(
            Gradient = gradient_scales
          ),
          selected = gs_virclass_scale_selected,
          width = "100%"
        )
      } else {
        if (!is.null(gs_virclass_scale())) {
          gs_virclass_scale_selected <- gs_virclass_scale()
        } else {
          gs_virclass_scale_selected <- "Set1"
        }

        gs_virclass_scale <- selectInput(
          "gs_virclass_scale",
          "",
          choices = list(
            Qualitative = qualitative_scales,
            Gradient = gradient_scales
          ),
          selected = gs_virclass_scale_selected,
          width = "100%"
        )
      }
    } else {
      if (!is.null(gs_virclass_scale())) {
        gs_virclass_scale_selected <- gs_virclass_scale()
      } else {
        gs_virclass_scale_selected <- "turbo"
      }

      gs_virclass_scale <- selectInput(
        "gs_virclass_scale",
        "",
        choices = list(
          Gradient = gradient_scales
        ),
        selected = gs_virclass_scale_selected,
        width = "100%"
      )
    }

    delay(
      0,
      runjs(
        gsub("#col_scale_id", "#gs_virclass_scale", color_scale_bg_JS)
      )
    )

    div(class = "gs-vir-class-scale", gs_virclass_scale)
  })

  output$gs_amrclass_scale_ui <- renderUI({
    req(Screening$available)

    if (!is.null(Screening$hm_meta)) {
      if (length(unique(Screening$hm_meta$amr)) > 7) {
        if (!is.null(gs_amrclass_scale())) {
          gs_amrclass_scale_selected <- gs_amrclass_scale()
        } else {
          gs_amrclass_scale_selected <- "turbo"
        }

        gs_amrclass_scale <- selectInput(
          "gs_amrclass_scale",
          "",
          choices = list(
            Gradient = gradient_scales
          ),
          selected = gs_amrclass_scale_selected,
          width = "100%"
        )
      } else {
        if (!is.null(gs_amrclass_scale())) {
          gs_amrclass_scale_selected <- gs_amrclass_scale()
        } else {
          gs_amrclass_scale_selected <- "Set1"
        }

        gs_amrclass_scale <- selectInput(
          "gs_amrclass_scale",
          "",
          choices = list(
            Qualitative = qualitative_scales,
            Gradient = gradient_scales
          ),
          selected = gs_amrclass_scale_selected,
          width = "100%"
        )
      }
    } else {
      if (!is.null(gs_amrclass_scale())) {
        gs_amrclass_scale_selected <- gs_amrclass_scale()
      } else {
        gs_amrclass_scale_selected <- "turbo"
      }

      gs_amrclass_scale <- selectInput(
        "gs_amrclass_scale",
        "",
        choices = list(
          Gradient = gradient_scales,
        ),
        selected = gs_amrclass_scale_selected,
        width = "100%"
      )
    }

    delay(
      0,
      runjs(
        gsub("#col_scale_id", "#gs_amrclass_scale", color_scale_bg_JS)
      )
    )

    div(class = "gs-amr-class-scale", gs_amrclass_scale)
  })

  # gs variable mapping scale
  output$gs_mapping_scale_ui <- renderUI({
    req(input$gs_var_mapping, Screening$available)

    if (input$gs_var_mapping != "None") {
      if (class(unlist(DB$meta[, input$gs_var_mapping])) == "numeric") {
        if (!is.null(gs_mapping_scale())) {
          gs_mapping_scale_selected <- gs_mapping_scale()
        } else {
          gs_mapping_scale_selected <- "magma"
        }

        gs_mapping_scale <- selectInput(
          "gs_mapping_scale",
          "",
          choices = list(
            Continous = gradient_scales,
            Diverging = diverging_scales
          ),
          width = "100%",
          selected = gs_mapping_scale_selected
        )
      } else {
        if (length(unique(unlist(DB$meta[input$gs_var_mapping]))) > 7) {
          if (!is.null(gs_mapping_scale())) {
            gs_mapping_scale_selected <- gs_mapping_scale()
          } else {
            gs_mapping_scale_selected <- "magma"
          }

          gs_mapping_scale <- selectInput(
            "gs_mapping_scale",
            "",
            choices = list(
              Gradient = gradient_scales
            ),
            selected = gs_mapping_scale_selected,
            width = "100%"
          )
        } else {
          if (!is.null(gs_mapping_scale())) {
            gs_mapping_scale_selected <- gs_mapping_scale()
          } else {
            gs_mapping_scale_selected <- "Set1"
          }

          gs_mapping_scale <- selectInput(
            "gs_mapping_scale",
            "",
            choices = list(
              Qualitative = qualitative_scales,
              Sequential = sequential_scales
            ),
            selected = gs_mapping_scale_selected,
            width = "100%"
          )
        }
      }
    } else {
      if (!is.null(gs_mapping_scale())) {
        gs_mapping_scale_selected <- gs_mapping_scale()
      } else {
        gs_mapping_scale_selected <- "Set1"
      }

      gs_mapping_scale <- selectInput(
        "gs_mapping_scale",
        "",
        choices = list(
          Qualitative = qualitative_scales,
          Sequential = sequential_scales
        ),
        selected = gs_mapping_scale_selected,
        width = "100%"
      )
    }

    delay(
      0,
      runjs(
        gsub("#col_scale_id", "#gs_mapping_scale", color_scale_bg_JS)
      )
    )

    div(class = "gs-mapping-scale", gs_mapping_scale)
  })

  # variable mapping
  input_gs_var_mapping <- reactive({
    input$gs_var_mapping
  }) %>%
    debounce(1000)

  output$gs_var_mapping_ui <- renderUI({
    req(DB$meta, Screening$available)

    if (!is.null(input_gs_var_mapping())) {
      gs_var_mapping_selected <- input_gs_var_mapping()
    } else {
      gs_var_mapping_selected <- "None"
    }

    div(
      class = "gs-var-mapping-sel",
      selectInput(
        "gs_var_mapping",
        "",
        choices = c("None", names(DB$meta)[-c(1:4, 6, 11:14)]),
        width = "100%",
        selected = gs_var_mapping_selected
      )
    )
  })

  output$gs_field <- renderUI({
    req(Screening$available)

    if (!is.null(input$gs_scale)) {
      gs_scale <- input$gs_scale
    } else {
      gs_scale <- 702
    }
    if (!is.null(input$gs_ratio)) {
      gs_ratio <- input$gs_ratio
    } else {
      gs_ratio <- c("16:9" = (16 / 9))
    }

    addSpinner(
      plotOutput(
        "gs_plot",
        width = paste0(
          as.character(as.numeric(gs_scale) * as.numeric(gs_ratio)),
          "px"
        ),
        height = paste0(as.character(gs_scale), "px")
      ),
      spin = "dots",
      color = "#ffffff"
    )
  })

  # Render isolate picker info text
  output$gs_plot_sel_isolate_info <- renderUI({
    req(DB$data, Screening$available)

    if (is.null(input$gs_plot_selected_isolate)) {
      tagList(
        tags$span(
          style = 'color: white; font-style: italic; font-size: 12px; position: relative;  ',
          "0 isolates(s) selected"
        ),
        tags$br(),
        tags$span(
          style = 'color: orange; font-style: italic; font-size: 12px; position: relative;  ',
          "Select min. 3 isolates"
        )
      )
    } else if (length(input$gs_plot_selected_isolate) < 3) {
      tagList(
        tags$span(
          style = 'color: white; font-style: italic; font-size: 12px; position: relative;  ',
          paste(length(input$gs_plot_selected_isolate), 'isolate(s) selected')
        ),
        tags$br(),
        tags$span(
          style = 'color: orange; font-style: italic; font-size: 12px; position: relative;  ',
          "Select min. 3 isolates"
        )
      )
    } else {
      tagList(
        tags$span(
          style = 'color: white; font-size: 12px; font-style: italic; position: relative;  ',
          paste(length(input$gs_plot_selected_isolate), 'isolate(s) selected')
        ),
        tags$br(),
        tags$span(
          style = 'color: #282F38; font-style: italic; font-size: 12px; position: relative;  ',
          "Select min. 3 isolates"
        )
      )
    }
  })

  # Render gene picker info texts
  output$gs_plot_sel_amr_info <- renderUI({
    req(DB$data, Screening$available)

    if (is.null(input$gs_plot_selected_amr)) {
      tagList(
        tags$span(
          style = 'color: white; font-style: italic; font-size: 12px; position: relative;',
          "0 gene(s) selected"
        ),
        tags$br(),
        tags$span(
          style = 'color: orange; font-style: italic; font-size: 12px; position: relative;',
          "Select min. 3 genes"
        )
      )
    } else if (length(input$gs_plot_selected_amr) < 3) {
      tagList(
        tags$span(
          style = 'color: orange; font-style: italic; font-size: 12px; position: relative;',
          paste(length(input$gs_plot_selected_amr), "gene(s) selected")
        ),
        tags$br(),
        tags$span(
          style = 'color: orange; font-style: italic; font-size: 12px; position: relative;',
          "Select min. 3 genes"
        )
      )
    } else {
      tagList(
        tags$span(
          style = 'color: white; font-style: italic; font-size: 12px; position: relative;',
          paste(length(input$gs_plot_selected_amr), "gene(s) selected")
        ),
        tags$br(),
        tags$span(
          style = 'color: #282F38; font-style: italic; font-size: 12px; position: relative;',
          "Select min. 3 genes"
        )
      )
    }
  })

  output$gs_plot_sel_vir_info <- renderUI({
    req(DB$data, Screening$available)

    if (is.null(input$gs_plot_selected_vir)) {
      tagList(
        tags$span(
          style = 'color: white; font-style: italic; font-size: 12px; position: relative; ',
          "0 gene(s) selected"
        ),
        tags$br(),
        tags$span(
          style = 'color: orange; font-style: italic; font-size: 12px; position: relative; ',
          "Select min. 3 genes"
        )
      )
    } else if (length(input$gs_plot_selected_vir) < 3) {
      tagList(
        tags$span(
          style = 'color: orange; font-style: italic; font-size: 12px; position: relative; ',
          paste(length(input$gs_plot_selected_vir), "gene(s) selected")
        ),
        tags$br(),
        tags$span(
          style = 'color: orange; font-style: italic; font-size: 12px; position: relative; ',
          "Select min. 3 genes"
        )
      )
    } else {
      tagList(
        tags$span(
          style = 'color: white; font-style: italic; font-size: 12px; position: relative; ',
          paste(length(input$gs_plot_selected_vir), "gene(s) selected")
        ),
        tags$br(),
        tags$span(
          style = 'color: #282F38; font-style: italic; font-size: 12px; position: relative; ',
          "Select min. 3 genes"
        )
      )
    }
  })

  output$gs_plot_sel_noclass_info <- renderUI({
    req(DB$data, Screening$available)

    if (is.null(input$gs_plot_selected_noclass)) {
      tagList(
        tags$span(
          style = 'color: white; font-style: italic; font-size: 12px; position: relative; ',
          "0 gene(s) selected"
        ),
        tags$br(),
        tags$span(
          style = 'color: orange; font-style: italic; font-size: 12px; position: relative; ',
          "Select min. 3 genes"
        )
      )
    } else if (length(input$gs_plot_selected_noclass) < 3) {
      tagList(
        tags$span(
          style = 'color: orange; font-style: italic; font-size: 12px; position: relative; ',
          paste(length(input$gs_plot_selected_noclass), "gene(s) selected")
        ),
        tags$br(),
        tags$span(
          style = 'color: orange; font-style: italic; font-size: 12px; position: relative; ',
          "Select min. 3 genes"
        )
      )
    } else {
      tagList(
        tags$span(
          style = 'color: white; font-style: italic; font-size: 12px; position: relative; ',
          paste(length(input$gs_plot_selected_noclass), "gene(s) selected")
        ),
        tags$br(),
        tags$span(
          style = 'color: #282F38; font-style: italic; font-size: 12px; position: relative; ',
          "Select min. 3 genes"
        )
      )
    }
  })

  # Rendering results table
  output$gs_results_table <- renderUI({
    req(DB$data, Screening$available)
    if (!is.null(input$gs_profile_select)) {
      if (
        length(input$gs_profile_select) > 0 &&
          any(input$gs_profile_select %in% DB$data$`Assembly ID`)
      ) {
        fluidRow(
          div(class = "amr-table", DT::dataTableOutput("gs_profile_table")),
          br(),
          HTML(
            paste0(
              "<span style='color: white; font-size: 12px'>",
              '<strong>RSL</strong> = <em>Reference Sequence Length</em>&nbsp&nbsp|&nbsp&nbsp',
              '<strong>%CRS</strong> = <em>% Coverage of Reference Sequence</em>&nbsp&nbsp|&nbsp&nbsp',
              '<strong>%IRS</strong> = <em>% Identity to Reference Sequence</em>&nbsp&nbsp|&nbsp&nbsp',
              '<strong>ACS</strong> = <em>Accession of Closest Sequence</em>&nbsp&nbsp|&nbsp&nbsp',
              '<strong>NCS</strong> = <em>Name of Closest Sequence</em>'
            )
          )
        )
      } else {
        fluidRow(
          br(),
          br(),
          p(
            HTML(
              paste0(
                "<span style='color: white; position: relative; top: 30px; left: 300px; font-size: 12px; font-style: italic'>",
                'Select entry from the table to display resistance profile'
              )
            )
          )
        )
      }
    } else {
      fluidRow(
        br(),
        br(),
        p(
          HTML(
            paste0(
              "<span style='color: white; position: relative; top: 30px; left: 300px; font-size: 12px; font-style: italic'>",
              'Select entry from the table to display resistance profile'
            )
          )
        )
      )
    }
  })

  # Gene screening download button
  output$gs_download <- renderUI({
    req(DB$data, Screening$available)

    if (!is.null(input$gs_profile_select)) {
      if (length(input$gs_profile_select) > 0) {
        fluidRow(
          downloadBttn(
            "download_resistance_profile",
            style = "simple",
            label = "Profile Table",
            size = "sm",
            icon = icon("download"),
            color = "primary"
          ),
          bsTooltip(
            "download_resistance_profile_bttn",
            HTML(paste0(
              "Save resistance profile table for</br>",
              input$gs_profile_select
            )),
            placement = "bottom",
            trigger = "hover"
          )
        )
      } else {
        NULL
      }
    } else {
      NULL
    }
  })

  # Resistance profile table output display
  output$gs_profile_display <- renderUI({
    req(DB$data, Screening$available)

    amrfinder_available <- check.amrfinder.available(
      selected_scheme = DB$scheme,
      amrfinder_species = amrfinder_species
    )
    if (!isFALSE(amrfinder_available)) {
      if (!is.null(DB$meta_gs)) {
        column(
          width = 10,
          fluidRow(
            column(
              width = 4,
              p(
                HTML(
                  paste0(
                    "<span style='color: white; font-size: 18px'>",
                    "Gene Screening Results</br>",
                    "<span style='color: white; font-size: 12px; font-style:italic'>",
                    "Comprising genes for resistance, virulence, stress, etc."
                  )
                )
              )
            ),
            column(
              width = 3,
              div(
                class = "gs-picker",
                pickerInput(
                  "gs_profile_select",
                  "",
                  choices = list(
                    Screened = if (
                      length(DB$data$`Assembly ID`[which(
                        DB$data$Screened == "Yes"
                      )]) ==
                        1
                    ) {
                      as.list(DB$data$`Assembly ID`[which(
                        DB$data$Screened == "Yes"
                      )])
                    } else {
                      DB$data$`Assembly ID`[which(
                        DB$data$Screened == "Yes"
                      )]
                    },
                    Unscreened = if (
                      length(DB$data$`Assembly ID`[which(
                        DB$data$Screened == "No"
                      )]) ==
                        1
                    ) {
                      as.list(DB$data$`Assembly ID`[which(
                        DB$data$Screened == "No"
                      )])
                    } else {
                      DB$data$`Assembly ID`[which(DB$data$Screened == "No")]
                    },
                    `No Assembly File` = if (
                      sum(
                        DB$data$Screened == "NA"
                      ) ==
                        1
                    ) {
                      as.list(DB$data$`Assembly ID`[which(
                        DB$data$Screened == "NA"
                      )])
                    } else {
                      DB$data$`Assembly ID`[which(DB$data$Screened == "NA")]
                    }
                  ),
                  choicesOpt = list(
                    disabled = c(
                      rep(
                        FALSE,
                        length(DB$data$`Assembly ID`[which(
                          DB$data$Screened == "Yes"
                        )])
                      ),
                      rep(
                        TRUE,
                        length(DB$data$`Assembly ID`[which(
                          DB$data$Screened == "No"
                        )])
                      ),
                      rep(
                        TRUE,
                        length(DB$data$`Assembly ID`[which(
                          DB$data$Screened == "NA"
                        )])
                      )
                    )
                  ),
                  options = list(
                    `live-search` = TRUE,
                    size = 10,
                    style = "background-color: white; border-radius: 5px;"
                  )
                )
              )
            ),
            column(
              width = 1,
              actionButton(
                "gs_table_view",
                "",
                icon = icon("table-list")
              ),
              bsTooltip(
                "gs_table_view",
                HTML("Select from table"),
                placement = "bottom",
                trigger = "hover"
              )
            ),
            column(
              width = 3,
              align = "center",
              uiOutput("gs_download")
            )
          ),
          br(),
          uiOutput("gs_results_table")
        )
      } else {
        NULL
      }
    } else {
      NULL
    }
  })

  # Resistance profile table
  observe({
    req(
      Screening$available,
      DB$meta_gs,
      input$gs_profile_select,
      Startup$database,
      DB$scheme,
      DB$data
    )

    if (
      length(input$gs_profile_select) > 0 &&
        any(
          input$gs_profile_select %in%
            DB$data$`Assembly ID`
        )
    ) {
      iso_select <- input$gs_profile_select
      iso_path <- file.path(
        Startup$database,
        gsub(" ", "_", DB$scheme),
        "Isolates",
        iso_select,
        "amrfinder.out"
      )

      if (file.exists(iso_path)) {
        res_profile <- read.delim(iso_path)

        colnames(res_profile) <- c(
          "Protein Identifier",
          "Contig ID",
          "Start",
          "Stop",
          "Strand",
          "Gene Symbol",
          "Sequence Name",
          "Scope",
          "Element Type",
          "Element Subtype",
          "Class",
          "Subclass",
          "Method",
          "Target Length",
          "RSL",
          "%CRS",
          "%IRS",
          "Alignment Length",
          "ACS",
          "Name of Closest Sequence",
          "HMM ID",
          "HMM Description"
        )

        Screening$res_profile <- res_profile %>%
          relocate(
            c(
              "Gene Symbol",
              "Sequence Name",
              "Element Subtype",
              "Class",
              "Subclass",
              "Scope",
              "Contig ID",
              "Target Length",
              "Alignment Length",
              "Start",
              "Stop",
              "Strand"
            )
          )

        # Generate gene profile table
        output$gs_profile_table <- DT::renderDataTable(
          Screening$res_profile,
          selection = "single",
          rownames = FALSE,
          options = list(
            scrollX = TRUE,
            autoWidth = TRUE,
            pageLength = 10,
            columnDefs = list(
              list(searchable = TRUE, targets = "_all"),
              list(
                width = '300px',
                targets = c("Sequence Name", "Name of Closest Sequence")
              )
            )
          )
        )
      }
    } else {
      output$gs_profile_table <- NULL
    }
  })

  # Resistance profile selection table
  observe({
    req(DB$meta, DB$data, Screening$available)
    output$gs_isolate_table <- renderDataTable(
      select(
        DB$meta_gs[which(DB$meta_gs$Screened == "Yes"), ],
        -c(3, 5, 11, 12)
      ),
      selection = "single",
      rownames = FALSE,
      options = list(
        scrollY = TRUE,
        pageLength = 10,
        autoWidth = TRUE,
        columnDefs = list(
          list(searchable = TRUE, targets = "_all", className = "dt-left")
        )
      )
    )
  })

  observeEvent(input$gs_table_view, {
    showModal(
      div(
        class = "start-modal",
        div(
          class = "modal-fit-content",
          modalDialog(
            fluidRow(
              fluidRow(
                br(),
                column(
                  width = 11,
                  p(
                    HTML(
                      paste0(
                        '<span style="color: white; display: block; font-size: 15px; margin-left: 15px;">',
                        "Click an isolate from the local database to display the corresponding gene screening results.",
                        '</span>'
                      )
                    )
                  ),
                  uiOutput("gs_table_selected_isolate")
                ),
                br()
              ),
              br(),
              column(
                width = 12,
                div(class = "amr-table", dataTableOutput("gs_isolate_table"))
              ),
              br()
            ),
            title = paste("Select Isolate"),
            fade = TRUE,
            easyClose = TRUE,
            footer = tagList(
              modalButton("Dismiss"),
              actionButton("conf_gs_select", "Select")
            )
          )
        )
      )
    )
  })

  observeEvent(input$conf_gs_select, {
    meta_gs <- DB$meta_gs[which(DB$meta_gs$Screened == "Yes"), ]
    selected_isolate <-
      meta_gs$`Assembly ID`[input$gs_isolate_table_rows_selected]

    updatePickerInput(session, "gs_profile_select", selected = selected_isolate)

    removeModal()
  })

  observe({
    req(
      input$screening_res_sel,
      Startup$database,
      DB$scheme,
      DB$data,
      Screening$available
    )

    result_path <- file.path(
      Startup$database,
      gsub(" ", "_", DB$scheme),
      "Isolates",
      input$screening_res_sel,
      "amrfinder.out"
    )

    if (
      !is.null(Screening$status_df) &&
        !is.null(input$screening_res_sel) &&
        !is.null(Screening$status_df$status) &&
        !is.null(Screening$status_df$isolate) &&
        file.exists(result_path)
    ) {
      if (length(input$screening_res_sel) > 0) {
        if (any(Screening$status_df$isolate == input$screening_res_sel)) {
          if (
            Screening$status_df$status[which(
              Screening$status_df$isolate == input$screening_res_sel
            )] ==
              "success"
          ) {
            results <- select(read.delim(result_path), c(6, 7, 8, 9, 11))
            colnames(results) <- c(
              "Gene Symbol",
              "Sequence Name",
              "Scope",
              "Element Type",
              "Class"
            )

            output$screening_table <- renderDataTable(
              results,
              selection = "single",
              options = list(
                scrollY = TRUE,
                pageLength = 10,
                columnDefs = list(list(searchable = TRUE, targets = "_all"))
              )
            )
          } else {
            output$screening_table <- NULL
          }
        }
      } else {
        output$screening_table <- NULL
      }
    } else {
      output$screening_table <- NULL
    }
  })

  # Availablity feedback
  output$gene_screening_info <- renderUI({
    req(DB$scheme)
    amrfinder_available <- check.amrfinder.available(
      selected_scheme = DB$scheme,
      amrfinder_species = amrfinder_species
    )

    if (!isFALSE(amrfinder_available)) {
      p(
        HTML(
          paste(
            '<i class="fa-solid fa-check" style="font-size:20px;color:#90EE90; position:relative; top:27px;margin-right: 10px;"></i>',
            '<span style="color: white; font-size: 15px; position:relative; top:25px;">',
            DB$scheme,
            'available for gene screening with ',
            '<a href="https://www.ncbi.nlm.nih.gov/pathogens/antimicrobial-resistance/AMRFinder/" target="_blank" style="color:#008edb; text-decoration:none;">NCBI/AMRFinder</a>.',
            '</span>'
          )
        )
      )
    } else {
      p(
        HTML(
          paste(
            '<i class="fa-solid fa-xmark" style="font-size:20px;color:#ff0000; position:relative; top:27px;margin-right: 10px;"></i>',
            '<span style="color: white; font-size: 15px; position:relative; top:25px;">',
            DB$scheme,
            'not available for gene screening with ',
            '<a href="https://www.ncbi.nlm.nih.gov/pathogens/antimicrobial-resistance/AMRFinder/" target="_blank" style="color:#008edb; text-decoration:none;">NCBI/AMRFinder</a>.',
            '</span>'
          )
        )
      )
    }
  })

  output$gene_resistance_info <- renderUI({
    req(DB$scheme)
    amrfinder_available <- check.amrfinder.available(
      selected_scheme = DB$scheme,
      amrfinder_species = amrfinder_species
    )

    if (!isFALSE(amrfinder_available)) {
      p(
        HTML(
          paste(
            '<i class="fa-solid fa-check" style="font-size:20px;color:#90EE90; position:relative; top:27px;margin-right: 10px;"></i>',
            '<span style="color: white; font-size: 15px; position:relative; top:25px;">',
            DB$scheme,
            'available for gene screening with ',
            '<a href="https://www.ncbi.nlm.nih.gov/pathogens/antimicrobial-resistance/AMRFinder/" target="_blank" style="color:#008edb; text-decoration:none;">NCBI/AMRFinder</a>.',
            '</span>'
          )
        )
      )
    } else {
      p(
        HTML(
          paste(
            '<i class="fa-solid fa-xmark" style="font-size:20px;color:#ff0000; position:relative; top:27px;margin-right: 10px;"></i>',
            '<span style="color: white; font-size: 15px; position:relative; top:25px;">',
            DB$scheme,
            'not available for gene screening with ',
            '<a href="https://www.ncbi.nlm.nih.gov/pathogens/antimicrobial-resistance/AMRFinder/" target="_blank" style="color:#008edb; text-decoration:none;">NCBI/AMRFinder</a>.',
            '</span>'
          )
        )
      )
    }
  })

  # Screening Interface

  output$screening_interface <- renderUI({
    req(DB$data, DB$scheme, Screening$available)
    amrfinder_available <- check.amrfinder.available(
      selected_scheme = DB$scheme,
      amrfinder_species = amrfinder_species
    )

    if (!isFALSE(amrfinder_available)) {
      column(
        width = 12,
        fluidRow(
          column(1),
          column(
            width = 10,
            fluidRow(
              column(
                width = 3,
                align = "left",
                div(
                  class = "screening-box",
                  box(
                    solidHeader = TRUE,
                    status = "primary",
                    width = "100%",
                    title = "Initiate Screening",
                    column(
                      width = 12,
                      align = "center",
                      p(
                        HTML(
                          paste(
                            tags$span(
                              style = 'color: white; font-size: 15px; margin-bottom: 0px',
                              'Select Isolates for Screening'
                            )
                          )
                        )
                      ),
                      if (Screening$picker_status) {
                        div(
                          class = "screening_div",
                          pickerInput(
                            "screening_select",
                            "",
                            choices = list(
                              Unscreened = if (
                                length(
                                  DB$data$`Assembly ID`[which(
                                    DB$data$Screened == "No"
                                  )]
                                ) ==
                                  1
                              ) {
                                as.list(DB$data$`Assembly ID`[which(
                                  DB$data$Screened == "No"
                                )])
                              } else {
                                DB$data$`Assembly ID`[which(
                                  DB$data$Screened == "No"
                                )]
                              },
                              Screened = if (
                                length(
                                  DB$data$`Assembly ID`[which(
                                    DB$data$Screened == "Yes"
                                  )]
                                ) ==
                                  1
                              ) {
                                as.list(DB$data$`Assembly ID`[which(
                                  DB$data$Screened == "Yes"
                                )])
                              } else {
                                DB$data$`Assembly ID`[which(
                                  DB$data$Screened == "Yes"
                                )]
                              },
                              `No Assembly File` = if (
                                sum(
                                  DB$data$Screened == "NA"
                                ) ==
                                  1
                              ) {
                                as.list(DB$data$`Assembly ID`[which(
                                  DB$data$Screened == "NA"
                                )])
                              } else {
                                DB$data$`Assembly ID`[which(
                                  DB$data$Screened == "NA"
                                )]
                              }
                            ),
                            choicesOpt = list(
                              disabled = c(
                                rep(
                                  FALSE,
                                  length(DB$data$`Assembly ID`[which(
                                    DB$data$Screened == "No"
                                  )])
                                ),
                                rep(
                                  TRUE,
                                  length(DB$data$`Assembly ID`[which(
                                    DB$data$Screened == "Yes"
                                  )])
                                ),
                                rep(
                                  TRUE,
                                  length(DB$data$`Assembly ID`[which(
                                    DB$data$Screened == "NA"
                                  )])
                                )
                              )
                            ),
                            options = list(
                              `live-search` = TRUE,
                              `actions-box` = TRUE,
                              size = 10,
                              style = "background-color: white; border-radius: 5px;"
                            ),
                            multiple = TRUE
                          )
                        )
                      } else {
                        div(
                          class = "screening_div",
                          pickerInput(
                            "screening_select",
                            "",
                            choices = Screening$picker_choices,
                            selected = Screening$picker_selected,
                            options = list(
                              `live-search` = TRUE,
                              `actions-box` = TRUE,
                              size = 10,
                              style = "background-color: white; border-radius: 5px;"
                            ),
                            multiple = TRUE
                          )
                        )
                      },
                      br(),
                      br(),
                      uiOutput("genome_path_gs"),
                      br()
                    )
                  )
                )
              ),
              column(
                width = 3,
                uiOutput("screening_start")
              ),
              uiOutput("screening_result_sel")
            )
          )
        ),
        fluidRow(
          column(1),
          column(
            width = 10,
            uiOutput("screening_result"),
            br(),
            br()
          )
        )
      )
    }
  })

  ### Screening Events ----

  output$download_resistance_profile <- downloadHandler(
    filename = function() {
      log_print(paste0(
        "Save resistance profile table ",
        input$gs_profile_select,
        "_Profile.csv"
      ))

      paste0(Sys.Date(), "_", input$gs_profile_select, "_AMR_Profile.csv")
    },
    content = function(file) {
      write.table(
        Screening$res_profile,
        file,
        sep = ";",
        row.names = FALSE,
        quote = FALSE
      )
    }
  )

  # Reset screening
  observeEvent(input$screening_reset_bttn, {
    runjs(block_ui)
    log_print("Reset gene screening")

    # reset status file
    sapply(
      Screening$status_df$isolate,
      remove.screening.status,
      database = Startup$database,
      scheme = DB$scheme
    )

    # set feedback variables
    Screening$status <- "idle"
    Screening$status_df <- NULL
    Screening$choices <- NULL
    Screening$picker_status <- TRUE
    Screening$first_result <- NULL

    # change reactive UI
    output$screening_table <- NULL
    output$screening_result <- NULL
    output$screening_fail <- NULL

    updatePickerInput(session, "screening_select", selected = character(0))

    # enable isolate picker
    delay(
      200,
      runjs("$('#screening_select').prop('disabled', false);")
    )
    delay(
      200,
      runjs("$('#screening_select').selectpicker('refresh');")
    )

    runjs(unblock_ui)
  })

  # Cancel screening
  observeEvent(input$screening_cancel, {
    showModal(
      div(
        class = "start-modal",
        modalDialog(
          fluidRow(
            br(),
            column(
              width = 11,
              p(
                HTML(
                  paste0(
                    '<span style="color: white; display: block; font-size: 15px; margin-left: 15px;">',
                    "Gene Screening still pending. Are you sure you want to stop the screening?",
                    '</span>'
                  )
                )
              )
            ),
            br()
          ),
          title = "Cancel Gene Screening",
          fade = TRUE,
          easyClose = TRUE,
          footer = tagList(
            modalButton("Cancel"),
            actionButton(
              "conf_screening_cancel",
              "Stop",
              class = "btn btn-danger"
            )
          )
        )
      )
    )
  })

  observeEvent(input$conf_screening_cancel, {
    runjs(block_ui)
    log_print("Cancelled gene screening")
    removeModal()

    show_toast(
      title = "Gene Screening Terminated",
      type = "success",
      position = "bottom-end",
      timer = 6000
    )

    # terminate screening
    system(paste0("kill $(pgrep -f 'bin/screening.sh')"), wait = FALSE)
    system(paste("killall -TERM tblastn"), wait = FALSE)

    # reset status file
    sapply(
      Screening$status_df$isolate,
      remove.screening.status,
      database = Startup$database,
      scheme = DB$scheme
    )

    # set feedback variables
    Screening$status <- "idle"
    Screening$status_df <- NULL
    Screening$choices <- NULL
    Screening$picker_status <- TRUE
    Screening$first_result <- NULL

    # change reactive UI
    output$screening_table <- NULL
    output$screening_result <- NULL

    updatePickerInput(session, "screening_select", selected = character(0))

    # disable isolate picker
    delay(
      200,
      runjs("$('#screening_select').prop('disabled', false);")
    )
    delay(
      200,
      runjs("$('#screening_select').selectpicker('refresh');")
    )

    runjs(unblock_ui)
  })

  # Get selected assembly
  observe({
    req(DB$data, Screening$status, Screening$available)
    if (length(input$screening_select) < 1) {
      output$genome_path_gs <- renderUI(HTML(
        paste(
          "<span style='color: white; font-style:italic'>",
          length(input$screening_select),
          " isolate(s) queried for screening"
        )
      ))

      output$screening_start <- NULL
    } else if (length(input$screening_select) > 0) {
      output$screening_start <- renderUI({
        div(
          class = "screening-box",
          box(
            solidHeader = TRUE,
            status = "primary",
            width = "100%",
            title = "Perform Screening",
            if (length(input$screening_select) < 1) {
              column(
                width = 12,
                align = "center",
                p(
                  HTML(
                    paste(
                      '<i class="fa-solid fa-circle-exclamation" style="font-size:16px;color:orange"></i>',
                      paste(
                        "<span style='color: white; font-style:italic'>",
                        "&nbsp Select Isolate(s) for Screening"
                      )
                    )
                  )
                ),
                br(),
                br()
              )
            } else if (Screening$status == "finished") {
              # Load AMR profile
              profile_path <- file.path(
                Startup$database,
                gsub(" ", "_", DB$scheme),
                "AMR_Profile.rds"
              )

              if (file.exists(profile_path)) {
                amr_profile <- readRDS(profile_path)
                Screening$amr_results <- amr_profile$results
                Screening$amr_class <- amr_profile$AMR_classification
                Screening$vir_class <- amr_profile$virulence_classification
              }

              column(
                width = 12,
                align = "center",
                p(
                  HTML(
                    paste(
                      '<i class="fa-solid fa-circle-exclamation" style="font-size:15px;color:white"></i>',
                      paste(
                        "<span style='color: white; font-style:italic'>",
                        "&nbsp Reset to perform screening again"
                      )
                    )
                  )
                ),
                fluidRow(
                  actionButton(
                    "screening_reset_bttn",
                    "Reset",
                    icon = icon("arrows-rotate")
                  )
                ),
                if (!is.null(Screening$status_df)) {
                  HTML(paste(
                    "<span style='color: white; font-style:italic;'>",
                    sum(Screening$status_df$status != "unfinished"),
                    "/",
                    nrow(Screening$status_df),
                    " isolate(s) screened"
                  ))
                },
                br(),
                br()
              )
            } else if (Screening$status == "idle") {
              column(
                width = 12,
                align = "center",
                p(
                  HTML(
                    paste(
                      '<i class="fa-solid fa-circle-check" style="font-size:15px;color:lightgreen"></i>',
                      paste(
                        "<span style='color: white;font-size:15px'>",
                        "&nbsp Select isolates for screening"
                      )
                    )
                  )
                ),
                actionButton(
                  inputId = "screening_start_button",
                  label = "Start",
                  icon = icon("circle-play")
                ),
                br(),
                br()
              )
            } else if (Screening$status == "started") {
              column(
                width = 12,
                align = "center",
                p(
                  HTML(paste(
                    '<i class="fa-solid fa-clock" style="font-size:16px;color:white"></i>',
                    paste(
                      "<span style='color: white;font-size:16px'>",
                      "&nbsp Running Screening ..."
                    )
                  ))
                ),
                fluidRow(
                  column(3),
                  column(
                    width = 3,
                    actionButton(
                      inputId = "screening_cancel",
                      label = "Terminate",
                      icon = icon("ban")
                    )
                  ),
                  column(
                    width = 3,
                    HTML(paste(
                      '<i class="fa fa-spinner fa-spin" style="font-size:22px;color:white;margin-top:27px;position:relative;left:35px; top:-10px"></i>'
                    ))
                  )
                ),
                if (!is.null(Screening$status_df)) {
                  HTML(paste(
                    "<span style='color: white; font-style:italic;'>",
                    sum(Screening$status_df$status != "unfinished"),
                    "/",
                    nrow(Screening$status_df),
                    " isolate(s) screened"
                  ))
                },
                br(),
                br()
              )
            }
          )
        )
      })
    } else {
      NULL
    }
  })

  #### Running Screening ----

  observeEvent(input$screening_start_button, {
    if (tail(readLogFile(), 1) != "0") {
      show_toast(
        title = "Pending Multi Typing",
        type = "warning",
        position = "bottom-end",
        timer = 6000
      )
    } else {
      log_print("Started gene screening")

      Screening$status <- "started"
      Screening$picker_choices <- list(
        Unscreened = if (sum(DB$data$Screened == "No") == 1) {
          as.list(DB$data$`Assembly ID`[which(DB$data$Screened == "No")])
        } else {
          DB$data$`Assembly ID`[which(DB$data$Screened == "No")]
        },
        Screened = if (sum(DB$data$Screened == "Yes") == 1) {
          as.list(DB$data$`Assembly ID`[which(DB$data$Screened == "Yes")])
        } else {
          DB$data$`Assembly ID`[which(DB$data$Screened == "Yes")]
        },
        `No Assembly File` = if (sum(DB$data$Screened == "NA") == 1) {
          as.list(DB$data$`Assembly ID`[which(DB$data$Screened == "NA")])
        } else {
          DB$data$`Assembly ID`[which(DB$data$Screened == "NA")]
        }
      )
      Screening$picker_selected <- input$screening_select
      Screening$picker_status <- FALSE

      show_toast(
        title = "Gene screening started",
        type = "info",
        position = "bottom-end",
        timer = 6000
      )

      Screening$meta_df <- data.frame(
        wd = shQuote(getwd()),
        selected = paste(
          shQuote(file.path(
            Startup$database,
            gsub(" ", "_", DB$scheme),
            "Isolates",
            input$screening_select,
            paste0(input$screening_select, ".zip")
          )),
          collapse = " ~ "
        ),
        species = gsub(
          " ",
          "_",
          check.amrfinder.available(
            selected_scheme = DB$scheme,
            amrfinder_species = amrfinder_species
          )
        ),
        database = shQuote(Startup$database),
        scheme = DB$scheme
      )

      Screening$status_df <- data.frame(
        isolate = input$screening_select,
        status = "unfinished"
      )

      # Reset screening status
      sapply(
        Screening$status_df$isolate,
        remove.screening.status,
        database = Startup$database,
        scheme = DB$scheme
      )

      saveRDS(
        Screening$meta_df,
        file.path(app_local_share_path, "screening_meta.rds")
      )

      # Disable pickerInput
      delay(
        200,
        runjs("$('#screening_select').prop('disabled', true);")
      )
      delay(
        200,
        runjs("$('#screening_select').selectpicker('refresh');")
      )

      # System execution screening.sh
      system(
        paste("bash", shQuote(paste0(getwd(), "/bin/screening.sh"))),
        wait = FALSE
      )
    }
  })

  observe({
    req(
      Screening$available,
      DB$data,
      Screening$status,
      input$screening_res_sel,
      Screening$status_df
    )
    if (
      !is.null(Screening$status_df) &&
        !is.null(Screening$status_df$status) &&
        !is.null(Screening$status_df$isolate) &&
        !is.null(input$screening_res_sel)
    ) {
      if (Screening$status != "idle" && length(input$screening_res_sel) > 0) {
        if (any(Screening$status_df$isolate == input$screening_res_sel)) {
          if (
            Screening$status_df$status[which(
              Screening$status_df$isolate == input$screening_res_sel
            )] ==
              "success"
          ) {
            output$screening_result <- renderUI(
              column(
                width = 12,
                p(
                  HTML(
                    paste(
                      '<i class="fa-solid fa-circle-check" style="font-size:16px;color:lightgreen"></i>',
                      paste(
                        "<span style='color: white; font-style:italic'>",
                        "&nbsp Results of gene screening appended to local database."
                      )
                    )
                  )
                ),
                div(
                  class = "screening-table",
                  dataTableOutput("screening_table")
                )
              )
            )
          } else {
            output$screening_result <- renderUI(
              column(
                width = 12,
                br(),
                verbatimTextOutput("screening_fail")
              )
            )
          }
        }
      } else {
        output$screening_result <- NULL
      }
    } else {
      output$screening_result <- NULL
    }
  })

  observe({
    req(Screening$available, DB$data, Screening$status, input$screening_res_sel)
    if (
      !is.null(Screening$status_df) &&
        !is.null(Screening$status_df$status) &&
        !is.null(Screening$status_df$isolate) &&
        !is.null(input$screening_res_sel)
    ) {
      if (Screening$status != "idle" && length(input$screening_res_sel) > 0) {
        if (any(Screening$status_df$isolate == input$screening_res_sel)) {
          if (
            Screening$status_df$status[which(
              Screening$status_df$isolate == input$screening_res_sel
            )] ==
              "fail"
          ) {
            output$screening_fail <- renderPrint({
              cat(paste(
                readLines(file.path(
                  Startup$database,
                  gsub(" ", "_", DB$scheme),
                  "Isolates",
                  input$screening_res_sel,
                  "status.txt"
                )),
                "\n"
              ))
            })
          }
        }
      } else {
        output$screening_fail <- NULL
      }
    } else {
      output$screening_fail <- NULL
    }
  })

  observe({
    req(DB$data, Screening$available)
    if (!is.null(Screening$status)) {
      if (Screening$status != "idle") {
        # start status screening for user feedback
        check_screening()

        if (isTRUE(Screening$first_result)) {
          output$screening_result_sel <- renderUI(
            fluidRow(
              column(
                width = 2,
                align = "center",
                HTML(
                  '<i class="fa-solid fa-right-long fa-beat-fade" style="font-size: 50px; color: white; margin-top: 80px;"></i>'
                )
              ),
              column(
                width = 3,
                align = "left",
                div(
                  class = "screening-box",
                  box(
                    solidHeader = TRUE,
                    status = "primary",
                    width = "100%",
                    title = "Browse Result(s)",
                    column(
                      width = 12,
                      align = "center",
                      p(
                        HTML(
                          paste(
                            tags$span(
                              style = 'color: white; font-size: 15px; margin-bottom: 0px',
                              'Select screening results'
                            )
                          )
                        )
                      ),
                      div(
                        class = "screening-div",
                        selectInput(
                          "screening_res_sel",
                          label = "",
                          choices = ""
                        )
                      ),
                      if (!is.null(Screening$status_df)) {
                        HTML(paste(
                          "<span style='color: white; font-style:italic;'>",
                          if (
                            sum(
                              Screening$status_df$status == "success"
                            ) ==
                              1
                          ) {
                            "1 success &nbsp / &nbsp"
                          } else {
                            paste0(
                              sum(Screening$status_df$status == "success"),
                              " successes &nbsp / &nbsp"
                            )
                          },
                          if (sum(Screening$status_df$status == "fail") == 1) {
                            "1 failure"
                          } else {
                            paste0(
                              sum(Screening$status_df$status == "fail"),
                              " failures"
                            )
                          }
                        ))
                      },
                      br(),
                      br()
                    )
                  )
                )
              )
            )
          )

          Screening$first_result <- FALSE
        }
      } else if (Screening$status == "idle") {
        output$screening_result_sel <- NULL
      }
    }
  })

  check_screening <- reactive({
    invalidateLater(500, session)

    req(Screening$status_df)

    if (Screening$status == "started") {
      Screening$status_df$status <- sapply(
        Screening$status_df$isolate,
        check_status,
        database = Startup$database,
        scheme = DB$scheme
      )

      if (
        any("unfinished" != Screening$status_df$status) &&
          !identical(
            Screening$choices,
            Screening$status_df$isolate[which(
              Screening$status_df$status != "unfinished"
            )]
          )
      ) {
        status_df <- Screening$status_df[
          which(
            Screening$status_df$status != "unfinished"
          ),
        ]

        Screening$choices <- Screening$status_df$isolate[which(
          Screening$status_df$status == "success" |
            Screening$status_df$status == "fail"
        )]

        if (sum(Screening$status_df$status != "unfinished") > 0) {
          if (is.null(Screening$first_result)) {
            Screening$first_result <- TRUE
          }
        }

        if (tail(status_df$status, 1) == "success") {
          # Changing "Screened" metadata variable in database
          Database <- readRDS(file.path(
            Startup$database,
            gsub(
              " ",
              "_",
              DB$scheme
            ),
            "Typing.rds"
          ))

          Database[["Typing"]]$Screened[which(
            Database[["Typing"]]["Assembly ID"] ==
              tail(
                Screening$choices,
                1
              )
          )] <- "Yes"

          saveRDS(
            Database,
            file.path(
              Startup$database,
              gsub(
                " ",
                "_",
                DB$scheme
              ),
              "Typing.rds"
            )
          )

          DB$data$Screened[which(
            DB$data["Assembly ID"] ==
              tail(
                Screening$choices,
                1
              )
          )] <- "Yes"

          DB$meta_gs <- select(DB$data, c(1, 3:14))
          DB$meta <- select(DB$data, 1:(14 + nrow(DB$cust_var)))
          DB$meta_true <- DB$meta[which(DB$data$Include == TRUE), ]

          show_toast(
            title = paste(
              "Successful screening of",
              tail(Screening$choices, 1)
            ),
            type = "success",
            position = "bottom-end",
            timer = 6000
          )

          updateSelectInput(
            session = session,
            inputId = "screening_res_sel",
            choices = Screening$choices,
            selected = tail(Screening$choices, 1)
          )

          # Disable pickerInput
          delay(
            200,
            runjs("$('#screening_select').prop('disabled', true);")
          )
          delay(
            200,
            runjs("$('#screening_select').selectpicker('refresh');")
          )
        } else if (tail(status_df$status, 1) == "fail") {
          show_toast(
            title = paste("Failed screening of", tail(status_df$isolate, 1)),
            type = "error",
            position = "bottom-end",
            timer = 6000
          )

          updateSelectInput(
            session = session,
            inputId = "screening_res_sel",
            choices = Screening$choices,
            selected = tail(Screening$choices, 1)
          )

          # Disable pickerInput
          delay(
            200,
            runjs("$('#screening_select').prop('disabled', true);")
          )
          delay(
            200,
            runjs("$('#screening_select').selectpicker('refresh');")
          )
        }

        if (
          sum("unfinished" != Screening$status_df$status) ==
            length(
              Screening$status_df$status
            )
        ) {
          Screening$status <- "finished"
        }
      } else {
        if (
          sum("unfinished" != Screening$status_df$status) ==
            length(
              Screening$status_df$status
            )
        ) {
          Screening$status <- "finished"
        }
      }

      if (
        sum("unfinished" != Screening$status_df$status) ==
          length(
            Screening$status_df$status
          )
      ) {
        Screening$status <- "finished"
      }
    }
  })

  ### AMR Visualization ----

  gs_plot_selected_isolate <- reactiveVal()
  gs_plot_selected_amr <- reactiveVal()
  gs_plot_selected_vir <- reactiveVal()
  gs_plot_selected_noclass <- reactiveVal()

  observe({
    if (!is.null(input$gs_plot_selected_isolate)) {
      gs_plot_selected_isolate(input$gs_plot_selected_isolate)
    }
    if (!is.null(input$gs_plot_selected_amr)) {
      gs_plot_selected_amr(input$gs_plot_selected_amr)
    } else {
      gs_plot_selected_amr(character(0))
    }
    if (!is.null(input$gs_plot_selected_vir)) {
      gs_plot_selected_vir(input$gs_plot_selected_vir)
    } else {
      gs_plot_selected_vir(character(0))
    }
    if (!is.null(input$gs_plot_selected_noclass)) {
      gs_plot_selected_noclass(input$gs_plot_selected_noclass)
    } else {
      gs_plot_selected_noclass(character(0))
    }
  })

  gs_virclass_scale <- reactiveVal()
  gs_amrclass_scale <- reactiveVal()
  gs_amr_variables <- reactiveVal()
  gs_vir_variables <- reactiveVal()
  gs_mapping_scale <- reactiveVal()
  gs_var_mapping <- reactiveVal()

  observe({
    if (!is.null(input$gs_virclass_scale)) {
      gs_virclass_scale(input$gs_virclass_scale)
    }

    if (!is.null(input$gs_amrclass_scale)) {
      gs_amrclass_scale(input$gs_amrclass_scale)
    }

    if (!is.null(input$gs_vir_variables)) {
      gs_vir_variables(input$gs_vir_variables)
    }

    if (!is.null(input$gs_amr_variables)) {
      gs_amr_variables(input$gs_amr_variables)
    }

    if (!is.null(input$gs_var_mapping)) {
      gs_var_mapping(input$gs_var_mapping)
    }

    if (!is.null(input$gs_mapping_scale)) {
      gs_mapping_scale(input$gs_mapping_scale)
    }
  })

  gsplot_color_text <- reactiveVal()
  gsplot_color_dend <- reactiveVal()
  gsplot_color_palette1 <- reactiveVal()
  gsplot_color_palette2 <- reactiveVal()
  gsplot_grid_color <- reactiveVal()
  gsplot_background <- reactiveVal()

  observe({
    if (!is.null(input$gsplot_color_text)) {
      gsplot_color_text(input$gsplot_color_text)
    }
    if (!is.null(input$gsplot_color_dend)) {
      gsplot_color_dend(input$gsplot_color_dend)
    }
    if (!is.null(input$gsplot_color_palette1)) {
      gsplot_color_palette1(input$gsplot_color_palette1)
    }
    if (!is.null(input$gsplot_color_palette2)) {
      gsplot_color_palette2(input$gsplot_color_palette2)
    }
    if (!is.null(input$gsplot_grid_color)) {
      gsplot_grid_color(input$gsplot_grid_color)
    }
    if (!is.null(input$gsplot_background)) {
      gsplot_background(input$gsplot_background)
    }
  })

  gs_plot <- reactive({
    req(DB$data, Screening$amr_results, Startup$database, DB$scheme)

    # get inputs
    if (!is.null(input$gs_plot_selected_isolate)) {
      gs_plot_selected_isolate <- input$gs_plot_selected_isolate
    } else {
      gs_plot_selected_isolate <- DB$data$`Assembly ID`[which(
        DB$data$Screened == "Yes"
      )]
    }

    if (length(gs_plot_selected_isolate) > 2) {
      gs_plot_selected_amr <- input$gs_plot_selected_amr
      gs_plot_selected_vir <- input$gs_plot_selected_vir
      gs_plot_selected_noclass <- input$gs_plot_selected_noclass

      # get heatmap
      amr_profile_numeric <- as.data.frame(lapply(
        Screening$amr_results,
        as.numeric
      ))
      rownames(amr_profile_numeric) <- rownames(Screening$amr_results)
      colnames(amr_profile_numeric) <- colnames(Screening$amr_results)

      if (
        (length(c(
          gs_plot_selected_amr,
          gs_plot_selected_vir,
          gs_plot_selected_noclass
        )) >=
          3) &&
          length(gs_plot_selected_isolate) >= 3
      ) {
        ### get heatmap meta
        amr_profile_numeric_all <- amr_profile_numeric[
          rownames(
            amr_profile_numeric
          ) %in%
            gs_plot_selected_isolate,
        ]

        combined_genes <- c(
          gs_plot_selected_amr,
          gs_plot_selected_vir,
          gs_plot_selected_noclass
        )

        amr_profile_numeric <-
          amr_profile_numeric_all[,
            combined_genes[
              combined_genes %in%
                colnames(
                  amr_profile_numeric_all
                )
            ]
          ]
        heatmap_mat <- as.matrix(amr_profile_numeric)

        # metadata
        if (nrow(Screening$amr_class) > 0) {
          amr_meta <- get.gsMeta(
            gene_class = Screening$amr_class,
            hm_matrix = heatmap_mat
          )
          colnames(amr_meta) <- "amr"
        } else {
          amr_meta <- data.frame(amr = NA)
        }

        if (nrow(Screening$vir_class) > 0) {
          vir_meta <- get.gsMeta(
            gene_class = Screening$vir_class,
            hm_matrix = heatmap_mat
          )
        } else {
          vir_meta <- data.frame(class = NA)
        }

        # unite meta
        Screening$hm_meta <- add_column(amr_meta, vir = vir_meta$class)
        hm_meta <- Screening$hm_meta

        # styling parameters
        ht_opt$ANNOTATION_LEGEND_PADDING = unit(10, "mm")
        ht_opt$HEATMAP_LEGEND_PADDING = unit(5, "mm")

        if (!is.null(input$gsplot_isolate_label)) {
          gsplot_isolate_label <- input$gsplot_isolate_label
        } else {
          gsplot_isolate_label <- "Assembly Name"
        }

        if (!is.null(input$gsplot_color_text)) {
          gsplot_color_text <- input$gsplot_color_text
        } else if (is.null(gsplot_color_text())) {
          gsplot_color_text <- "#000000"
        } else {
          gsplot_color_text <- gsplot_color_text()
        }

        if (!is.null(input$gsplot_treeheight_col)) {
          gsplot_treeheight_col <- input$gsplot_treeheight_col
        } else {
          gsplot_treeheight_col <- 2
        }

        if (!is.null(input$gsplot_treeheight_row)) {
          gsplot_treeheight_row <- input$gsplot_treeheight_row
        } else {
          gsplot_treeheight_row <- 2
        }

        if (!is.null(input$gsplot_legend_labelsize)) {
          gsplot_legend_labelsize <- input$gsplot_legend_labelsize
        } else {
          gsplot_legend_labelsize <- 9
        }

        if (!is.null(input$gsplot_color_palette1)) {
          gsplot_color_palette1 <- input$gsplot_color_palette1
        } else {
          gsplot_color_palette1 <- "#66C2A5"
        }

        if (!is.null(input$gsplot_color_palette2)) {
          gsplot_color_palette2 <- input$gsplot_color_palette2
        } else {
          gsplot_color_palette2 <- "#E5C494"
        }

        if (!is.null(input$gs_amr_variables)) {
          gs_amr_variables <- input$gs_amr_variables
        } else {
          gs_amr_variables <- "Classification"
        }

        if (!is.null(input$gs_vir_variables)) {
          gs_vir_variables <- input$gs_vir_variables
        } else {
          gs_vir_variables <- "Classification"
        }

        if (!is.null(input$gsplot_grid_color)) {
          gsplot_grid_color <- input$gsplot_grid_color
        } else {
          gsplot_grid_color <- "#FFFFFF"
        }

        if (!is.null(input$gsplot_grid_width)) {
          gsplot_grid_width <- input$gsplot_grid_width
        } else {
          gsplot_grid_width <- 1
        }

        if (!is.null(input$gsplot_fontsize_title)) {
          gsplot_fontsize_title <- input$gsplot_fontsize_title
        } else {
          gsplot_fontsize_title <- 16
        }

        if (!is.null(input$gs_cluster_col)) {
          gs_cluster_col <- input$gs_cluster_col
        } else {
          gs_cluster_col <- TRUE
        }

        if (!is.null(input$gs_cluster_distance_col)) {
          if (input$gs_cluster_distance_col == "binary") {
            gs_cluster_distance_col <- "binary"
          } else if (input$gs_cluster_distance_col == "mcc") {
            gs_cluster_distance_col <- mcc_dist_matrix
          } else if (input$gs_cluster_distance_col == "hamming") {
            gs_cluster_distance_col <- hamming_dist_matrix
          }
        } else {
          gs_cluster_distance_col <- "binary"
        }

        if (!is.null(input$gs_cluster_method_col)) {
          gs_cluster_method_col <- input$gs_cluster_method_col
        } else {
          gs_cluster_method_col <- "average"
        }

        if (!is.null(input$gs_cluster_row)) {
          gs_cluster_row <- input$gs_cluster_row
        } else {
          gs_cluster_row <- TRUE
        }

        if (!is.null(input$gs_cluster_distance_row)) {
          if (input$gs_cluster_distance_row == "binary") {
            gs_cluster_distance_row <- "binary"
          } else if (input$gs_cluster_distance_row == "mcc") {
            gs_cluster_distance_row <- mcc_dist_matrix
          } else if (input$gs_cluster_distance_col == "hamming") {
            gs_cluster_distance_row <- hamming_dist_matrix
          }
        } else {
          gs_cluster_distance_row <- "binary"
        }

        if (!is.null(input$gs_cluster_method_row)) {
          gs_cluster_method_row <- input$gs_cluster_method_row
        } else {
          gs_cluster_method_row <- "average"
        }

        if (!is.null(input$gsplot_color_dend)) {
          gsplot_color_dend <- input$gsplot_color_dend
        } else {
          gsplot_color_dend <- "#000000"
        }

        if (!is.null(input$gsplot_background)) {
          gsplot_background <- input$gsplot_background
        } else {
          gsplot_background <- "#FFFFFF"
        }

        legend_gp <- gpar(
          col = gsplot_color_text,
          fill = c(gsplot_color_palette1, gsplot_color_palette2)
        )
        labels_gp <- gpar(
          col = gsplot_color_text,
          fontsize = gsplot_legend_labelsize
        )
        title_gp <- gpar(
          col = gsplot_color_text,
          fontsize = gsplot_legend_labelsize + 2
        )
        grid_height <- unit(gsplot_legend_labelsize * 0.8, "mm")
        grid_width <- unit(gsplot_legend_labelsize * 0.8, "mm")

        if (length(gs_plot_selected_isolate) < 10) {
          fontsize_row <- 14
        } else if (length(gs_plot_selected_isolate) < 20) {
          fontsize_row <- 12
        } else if (length(gs_plot_selected_isolate) < 30) {
          fontsize_row <- 11
        } else if (length(gs_plot_selected_isolate) < 50) {
          fontsize_row <- 10
        } else if (length(gs_plot_selected_isolate) < 80) {
          fontsize_row <- 9
        } else if (length(gs_plot_selected_isolate) < 120) {
          fontsize_row <- 8
        } else if (length(gs_plot_selected_isolate) < 160) {
          fontsize_row <- 7
        } else if (length(gs_plot_selected_isolate) < 200) {
          fontsize_row <- 6
        } else {
          fontsize_row <- 5
        }

        col_count <- length(c(
          gs_plot_selected_amr,
          gs_plot_selected_vir,
          gs_plot_selected_noclass
        ))
        if (col_count < 10) {
          fontsize_col <- 15
        } else if (col_count < 20) {
          fontsize_col <- 13
        } else if (col_count < 30) {
          fontsize_col <- 12
        } else if (col_count < 50) {
          fontsize_col <- 10
        } else if (col_count < 80) {
          fontsize_col <- 9
        } else if (col_count < 120) {
          fontsize_col <- 8
        } else if (col_count < 160) {
          fontsize_col <- 7
        } else if (col_count < 200) {
          fontsize_col <- 6
        } else {
          fontsize_col <- 5
        }

        fontsize_legend <- 2

        if (isTRUE(gs_cluster_row)) {
          row_order <- NULL
        } else {
          row_order <- rownames(heatmap_mat)
        }

        amr_order <- sort(arrange(hm_meta, amr))
        column_order_amr <- rownames(amr_order)[!is.na(amr_order$amr)]
        vir_order <- sort(arrange(hm_meta, vir))
        column_order_vir <- rownames(vir_order)[!is.na(vir_order$vir)]

        # heatmap annotations
        if (!is.null(input$gs_var_mapping)) {
          gs_var_mapping <- input$gs_var_mapping
        } else {
          gs_var_mapping <- "None"
        }

        isolate_annotation <- NULL
        isolate_annotation_legend <- NULL
        if (!is.null(input$gs_var_mapping)) {
          if (gs_var_mapping != "None") {
            isolate_meta <- DB$meta[
              which(
                DB$meta$`Assembly ID` %in%
                  rownames(heatmap_mat)
              ),
            ]

            rownames(isolate_meta) <- rownames(heatmap_mat)

            sel_isolate_var <- isolate_meta[[gs_var_mapping]]
            dist_col <- length(unique(sel_isolate_var))
            if (all(sel_isolate_var == "")) {
              var_colors <- "grey"
              names(var_colors) <- "NA"
            } else {
              if (!is.null(input$gs_mapping_scale)) {
                if (input$gs_mapping_scale %in% unlist(gradient_scales)) {
                  var_colors <- get(input$gs_mapping_scale)(dist_col)
                } else {
                  if (dist_col < 3) {
                    var_colors <- brewer.pal(3, input$gs_mapping_scale)[
                      1:dist_col
                    ]
                  } else {
                    var_colors <- brewer.pal(dist_col, input$gs_mapping_scale)
                  }
                }
              } else {
                var_colors <- get("viridis")(dist_col)
              }
              names(var_colors) <- unique(sort(sel_isolate_var))
            }

            isolate_annotation <- HeatmapAnnotation(
              Var = sel_isolate_var,
              show_legend = FALSE,
              col = list(Var = var_colors),
              show_annotation_name = TRUE,
              annotation_label = gs_var_mapping,
              annotation_name_gp = gpar(col = gsplot_color_text),
              which = "row"
            )

            isolate_annotation_legend <- packLegend(
              Legend(
                at = names(var_colors),
                labels = names(var_colors),
                title = gs_var_mapping,
                labels_gp = labels_gp,
                title_gp = title_gp,
                grid_height = grid_height,
                grid_width = grid_width,
                legend_gp = gpar(fill = var_colors)
              )
            )
          }
        }

        amr_annotation <- NULL
        amr_heatmap <- NULL
        sel_amr <- NULL
        if (!is.null(gs_plot_selected_amr)) {
          if (all(is.na(hm_meta$amr)) || length(gs_plot_selected_amr) < 3) {
            amr_annotation <- NULL
            amr_heatmap <- NULL
          } else {
            if (gs_amr_variables != "None") {
              sel_amr <- amr_order$amr[!is.na(amr_order$amr)]
              dist_col <- length(unique(sel_amr))

              if (!is.null(input$gs_amrclass_scale)) {
                if (input$gs_amrclass_scale %in% unlist(gradient_scales)) {
                  amr_colors <- get(input$gs_amrclass_scale)(length(
                    unique(sel_amr)
                  ))
                } else {
                  if (dist_col < 3) {
                    amr_colors <- brewer.pal(
                      3,
                      input$gs_amrclass_scale
                    )[1:dist_col]
                  } else {
                    amr_colors <- brewer.pal(dist_col, input$gs_amrclass_scale)
                  }
                }
              } else {
                if (length(unique(Screening$hm_meta$amr)) > 7) {
                  amr_colors <- get("turbo")(length(unique(sel_amr)))
                } else {
                  if (dist_col < 3) {
                    amr_colors <- brewer.pal(3, "Set1")[1:dist_col]
                  } else {
                    amr_colors <- brewer.pal(dist_col, "Set1")
                  }
                }
              }
              names(amr_colors) <- unique(sel_amr)
              amr_annotation <- HeatmapAnnotation(
                AMR = sel_amr,
                show_legend = TRUE,
                col = list(AMR = amr_colors),
                show_annotation_name = FALSE,
                annotation_legend_param = list(
                  title = "AMR",
                  labels = gsub("\\.", "-", names(amr_colors)),
                  labels_gp = labels_gp,
                  title_gp = title_gp,
                  grid_height = grid_height,
                  grid_width = grid_width,
                  legend_gp = gpar(fill = amr_colors)
                )
              )
            }

            # AMR heatmap
            amr_profile_matrix <- NULL
            amr_genes <- rownames(hm_meta)[!is.na(hm_meta$amr)]
            amr_profile_matrix_unordered <- heatmap_mat[,
              colnames(
                heatmap_mat
              ) %in%
                amr_genes
            ]
            amr_profile_matrix <- amr_profile_matrix_unordered[, c(
              column_order_amr
            )]

            if (
              (all(amr_profile_matrix == 0)) || all(amr_profile_matrix == 1)
            ) {
              amr_cols <- gsplot_color_palette1
            } else {
              amr_cols <- c(gsplot_color_palette1, gsplot_color_palette2)
            }

            amr_heatmap <- ComplexHeatmap::Heatmap(
              amr_profile_matrix,
              col = amr_cols,
              rect_gp = gpar(col = gsplot_grid_color, lwd = gsplot_grid_width),
              column_title = "AMR",
              column_title_gp = gpar(
                col = gsplot_color_text,
                fontsize = gsplot_fontsize_title
              ),
              row_title = "Isolates",
              row_title_gp = gpar(
                col = gsplot_color_text,
                fontsize = gsplot_fontsize_title
              ),
              row_names_gp = gpar(
                fontsize = fontsize_row,
                col = gsplot_color_text
              ),
              column_names_gp = gpar(
                fontsize = fontsize_col,
                col = gsplot_color_text
              ),
              cluster_columns = gs_cluster_col,
              clustering_distance_columns = gs_cluster_distance_col,
              clustering_method_columns = gs_cluster_method_col,
              cluster_rows = gs_cluster_row,
              clustering_distance_rows = gs_cluster_distance_row,
              clustering_method_rows = gs_cluster_method_row,
              column_dend_height = unit(gsplot_treeheight_col, "cm"),
              row_dend_width = unit(gsplot_treeheight_row, "cm"),
              row_dend_gp = gpar(col = gsplot_color_dend),
              column_dend_gp = gpar(col = gsplot_color_dend),
              top_annotation = amr_annotation,
              show_heatmap_legend = FALSE,
              left_annotation = isolate_annotation,
              row_order = row_order,
              row_labels = DB$data[c(gsplot_isolate_label)][
                which(
                  DB$data$`Assembly ID` %in% rownames(amr_profile_matrix)
                ),
              ]
            )
          }
        } else {
          amr_annotation <- NULL
          amr_heatmap <- NULL
        }

        vir_annotation <- NULL
        vir_heatmap <- NULL
        if (!is.null(gs_plot_selected_vir)) {
          if (all(is.na(hm_meta$vir)) || length(gs_plot_selected_vir) < 3) {
            vir_annotation <- NULL
            vir_heatmap <- NULL
          } else {
            if (gs_vir_variables != "None") {
              sel_vir <- vir_order$vir[!is.na(vir_order$vir)]
              dist_col <- length(unique(sel_vir))

              if (!is.null(input$gs_virclass_scale)) {
                if (input$gs_virclass_scale %in% unlist(gradient_scales)) {
                  vir_colors <- get(input$gs_virclass_scale)(length(
                    unique(sel_vir)
                  ))
                } else {
                  if (dist_col < 3) {
                    vir_colors <- brewer.pal(
                      3,
                      input$gs_virclass_scale
                    )[1:dist_col]
                  } else {
                    vir_colors <- brewer.pal(dist_col, input$gs_virclass_scale)
                  }
                }
              } else {
                if (length(unique(Screening$hm_meta$vir)) > 7) {
                  vir_colors <- get("turbo")(length(unique(sel_vir)))
                } else {
                  if (dist_col < 3) {
                    vir_colors <- brewer.pal(3, "Set1")[1:dist_col]
                  } else {
                    vir_colors <- brewer.pal(dist_col, "Set1")
                  }
                }
              }
              names(vir_colors) <- unique(sel_vir)

              vir_annotation <- HeatmapAnnotation(
                Vir = sel_vir,
                show_legend = TRUE,
                col = list(Vir = vir_colors),
                show_annotation_name = FALSE,
                annotation_legend_param = list(
                  title = "Virulence",
                  legend_gp = legend_gp,
                  labels_gp = labels_gp,
                  title_gp = title_gp,
                  grid_height = grid_height,
                  grid_width = grid_width,
                  legend_gp = gpar(fill = vir_colors)
                )
              )
            }

            # Vir heatmap
            vir_profile_matrix <- NULL
            vir_genes <- rownames(hm_meta)[!is.na(hm_meta$vir)]
            vir_profile_matrix <- heatmap_mat[,
              colnames(
                heatmap_mat
              ) %in%
                vir_genes
            ]
            vir_profile_matrix_unordered <- heatmap_mat[,
              colnames(
                heatmap_mat
              ) %in%
                vir_genes
            ]
            vir_profile_matrix <- vir_profile_matrix_unordered[, c(
              column_order_vir
            )]

            if (
              (all(vir_profile_matrix == 0)) || all(vir_profile_matrix == 1)
            ) {
              vir_cols <- gsplot_color_palette1
            } else {
              vir_cols <- c(gsplot_color_palette1, gsplot_color_palette2)
            }

            if (all(is.na(hm_meta$amr)) || length(gs_plot_selected_amr) < 3) {
              left_annotation <- isolate_annotation
            } else {
              left_annotation <- NULL
            }

            vir_heatmap <- ComplexHeatmap::Heatmap(
              vir_profile_matrix,
              col = vir_cols,
              rect_gp = gpar(col = gsplot_grid_color, lwd = gsplot_grid_width),
              column_title = "Virulence",
              column_title_gp = gpar(
                col = gsplot_color_text,
                fontsize = gsplot_fontsize_title
              ),
              row_title = "Isolates",
              row_title_gp = gpar(
                col = gsplot_color_text,
                fontsize = gsplot_fontsize_title
              ),
              row_names_gp = gpar(
                fontsize = fontsize_row,
                col = gsplot_color_text
              ),
              column_names_gp = gpar(
                fontsize = fontsize_col,
                col = gsplot_color_text
              ),
              column_dend_height = unit(gsplot_treeheight_col, "cm"),
              cluster_columns = gs_cluster_col,
              clustering_distance_columns = gs_cluster_distance_col,
              clustering_method_columns = gs_cluster_method_col,
              cluster_rows = gs_cluster_row,
              row_order = row_order,
              clustering_distance_rows = gs_cluster_distance_row,
              clustering_method_rows = gs_cluster_method_row,
              row_dend_width = unit(gsplot_treeheight_row, "cm"),
              row_dend_gp = gpar(col = gsplot_color_dend),
              column_dend_gp = gpar(col = gsplot_color_dend),
              top_annotation = vir_annotation,
              show_heatmap_legend = FALSE,
              left_annotation = left_annotation,
              row_labels = DB$data[c(gsplot_isolate_label)][
                which(
                  DB$data$`Assembly ID` %in% rownames(vir_profile_matrix)
                ),
              ]
            )
          }
        } else {
          vir_annotation <- NULL
          vir_heatmap <- NULL
        }

        # None heatmap
        noclass_heatmap <- NULL
        noclass_profile_matrix <- NULL
        if (!is.null(gs_plot_selected_noclass)) {
          if (any(is.na(hm_meta$vir) && is.na(hm_meta$amr))) {
            unclass_genes <- rownames(hm_meta)[
              is.na(hm_meta$vir) &&
                is.na(hm_meta$amr)
            ]
            noclass_profile_matrix <- heatmap_mat[,
              colnames(heatmap_mat) %in%
                unclass_genes
            ]

            if (
              (all(is.na(hm_meta$amr)) || length(gs_plot_selected_amr) < 3) &&
                (all(is.na(hm_meta$vir)) || length(gs_plot_selected_vir) < 3)
            ) {
              left_annotation <- isolate_annotation
            } else {
              left_annotation <- NULL
            }

            if (
              (all(noclass_profile_matrix == 0)) ||
                all(noclass_profile_matrix == 1)
            ) {
              noclass_cols <- gsplot_color_palette1
            } else {
              noclass_cols <- c(gsplot_color_palette1, gsplot_color_palette2)
            }

            noclass_heatmap <- ComplexHeatmap::Heatmap(
              noclass_profile_matrix,
              col = noclass_cols,
              rect_gp = gpar(col = gsplot_grid_color, lwd = gsplot_grid_width),
              row_title = "Isolates",
              row_title_gp = gpar(
                col = gsplot_color_text,
                fontsize = gsplot_fontsize_title
              ),
              row_names_gp = gpar(
                fontsize = fontsize_row,
                col = gsplot_color_text
              ),
              column_names_gp = gpar(
                fontsize = fontsize_col,
                col = gsplot_color_text
              ),
              column_dend_height = unit(gsplot_treeheight_col, "cm"),
              cluster_columns = gs_cluster_col,
              clustering_distance_columns = gs_cluster_distance_col,
              clustering_method_columns = gs_cluster_method_col,
              cluster_rows = gs_cluster_row,
              row_order = row_order,
              clustering_distance_rows = gs_cluster_distance_row,
              clustering_method_rows = gs_cluster_method_row,
              row_dend_width = unit(gsplot_treeheight_row, "cm"),
              row_dend_gp = gpar(col = gsplot_color_dend),
              column_dend_gp = gpar(col = gsplot_color_dend),
              show_heatmap_legend = FALSE,
              left_annotation = left_annotation,
              row_labels = DB$data[c(gsplot_isolate_label)][
                which(
                  DB$data$`Assembly ID` %in% rownames(noclass_profile_matrix)
                ),
              ]
            )
          } else {
            noclass_heatmap <- NULL
          }
        } else {
          noclass_heatmap <- NULL
        }

        # custom legend
        custom_legend <- packLegend(
          Legend(
            labels = c("Present", "Absent"),
            title = "Gene Presence",
            legend_gp = gpar(
              col = gsplot_color_text,
              fill = c(gsplot_color_palette1, gsplot_color_palette2)
            ),
            labels_gp = gpar(
              col = gsplot_color_text,
              fontsize = gsplot_legend_labelsize + 1
            ),
            title_gp = gpar(
              col = gsplot_color_text,
              fontsize = gsplot_legend_labelsize + 3,
              fontface = "bold"
            ),
            grid_height = unit((gsplot_legend_labelsize + 1) * 0.8, "mm"),
            grid_width = unit((gsplot_legend_labelsize + 1) * 0.8, "mm"),
            at = c(1, 0),
            nrow = 1,
            direction = "horizontal",
            title_position = "lefttop"
          )
        )

        # summarize heatmaps
        if (
          is.null(amr_heatmap) &&
            is.null(vir_heatmap) &&
            is.null(noclass_heatmap)
        ) {
          output$gs_plot <- NULL
        } else {
          if (
            !is.null(amr_heatmap) &&
              !is.null(vir_heatmap) &&
              !is.null(noclass_heatmap)
          ) {
            heatmaps <- amr_heatmap + vir_heatmap + noclass_heatmap
          } else if (
            !is.null(amr_heatmap) &&
              !is.null(vir_heatmap) &&
              is.null(noclass_heatmap)
          ) {
            heatmaps <- amr_heatmap + vir_heatmap
          } else if (
            !is.null(amr_heatmap) &&
              is.null(vir_heatmap) &&
              !is.null(noclass_heatmap)
          ) {
            heatmaps <- amr_heatmap + noclass_heatmap
          } else if (
            is.null(amr_heatmap) &&
              !is.null(vir_heatmap) &&
              !is.null(noclass_heatmap)
          ) {
            heatmaps <- vir_heatmap + noclass_heatmap
          } else if (
            is.null(amr_heatmap) &&
              is.null(vir_heatmap) &&
              !is.null(noclass_heatmap)
          ) {
            heatmaps <- noclass_heatmap
          } else if (
            !is.null(amr_heatmap) &&
              is.null(vir_heatmap) &&
              is.null(noclass_heatmap)
          ) {
            heatmaps <- amr_heatmap
          } else if (
            is.null(amr_heatmap) &&
              !is.null(vir_heatmap) &&
              is.null(noclass_heatmap)
          ) {
            heatmaps <- vir_heatmap
          }

          if (is.null(isolate_annotation_legend)) {
            ComplexHeatmap::draw(
              heatmaps,
              background = gsplot_background,
              heatmap_legend_list = custom_legend,
              heatmap_legend_side = "bottom",
              padding = unit(c(2, 2, 2, 2), "mm")
            )
          } else {
            ComplexHeatmap::draw(
              heatmaps,
              background = gsplot_background,
              heatmap_legend_list = custom_legend,
              heatmap_legend_side = "bottom",
              annotation_legend_list = isolate_annotation_legend,
              annotation_legend_side = "right"
            )
          }
        }
      }
    } else {
      NULL
    }
  })

  # make plot
  output$gs_plot <- renderPlot({
    gs_plot()
  })

  # _______________________ ####

  ## Typing  ----

  # Render Single/Multi Switch

  readLogFile <- reactive({
    invalidateLater(5000, session)
    readLines(file.path(logdir, "script_log.txt"))
  })

  # No db typing message
  output$typing_no_db <- renderUI({
    if (!is.null(DB$exist)) {
      if (DB$exist) {
        column(
          width = 4,
          align = "left",
          br(),
          br(),
          br(),
          br(),
          p(
            HTML(
              paste0(
                tags$span(
                  style = 'color: white; font-size: 15px; margin-bottom: 0px; margin-left: 50px',
                  'To initiate allelic typing, a cgMLST scheme must be downloaded first.'
                )
              )
            )
          )
        )
      } else {
        NULL
      }
    } else {
      NULL
    }
  })

  ### Typing UI Elements ----
  output$initiate_multi_typing_ui <- initiate_multi_typing_ui

  # Render selection info
  output$multi_folder_sel_info <- renderUI({
    req(Typing$assembly_folder_path, Typing$file_selection)

    if (Typing$file_selection == "folder") {
      if (length(Typing$assembly_folder_path) < 1) {
        HTML(paste(
          "<span style='color: white; position:relative; top:8px; font-style: italic'>",
          "No files selected"
        ))
      } else {
        HTML(paste(
          "<span style='color: white; position:relative; top:8px; font-style: italic'>",
          '<i class="fa-solid fa-arrow-right" style="font-size:15px;color:white"></i>',
          "&nbsp",
          length(Typing$files_filtered),
          if (length(Typing$files_filtered) == 1) {
            " file selected"
          } else {
            " files selected"
          }
        ))
      }
    } else {
      NULL
    }
  })

  output$multi_file_sel_info <- renderUI({
    req(Typing$assembly_files_path, Typing$file_selection)

    if (Typing$file_selection == "files") {
      if (nrow(Typing$assembly_files_path) < 1) {
        HTML(paste(
          "<span style='color: white; position:relative; top:7px; font-style: italic'>",
          "No files selected"
        ))
      } else {
        HTML(paste(
          "<span style='color: white; position:relative; top:7px; font-style: italic'>",
          '<i class="fa-solid fa-arrow-right" style="font-size:15px;color:white"></i>',
          "&nbsp",
          length(Typing$files_filtered),
          if (length(Typing$files_filtered) == 1) {
            " file selected"
          } else {
            " files selected"
          }
        ))
      }
    } else {
      NULL
    }
  })

  # Render multi selection table issues
  output$multi_select_issues <- renderUI({
    req(Typing$multi_sel_table, input$multi_select_table)

    if (
      any(
        hot_to_r(input$multi_select_table)$Files %in%
          unlist(DB$data["Assembly ID"])
      ) &&
        any(duplicated(hot_to_r(input$multi_select_table)$Files))
    ) {
      HTML(
        paste(
          paste(
            "<span style='color: orange; position:relative; top:2px'>",
            "Some name(s) are already present in local database<br/>"
          ),
          paste("<span style='color: #ff7334;'>", "Duplicated name(s). <br/>")
        )
      )
    } else if (
      any(
        hot_to_r(input$multi_select_table)$Files %in%
          unlist(DB$data["Assembly ID"])
      ) &&
        !any(duplicated(hot_to_r(input$multi_select_table)$Files))
    ) {
      HTML(
        paste(
          "<span style='color: #e0b300; position:relative; top:2px'>",
          "Some name(s) are already present in local database<br/>"
        )
      )
    } else if (
      !any(
        hot_to_r(input$multi_select_table)$Files %in%
          dupl_mult_id()
      ) &&
        any(duplicated(hot_to_r(input$multi_select_table)$Files))
    ) {
      HTML(
        paste(
          "<span style='color: #ff7334; position:relative; top:2px'>",
          "Duplicated name(s) <br/>"
        )
      )
    }
  })

  output$multi_select_issue_info <- renderUI({
    req(Typing$multi_sel_table, input$multi_select_table)

    multi_select_table <- hot_to_r(input$multi_select_table)

    if (
      any(
        multi_select_table$Files[which(
          multi_select_table$Include == TRUE
        )] %in%
          dupl_mult_id()
      ) ||
        any(duplicated(multi_select_table$Files[which(
          multi_select_table$Include == TRUE
        )])) ||
        any(grepl(
          " ",
          multi_select_table$Files[which(
            multi_select_table$Include == TRUE
          )]
        ))
    ) {
      disable("conf_meta_multi")

      if (
        any(grepl(
          " ",
          multi_select_table$Files[which(
            multi_select_table$Include == TRUE
          )]
        ))
      ) {
        if (
          any(
            multi_select_table$Files[which(
              multi_select_table$Include == TRUE
            )] %in%
              dupl_mult_id()
          ) ||
            any(duplicated(multi_select_table$Files[which(
              multi_select_table$Include == TRUE
            )]))
        ) {
          HTML(paste(
            paste(
              '<i class="fa-solid fa-circle-exclamation" style="font-size:15px;color:orange"></i>',
              paste(
                "<span style='color: white; font-style:italic'>",
                "&nbspRename highlighted isolates or deselect them</br>"
              )
            ),
            paste(
              '<i class="fa-solid fa-circle-exclamation" style="font-size:15px;color:orange"></i>',
              paste(
                "<span style='color: white; font-style:italic'>",
                "&nbspFilename(s) contain(s) empty spaces"
              )
            )
          ))
        } else {
          HTML(paste(
            '<i class="fa-solid fa-circle-exclamation" style="font-size:15px;color:orange"></i>',
            paste(
              "<span style='color: white; font-style:italic'>",
              "&nbspFilename(s) contain(s) empty spaces"
            )
          ))
        }
      } else {
        HTML(paste(
          '<i class="fa-solid fa-circle-exclamation" style="font-size:15px;color:orange"></i>',
          paste(
            "<span style='color: white; font-style:italic'>",
            "&nbspRename highlighted isolates or deselect them"
          )
        ))
      }
    } else {
      enable("conf_meta_multi")
      HTML(paste(
        '<i class="fa-solid fa-circle-check" style="font-size:15px;color:lightgreen"></i>',
        paste(
          "<span style='color: white; font-style:italic'>",
          "&nbspFiles ready for allelic typing"
        )
      ))
    }
  })

  # Render Metadata Select Box after Folder selection
  observe({
    if (!is.null(Typing$multi_sel_table)) {
      if (nrow(Typing$multi_sel_table) > 0) {
        output$multi_select_tab_ctrls <- renderUI(
          fluidRow(
            h3(
              p("Metadata Declaration"),
              style = "color:white; margin-left: 15px"
            ),
            br(),
            column(
              width = 2,
              align = "left",
              actionButton(
                "sel_all_mt",
                "All",
                icon = icon("check")
              )
            ),
            column(
              width = 2,
              align = "left",
              actionButton(
                "desel_all_mt",
                "None",
                icon = icon("xmark")
              )
            ),
            column(
              width = 8,
              align = "center",
              br(),
              uiOutput("multi_select_issues")
            )
          )
        )

        output$metadata_multi_box <- renderUI({
          column(
            width = 11,
            align = "left",
            hr(),
            br(),
            uiOutput("multi_select_issue_info"),
            br(),
            br(),
            actionButton(
              inputId = "conf_meta_multi",
              label = "Confirm",
              icon = icon("arrow-right")
            )
          )
        })
      } else {
        output$metadata_multi_box <- NULL
      }
    }
  })

  trigger_multi_select_table <- shiny::reactiveVal(1)

  # Check if ongoing Multi Typing - Render accordingly
  observe({
    req(Typing$file_selection)

    file_selection <- Typing$file_selection

    # Folder selection
    shinyDirChoose(
      input,
      "assembly_folder",
      roots = c(Home = path_home(), Root = "/"),
      defaultRoot = "Home",
      session = session,
      filetypes = c('', 'fasta', 'fna', 'fa')
    )

    Typing$assembly_folder_path <- assembly_folder_path <- parseDirPath(
      roots = c(Home = path_home(), Root = "/"),
      input$assembly_folder
    )

    # File(s) selection
    shinyFileChoose(
      input,
      "assembly_files",
      roots = c(Home = path_home(), Root = "/"),
      defaultRoot = "Home",
      session = session,
      filetypes = c('', 'fasta', 'fna', 'fa')
    )

    Typing$assembly_files_path <- assembly_files_path <- parseFilePaths(
      roots = c(Home = path_home(), Root = "/"),
      input$assembly_files
    )

    # Format selection
    if (file_selection != "") {
      if (
        (!is.null(assembly_files_path) ||
          !is.null(assembly_folder_path)) &&
          !is.null(file_selection)
      ) {
        if (file_selection == "files") {
          Typing$files_filtered <- files_filtered <- assembly_files_path$name[which(
            !endsWith(assembly_files_path$name, ".gz") &
              grepl("\\.fasta|\\.fna|\\.fa", assembly_files_path$name)
          )]
        } else if (file_selection == "folder") {
          files_selected <- list.files(as.character(
            assembly_folder_path
          ))
          Typing$files_filtered <- files_filtered <- files_selected[which(
            !endsWith(files_selected, ".gz") &
              grepl("\\.fasta|\\.fna|\\.fa", files_selected)
          )]
        }
      }
    }

    multi_sel_table <- data.frame(
      Include = rep(TRUE, length(files_filtered)),
      Files = gsub(
        ".fasta|.fna|.fa|.fasta.gz|.fna.gz|.fa.gz",
        "",
        files_filtered
      ),
      Type = sub(
        ".*(\\.fasta|\\.fasta\\.gz|\\.fna|\\.fna\\.gz|\\.fa|\\.fa\\.gz)$",
        "\\1",
        files_filtered,
        perl = FALSE
      ),
      Host = rep("", length(files_filtered)),
      Country = rep("", length(files_filtered)),
      City = rep("", length(files_filtered)),
      Isolation.Date = rep(format(Sys.Date()), length(files_filtered))
    )
    colnames(multi_sel_table)[7] <- "Isolation Date"

    Typing$multi_sel_table <- multi_sel_table

    # output$multi_select_table <- renderRHandsontable({
    #   message(TRUE, Sys.time())

    #   make_typing_select_handsontable(
    #     Typing$multi_sel_table,
    #     dupl_mult_id = dupl_mult_id()
    #   )
    # })

    isolate({
      message(Sys.time())
      trigger_multi_select_table(trigger_multi_select_table() + 1)
    })

    output$multi_select_table <- renderRHandsontable({
      message(TRUE, Sys.time())
      make_typing_select_handsontable(
        Typing$multi_sel_table,
        dupl_mult_id = dupl_mult_id()
      )
    }) |>
      shiny::bindEvent(
        trigger_multi_select_table(),
        Typing$multi_sel_table,
        dupl_mult_id(),
        ignoreNULL = FALSE
      )

    output$multi_select_tab_ctrls <- renderUI({
      req(nrow(Typing$multi_sel_table) > 0)

      fluidRow(
        h3(
          p("Metadata Declaration"),
          style = "color:white; margin-left: 15px"
        ),
        br(),
        column(
          width = 2,
          align = "left",
          actionButton(
            "sel_all_mt",
            "All",
            icon = icon("check")
          )
        ),
        column(
          width = 2,
          align = "left",
          actionButton(
            "desel_all_mt",
            "None",
            icon = icon("xmark")
          )
        ),
        column(
          width = 10,
          align = "center",
          br(),
          uiOutput("multi_select_issues")
        )
      )
    }) |>
      shiny::bindEvent(
        Typing$multi_sel_table
      )
  })

  ### Typing Events ----

  # Confirm typing metadata
  observeEvent(input$conf_meta_multi, {
    runjs(block_ui)

    multi_select_table <- hot_to_r(input$multi_select_table)[
      which(
        hot_to_r(input$multi_select_table)$Include == TRUE
      ),
    ]

    # Safety checks
    if (
      any(
        unlist(gsub(
          ".fasta|.fna|.fa|.fasta.gz|.fna.gz|.fa.gz",
          "",
          multi_select_table$Files
        )) %in%
          unlist(DB$data["Assembly ID"])
      )
    ) {
      show_toast(
        title = "Assembly ID(s) already present",
        type = "error",
        position = "bottom-end",
        timer = 3000
      )
    } else if (any(duplicated(multi_select_table$Files))) {
      show_toast(
        title = "Duplicated filename(s)",
        type = "error",
        position = "bottom-end",
        timer = 3000
      )
    } else if (any(multi_select_table$Files == "")) {
      show_toast(
        title = "Empty filename(s)",
        type = "error",
        position = "bottom-end",
        timer = 3000
      )
    } else if (any(grepl("[()/\\:*?\"<>|]", multi_select_table$Files))) {
      show_toast(
        title = "Invalid filename(s). No special characters allowed: ()/\\:*?\"<>|",
        type = "error",
        position = "bottom-end",
        timer = 3000
      )
    } else if (!any(multi_select_table$Include == TRUE)) {
      show_toast(
        title = "No files selected",
        type = "error",
        position = "bottom-end",
        timer = 3000
      )
    } else if (
      any(grepl(
        " ",
        multi_select_table$Files[which(
          multi_select_table$Include == TRUE
        )]
      ))
    ) {
      show_toast(
        title = "Empty spaces in filename(s) not allowed",
        type = "error",
        position = "bottom-end",
        timer = 3000
      )
    } else if (isFALSE(Typing$reload)) {
      show_toast(
        title = "Reload Database first",
        type = "warning",
        position = "bottom-end",
        timer = 6000
      )
    } else if (Screening$status == "started") {
      show_toast(
        title = "Pending Single Typing",
        type = "warning",
        position = "bottom-end",
        timer = 6000
      )
    } else {
      showModal(
        div(
          class = "typing-modal",
          modalDialog(
            column(
              width = 12,
              align = "left",
              br(),
              br(),
              HTML(
                paste(
                  '<i class="fa-solid fa-circle-exclamation" style="font-size:15px;color:white"></i>',
                  "<span style='color: white;'>",
                  "&nbsp",
                  nrow(multi_select_table),
                  " assemblies queried for Allelic Typing\n"
                )
              ),
              br(),
              br(),
              HTML(
                paste(
                  '<i class="fa-solid fa-circle-exclamation" style="font-size:15px;color:white"></i>',
                  "<span style='color: white;'>",
                  "&nbsp",
                  "Typing by <strong>",
                  DB$scheme,
                  "</strong> scheme."
                )
              ),
              br(),
              br(),
              br(),
              div(
                class = "save-assembly",
                materialSwitch(
                  "save_assembly_mt",
                  h5(
                    p("Save Assemblies in Local Database"),
                    style = "color:white; padding-left: 0px; position: relative; top: -3px; right: -20px;"
                  ),
                  value = TRUE,
                  right = TRUE
                )
              ),
              HTML(
                paste(
                  "<span style='color: orange;font-style:italic'>",
                  "Isolates with unsaved assembly files can NOT be applied to screening for resistance genes."
                )
              ),
              br(),
              br(),
              br(),
              br()
            ),
            title = "Start Allelic Typing",
            fade = TRUE,
            easyClose = TRUE,
            footer = tagList(
              modalButton("Cancel"),
              actionButton(
                "start_typ_multi",
                "Start",
                icon = icon("circle-play")
              )
            )
          )
        )
      )
    }

    runjs(unblock_ui)
  })

  # Set reactive variable to distinguish files/folder selection
  observeEvent(input$assembly_files, {
    Typing$file_selection <- "files"
  })

  observeEvent(input$assembly_folder, {
    Typing$file_selection <- "folder"
  })

  observeEvent(input$sel_all_mt, {
    Typing$multi_sel_table$Include <- TRUE

    # session$sendCustomMessage(
    #   type = "setColumnValue",
    #   message = list(value = TRUE)
    # )
  })

  observeEvent(input$desel_all_mt, {
    Typing$multi_sel_table$Include <- FALSE

    # session$sendCustomMessage(
    #   type = "setColumnValue",
    #   message = list(value = FALSE)
    # )
  })

  # Print Log
  output$print_log <- downloadHandler(
    filename = function() {
      log_print(paste0(
        "Save multi typing log ",
        paste("Multi_Typing_", Sys.Date(), ".txt", sep = "")
      ))
      paste0(Sys.Date(), "_", gsub(" ", "_", DB$scheme), "_Typing_Log.txt")
    },
    content = function(file) {
      writeLines(readLines(file.path(logdir, "script_log.txt")), file)
    }
  )

  # Reset Multi Typing
  observeEvent(input$reset_multi, {
    if (
      !grepl(
        "Multi Typing",
        tail(
          readLines(file.path(
            logdir,
            "script_log.txt"
          )),
          n = 1
        )
      )
    ) {
      showModal(
        div(
          class = "start-modal",
          modalDialog(
            fluidRow(
              br(),
              column(
                width = 11,
                p(
                  HTML(
                    paste0(
                      '<span style="color: white; display: block; font-size: 15px; margin-left: 15px;">',
                      "A Multi Typing process is still pending. Stopping this process will cancel the processing.",
                      '</span>'
                    )
                  )
                )
              ),
              br()
            ),
            title = "Reset Multi Typing",
            fade = TRUE,
            easyClose = TRUE,
            footer = tagList(
              modalButton("Cancel"),
              actionButton("conf_multi_kill", "Stop", class = "btn btn-danger")
            )
          )
        )
      )
    } else {
      log_print("Reset multi typing")
      delay(1000, runjs("unhighlight_typing();"))

      # Reset multi typing result list
      saveRDS(list(), file.path(app_local_share_path, "event_list.rds"))
      multi_help <- FALSE
      Typing$result_list <- NULL

      # Null logfile
      writeLines("0", file.path(logdir, "script_log.txt"))

      # Reset User Feedback variable
      Typing$multi_started <- FALSE

      output$initiate_multi_typing_ui <- initiate_multi_typing_ui

      output$pending_typing <- NULL
      output$multi_typing_results <- NULL
    }
  })

  # Confirm Reset after
  observeEvent(input$conf_multi_kill, {
    runjs(block_ui)
    removeModal()
    delay(1000, runjs("unhighlight_typing();"))
    log_print("Kill multi typing")

    # Kill multi typing and reset logfile
    system(
      paste("bash", shQuote(paste0(getwd(), "/bin/kill_multi.sh"))),
      wait = TRUE
    )

    show_toast(
      title = "Execution cancelled",
      type = "success",
      position = "bottom-end",
      timer = 6000
    )

    # Kill multi typing and reset logfile
    writeLines("0", file.path(logdir, "script_log.txt"))

    #Reset multi typing result list
    saveRDS(list(), file.path(app_local_share_path, "event_list.rds"))
    multi_help <- FALSE
    Typing$result_list <- NULL

    # Reset User Feedback variable
    output$pending_typing <- NULL
    output$multi_typing_results <- NULL
    Typing$multi_started <- FALSE

    output$initiate_multi_typing_ui <- initiate_multi_typing_ui

    runjs(unblock_ui)
  })

  observeEvent(input$start_typ_multi, {
    log_print("Initiate multi typing")

    if (Screening$status == "started") {
      show_toast(
        title = "Pending Gene Screening",
        type = "warning",
        position = "bottom-end",
        timer = 6000
      )
    } else {
      removeModal()

      show_toast(
        title = "Multi Typing started",
        type = "info",
        position = "bottom-end",
        timer = 10000
      )

      delay(1000, runjs("highlight_typing();"))

      # Remove Allelic Typing Controls
      output$initiate_multi_typing_ui <- NULL
      output$metadata_multi_box <- NULL
      output$start_multi_typing_ui <- NULL

      # Activate entry detection
      DB$check_new_entries <- TRUE

      # Initiate Feedback variables
      Typing$multi_started <- TRUE
      Typing$pending <- TRUE

      # Arrange metadata
      multi_select_table <- hot_to_r(input$multi_select_table)

      filenames <- paste(
        multi_select_table$Files[which(
          multi_select_table$Include == TRUE
        )],
        collapse = " "
      )
      type <- Typing$multi_sel_table$Type[which(
        multi_select_table$Include == TRUE
      )]
      files <- Typing$multi_sel_table$Files[which(
        multi_select_table$Include == TRUE
      )]
      genome_names <- paste(paste0(gsub(" ", "~", files), type), collapse = " ")

      if (Typing$file_selection == "folder") {
        genome_folder <- as.character(parseDirPath(
          roots = c(Home = path_home(), Root = "/"),
          input$assembly_folder
        ))
      } else if (Typing$file_selection == "files") {
        genome_folder <- dirname(Typing$assembly_files_path$datapath[1])
      }

      # Save metadata RDS file for bash script
      multi_typing_df <- list(
        db_path = Startup$database,
        wd = getwd(),
        metadata = hot_to_r(input$multi_select_table)[
          hot_to_r(
            input$multi_select_table
          )$Include ==
            TRUE,
        ],
        save = input$save_assembly_mt,
        scheme = paste0(gsub(" ", "_", DB$scheme)),
        genome_folder = genome_folder,
        filenames = paste0(filenames, collapse = " "),
        genome_names = genome_names,
        alleles = file.path(
          Startup$database,
          gsub(" ", "_", DB$scheme),
          paste0(gsub(" ", "_", DB$scheme), "_alleles")
        )
      )

      saveRDS(
        multi_typing_df,
        file.path(app_local_share_path, "multi_typing_df.rds")
      )

      # Execute multi blat script
      system(
        paste("bash", shQuote(paste0(getwd(), "/bin/multi_typing.sh"))),
        wait = FALSE
      )
    }
  })

  ### User Feedback ----

  observe({
    if (file.exists(file.path(logdir, "script_log.txt"))) {
      if (isTRUE(Typing$multi_started)) {
        check_multi_status()
      } else {
        Typing$status <- "Inactive"
      }
    }
  })

  check_multi_status <- reactive({
    invalidateLater(3000, session)

    log <- readLines(file.path(logdir, "script_log.txt"))

    # Check typing status
    if (str_detect(tail(log, 1), "Attaching")) {
      Typing$status <- "Attaching"
    } else if (str_detect(tail(log, 1), "Successful")) {
      Typing$multi_help <- TRUE
      Typing$status <- "Successful"
      show_toast(
        title = paste0("Successful", sub(".*Successful", "", tail(log, 1))),
        type = "success",
        position = "bottom-end",
        timer = 8000
      )
    } else if (str_detect(tail(log, 1), "failed")) {
      Typing$status <- "Failed"
      show_toast(
        title = sub(".* - ", "", tail(log, 1)),
        type = "error",
        position = "bottom-end",
        timer = 8000
      )
    } else if (str_detect(tail(log, 1), "Processing")) {
      Typing$status <- "Processing"

      if (any(str_detect(tail(log, 2), "Successful"))) {
        if (!identical(Typing$last_success, tail(log, 2)[1])) {
          Typing$multi_help <- TRUE
          show_toast(
            title = paste0(
              "Successful",
              sub(".*Successful", "", tail(log, 2)[1])
            ),
            type = "success",
            position = "bottom-end",
            timer = 8000
          )

          Typing$last_success <- tail(log, 2)[1]
        }
      } else if (any(str_detect(tail(log, 2), "failed"))) {
        if (!identical(Typing$last_failure, tail(log, 2)[1])) {
          show_toast(
            title = sub(".* - ", "", tail(log, 2)[1]),
            type = "error",
            position = "bottom-end",
            timer = 8000
          )

          Typing$last_failure <- tail(log, 2)[1]
        }
      }
    } else if (str_detect(tail(log, 1), "finalized")) {
      Typing$multi_help <- TRUE
      Typing$status <- "Finalized"

      if (isTRUE(Typing$pending)) {
        show_toast(
          title = "Typing finalized",
          type = "success",
          position = "bottom-end",
          timer = 8000
        )

        Typing$pending <- FALSE
      }
    }
  })

  observe({
    if (!is.null(input$multi_results_picker)) {
      Typing$multi_table_length <- nrow(
        Typing$result_list[[input$multi_results_picker]]
      )
    } else {
      Typing$multi_table_length <- NULL
    }
  })

  observe({
    if (!is.null(Typing$result_list)) {
      if (length(Typing$result_list) > 0) {
        if (is.null(Typing$multi_table_length)) {
          output$multi_typing_result_table <- renderDataTable(
            Typing$result_list[[input$multi_results_picker]],
            selection = "single",
            options = list(
              pageLength = 10,
              scrollY = TRUE,
              columnDefs = list(list(searchable = TRUE, targets = "_all")),
              initComplete = DT::JS(
                "function(settings, json) {",
                "$('th:first-child').css({'border-top-left-radius': '5px'});",
                "$('th:last-child').css({'border-top-right-radius': '5px'});",
                "$('tbody tr:last-child td:first-child').css({'border-bottom-left-radius': '5px'});",
                "$('tbody tr:last-child td:last-child').css({'border-bottom-right-radius': '5px'});",
                "}"
              ),
              drawCallback = DT::JS(
                "function(settings) {",
                "$('tbody tr:last-child td:first-child').css({'border-bottom-left-radius': '5px'});",
                "$('tbody tr:last-child td:last-child').css({'border-bottom-right-radius': '5px'});",
                "}"
              )
            )
          )
        } else {
          if (Typing$multi_table_length > 15) {
            output$multi_typing_result_table <- renderDataTable(
              Typing$result_list[[input$multi_results_picker]],
              selection = "single",
              options = list(
                pageLength = 10,
                scrollY = TRUE,
                columnDefs = list(list(searchable = TRUE, targets = "_all")),
                initComplete = DT::JS(
                  "function(settings, json) {",
                  "$('th:first-child').css({'border-top-left-radius': '5px'});",
                  "$('th:last-child').css({'border-top-right-radius': '5px'});",
                  "$('tbody tr:last-child td:first-child').css({'border-bottom-left-radius': '5px'});",
                  "$('tbody tr:last-child td:last-child').css({'border-bottom-right-radius': '5px'});",
                  "}"
                ),
                drawCallback = DT::JS(
                  "function(settings) {",
                  "$('tbody tr:last-child td:first-child').css({'border-bottom-left-radius': '5px'});",
                  "$('tbody tr:last-child td:last-child').css({'border-bottom-right-radius': '5px'});",
                  "}"
                )
              )
            )
          } else {
            output$multi_typing_result_table <- renderDataTable(
              Typing$result_list[[input$multi_results_picker]],
              selection = "single",
              options = list(
                pageLength = 10,
                scrollY = TRUE,
                columnDefs = list(list(searchable = TRUE, targets = "_all")),
                initComplete = DT::JS(
                  "function(settings, json) {",
                  "$('th:first-child').css({'border-top-left-radius': '5px'});",
                  "$('th:last-child').css({'border-top-right-radius': '5px'});",
                  "$('tbody tr:last-child td:first-child').css({'border-bottom-left-radius': '5px'});",
                  "$('tbody tr:last-child td:last-child').css({'border-bottom-right-radius': '5px'});",
                  "}"
                ),
                drawCallback = DT::JS(
                  "function(settings) {",
                  "$('tbody tr:last-child td:first-child').css({'border-bottom-left-radius': '5px'});",
                  "$('tbody tr:last-child td:last-child').css({'border-bottom-right-radius': '5px'});",
                  "}"
                )
              )
            )
          }
        }
      } else {
        output$multi_typing_result_table <- NULL
      }
    } else {
      output$multi_typing_result_table <- NULL
    }
  })

  observe({
    if (!is.null(Typing$multi_result_status)) {
      if (
        Typing$multi_result_status == "start" ||
          Typing$multi_result_status == "finalized"
      ) {
        if (Typing$multi_help == TRUE) {
          Typing$result_list <- readRDS(file.path(
            app_local_share_path,
            "event_list.rds"
          ))
          Typing$multi_help <- FALSE
        }
      }
    }
  })

  #Render multi typing result feedback table
  observe({
    if (!is.null(Typing$result_list)) {
      if (length(Typing$result_list) > 0) {
        output$multi_typing_results <- renderUI({
          column(
            width = 11,
            fluidRow(
              column(
                width = 9,
                br(),
                br(),
                br(),
                br(),
                br(),
                div(
                  class = "mult_res_sel",
                  selectInput(
                    "multi_results_picker",
                    label = h5("Select Typing Results", style = "color:white"),
                    choices = names(Typing$result_list),
                    selected = names(Typing$result_list)[length(
                      names(Typing$result_list)
                    )],
                    width = "100%"
                  )
                ),
                br()
              )
            ),
            div(
              class = "typing-result-table",
              dataTableOutput("multi_typing_result_table")
            )
          )
        })
      }
    }
  })

  observe({
    # Render log content
    output$logText <- renderPrint({
      cat(rev(paste0(tail(readLogFile(), 50), "\n")))
    })

    output$logTextFull <- renderPrint({
      cat(rev(paste0(readLines(file.path(logdir, "script_log.txt")), "\n")))
    })

    # Render Pending UI
    if (
      !grepl("Multi Typing", tail(readLogFile(), n = 1)) &&
        grepl("Start Multi Typing", head(readLogFile(), n = 1))
    ) {
      Typing$multi_result_status <- "start"

      output$initiate_multi_typing_ui <- NULL

      output$pending_typing <- renderUI({
        fluidRow(
          fluidRow(
            br(),
            br(),
            column(width = 2),
            column(
              width = 4,
              h3(p("Pending Typing ..."), style = "color:white"),
              br(),
              br(),
              fluidRow(
                column(
                  width = 5,
                  HTML(paste(
                    '<i class="fa fa-spinner fa-spin" style="font-size:24px;color:white;margin-top:5px"></i>'
                  ))
                ),
                column(
                  width = 6,
                  align = "left",
                  actionButton(
                    "reset_multi",
                    "Terminate",
                    icon = icon("ban")
                  )
                )
              ),
            )
          ),
          br(),
          br(),
          fluidRow(
            column(width = 2),
            column(
              width = 10,
              verbatimTextOutput("logText")
            )
          )
        )
      })
    } else if (grepl("Multi Typing finalized", tail(readLogFile(), n = 1))) {
      Typing$multi_result_status <- "finalized"

      Typing$last_scheme <- NULL

      output$initiate_multi_typing_ui <- NULL

      output$pending_typing <- renderUI({
        fluidRow(
          fluidRow(
            br(),
            br(),
            column(width = 2),
            column(
              width = 4,
              h3(p("Pending Multi Typing ..."), style = "color:white"),
              br(),
              br(),
              HTML(
                paste(
                  "<span style='color: white;'>",
                  paste(
                    "Typing of",
                    sum(str_detect(
                      readLines(file.path(
                        logdir,
                        "script_log.txt"
                      )),
                      "Processing"
                    )),
                    "assemblies finalized."
                  ),
                  paste(
                    sum(str_detect(
                      readLines(file.path(
                        logdir,
                        "script_log.txt"
                      )),
                      "Successful"
                    )),
                    "successes."
                  ),
                  paste(
                    sum(str_detect(
                      readLines(file.path(
                        logdir,
                        "script_log.txt"
                      )),
                      "failed"
                    )),
                    "failures."
                  ),
                  "Reset to start another typing process.",
                  sep = '<br/>'
                )
              ),
              br(),
              br(),
              fluidRow(
                column(
                  width = 5,
                  actionButton(
                    "reset_multi",
                    "Reset",
                    icon = icon("arrows-rotate")
                  )
                ),
                column(
                  width = 5,
                  downloadButton(
                    "print_log",
                    "Logfile",
                    icon = icon("floppy-disk")
                  )
                )
              )
            )
          ),
          br(),
          br(),
          fluidRow(
            column(width = 2),
            column(
              width = 10,
              verbatimTextOutput("logTextFull")
            )
          )
        )
      })
    } else if (!grepl("Start Multi Typing", head(readLogFile(), n = 1))) {
      output$pending_typing <- NULL
      Typing$multi_result_status <- "idle"
    }
  })
} # end server

# _______________________ ####

# Shiny ----

shinyApp(ui = ui, server = server)
