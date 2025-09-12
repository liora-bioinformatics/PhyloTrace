library(shiny)
library(ggplot2)

# Assume tree1 is defined, e.g.:
# tree1 <- ggplot(data.frame(x = 1:10, y = 1:10), aes(x, y)) + geom_point()

# Function to programmatically crop a ggplot to its content
crop_ggplot <- function(plot) {
  # Convert ggplot to grob
  grob <- ggplotGrob(plot)

  # Identify the panel (main plot content) grob
  panel_grob <- grob$grobs[[which(grob$layout$name == "panel")]]

  # Create a new grob with only the panel content
  new_grob <- gTree(children = gList(panel_grob))

  return(new_grob)
}

# Define UI
ui <- fluidPage(
  # Custom CSS for responsive plot sizing
  tags$style(HTML(
    "
    #plot {
      width: 100%;
      max-width: 100%;
      margin: auto;
    }
    .shiny-plot-output img {
      width: unset;
      height: unset;
    }
  "
  )),
  # Input controls for dynamic dimensions
  sidebarLayout(
    sidebarPanel(
      sliderInput(
        "aspect_ratio",
        "Aspect Ratio (Height/Width):",
        min = 0.5,
        max = 2.0,
        value = 1.5,
        step = 0.1
      )
    ),
    mainPanel(
      plotOutput("plot", height = "auto")
    )
  )
)

# Define server
server <- function(input, output, session) {
  output$plot <- renderPlot(
    {
      # Crop the plot to its content
      cropped_plot <- crop_ggplot(test1)
      # Render the cropped grob
      grid.draw(cropped_plot)
    },
    height = function() {
      # Use actual container width from client data
      session$clientData$output_plot_width * input$aspect_ratio
    },
    res = 96
  )
}

# Run the app
shinyApp(ui = ui, server = server)
