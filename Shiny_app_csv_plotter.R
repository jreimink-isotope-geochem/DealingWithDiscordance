install.packages('shiny')
library(viridis)
library(shiny)
library(ggplot2)

# Define UI for app
ui <- fluidPage(
  
  # Title
  titlePanel("CSV Plotter"),
  
  # Sidebar with input file and column selections
  sidebarLayout(
    sidebarPanel(
      # Input file selection
      fileInput("file", "Select CSV file"),
      
      # Column selections
      selectInput(inputId = "xcol", 
                  label = "X-axis Column", 
                  choices = c("Option 1", "Option 2", "Option 3"), 
                  selected = "Option 1", 
                  multiple = FALSE,
                  selectize = TRUE),
      selectInput(inputId = "ycol", 
                  label = "Y-axis Column", 
                  choices = c("Option 1", "Option 2", "Option 3"), 
                  selected = "Option 1", 
                  multiple = FALSE,
                  selectize = TRUE),
      selectInput(inputId = "colorcol", 
                  label = "Color Column (Optional)",
                  choices =  c("", "None"), 
                  selected = "",
                  multiple = F,
                  selectize = TRUE),
      
      # Formatting options
      radioButtons(inputId = "plottype", 
                   label = "Plot Type", 
                   choices = c("Scatterplot" = "points", "Line Plot" = "line"), 
                   selected = "points" 
      ),
      radioButtons(inputId = "colorscale", 
                   label = "Color Scale",
                   choices= c("Rainbow" = "turbo","Hot" = 'magma', "Normal" = "viridis"),
                   selected = "turbo" 
      )
    ),
    
    # Output plot
    mainPanel(
      plotOutput(outputId = "plot")
    )
  )
)


# Define server for app
server <- function(input, output, session) {
  
  # Read CSV file
  data <- reactive({
    req(input$file)
    read.csv(input$file$datapath)
  })
  
  # Update column selections based on input file
  observe({
    req(data())
    updateSelectInput(session, "xcol", choices = names(data()))
    updateSelectInput(session, "ycol", choices = names(data()))
    updateSelectInput(session, "colorcol", choices = c("", names(data())))
  })
  
  # Create plot
  output$plot <- renderPlot({
    req(data(), input$xcol, input$ycol)
    
    # Create plot
    plot <- ggplot(data(), aes_string(x = input$xcol, y = input$ycol, 
                                      color = input$colorcol)) +
      scale_color_viridis(option = input$colorscale) +
      theme_bw()
    
    # Add formatting based on plot type
    if (input$plottype == "line") {
      plot <- plot + geom_line()
    } else {
      plot <- plot + geom_point()
    }
    
    plot
  })
}

# Run app
shinyApp(ui, server)
