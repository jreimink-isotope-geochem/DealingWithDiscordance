library(shiny)
library(IsoplotR)
library('cardidates')
library(dplyr)
library(reactlog)

reactlog_enable()


ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      selectInput(
        "data_source",
        label = "Data Source Test",
        choices = c("Copy-Paste", "Upload File"),
        selected = "Upload File"
      ),
      
      conditionalPanel(
        condition = "input.data_source == 'Copy-Paste'",
        textAreaInput(
          "data_input",
          label = "Paste your data here:",
          value = "",
          width = "100%",
          height = "200px"
        )
      ),
      
      conditionalPanel(
        condition = "input.data_source == 'Upload File'",
        fileInput(
          "data_file",
          label = "Upload data file (CSV)",
          accept = ".csv"
        )
      ),
      
      actionButton(
        inputId = "reduce_data",
        label = "Discordance Reduction"
      ),
      
      sliderInput(
        inputId = "node_space",
        label = "Node Spacing (ma)",
        min = 1,
        max = 50,
        step = 7,
        value = 15
      ),
      
      radioButtons(
        inputId ="normalize_unc",
        label = 'Uncertainty normalization',
        choiceNames = c("Normalized", "Unnormalized"),
        choiceValues = c('Y', 'N'),
        selected = "N",
        inline = TRUE
      ),
      
      radioButtons(
        inputId ="data_type",
        label = 'Type of population',
        choiceNames = c("Detrital (weight against concordance)", "Single (no weighting)"),
        choiceValues = c('detrital', 'single'),
        selected = "single",
        inline = TRUE
      ),
      
      sliderInput(
        inputId = "start_cut_upper_min",
        label = "Minimum Upper Intercept Value (Ma)",
        min = 0,
        max = 4500,
        step = 250,
        value = 0
      ),
      
      sliderInput(
        inputId = "start_cut_upper_max",
        label = "Maximum Upper Intercept Value (Ma)",
        min = 0,
        max = 4500,
        step = 250,
        value = 2000
      ),
      
      sliderInput(
        inputId = "start_cut_lower_min",
        label = "Minimum Lower Intercept Value (Ma)",
        min = 0,
        max = 4500,
        step = 250,
        value = 0
      ),
      
      sliderInput(
        inputId = "start_cut_lower_max",
        label = "Maximum Lower Intercept Value (Ma)",
        min = 0,
        max = 4500,
        step = 250,
        value = 2000
      )
    ),
    
    mainPanel(
      textOutput("my_csv_name"),  # Shows file name
      tabsetPanel(
        # Tab 1: Output table
        tabPanel("Input Table", tableOutput("output_table")),
        
        # Tab 2: Output plot - Concordia Diagram
        tabPanel("Concordia Plot", plotOutput("output_plot")),
        
        #Tab 3: Output plot - Discordance Plot (lower and Upper int)
        tabPanel("Discordance Plot (Lower and Upper Int.)", plotOutput("Discordance_plot_upper_lower")),
        
        # Tab 4: Output plot - Discordance Plot (Lower Summed)
        tabPanel("Discordance Plot (Lower Int. Summed)", plotOutput("Discordance_plot_lower_summed")),
        
        # Tab 5: Output plot - Heatmap 2D Histogram
        tabPanel("Heatmap 2D Histogram", plotOutput("Discordance_plot_heat_map"))
    )
  )
))




server <- function(input, output, session) {
  # Function to load data based on input type
  loadData <- function() {
    if (input$data_source == "Copy-Paste") {
      # Read the input data from copy-paste
      data <- read.table(text = input$data_input, sep = "\t", header = TRUE, check.names = FALSE)
    } else {
      # Read the input data from file upload
      req(input$data_file)
      data <- read.csv(input$data_file$datapath, header = TRUE, check.names = FALSE)
    }
    
    # Subset the data to include only the desired columns
    desired_columns <- c("Samples", "Final Pb207/U235_mean", "Final Pb207/U235_2SE(int)", "Final Pb206/U238_mean",
                         "Final Pb206/U238_2SE(int)", "rho 206Pb/238U v 207Pb/235U", "Final Pb207/Pb206 age_mean")
    data <- data[, desired_columns, drop = FALSE]
    
    # Return the data frame
    return(data)
  }
  
  # Create reactiveValues to store the data
  reducedData <- reactiveValues(data = NULL)
  
  # Define a reactive expression to update the data when necessary
  #will do reduction as soon as data is in the system
  observeEvent(input$data_file, {
    # Reduce the data
    req(input$data_file)
    Data.raw <- loadData()
    
    # Define variables
    file_name <- input$data_file$name
    sample.name <- sub(".csv", "", file_name)
    node.spacing <- as.numeric(input$node_space) # node spacing from drop down
    
    source('App_reduction.R', local = TRUE)
    
    # Update the reducedData reactiveValues
    reducedData$data <- list(xy_plot,
                             heat_map_plot,
                             lower_int_summed)
  })
  
  #will update data after sliders change and then button is pressed
  observeEvent(input$reduce_data, {
    # Reduce the data
    req(input$data_file)
    Data.raw <- loadData()
    
    # Define variables
    file_name <- input$data_file$name
    sample.name <- sub(".csv", "", file_name)
    node.spacing <- as.numeric(input$node_space) # node spacing from drop down
    
    source('App_reduction.R', local = TRUE)
    
    # Update the reducedData reactiveValues
    reducedData$data <- list(xy_plot,
                             heat_map_plot,
                             lower_int_summed)
  })
  
  
  
  
  # Generate file name
  output$my_csv_name <- renderText({ # Output file name
    # Test if file is selected
    if (!is.null(input$data_file$datapath)) {
      # Extract file name (additionally remove file extension using sub)
      return(sub(".csv", "", input$data_file$name))
    } else {
      return(NULL)
    }
  })
  
  # Process the user input and generate output table
  output$output_table <- renderTable({
    data <- loadData()
    data
  })
  
  # Process the user input and generate output plot
  output$output_plot <- renderPlot({
    data <- loadData()
    
    # Generate the plot or perform other functions
    
    # Example: Concordia Diagram
    data_concordia <- read.data(data[, 2:6], ierr = 2, method = 'U-Pb', format = 1)
    concordia(data_concordia, type = 1)
  })
  
  # Upper and lower intercept xy plot
  output$Discordance_plot_upper_lower <- renderPlot({
    req(reducedData$data) # Wait for the data to be available
    

   reducedData$data[[1]]

  })
  
  # Summed lower intercept probability plot
  output$Discordance_plot_lower_summed <- renderPlot({
    req(reducedData$data) # Wait for the data to be available
    

    reducedData$data[[3]]
  })
  
  # Heatmap 2d histogram plot
  output$Discordance_plot_heat_map <- renderPlot({
   req(reducedData$data) # Wait for the data to be available
    
    reducedData$data[[2]]

  })
}

# Run the Shiny app
shinyApp(ui = ui, server = server)






      