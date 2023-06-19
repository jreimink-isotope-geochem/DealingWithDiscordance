library(shiny)
library(IsoplotR)
library('cardidates')
library(dplyr)
library(reactlog)

reactlog_enable()


ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      selectInput( #builds the input tab to design the input options
        "data_source",
        label = "Data Source Test",
        choices = c("Copy-Paste", "Upload File"),
        selected = "Upload File"
      ),
      
      conditionalPanel(# for the copy paste input, needs to be changed to react table
        condition = "input.data_source == 'Copy-Paste'",
        textAreaInput(
          "data_input",
          label = "Paste your data here:",
          value = "",
          width = "100%",
          height = "200px"
        )
      ),
      
      conditionalPanel(#just a file upload
        condition = "input.data_source == 'Upload File'",
        fileInput(
          "data_file",
          label = "Upload data file (CSV)",
          accept = ".csv"
        )
      ),
      
      actionButton(#creates the button for the obserEvent call to reduce
        inputId = "reduce_data",
        label = "Discordance Reduction"
      ),
      
      selectInput(# drop down for the node spacing
        inputId = "node_space",
        label = "Node Spacing (ma)",
        choices = c(0.5,1,5,10,15,20,25,50,100),
        selected = 25
      ),
      
      radioButtons( #input for normalizing the uncertainty
        inputId ="normalize_unc",
        label = 'Uncertainty normalization',
        choiceNames = c("Normalized", "Unnormalized"),
        choiceValues = c('Y', 'N'),
        selected = "N",
        inline = TRUE
      ),
      
      radioButtons( # input to weight against concordant analysis
        inputId ="data_type",
        label = 'Type of population',
        choiceNames = c("Detrital (weight against concordance)", "Single (no weighting)"),
        choiceValues = c('detrital', 'single'),
        selected = "single",
        inline = TRUE
      ),
      
      #NEXT inputs pertain to the entry windows for the axes      
      numericInput( 
        inputId = "start_cut_upper_min",
        label = "Minimum Upper Intercept Value (Ma)",
        value = 0,
        min = 0,
        max = 4500
      ),
      
      numericInput(
        inputId = "start_cut_upper_max",
        label = "Maximum Upper Intercept Value (Ma)",
        min = 0,
        max = 4500,
        value = 2000
      ),
      
      numericInput(
        inputId = "start_cut_lower_min",
        label = "Minimum Lower Intercept Value (Ma)",
        min = 0,
        max = 4500,
        value = 0
      ),
      
      numericInput(
        inputId = "start_cut_lower_max",
        label = "Maximum Lower Intercept Value (Ma)",
        min = 0,
        max = 4500,
        value = 2000
      )
    ),
    
    mainPanel(# sets up the tabs to show outputs
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
    req(input$data_file) #requires the data to exist to run
    Data.raw <- loadData() # raw data use the input data from the function above
    
    # Define variables
    file_name <- input$data_file$name # used in labeling and titles
    sample.name <- sub(".csv", "", file_name) # used in reduction naming
    node.spacing <- as.numeric(input$node_space) # node spacing from drop down
    
    source('App_reduction.R', local = TRUE) # sources the reduction code and ties all the inputs to the variables
    
    # Update the reducedData reactiveValues
    #the reactive variables only change when something else changes. 
    # here the variables are the plots from the reworked discordant reduction plotting functions
    # when the values are stored in this list, they can be called elsewhere
    # they are called as list elements in the output plots
    reducedData$data <- list(xy_plot,
                             heat_map_plot,
                             lower_int_summed)
  })
  
  #will update data after inputs change and then button is pressed
  # does the same as above and updates the reactiveVar with new plots
  observeEvent(input$reduce_data, { # calls the input to see if the button is pressed
    # requires the data to be input for it to try and run
    req(input$data_file)
    
    #cleans the data using the function
    Data.raw <- loadData()
    
    # Define variables
    file_name <- input$data_file$name
    sample.name <- sub(".csv", "", file_name)
    node.spacing <- as.numeric(input$node_space) # node spacing from drop down
    
    # runs the cleaned discordance code and sets all the inputs
    # this automates all the things we used to do
    source('App_reduction.R', local = TRUE)
    
    # Update the reducedData reactiveValues
    reducedData$data <- list(xy_plot, # upper and lower intercept plot
                             heat_map_plot, # heat map plot
                             lower_int_summed) # summed lower intercept plot
  })
  
  
  
  
  # Generate file name
  # over worked but it gets the file name nonetheless
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
  
  # Process the user input and generate output concordia
  output$output_plot <- renderPlot({
    data <- loadData()
    
    
    #Concordia Diagram using isoplotR
    #UPDATE OPTION: add a change to use type 1 or 2
    data_concordia <- read.data(data[, 2:6], ierr = 2, method = 'U-Pb', format = 1)
    concordia(data_concordia, type = 1)
  })
  
  # Upper and lower intercept xy plot
  output$Discordance_plot_upper_lower <- renderPlot({
    req(reducedData$data) # Wait for the data to be available
    
    
    reducedData$data[[1]] #calls the plot out of the react variable
    # the syntax is weird here. double brackets first set calls the list, second calls the item
    
  })
  
  # Summed lower intercept probability plot
  output$Discordance_plot_lower_summed <- renderPlot({
    req(reducedData$data) # Wait for the data to be available
    
    
    reducedData$data[[3]] #calls the plot out of the react variable
  })
  
  # Heatmap 2d histogram plot
  output$Discordance_plot_heat_map <- renderPlot({
    req(reducedData$data) # Wait for the data to be available
    
    reducedData$data[[2]] #calls the plot out of the react variable
    
  })
}

# Run the Shiny app
shinyApp(ui = ui, server = server)






