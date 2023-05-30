library(shiny)
library(IsoplotR)


ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      # Option to choose between copy-pasted data and file upload
      selectInput(
        "data_source",
        label = "Data Source",
        choices = c("Copy-Paste", "Upload File"),
        selected = "Copy-Paste"
      ),
      
      # Text input as a table
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
      
      # File upload input
      conditionalPanel(
        condition = "input.data_source == 'Upload File'",
        fileInput(
          "data_file",
          label = "Upload data file (CSV)",
          accept = ".csv"
        )
      )
    ),
    mainPanel(
      # Output plot or other functions
      plotOutput("output_plot"),
      
      # Output table
      tableOutput("output_table")
    )
  )
)


server <- function(input, output, session) {
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
    data_concordia <- read.data(  data[ , 2:6], ierr = 2,
                method = 'U-Pb', format = 1 )
      concordia(data_concordia, type = 1)

  })
  
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
    data
  }
}

# Run the Shiny app
shinyApp(ui = ui, server = server)





      