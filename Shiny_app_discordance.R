library(shiny)
library(IsoplotR)
library('cardidates')
library(dplyr)
library(reactlog)
library(rhandsontable)
library(DT)

reactlog_enable()


ui <- fluidPage(
  h1( id="big-heading", "Discordance Modeling of U-Pb Data"),
  tags$style( HTML( "#big-heading{ color: #213e47;
                    text-align:center}")),
  sidebarLayout(
    sidebarPanel(
      h3( id="settings-heading", "Modeling Settings"),
      br(),
      tags$style( HTML( "#settings-heading{ color: #213e47;
                    text-align:center}")),
      textInput( "sampleName", ("Sample Name:"), value = "sample name here" ),
      selectInput(# drop down for the node spacing
        inputId = "node_space",
        label = ("Node Spacing (Ma)"),
        choices = c(0.5,1,5,10,15,20,25,50,100),
        selected = 25
      ),
      
      radioButtons( #input for normalizing the uncertainty
        inputId ="normalize_unc",
        label = ('Uncertainty normalization'),
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
        label = ("Minimum Upper Intercept Value (Ma)"),
        value = 0,
        min = 0,
        max = 4500
      ),
      
      numericInput(
        inputId = "start_cut_upper_max",
        label = ("Maximum Upper Intercept Value (Ma)"),
        min = 0,
        max = 4500,
        value = 2000
      ),
      
      numericInput(
        inputId = "start_cut_lower_min",
        label = ("Minimum Lower Intercept Value (Ma)"),
        min = 0,
        max = 4500,
        value = 0
      ),
      
      numericInput(
        inputId = "start_cut_lower_max",
        label = ("Maximum Lower Intercept Value (Ma)"),
        min = 0,
        max = 4500,
        value = 2000
      ),
      actionButton( "saveBtn", "Save Table"),
      actionButton(#creates the button for the obserEvent call to reduce
        inputId = "reduce_data",
        label = "Run Discordance Modeling"
      )
    ),
    
    mainPanel(# sets up the tabs to show outputs
      textOutput("my_csv_name"),  # Shows file name
      tabsetPanel(
        tabPanel( "Input Data", 
                  
        # # Tab 1: Output table
        # conditionalPanel( # for the copy paste input, needs to be changed to react table
        #   condition = "input.data_source == 'Manual'",
        # 
        #   downloadButton("dwnldTableBtn", "Download Example Table"),
        #   br(),
        #   tabPanel("Input Data", rHandsontableOutput("output_table")),
        # 
        # ),
        # conditionalPanel( # for the copy paste input, needs to be changed to react table
        #   condition = "input.data_source == 'Upload'",
        #   fileInput(
        #     "data_file",
        #     label = "Upload data file (CSV)",
        #     accept = ".csv"
        #   )
        # )


        radioButtons(
          inputId ="data_source",
          label = 'Data Input Type',
          choiceNames = c("Manual", "Upload File"),
          choiceValues = c('Manual', 'Upload'),
          selected = "Manual",
          inline = TRUE ),
        #deactivate input sample size field when range of samples is selected
        conditionalPanel(
          condition = "input.data_source == 'Upload'",
          fileInput("fileInput", "Upload CSV File"),
          dataTableOutput( "data_file")
        ),
        rHandsontableOutput("output_table") ),

        
        # Tab 2: Output plot - Concordia Diagram
        tabPanel("Concordia Plot", plotOutput("concordia_plot", 
                                              height = "700px", width = "700px") ),
        
        #Tab 3: Output plot - Discordance Plot (lower and Upper int)
        tabPanel("Discordance Plot (Lower and Upper Int.)", plotOutput("Discordance_plot_upper_lower")),
        
        # Tab 4: Output plot - Discordance Plot (Lower Summed)
        tabPanel("Discordance Plot (Lower Int. Summed)", plotOutput("Discordance_plot_lower_summed")),
        
        # Tab 5: Output plot - Heatmap 2D Histogram
        tabPanel("Heatmap 2D Histogram", plotOutput("Discordance_plot_heat_map", height = "700px", width = "700px" )),
        
        # tab 6: output data
        tabPanel("Results Table", dataTableOutput("results_table") )
      )
    )
  ),
  # CSS styling
  tags$head(
    tags$style(HTML("
      #reduce_data {
        background-color: #213e47;
        color: white;
      }
      
      #reduce_data:hover {
        background-color: #3A8997;
      }
      
            #saveBtn {
        background-color: #f3d098;
        color: #000000;
      }
      
      #saveBtn:hover {
        background-color: #645153;
        color: #ffffff
      }
      
      #saveSzBtn {
        background-color: #f3d098;
        color: #000000;
      }
      
      #saveSzBtn:hover {
        background-color: #645153;
        color: #ffffff
      }
      
      
    ") ) )
  )




server <- function(input, output, session) {
  
  # Process the user input and generate output table
  # read in base table
  upb_data_input <- reactiveVal( read.csv( "Example_data.csv", stringsAsFactors = FALSE ) )
  
  uploadedData <- reactive({
    req(input$fileInput)
    inFile <- input$fileInput
    if (!is.null(inFile)) {
      # Read the uploaded file
      input.data <- read.csv(inFile$datapath, stringsAsFactors = FALSE)
      return(input.data)
    }
  })
  
  ## display input data
  observe( 
    if( input$data_source == "Upload" ) {
      output$output_table <- renderRHandsontable({
        rhandsontable( uploadedData(), rowHeaders = NULL, width = "100%" ) 
      })
    }
    else {
      output$output_table <- renderRHandsontable({
        rhandsontable( upb_data_input(), rowHeaders = NULL ) 
      })
    }
  )
  
  ## download input data tables when button pushed
  output$dwnldTableBtn <- downloadHandler(
    newData <- hot_to_r(input$output_table),
    filename = function() {
      paste("Input_Data_", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write.csv( hot_to_r(input$output_table), file, row.names = FALSE )
    }
  )
  
  # Process the user input and generate output concordia
  output$concordia_plot <- renderPlot({
    data <- hot_to_r(input$output_table)
    #Concordia Diagram using isoplotR
    #UPDATE OPTION: add a change to use type 1 or 2
    data_concordia <- read.data(data[, 2:6], ierr = 2, method = 'U-Pb', format = 1)
    concordia( data_concordia, type = 1, ellipse.fill = "#3A899780", concordia.col = "#8f7767"  )
  })
  
  # save input parameters and the rhandsontable
  # when the Save Inputs button is pushed
  observeEvent(input$saveBtn, {
    newData <- hot_to_r( input$output_table )
    input.settings.data <- data.frame( node_space = input$node_space,
                                       normalize_unc = input$normalize_unc,
                                       data_type = input$data_type, 
                                       start_cut_upper_min = input$start_cut_upper_min,
                                       start_cut_upper_max = input$start_cut_upper_max,
                                       start_cut_lower_min = input$start_cut_lower_min,
                                       start_cut_lower_max = input$start_cut_lower_max,
                                       sample.name = input$sampleName )
    
    # Replace "path/to/file.csv" with the actual path to your CSV file
    write.csv( newData, "Shiny_OutPut.csv", row.names = FALSE )
    write.csv( newData, paste( input$sampleName,"_upb_inputdata.csv", sep=""), row.names = FALSE)
    write.csv( input.settings.data, "input.settings.data.csv", row.names = FALSE )
    showModal( modalDialog(
      "Changes saved successfully!",
      title = "Success"
    ))
  })
  
  
  #######         Run the modeling          ###################################################
  observeEvent(input$reduce_data, {
    showModal(
      modalDialog(
        "The Modeling Process is Running",
        easyClose = TRUE
      )
    )
    
    # Set data1 as an environment variable for the sourced script
    env <- new.env()

    source( 'App_reduction.R', local = env)
    
    output$Discordance_plot_upper_lower <- renderPlot({
      env$xy_plot 
    })   
    output$Discordance_plot_lower_summed <- renderPlot({
      env$lower_int_summed
    })
    output$Discordance_plot_heat_map <- renderPlot({
      env$heat_map_plot
    })
    
    output$results_table <- renderDataTable({
        datatable( env$DiscGridTableFinal )
    })
    
    observeEvent( input$reduce_data, {
      showModal(
        modalDialog(
            print("Discordance Modeling Completed")
            )
      )
    })
    
  }
  )
  
  

}

# Run the Shiny app
shinyApp(ui = ui, server = server)






