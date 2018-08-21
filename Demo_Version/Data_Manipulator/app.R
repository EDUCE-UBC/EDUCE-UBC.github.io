#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
# http://shiny.rstudio.com/

# Libraries Used ----
#library(DESeq2)
#library(data.table)
library(dplyr)
library(grid)
library(ggplot2)
library(gridExtra)
#library(mice)
library(rdrop2)
library(shiny)
library(shinyjs)
library(shinyalert)
library(shinythemes)
library(tidyverse)
source("App_Functions.R")

# Number of sections to DELETE for downloadable version = 2

# UI -----------------------------------------------------------------------------------------------

# Define UI for data upload app ----
ui <- navbarPage("Data Manipulator", theme = shinytheme("cyborg"), 
                 tabPanel("Data Plotter", useShinyjs(), useShinyalert(),
                          # Sidebar layout with input and output definitions ----
                          sidebarLayout(
                            # Sidebar panel for inputs ----
                            sidebarPanel(
                              # Return to EDUCE Button ----
                              actionButton("return", label = "Return to EDUCE", icon = icon("home"), 
                                           onclick ="location.href='https://educe-ubc.github.io/';"),
                              
                              # Horizontal Line ----
                              tags$hr(),
                              
                              # Input: Select a file ----
                              fileInput("plotData", "Choose Metadata File",
                                        multiple = TRUE,
                                        accept = c("text/csv",
                                                   "text/comma-separated-values,text/plain",
                                                   ".csv")),
                              
                              # Horizontal Line ----
                              tags$hr(),
                              tags$h5("Data Table Options"),
                              
                              # Input: Select columns ----
                              selectizeInput("col", "Select Data Columns (Optional)", multiple = TRUE, choices = NULL),
                              
                              # Horizontal Line ----
                              tags$hr(),
                              tags$h5("Data Plot Options"),
                              
                              fluidRow(
                                # Input: X Axis ----
                                column(6, selectizeInput("x", "Input X Axis Name (Required)", choices = NULL)),
                                # Input: Y Axis ----
                                column(6, selectizeInput("y", "Input Y Axis Name (Required)", choices = NULL))
                              ),
                              
                              selectizeInput("types", "Select Graph Types (Required)", multiple = TRUE, 
                                             choices = c("Line", "Point", "Smooth", "Barplot", "Area"))
                            ),
                            mainPanel(
                              tabsetPanel(
                                tabPanel("Data Table", tableOutput("table"), icon = icon("table")),
                                tabPanel("Data Plot", plotOutput("plot", height = "auto"), icon = icon("line-chart")),
                                tabPanel("Data Summary", verbatimTextOutput("summary"), icon = icon("info"))
                              )
                            )
                          )
                 ),
                 tabPanel("Data Normalization", useShinyjs(), useShinyalert(),
                          # Sidebar layout with input and output definitions ----
                          sidebarLayout(
                            # Sidebar panel for inputs ----
                            sidebarPanel(
                              # Return to EDUCE Button ----
                              actionButton("return", label = "Return to EDUCE", icon = icon("home"), 
                                           onclick ="location.href='https://educe-ubc.github.io/';"),
                              
                              # DELETE Section not needed for Downloadable App Version ----------------------------
                              
                              # Saves user file to Dropbox for testing ----
                              actionButton("validate", label = "Validation Data", icon = icon("archive")),
                              
                              # Comment this out to prevent users from bogging down server ----
                              actionButton("testrun", label = "Test App Version", icon = icon("archive")),
                              
                              # End of DELETE Section not needed for Downloadable App Version ---------------------
                              
                              # Horizontal Line ----
                              tags$hr(),
                              
                              # Input: Select a file ----
                              fileInput("normData", "Choose OTU/ASV File (Recommended File Size: Below 500 KB)",
                                        multiple = TRUE,
                                        accept = c("text/csv",
                                                   "text/comma-separated-values,text/plain",
                                                   ".csv")),
                              
                              # Input: Select file type ----
                              selectInput("type", "Select File Type",
                                          choices = c("OTU TXT" = "OTUTxt",
                                                      "ASV TXT" = "ASVTxt"),
                                          selected = "OTUTxt"),
                              
                              fluidRow(
                                # Input: Checkbox for file header ----
                                column(2, checkboxInput("header", "Header", TRUE)),
                                # Input: Select separator ----
                                column(3, radioButtons("sep", "Separator",
                                                       choices = c(Tab = "\t",
                                                                   Comma = ","),
                                                       selected = "\t")),
                                # Input: Select quotes ----
                                column(3, radioButtons("quote", "Quote",
                                                       choices = c(None = "",
                                                                   "Double Quote" = '"',
                                                                   "Single Quote" = "'"),
                                                       selected = '"'))
                              ),
                              
                              # Axes Legend ----
                              "Barplot Information: X-Axis = OTU/ASV, Y-Axis = Abundance",
                              
                              # Input: Enter number of OTU/ASV to display
                              numericInput("bins", "Number of OTU/ASV to Display (Max 5000)", 
                                           20, min = 1, max = 5000),
                              
                              # Input: Enter number of columns in data
                              numericInput("dataNum", "Number of Columns of Data (Max 100)", 
                                           1, min = 1, max = 100),
                              
                              # Use random generator for Random Subsampling ----
                              numericInput("rng", "Random Generator Seed (Set to 0 for no fixed seed)", 123),
                              
                              # Optional email input for Validation Data ----
                              textInput("email", "Email (Required for submitting Validation Data only)", value = ""),
                              
                              # Download normalized table ----
                              selectizeInput("downloadType", "Type of Normalization", choices = c("Percent Relative Abundance" = "PRA", "Random Subsampling" = "RS")),
                              downloadButton('downloadReport')
                            ),
                            
                            # Main panel for displaying outputs ----
                            mainPanel(
                              # Output: Data file ----
                              tabsetPanel(
                                # TabPanel for data tables ----
                                tabPanel("PRA Table", tableOutput("dt"), icon = icon("table")),
                                tabPanel("RS Table", tableOutput("rst"), icon = icon("table")),
                                # TabPanels for normalized data graphs ----
                                tabPanel("Percentage Relative Abundance", plotOutput("pra", height = "auto"), icon = icon("bar-chart")),
                                tabPanel("Random Subsampling", plotOutput("rs", height = "auto"), icon = icon("bar-chart"))
                              )
                            )
                          )
                 )
)


# End of UI ----------------------------------------------------------------------------------------

# Server -------------------------------------------------------------------------------------------

# Define server logic to read selected file ----
server <- function(input, output, session) {
  
  # Reactive Table Variable ----
  inData <- reactive({
    req(input$plotData)
    
    # Read input files ----
    df <- file_reader(input$plotData$datapath, TRUE, "\t", "")
  })
  
  observe({
    data_choices <- names(inData())
    updateSelectizeInput(session, "col", choices = data_choices, server = TRUE)
    updateSelectizeInput(session, "x", choices = data_choices, server = TRUE)
    updateSelectizeInput(session, "y", choices = data_choices, server = TRUE)
  })
  
  # Generates table ----
  output$table <- renderTable({
    if (!is.null(input$col)) {
      Result <- inData() %>% select(input$col)
      return(as.matrix(Result))
    }
    else {
      return(as.matrix(inData()))
    }
  })
  
  # Generates plot ----
  output$plot <- renderPlot({
    req(input$plotData, input$x, input$y, input$types)
    plotData <- inData()
    plot <- ggplot(plotData, aes_string(input$x, input$y))
    for (i in input$types) {
      plot <- plot + switch(i,
                            "Line" = geom_line(),
                            "Point" = geom_point(),
                            "Smooth" = stat_smooth(),
                            "Barplot" = geom_bar(stat = "identity", fill =  "#009E73"),
                            "Area" = geom_area(stat = "identity", fill = "#009E73"))
    }
    plot
  }, height = 500)
  
  # Generates summary ----
  output$summary <- renderPrint({
    req(input$plotData)
    summary(inData())
  })
  
  # DELETE Section not needed for Downloadable App Version --------------------------------------------------------
  
  # When the Validation button is clicked, save the form data ----
  observeEvent(input$validate, {
    # Checks for available space in Dropbox ----
    storageFull <- storageCheck()
    if (storageFull) {
      shinyalert(title = 'Thank you for offering your data for our validation testing! Unfortunately we do not have the capacity to store these files.', animation = FALSE)
    }
    
    if (input$email == "") {
      shinyalert(title = 'When submitting your data, please input your email.', animation = FALSE)
    }
    
    req(input$normData, input$email != "", !storageFull)
    
    # Error catching for valid file contents ----
    tryCatch({
      shinyalert(
        title = paste("By submitting this data for validation testing, you agree that the information contained here is non-confidential and can be shared with the public.", "Please type 'I Agree' to continue.", sep = "\n") , type = "input",
        callbackR = function(value) {
          if (input$rng != 0 && value == "I Agree") {
            # Save validation data ----
            saveData(sData(), "Validation_Norm_Data", input$rng, input$email)
            saveData(dfData(), "Validation_Data", input$rng, input$email)
            
            shinyalert(title = "Thank you. Your data has been saved for validation!", animation = FALSE)
          }
          else if (input$rng == 0) {
            shinyalert(title = "Please choose another RNG for validation data.", animation = FALSE)
          }
          else {
            shinyalert(title = "Data not saved for validation.", animation = FALSE)
          }
        }
      )
    },
    error = function(cond) {
      shinyalert(title = paste("Invalid Validation Data Format or Content Error. Please check the contents of the file or whether the correct settings were selected.", 
                               "-----Original Error Message-----", cond, sep = "\n"), animation = FALSE)
      # Choose a return value in case of error ----
      return(NA)
    },
    warning = function(cond) {
      shinyalert(title = paste("Invalid Validation Data Format or Content Error. Please check the contents of the file or whether the correct settings were selected.", 
                               "-----Original Error Message-----", cond, sep = "\n"), animation = FALSE)
      # Choose a return value in case of warning ----
      return(NULL)
    })
  })
  
  # When test button is clicked, check whether files are the same for current app version ----
  observeEvent(input$testrun, {
    tests <- loadData()
    if (isTRUE(tests)) {
      shinyalert(title = "Current App Version Passes Validation!", animation = FALSE)
    }
    else {
      files <- paste(tests, collapse = ", ")
      shinyalert(title = paste("Current App Version Failed Validation. Please Check Test Files or App Code.", "Tests failed:", files, sep = "\n"), animation = FALSE)
    }
  })
  
  # End of DELETE Section not needed for Downloadable App Version -------------------------------------------------
  
  # Reactive Table Variable ----
  dfData <- reactive({
    req(input$normData)
    
    # Read input files ----
    df <- file_reader(input$normData$datapath, input$header, input$sep, input$quote)
    df <- file_converter(df, input$type)
  })
  
  # Data for Random Subsampling ----
  sData <- reactive({
    data <- as.matrix(dfData())
    data <- sampler(data, input$rng)
  })
  
  # Section where server renders table based on data and user inputs ----
  output$dt <- renderTable({
    Sample <- rownames(dfData())
    return(cbind(Sample, as.matrix(dfData())))
  })
  
  output$rst <- renderTable({
    Sample <- rownames(sData())
    return(cbind(Sample, sData()))
  })
  
  # Section where server renders plots for Percentage Relative Abundance ----
  output$pra <- renderPlot({
    # Convert data to matrix and normalize each column ----
    data <- as.matrix(dfData())
    class(data) <- "numeric"
    norm_data <- t(t(data)/colSums(data))
    
    # Organize placements of normalized graphs and dynamically generate barplots based on number of columns ----
    par(mfrow = c(input$dataNum, 1), mar = c(7, 4, 4, 2) + 0.1)
    barplot_gen(norm_data, input$dataNum, input$bins)
  }, 
  # Dynamically generates space for graphs based on number of columns ----
  height = function(){500*input$dataNum})
  
  # Section where server renders plots for Random Subsampling ----
  output$rs <- renderPlot({
    # Randomly subsample each sample that is not 0, and normalize each column ----
    norm_data <- t(t(sData())/colSums(sData()))
    
    # Organize placements of normalized graphs and dynamically generate barplots based on number of columns ----
    par(mfrow = c(input$dataNum, 1), mar = c(7, 4, 4, 2) + 0.1)
    barplot_gen(norm_data, input$dataNum, input$bins)
  }, 
  # Dynamically generates space for graphs based on number of columns ----
  height = function(){500*input$dataNum})
  
  # Download normalized table ----
  output$downloadReport <- downloadHandler(
    # File output name ----
    filename = function() {
      paste('Normalized_Data', sep = '.', 'txt')
    },
    
    # Function for outputting file ----
    content = function(file) {
      if (input$downloadType == 'PRA') {
        data <- as.matrix(dfData())
        class(data) <- "numeric"
        data <- t(t(data)/colSums(data))
      }
      else if (input$downloadType == 'RS') {
        data <- t(t(sData())/colSums(sData()))
      }
      write.table(data, file)
    }  
  )
}

# End of Server ------------------------------------------------------------------------------------

# Launches UI and server ----
shinyApp(ui, server)