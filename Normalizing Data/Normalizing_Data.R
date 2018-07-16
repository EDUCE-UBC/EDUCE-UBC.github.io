#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
# http://shiny.rstudio.com/

# Libraries Used ----
library(DESeq2)
library(grid)
library(ggplot2)
library(mice)
library(shiny)
library(shinyjs)
library(shinyalert)
library(shinythemes)

# UI -----------------------------------------------------------------------------------------------

# Define UI for data upload app ----
ui <- fluidPage("", id = "navibar", theme = shinytheme("darkly"), useShinyjs(), useShinyalert(),
                # Sidebar layout with input and output definitions ----
                sidebarLayout(
                  # Sidebar panel for inputs ----
                  sidebarPanel(
                    # Return to EDUCE Button ----
                    actionButton("return", label = "Return to EDUCE", icon = icon("home"), 
                                 onclick ="location.href='http://google.com';"),
                    actionButton("validate", label = "Validate Data", icon = icon("archive")),
                    actionButton("testrun", label = "Test App Version", icon = icon("archive")),
                    
                    # Horizontal Line ----
                    tags$hr(),
                    
                    # Input: Select a file ----
                    fileInput("fileData", "Choose File",
                              multiple = TRUE,
                              accept = c("text/csv",
                                         "text/comma-separated-values,text/plain",
                                         ".csv")),
                    
                    # Horizontal Line ----
                    tags$hr(),
                    
                    # Input: Checkbox if file has header ----
                    checkboxInput("header", "Header", TRUE),
                    
                    # Input: Select file type ----
                    selectInput("type", "Select File Type",
                                choices = c("Neither (For txt files)" = "txt",
                                            "OTU CSV" = "otu",
                                            "ASV CSV" = "asv"),
                                selected = "txt"),
                    
                    # Input: Select separator ----
                    radioButtons("sep", "Separator",
                                 choices = c(Tab = "\t",
                                             Comma = ","),
                                 selected = "\t"),
                    
                    # Input: Select quotes ----
                    radioButtons("quote", "Quote",
                                 choices = c(None = "",
                                             "Double Quote" = '"',
                                             "Single Quote" = "'"),
                                 selected = '"'),
                    
                    # Horizontal Line ----
                    tags$hr(),
                    
                    # Axes Legend ----
                    "Barplot Information: X-Axis = OTU/ASV, Y-Axis = Abundance",
                    
                    # Input: Enter number of OTU/ASV to display
                    numericInput("bins", "Number of OTU/ASV to Display (Max 5000)", 
                                 20, min = 1, max = 5000),
                    
                    # Input: Enter number of columns in data
                    numericInput("dataNum", "Number of Columns of Data (Max 100)", 
                                 1, min = 1, max = 100),
                    
                    # Use random generator for Random Subsampling? ----
                    numericInput("rng", "Random Generator Seed (Set to 0 for no fixed seed)", 123)
                  ),
                  
                  # Main panel for displaying outputs ----
                  mainPanel(
                    # Output: Data file ----
                    tabsetPanel(
                      # TabPanel for data tables ----
                      tabPanel("PRA Table", tableOutput("dt"), icon = icon("table")),
                      tabPanel("RS Table", tableOutput("rst"), icon = icon("table")),
                      tabPanel("MI Table", tableOutput("mit"), icon = icon("table")),
                      tabPanel("VST Table", tableOutput("vstt"), icon = icon("table")),
                      # TabPanels for normalized data graphs ----
                      tabPanel("Percentage Relative Abundance", plotOutput("pra", height = "auto"), icon = icon("bar-chart")),
                      tabPanel("Random Subsampling", plotOutput("rs", height = "auto"), icon = icon("bar-chart")),
                      tabPanel("Multiple Imputation", plotOutput("mi", height = "auto"), icon = icon("bar-chart")),
                      tabPanel("VST", plotOutput("vst", height = "auto"), icon = icon("bar-chart"))
                    )
                  )
                )
                
)

# End of UI ----------------------------------------------------------------------------------------

# Functions ----------------------------------------------------------------------------------------

# Function for reading OTU/ASV files (CSV/TXT) ----
file_reader <- function(path, header, sep, quote) {
  file <- read.csv(path,
                   header = header,
                   sep = sep,
                   quote = quote)
  return(file)
}

# Function for converting OTU/ASV files to Tables/Graphs ----
file_converter <- function(df, type, render_type) {
  # Transpose data so users can scroll vertically ----
  if (ncol(df) > 10) {
    df <- t(df)
  }
  
  # Switch conversion method based on input file ----
  switch(
    type, 
    "otu" = {
      ID <- as.data.frame(rownames(df))
      ID <- as.data.frame(ID[-1,])
      names(ID) <- "ID"
      colnames(df) <- df[1,]
      df <- df[-1,]
      if (render_type == "graph") {
        class(df) <- "numeric"
      }
    },
    "asv" = {
      ID <- df[,1]
      df$X <- NULL
      rownames(df) <- ID
    },
    "txt" = if (render_type == "table") {
      ID <- rownames(df)
    }
  )
  
  return(switch(render_type, "table" = cbind(ID, df), "graph" = df, df))
}

# Function for Random Subsampling ----
sampler <- function(data, rng) {
  dataColumn <- as.matrix(data[,1])
  newData <- apply(dataColumn, 1, function(var) random_gen(var, rng))
  for (x in 2:ncol(data)) {
    dataColumn <- as.matrix(data[,x])
    newData <- cbind(newData, apply(dataColumn, 1, function(var) random_gen(var, rng)))
  }
  colnames(newData) <- colnames(data)
  return(newData)
}

# Function for random number generator (Random Subsampling) ----
random_gen <- function(data, rng) {
  if (rng != 0) {
    set.seed(rng)
    randomNum <- sample(c(0:data), 1, replace = F)
  }
  else {
    randomNum <- sample(c(0:data), 1, replace = F)
  }
  return(randomNum)
}

impute <- function(data, rng) {
  if (rng == 0) {
    imp <- mice(data)
  }
  else {
    imp <- mice(data, seed = rng)
  }
  MIData <- complete(imp)
  rownames(MIData) <- rownames(data)
  MIData <- as.matrix(MIData)
  return(MIData)
}

# Function for generic barplot generator ----
barplot_gen <- function(norm_data, colNum, binNum) {
  for (i in 1:colNum) {
    selected_data <- head(sort(norm_data[, i], decreasing = TRUE), binNum)
    plot <- barplot(selected_data,
                    main = colnames(norm_data)[i],
                    ylim = range(pretty(c(0, norm_data[, i]+0.01))),
                    las = 2, cex.axis = 1.5, cex.names = 1.5)
  }
  return(plot)
}

# Function for saving validation data ----
saveData <- function(data, folder) {
  # Create a unique file name
  fileName <- sprintf("%s_%s.txt", as.integer(Sys.time()), digest::digest(data))
  # Write the file to the local system
  write.table(
    x = data,
    file = file.path(folder, fileName), 
    sep = "\t", row.names = TRUE, quote = TRUE
  )
}

# Function for testing validation data ----
loadData <- function() {
  # Read files
  origin_data <- Sys.glob("C:/Users/Administrator/Desktop/CPSC448/EDUCE/_User/Validation_Data/*.txt")
  norm_data <- Sys.glob("C:/Users/Administrator/Desktop/CPSC448/EDUCE/_User/Validation_Norm_Data/*.txt")
  cryptoID <- c()
  testResult <- c()
  for (i in 1:length(origin_data)) {
    df <- read.csv(origin_data[i], header = TRUE, sep = "\t")
    df[df==0] <- NA
    df <- t(df)
    data <- impute(df, 123)
    test_data <-as.matrix(read.csv(norm_data[i], header = TRUE, sep = "\t"))
    testResult <- data == test_data
  }
  
  return(!any(testResult == 0))
}

# End of Functions ---------------------------------------------------------------------------------

# Server -------------------------------------------------------------------------------------------

# Define server logic to read selected file ----
server <- function(input, output, session) {
  
  # Reactive Table Variable ----
  dfTable <- reactive({
    # input$fileData will be NULL initially. After the user selects
    # and uploads a file, head of that data file by default,
    # or all rows if selected, will be shown.
    req(input$fileData)
    
    # Read input files ----
    df <- file_reader(input$fileData$datapath, input$header, input$sep, input$quote)
    df <- file_converter(df, input$type, "table")
  })
  
  # Reactive Graph Variable ----
  dfGraph <- reactive({
    req(input$fileData)
    
    # Read input files ----
    df <- file_reader(input$fileData$datapath, input$header, input$sep, input$quote)
    df <- file_converter(df, input$type, "graph")
  })
  
  # Data for Random Subsampling ----
  sData <- reactive({
    data <- as.matrix(dfGraph())
    data <- sampler(data, input$rng)
  })
  
  # Data for Multiple Imputation ----
  miGraph <- reactive({
    df <- dfGraph()
    df[df==0] <- NA
    data <- impute(df, input$rng)
  })
  
  # Data for VST ----
  vstGraph <- reactive({
    data <- dfGraph()
    data <- varianceStabilizingTransformation(data)
  })
  
  # When the Validation button is clicked, save the form data ----
  observeEvent(input$validate, {
    req(input$fileData)
    
    # Read input files ----
    df <- file_reader(input$fileData$datapath, input$header, input$sep, input$quote)
    
    # Save validation data ----
    saveData(df, "C:/Users/Administrator/Desktop/CPSC448/EDUCE/_User/Validation_Data")
    saveData(miGraph(), "C:/Users/Administrator/Desktop/CPSC448/EDUCE/_User/Validation_Norm_Data")
    
    shinyalert(title = "Data saved for validation!", animation = FALSE)
  })
  
  # When test button is clicked, check whether files are the same for current app version ----
  observeEvent(input$testrun, {
    testResult <- loadData()
    if (testResult) {
      shinyalert(title = "Current App Version Passes Validation!", animation = FALSE)
    }
    else {
      shinyalert(title = "Current App Version Failed Validation. Please Check Test Files or App Code.", animation = FALSE)
    }
  })  
  
  # Section where server renders table based on data and user inputs ----
  output$dt <- renderTable({
    return(dfTable())
  })
  
  output$rst <- renderTable({
    ID <- rownames(sData())
    return(cbind(ID, sData()))
  })
  
  output$mit <- renderTable({
    ID <- rownames(miGraph())
    return(cbind(ID, miGraph()))
  })
  
  output$vstt <- renderTable({
    ID <- rownames(vstGraph())
    return(cbind(ID, vstGraph()))
  })
  
  # Section where server renders plots for Percentage Relative Abundance ----
  output$pra <- renderPlot({
    # Convert data to matrix and normalize each column ----
    data <- as.matrix(dfGraph())
    norm_data <- t(t(data)/colSums(data))
    
    # Organize placements of normalized graphs ----
    par(mfrow = c(input$dataNum, 1), mar = c(7, 4, 4, 2) + 0.1)
    
    # Dynamically generate barplots based on number of columns ----
    barplot_gen(norm_data, input$dataNum, input$bins)
  }, 
  # Dynamically generates space for graphs based on number of columns ----
  height = function(){500*input$dataNum})
  
  # Section where server renders plots for Random Subsampling ----
  output$rs <- renderPlot({
    # Convert data to matrix ----
    sData <- sData()
    
    # Randomly subsample each sample that is not 0, and normalize each column ----
    
    norm_data <- t(t(sData)/colSums(sData))
    
    # Organize placements of normalized graphs ----
    par(mfrow = c(input$dataNum, 1), mar = c(7, 4, 4, 2) + 0.1)
    
    # Dynamically generate barplots based on number of columns ----
    barplot_gen(norm_data, input$dataNum, input$bins)
  }, 
  # Dynamically generates space for graphs based on number of columns ----
  height = function(){500*input$dataNum})
  
  # Section where server renders plots for Multiple Imputation ----
  output$mi <- renderPlot({
    # Impute data and normalize ----
    MIData <- miGraph()
    
    norm_data <- t(t(MIData)/colSums(MIData))
    
    # Organize placements of normalized graphs ----
    par(mfrow = c(input$dataNum, 1), mar = c(7, 4, 4, 2) + 0.1)
    
    # Dynamically generate barplots based on number of columns ----
    barplot_gen(norm_data, input$dataNum, input$bins)
  }, 
  # Dynamically generates space for graphs based on number of columns ----
  height = function(){500*input$dataNum})
  
  # Section where server renders plots for VST ----
  output$vst <- renderPlot({
    # Impute data and normalize ----
    VSTData <- vstGraph()
    
    #norm_data <- t(t(VSTData)/colSums(VSTData))
    
    # Organize placements of normalized graphs ----
    par(mfrow = c(input$dataNum, 1), mar = c(7, 4, 4, 2) + 0.1)
    
    # Dynamically generate barplots based on number of columns ----
    barplot_gen(VSTData, input$dataNum, input$bins)
  }, 
  # Dynamically generates space for graphs based on number of columns ----
  height = function(){500*input$dataNum})
}

# End of Server ------------------------------------------------------------------------------------

# Launches UI and server ----
shinyApp(ui, server)
