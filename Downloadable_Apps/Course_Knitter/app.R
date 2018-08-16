#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

# Libraries Used ----
library(ggplot2)
library(magrittr)
library(rmarkdown)
library(shiny)
library(shinyjs)
library(shinyalert)
library(shinythemes)
library(shinyWidgets)

# Extract list of files AND Extract course titles ----
files <- Sys.glob("Rmd_Input/*.Rmd")
courses <- gsub("^Rmd_Input/", "", files)
courses <- gsub("\\.Rmd", "", courses)
courses <- gsub("_", " ", courses)
names(files) <- courses

# Define UI for data download app ----
ui <- fluidPage("", id = "navibar", theme = shinytheme("darkly"), useShinyjs(), useShinyalert(), 
                # Sidebar layout with input and output definitions ----
                sidebarLayout(
                  # Sidebar panel for inputs ----
                  sidebarPanel(
                    # Return to EDUCE Button ----
                    actionButton("return", label = "Return to EDUCE", icon = icon("home"), onclick ="location.href='https://educe-ubc.github.io/';"),
                    
                    # Horizontal Line ----
                    tags$hr(),
                    
                    # Input: Choose courses ----
                    selectInput("courses", "Select your courses:", multiple = TRUE, choices = files),
                    
                    # Select All Courses Button ----
                    actionButton("all", label = "Select All Courses"),
                    
                    # Deselect All Courses Button ----
                    actionButton("none", label = "Deselect All Courses"),
                    
                    # Horizontal Line ----
                    tags$hr(),
                    
                    # Reference: https://stackoverflow.com/questions/22769940/linked-selectinput-controls-in-r-shiny-is-it-possible
                    # CHOICES: 1 Folder ALL FILES, Multiple PROGRAM Folders each containing COURSE FILES
                    
                    #selectInput(multiple = TRUE, "programs", "Programs:",
                    #             list("ProgramA - Intro" = c( "CourseA0 - Intro Command Line", "CourseA1 - Intro Data Science"),
                    #                  "ProgramB - Intermed" = c( "CourseB0 - R", "CourseB1 - Tidyverse"), 
                    #                  "ProgramC - Advanced" = c( "CourseC0 - Phyloseq", "CourseC1 - Microbe Community"))),
                    
                    # Download Button with file types ----
                    radioButtons('format', 'Document format', 
                                 c('PDF', 'HTML', 'ZIP'),
                                 inline = TRUE),
                    downloadButton('downloadReport')
                  ),
                  
                  # Main panel for displaying outputs ----
                  mainPanel(
                    # App Background ----
                    # Soure: https://dribbble.com/shots/2405982-Data-Grid-2
                    #setBackgroundImage(src = "https://cdn.dribbble.com/users/605032/screenshots/2405982/datagrid2.gif")
                    #setBackgroundImage(src = "https://i.gifer.com/Dv9E.gif")
                    #setBackgroundImage(src = "https://i.pinimg.com/originals/20/ee/00/20ee00448f56d10ac7297bf4b62a7f22.gif")
                    setBackgroundImage(src = "https://raw.githubusercontent.com/EDUCE-UBC/EDUCE-UBC.github.io/master/Images/Futuristic_City.jpg")
                  )
                )
)

# Define server logic to display and download selected file ----
server <- function(input, output, session) {
  # Pop-up for Select All Courses Button ----
  observeEvent(input$all, {
    updateSelectInput(session, "courses",
                      label = "Select your courses:",
                      choices = files,
                      selected = files
    )
    
    shinyalert(title = "All Courses Selected!", animation = FALSE)
  })
  
  # Pop-up for Deselect All Courses Button ----
  observeEvent(input$none, {
    reset("courses")
    actionButton("all", label = "Select All Courses")
    actionButton("none", label = "Deselect All Courses")
    shinyalert(title = "All Courses Deselected!", animation = FALSE)
  })
  
  # Download handler for course knitter ----
  output$downloadReport <- downloadHandler(
    # File output name ----
    filename = function() {
      paste('EDUCE_Courses', sep = '.', switch(input$format, PDF = 'pdf', HTML = 'html', ZIP = 'zip'))
    },
    
    # Function for outputting file ----
    content = function(file) {
      # Clear out folders ----
      do.call(file.remove, list(list.files("Rmd_Output", full.names = TRUE)))
      unlink("Download_Files", recursive = TRUE)
      
      # Input courses based on user ----
      input_list = list()
      input_list <- append(input$courses, input_list)
      
      if (length(input_list) == 0) {
        shinyalert(title = "Please select a course.", animation = FALSE)
      }

      # Copy all files in Rmd_Input to Rmd_Output ----
      for (i in input_list) {
        file.copy(i, "Rmd_Output")
      }
      
      # Run script for generating files ---- 
      out <- render("User_File.Rmd", switch(
        input$format,
        PDF = pdf_document(), HTML = html_document(), ZIP = html_document()
      ))
      
      # Display file to user for download ----
      out <- render("Rmd_Master_File.Rmd", switch(
        input$format,
        PDF = pdf_document(), HTML = html_document(), ZIP = html_document()
      ))
      
      if (input$format == "ZIP") {
        fs <- c()
        dir.create("Download_Files")
        
        file.copy("Rmd_Master_File.Rmd", "Download_Files")
        
        file.copy("User_File.Rmd", "Download_Files")
        
        file.copy("Rmd_Output", "Download_Files", recursive = TRUE)
        
        OutputZip <- dir("Download_Files", full.names = TRUE)
        
        zip(zipfile = 'EDUCE_Courses', files = OutputZip)
        
        out <- c("EDUCE_Courses.zip")
      }
      
      # Rename file ----
      file.rename(out, file)
    }
  )
}

shinyApp(ui, server)