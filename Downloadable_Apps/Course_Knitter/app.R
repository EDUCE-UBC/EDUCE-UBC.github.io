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
                    
                    # Download Button with file types ----
                    radioButtons('format', 'Document format', 
                                 c('PDF', 'HTML', 'ZIP'),
                                 inline = TRUE),
                    downloadButton('downloadReport')
                  ),
                  
                  # Main panel for displaying outputs ----
                  mainPanel(
                    setBackgroundImage(src = "https://raw.githubusercontent.com/EDUCE-UBC/EDUCE-UBC.github.io/master/Images/EDUCE_Globe.png")
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
      
      # Create Rmd_Output folder if it doesn't exist ----
      if (!dir.exists("./Rmd_Output")) {
        dir.create("./Rmd_Output")
      }
      
      # Input courses based on user ----
      input_list = list()
      input_list <- append(input$courses, input_list)
      
      if (length(input_list) == 0) {
        shinyalert(title = "Please select a course.", animation = FALSE)
      }
      
      # Copy all files in Rmd_Input to Rmd_Output ----
      if (input$format == 'ZIP') {
        for (i in input_list) {
          file.copy(i, "Rmd_Output")
        }
      }
      
      # Create title of Rmd ----
      title <- paste0("---\n", "title: 'Course Files'\n", "output:\n", "  html_document:\n", "    toc: true\n", 
                      "    toc_depth: 4\n", "    toc_float:\n", "      collapsed: false\n", "      smooth_scroll: true\n", 
                      "  pdf_document:\n", "    toc: yes\n", "    toc_depth: 4\n", "    number_sections: yes\n", "    df_print: kable\n", "---\n")
      
      # Set list of Rmd file inputs ----
      rmd <- input_list
      
      # Generate R children ----
      if (length(rmd) != 0) {
        chunks <- paste0("```{r child = '", rmd, "'}\n```\n")
        cat(title, chunks, file = "Rmd_Master_File.Rmd", sep = '\n')
      }
      
      # Print message for user to select some files ----
      else if (length(rmd) == 0) {
        chunks <- paste0("No Files Selected. Please select some files.")
        cat(title, chunks, file = "Rmd_Master_File.Rmd", sep = '\n')
      }
      
      # Display file to user for download ----
      out <- render("Rmd_Master_File.Rmd", switch(
        input$format,
        PDF = pdf_document(), HTML = html_document(), ZIP = html_document()
      ))
      
      if (input$format == 'ZIP') {
        dir.create("Download_Files")
        
        file.copy("Rmd_Master_File.Rmd", "Download_Files")
        
        file.copy("Rmd_Output", "Download_Files", recursive = TRUE)
        
        file.rename("Download_Files/Rmd_Output", "Download_Files/Rmd_Input")
        
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