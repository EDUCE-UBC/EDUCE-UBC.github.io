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
    ### List course dir
    courses <- Sys.glob("Rmd_Input/*/")
    ### Create blank list to hold file names
    files <- list()
    ### List all files from course dirs, grouped by course and named by file name
    for(c in 1:length(courses)){
      path <- paste(courses[c], "*.Rmd", sep="")
      files[c] <- list(Sys.glob(path))
      names(files[[c]]) <- gsub("_", " ",
                           gsub("\\.Rmd", "",
                           gsub("^Rmd_Input/.*/", "", files[[c]])))
      }
    ### Name lists by course
    names(files) <- gsub("_", " ", gsub("/", "", gsub("^Rmd_Input/", "", courses)))


# Define UI for data download app ----
ui <- fluidPage(titlePanel("EDUCE Course compiler"),
                id = "navibar",
                theme = shinytheme("lumen"),
                useShinyjs(), useShinyalert(),
                
      # Sidebar on left ----
      sidebarLayout(position="left",
          
          # Sidebar content
          sidebarPanel(width=4,
          # Logo
          HTML('<center><img src = "https://raw.githubusercontent.com/EDUCE-UBC/EDUCE-UBC.github.io/master/Images/EDUCE_Globe.png",
              height = 280, width = 280></center>'),
          br(),
          tags$b("Experiential Data science for Undergraduate Cross-disciplinary Education"),
          br(),
          br(),
          "Course Compiler allows you to access all of EDUCE's data science modules for teaching or self-learning purposes. Simply select the content that fits your interests and have the Course Compiler create all the materials that you will need including:",
          br(),br(),
          tags$b("Documents "), "such as introductions, instructions, and cheatsheets,",
          br(),
          tags$b("Tutorials "), "with coding examples and explanations,",
          br(),
          tags$b("Practice "), "with exercises to test your knowledge.",
          br(),br(),
          # Return to EDUCE Button
          actionButton("return", label = "Return to EDUCE", icon = icon("home"), onclick ="location.href='https://educe-ubc.github.io/';")
                      ),

      # Main content ----
          mainPanel(width=8,
          # Input: Choose courses
          selectInput("courses", tags$b("Select course content:"), 
                      multiple = TRUE, choices = files),
          # Select All Button
          actionButton("all", label = "Select all"),
          # Deselect All Button
          actionButton("none", label = "Deselect all"),
          
          # Horizontal Line
          tags$hr(),
                    
          # Download Button with file types
          radioButtons('format', tags$b("Select download format:"), 
                       c('PDF', 'HTML'),
                       inline = TRUE),
          downloadButton('downloadReport'),
          
          # Horizontal Line
          tags$hr(),
          
          # Link to GitHub for Rmds
          tags$i("Raw Rmarkdowns are available on our ",
          tags$a(href = "https://github.com/EDUCE-UBC/EDUCE-UBC.github.io/tree/master/Downloadable_Apps", "GitHub"))
                 
                 )))

# Define server logic to display and download selected file ----
server <- function(input, output, session) {
  # Pop-up for Select All Button ----
  observeEvent(input$all, {
    updateSelectInput(session, "courses",
                      label = "Select course content:",
                      choices = files,
                      selected = unlist(files))
    })
  
  # Pop-up for Deselect All Button ----
  observeEvent(input$none, {
    reset("courses")
    actionButton("all", label = "Select all")
    actionButton("none", label = "Deselect all")
    })
  
  # Download handler for course knitter ----
  output$downloadReport <- downloadHandler(
    # File output name ----
    filename = function() {
      paste('EDUCE_data_science_curriculum', sep = '.', switch(input$format, PDF = 'pdf', HTML = 'html'))
    },
    
    # Function for outputting file ----
    content = function(file) {
      # Clear out folders ----
      unlink("./Rmd_Output", recursive = TRUE)
      
      # Create Rmd_Output folder if it doesn't exist ----
      if (!dir.exists("./Rmd_Output")) {
        dir.create("./Rmd_Output")
      }
      
      # Input courses based on user ----
      input_list = list()
      course_list <- unlist(files)
      input_list <- append(input$courses, input_list)
      
      if (length(input_list) == 0) {
        shinyalert(title = "Please select content to compile.", animation = FALSE)
      }

      # Copy all files in Rmd_Input to Rmd_Output ----
      # Create YAML header of Rmd ----
      title <- paste0(
"---\n",
"title: 'EDUCE course files'\n",
"output:\n",
"  html_document:\n",
"    toc: yes\n", 
"    toc_depth: 4\n",
"    toc_float:\n",
"      collapsed: false\n",
"      smooth_scroll: true\n", 
"  pdf_document:\n",
"    toc: yes\n",
"    toc_depth: 4\n",
"    number_sections: yes\n",
"    df_print: kable\n",
"urlcolor: blue\n",
"---\n",
## Add EDUCE info module
"```{r child = 'Rmd_Input/Introduction_to_EDUCE.Rmd'}\n",
"```\n"
                    )
      
      # Set list of Rmd file inputs ----
      rmd <- input_list
      
      # Generate R children ----
      if (length(rmd) != 0) {
        chunks <- paste0("```{r child = '", rmd, "'}\n```\n")
        cat(title, chunks, file = "Rmd_Master_File.Rmd", sep = '\n')
      }
      
      # Print message for user to select some files ----
      else if (length(rmd) == 0) {
        chunks <- paste0("No Files Selected.")
        cat(title, chunks, file = "Rmd_Master_File.Rmd", sep = '\n')
      }
      
      # Display file to user for download ----
      out <- render("Rmd_Master_File.Rmd", switch(
        input$format,
        PDF = pdf_document(), HTML = html_document()
      ))

      # Rename file ----
      file.rename(out, file)
    }
  )
}

shinyApp(ui, server)