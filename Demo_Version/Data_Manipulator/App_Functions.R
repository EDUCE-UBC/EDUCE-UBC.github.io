# Libraries Used ----
#library(DESeq2)
library(grid)
library(ggplot2)
library(gridExtra)
#library(mice)
library(rdrop2)
library(shiny)
library(shinyjs)
library(shinyalert)
library(shinythemes)

# Number of sections to DELETE for downloadable version = 1

# Functions ----------------------------------------------------------------------------------------

# Function for reading OTU/ASV files (CSV/TXT) ----
file_reader <- function(path, col, sep, quote) {
  file <- read.csv(path,
                   header = col,
                   sep = sep,
                   quote = quote)
  return(file)
}

# Function for converting OTU/ASV files to Tables/Graphs ----
file_converter <- function(df, type) {
  # Transpose data so users can scroll vertically ----
  if (ncol(df) > 10) {
    df <- t(df)
  }
  
  # Switch conversion method based on input file ----
  switch(
    type, 
    "OTUTxt" = {
      colnames(df) <- df[1,]
      df <- df[-c(1),]
    },
    "ASVTxt" = {
      rownames(df) <- df[,1]
      df <- df[,-c(1)]
    }
  )
  
  return(df)
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

# DELETE Non-Downloadable Functions (Not needed for versions in public repo) -----------------------

# Read Dropbox token ----
token <- readRDS("droptoken.rds")
# Then pass the token to each drop_ function ----
drop_acc(dtoken = token)

# Function for saving validation data ----
saveData <- function(data, folder, rng, email) {
  # Set Test Number ----
  Origin_Folder <- drop_dir("Validation_Data")
  if (nrow(Origin_Folder) == 0) {
    testNumber <- 0
  }
  else {
    # Obtains last test file name ----
    lastTest <- Origin_Folder$name[nrow(Origin_Folder)]
    # Obtains last test file number ----
    lastNumber <- gsub("_\\S*_\\d*_\\S*.txt", "", lastTest)
    lastNumber <- as.numeric(gsub("Test", "", lastNumber))
    # Set test number for next test file ----
    testNumber <- lastNumber + 1
  }
  # Edit email for storage, can reverse this process by swapping the first and second variables around to obtain email ----
  newMail <- gsub("@", "AT", email)
  newMail <- gsub("\\.", "DOT", newMail)
  
  # Create a unique file name with user inputs: Test #, Rng, system time, and data ----
  fileName <- sprintf("Test%s_RNG%s_%s_%s_%s.txt", testNumber, rng, newMail, as.integer(Sys.time()), digest::digest(data))
  filePath <- file.path(tempdir(), fileName)
  # Write the file to the local system
  write.table(data, filePath, sep = "\t", row.names = TRUE, quote = TRUE)
  drop_upload(filePath, path = folder)
}

# Function for testing validation data ----
loadData <- function() {
  # Read files ----
  Origin_Folder <- drop_dir("Validation_Data")
  Origin_Names <- Origin_Folder$name
  Norm_Folder <- drop_dir("Validation_Norm_Data")
  Norm_Names <- Norm_Folder$name
  # Create variables ----
  cryptoID <- c()
  testResult <- c()
  errorFiles <- c()
  # Compare each file to determine if current app version does the same thing ----
  for (i in 1:length(Origin_Names)) {
    df <- drop_read_csv(file.path("Validation_Data", Origin_Names[i]), header = TRUE, sep = "\t")
    rng <- gsub("_\\d*_\\S*.txt", "", Origin_Names[i])
    rng <- gsub("Test\\d*_RNG", "", rng)
    rng <- as.numeric(gsub("_\\S*", "", rng))
    data <- sampler(df, rng)
    test_data <- as.matrix(drop_read_csv(file.path("Validation_Norm_Data", Norm_Names[i]), header = TRUE, sep = "\t"))
    if (all(data == test_data)) {
      testResult <- c(testResult, TRUE)
    }
    else {
      testResult <- c(testResult, FALSE)
      testNum <- gsub("_\\d*_\\S*.txt", "", Origin_Names[i])
      testNum <- gsub("_RNG\\d*", "", testNum)
      errorFiles <- c(errorFiles, testNum)
    }
  }
  
  # Returns TRUE or list of failed tests ----
  if (any(testResult == FALSE)) {
    return(errorFiles)
  }
  else {
    return(TRUE)
  }
}

# Function for checking available storage space ----
storageCheck <- function() {
  Origin_Folder <- drop_dir("Validation_Data")
  Norm_Folder <- drop_dir("Validation_Norm_Data")
  if (nrow(Origin_Folder) > 1000 && nrow(Norm_Folder) > 1000) {
    return(TRUE)
  }
  else {
    return(FALSE)
  }
}

# End of DELETE Non-Downloadable Functions ---------------------------------------------------------

# End of Functions ---------------------------------------------------------------------------------