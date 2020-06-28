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

# End of Functions ---------------------------------------------------------------------------------