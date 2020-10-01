# getOriginalDataDF.R
#
# Purpose: Return the original data data frame if it exists. Otherwise, 
# return NULL.
#
# Date: 2020-03-03
#
# ==============================================================================

getOriginalDataDF <- function(input){
  # Get the computer volumes.
  computerVolumes <- c(Home = fs::path_home(), "R Installation" = R.home(), 
                       getVolumes()())
  shinyFileChoose(input, id = "selectDataFile", roots = computerVolumes, 
                  filetypes = c("csv"))
  
  dataFileInfo <- parseFilePaths(computerVolumes, input$selectDataFile)
  
  if (length(dataFileInfo$datapath) > 0){
    dataFileInfo <- parseFilePaths(computerVolumes, input$selectDataFile)
    dataDF <- read.csv(dataFileInfo$datapath, stringsAsFactors = FALSE, 
                       colClasses = "character")
  }
  else{
    dataDF <- NULL
  }
  
  # NEW: Remove row names from data if they exist.
  row.names(dataDF) <- NULL
  
  return (dataDF)
}

# [END]