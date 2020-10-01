# getDictDF.R
#
# Purpose: Return the dictionary data frame if it exists. Otherwise, 
# return NULL.
#
# Date: 2020-03-03
#
# ==============================================================================

getDictDF <- function(input){
  # Get the computer volumes.
  computerVolumes <- c(Home = fs::path_home(), "R Installation" = R.home(),
                       getVolumes()())
  shinyFileChoose(input, id = "selectDictFile", roots = computerVolumes, 
                  filetypes = c("csv"))
  
  dictFileInfo <- parseFilePaths(computerVolumes, input$selectDictFile)
  
  if (length(dictFileInfo$datapath) > 0){
    dictDF <- read.csv(dictFileInfo$datapath, stringsAsFactors = FALSE,
                       colClasses = "character")
  }
  else{
    dictDF <- NULL
  }

  
  return (dictDF)
}

# [END]