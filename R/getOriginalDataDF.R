#' @title getOriginalDataDF
#'
#' @description Getter function to retrieve the original data data frame.
#' @author Jedid Ahn
#' 
#' @param input Shiny input
#' @return Data data frame if it exists, otherwise NULL.
#' 
getOriginalDataDF <- function(input){
  # Get the computer volumes.
  computerVolumes <- c(Home = fs::path_home(), "R Installation" = R.home(), 
                       shinyFiles::getVolumes()())
  shinyFiles::shinyFileChoose(input, id = "selectDataFile", 
                              roots = computerVolumes, filetypes = c("csv"))
  
  dataFileInfo <- shinyFiles::parseFilePaths(computerVolumes, 
                                             input$selectDataFile)
  
  if (length(dataFileInfo$datapath) > 0){
    dataFileInfo <- shinyFiles::parseFilePaths(computerVolumes, 
                                               input$selectDataFile)
    dataDF <- utils::read.csv(dataFileInfo$datapath, stringsAsFactors = FALSE, 
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