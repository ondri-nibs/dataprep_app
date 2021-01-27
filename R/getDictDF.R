#' @title getDictDF
#'
#' @description Getter function to retrieve dictionary data frame.
#' @author Jedid Ahn
#' 
#' @param input Shiny input
#' @return Dictionary data frame if it exists, otherwise NULL.
#' 
getDictDF <- function(input){
  # Get the computer volumes.
  computerVolumes <- c(Home = fs::path_home(), "R Installation" = R.home(),
                       shinyFiles::getVolumes()())
  shinyFiles::shinyFileChoose(input, id = "selectDictFile", 
                              roots = computerVolumes, filetypes = c("csv"))
  
  dictFileInfo <- shinyFiles::parseFilePaths(computerVolumes, 
                                             input$selectDictFile)
  
  if (length(dictFileInfo$datapath) > 0){
    dictDF <- utils::read.csv(dictFileInfo$datapath, stringsAsFactors = FALSE,
                              colClasses = "character")
  }
  else{
    dictDF <- NULL
  }

  
  return (dictDF)
}

# [END]