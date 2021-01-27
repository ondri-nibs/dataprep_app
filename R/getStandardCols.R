#' @title getStandardCols
#'
#' @description Getter function to retrieve the following ONDRI standard 
#' columns: SUBJECT, VISIT, _SITE, _DATE.
#' @author Jedid Ahn
#' 
#' @param dictDF Dictionary data frame.
#' @param includeSubject Optional logical parameter, which indicates whether 
#' SUBJECT column should be included or not (as some data frames will contain
#' the subject ID in the row names).
#' 
#' @return Column numbers corresponding to the ONDRI standard columns. If
#' dictDF is NULL, return numeric(0).
#' 
getStandardCols <- function(dictDF, includeSubject = FALSE){
  colNums <- c()
  
  if (includeSubject){
    # 1) SUBJECT variable.
    colNums <- c(colNums, which(dictDF$COLUMN_LABEL == "SUBJECT"))
  }
  
  # 2) VISIT variable.
  colNums <- c(colNums, which(dictDF$COLUMN_LABEL == "VISIT"))
  
  # 3) Look for DATE variable(s), and set the column number of the last DATE
  # variable as an upper bound for search of SITE variable(s).
  colNums <- c(colNums, which(dictDF$TYPE == "DATE"))
  
  if (length(which(dictDF$TYPE == "DATE")) > 0){
    boundaryCol <- max(which(dictDF$TYPE == "DATE"))
    
    # 4) SITE variable(s).
    siteVars <- which(sapply(strsplit(dictDF$COLUMN_LABEL, split = "_"), 
                             function(x) any(x == "SITE")))
    colNums <- c(colNums, siteVars[siteVars < boundaryCol])
  }
  
  
  if (!includeSubject){
    colNums <- colNums - 1
  }
  
  return (sort(colNums))
}

# [END]