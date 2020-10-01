# getStandardCols.R
#
# Purpose: Return the column numbers corresponding to the following ONDRI
# standard columns: SUBJECT, VISIT, *_SITE, *_DATE.
#
# includeSubject indicates whether SUBJECT should be included or not, as some
# data frames will contain the subject ID in the row names.
#
# This function will return numeric(0) if dictDF is NULL.
#
# Date: 2020-09-16
#
# ==============================================================================

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