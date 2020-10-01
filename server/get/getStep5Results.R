# getStep5Results.R
#
# Purpose: Return the updated data data frame after step #5 is complete 
# along with the variables and participants that were dropped.
#
# Date: 2020-03-17
#
# ==============================================================================

getStep5Results <- function(input, rvOriginalDataDF, rvDictDF, 
                            ONDRIMissingCodes){
  originalDataDF <- rvOriginalDataDF()
  dictDF <- rvDictDF()
  
  # Column drop: Variable names.
  varNames <- c()
  # Row drop: Participant IDs/names.
  parNames <- c()
  
  # Replace all ONDRI missing codes with NA's. IMPORTANT: CONDITIONAL
  # BLANKS REMAIN.
  
  # BEFORE: Old code with gather and spread was not preserving order of 
  # subject IDs.
  # dataDF <- originalDataDF %>% 
  #   gather(-SUBJECT, key = "VARIABLE", value = "VALUE") %>%
  #   mutate(VALUE = replace(VALUE, VALUE %in% ONDRIMissingCodes, NA)) %>%
  #   spread(key = "VARIABLE", value = "VALUE")
  
  # NEW: New code with pivot_longer and pivot_wider maintains order of 
  # subject IDs.
  dataDF <- originalDataDF %>%
    mutate_all(as.character) %>% 
    pivot_longer(-SUBJECT, names_to = "VARIABLE", values_to = "VALUE") %>% 
    mutate(VALUE = replace(VALUE, VALUE %in% ONDRIMissingCodes, NA)) %>% 
    pivot_wider(names_from = "VARIABLE", values_from = "VALUE") %>% 
    as.data.frame()
  
  # Add SUBJECT as row name.
  row.names(dataDF) <- dataDF$SUBJECT
  
  # Retrieve ONDRI standard columns if data dictionary exists.
  if (!is.null(dictDF)){
    standardColNums <- getStandardCols(dictDF, TRUE)
  }
  # Retrieve SUBJECT variable only.
  else{
    standardColNums <- which(colnames(originalDataDF) == "SUBJECT")
  }
  
  # Remove standard columns from data frame before running algorithm.
  dataDF <- dataDF[ , -c(standardColNums) ]
  
  # ============================================================================
  
  # ALGORITHM STARTS HERE.  
  while (any(((colSums(is.na(dataDF)) / nrow(dataDF)) > (input$varThreshold5 / 100))) | 
         any(((rowSums(is.na(dataDF)) / ncol(dataDF)) > (input$parThreshold5 / 100)))){
    
    if (any((colSums(is.na(dataDF)) / nrow(dataDF)) > (input$varThreshold5 / 100))){
      varDrop <- which(
        (colSums(is.na(dataDF)) / nrow(dataDF)) > (input$varThreshold5 / 100)
      )
      varNames <- c(varNames, colnames(dataDF)[varDrop])
      dataDF <- dataDF[ , -c(varDrop) , drop = FALSE ]
    }
    
    # Corner case.
    if (nrow(dataDF) == 0 || ncol(dataDF) == 0){
      break
    }
    
    if (any((rowSums(is.na(dataDF)) / ncol(dataDF)) > (input$parThreshold5 / 100))){
      parDrop <- which(
        (rowSums(is.na(dataDF)) / ncol(dataDF)) > (input$parThreshold5 / 100)
        )
      parNames <- c(parNames, row.names(dataDF)[parDrop])
      dataDF <- dataDF[ -c(parDrop) , , drop = FALSE ]
    }
    
    # Corner case.
    if (nrow(dataDF) == 0 || ncol(dataDF) == 0){
      break
    }
  }
  
  # ============================================================================
  
  # NEW: Export dropped columns (variables) and rows (participants) as 
  # data frames.
  if (length(varNames) > 0){
    DF_DROPPED_COLS <- cbind(originalDataDF[ , standardColNums , drop = FALSE],
                             originalDataDF[ , varNames , drop = FALSE])
  }
  else{
    DF_DROPPED_COLS <- NULL
  }
  
  if (length(parNames) > 0){
    DF_DROPPED_ROWS <- originalDataDF[ originalDataDF$SUBJECT %in% parNames , 
                                       , drop = FALSE] 
  }
  else{
    DF_DROPPED_ROWS <- NULL
  }
  
  # Rebind ONDRI standard columns back to dataDF if data dictionary exists.
  dataDF <- cbind(originalDataDF[ !(originalDataDF$SUBJECT %in% parNames) , 
                                  standardColNums , drop = FALSE], dataDF)
  row.names(dataDF) <- NULL
  dataDF <- dataDF %>% column_to_rownames(var = "SUBJECT")
  
  return (list(DF_DROPPED_COLS = DF_DROPPED_COLS,
               DF_DROPPED_ROWS = DF_DROPPED_ROWS,
               varNames = varNames, parNames = parNames,
               dataDF = dataDF))
}

# [END]