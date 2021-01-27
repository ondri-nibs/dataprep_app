#' @title getVarDataTypes
#'
#' @description Getter function to retrieve the variable data types after
#' selection by user in step #6 of the pipeline. The 5 data types are as 
#' follows: CATEGORICAL, ORDINAL, NUMERIC, AS IS, EXCLUDE
#' @author Jedid Ahn
#' 
#' @param input Shiny input
#' @param rvStep5Results Reactive value list containing DF_DROPPED_COLS, 
#' DF_DROPPED_ROWS, varNames, parNames, dataDF
#' @param rvDictDF Reactive value containing the dictionary data frame
#' 
#' @return List of 5 vectors, where each vector contains variable 
#' names for a specific data type.
#' 
getVarDataTypes <- function(input, rvStep5Results, rvDictDF){
  dataDF <- rvStep5Results()$dataDF
  dictDF <- rvDictDF()
  
  catVars <- character()
  ordVars <- character()
  numVars <- character()
  asIsVars <- character()
  excludeVars <- character()
  
  # Exclude the ONDRI standard columns if a data dictionary exists.
  if (!is.null(dictDF)){
    startColNum <- max(getStandardCols(dictDF, FALSE)) + 1
  }
  # Otherwise, start from the 1st column (subject IDs represent the row names).
  else{
    startColNum <- 1
  }
  
  
  # Go through each variable (excluding the ONDRI standard columns if they
  # exist) and identify the variable data type selected.
  for (i in startColNum:ncol(dataDF)){
    if (input[[colnames(dataDF)[i]]] == "CATEGORICAL"){
      catVars <- c(catVars, colnames(dataDF)[i])
    }
    else if (input[[colnames(dataDF)[i]]] == "ORDINAL"){
      ordVars <- c(ordVars, colnames(dataDF)[i])
    }
    else if (input[[colnames(dataDF)[i]]] == "NUMERIC"){
      numVars <- c(numVars, colnames(dataDF)[i])
    }
    else if (input[[colnames(dataDF)[i]]] == "AS IS"){
      asIsVars <- c(asIsVars, colnames(dataDF)[i])
    }
    else if (input[[colnames(dataDF)[i]]] == "EXCLUDE"){
      excludeVars <- c(excludeVars, colnames(dataDF)[i])
    }
  }
  
  return (list(catVars = catVars, ordVars = ordVars, numVars = numVars,
               asIsVars = asIsVars, excludeVars = excludeVars))
}

# [END]