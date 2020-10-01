# getVarDataTypes.R
#
# Purpose: Return a list of 5 vectors, where each vector contains variable 
# names for a specific data type. The 5 data types are as follows: 
# CATEGORICAL, ORDINAL, NUMERIC, AS IS, EXCLUDE
#
# Each variable's data type will be selected by the user in step #6 of the
# pipeline.
#
# Date: 2020-09-09
#
# ==============================================================================

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