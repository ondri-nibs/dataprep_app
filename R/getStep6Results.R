#' @title getStep6Results
#'
#' @description Getter function to retrieve the updated data data frame after
#' performing transformations according to the variable types chosen.
#' @author Jedid Ahn
#' 
#' @param input Shiny input
#' @param rvStep5Results Reactive value list containing DF_DROPPED_COLS, 
#' DF_DROPPED_ROWS, varNames, parNames, dataDF
#' @param rvVarDataTypes Reactive value list containing catVars, ordVars,
#' numVars, asIsVars, excludeVars
#' @param rvDictDF Reactive value containing the dictionary data frame
#' 
#' @return List containing transformed data frame (transDataDF), data frame
#' of columns to leave as is (asIsDF), and data frame of columns to exclude
#' (DF_EXCLUDED_COLS).
#' 
getStep6Results <- function(input, rvStep5Results, rvVarDataTypes, rvDictDF){
  dataDF <- rvStep5Results()$dataDF
  types <- rvVarDataTypes()
  dictDF <- rvDictDF()
  
  transDataDF <- NULL
  
  if (length(types$catVars) > 0){
    catDF <- dataDF[ , types$catVars , drop = FALSE ]
    catDF <- sapply(catDF, as.character)
    # Perform disjunctive transformation when categorical.
    transCatDF <- as.data.frame(ours::disjunctive_coding(catDF), 
                                stringsAsFactors = FALSE)
    transDataDF <- transCatDF
  }
  
  if (length(types$ordVars) > 0){
    ordDF <- dataDF[ , types$ordVars , drop = FALSE ]
    ordDF <- sapply(ordDF, as.numeric)
    
    # Perform thermometer transformation when ordinal.
    transOrdDF <- as.data.frame(ours::thermometer_coding(ordDF), 
                                stringsAsFactors = FALSE)
    
    if (is.null(transDataDF)){
      transDataDF <- transOrdDF
    }
    else{
      transDataDF <- cbind(transDataDF, transOrdDF)
    } 
  }
  
  if (length(types$numVars) > 0){
    numDF <- dataDF[ , types$numVars , drop = FALSE ]
    numDF <- sapply(numDF, as.numeric)
    
    # Continuous when by itself: Do nothing/AS IS.
    if (is.null(types$catVars) && is.null(types$ordVars)){
      invisible()
    }
    # Continuous when with other variable types: Escofier transformation.
    else{
      transNumDF <- as.data.frame(ours::escofier_coding(numDF), 
                                  stringsAsFactors = FALSE)
    }
    
    if (is.null(transDataDF)){
      transDataDF <- numDF
    }
    else{
      transDataDF <- cbind(transDataDF, transNumDF)
    } 
  }
  
  if (!is.null(transDataDF)){
    # Adding subject IDs as row names of transformed data.
    row.names(transDataDF) <- row.names(dataDF)
  }
  
  # Create a separate data frame containing "as is" variables, which includes
  # the ONDRI standard columns.
  if (length(types$asIsVars) > 0){
    asIsDF <- cbind(dataDF[ , getStandardCols(dictDF), drop = FALSE ], 
                    dataDF[ , types$asIsVars , drop = FALSE ])
  }
  else{
    asIsDF <- dataDF[ , getStandardCols(dictDF)]
  }
  
  
  # Create a separate data frame containing the excluded columns.
  if (length(types$excludeVars) > 0){
    DF_EXCLUDED_COLS <- cbind(dataDF[ , getStandardCols(dictDF) , drop = FALSE], 
                              dataDF[ , types$excludeVars , drop = FALSE ]) %>% 
      tibble::rownames_to_column(var = "SUBJECT")
  }
  else{
    DF_EXCLUDED_COLS <- NULL
  }
  
  return (list(transDataDF = transDataDF, asIsDF = asIsDF, 
               DF_EXCLUDED_COLS = DF_EXCLUDED_COLS))
}

# [END]