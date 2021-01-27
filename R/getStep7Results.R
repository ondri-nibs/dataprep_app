#' @title getStep7Results
#'
#' @description Getter function to retrieve the updated data data frame after
#' imputation.
#' @author Jedid Ahn
#' 
#' @param input Shiny input
#' @param rvStep6Results Reactive value list containing transDataDF, asIsDF,
#' DF_EXCLUDED_COLS
#' @param rvVarDataTypes Reactive value list containing catVars, ordVars,
#' numVars, asIsVars, excludeVars
#' 
#' @return Prepared data to export.
#' 
getStep7Results <- function(input, rvStep6Results, rvVarDataTypes){
  results <- rvStep6Results()
  types <- rvVarDataTypes()
  
  transDataDF <- results$transDataDF
  asIsDF <- results$asIsDF
  
  imputedDataDF <- NULL
  
  if (input$selectImputeOpt7 == "yes"){
    
    # All continuous/numeric variables.
    if (length(types$catVars) == 0 && length(types$ordVars) == 0){
      if (sum(is.na(transDataDF)) > 0){
        # OLD CODE before manual input of number of components through slider.
        # imputeComponentMax <- ncol(transDataDF) - 1
        # ncpTest <- missMDA::estim_ncpPCA(transDataDF, 
        #                                  ncp.max = imputeComponentMax)
        # output <- missMDA::imputePCA(transDataDF, ncp = ncpTest$ncp)
        # imputedDataDF <- as.data.frame(output$completeObs)
        imputedDataDF <- as.data.frame(
          missMDA::imputePCA(transDataDF, ncp = input$numComponents7, 
                             scale = TRUE)$completeObs
        )
      }
      else{
        imputedDataDF <- transDataDF
      }
    }
    # Mix of categorical, ordinal, and continuous/numeric variables.
    else{
      if (sum(is.na(transDataDF)) > 0){
        imputedDataDF <- as.data.frame(
          missMDA::imputeCA(transDataDF, ncp = input$numComponents7),
          stringsAsFactors = FALSE
        )
      }
      else{
        imputedDataDF <- transDataDF
      }
    }
    
  }
  # Skip imputation step. Leave NA's as they are.
  else if (input$selectImputeOpt7 == "no"){
    imputedDataDF <- transDataDF
  }
  
  # LAST STEP: Bind back ONDRI standard columns and AS IS variables to rest
  # of data.
  DF_PREPPED_DATA <- cbind(asIsDF, imputedDataDF) %>% 
    tibble::rownames_to_column(var = "SUBJECT")
  
  return (DF_PREPPED_DATA)
}

# [END]