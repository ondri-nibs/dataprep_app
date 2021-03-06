#' @title getNumComponents
#'
#' @description Getter function to retrieve the number of components to set
#' the slider value in step #7.
#' @author Jedid Ahn
#' 
#' @param input Shiny input
#' @param rvStep6Results Reactive value list containing transDataDF, asIsDF,
#' DF_EXCLUDED_COLS
#' @param rvVarDataTypes Reactive value list containing catVars, ordVars,
#' numVars, asIsVars, excludeVars
#' 
#' @return List containing max and default number of components to impute on.
#' 
getNumComponents <- function(input, rvStep6Results, rvVarDataTypes){
  transDataDF <- rvStep6Results()$transDataDF
  types <- rvVarDataTypes()
  
  # All continuous/numeric variables.
  if (length(types$catVars) == 0 && length(types$ordVars) == 0){
    ## eliminated the need for a temp data
    cumulativePercent <- transDataDF %>% 
      as.data.frame %>%
      tidyr::replace_na(., as.list(colMeans(.,na.rm=T))) %>%
      scale() %>%
      as.matrix() %>%
      GSVD::tolerance_svd() %>%
      purrr::pluck("d") %>% 
      {.^2/sum(.^2)} %>% 
      cumsum
  }
  # Mix of categorical, ordinal, and continuous/numeric variables.
  else{
    ## eliminated the need for a temp data
    cumulativePercent <- transDataDF %>% 
      as.data.frame %>%
      tidyr::replace_na(., as.list(colMeans(.,na.rm=T))) %>%
      ours::ca_preproc(.) %>%
      purrr::pluck("weightedZx") %>%
      as.matrix() %>%
      GSVD::tolerance_svd() %>%
      purrr::pluck("d") %>% 
      {.^2/sum(.^2)} %>% 
      cumsum
    
  }
  
  # cumulativePercent <- tempData %>% 
  #   as.matrix() %>% 
  #   GSVD::tolerance_svd(.) %>% 
  #   purrr::pluck("d") %>% 
  #   {.^2/sum(.^2)} %>% 
  #   cumsum
  
  # Where to set the slider - the maximum number of components between either:
  # the component that explains (cumulatively) more than 50% or 2 components.
  
  ## only -1 because missMDA has a silly dimensionality test
  sliderMax <- length(cumulativePercent) - 1
  sliderDefault <- max( c(min(which(cumulativePercent > 0.5)), 2) )
  
  return (list(sliderMax = sliderMax, sliderDefault = sliderDefault))
}

# [END]