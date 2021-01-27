#' @title installPackages
#'
#' @description A function for installing all required packages and dependencies.
#' @author Jedid Ahn
#' 
#' @export
#' 
installPackages <- function(){
  # CRAN packages.
  cranPackages <- c("devtools", "shiny", "shinydashboard", "shinyFiles", 
                    "shinyWidgets", "shinyjs", "shinycssloaders",
                    "DT", "dplyr", "tidyr", "tibble", "ggplot2", "purrr", 
                    "magrittr", "missMDA")
  
  invisible(sapply(cranPackages, function(p){
    if (p %in% rownames(utils::installed.packages()) == FALSE) {
      utils::install.packages(p)
    }
  }))
  
  
  # GitHub packages.
  if ("dashboardthemes" %in% rownames(utils::installed.packages()) == FALSE) {
    remotes::install_github("nik01010/dashboardthemes")
  }
  if ("GSVD" %in% rownames(utils::installed.packages()) == FALSE) {
    remotes::install_github("derekbeaton/GSVD")
  }
  if ("ours" %in% rownames(utils::installed.packages()) == FALSE) {
    Sys.setenv(R_REMOTES_NO_ERRORS_FROM_WARNINGS = TRUE)
    remotes::install_github("derekbeaton/OuRS/OuRS")
  }
}

# [END]