#' @title disableSteps
#'
#' @description Server logic to disable all step tabs except the 1st tab 
#' (Select data) at the beginning of the app execution.
#' @author Jedid Ahn
#' 
disableSteps <- function(){
  shinyjs::disable(selector = "a[data-value='step2']")
  shinyjs::addCssClass(selector = "a[data-value='step2']", class = "disable")
  shinyjs::disable(selector = "a[data-value='step3']")
  shinyjs::addCssClass(selector = "a[data-value='step3']", class = "disable")
  shinyjs::disable(selector = "a[data-value='step4']")
  shinyjs::addCssClass(selector = "a[data-value='step4']", class = "disable")
  shinyjs::disable(selector = "a[data-value='step5']")
  shinyjs::addCssClass(selector = "a[data-value='step5']", class = "disable")
  shinyjs::disable(selector = "a[data-value='step6']")
  shinyjs::addCssClass(selector = "a[data-value='step6']", class = "disable")
  shinyjs::disable(selector = "a[data-value='step7']")
  shinyjs::addCssClass(selector = "a[data-value='step7']", class = "disable")
  shinyjs::disable(selector = "a[data-value='step8']")
  shinyjs::addCssClass(selector = "a[data-value='step8']", class = "disable")
}

# [END]