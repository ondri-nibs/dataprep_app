#' @title step3UI
#'
#' @description User interface for the step #3 (Visualize missingness by type)
#' tab to visualize missingness by type through colour-coded heat maps.
#' @author Jedid Ahn
#' 
step3UI <- function() {
  tabItem(
    tabName = "step3",
    
    fluidRow(box(
      width = 12,
      height = 625,
      plotOutput("missingnessPlot") %>%
        shinycssloaders::withSpinner(type = 7, color = "#5BC0DE")
    )),
    
    br(),
    
    fluidRow(box(
      width = 12,
      actionButton(
        inputId = "nextStep3",
        label = "Next Step",
        class = "step-button"
      )
    ))
  )
}

# [END]