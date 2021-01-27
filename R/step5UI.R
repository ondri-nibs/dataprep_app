#' @title step5UI
#'
#' @description User interface for the step #5 (Define missingness thresholds) 
#' tab to select rows and columns to drop from our data frame.
#' @author Jedid Ahn
#' 
step5UI <- function() {
  tabItem(
    tabName = "step5",
    
    fluidRow(
      class = "selection",
      box(
        width = 6,
        sliderInput(
          inputId = "varThreshold5",
          label = "Threshold for Variable Missingness (%):",
          min = 0,
          max = 100,
          value = 10
        )
      ),
      
      box(
        width = 6,
        sliderInput(
          inputId = "parThreshold5",
          label = "Threshold for Participant Missingness (%):",
          min = 0,
          max = 100,
          value = 10
        )
      )
    ),
    
    br(),
    
    fluidRow(
      box(
        id = "varOutput",
        width = 6,
        htmlOutput("varDrop") %>%
          shinycssloaders::withSpinner(type = 7, color = "#5BC0DE")
      ),
      box(
        id = "parOutput",
        width = 6,
        htmlOutput("parDrop") %>%
          shinycssloaders::withSpinner(type = 7, color = "#5BC0DE")
      )
    ),
    
    br(),
    
    fluidRow(
      box(
        width = 12,
        tags$span(class = "header",
                  "Data frame preview after drop:"),
        br(),
        br(),
        DT::dataTableOutput("dataDFOutput5") %>%
          shinycssloaders::withSpinner(type = 7, color = "#5BC0DE")
      )
    ),
    
    br(),
    
    fluidRow(class = "selection",
             box(
               width = 12,
               actionButton(
                 inputId = "confirmDrop5",
                 label = "Confirm dropping",
                 class = "confirm-button"
               ),
               
               actionButton(
                 inputId = "confirmReset5",
                 label = "Reset",
                 class = "reset-button"
               )
             )),
    
    br(),
    
    fluidRow(box(
      width = 12,
      actionButton(
        inputId = "nextStep5",
        label = "Next Step",
        class = "step-button"
      )
    ))
  )
}

# [END]