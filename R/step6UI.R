#' @title step6UI
#'
#' @description User interface for the step #6 (Define variable types and
#' select variables to drop) tab to allow the user to customize variable 
#' types in their data.  Variable types include the following: 
#' TEXT, DATE, CATEGORICAL, ORDINAL, NUMERIC, MIXED.
#' @author Jedid Ahn
#' 
step6UI <- function() {
  tabItem(
    tabName = "step6",
    
    fluidRow(class = "selection",
             box(
               width = 12,
               class = "step6-box",
               wellPanel(
                 uiOutput("varPane") %>%
                   shinycssloaders::withSpinner(type = 7, color = "#5BC0DE")
               )
             )),
    
    br(),
    
    fluidRow(
      box(
        width = 12,
        tags$span(class = "header",
                  "Data frame preview after transformations:"),
        br(),
        br(),
        DT::dataTableOutput("dataDFOutput6") %>%
          shinycssloaders::withSpinner(type = 7, color = "#5BC0DE")
      )
    ),
    
    br(),
    
    fluidRow(class = "selection",
             box(
               width = 12,
               actionButton(
                 inputId = "confirmVarType6",
                 label = "Confirm variable types",
                 class = "confirm-button"
               ),
               
               actionButton(
                 inputId = "confirmReset6",
                 label = "Reset",
                 class = "reset-button"
               )
             )),
    
    br(),
    
    fluidRow(box(
      width = 12,
      actionButton(
        inputId = "nextStep6",
        label = "Next Step",
        class = "step-button"
      )
    ))
  )
}

# [END]