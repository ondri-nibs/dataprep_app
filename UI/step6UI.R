# step6UI.R
#
# Purpose: Create the user interface for the step #6 (Define variable types 
# and select variables to drop) tab to allow the user to customize variable 
# types in their data.  Variable types include the following: TEXT, DATE,
# CATEGORICAL, ORDINAL, NUMERIC, MIXED.
#
# Date: 2020-03-03
#
# ==============================================================================

step6UI <- function(dataTypes) {
  tabItem(
    tabName = "step6",
    
    fluidRow(class = "selection",
             box(
               width = 12,
               class = "step6-box",
               wellPanel(
                 uiOutput("varPane") %>%
                   withSpinner(type = 7, color = "#5BC0DE")
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
          withSpinner(type = 7, color = "#5BC0DE")
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