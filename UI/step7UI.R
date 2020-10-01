# step7UI.R
#
# Purpose: Create the user interface for the step #7 (Perform imputation) tab 
# to perform imputation on continuous and non-continuous data.
#
# Date: 2020-02-18
#
# ==============================================================================

step7UI <- function() {
  tabItem(
    tabName = "step7",
    
    fluidRow(
      class = "selection",
      box(
        width = 6,
        class = "step7-box",
        radioButtons(
          inputId = "selectImputeOpt7",
          label = "Impute data?",
          choices = c("Yes" = "yes", "No" = "no"),
          inline = TRUE
        )
      ),
      
      box(
        width = 6,
        class = "step7-box",
        sliderInput(
          inputId = "numComponents7",
          label = "Number of components to impute on:",
          min = 2,
          max = 2,
          value = 2
        )
      )
    ),
    
    br(),
    
    fluidRow(
      box(
        width = 12,
        tags$span(class = "header",
                  "Data frame preview after imputation:"),
        br(),
        br(),
        DT::dataTableOutput("dataDFOutput7") %>%
          withSpinner(type = 7, color = "#5BC0DE")
      )
    ),
    
    br(),
    
    fluidRow(class = "selection",
             box(
               width = 12,
               actionButton(
                 inputId = "confirmImputation7",
                 label = "Confirm Selection",
                 class = "confirm-button"
               ),
               
               actionButton(
                 inputId = "confirmReset7",
                 label = "Reset",
                 class = "reset-button"
               )
             )),
    
    br(),
    
    fluidRow(box(
      width = 12,
      actionButton(
        inputId = "exportDF",
        label = "Export into global environment",
        icon = icon("file-export"),
        class = "export-button"
      )
    ))
  )
}

# [END]