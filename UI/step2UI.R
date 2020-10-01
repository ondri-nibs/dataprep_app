# step2UI.R
#
# Purpose: Create the user interface for the step #2 (Select dictionary) tab 
# to load in a data dictionary frame from the local computer.
#
# Date: 2019-11-20
#
# ==============================================================================

step2UI <- function() {
  tabItem(
    tabName = "step2",
    
    fluidRow(
      class = "selection",
      
      box(
        id = "fileSelection2",
        width = 12,
        shinyFilesButton(
          id = "selectDictFile",
          label = "Select dictionary file from local computer",
          title = "Please select a dictionary csv file to analyze.",
          multiple = FALSE,
          icon = icon("table")
        ),
        verbatimTextOutput("dictFilePathOutput", placeholder = TRUE)
      )
    ),
    
    br(),
    
    fluidRow(
      box(
        width = 12,
        tags$span(class = "header", "Data frame preview:"),
        br(),
        br(),
        DT::dataTableOutput("dictDFOutput") %>%
          withSpinner(type = 7, color = "#5BC0DE")
      )
    ),
    
    fluidRow(box(
      width = 12,
      actionButton(
        inputId = "nextStep2",
        label = "Next Step",
        class = "step-button"
      )
    ))
  )
}

# [END]