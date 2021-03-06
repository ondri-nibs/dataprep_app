#' @title step1UI
#'
#' @description User interface for the step #1 (Select data) tab to load in a 
#' data frame from the local computer.
#' @author Jedid Ahn
#' 
step1UI <- function() {
  tabItem(
    tabName = "step1",
    
    fluidRow(
      class = "selection",
      
      box(
        id = "fileSelection1",
        width = 12,
        shinyFiles::shinyFilesButton(
          id = "selectDataFile",
          label = "Select data file from local computer",
          title = "Please select a data csv file to analyze.",
          multiple = FALSE,
          icon = icon("table")
        ),
        verbatimTextOutput("dataFilePathOutput", placeholder = TRUE)
      )
    ),
    
    br(),
    
    fluidRow(
      box(
        width = 12,
        tags$span(class = "header", "Data frame preview:"),
        br(),
        br(),
        DT::dataTableOutput("dataDFOutput1") %>%
          shinycssloaders::withSpinner(type = 7, color = "#5BC0DE")
      )
    ),
    
    fluidRow(box(
      width = 12,
      actionButton(
        inputId = "nextStep1",
        label = "Next Step",
        class = "step-button"
      )
    ))
  )
}

# [END]