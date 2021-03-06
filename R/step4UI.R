#' @title step4UI
#'
#' @description User interface for the step #4 (Assess summary of missingness)
#' tab to assess summary of missingness through contingency tables of rows
#' and columns.
#' @author Jedid Ahn
#' 
step4UI <- function() {
  tabItem(
    tabName = "step4",
    
    fluidRow(
      class = "selection",
      
      box(
        width = 6,
        radioButtons(
          inputId = "selectPercentOrCount4",
          label = "View by:",
          choices = c("Percentage" = "byPercentage",
                      "Absolute Count" = "byCount"),
          inline = TRUE
        )
      ),
      
      box(
        width = 6,
        radioButtons(
          inputId = "selectVarOrPar4",
          label = "View by:",
          choices = c("Variable" = "byVariable",
                      "Participant" = "byParticipant"),
          inline = TRUE
        )
      )
    ),
    
    br(),
    
    fluidRow(box(
      width = 12,
      DT::dataTableOutput("contingencyTable") %>%
        shinycssloaders::withSpinner(type = 7, color = "#5BC0DE")
    )),
    
    br(),
    
    fluidRow(box(
      width = 12,
      actionButton(
        inputId = "nextStep4",
        label = "Next Step",
        class = "step-button"
      )
    ))
  )
}

# [END]