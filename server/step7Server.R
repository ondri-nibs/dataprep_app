# step7Server.R
#
# Purpose: Create the server logic for the step #7 (Perform imputation) tab.
#
# Date: 2020-02-18
#
# ==============================================================================

step7Server <- function(input, output, session, rvStep5Results, 
                        rvStep6Results, rvStep7Results, rvVarDataTypes){
  sliderValues <- reactiveVal()
  
  # Run this code as soon as step 6 is confirmed.
  observeEvent(input$nextStep6, {
    # Determine the default and max number of components to impute 
    # transDataDF on.
    sliderValues(getNumComponents(input, rvStep6Results, rvVarDataTypes))
    
    resetStep7(output, session, sliderValues())
  })
  
  
  # Disable slider input if user wants to skip the imputation step.
  observeEvent(input$selectImputeOpt7, {
    if (input$selectImputeOpt7 == "no"){
      shinyjs::disable(id = "numComponents7")
    }
    else{
      shinyjs::enable(id = "numComponents7")
    }
  })
  
  
  # Perform imputation once confirmation button is clicked.
  observeEvent(input$confirmImputation7, {
    # Run imputation algorithm.
    dataPreview <- rvStep7Results()
    
    # Output truncated data frame for visualization.
    output$dataDFOutput7 <- DT::renderDataTable({
      DT::datatable(dataPreview, options = list(scrollX = TRUE))
    })
    
    # Disable radio buttons.
    shinyjs::disable(id = "selectImputeOpt7")
    
    # Disable slider input.
    shinyjs::disable(id = "numComponents7")
    
    # Disable confirm button.
    shinyjs::disable(id = "confirmImputation7")
    
    # Enable reset and export buttons.
    shinyjs::enable(id = "confirmReset7")
    shinyjs::enable(id = "exportDF")
  })
  
  
  # Reset button: Clear output.
  observeEvent(input$confirmReset7, {
    resetStep7(output, session, sliderValues())
  })
  
  
  # Export up to 4 data frames into the local environment, as long as
  # they are not NULL.
  observeEvent(input$exportDF, {
    exported <- c("DF_PREPPED_DATA")
    
    DF_PREPPED_DATA <- rvStep7Results()
    assign("DF_PREPPED_DATA", DF_PREPPED_DATA, envir = globalenv())
    
    DF_DROPPED_ROWS <- rvStep5Results()$DF_DROPPED_ROWS
    if (!is.null(DF_DROPPED_ROWS)){
      assign("DF_DROPPED_ROWS", DF_DROPPED_ROWS, envir = globalenv())
      exported <- c(exported, "DF_DROPPED_ROWS")
    }
    
    DF_DROPPED_COLS <- rvStep5Results()$DF_DROPPED_COLS
    if (!is.null(DF_DROPPED_COLS)){
      assign("DF_DROPPED_COLS", DF_DROPPED_COLS, envir = globalenv())
      exported <- c(exported, "DF_DROPPED_COLS")
    }
    
    DF_EXCLUDED_COLS <- rvStep6Results()$DF_EXCLUDED_COLS
    if (!is.null(DF_EXCLUDED_COLS)){
      assign("DF_EXCLUDED_COLS", DF_EXCLUDED_COLS, envir = globalenv())
      exported <- c(exported, "DF_EXCLUDED_COLS")
    }
    
    sendSweetAlert(
      session,
      title = "SUCCESS",
      text = tags$span(
        "The following data frames were exported successfully: ",
        tags$b(paste(exported, collapse = ", ")), ".",
        tags$br(), tags$br(),
        "Please close the app and click the stop button in the RStudio 
        console to view addition(s)."
      ),
      type = "success",
      btn_labels = "Ok",
      btn_colors = "#5BC0DE"
    ) 
  })
  
}

# Helper function to avoid duplicate code.
resetStep7 <- function(output, session, sliderValues){
  # Clear data frame preview.
  output$dataDFOutput7 <- DT::renderDataTable(NULL)
  
  # Enable radio buttons.
  shinyjs::enable(id = "selectImputeOpt7")
  
  # Reset selected radio button.
  updateRadioButtons(
    session,
    "selectImputeOpt7",
    label = "Impute data?",
    choices = c("Yes" = "yes", "No" = "no"),
    selected = NULL,
    inline = TRUE
  )
  
  # Enable slider input.
  shinyjs::enable(id = "numComponents7")
  
  # NEW: Update the default and max slider values.
  updateSliderInput(
    session,
    "numComponents7",
    label = "Number of components to impute on:",
    min = 2,
    max = sliderValues$sliderMax,
    value = sliderValues$sliderDefault,
    step = 1
  )
  
  # Enable confirm button.
  shinyjs::enable(id = "confirmImputation7")
  
  # Disable reset and export buttons.
  shinyjs::disable(id = "confirmReset7")
  shinyjs::disable(id = "exportDF")
}

# [END]