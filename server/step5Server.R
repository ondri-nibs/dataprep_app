# step5Server.R
#
# Purpose: Create the server logic for the step #5 (Define missingness 
# thresholds) tab.
#
# Date: 2020-02-25
#
# ==============================================================================

step5Server <- function(input, output, session, rvStep5Results){
  # Run this code as soon as step 4 is confirmed.
  observeEvent(input$nextStep4, {
    resetStep5(output, session, rvStep5Results)
  })
  
  
  # Slider inputs for variable missingness threshold and participant
  # missingness threshold.
  observeEvent(c(input$varThreshold5, input$parThreshold5), {
    if (input$steps == "step5"){
      updateVarParOutputs(output, rvStep5Results)
    }
  })
  
  
  observeEvent(input$confirmDrop5, {
    dataPreview <- rvStep5Results()$dataDF %>% 
      rownames_to_column(var = "SUBJECT")
    
    # Render preview of modified data frame.
    output$dataDFOutput5 <- DT::renderDataTable({
      DT::datatable(dataPreview, options = list(scrollX = TRUE))
    })
    
    # Disable slider input widgets.
    shinyjs::disable(id = "varThreshold5")
    shinyjs::disable(id = "parThreshold5")
    
    # Disable confirm button and enable reset button.
    shinyjs::disable(id = "confirmDrop5")
    shinyjs::enable(id = "confirmReset5")
    
    # Enable next step button once thresholds are confirmed.
    shinyjs::enable(id = "nextStep5")
  })
  
  
  observeEvent(input$confirmReset5, {
    resetStep5(output, session, rvStep5Results)
  })
  
  
  # "Next Step" button.
  observeEvent(input$nextStep5, {
    shinyjs::enable(selector = "a[data-value='step6']")
    shinyjs::removeCssClass(selector = "a[data-value='step6']", 
                            class = "disable")
    
    updateTabItems(session, "steps", "step6")
  })
  
  
  # If user goes back to step #5, disable steps #6 and #7 as their inputs 
  # will be modified due to changes in step #5.
  observe({
    if (input$steps == "step5"){
      shinyjs::disable(selector = "a[data-value='step6']")
      shinyjs::addCssClass(selector = "a[data-value='step6']",
                           class = "disable")
      shinyjs::disable(selector = "a[data-value='step7']")
      shinyjs::addCssClass(selector = "a[data-value='step7']", 
                           class = "disable")
    }
  })
}


# Helper function to dynamically update the output list of variables and 
# participants to be dropped as the slider value(s) change.
updateVarParOutputs <- function(output, rvStep5Results){
  results <- rvStep5Results()
  varNames <- results$varNames
  parNames <- results$parNames
  
  # Render list of variables and participants that were dropped.
  if (length(varNames) == 0){
    varNames <- c("NONE")
  }
  varDropText <- HTML(paste(varNames, collapse = "<br/>"))
  varDropText <- HTML(paste0(tags$span(class = "header", 
                                       "Variables to be dropped: "), 
                             "<br/>", varDropText))
  
  if (length(parNames) == 0){
    parNames <- c("NONE")
  }
  parDropText <- HTML(paste(parNames, collapse = "<br/>"))
  parDropText <- HTML(paste0(tags$span(class = "header", 
                                       "Participants to be dropped: "), 
                             "<br/>", parDropText))
  
  output$varDrop <- renderPrint(varDropText)
  output$parDrop <- renderPrint(parDropText)
}


# Helper function to avoid duplicate code.
resetStep5 <- function(output, session, rvStep5Results){
  # Clear all output.
  output$varDrop <- renderText("")
  output$parDrop <- renderText("")
  output$dataDFOutput5 <- DT::renderDataTable(NULL)
  
  # Enable slider input widgets.
  shinyjs::enable(id = "varThreshold5")
  shinyjs::enable(id = "parThreshold5")
  
  # Reset slider input values.
  updateSliderInput(
    session,
    inputId = "varThreshold5",
    label = "Threshold for Variable Missingness (%):",
    min = 0,
    max = 100,
    value = 10
  )
  updateSliderInput(
    session,
    inputId = "parThreshold5",
    label = "Threshold for Participant Missingness (%):",
    min = 0,
    max = 100,
    value = 10
  )
  
  # Update output list of variables and participants to be dropped when the thresholds
  # are set to default (10%).
  updateVarParOutputs(output, rvStep5Results)
  
  # Disable reset button and enable confirm button.
  shinyjs::disable(id = "confirmReset5")
  shinyjs::enable(id = "confirmDrop5")
  
  # Disable next step button.
  shinyjs::disable(id = "nextStep5")
}

# [END]