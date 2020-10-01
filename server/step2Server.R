# step2Server.R
#
# Purpose: Create the server logic for the step #2 (Select dictionary) tab 
# by reading in DICT file from the local computer.
#
# Date: 2019-11-20
#
# ==============================================================================


step2Server <- function(input, output, session, rvDataDF, rvDictDF){
  # Get the computer volumes.
  computerVolumes <- c(Home = fs::path_home(), "R Installation" = R.home(),
                       getVolumes()())
  shinyFileChoose(input, id = "selectDictFile", roots = computerVolumes,
                  filetypes = c("csv"))
  output$dictFilePathOutput <- renderText("No dictionary file selected.")
  output$dictDFOutput <- DT::renderDataTable({DT::datatable(NULL)})
  
  
  # This code is run when the button is clicked.
  observeEvent(input$selectDictFile, {
    output$dictDFOutput <- DT::renderDataTable({updateDictDFOutput()})
  })
  
  # Update values and data table output.
  updateDictDFOutput <- reactive({
    dictDF <- rvDictDF()
    dictFileInfo <- parseFilePaths(computerVolumes, input$selectDictFile)
    
    if (length(dictFileInfo$datapath) > 0){
      output$dictFilePathOutput <- renderText(dictFileInfo$datapath)
      
      if (!is.null(dictDF)){
        return (DT::datatable(dictDF, options = list(pageLength = 5, 
                                                     scrollX = TRUE)))
      }
    }
    else{
      output$dictFilePathOutput <- renderText("No dictionary file selected.")
    }
    
    return (DT::datatable(NULL))
  })
  
  
  # "Next Step" button.
  observeEvent(input$nextStep2, {
    dataDF <- rvDataDF()
    dictDF <- rvDictDF()
    dictFileInfo <- parseFilePaths(computerVolumes, input$selectDictFile)
    
    if (length(dictFileInfo$datapath) > 0){
      if ("COLUMN_LABEL" %in% colnames(dictDF) && "TYPE" %in% colnames(dictDF)){
        if (nrow(dictDF) == ncol(dataDF)){
          if (all(dictDF$COLUMN_LABEL == colnames(dataDF))){
            shinyjs::enable(selector = "a[data-value='step3']")
            shinyjs::removeCssClass(selector = "a[data-value='step3']", 
                                    class = "disable")
            
            updateTabItems(session, "steps", "step3")
          }
          else{
            sendSweetAlert(
              session,
              title = "ERROR",
              text = "Column labels in the dictionary do not line up with
              variables in the data.",
              type = "error",
              btn_labels = "Ok",
              btn_colors = "#5BC0DE"
            )
          }
        }
        else{
          sendSweetAlert(
            session,
            title = "ERROR",
            text = "The number of column labels in the dictionary need to
            match the number of variables in the data.",
            type = "error",
            btn_labels = "Ok",
            btn_colors = "#5BC0DE"
          )
        }
      }
      else{
        sendSweetAlert(
          session,
          title = "ERROR",
          text = "Dictionary requires a COLUMN_LABEL and/or TYPE variable,
          as per ONDRI conventions.",
          type = "error",
          btn_labels = "Ok",
          btn_colors = "#5BC0DE"
        )
      }
    }
    else{
      sendSweetAlert(
        session,
        title = "ERROR",
        text = "No dictionary file was chosen.",
        type = "error",
        btn_labels = "Ok",
        btn_colors = "#5BC0DE"
      )
    }
  })
  
  
  # If user goes back to step #2, disable steps #5, #6, and #7 as they 
  # require use of the dictionary file.
  observe({
    if (input$steps == "step2") {
      shinyjs::disable(selector = "a[data-value='step5']")
      shinyjs::addCssClass(selector = "a[data-value='step5']", 
                           class = "disable")
      shinyjs::disable(selector = "a[data-value='step6']")
      shinyjs::addCssClass(selector = "a[data-value='step6']",
                           class = "disable")
      shinyjs::disable(selector = "a[data-value='step7']")
      shinyjs::addCssClass(selector = "a[data-value='step7']",
                           class = "disable")
    }
  })
}

# [END]