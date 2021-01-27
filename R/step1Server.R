#' @title step1Server
#'
#' @description Server logic for the step #1 (Select data) tab by reading in
#' DATA file from the local computer.
#' @author Jedid Ahn
#' 
#' @param input Shiny input
#' @param output Shiny output
#' @param session Shiny session
#' @param rvDataDF Data file
#' 
step1Server <- function(input, output, session, rvDataDF){
  # Get the computer volumes.
  computerVolumes <- c(Home = fs::path_home(), "R Installation" = R.home(), 
                       shinyFiles::getVolumes()())
  shinyFiles::shinyFileChoose(input, id = "selectDataFile", 
                              roots = computerVolumes, filetypes = c("csv"))
  output$dataFilePathOutput <- renderText("No data file selected.")
  output$dataDFOutput1 <- DT::renderDataTable({DT::datatable(NULL)})
  
  # This code is no longer necessary as the app automatically opens in 
  # a browser now.
  # showModal(
  #   modalDialog(
  #     "If you opened the app through a window, please exit and open it 
  #     through an internet browser instead. For further instructions, please
  #     consult the reference guide.",
  #     title = tags$strong("WARNING"),
  #     footer = modalButton("Ok")
  #   )
  # )
  
  
  # This code is run when the button is clicked.
  observeEvent(input$selectDataFile, {
    output$dataDFOutput1 <- DT::renderDataTable({updateDataDFOutput()})
  })
  
  
  # Update values and data table output.
  updateDataDFOutput <- reactive({
    dataDF <- rvDataDF()
    dataFileInfo <- shinyFiles::parseFilePaths(computerVolumes, 
                                               input$selectDataFile)
    
    if (length(dataFileInfo$datapath) > 0){
      output$dataFilePathOutput <- renderText(dataFileInfo$datapath)
      
      if (!is.null(dataDF)) {
        return (DT::datatable(dataDF, options = list(pageLength = 5, 
                                                     scrollX = TRUE)))
      }
    }
    else{
      output$dataFilePathOutput <- renderText("No data file selected.")
    }
    
    return (DT::datatable(NULL))
  })
  
  
  # "Next Step" button.
  observeEvent(input$nextStep1, {
    dataDF <- rvDataDF()
    dataFileInfo <- shinyFiles::parseFilePaths(computerVolumes, 
                                               input$selectDataFile)
    
    if (length(dataFileInfo$datapath) > 0) {
      if ("SUBJECT" %in% colnames(dataDF)) {
        shinyjs::enable(selector = "a[data-value='step2']")
        shinyjs::removeCssClass(selector = "a[data-value='step2']",
                                class = "disable")
        
        updateTabItems(session, "steps", "step2")
      }
      else{
        shinyWidgets::sendSweetAlert(
          session,
          title = "ERROR",
          text = "Data file requires a SUBJECT variable.",
          type = "error",
          btn_labels = "Ok",
          btn_colors = "#5BC0DE"
        )
      }
    }
    else{
      shinyWidgets::sendSweetAlert(
        session,
        title = "ERROR",
        text = "No data file was chosen.",
        type = "error",
        btn_labels = "Ok",
        btn_colors = "#5BC0DE"
      )
    }
  })
  

  # If user goes back to step #1, disable all subsequent steps.
  observe({
    if (input$steps == "step1") {
      disableSteps()
    }
  })
}

# [END]