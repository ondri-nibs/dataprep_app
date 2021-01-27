#' @title step6Server
#'
#' @description Server logic for the step #6 (Define variable types and select
#' variables to drop) tab.
#' @author Jedid Ahn
#' 
#' @param input Shiny input
#' @param output Shiny output
#' @param session Shiny session
#' @param rvStep5Results Reactive value list containing DF_DROPPED_COLS, 
#' DF_DROPPED_ROWS, varNames, parNames, dataDF
#' @param rvStep6Results Reactive value list containing transDataDF, asIsDF,
#' DF_EXCLUDED_COLS
#' @param rvDictDF Dictionary file
#' 
step6Server <- function(input, output, session, rvStep5Results, 
                        rvStep6Results, rvDictDF){
  # Run this code as soon as step 5 is confirmed.
  observeEvent(input$nextStep5, {
    resetStep6(output, rvStep5Results, rvDictDF)
  })
  
  
  # Confirmation button: Perform transformations on the different data types
  # and then rebind back into 1 data frame.
  observeEvent(input$confirmVarType6, {
    dataPreview <- cbind(rvStep6Results()$asIsDF,
                         rvStep6Results()$transDataDF) %>%
      tibble::rownames_to_column(var = "SUBJECT")
    
    if (is.null(dataPreview)){
      output$dataDFOutput6 <- DT::renderDataTable(NULL)
      shinyWidgets::sendSweetAlert(
        session,
        title = "ERROR",
        text = "No variables were defined. Please define at least 1 variable
        as categorical, ordinal, or numeric.",
        type = "error",
        btn_labels = "Ok",
        btn_colors = "#5BC0DE"
      )
    }
    else{
      # Add data frame preview.
      output$dataDFOutput6 <- DT::renderDataTable({
        DT::datatable(dataPreview, options = list(scrollX = TRUE))
      })
      
      # Disable variable pane box.
      shinyjs::disable(id = "varPane")
      shinyjs::addCssClass(id = "varPane", class = "disable")
      
      # Disable confirm button and enable reset button.
      shinyjs::disable(id = "confirmVarType6")
      shinyjs::enable(id = "confirmReset6")
      
      # Enable next step button once variable types confirmed.
      shinyjs::enable(id = "nextStep6")
    }
  })
  
  
  # Reset button: Reassign selection choices back to default.
  observeEvent(input$confirmReset6, {
    resetStep6(output, rvStep5Results, rvDictDF)
  })
  
  
  # "Next Step" button.
  observeEvent(input$nextStep6, {
    shinyjs::enable(selector = "a[data-value='step7']")
    shinyjs::removeCssClass(selector = "a[data-value='step7']", 
                            class = "disable")
    
    updateTabItems(session, "steps", "step7")
  })
  
  
  # If user goes back to step #6, disable step #7 as its input will be 
  # modified due to changes in step #6.
  observe({
    if (input$steps == "step6"){
      shinyjs::disable(selector = "a[data-value='step7']")
      shinyjs::addCssClass(selector = "a[data-value='step7']", 
                           class = "disable")
    }
  })
}

# ==============================================================================


# Helper function to avoid duplicate code.
resetStep6 <- function(output, rvStep5Results, rvDictDF){
  results <- rvStep5Results()
  varNames <- results$varNames
  dataDF <- results$dataDF
  dictDF <- rvDictDF()
  
  # Clear data frame preview.
  output$dataDFOutput6 <- DT::renderDataTable(NULL)
  
  # Scenario: Dictionary file exists. Choices and selections will be 
  # predefined based on contents of DICT file.
  if (!is.null(dictDF)){
    # Remove variables that were dropped in the previous step from the 
    # DICT file.
    dictDF <- dictDF[ !(dictDF$COLUMN_LABEL %in% varNames) , ]
    
    # Get the column number to start the search at.
    startColNum <- max(getStandardCols(dictDF, TRUE)) + 1
    
    # Create a radio buttons widget for each column name. Selected option will
    # correlate with the data type listed in the DICT file.
    output$varPane <- renderUI({
      lapply(startColNum:nrow(dictDF), function(i) {
        
        colVector <- as.character(dataDF[ , colnames(dataDF)[i - 1] ])
        
        # Check 1: If at least 1 blank cell is detected in the column, the
        # variable will automatically be put into the EXCLUDE category, with
        # no other options being allowed.
        if (any(colVector[!is.na(colVector)] == "")){
          tags$div(
            class = "var-box",
            radioButtons(
              inputId = dictDF$COLUMN_LABEL[i],
              label = dictDF$COLUMN_LABEL[i],
              choices = c("EXCLUDE"),
              inline = TRUE
            )
          )
        }
        # Check 2: If only 1 unique value exists in the column (excluding NA's),
        # only provide AS IS and EXCLUDE as options.
        else if (length(unique(colVector[!is.na(colVector)])) == 1){
          tags$div(
            class = "var-box",
            radioButtons(
              inputId = dictDF$COLUMN_LABEL[i],
              label = dictDF$COLUMN_LABEL[i],
              choices = c("AS IS", "EXCLUDE"),
              inline = TRUE
            )
          )
        }
        # Check 3: If column contains non-numeric values (excluding ONDRI
        # missing codes which were filled in as NA's in step 5), remove 
        # ORDINAL and NUMERIC as options.
        else if (any(is.na(suppressWarnings(
          as.numeric(colVector[!is.na(colVector)])))) && 
          (dictDF$TYPE[i] == "ORDINAL" || dictDF$TYPE[i] == "NUMERIC")) {
          tags$div(
            class = "var-box",
            radioButtons(
              inputId = dictDF$COLUMN_LABEL[i],
              label = dictDF$COLUMN_LABEL[i],
              choices = c("CATEGORICAL", "AS IS", "EXCLUDE"),
              selected = "CATEGORICAL",
              inline = TRUE
            )
          ) 
        }
        # If first 3 checks pass, then check the data type for each variable.
        else{
          if (dictDF$TYPE[i] == "CATEGORICAL") {
            tags$div(
              class = "var-box",
              radioButtons(
                inputId = dictDF$COLUMN_LABEL[i],
                label = dictDF$COLUMN_LABEL[i],
                choices = c("CATEGORICAL", "AS IS", "EXCLUDE"),
                selected = "CATEGORICAL",
                inline = TRUE
              )
            )
          }
          else if (dictDF$TYPE[i] == "ORDINAL") {
            tags$div(
              class = "var-box",
              radioButtons(
                inputId = dictDF$COLUMN_LABEL[i],
                label = dictDF$COLUMN_LABEL[i],
                choices = c("CATEGORICAL", "ORDINAL", "NUMERIC", "AS IS",
                            "EXCLUDE"),
                selected = "ORDINAL",
                inline = TRUE
              )
            )
          }
          else if (dictDF$TYPE[i] == "NUMERIC") {
            tags$div(
              class = "var-box",
              radioButtons(
                inputId = dictDF$COLUMN_LABEL[i],
                label = dictDF$COLUMN_LABEL[i],
                choices = c("ORDINAL", "NUMERIC", "AS IS", "EXCLUDE"),
                selected = "NUMERIC",
                inline = TRUE
              )
            )
          }
          else if (dictDF$TYPE[i] == "MIXED" ||
                   dictDF$TYPE[i] == "TEXT" ||
                   dictDF$TYPE[i] == "DATE") {
            tags$div(
              class = "var-box",
              radioButtons(
                inputId = dictDF$COLUMN_LABEL[i],
                label = dictDF$COLUMN_LABEL[i],
                choices = c("AS IS", "EXCLUDE"),
                selected = "AS IS",
                inline = TRUE
              )
            )
          }
        }
        
      })
    })
  } 
  else{
    varNames <- colnames(dataDF)
    
    # Create a radio buttons widget for each column name. Since no DICT file
    # exists, choices are determined computationally.
    output$varPane <- renderUI({
      lapply(1:ncol(dataDF), function(i) {
        
        colVector <- as.character(dataDF[ , varNames[i] ])
        
        # Check 1: If at least 1 blank cell is detected in the column, the
        # variable will automatically be put into the EXCLUDE category, with
        # no other options being allowed.
        if (any(colVector[!is.na(colVector)] == "")){
          tags$div(
            class = "var-box",
            radioButtons(
              inputId = varNames[i],
              label = varNames[i],
              choices = c("EXCLUDE"),
              inline = TRUE
            )
          )
        }
        # Check 2: If only 1 unique value exists in the column (excluding NA's),
        # only provide AS IS and EXCLUDE as options.
        else if (length(unique(colVector[!is.na(colVector)])) == 1){
          tags$div(
            class = "var-box",
            radioButtons(
              inputId = varNames[i],
              label = varNames[i],
              choices = c("AS IS", "EXCLUDE"),
              inline = TRUE
            )
          )
        }
        else{
          # Check 3: This is a non-numeric column. Treat as categorical.
          if (any(is.na(suppressWarnings(as.numeric(dataDF[ , varNames[i] ]))))){
            tags$div(
              class = "var-box",
              radioButtons(
                inputId = varNames[i],
                label = varNames[i],
                choices = c("CATEGORICAL", "AS IS", "EXCLUDE"),
                selected = "CATEGORICAL",
                inline = TRUE
              )
            )
          }
          # Check 4: Continuous values signal a numeric column.
          else if (any(as.numeric(dataDF[ , varNames[i] ]) %% 1 != 0)){
            tags$div(
              class = "var-box",
              radioButtons(
                inputId = varNames[i],
                label = varNames[i],
                choices = c("ORDINAL", "NUMERIC", "AS IS", "EXCLUDE"),
                selected = "NUMERIC",
                inline = TRUE
              )
            )
          }
          # Check 5: Discrete values signal either ordinal or numeric, so
          # provide both options.
          else{
            tags$div(
              class = "var-box",
              radioButtons(
                inputId = varNames[i],
                label = varNames[i],
                choices = c("CATEGORICAL", "ORDINAL", "NUMERIC", "AS IS",
                            "EXCLUDE"),
                selected = "ORDINAL",
                inline = TRUE
              )
            )
          }
        }
        
      })
    })
  }
  
  # Enable variable pane box.
  shinyjs::enable(id = "varPane")
  shinyjs::removeCssClass(id = "varPane", class = "disable")
  
  # Disable reset button and enable confirm button.
  shinyjs::disable(id = "confirmReset6")
  shinyjs::enable(id = "confirmVarType6")
  
  # Disable next step button.
  shinyjs::disable(id = "nextStep6")
}

# [END]