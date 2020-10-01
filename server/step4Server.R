# step4Server.R
#
# Purpose: Create the server logic for the step #4 (Assess summary of 
# missingness) tab.
#
# Date: 2020-02-18
#
# ==============================================================================

step4Server <- function(input, output, session, rvDataDF, ONDRIMissingCodes){
  
  # Different contingency tables act as reactive values.
  rvONDRI <-
    reactiveValues(
      percentVariableTable = NULL,
      percentParticipantTable = NULL,
      countVariableTable = NULL,
      countParticipantTable = NULL
    )
  
  # Run this code as soon as step 3 is confirmed.
  observeEvent(input$nextStep3, {
    dataDF <- rvDataDF()
    
    # A) Percentage variable contingency table.
    transDataDF1_PercentVar <- dataDF %>%
      tidyr::gather(-SUBJECT, key = "VARIABLE", value = "VALUE") %>%
      dplyr::mutate(VALUE = replace(VALUE, VALUE == "", "BLANK")) %>%
      dplyr::mutate(VALUE = replace(VALUE, !(VALUE %in% 
                                               c(ONDRIMissingCodes, "BLANK")),
                                    "DATA"))
    
    # Convert to factors to keep order of variables for visualization.
    transDataDF1_PercentVar$VARIABLE <-
      factor(
        as.character(transDataDF1_PercentVar$VARIABLE),
        levels = unique(transDataDF1_PercentVar$VARIABLE)
      )
    
    transDataDF1_PercentVar <- transDataDF1_PercentVar %>%
      dplyr::group_by(VARIABLE, VALUE) %>% 
      dplyr::summarise(COUNT = n(), .groups = "drop") %>%
      dplyr::mutate(PERCENT = paste0(round(COUNT * 100 / nrow(dataDF), 2),
                                     "%")) %>%
      dplyr::select(-COUNT) %>% 
      tidyr::spread(key = VALUE, value = PERCENT, fill = "0%") %>%
      dplyr::select(
        VARIABLE, starts_with("M_CB"), starts_with("M_PI"), starts_with("M_VR"),
        starts_with("M_AE"), starts_with("M_DNA"), starts_with("M_TE"),
        starts_with("M_NP"), starts_with("M_ART"), starts_with("M_TBC"),
        starts_with("M_OTHER"), starts_with("BLANK"), DATA
      )
    
    
    # B) Percentage participant contingency table.
    # ncol(dataDF) - 1 because we omit SUBJECT variable.
    transDataDF1_PercentPar <- dataDF %>%
      tidyr::gather(-SUBJECT, key = "VARIABLE", value = "VALUE") %>%
      dplyr::select(-VARIABLE) %>% 
      dplyr::mutate(VALUE = replace(VALUE, VALUE == "", "BLANK")) %>%
      dplyr::mutate(VALUE = replace(VALUE, !(VALUE %in% 
                                               c(ONDRIMissingCodes, "BLANK")), 
                                    "DATA"))
    
    # Convert to factors to keep order of subjects for visualization.
    transDataDF1_PercentPar$SUBJECT <-
      factor(
        as.character(transDataDF1_PercentPar$SUBJECT),
        levels = dataDF$SUBJECT
      )
    
    transDataDF1_PercentPar <- transDataDF1_PercentPar %>%
      dplyr::group_by(SUBJECT, VALUE) %>% 
      dplyr::summarise(COUNT = n(), .groups = "drop") %>%
      dplyr::mutate(PERCENT = paste0(round(COUNT * 100 / (ncol(dataDF) - 1), 2), 
                              "%")) %>%
      dplyr::select(-COUNT) %>% 
      tidyr::spread(key = VALUE, value = PERCENT, fill = "0%") %>%
      dplyr::select(
        SUBJECT, starts_with("M_CB"), starts_with("M_PI"), starts_with("M_VR"),
        starts_with("M_AE"), starts_with("M_DNA"), starts_with("M_TE"),
        starts_with("M_NP"), starts_with("M_ART"), starts_with("M_TBC"),
        starts_with("M_OTHER"), starts_with("BLANK"), DATA
      )
    
    
    # C) Count variable contingency table.
    transDataDF1_CountVar <- dataDF %>%
      tidyr::gather(-SUBJECT, key = "VARIABLE", value = "VALUE") %>%
      dplyr::mutate(VALUE = replace(VALUE, VALUE == "", "BLANK")) %>%
      dplyr::mutate(VALUE = replace(VALUE, !(VALUE %in% 
                                               c(ONDRIMissingCodes, "BLANK")),
                                    "DATA"))
    
    # Convert to factors to keep order of variables for visualization.
    transDataDF1_CountVar$VARIABLE <-
      factor(
        as.character(transDataDF1_CountVar$VARIABLE),
        levels = unique(transDataDF1_CountVar$VARIABLE)
      )
    
    transDataDF1_CountVar <- transDataDF1_CountVar %>% 
      dplyr::group_by(VARIABLE, VALUE) %>% 
      dplyr::summarise(COUNT = n(), .groups = "drop") %>%
      tidyr::spread(key = VALUE, value = COUNT, fill = 0) %>% 
      dplyr::select(
        VARIABLE, starts_with("M_CB"), starts_with("M_PI"), starts_with("M_VR"),
        starts_with("M_AE"), starts_with("M_DNA"), starts_with("M_TE"), 
        starts_with("M_NP"), starts_with("M_ART"), starts_with("M_TBC"), 
        starts_with("M_OTHER"), starts_with("BLANK"), DATA
      )
    
    
    # D) Count participant contingency table.
    transDataDF1_CountPar <- dataDF %>%
      tidyr::gather(-SUBJECT, key = "VARIABLE", value = "VALUE") %>%
      dplyr::select(-VARIABLE) %>% 
      dplyr::mutate(VALUE = replace(VALUE, VALUE == "", "BLANK")) %>%
      dplyr::mutate(VALUE = replace(VALUE, !(VALUE %in% c(ONDRIMissingCodes, 
                                                          "BLANK")),
                                    "DATA"))
    
    # Convert to factors to keep order of subjects for visualization.
    transDataDF1_CountPar$SUBJECT <- 
      factor(
        as.character(transDataDF1_CountPar$SUBJECT),
        levels = dataDF$SUBJECT
      )
    
    transDataDF1_CountPar <- transDataDF1_CountPar %>% 
      dplyr::group_by(SUBJECT, VALUE) %>% 
      dplyr::summarise(COUNT = n(), .groups = "drop") %>% 
      tidyr::spread(key = VALUE, value = COUNT, fill = 0) %>% 
      dplyr::select(
        SUBJECT, starts_with("M_CB"), starts_with("M_PI"), starts_with("M_VR"),
        starts_with("M_AE"), starts_with("M_DNA"), starts_with("M_TE"), 
        starts_with("M_NP"), starts_with("M_ART"), starts_with("M_TBC"), 
        starts_with("M_OTHER"), starts_with("BLANK"), DATA
      )
    
    
    rvONDRI$percentVariableTable <-
      DT::datatable(transDataDF1_PercentVar, options = list(scrollX = TRUE))
    rvONDRI$percentParticipantTable <-
      DT::datatable(transDataDF1_PercentPar, options = list(scrollX = TRUE))
    rvONDRI$countVariableTable <-
      DT::datatable(transDataDF1_CountVar, options = list(scrollX = TRUE))
    rvONDRI$countParticipantTable <-
      DT::datatable(transDataDF1_CountPar, options = list(scrollX = TRUE))
  })
  
  
  # Update contingency tables depending upon selection.
  observe({
    if (input$selectPercentOrCount4 == "byPercentage") {
      if (input$selectVarOrPar4 == "byVariable") {
        output$contingencyTable <-
          DT::renderDataTable({
            rvONDRI$percentVariableTable
          })
      }
      else if (input$selectVarOrPar4 == "byParticipant") {
        output$contingencyTable <-
          DT::renderDataTable({
            rvONDRI$percentParticipantTable
          })
      }
    }
    else if (input$selectPercentOrCount4 == "byCount") {
      if (input$selectVarOrPar4 == "byVariable") {
        output$contingencyTable <-
          DT::renderDataTable({
            rvONDRI$countVariableTable
          })
      }
      else if (input$selectVarOrPar4 == "byParticipant") {
        output$contingencyTable <-
          DT::renderDataTable({
            rvONDRI$countParticipantTable
          })
      }
    }
  })
  
  
  # "Next Step" button.
  observeEvent(input$nextStep4, {
    shinyjs::enable(selector = "a[data-value='step5']")
    shinyjs::removeCssClass(selector = "a[data-value='step5']", 
                            class = "disable")
    
    updateTabItems(session, "steps", "step5")
  })
}

# [END]