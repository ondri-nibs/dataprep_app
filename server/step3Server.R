# step3Server.R
#
# Purpose: Create the server logic for the step #3 (Visualize missingness 
# by type) tab.
#
# Date: 2019-11-20
#
# ==============================================================================

step3Server <- function(input, output, session, rvDataDF, ONDRIMissingCodes){
  uniqueCases <- c(ONDRIMissingCodes, "Blank", "Data")
  
  # Colour blind friendly palette.
  cbPalette1 <- c("M_CB"="#CC79A7", "M_PI"="#D55E00", "M_VR"="#E69F00", 
                  "M_AE"="#F0E442", "M_DNA"="#009E73", "M_TE"="#56B4E9",
                  "M_NP"="#0072B2", "M_ART"="#4B0082", "M_TBC"="#800080", 
                  "M_OTHER"="#000000", "Blank"="#F5F5F5", "Data"="#999999")
  
  # Variable for storing the ggplot2 object.
  ONDRICasePlot <- NULL
  
  # Run this code as soon as step 2 is confirmed.
  observeEvent(input$nextStep2, {
    dataDF <- rvDataDF()
    
    # This DF is for the missing code missingness plot.
    transDataDF1 <- dataDF %>% 
      dplyr::mutate_all(as.character) %>% 
      tidyr::gather(-SUBJECT, key = "VARIABLE", value = "VALUE") %>%
      dplyr::mutate(VALUE = replace(VALUE, VALUE == "", "Blank")) %>%
      dplyr::mutate(VALUE = replace(VALUE, !(VALUE %in% c(ONDRIMissingCodes, 
                                                          "Blank")), 
                             "Data"))
    # Create count DF for labels.
    countDF1 <- transDataDF1 %>%
      dplyr::group_by(VALUE) %>%
      dplyr::summarise(COUNT = n(), .groups = "drop")
    countDF1 <- dplyr::full_join(
      as.data.frame(uniqueCases, stringsAsFactors = FALSE) %>%
        dplyr::rename(VALUE = uniqueCases),
      countDF1,
      by = "VALUE"
    )
    countDF1$COUNT[is.na(countDF1$COUNT)] <- 0
    # Transform VALUE variable into factor so the full legend can display.
    transDataDF1$VALUE <- factor(transDataDF1$VALUE, levels = uniqueCases)
    
    # Convert to factors to keep order of subject IDs for visualization.
    transDataDF1$SUBJECT <- factor(as.character(transDataDF1$SUBJECT),
                                   levels = rev(unique(transDataDF1$SUBJECT)))
    # Convert to factors to keep order of variables for visualization.
    transDataDF1$VARIABLE <- factor(as.character(transDataDF1$VARIABLE),
                                    levels = unique(transDataDF1$VARIABLE))
    
    # If there are greater than 30 subject IDs, set the Y scale to every 
    # 10th subject.
    if (nrow(dataDF) > 30){
      yScale <- dataDF$SUBJECT[seq(1, nrow(dataDF) , by = 10)]
    }
    # Otherwise, set the Y scale to every subject.
    else{
      yScale <- dataDF$SUBJECT
    }
    
    # If there are greater than 200 variables, set the X scale to every 
    # 100th variable, and remove vertical lines for better visualization.
    if (ncol(dataDF) > 200){
      xScale <- colnames(dataDF)[seq(1, ncol(dataDF), by = 100)]
      
      ONDRICasePlot <- ggplot(data = transDataDF1,
                              aes(x = VARIABLE, y = SUBJECT, fill = VALUE)) +
        geom_raster() +
        scale_x_discrete(breaks = xScale, drop = FALSE) +
        scale_y_discrete(breaks = yScale, drop = FALSE) +
        scale_fill_manual(
          name = "Type",
          values = cbPalette1,
          breaks = uniqueCases,
          labels = paste0(countDF1$VALUE, ": ", countDF1$COUNT),
          drop = FALSE
        ) +
        theme(
          axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          axis.text.x = element_text(
            angle = 90,
            hjust = 1,
            vjust = 0.5
          )
        )
    }
    else{
      ONDRICasePlot <- ggplot(data = transDataDF1,
                              aes(x = VARIABLE, y = SUBJECT, fill = VALUE)) +
        geom_raster() +
        geom_vline(xintercept = seq(0.5, length(transDataDF1$VARIABLE) - 0.5,
                                    1)) +
        scale_y_discrete(breaks = yScale, drop = FALSE) +
        scale_fill_manual(
          name = "Type",
          values = cbPalette1,
          breaks = uniqueCases,
          labels = paste0(countDF1$VALUE, ": ", countDF1$COUNT),
          drop = FALSE
        ) +
        theme(
          axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          axis.text.x = element_text(
            angle = 90,
            hjust = 1,
            vjust = 0.5
          ),
          axis.ticks.x = element_blank()
        )
    }
    
    output$missingnessPlot <- renderPlot({ ONDRICasePlot }, height = 600)
  })
  
  
  # "Next Step" button.
  observeEvent(input$nextStep3, {
    shinyjs::enable(selector = "a[data-value='step4']")
    shinyjs::removeCssClass(selector = "a[data-value='step4']", 
                            class = "disable")
    
    updateTabItems(session, "steps", "step4")
  })
}

# [END]