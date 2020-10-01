# app.R
#
# Purpose: Run the ONDRI Data Prep App by connecting user interface (front end)
# with server logic (back end).
#
# Please click on the "Run App" button in the top right corner of this pane to
# open the Shiny app. Please ensure that the app is being run externally by
# clicking the upside down black triangle to the right of "Run App".
#
# ==============================================================================

# shiny libraries
library(shiny)
library(shinydashboard)
library(dashboardthemes)
library(shinyFiles)
library(shinyWidgets)
library(shinyjs)
library(shinyjqui)
library(shinycssloaders)
library(DT)

# tidyverse libraries
library(dplyr)
library(tidyr)
library(tibble)
library(ggplot2)
library(purrr)
library(magrittr)

# imputation library
library(missMDA)

# OuRS package
library(ours)

source("UI/step1UI.R")
source("UI/step2UI.R")
source("UI/step3UI.R")
source("UI/step4UI.R")
source("UI/step5UI.R")
source("UI/step6UI.R")
source("UI/step7UI.R")

source("server/disableSteps.R")
source("server/step1Server.R")
source("server/step2Server.R")
source("server/step3Server.R")
source("server/step4Server.R")
source("server/step5Server.R")
source("server/step6Server.R")
source("server/step7Server.R")

source("server/get/getDictDF.R")
source("server/get/getOriginalDataDF.R")
source("server/get/getStandardCols.R")
source("server/get/getStep5Results.R")
source("server/get/getStep6Results.R")
source("server/get/getStep7Results.R")
source("server/get/getVarDataTypes.R")
source("server/get/getNumComponents.R")

# 1) Window title.
windowTitle <- "Data Preparation App"

# 2) Shiny dashboard header.
header <- dashboardHeader(
    titleWidth = 305,
    title = shinyDashboardLogoDIY(
        boldText = "ONDRI",
        mainText = "Data Prep App",
        textSize = 18,
        badgeText = "0.9.0.9001",
        badgeTextColor = "white",
        badgeTextSize = 2,
        badgeBackColor = "#9d9d9d",
        badgeBorderRadius = 3
    ),
    
    dropdownMenu(
        type = "notifications",
        badgeStatus = "warning",
        notificationItem(
            text = "App is currently in beta phase.",
            icon = icon("exclamation-triangle"),
            status = "warning"
        )
    )
)

# 3) Shiny dashboard sidebar.
sidebar <- dashboardSidebar(
    useShinyjs(),
    tags$head(
        tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
    ),
    width = 305,
    sidebarMenu(
        id = "steps", br(),
        img(class = "ondri-logo", src = "ondri-logo.png"), br(), br(), br(),
        
        menuItem(
            "Step #1: Select data",
            tabName = "step1",
            icon = icon("chevron-circle-right")
        ),
        
        menuItem(
            "Step #2: Select dictionary",
            tabName = "step2",
            icon = icon("chevron-circle-right")
        ),
        
        menuItem(
            HTML(paste0("Step #3: Visualize missingness", "<br/>",
                        "&emsp;&nbsp; by type")),
            tabName = "step3",
            icon = icon("chevron-circle-right")
        ),
        
        menuItem(
            HTML(paste0("Step #4: Assess summary", "<br/>",
                        "&emsp;&nbsp; of missingness")),
            tabName = "step4",
            icon = icon("chevron-circle-right")
        ),
        
        menuItem(
            HTML(paste0("Step #5: Define missingness", "<br/>",
                        "&emsp;&nbsp; thresholds")),
            tabName = "step5",
            icon = icon("chevron-circle-right")
        ),
        
        menuItem(
            HTML(paste0("Step #6: Define variable types","<br/>",
                        "&emsp;&nbsp; and select variables to drop")),
            tabName = "step6",
            icon = icon("chevron-circle-right")
        ),
        
        menuItem(
            "Step #7: Perform imputation",
            tabName = "step7",
            icon = icon("chevron-circle-right")
        )
    )
)

# 4) Shiny dashboard body.
body <- dashboardBody(
    useShinyjs(),
    tags$head(
        tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
    ),
    
    # Special theme.
    shinyDashboardThemes(theme = "grey_light"),
    
    tabItems(
        step1UI(), step2UI(), step3UI(), step4UI(),
        step5UI(), step6UI(dataTypes), step7UI()
    )
)

ui <- dashboardPage(title = windowTitle, header, sidebar, body)

server <- function(input, output, session) {
    # 1) Get ONDRI missing codes and data types.
    appendixDF <- read.csv("appendix/codes.csv", stringsAsFactors = FALSE,
                           colClasses = "character")
    ONDRIMissingCodes <- appendixDF %>%
        dplyr::select(MISSING_CODES) %>%
        dplyr::filter(MISSING_CODES != "") %>%
        pull(MISSING_CODES)
    
    # 2) Create reactive values to pass between steps.
    rv <- reactiveValues(
        dictDF = NULL, originalDataDF = NULL, step5Results = NULL,
        varDataTypes = NULL, step6Results = NULL, step7Results = NULL
    )
    rv$dictDF <- reactive({
        return (getDictDF(input))
    })
    rv$originalDataDF <- reactive({
        return (getOriginalDataDF(input))
    })
    rv$step5Results <- reactive({
        return (getStep5Results(input, rv$originalDataDF, rv$dictDF, 
                                ONDRIMissingCodes))
    })
    rv$varDataTypes <- reactive({
        return (getVarDataTypes(input, rv$step5Results, rv$dictDF))
    })
    rv$step6Results <- reactive({
        return (getStep6Results(input, rv$step5Results, rv$varDataTypes,
                                rv$dictDF))
    })
    rv$step7Results <- reactive({
        return (getStep7Results(input, rv$step6Results, rv$varDataTypes))
    })
    
    # 3) Add in server code for each step in the pipeline.
    disableSteps()
    step1Server(input, output, session, rv$originalDataDF)
    step2Server(input, output, session, rv$originalDataDF, rv$dictDF)
    step3Server(input, output, session, rv$originalDataDF, ONDRIMissingCodes)
    step4Server(input, output, session, rv$originalDataDF, ONDRIMissingCodes)
    step5Server(input, output, session, rv$step5Results)
    step6Server(input, output, session, rv$step5Results, rv$step6Results,
                rv$dictDF)
    step7Server(input, output, session, rv$step5Results, rv$step6Results,
                rv$step7Results, rv$varDataTypes)
}

shinyApp(ui, server)

# [END]