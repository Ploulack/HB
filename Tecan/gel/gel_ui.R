library(shiny)

gel_ui <- function(id) {
        ns <- NS(id)
        fluidPage(
                sidebarPanel(width = 3,
                        #Todo: Put on two different columns so that they're aligned
                        conditionalPanel(condition = paste0("input['", ns("file"), "'] != '", wait_msg, "'"),
                                actionButton(ns("refresh"),
                                                      label = "Check for new Gel Picture")),
                        
                        selectInput(ns("file"),
                                label = "Select from latest gel captures",
                                choices = list(wait_msg)),
                        # radioButtons(inputId = ns("picture_quality"),
                        #              label = "Change picture quality",
                        #              choiceNames =  c("low", "fast"),
                        #              choiceValues = c("25", "75")),
                        tags$hr(id = ns("_bar")),
                        uiOutput(outputId = ns("buttons"))
                        ),
                
                mainPanel(
                        plotOutput(ns("gel"),
                                dblclick = ns("click"),
                                brush = brushOpts(ns("drag_area"),
                                        delay = 800,
                                        delayType = "debounce",
                                        resetOnNew = TRUE),
                                height = 800)
                )
        )
}