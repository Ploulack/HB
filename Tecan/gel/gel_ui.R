library(shiny)
msg <- "Waiting from Google Drive"
gel_ui <- function(id) {
        ns <- NS(id)
        fluidPage(
                sidebarPanel(#Todo: Put on two different columns so that they're aligned
                        actionButton(ns("refresh"),
                                label = "Check for new Gel Picture"),
                        selectInput(ns("file"),
                                label = "Select from latest gel captures",
                                choices = list(msg)),
                        tags$hr(id = ns("_bar"))
                        ),
                
                mainPanel(
                        plotOutput(ns("gel"),
                                click = ns("click"),
                                brush = brushOpts(ns("drag_area"),
                                        delayType = "debounce",
                                        resetOnNew = TRUE),
                                height = 800)
                )
        )
}