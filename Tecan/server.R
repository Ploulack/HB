library(shiny); library(tidyverse)
library(lubridate); library(googlesheets)
library(stringr)

token_name <- "hblab_token.rds"

function(input, output, session) {
        source("auth/google_button_server.R")

        # Display Google Auth button if no Token
        token_test <- reactive({
                pars <- parseQueryString(session$clientData$url_search)
                return((length(pars$code) > 0))
        })

        output$token_exists <- reactive({
                # token_test()
                TRUE
                })
        outputOptions(output, "token_exists", suspendWhenHidden = FALSE)

        #Only start the Google Auth button module if no token
        google_token <- callModule(google_button_server, "Auth")

        #DECISION TOOL
        observeEvent(input$decision_go, {
                source("order_dna_decision/decision_server.R")
                callModule(decision_server, "decision", google_token())
                })

        # TECAN
        observeEvent(input$tecan_go, {
                shiny::validate(need((input$tecan_go > 0), message = FALSE))
                shiny::validate(need(!is.null(google_token()), message = FALSE))
                source("tecan/tecan_server.R")
                callModule(tecan_server, "tecan")
                # callModule(tecan_server, "tecan",gtoken = readRDS(token_name))
        })

        # GEL
        observeEvent(input$gel_go, {
                shiny::validate(need((input$gel_go > 0), message = FALSE))
                source("gel/gel_server.R")
                callModule(gel_server, "gel", gtoken = google_token())
        })

        # MS
        observeEvent(input$ms_go, {
                shiny::validate(need((input$ms_go > 0), message = FALSE))
                source("ms/ms_server.R")
                callModule(ms_server, "ms")
        })

        #HAMILTON
        observeEvent(input$hami_go, {
                shiny::validate(need((input$hami_go > 0), message = FALSE))
                source("hamilton/hamilton.R")
                callModule(hami_server, "hami")
        })

        shiny::onStop(
                fun = function() {
                        if (file.exists("hblab_token.rds")) {file.remove("hblab_token.rds")}
                        file.remove(list.files("temp", full.names = TRUE))

                        if (exists("db")) {
                                rm(db)
                        }
                }
        )
}