suppressPackageStartupMessages(library(tidyverse))
library(lubridate)
library(shiny)
library(stringr)
library(rdrop2)

# tabs <- c("Tecan","Order Decision Tool","MS Analysis", "Gel")
token_name <- "hblab_token.rds"

function(session, input, output) {
        
        source("auth/google_button_server.R")
        
        # #Opening the session post-auth on the tab that initiated it
        # observeEvent(session$clientData$url_search, {
        #         shiny::validate(need(
        #                 !is.null(google_auth_from()), message = FALSE
        #         ))
        #         query <- parseQueryString(session$clientData$url_search)
        #         if (length(query$code) > 0) {
        #                 updateTabsetPanel(session,
        #                         inputId = "tab",
        #                         selected = google_auth_from())
        #         }
        # })
        
        # Display Google Auth button if no Token
        token_test <- reactive({
                # return(file.exists(token_name)) 
                pars <- parseQueryString(session$clientData$url_search)
                print(paste0("token_test reactive:", length(pars$code)>0))
                return((length(pars$code) > 0))
        })
        output$token_exists <- reactive({token_test()})
        outputOptions(output, "token_exists", suspendWhenHidden = FALSE)
        
        # #Todo: change now that we use tab info
        # google_auth_from <- reactive({
        #         tab <- tabs[file.exists(paste0("temp/tag/",tabs))]
        #         stopifnot(length(tab) <2)
        #         if (token_test() & length(tab) == 1) {
        #                 file.remove(paste0("temp/tag/", tab))
        #         }
        #         print(paste0("google_auth_from called, and answered", tab))
        #         return(tab)
        # })
        # output$auth_with <- reactive({google_auth_from()})
        # outputOptions(output, "auth_with", suspendWhenHidden = FALSE)
        
        #Only start the Google Auth button module if no token
        
        #Todo: retry the multi-button once tecan is backup
        # gel_token <- callModule(google_button_server, "Auth")
        google_token <- callModule(google_button_server, "Auth")
        
        #Leave a trace for when coming back from auth with a new session
        #Store the tab that auth was called from
        # observeEvent(input$auth_button, {
        #         print("input auth button touched!")
        #         if(input$auth_button>0) {
        #                 file.create(paste0("temp/tag/",input$tab))
        #                 print(paste0("created tab tag file:", input$tab))
        #         }
        # })
        
        #DECISION TOOL
        observeEvent(input$decision_go, {
                source("order_dna_decision/decision_server.R")
                callModule(decision_server, "decision")
                })
        
        # # MS
        # source("MS/MS_server.R")
        # callModule(MS_server, "MS")

        
        # TEKAN
        observeEvent(input$tecan_go, {
                shiny::validate(need((input$tecan_go>0), message = FALSE))
                shiny::validate(need(!is.null(google_token()), message = FALSE))
                source("tecan/tecan_server.R")
                callModule(tecan_server, "Tecan", gtoken = readRDS(token_name))
        })
        
        # GEL
        observeEvent(input$gel_go, {
                shiny::validate(need((input$gel_go>0), message = FALSE))
                source("gel/gel_server.R")
                callModule(gel_server, "Gel", gtoken = google_token())
        })

        shiny::onStop(
                fun = function() {
                        if(file.exists("hblab_token.rds")) {file.remove("hblab_token.rds")}
                }
        )
        observeEvent(c(input$tecan_go, input$gel_go), {
                cat("tecan go", input$tecan_go, sep = "\n", "gel go", input$gel_go)
        })
}