MS_server <- function(input, output, session) {
        library(googledrive)
        library(httr)
        library(readxl)
        
        source("MS/MS_values.R")
        source("google_auth_functions.R")
        
        hblab_uri <- reactive({
                shiny_get_url(session)
        })
        
        output$button <- renderUI({
                
                if (is.null(isolate(access_token()))) {
                        tags$a("Authorize App",
                                #Todo: add as a function to google_auth_function.R
                                href = httr::modify_url(httr::oauth_endpoints("google")$authorize, 
                                        query = list(response_type = "code", client_id = hblab_id, 
                                                redirect_uri = hblab_uri(), scope = hb_scopes, 
                                                state = "securitytoken", access_type = "offline", 
                                                approval_prompt = "auto")),
                                class = "btn btn-default")
                } else {
                        return()}
        })
        
        ## Get auth code from return URL
        access_token  <- reactive({
                ## gets all the parameters in the URL. The auth code should be one of them.
                pars <- parseQueryString(session$clientData$url_search)
                
                if (length(pars$code) > 0) {
                        ## extract the authorization code
                        get_token(auth_code = pars$code,
                                client_id = hblab_id,
                                client_secret = hblab_secret,
                                redirect_uri = hblab_uri())
                } else {
                        NULL
                }
        })
        
        output$token <- renderTable ({
                # validate(
                #         need(access_token(),
                #                 message = "Click Authorize (this also resets other tabs)")
                # )
                if (is.null(access_token())) return()
                
                saveRDS(access_token(), file = "hblab_token.rds")
                drive_auth(oauth_token = "hblab_token.rds", cache = NA, reset = FALSE)
                #IMPORTANT Todo: remove Token after usage!
                downloaded_file <- drive_download(
                        file = as_id("https://drive.google.com/open?id=0B5hr9AzcSe-sNUZ5M1BrUDZ0MHFzWEpJdC1hTXlZZ1gwZVdF"),
                        overwrite = TRUE)
                
                sheets_name <- excel_sheets(path = downloaded_file$local_path)
                
                norcoclaurine <- read_excel(path = downloaded_file$local_path, sheets_name[1], skip = 1)
                
                if (file.exists(downloaded_file$local_path)) {
                        file.remove(downloaded_file$local_path)
                }
                
                #Remove empty columns
                norcoclaurine <- select(norcoclaurine, -(1:2))
                
                
                norco <- 
                        norcoclaurine %>%
                        filter(is.na(Level) & !Name == "blank") %>%
                        select(Name, `Final Conc.`, `Acq. Date-Time`) %>%
                        group_by(Name)
                
                norco_summary <- summarise(
                        norco,
                        Mean = mean(`Final Conc.`),
                        SD = sd(`Final Conc.`),
                        SE = sd(`Final Conc.`)/sqrt(n()))
                
                norco_summary
        })
}