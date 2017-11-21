google_button_server <- function(input, output, session) {
        library(googledrive)
        library(httr)
        
        source("auth/google_button_values.R")
        source("auth/google_auth_functions.R")
        
        hblab_uri <- reactive({
                res <- shiny_get_url(session)
                #cat("session url: ", res)
                return(res)
        })
        
        output$button <- renderUI({
                if (is.null(isolate(access_token()))) {

                        actionButton(inputId = "auth_button",
                                label = "Authorize App",
                                onclick = paste0(
                                        "location.href='",
                                        httr::modify_url(httr::oauth_endpoints("google")$authorize,
                                                query = list(response_type = "code", client_id = hblab_id,
                                                        redirect_uri = hblab_uri(), scope = hb_scopes,
                                                        state = "securitytoken", access_type = "offline",
                                                        approval_prompt = "auto")),
                                        "';")
                                )
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
                                redirect_uri = hblab_uri(),
                                scope_list = hb_scopes)
                } else {
                        NULL
                }
        })
        
        # drive_auth(oauth_token = "hblab_token.rds", cache = NA, reset = FALSE)
        observeEvent(access_token(), {
                shiny::validate(need(
                        !is.null(access_token()), message = FALSE
                ))
                saveRDS(access_token(), file = "hblab_token.rds")
                drive_auth(oauth_token = "hblab_token.rds",cache = FALSE, reset = FALSE)
                gs_auth(token = access_token(),
                        key = hblab_id,
                        secret = hblab_secret, cache = TRUE)
        })
        
        return(access_token)
}