get_token <- function (auth_code, client_id, 
        client_secret, 
        redirect_uri,
        scope_list) 
{
        my_drive_app <- httr::oauth_app("google",
                key = client_id, 
                secret = client_secret)
        req <- httr::POST("https://accounts.google.com/o/oauth2/token", 
                body = list(code = auth_code, client_id = client_id, 
                        client_secret = client_secret, redirect_uri = redirect_uri, 
                        grant_type = "authorization_code"), verbose = TRUE)
        
        stop_for_content_type(req, "application/json; charset=utf-8")
        
        token <- httr::content(req, type = "application/json")
        
        token_formatted <- httr::Token2.0$new(app = my_drive_app, 
                endpoint = httr::oauth_endpoints("google"),
                credentials = list(access_token = token$access_token, 
                        token_type = token$token_type, expires_in = token$expires_in, 
                        refresh_token = token$refresh_token),
                params = list(scope = scope_list, 
                        type = NULL, use_oob = FALSE, as_header = TRUE), 
                cache_path = FALSE)
        
        token_formatted
}

stop_for_content_type <- function(req, expected) {
        actual <- req$headers$`Content-Type`
        if (actual != expected) {
                stop(
                        sprintf(
                                paste0("Expected content-type:\n%s",
                                        "\n",
                                        "Actual content-type:\n%s"),
                                expected, actual
                        )
                )
        }
        invisible(NULL)
}

shiny_get_url <- function(session){
        
        if(!is.null(session)){
                pathname <- session$clientData$url_pathname
                hostname <- session$clientData$url_hostname
                port <- session$clientData$url_port
                
                url <- paste0(session$clientData$url_protocol,
                        "//",
                        hostname,
                        if(port != "") paste0(":", port),
                        if(pathname != "/") pathname) 
                url
        } else {
                NULL
        }
}