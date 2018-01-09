library(stringr)

is_dev_server <- function(session) {
        host_name <- session$clientData$url_hostname
        dev_env <- c("ploulack", "127.0.0.1")
        prod_env <- c("hbio")
        if (any(str_detect(host_name, dev_env))) return(TRUE)
        else if (str_detect(host_name, prod_env)) return(FALSE)
        else stop("Unknown server environment")
}