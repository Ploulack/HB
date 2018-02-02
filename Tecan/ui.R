library(shiny); library(rlang)

source("order_dna_decision/decision_ui.R")
source("ms/ms_ui.R")
source("tecan/tecan_ui.R")
source("gel/gel_ui.R")
source("auth/google_button_ui.R")
source("hamilton/hamilton.R")

display_tab <- function(panel_name, btn_label, module_ui_name) {
        btn_id <- paste0(module_ui_name, "_go")
        condition_string <- paste0("input.", btn_id)
        module_function <- rlang::as_function(paste0(module_ui_name, "_ui"))

        tabPanel(panel_name,
                 conditionalPanel(condition = "output.token_exists == true",
                                  conditionalPanel(
                                          condition = paste0(condition_string," == 0"),
                                          actionButton(btn_id, btn_label)
                                  ),
                                  conditionalPanel(
                                          condition =  paste0(condition_string," > 0"),
                                          module_function(module_ui_name)
                                  )
                 )
        )
}
tabsetPanel(id = "tab",
            conditionalPanel(
                    condition = "output.token_exists == false",
                    google_auth_button_ui("Auth")),

            display_tab(panel_name = "Tecan",
                        btn_label = "Launch Tecan tool",
                        module_ui_name = "tecan"),

            display_tab(panel_name = "Order DNA",
                        btn_label = "Launch Order DNA tool",
                        module_ui_name = "decision"),

            display_tab(panel_name = "Gel",
                        btn_label = "Start Gel viewer",
                        module_ui_name = "gel"),

            display_tab(panel_name = "MS",
                        btn_label = "Open MS viewer",
                        module_ui_name = "ms"),

            display_tab(panel_name = "Hamilton",
                        btn_label = "Start Hamilton editor",
                        module_ui_name = "hami")
)
