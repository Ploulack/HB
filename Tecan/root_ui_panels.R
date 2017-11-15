tecan_tab <- function() {
        tabPanel("Tecan",
                 conditionalPanel(condition = "output.token_exists == true",
                                  conditionalPanel(
                                          condition = "input.tecan_go == 0",
                                          actionButton("tecan_go", "Launch Tecan tool")
                                  ),
                                  conditionalPanel(
                                          condition = "input.tecan_go > 0",
                                          tecan_ui("Tecan")
                                  )
                 )
        )
}

order_tab <- function() {
        tabPanel("Order Decision Tool",
                 conditionalPanel(condition = "output.token_exists == true",
                                  conditionalPanel(
                                          condition = "input.decision_go == 0",
                                          actionButton("decision_go", "Launch Order Decision Tool")
                                  ),
                                  conditionalPanel(
                                          condition = "input.decision_go > 0",
                                          decisionUI("decision")
                                  )
                 )
        )
}

gel_tab <- function() {
        tabPanel("Gel",
                 conditionalPanel(condition = "output.token_exists == true",
                                  conditionalPanel(
                                          condition = "input.gel_go == 0",
                                          actionButton("gel_go", "Start Gel viewer")
                                  ),
                                  conditionalPanel(
                                          condition = "input.gel_go > 0",
                                          gel_ui("Gel")
                                  )
                 )
        )
}

hamilton_tab <- function() {
        tabPanel("Hamilton",
                 conditionalPanel(condition = "output.token_exists == true",
                                  conditionalPanel(
                                          condition = "input.hami_go == 0",
                                          actionButton("hami_go", "Start Hamilton editor")
                                  ),
                                  conditionalPanel(
                                          condition = "input.hami_go > 0",
                                          hami_ui("Hami")
                                  )
                 )
        )
}