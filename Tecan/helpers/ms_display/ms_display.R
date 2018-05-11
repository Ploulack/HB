source("helpers/delete_file_button_module.R")
source("helpers/ui_generics/select_file_ui.R")

ms_data_display_ui_sidebar <- function(id) {
     ns <- NS(id)

     tagList(

          checkboxGroupInput(inputId = ns("molecules"),
                             label = "Molecules",
                             choices = "Waiting for server..."),
          tags$hr(),
          checkboxInput(inputId = ns("select_all"),
                        label = "Select samples with a reading",
                        value = FALSE),
          uiOutput(ns("blanks_option")),
          actionButton(inputId = ns("unselect"),
                       label = "Unselect"),
          tags$hr(),
          checkboxGroupInput(inputId = ns("samples"),
                             label = "Samples",
                             choices = "Waiting for server..."
          )
     )
}

ms_data_display_ui_main <- function(id) {
     ns <- NS(id)
     tagList(
          fluidRow(
               column(4,
                      htmlOutput(outputId = ns("x_val"))
               ),
               column(2,
                      actionButton(ns("clear_selected_sample"),
                                   "Clear")
               ),
               column(3,
                      uiOutput(ns("tags_widget")))
          ),
          checkboxInput(ns("log_scale"),
                        label = "Switch to log scale"),
          plotOutput(ns("bar"),
                     click = ns("click")
                     # hover = hoverOpts(id = ns("hover"),
                     #                   delayType = "debounce", delay = 300)
          ),
          fluidRow(
               column(3,checkboxInput(inputId = ns("display_raw"),
                                      label = "Display unaggregated data")
               ),
               column(3, downloadLink(outputId = ns("save_csv"),
                                      label = "Download data as csv")
               )
          ),
          tableOutput(ns("table"))
     )
}

ms_data_display_server <- function(input, output, session,
                                   go_button,
                                   ms_tbl,
                                   type = c("ms", "search"),
                                   db_ms = NULL,
                                   db_tags = NULL,
                                   db_tags_view = NULL) {

     if (is.null(db_ms)) db_ms <- db_from_environment(session, "ms")
     if (is.null(db_tags)) db_tags <- db_from_environment(session, "tags")
     if (is.null(db_tags_view)) db_tags_view <- db_from_environment(session, "tags_view")
     ns <- session$ns
     stored_choices <- reactiveVal(NULL)
     display_tbl <- reactiveVal()
     last_click <- reactiveVal(NULL)
     ms_data <- reactiveVal(NULL)

     output$blanks_option <- renderUI({
          if (type == "ms") {
               checkboxInput(inputId = ns("show_blanks"),
                             label = "Display blanks",
                             value = FALSE)
          }
     })

     observeEvent(c(go_button(), input$show_blanks), {
          if (is.null(ms_tbl())) return()

          if (type == "search" || (input$show_blanks %||% FALSE)) {
               ms_data(ms_tbl())
          }
          else {
               ms_data(ms_tbl() %>% filter(type == "Analyte"))
          }
     })

     observeEvent(c(go_button(), input$show_blanks), {

          if (is.null(ms_data())) return()
          #Reset stored choices
          stored_choices(NULL)

          #Reset the select all button
          updateCheckboxInput(session, "select_all", value = FALSE)

          updateCheckboxGroupInput(session = session,
                                   inputId = "samples",
                                   choices = unique(ms_data()$Name),
                                   selected = unique(ms_data()$Name)[1]
          )

          molecules <- unique(ms_data()$Molecule)
          updateCheckboxGroupInput(session = session,
                                   inputId = "molecules",
                                   choices = molecules,
                                   selected = molecules
          )
     })

     observeEvent(input$select_all, {

          if (input$select_all) {
               stored_choices(input$samples)

               non_0_conc_choices <- ms_data() %>%
                    filter(Molecule %in% input$molecules,
                           Concentration > 0) %>%
                    pull(Name) %>%
                    unique()

               updateCheckboxGroupInput(session = session,
                                        inputId = "samples",
                                        selected = non_0_conc_choices)

          } else if (!is.null(stored_choices())) {
               updateCheckboxGroupInput(session = session,
                                        inputId = "samples",
                                        selected = stored_choices())
          }
     }, ignoreInit = TRUE)

     # remove any sample selection
     observeEvent(input$unselect, {
          stored_choices(NULL)
          last_click(NULL)

          updateCheckboxGroupInput(session = session,
                                   inputId = "samples",
                                   selected = "")
     })

     unaggregated_tbl <- reactive({
          if (any(is.null(c(input$samples, input$molecules)))) return()

          res_tbl <- ms_data() %>%
               select(c(Name, Molecule, Concentration)) %>%
               group_by(Name, Molecule) %>%
               arrange(Name) %>%
               filter(Name %in% input$samples) %>%
               filter(Molecule %in% input$molecules)

          return(res_tbl)
     })

     observeEvent(unaggregated_tbl(), {
          display_tbl(unaggregated_tbl() %>%
                           summarise(sd = sd(Concentration),
                                     Mean = mean(Concentration)) %>%
                           ungroup() %>%
                           mutate(cut_off = if (is.null(clicked_sample())) TRUE else display_tbl()$cut_off)
                           )
     }, priority = -1)

     observeEvent(input$click, {
          if (!is.null(input$click)) last_click(input$click)
     })

     #On file change reset the value selection from click
     observeEvent(c(go_button(), input$clear_selected_sample), {
          last_click(NULL)
     })

     clicked_sample <- eventReactive(last_click(), {

          if (is.null(last_click())) return(NULL)

          click_x <- last_click()$x
          n_molecules <- length(input$molecules)
          splits <- seq(1/(2 * n_molecules), 1 - 1/(2 * n_molecules), 1/n_molecules)

          sample_lvls <- display_tbl()$Name %>%
               as_factor() %>%
               levels()
          name <- sample_lvls[round(click_x)]

          molecule_lvls <- display_tbl()$Molecule %>%
               as_factor() %>%
               droplevels() %>%
               levels()

          x <- click_x - round(click_x) + 1/2

          molecule_name <- molecule_lvls[which.min(abs(splits - x))]

          value <- display_tbl() %>%
               filter(Molecule == molecule_name & Name == name) %>%
               pull(Mean)

          list(name = name,
               molecule = molecule_name,
               value = value)

     }, ignoreNULL = FALSE)

     observeEvent(clicked_sample(), {
          if (is.null(display_tbl())) return()

          if (is.null(clicked_sample()$value)) {
               display_tbl(
                    display_tbl() %>%
                         mutate(cut_off = TRUE)
               )
          } else {

               display_tbl(
                    display_tbl() %>%
                         mutate(cut_off = if_else(
                              Mean >= clicked_sample()$value,
                              TRUE,
                              FALSE,
                              missing = FALSE)
                         )
               )
          }
     }, ignoreNULL = FALSE, priority = 10)

     #### TAGS ####

     output$tags_widget <- renderUI({
          if (nrow(ms_data()) == 0 || is.null(clicked_sample())) return()


          selectizeInput(inputId = ns("tags"),
                         label = "Tags",
                         choices = db_tags_view$find('{}') %>%
                              flatten() %>%
                              flatten_chr(),
                         multiple = TRUE,
                         selected = ms_data() %>%
                              filter(Name == clicked_sample()$name) %>%
                              pull(Tags) %>%
                              unlist(),
                         options = list(create = 'true'
                                        ,
                                        allowEmptyOption = 'true'
                                        # createOnBlur = 'true'
                         )
          )
     })

     observeEvent(input$tags,{

          tags <- input$tags %||% "" %>%
               map_chr(~ str_interp('"${.}"'))

          tags <- str_interp('${str_c(tags, collapse = ", ")}')

          tags_add(db = db_tags, tags = tags)

          clicked_data <- ms_data() %>%
               filter(Name %in% clicked_sample()$name & Molecule %in% clicked_sample()$molecule)

          clicked_data_tags <- clicked_data %>%
               pull(Tags) %>%
               unlist() %>%
               sort()

          if (!(input$tags %||% "" %>% sort() == clicked_data_tags) %>% all()) {
               tags_json <- input$tags %||% "" %>%
                    jsonlite::toJSON()

               query <- str_interp('{"_id" : "${clicked_data$`_id`}",
                                   "data" :
                                        {"$elemMatch" :
                                             {"sampleid" : "${clicked_data$sampleid}",
                                             "Molecule" : "${clicked_data$Molecule}"}
                                        }
                                   }')
               update <- str_interp('{"$set" : {"data.$.Tags" : ${tags_json}}}')
               upd_log <- db_ms$update(query, update)


               if (ms_data() %>% nrow() > 0) {
                    tmp_ms_data <- ms_data()
                    tmp_ms_data$Tags[ms_data()$sampleid == clicked_data$sampleid] <- list(input$tags %||% "")
                    ms_data(tmp_ms_data)
               }
          }
     }, ignoreNULL = FALSE, ignoreInit = TRUE)

     #### GRAPHS ####

     barplot_scale <- reactive({
          ifelse(input$log_scale, "log1p", "identity")
     })

     output$bar <- renderPlot({
          if (is.null(display_tbl()) || nrow(display_tbl()) == 0) return()

          g <- ggplot(display_tbl()) +
               aes(x = Name, y = Mean, fill = Molecule) +
               geom_bar(position = "dodge",
                        stat = "identity",
                        aes(alpha = cut_off %>%
                                 factor(levels = c(FALSE, TRUE))
                        )
               ) +
               geom_errorbar(position = position_dodge(.9),
                             aes(ymax = Mean + sd,
                                 ymin = Mean - sd,
                                 width = .15)) +
               theme(axis.text.x = element_text(angle = 60,
                                                hjust = .8,
                                                size = 10,
                                                face = if_else(display_tbl()$cut_off,"bold", "plain"))) +
               scale_y_continuous(trans = barplot_scale()) +
               scale_fill_discrete(limits = levels(ms_data()$Molecule)) +
               scale_alpha_discrete(drop = FALSE, guide = "none")

          if (!is.null(clicked_sample()$value)) {
               g + geom_hline(yintercept = clicked_sample()$value)
          } else {
               g
          }
     })

     output$table <- renderTable({
          if (input$display_raw) {
               unaggregated_tbl()
          }  else display_tbl()
     })


     # Print the name of the x value
     output$x_val <- renderText({
          if (is.null(clicked_sample())) return()
          else {
               HTML("You've selected sample <code>", clicked_sample()$name, "</code>",
                    "<br>and molecule <code>", clicked_sample()$molecule,"</code>",
                    "<br>of value <code>", round(clicked_sample()$value,2), "</code>")
          }
     })

     output$save_csv <- downloadHandler(
          filename = function() {
               if (type == "ms")
                    paste0(ms$file_dribble()$name, ".csv")
               else
                    cat("about to force tz in ms_display", "\n")
                    paste0("search export ", Sys.time() %>% force_tz(tzone = "America/Montreal"), ".csv")
          },
          content = function(file) {
               write_csv(
                    ms_data() %>%
                         select(Name, Molecule, Concentration),
                    file,
                    na = "0")
          }
     )
}
