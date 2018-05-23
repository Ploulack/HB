library(purrrlyr)
source("helpers/delete_file_button_module.R")
source("helpers/ui_generics/select_file_ui.R")

#### UI #####

ms_data_display_ui_sidebar <- function(id) {
     ns <- NS(id)

     tagList(

          checkboxGroupInput(inputId = ns("molecules"),
                             label = "Molecules",
                             choices = "Waiting for server..."),
          tags$hr(),
          actionButton(inputId = ns("select_all"),
                       label = "Select samples with a reading"),
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
          uiOutput(ns("selected_samples_tags")),
          fluidRow(
               column(4,
                      htmlOutput(outputId = ns("x_val"))
               ),
               column(2,
                      actionButton(ns("clear_selected_sample"), "Clear")
               ),
               column(4,
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
                                      label = "Unaggregated data (Summarise groups by their mean)",
                                      value = TRUE)
               ),
               column(3, downloadLink(outputId = ns("save_csv"),
                                      label = "Download data as csv")
               )
          ),
          tableOutput(ns("table"))
     )
}

#### INITIALIZE ####
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
     display_tbl <- reactiveVal()
     last_click <- reactiveVal(NULL)
     ms_data <- reactiveVal(NULL)
     sample_choices <- reactiveVal(NULL)

     output$blanks_option <- renderUI({
          if (type == "ms") {
               checkboxInput(inputId = ns("show_blanks"),
                             label = "Display blanks",
                             value = FALSE)
          }
     })

     observeEvent(c(go_button(), input$show_blanks), {
          if (is.null(ms_tbl()) || ms_tbl() %>% nrow() == 0) return()
          browser()

          if (type == "search" || (input$show_blanks %||% FALSE)) {
               ms_data(ms_tbl())
          }
          else {
               ms_data(ms_tbl() %>% filter(type == "Analyte"))
          }
          if (ms_data() %>% nrow() > 0) {
               #For entries prior to Tag feature, we replace the character(0) with empty string
               ms_data(
                    ms_data() %>%
                         mutate(
                              Tags = map(Tags, ~{if (is_empty(.)) "" else .}),
                              sample_id = group_indices(., `_id`, Name) %>% as.character()
                         )
               )

               sample_choices(
                    ms_data() %>%
                         distinct(Name, `_id`, xml, .keep_all = TRUE) %>%
                         {
                              dat <- .
                              pull(dat, sample_id) %>% set_names(dat$Name)
                         }
               )
          }
     })

     observeEvent(c(go_button(), input$show_blanks), {

          if (is.null(ms_data())) return()

          updateCheckboxGroupInput(session = session,
                                   inputId = "samples",
                                   choices = sample_choices(),
                                   selected = sample_choices()[1]
          )

          molecules <- unique(ms_data()$Molecule)
          updateCheckboxGroupInput(session = session,
                                   inputId = "molecules",
                                   choices = molecules,
                                   selected = molecules
          )
     })

     observeEvent(input$select_all, {

          positive_sample_id <- ms_data() %>%
               filter(Molecule %in% input$molecules,
                      Concentration > 0) %>%
               pull(sample_id) %>%
               unique()



          updateCheckboxGroupInput(session = session,
                                   inputId = "samples",
                                   selected = sample_choices() %>%
                                        keep(~ . %in% positive_sample_id))
     }, ignoreInit = TRUE)

     # remove any sample selection
     observeEvent(input$unselect, {
          last_click(NULL)

          updateCheckboxGroupInput(session = session,
                                   inputId = "samples",
                                   selected = "")
     })

     unaggregated_tbl <- reactive({
          if (any(is.null(c(input$samples, input$molecules)))) return()
browser()
          res_tbl <- ms_data() %>%
               select(c(Name, Molecule, Concentration, Tags, sample_id, xml)) %>%
               mutate(Tags = Tags %>%
                           map_chr(~ str_c(., collapse = ", ")) %>%
                           str_remove("^(,\\s*)")
               ) %>%
               group_by(Name, sample_id, Molecule) %>%
               arrange(Name) %>%
               filter(sample_id %in% input$samples) %>%
               filter(Molecule %in% input$molecules)

          return(res_tbl)
     })

     observeEvent(unaggregated_tbl(), {
          display_tbl(unaggregated_tbl() %>%
                           summarise(sd = sd(Concentration),
                                     Mean = mean(Concentration),
                                     xml = unique(xml)) %>%
                           ungroup() %>%
                           mutate(cut_off = if (is.null(clicked_sample())) TRUE else display_tbl()$cut_off) %>%
                           group_by(Name) %>%
                           add_count() %>%
                           mutate(rank = row_number()) %>%
                           ungroup()
          )
     }, priority = -1)

     #### CLICKED SAMPLE ####
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

          sample_lvls <- display_tbl()$sample_id %>%
               as_factor() %>%
               levels()

          clicked_sample_id <- sample_lvls[round(click_x)]

          molecule_lvls <- display_tbl()$Molecule %>%
               as_factor() %>%
               droplevels() %>%
               levels()

          x <- click_x - round(click_x) + 1/2

          clicked_molecule <- molecule_lvls[which.min(abs(splits - x))]

          display_tbl() %>%
               filter(Molecule == clicked_molecule & sample_id == clicked_sample_id) %>%
               rename(value = Mean)

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
                              filter(sample_id == clicked_sample()$sample_id) %>%
                              pull(Tags) %>%
                              unlist(),
                         options = list(create = 'true',
                                        allowEmptyOption = 'true'
                         )
          )
     })

     observeEvent(input$tags,{

          tags <- input$tags %||% "" %>%
               map_chr(~ str_interp('"${.}"'))

          tags <- str_interp('${str_c(tags, collapse = ", ")}')

          tags_add(db = db_tags, tags = tags)

          clicked_data <- ms_data() %>%
               mutate(row_nb = row_number()) %>%
               filter(Name %in% clicked_sample()$Name & Molecule %in% clicked_sample()$Molecule)

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


               if (ms_data() %>% nrow() > 0 && upd_log$modified == 1) {
                    browser()
                    tmp_ms_data <- ms_data()
                    tmp_ms_data$Tags[clicked_data$row_nb] <- list(input$tags %||% "")
                    ms_data(tmp_ms_data)
               }
          }
     }, ignoreNULL = FALSE, ignoreInit = TRUE)

     #### MULTIPLE SAMPLES TAGGING ####

     common_tags <- reactive({
          unaggregated_tbl() %>%
               pull(Tags) %>%
               str_split(", ") %>%
               reduce(~keep(.x, .x %in% .y))
     })

     output$selected_samples_tags <- renderUI({
          if (unaggregated_tbl() %>% is.null() ||
              nrow(unaggregated_tbl()) == 0 ||
              is.null(input$samples)) return()

          selectizeInput(inputId = ns("selected_samples_tags"),
                         label = "Common tags acrosse selected samples",
                         choices = db_tags_view$find('{}') %>%
                              flatten() %>%
                              flatten_chr(),
                         multiple = TRUE,
                         selected = common_tags(),
                         options = list(create = 'true',
                                        allowEmptyOption = 'true'
                         )
          )
     })

     observeEvent(input$selected_samples_tags, {
          input_tags <- input$selected_samples_tags

          tags_to_add <- input_tags[!(input_tags %in% common_tags())]
          tags_to_remove <- common_tags()[!(common_tags() %in% input_tags)]

          if ( !c(tags_to_add, tags_to_remove) %>% is_empty() ) {

               progress <- shiny::Progress$new()
               temp_data <- ms_data() %>%
                    mutate(row_nb = row_number()) %>%
                    filter( (Molecule %in% input$molecules) & (sample_id %in% input$samples) )
               inc <- 1/nrow(temp_data)

               added <- NULL
               browser()
               temp_data <- temp_data %>%
                    by_row(~{
                         sample <- .
                         query <- str_interp('{"_id" : "${sample$`_id`}",
                                             "data":{
                                                  "$elemMatch": {
                                                       "Molecule": "${sample$Molecule}",
                                                       "sampleid": "${sample$sampleid}"}
                                             }
                                   }')

                         if (!tags_to_add %>% is_empty()) {
                              if (tags_to_add == "") return()
                              added <- TRUE
                              tags_to_add_json <- tags_to_add %>% jsonlite::toJSON()
                              progress$inc(inc, str_interp("Adding ${tags_to_add %>% str_c(collapse = ', ')} to ${sample$Name}."))

                              update <- str_interp('{"$addToSet" : {
                                             "data.$.Tags" : {
                                                  "$each": ${tags_to_add_json}
                                                  }
                                             }
                                              }')

                              #If db was changed, update the current table (obtained from the 'display samples' button)
                         } else if (!tags_to_remove %>% is_empty()) {
                              if (tags_to_remove == "") return()
                              added <- FALSE
                              tags_to_remove_json <- tags_to_remove %>% jsonlite::toJSON()
                              progress$inc(inc, str_interp("Removing ${tags_to_remove %>% str_c(collapse = ', ')} tags in ${sample$Name}."))

                              update <- str_interp('{"$pullAll" : {
                                                  "data.$.Tags" : ${tags_to_remove_json}
                                                  }
                                             }')
                         }

                         log <- db_ms$update(query, update)

                         if (log$modifiedCount == 1) {
browser()
                              x <- ms_data() %>%
                                   mutate(row_nb = row_number())
                              x[sample$row_nb, ]$Tags <- sample$Tags[[1]] %>%
                              {
                                   if (added) append(.,tags_to_add)
                                   else keep(., ~!. %in% tags_to_remove)
                              } %>% list()

                              ms_data(x)
                         }

                         ifelse(log$modifiedCount == 1, TRUE, FALSE)
                    }, .collate = "list", .to = "modified")
               progress$close()

          }
     }, priority = -1, ignoreNULL = FALSE, ignoreInit = TRUE)

     #### GRAPHS ####

     barplot_scale <- reactive({
          ifelse(input$log_scale, "log1p", "identity")
     })

     output$bar <- renderPlot({
          if (is.null(display_tbl()) || nrow(display_tbl()) == 0) return()

          g <- ggplot(display_tbl()) +
               aes(x = if_else(n == 1, Name, paste0(Name, " - ", rank)),
                   y = Mean, fill = Molecule) +
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
               labs(x = "Sample names", y = "Concentration") +
               theme(axis.text.x = element_text(angle = 60,
                                                hjust = .8,
                                                size = 10,
                                                face = if_else(display_tbl()$cut_off,"bold", "plain")),
                     legend.position = c(.9, .85)) +
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
               HTML("You've selected sample <code>", clicked_sample()$Name, "</code>",
                    "<br>and molecule <code>", clicked_sample()$Molecule,"</code>",
                    "<br>of value <code>", round(clicked_sample()$value,2), "</code>",
                    "<br>of file <code>", clicked_sample()$xml, "</code>")
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
