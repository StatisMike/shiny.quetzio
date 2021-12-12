#' Function that creates 'backend' for the generated survey
#' @param self R6 'self' object
#' @param private R6 'private' object
#'
#' @import shiny
#' @import shinyjs

.survey_backend <- function(
  self,
  private
){
  moduleServer(
    id = self$module_id,
    function(input, output, session) {

      output$quetzio_UI <- renderUI({

        req(private$render_ui())

        .generate_ui(source_list = self$source_list,
                     div_id = self$div_id,
                     css = private$css,
                     button_label = self$button_labels[1],
                     module_ui_id = self$module_ui_id)}
      )

      # reactiveValues for storing valid and mandatory inputs status
      # as valid$mandatory_filled and valid$minmax_matched
      # (their negations are invalid and mandatory not-filled questions)
      valid <- reactiveValues()

      # gather the form data into the right shape
      form_data <- reactive({
        data <- reactiveValuesToList(input)[names(self$source_list)]
        data <- c(data, timestamp = as.character(Sys.time()))
      })

      observe({

        req(!is.null(input$submit))
        valid$mandatory_ids <- names(self$source_list)[private$mandatory_items]
        valid$numeric_ids <- names(self$source_list)[private$numeric_items]

        # check if all fields are valid
        valid$items_validity <-
          vapply(names(self$source_list),

                 function(x) {

                   state <- list()
                   # if the item is mandatory, check if its not null
                   if(x %in% valid$mandatory_ids){
                     state[[1]] <- !is.null(input[[x]]) && input[[x]] != "" && !is.na(input[[x]])
                   }
                   # if the item is numeric, check if its in correct min-max range
                   if(x %in% valid$numeric_ids){
                     state[[2]] <- (input[[x]] >= self$source_list[[x]]$min &&
                        input[[x]] <= self$source_list[[x]]$max) || is.null(input[[x]])
                     # last condition for inputs which aren't mandatory (can be null),
                     # but need to be in correct min-max range!

                   }
                   !any(sapply(state, isFALSE))
                 },
                 logical(1))

        # get ids and labels of items
        valid$invalid_items <- names(self$source_list)[!valid$items_validity]
        valid$invalid_labels <- as.character(lapply(self$source_list[valid$invalid_items], function(x) {x$label} ))

        # give or remove 'invalid input' class after the 'submit button' was pressed
        if(input$submit > 0){
          for (input in 1:length(valid$items_validity)) {

            item_id <- names(valid$items_validity[input])

            if (isTRUE(valid$items_validity[input])) {

              shinyjs::removeCssClass(id = paste(paste(self$module_ui_id, item_id, sep = ns.sep), "label", sep = "-"),
                                      class = "invalid_input",
                                      asis = TRUE)

            } else {

              shinyjs::addCssClass(id = paste(paste(self$module_ui_id, item_id, sep = ns.sep), "label", sep = "-"),
                                     class = "invalid_input",
                                   asis = TRUE)

            }
          }
        }

        # update buttons if there are any non-valid inputs AND survey isn't done already!
        if (!all(valid$items_validity) && !isTRUE(self$is_done())) {

          updateActionButton(session, inputId = "submit",
                             label = self$button_labels[2])

        } else if (!isTRUE(self$is_done())){

          updateActionButton(session, inputId = "submit",
                             label = self$button_labels[1])

        }

      })

      # action to take when submit button is pressed
      observeEvent(input$submit, {

        if (!all(valid$items_validity)) {

          # if something is not right, show the modalDialog!

          showModal(
            modalDialog(
              title = "Error!",
                tags$p("Some mandatory inputs aren't filled and/or numeric inputs aren't withing correct range:",
                       HTML(paste0("<ul>",
                              paste(
                                paste("<li>", valid$invalid_labels, "</li>"), collapse = ""),
                              "</ul>")
                       ))
            )
          )

        } else {

          # but if everything works, go on with it

          shinyjs::disable("submit")

          # checks on the save
          tryCatch({

            if(isTRUE(as.logical(private$output_gsheet))){

            .save_new_answers(form_data(),
                              private$output_ss,
                              private$output_sheet)

            }

            self$is_done(TRUE)
            self$message(NULL)
            self$answers(as.list(form_data()))

            updateActionButton(session,
                               inputId = "submit",
                               label = self$button_labels[3],
                               icon = icon("thumbs-up"))
          },
          error = function(err){

            self$is_done(FALSE)
            self$message(err)

            updateActionButton(session,
                               inputId = "submit",
                               label = self$button_labels[4],
                               icon = icon("frown-open"))

          }
          )
        }
      }
      )
    }
  )
}

#' Function that creates backend for the linked surveys
#'
#' @param self the 'self' component of R6 object
#' @param private the 'private' component of R6 object
#' @param uneval the unevaluated expression to create reactiveValues with
#' list of questionnaires
#'
#' @import shiny

.link_backend <- function(self, private, uneval){
  moduleServer(
    id = self$link_id,
    function(input, output, session) {

      # assign the provided 'quetzio_server' objects inside a reactiveValues
      private$quetzio_list <- eval(uneval)

      # toggle the state of UIs - hide the UI of the completed questionnaire
      # and show the next one (minus the last, which will be retained)
      observe({
        for (i in 1:(length(private$quetzio_names) - 1)) {

          # check if the questionnaire is done
          req(private$quetzio_list[[private$quetzio_names[i]]]$is_done())

          # and toggle!
          private$quetzio_list[[private$quetzio_names[i]]]$toggle_ui(FALSE)
          private$quetzio_list[[private$quetzio_names[i+1]]]$toggle_ui(TRUE)

        }
      })

      # create the UI holding the UIs of all linked questionnaires

      output$quetzio_link_UI <- renderUI(
        tagList(
          lapply(seq_along(private$quetzio_names),
                 function(i) quetzio_UI(session$ns(
                   private$quetzio_list[[private$quetzio_names[i]]]$module_id)
                 )
          ) ) )

      # initialize the reactiveVals holding the objects
      self$completion <- reactiveVal()
      self$message <- reactiveVal()
      self$answers <- reactiveVal()


      # assign the value at every change to the correspoding reactiveVal
      observe({

        self$completion(sum(sapply(reactiveValuesToList(private$quetzio_list), \(x) x$is_done()))/length(private$quetzio_names))
        self$message(lapply(reactiveValuesToList(private$quetzio_list), \(x) x$message()))
        self$answers(lapply(reactiveValuesToList(private$quetzio_list), \(x) x$answers()))
      })
    })
}
