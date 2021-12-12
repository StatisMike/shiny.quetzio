#' Function that creates 'backend' for the generated survey
#' @param id The module id
#' @param source_list The list used to create the UI
#' @param mandatory_items Numeric vector with indices of items that are mandatory
#' @param numeric_items Numeric vector with indices of items that are numeric
#' @param output_gsheet Logical indicating if the survey results have to be automatically
#' saved into googlesheet
#' @param output_ss Character vector with output googlesheet id (if \code{output_gsheet = T})
#' @param output_sheet Character vector with output spreadsheet name (if \code{output_gsheet = T})
#' @param div_id Character vector with div id
#' @param css parsed css line to include for the survey
#' @param button_labels Character vector with labels for both active and inactive buttons
#' @param render_ui reactiveVal holding the logical indicating if the UI should be
#' rendered or not
#' @param module_ui_id Character vector with id for UI elements that will be generated
#'
#' @import shiny
#' @import shinyjs

.survey_backend <- function(
  id,
  source_list,
  mandatory_items,
  numeric_items,
  output_gsheet,
  output_ss,
  output_sheet,
  div_id,
  css,
  button_labels,
  render_ui,
  module_ui_id
){
  moduleServer(
    id = id,
    function(input, output, session) {

      output$quetzio_UI <- renderUI({

        req(render_ui())

        .generate_ui(source_list = source_list,
                     div_id = div_id,
                     css = css,
                     button_label = button_labels[1],
                     module_ui_id = module_ui_id)}
      )

      # reactiveValues for storing valid and mandatory inputs status
      # as valid$mandatory_filled and valid$minmax_matched
      # (their negations are invalid and mandatory not-filled questions)
      valid <- reactiveValues()
      status <- reactiveValues(is_done = FALSE,
                               message = NULL,
                               answers = NULL)

      # gather the form data into the right shape
      form_data <- reactive({
        data <- reactiveValuesToList(input)[names(source_list)]
        data <- c(data, timestamp = as.character(Sys.time()))
      })

      observe({

        req(!is.null(input$submit))
        valid$mandatory_ids <- names(source_list)[mandatory_items]
        valid$numeric_ids <- names(source_list)[numeric_items]

        # check if all fields are valid
        valid$items_validity <-
          vapply(names(source_list),

                 function(x) {

                   state <- list()
                   # if the item is mandatory, check if its not null
                   if(x %in% valid$mandatory_ids){
                     state[[1]] <- !is.null(input[[x]]) && input[[x]] != "" && !is.na(input[[x]])
                   }
                   # if the item is numeric, check if its in correct min-max range
                   if(x %in% valid$numeric_ids){
                     state[[2]] <- (input[[x]] >= source_list[[x]]$min &&
                        input[[x]] <= source_list[[x]]$max) || is.null(input[[x]])
                     # last condition for inputs which aren't mandatory (can be null),
                     # but need to be in correct min-max range!

                   }
                   !any(sapply(state, isFALSE))
                 },
                 logical(1))

        # get ids and labels of items
        valid$invalid_items <- names(source_list)[!valid$items_validity]
        valid$invalid_labels <- as.character(lapply(source_list[valid$invalid_items], function(x) {x$label} ))

        # give or remove 'invalid input' class after the 'submit button' was pressed
        if(input$submit > 0){
          for (input in 1:length(valid$items_validity)) {

            item_id <- names(valid$items_validity[input])

            if (isTRUE(valid$items_validity[input])) {

              shinyjs::removeCssClass(id = paste(paste(module_ui_id, item_id, sep = ns.sep), "label", sep = "-"),
                                      class = "invalid_input",
                                      asis = TRUE)

            } else {

              shinyjs::addCssClass(id = paste(paste(module_ui_id, item_id, sep = ns.sep), "label", sep = "-"),
                                     class = "invalid_input",
                                   asis = TRUE)

            }
          }
        }

        # update buttons if there are any non-valid inputs AND survey isn't done already!
        if (!all(valid$items_validity) && !isTRUE(status$is_done)) {

          updateActionButton(session, inputId = "submit",
                             label = button_labels[2])

        } else if (!isTRUE(status$is_done)){

          updateActionButton(session, inputId = "submit",
                             label = button_labels[1])

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

            if(isTRUE(as.logical(output_gsheet))){

            .save_new_answers(form_data(),
                              output_ss,
                              output_sheet)

            }

            status$is_done <- TRUE
            status$message <- NULL
            status$answers <- as.list(form_data())

            updateActionButton(session,
                               inputId = "submit",
                               label = "Submitted!",
                               icon = icon("thumbs-up"))
          },
          error = function(err){

            status$is_done <- FALSE
            status$message <- err

            updateActionButton(session,
                               inputId = "submit",
                               label = "Error occured",
                               icon = icon("frown-open"))

          }
          )
        }
      }
      )
      # finally, return the status of the object if you want your app to
      # listen to the status of the survey
      return(status)
    }
  )
}
