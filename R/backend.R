#' Function that creates 'backend' for the generated survey
#' @param self R6 'self' object
#' @param private R6 'private' object
#'
#' @import shiny
#' @keywords internal

.survey_backend <- function(
  self,
  private
){

  moduleServer(
    id = self$module_id,
    function(input, output, session) {

      # get labels for buttons

      button_labels <- c(
        quetzio_txt(lang = private$language, private = private, x = "submit_enabled"),
        quetzio_txt(lang = private$language, private = private, x = "submit_disabled"),
        quetzio_txt(lang = private$language, private = private, x = "submit_done"),
        quetzio_txt(lang = private$language, private = private, x = "submit_error")
      )

      observeEvent(private$render_ui(), {

        output$quetzio_UI <- renderUI(
          if (private$render_ui())
          .generate_ui(source_list = self$source_list,
                       div_id = self$div_id,
                       css = private$css,
                       button_label = button_labels[1],
                       module_ui_id = self$module_ui_id)
        )

        outputOptions(output, "quetzio_UI", suspendWhenHidden = F)

        if (private$render_ui() && !is.null(self$description))
          .generate_description(self)

      })

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
                        input[[x]] <= self$source_list[[x]]$max) || is.null(input[[x]]) ||
                       is.na(input[[x]])

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

          if (all(isTRUE(valid$items_validity)) && isFALSE(self$is_done())) {
            self$message(NULL)
          } else if (all(!isTRUE(valid$items_validity)) && isFALSE(self$is_done())) {
            self$message("invalid_inputs")
          }

        }

        # update buttons if there are any non-valid inputs AND survey isn't done already!
        if (!all(valid$items_validity) && !isTRUE(self$is_done())) {

          updateActionButton(session, inputId = "submit",
                             label = button_labels[2])

        } else if (!isTRUE(self$is_done())){

          updateActionButton(session, inputId = "submit",
                             label = button_labels[1])

        }

      })

      # action to take when submit button is pressed
      observeEvent(input$submit, {

        if (!all(valid$items_validity)) {

          if (isTRUE(private$use_modal)) {

            # if something is not right, show the modalDialog!

            showModal(
              modalDialog(
                title = quetzio_txt(lang = private$language, private = private, x = "modal_title"),
                tags$p(quetzio_txt(lang = private$language, private = private, x = "modal_content"),
                       HTML(paste0("<ul>",
                                   paste(
                                     paste("<li>", valid$invalid_labels, "</li>"), collapse = ""),
                                   "</ul>")
                       )),
                footer = modalButton(quetzio_txt(lang = private$language, private = private, x = "modal_button"))
              )
            )
          }

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
                               label = button_labels[3],
                               icon = icon("thumbs-up"))

            lapply(seq_along(self$source_list), \(i) {
              #disable all inputs after questionnaire is done
              shinyjs::disable(id = paste(self$module_ui_id,
                                                names(self$source_list)[i],
                                                sep = ns.sep),
                               asis = TRUE)
              })


          },
          error = function(err){

            self$is_done(FALSE)
            self$message(err)

            updateActionButton(session,
                               inputId = "submit",
                               label = button_labels[4],
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
#' @keywords internal

.link_backend <- function(self, private, uneval){
  moduleServer(
    id = self$link_id,
    function(input, output, session) {

      # assign the provided 'quetzio_server' objects inside a reactiveValues
      self$quetzio_list <- eval(uneval)

      # create the UI holding the UIs of all linked questionnaires

      # output$quetzio_link_UI <- renderUI(
      #   tagList(
      #     lapply(seq_along(private$quetzio_names),
      #            function(i) quetzio_UI(session$ns(
      #              self$quetzio_list[[private$quetzio_names[i]]]$module_id)
      #            )
      #     ) ) )

      observe({

        output$quetzio_link_UI <- renderUI(
          tagList(
            lapply(seq_along(private$quetzio_names),
                   function(i) quetzio_UI(session$ns(
                     self$quetzio_list[[private$quetzio_names[i]]]$module_id)
                   )
            ) ) )

        outputOptions(output, "quetzio_link_UI", suspendWhenHidden = F)

        self$quetzio_list[[private$quetzio_names[1]]]$toggle_ui(TRUE)

        })

      # toggle the state of UIs - hide the UI of the completed questionnaire
      # and show the next one (minus the last, which will be retained)
      observe({
        for (i in 1:(length(private$quetzio_names) - 1)) {

          # check if the questionnaire is done
          req(self$quetzio_list[[private$quetzio_names[i]]]$is_done())

          # and toggle!
          self$quetzio_list[[private$quetzio_names[i]]]$toggle_ui(FALSE)
          self$quetzio_list[[private$quetzio_names[i+1]]]$toggle_ui(TRUE)

        }
      })

      # initialize the reactiveVals holding the objects
      self$completion <- reactiveVal()
      self$message <- reactiveVal()
      self$answers <- reactiveVal()


      observe({
        # assign the value at every change to the correspoding reactiveVal
        self$completion(sum(sapply(reactiveValuesToList(self$quetzio_list), \(x) x$is_done()))/length(private$quetzio_names))
        self$message(lapply(reactiveValuesToList(self$quetzio_list), \(x) x$message()))
        self$answers(lapply(reactiveValuesToList(self$quetzio_list), \(x) x$answers()))

        # save the answers into googlesheet if specified
        if(isTRUE(as.logical(private$output_gsheet)) && self$completion() == 1){

          .save_new_answers(
            .merge_linked_answers_to_df(
              answers_object = self$answers(),
              quetzio_names = private$quetzio_names
            ),
            private$output_gsheet_id, private$output_gsheet_sheetname)

        }
      })
    })
}

#' Server module handling label updates
#'
#' @param self the public element of 'quetzio_server' or 'quetzio_link_server'
#' @param tigger reactive triggering the update
#' @param source_method character string specifying in what form the source
#' config file will be provided. Can be either 'gsheet', 'yaml' or 'raw'.
#' Necessity of other arguments is dependent on this choice
#' @param source_yaml path to the source yaml file
#' @param source_gsheet_id id of the source googlesheet file
#' @param source_gsheet_sheetname name of the source spreadsheet
#' @param source_object object of class `list` (similiar in structure to
#' 'yaml' source) or `data.frame` (similiar in structure to 'googlesheet'
#' source) to be the source of questions. You can create a sample data.frame
#' with \code{create_survey_source()}. Needed when `source_method == 'raw'`
#'
#' @import shiny
#' @keywords internal

.quetzio_label_update <- function(
  self,
  private,
  trigger,
  source_method,
  source_yaml,
  source_gsheet_id,
  source_gsheet_sheetname,
  source_object
) {

  # initialize checks

  # check if all needed arguments are provided for source methods
  if (source_method == "gsheet") {
    #for gsheet source: if package is installed and if source ids are specified
    .check_package("googlesheets4")
    if (is.null(source_gsheet_id) || is.null(source_gsheet_sheetname)) {
      stop("When 'source_method' == 'gsheet', you need to specify 'source_gsheet_id' and 'source_gsheet_sheetname'.")
    }
    #for yaml source: if package is installed and if source file is provided
  } else if (source_method == "yaml") {
    .check_package("yaml")
    if (is.null(source_yaml)) {
      stop("When 'source_method' == 'yaml', you need to specify 'source_yaml'")
    }
    # for raw: if object is a dataframe or list
  } else if (source_method == "raw" && (is.null(source_object) && !class(source_object) %in% c("data.frame", "list"))) {
    stop("When 'source_method' == 'raw', you need to pass an object of class 'data.frame' or 'list' to 'source_object'")
    # if other source method is provided: error
  } else if (!source_method %in% c("gsheet", "yaml", "raw")) {
    stop("'source_method' must be chosen between 'gsheet', 'yaml' or raw.")
  }

  # loading data

  if (source_method == "yaml") {
    source <- .list_to_df(yaml::read_yaml(source_yaml))

  } else if (source_method == "gsheet") {

    source <- googlesheets4::read_sheet(
      ss = source_gsheet_id,
      sheet = source_gsheet_sheetname
    )
  } else if (source_method == "raw") {

    if (class(source_object) == "data.frame") {

      # checks if df is valid
      # .check_source_df(source_object)
      source <- source_object

    } else if (class(source_object) == "list") {

      # checks if list is valid
      # .check_source_list(source_object)
      source <- .list_to_df(source_object)

    } else {
      stop("Source object needs to be of class 'data.frame' or 'list'")
    }

  }

  moduleServer(
    id = self$module_ui_id,
    function(input, output, session) {

      # observe the change in the trigger reactive
      observe({

      # some initial checks - change if any of these trigger the label change #
        # make sure that the trigger value is not null
        req(!is.null(trigger()))
        # make sure that the trigger is reactive
        req(any(class(trigger) == "reactive"))
        # make sure that the UI is currently set to be rendered
        req(isTRUE(private$render_ui()))
        # make sure that the UI has been rendered completely
        req(!is.null(input$submit))

        for (row in 1:nrow(source)) {

          # deterime if the item is mandatory - the label needs to be updated
          # with 'mandatory_star' if that is the case
          is_mandatory <- isTRUE(self$source_list[[source[row, ]$id]]$mandatory)

          # all columns beside id are holding the labels to change with reactive
          # value
          if (trigger() %in% names(source)[names(source) != "id"]) {

            new_label <- as.character(source[row, trigger()])

            # update the label accordingly
            .update_label(self,
                          inputId = source[row, ]$id,
                          label = new_label,
                          is_mandatory = is_mandatory)


          } else {

            # if the trigger() value is not specified in config, return to the
            # default label
            default_label <- as.character(self$source_list[[source[row, ]$id]]$label)

            # update the label accordingly
            .update_label(self,
                          inputId = source[row, ]$id,
                          label = default_label,
                          is_mandatory = is_mandatory)

          }
        }
      })
    }
  )
}

#' Server module handling value updates
#'
#' @param self R6 self object
#' @param values named list containing values to update inputs with
#' @param values reactive object that triggers the change and contains
#' new values
#'
#' @import shiny
#' @keywords internal

.quetzio_value_update <- function(
  self,
  values
) {

  moduleServer(
    id = self$module_ui_id,

    function(input, output, session){

      observe({

        # make sure that 'values' are not null
        req(values)
        # and that they are in form of named list
        req(class(values) == "list" && !is.null(names(values)))

        # firstly, filter the values for only these, that have the same names
        # as any of the inputs in quetzio's source_list
        filtered_values <- values[names(values) %in% names(self$source_list)]

        lapply(seq_along(filtered_values), \(i) {

          if (!is.null(filtered_values[[i]]) && !is.na(filtered_values[[i]])) {

            # get the type of the shinyInput in the source list
            input_name <- names(filtered_values)[i]
            input_type <- self$source_list[[input_name]]$type

            # call update*Input function for the type of shinyInput
            switch(
              input_type,

              numericInput = updateNumericInput(session,
                                                inputId = input_name,
                                                value = filtered_values[[input_name]]),

              textInput = updateTextInput(session,
                                          inputId = input_name,
                                          value = filtered_values[[input_name]]),

              selectizeInput = updateSelectizeInput(session,
                                                    inputId = input_name,
                                                    selected = filtered_values[[input_name]]),

              radioButtons = updateRadioButtons(session,
                                                inputId = input_name,
                                                selected = filtered_values[[input_name]])
            )
          }
        })
      })
    })
}
