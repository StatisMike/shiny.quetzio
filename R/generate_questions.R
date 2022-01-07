#' Generate shinyInput code for question
#'
#' @param x List to get the params for UI generation
#' @param inputId Character string with inputId if not in the x
#' @param module_ui_id Character string getting the id for module UI
#'
#' @import shiny
#' @importFrom stats setNames
#' @keywords internal

.question_ui <- function(x, inputId, module_ui_id) {

  ns <- NS(module_ui_id)

  switch(x$type,
         textInput = {
           args <- list(
             inputId = bquote(ns(.(inputId))),
             label = if(isTRUE(x$mandatory)){bquote(.label_mandatory(.(x$label)))} else {x$label},
             placeholder = .null_def(x$placeholder, "Your answer"),
             width = .null_def(x$width, 500)
           )
           bquote(textInput(..(args)), splice = TRUE)
         },
         numericInput = {
           args <- list(
             inputId = bquote(ns(.(inputId))),
             label = if(isTRUE(x$mandatory)){bquote(.label_mandatory(.(x$label)))} else {x$label},
             value = .null_def(x$value, NA),
             placeholder = .null_def(x$placeholder, "Choose a number"),
             min = .null_def(x$min, NA),
             max = .null_def(x$max, NA),
             step = .null_def(x$step, NA),
             width = .null_def(x$width, 500)
           )
           bquote(numInput(..(args)), splice = TRUE)
         },
         selectizeInput = {
           args <- list(
             inputId = bquote(ns(.(inputId))),
             label = if(isTRUE(x$mandatory)){bquote(.label_mandatory(.(x$label)))} else {x$label},
             choices = x$choices,
             multiple = TRUE,
             width = .null_def(x$width, 500),
             options = list(
               maxItems = x$maxItems
             )
           )
           if ((!is.null(x$choiceNames)&&!is.na(x$choiceNames)) &&
               (!is.null(x$choiceValues)&&!is.na(x$choiceValues))) {
             args[["choices"]] <- stats::setNames(object = x$choiceValues,
                                           nm = x$choiceNames)
           } else if(!is.null(x$choices)&&!is.na(x$choices)){
             args[["choices"]] <- x$choices
           }
           if(!is.null(x$selected)&&!is.na(x$selected)){
             args[["selected"]] <- x$selected
           }
           bquote(selectizeInput(..(args)), splice = TRUE)
         },
         radioButtons = {
           args <- list(
             inputId = bquote(ns(.(inputId))),
             label = if(isTRUE(x$mandatory)){bquote(.label_mandatory(.(x$label)))} else {x$label},
             inline = .null_def(x$inline, FALSE),
             selected = .null_def(x$selected, character(0)),
             width = .null_def(x$width, 500)
           )
           if ((!is.null(x$choiceNames)&&!is.na(x$choiceNames)) &&
               (!is.null(x$choiceValues)&&!is.na(x$choiceValues))) {
             args[["choices"]] <- stats::setNames(object = x$choiceValues,
                                           nm = x$choiceNames)
           } else if(!is.null(x$choices)&&!is.na(x$choices)){
             args[["choices"]] <- x$choices
           }
           bquote(radioButtons(..(args)), splice = TRUE)
         }
         )
}

#' Generate all shinyInputs from yaml source file function
#'
#' @param source_list list object with inputs parameters
#' @param div_id Character string declaring id for the div
#' @param css Character string containing custom css rules for classes
#' 'mandatory_star' and 'invalid_input'
#' @param button_label character string with active label for the submission
#' button
#' @param module_ui_id character string declaring module id
#' @import shiny
#' @import htmltools
#' @keywords internal

.generate_ui <- function(
  source_list,
  div_id,
  css,
  button_label,
  module_ui_id) {

  ns <- NS(module_ui_id)

  inputs_names <- names(source_list)
  inputs <- list()
  inputs_n <- 0

  for (input in source_list) {

    inputs_n <- inputs_n + 1
    name <- inputs_names[inputs_n]
    inputs[[inputs_n]] <- .question_ui(input, name, module_ui_id)

  }

  return(
    eval(
      bquote(
        htmltools::div(
          shinyjs::useShinyjs(),
          shinyjs::inlineCSS(css),
          id = div_id,
          ..(inputs),
          actionButton(
            inputId = ns("submit"),
            label = button_label,
            class = "quetzio_submit"
          )
        ), splice = TRUE)
    )
  )

}

#' Populate missing values in your inputs with default provided data
#'
#' @param source_list list containing source data
#' @param default_config list containing default configuration
#'
#' @details The default_config object can have only one default configuration
#' per input type specified.
#' If there were some values specified in the vanilla configuration, they won't
#' be overwritten by configuration
#' @keywords internal

.populate_from_default <- function(
  source_list,
  default_config
) {

  option_names <- list(
    uni = c("mandatory", "width"),
    textInput = c("placeholder"),
    numericInput = c("placeholder", "value", "min", "max", "step"),
    radioButtons = c("choices", "choiceValues", "choiceNames", "selected", "inline"),
    selectizeInput = c("choices", "choiceValues", "choiceNames", "selected", "maxItems")
  )

  output_source <- source_list

  # check for all input types with default configuration
  for (input_type in names(default_config)) {

    # loop on all items in source_list
    for (item in names(source_list)) {

      # and check only types specified in config
      if (source_list[[item]][["type"]] == input_type) {

        # check for every option
        for (option in c(option_names[["uni"]], option_names[[input_type]])) {

          # check if the option is specified in default and not in output_source
          if (is.null(source_list[[item]][[option]]) && !is.null(default_config[[input_type]][[option]])) {

            # replace the option with one from default
            output_source[[item]][[option]] <- default_config[[input_type]][[option]]

          }
        }
        # safety measure - if the 'choice' option was provided before or has been provided
        # during population from default, the choiceValues and choiceNames are being deleted
        if (!is.null(output_source[[item]][["choices"]])) {

          output_source[[item]][["choiceValues"]] <- NULL
          output_source[[item]][["choiceNames"]] <- NULL

        }
      }
    }
  }

  return(output_source)
}
