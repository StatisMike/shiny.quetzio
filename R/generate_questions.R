#' Generate shinyInput code for question
#'
#' @param x List to get the params for UI generation
#' @param inputId Character string with inputId if not in the x
#'
#' @import shiny
#' @importFrom stats setNames

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
             value = x$value,
             min = .null_def(x$min, NA),
             max = .null_def(x$max, NA),
             step = .null_def(x$step, NA),
             width = .null_def(x$width, 500)
           )
           bquote(numericInput(..(args)), splice = TRUE)
         },
         selectizeInput = {
           args <- list(
             inputId = bquote(ns(.(inputId))),
             label = if(isTRUE(x$mandatory)){bquote(.label_mandatory(.(x$label)))} else {x$label},
             choices = x$choices,
             multiple = .null_def(x$multiple, FALSE),
             width = .null_def(x$width, 500)
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
#' @param module_ui_id character string declaring module id
#' @param css Character string containing custom css rules for classes
#' 'mandatory_star' and 'invalid_input'
#' @import shiny
#' @import htmltools

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
            class = "btn-primary",
            width = "350px"
          )
        ), splice = TRUE)
    )
  )

}

