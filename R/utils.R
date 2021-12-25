#' Helper function to check namespace
#'
#' @param package Character string declaring which package to check for
#' @keywords internal

.check_package <- function(package) {

    if(length(find.package(package, quiet = T)) == 0){

      stop(paste0("To use this method please install '",package,"' package: install.packages('",package,"')"),
           call. = F)
    }
}


#' function used to add red asterisk to mandatory fields
#'
#' @param label Character string with item label
#' @import shiny
#' @keywords internal
#'
.label_mandatory <- function(label){
  tagList(
    label,
    span("*", class = "mandatory_star")
  )
}


#' function to handle css for quetzio UI css classes
#'
#' @param css list containing two character objects named with names of css classes. By default it
#' will provide these styles:
#' \itemize{
#' \item{invalid_input = "color: red; font-style: italic;"}
#' \item{mandatory_star = "color: red;"}
#' \item{quetzio_submit = "color: #fff; background-color: #337ab7; border-color: #2e6da4; width: 200px;"}
#' \item{quetzio_description = "font-size: 0.7em;"}
#' }
#' If any of these aren't provided in the list passed to `css`, the default values would be added.
#' @param div_id character string indicating the div id to use this stylesheet
#' @keywords internal

.custom_css_handler <- function(css = NULL,
                                div_id) {


  defaults_css <- list(
    invalid_input = "color: red; font-style: italic;",
    mandatory_star = "color: red;",
    quetzio_submit = "color: #fff; background-color: #337ab7; border-color: #2e6da4; width: 200px;",
    quetzio_description = "font-size: 0.9em;"
  )

  if (!is.null(css) && class(css) != "list") {
    stop("Object provided to 'css' argument should be a named 'list' object (with names indicating css classes)",
         call. = F)
  }
  # if any default isn't in css provided, it should take the default value

  if (!is.null(css)) {
    css_def_not_present <- defaults_css[!names(defaults_css) %in% names(css)]
    css <- c(css_def_not_present, css)
  } else {
    css <- defaults_css
  }

  paste(
    sapply(seq_along(css), \(i) {
      paste0("#", div_id, " .", names(css)[i], " { ", css[[i]], " }")
    }), collapse = " ")

}

#' function for default values if null or NA
#'
#' @param x the value to check
#' @param default the default value if null or NA
#' @keywords internal

.null_def <- function(x, default){
  ifelse(is.null(x) || is.na(x), default, x)
}

#' function to get indices of mandatory fields
#'
#' @param source_list Source list of inputs
#' @keywords internal

.get_mandatory <- function(source_list){

  fields_mandatory <- c()
  input_n <- 0
  for(input in source_list){
    input_n <- input_n + 1
    if(isTRUE(input$mandatory)){
      fields_mandatory <- c(fields_mandatory, input_n)
    }
  }

  return(fields_mandatory)
}

#' function to get indices of numeric fields
#'
#' @param source_list Source list of inputs
#' @keywords internal

.get_type <- function(source_list, type){

  fields_of_type <- c()
  input_n <- 0
  for(input in source_list){
    input_n <- input_n + 1
    if(input$type == type){
      fields_of_type <- c(fields_of_type, input_n)
    }
  }

  return(fields_of_type)
}

#' Save the answer user input to Google Sheet
#' @param user_answers the object with inputs to extract
#' @param output_ss character vector with output googlesheet ID
#' @param output_sheet character vector with output spreadsheet name
#'
#' @import dplyr
#' @keywords internal

.save_new_answers <- function(
  user_answers,
  output_ss,
  output_sheet
  ) {

  # sanitize answers
  user_answers <- .sanitize_answers(
    answers = user_answers
  )

  to_upload_answers <- tryCatch({

    old_answers <- .read_all_answers(
      output_ss = output_ss,
      output_sheet = output_sheet)

    ## I'm using `merge()` here and not `full_join()` because
    ## I want to dynamically coerce variable types. Otherwise,
    ## `full_join()`'s strict type safety raises an error
    to_upload_answers <- merge(old_answers, user_answers, all = TRUE, sort = FALSE)
    dplyr::relocate(.data = to_upload_answers,
                                         dplyr::ends_with("timestamp"),
                                         .after = dplyr::last_col())

  }, error = function(cond){

    dplyr::relocate(.data = user_answers,
                    dplyr::ends_with("timestamp"), .after = dplyr::last_col())

  })

  googlesheets4::sheet_write(
    ss = output_ss,
    data = to_upload_answers,
    sheet = output_sheet
  )
}

#' function to modify the chosen call argument before evaluation
#' @param call the function call to modify
#' @param arg_name argument name
#' @param arg_value argument value to set
#' @keywords internal

.modify_arg <- function(call, arg_name, arg_value) {

  # get the call - fed to the function as unevaluated one
  call <- as.list(call)

  # then modify the 'arg_name' argument with 'arg_value' value
  call[[1]][[as.character(arg_name)]] <- arg_value

  # and return as call
  return(as.call(call))

}

#' function to modify the 'quetzio_server' arguments
#' @param ... dotdotdot passed from parent function
#' @param link_id character string indicating what the value of the argument should be
#' @keywords internal

.modify_quetzio_arg <- function(..., link_id) {

  #firstly, it is needed to catch the call to ...
  raw_call <- substitute(list(...))

  # then modify all elements other than 1st
  for (i in 2:length(raw_call)) {

    # all will have added link_id argument
    raw_call[i] <- .modify_arg(raw_call[i], "link_id", link_id)

    # if (i == 2) {
    #   # only first value will have the render ui set to TRUE
    #   raw_call[i] <- .modify_arg(raw_call[i], "render_ui", TRUE)
    # } else {
    #   # every next will have it set to FALSE
      raw_call[i] <- .modify_arg(raw_call[i], "render_ui", FALSE)
    # }
  }

  # finally, we need to substitute 'list()' with 'reactiveValues()' in the call
  raw_call[1] <- substitute(reactiveValues())

  return(substitute(raw_call))

}

#' Merge answers from linked questionnaires into data.frame
#'
#' @param answers_object the 'self$answers()' object from 'quetzio_link_server'
#' @param quetzio_names the names of the surveys in correct order from the
#' 'self$quetzio_names' object from 'quetzio_link_server'
#' @keywords internal

.merge_linked_answers_to_df <- function(answers_object, quetzio_names) {

  # create empty data.frame with one row
  answers_df <- data.frame(. = "")

  # loop over the data.frame merging answers of all questionnaires
  for(name in quetzio_names) {
    answers_df <- cbind(answers_df,
                        .sanitize_answers(answers = answers_object[[name]],
                                          name = name))
  }
  return(answers_df[, -1])
}

#' sanitize answers from questionnaire
#'
#' @param answers list of answers from one questionnaire
#' @param name name of the questionnaire
#' @keywords internal

.sanitize_answers <- function(answers, name = NULL) {

  # get the answers
  user_answers <- answers[!sapply(answers, is.null)]
  ## for multiple answers possible, separate them with semicolon
  user_answers <- lapply(user_answers, function(x) {if(length(x)>1) {paste(x, collapse = ";")} else {x} })
  user_answers <- as.data.frame(user_answers)

  # modify the names to make the answers from different questionnaires distinct
  if (!is.null(name)) {
    names(user_answers) <- paste(name, names(user_answers), sep = "_")
  }

  return(user_answers)

}

#' Create correct data.frame from list
#'
#' @param list source to be converted into data.frame
#' @keywords internal

.list_to_df <- function(list) {

  output <- data.frame(id = names(list))
  output$type <- sapply(list, \(x) x$type)

  option_names <- unique(as.character(sapply(list, \(x) names(x))))
  option_names <- option_names[option_names != "type"]

  for (option in option_names) {

    output[, option] <- sapply(list, \(x) if (length(x[[option]]) > 1) paste(x[[option]], collapse = ";") else x[[option]])

  }

  return(output)

}

#' Update label of the shinyInput
#'
#' @param self the public component of the 'quetzio_server' object
#' @param inputId id of the input to be updated
#' @param label character string with updated label value
#' @param is_mandatory logical indicating if the item is mandatory
#'
#' @details
#' New function is used to handle input label update for two reasons. Firstly,
#' specific `update*Input` functions need to be called for specific input types,
#' which would create a need for more verbose code with multiple strategies.
#' Secondly - and most important - `update*Input` functions don't allow
#' supplementing labels with 'HTML' tags, as everything passed through them as
#' label is coerced to character. `shinyjs::html()` remedies this problem.
#' @keywords internal

.update_label <- function (self, inputId, label,
                           is_mandatory)
{

  if (is_mandatory) {
    html_label <- paste(label, '<span class="mandatory_star">*</span>')
  } else {
    html_label <- label
  }

  shinyjs::html(
    id = paste(self$module_ui_id, inputId, "label", sep = ns.sep),
    html = html_label,
    asis = T
  )

}

#' Generate source list from yaml
#'
#' @param yaml_file path to the source yaml file
#' @keywords internal


.yaml_to_list <- function(yaml_file){
  yaml::read_yaml(file = yaml_file)
}

#' Read the Answer data from Google Sheets
#' @param output_ss character vector with output googlesheet ID
#' @param output_sheet character vector with output spreadsheet name
#' @keywords internal


.read_all_answers <- function(
  output_ss,
  output_sheet
) {
  googlesheets4::read_sheet(
    ss = output_ss,
    sheet = output_sheet
  )
}
