#' Helper function to check namespace
#'
#' @param package Character string declaring which package to check for
#'

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
#'
.label_mandatory <- function(label){
  tagList(
    label,
    span("*", class = "mandatory_star")
  )
}

#' function for default values if null or NA
#'
#' @param x the value to check
#' @param default the default value if null or NA

.null_def <- function(x, default){
  ifelse(is.null(x) || is.na(x), default, x)
}

#' function to get indices of mandatory fields
#'
#' @param source_list Source list of inputs
#'

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
#'

.get_numeric <- function(source_list){

  fields_numeric <- c()
  input_n <- 0
  for(input in source_list){
    input_n <- input_n + 1
    if(input$type == "numericInput"){
      fields_numeric <- c(fields_numeric, input_n)
    }
  }

  return(fields_numeric)
}

#' Create source list from 'data.frame'
#' @param source_df source `data.frame` object
#'
#' @import stringr
#'

.df_to_list <- function(source_df){

  source_list <- list()

  for (row in 1:nrow(source_df)){

    data_row <- source_df[row, ]
    inputId <- data_row$inputId

    row_as_list <-
      list(type = as.character(data_row$type),
           label = as.character(data_row$label),
           mandatory = as.logical(data_row$mandatory),
           width = as.character(data_row$width))

    if (row_as_list$type == "textInput") {
      pat <- "^chr_"

    } else if (row_as_list$type == "numericInput") {
      pat <- "^num_"

    } else if (row_as_list$type == "selectizeInput") {
      pat <- "^mult_|^select_"

    } else if (row_as_list$type == "radioButtons") {
      pat <- "^mult_|^radio_"

    } else {
      stop("Type of the question needs to be one of 'textInput', 'numericInput', 'selectizeInput', 'radioButtons'")
    }

    data_row <- data_row[, grepl(x = names(data_row), pattern = pat)]
    names(data_row) <- gsub(names(data_row), pattern = pat, replacement = "")
    row_as_list <- c(row_as_list, as.list(data_row))

    if (row_as_list$type == "selectizeInput" || row_as_list$type == "radioButtons") {

      if ((is.null(row_as_list$choices)||is.na(row_as_list$choices)) &&
          ((is.null(row_as_list$choiceValues)||is.na(row_as_list$choiceValues)) &
           (is.null(row_as_list$choiceNames)||is.na(row_as_list$choiceNames)))) {
        stop (paste0("For ", inputId, "both choices and choiceValues, choiceNames are missing."))
      }

      row_as_list[["choices"]] <-
        stringr::str_trim(unlist(stringr::str_split(row_as_list$choices, pattern = ";|\n")))
      row_as_list[["choiceValues"]] <-
        stringr::str_trim(unlist(stringr::str_split(row_as_list$choiceValues, pattern = ";|\n")))
      row_as_list[["choiceNames"]] <-
        stringr::str_trim(unlist(stringr::str_split(row_as_list$choiceNames, pattern = ";|\n")))

    }

    source_list[[as.character(inputId)]] <- row_as_list

  }

  return(source_list)

}

#' Generate source list from yaml
#'
#' @param yaml_file path to the source yaml file
#' @import yaml

.yaml_to_list <- function(yaml_file){
  yaml::read_yaml(file = yaml_file)
}

#' Read the Answer data from Google Sheets
#' @param output_ss character vector with output googlesheet ID
#' @param output_sheet character vector with output spreadsheet name
#' @import googlesheets4

.read_all_answers <- function(
  output_ss,
  output_sheet
) {
  googlesheets4::read_sheet(
    ss = output_ss,
    sheet = output_sheet
  )
}

#' Save the answer user input to Google Sheet
#' @param user_answers the object with inputs to extract
#' @param output_ss character vector with output googlesheet ID
#' @param output_sheet character vector with output spreadsheet name
#'
#' @import dplyr
#' @import googlesheets4
.save_new_answers <- function(
  user_answers,
  output_ss,
  output_sheet
  ) {

  ## drop invalid answers
  user_answers <- user_answers[!sapply(user_answers, is.null)]
  ## for multiple answers possible, seperate them with semicolon
  user_answers <- lapply(user_answers, function(x) {if(length(x)>1) {paste(x, collapse = ";")} else {x} })
  user_answers <- as.data.frame(user_answers)

  to_upload_answers <- tryCatch({

    old_answers <- .read_all_answers(
      output_ss = output_ss,
      output_sheet = output_sheet)

    ## I'm using `merge()` here and not `full_join()` because
    ## I want to dynamically coerce variable types. Otherwise,
    ## `full_join()`'s strict type safety raises an error
    to_upload_answers <- merge(old_answers, user_answers, all = TRUE, sort = FALSE)
    dplyr::relocate(.data = to_upload_answers,
                                         "timestamp",
                                         .after = dplyr::last_col())

  }, error = function(cond){

    dplyr::relocate(.data = user_answers,
                    "timestamp", .after = dplyr::last_col())

  })

  googlesheets4::sheet_write(
    ss = output_ss,
    data = to_upload_answers,
    sheet = output_sheet
  )
}
