#' Create data.frame source compatible with shiny.survey.
#'
#' @details Function to help initialize source data.frame for survey generation.
#' It should contain specific columns, which are created here alongside one
#' placeholder input to help populating next ones.
#'
#' Name suffix helps to determine for which types of inputs these columns
#' are used. Variables without suffix are used in creation of all types.
#' \itemize{
#' \item{\code{chr_}: textInput}
#' \item{\code{num_}: numericInput}
#' \item{\code{select_}: selectizeInput}
#' \item{\code{radio_}: radioButtons}
#' \item{\code{mult_}: selectizeInput and radioButtons}
#' }
#'
#' To use 'gsheet' method, the 'googlesheets4' package needs
#' to be installed.
#'
#' You can create the source 'data.frame' yourself, keeping only the columns that
#' are universally mandatory and specific to used inputTypes, though it is
#' recommended to keep all columns that are created during initialization - the
#' variable classes should be kept.
#'
#' When using 'googlesheet' as a source, all columns should be kept alongside
#' their order to ensure correct import.
#'
#' @param method Which method to use. One of 'df' (default) and 'gsheet'.
#' @param name optional name for your googlesheet. Used only
#' if \code{method == 'gsheet'}
#'
#' @return Depends on specified method:
#' \itemize{
#' \item{\code{df}: data.frame object}
#' \item{\code{gsheet}: ID of the created googlesheet}
#' }
#'
#' @export

create_survey_source <- function(
  method = "df",
  name = NULL
){

  if(!method %in% c("df", "gsheet")){
    stop("The method should be one of 'df' or 'gsheet'")
  }

  out = data.frame(
      inputId = "placeholder",
      type = "textInput",
      mandatory = TRUE,
      label = "Remove before production",
      width = "500px",
      chr_placeholder = "some text",
      num_value = 1,
      num_min = 0,
      num_max = 2,
      num_step = 0.5,
      mult_choices = "Something\nElse\nMore",
      mult_choiceValues = "1\n2\n3",
      mult_choiceNames = "One\nTwo\nThree",
      mult_selected = "NULL",
      select_maxItems = 1,
      radio_inline = TRUE
  )

  if (method == "gsheet") {
    # checking if googlesheets4 is installed
   .check_package("googlesheets4")

    # getting random name if not specified
    if(is.null(name)){
      name <- googlesheets4::gs4_random()
    }

    # create the googlesheet and return id
    out <- googlesheets4::gs4_create(
      name = name,
      sheets = list(Questions = out)
    )

  }

  return(out)

}

#' Create source list from 'data.frame'
#' @param source_df source `data.frame` object
#' @param type character which strategy to take
#'
#' @import stringr
#'

.df_to_list <- function(source_df, type = "quetzio_source"){

  source_list <- list()

  if (type == "quetzio_source") {

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

  } else if (type == "quetzio_desc") {

    source_list <- lapply(c(1:nrow(source_df)), \(i) {

      x <- as.list(source_df[i,])

      row_as_list <- list(
        type = x$type,
        html = .null_def(x$html, FALSE),
        text = x$text
      )

      switch(
        row_as_list$type,

        instruction_title = {
          row_as_list$align <- .null_def(x$align, "left")

        },

        instruction_para = {
          row_as_list$align <- .null_def(x$align, "left")

        },

        instruction_list = {
          row_as_list$order <- .null_def(x$align, FALSE)
          row_as_list$text <- unlist(
            stringr::str_split(
              string = row_as_list$text,
              pattern = ";|\n")
          )

        },

        item_desc = {
          row_as_list$inputId <- x$inputId
          row_as_list$align <- .null_def(x$align, "left")

        }
      )

      return(row_as_list)

    })


  }

  return(source_list)

}

#' Generate source list from yaml
#'
#' @param yaml_file path to the source yaml file


.yaml_to_list <- function(yaml_file){
  yaml::read_yaml(file = yaml_file)
}

#' Read the Answer data from Google Sheets
#' @param output_ss character vector with output googlesheet ID
#' @param output_sheet character vector with output spreadsheet name


.read_all_answers <- function(
  output_ss,
  output_sheet
) {
  googlesheets4::read_sheet(
    ss = output_ss,
    sheet = output_sheet
  )
}

