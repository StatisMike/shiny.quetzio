#' Create data.frame source compatible with shiny.survey.
#'
#' @details Function to help initialize source data.frame for survey generation.
#' It should contain specific columns, which are created here alongside one
#' placeholder input to help populating next ones.
#'
#' Name suffix helps to determing for which types of inputs these columns
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
#' recommended to keep all columns that are created during initialization.
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
      mandatory = "true",
      label = "Remove before production",
      width = "500px",
      chr_placeholder = "some text",
      num_value = "1",
      num_min = "0",
      num_max = "2",
      num_step = "0.5",
      mult_choices = "Something\nElse\nMore",
      mult_choiceValues = "1\n2\n3",
      mult_choiceNames = "One\nTwo\nThree",
      mult_selected = "NULL",
      select_multiple = "TRUE",
      radio_inline = "TRUE"
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
