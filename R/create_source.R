#' Create data.frame source compatible with shiny.survey
#'
#' @details Function to help initialize source data.frame for quetzio generation.
#' It should contain specific columns, which are created here alongside one
#' placeholder input to help populating next ones.
#'
#' Name suffix helps to determine for which types of inputs these columns
#' are used. Variables without suffix are used in creation of which input types.
#' 
#' \itemize{
#' \item{\code{chr_}: textInput}
#' \item{\code{num_}: numericInput}
#' \item{\code{select_}: selectizeInput}
#' \item{\code{radio_}: radioButtons}
#' \item{\code{mult_}: selectizeInput, radioButtons and likertRadioButtons}
#' }
#' 
#' For more details about specific variables and their meaning check 
#' **Question parameters** section. For intricacies of every input type check
#' their help pages.
#' 
#' - 'numericInput' uses actually custom 'numInput' function
#' - 'likertRadioButtons' is custom new shinyInput introduced with the package
#'
#' To use 'gsheet' method, the 'googlesheets4' package needs
#' to be installed.
#'
#' You can create the source 'data.frame' yourself, keeping only the columns that
#' are universally mandatory and specific to used inputTypes, though it is
#' recommended to keep all columns that are created during initialization - the
#' column classes should be kept.
#'
#' When using 'googlesheet' as a source, all columns should be kept alongside
#' their order to ensure correct import.
#' 
#' @section Question parameters:
#' 
#' Bolded are mandatory. Parentheses contains the suffix that need to be provided
#' during *data.frame* or *googlesheets* source creation. If you are creating a *YAML*
#' or *list* source, provide the names without suffix.
#' 
#' - All input types:
#'   - **InputId**: *character* Id for input
#'   - **type**: *character* Input type. One of 'textInput', 'numericInput', 'selectizeInput',
#'   'radioButtons' or 'likertRadioButtons'
#'   - mandatory: *boolean* Is the question is mandatory
#'   - width: *character* CSS-valid width value
#' - textInput:
#'   - placeholder: *character* Text that will be displayed before user provides
#'   their answer
#'   - (chr_)regex: *character* regex pattern to check validity
#' - numericInput:
#'   - placeholder: *character* Text that will be displayed before user provides
#'   their answer
#'   - (num_)value: *numeric* Pre-filled value that will be placed. Leave empty or
#'   NA to no pre-fill.
#'   - (num_)min: *numeric* Minimal possible value
#'   - (num_)max: *numeric* Maximal possible value
#'   - (num_)step: *numeric* Step to increase/decrease value
#' - selectizeInput:
#'   - placeholder: *character* Text that will be displayed before user provides
#'   their answer
#'   - **(mult_)choices**: *character* Options that will be presented to the user
#'   and will be saved as values. You can separate them with either `;` or `\n`
#'   - **(mult_)choiceValues**: *character* Values that will be saved per option. 
#'   Need to be provided alongside *choiceNames*. You can separate them with 
#'   either `;` or `\n`
#'   - **(mult_)choiceNames**: *character* Names of the options that will be
#'   displayed to the user. Need to be provided alongside *choiceValues*. 
#'   You can separate them with either `;` or `\n`
#'   - (mult_)selected: *character* Value of the option to be preselect. No selection
#'   if left empty / NA
#'   - (select_)maxItems: *numeric* Number of items which the user will be able
#'   to select. Levae empty or NA to let the user choose all.
#'   - (select_)create: *boolean* Should the questionee be able to create their
#'   own value? Defaults to FALSE.
#' - radioButtons:
#'   - **(mult_)choices**: *character* Options that will be presented to the user
#'   and will be saved as values. You can separate them with either `;` or `\n`
#'   - **(mult_)choiceValues**: *character* Values that will be saved per option. 
#'   Need to be provided alongside *choiceNames*. You can separate them with 
#'   either `;` or `\n`
#'   - **(mult_)choiceNames**: *character* Names of the options that will be
#'   displayed to the user. Need to be provided alongside *choiceValues*. 
#'   You can separate them with either `;` or `\n`
#'   - (mult_)selected: *character* Value of the option to be preselect. No selection
#'   if left empty / NA
#'   - (radio_)inline: *boolean* Should the buttons be placed inline? Defaults
#'   to FALSE
#' - likertRadioButtons:
#'   - placeholder: *character* Text that will be displayed before user provides
#'   their answer. 
#'   - **(mult_)choiceValues**: *character* Values that will be saved per option. 
#'   Need to be provided alongside *choiceNames*. You can separate them with 
#'   either `;` or `\n`
#'   - **(mult_)choiceNames**: *character* Names of the options that will be
#'   displayed to the user. Need to be provided alongside *choiceValues*. 
#'   You can separate them with either `;` or `\n`
#'   - (mult_)selected: *character* Value of the option to be preselect. No selection
#'   if left empty / NA
#'
#' @param method Which method to use. One of 'df' (default) and 'gsheet'.
#' @param gsheet_id id if you want to append the sheet to existing googlesheet
#' @param gsheet_name optional name for your googlesheet. Used only
#' if \code{method == 'gsheet'}
#' @param gsheet_sheetname name for the spreadsheet name
#'
#' @return Depends on specified method:
#' \itemize{
#' \item{\code{df}: data.frame object}
#' \item{\code{gsheet}: ID of the created googlesheet}
#' }
#'
#' @export

create_quetzio_source <- function(
  method = "df",
  gsheet_id = NULL,
  gsheet_name = NULL,
  gsheet_sheetname = "Questions"
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
      placeholder = "some text",
      chr_regex = "^pattern$",
      num_value = 1,
      num_min = 0,
      num_max = 2,
      num_step = 0.5,
      mult_choices = "Something\nElse\nMore",
      mult_choiceValues = "1\n2\n3",
      mult_choiceNames = "One\nTwo\nThree",
      mult_selected = as.character(NA),
      select_maxItems = 1,
      select_create = TRUE,
      radio_inline = TRUE
  )

  if (method == "gsheet") {
    # checking if googlesheets4 is installed
   .check_package("googlesheets4")

    if (is.null(gsheet_id)) {
      # when gsheet_id is not specified - create new gsheet
      # getting random name if not specified
      if(is.null(gsheet_name)){
        gsheet_name <- googlesheets4::gs4_random()
      }
      # create data for the append
      sheet_to_append <- list(Questions = out)
      names(sheet_to_append) <- gsheet_sheetname

      # create the googlesheet and return id
      out <- googlesheets4::gs4_create(
        name = gsheet_name,
        sheets = sheet_to_append
      )

    } else {
      # when gsheet_id is specified - append new sheet
      out <- googlesheets4::sheet_write(
        ss = gsheet_id,
        sheet = gsheet_sheetname,
        data = out
      )
    }
  }

  return(out)

}

#' Create data.frame for optional instruction and descriptions compatible with shiny.survey.
#'
#' @details Function to help initialize source data.frame to generate
#' optional instructions and descriptions for quetzio.
#' It should contain specific columns, which are created here alongside one
#' placeholder input to help populating next ones.
#'
#'
#' To use 'gsheet' method, the 'googlesheets4' package needs
#' to be installed.
#'
#' You can create the source 'data.frame' yourself, keeping only the columns that
#' are universally mandatory and specific to used types, though it is
#' recommended to keep all columns that are created during initialization - the
#' column classes should be kept.
#'
#' When using 'googlesheet' as a source, all columns should be kept alongside
#' their order to ensure correct import.
#'
#' @param method Which method to use. One of 'df' (default) and 'gsheet'.
#' @param gsheet_id id if you want to append the sheet to existing googlesheet
#' @param gsheet_name optional name for your googlesheet. Used only
#' if \code{method == 'gsheet'}
#' @param gsheet_sheetname name for the spreadsheet name
#'
#' @return Depends on specified method:
#' \itemize{
#' \item{\code{df}: data.frame object}
#' \item{\code{gsheet}: ID of the created googlesheet}
#' }
#'
#' @export

create_desc_source <- function(
  method = "df",
  gsheet_id = NULL,
  gsheet_name = NULL,
  gsheet_sheetname = "Descriptions"
){

  if(!method %in% c("df", "gsheet")){
    stop("The method should be one of 'df' or 'gsheet'")
  }

  out = data.frame(
    type = "placeholder",
    align = "left",
    html = TRUE,
    order = TRUE,
    inputId = "placeholder",
    content = "<b>Delete</b> this <i>example</i>"
  )

  if (method == "gsheet") {
    # checking if googlesheets4 is installed
    .check_package("googlesheets4")

    if (is.null(gsheet_id)) {
      # when gsheet_id is not specified - create new gsheet
      # getting random name if not specified
      if(is.null(gsheet_name)){
        gsheet_name <- googlesheets4::gs4_random()
      }
      # create data for the append
      sheet_to_append <- list(Questions = out)
      names(sheet_to_append) <- gsheet_sheetname

      # create the googlesheet and return id
      out <- googlesheets4::gs4_create(
        name = gsheet_name,
        sheets = sheet_to_append
      )

    } else {
      # when gsheet_id is specified - append new sheet
      out <- googlesheets4::sheet_write(
        ss = gsheet_id,
        sheet = gsheet_sheetname,
        data = out
      )
    }
  }

  return(out)

}

#' Create source list from 'data.frame'
#' @param source_df source `data.frame` object
#' @param type character indicating which strategy to take
#' @noRd
#' @keywords internal

.df_to_list <- function(source_df, type = "quetzio_source"){

  source_list <- list()

  # strategy for quetzio source
  if (type == "quetzio_source") {

    # for every row of the dataframe
    for (row in 1:nrow(source_df)){

      data_row <- source_df[row, ]
      inputId <- data_row$inputId

      # get universal arguments
      row_as_list <-
        list(type = as.character(data_row$type),
             label = as.character(data_row$label),
             mandatory = as.logical(data_row$mandatory),
             width = as.character(data_row$width))
      
      # get also placeholder if present
      
      if (!is.null(data_row$placeholder) || !is.na(data_row$placeholder)) {
        row_as_list$placeholder <- as.character(data_row$placeholder)  
      }

      # detect the type and set the pattern accordingly
      
      pat <- switch(
        row_as_list$type,
        textInput = "^chr_",
        numericInput = "^num_",
        selectizeInput = "^mult_|^select_",
        radioButtons = "^mult_|^radio_",
        likertRadioButtons = "^mult_",
        stop("Type of the question needs to be one of 'textInput', 'numericInput', 'selectizeInput', 'radioButtons', 'likertRadioButtons")
      )

      # select only correctly named columns
      data_row <- data_row[, grepl(x = names(data_row), pattern = pat)]
      # name them accordingly - without the prefix
      names(data_row) <- gsub(names(data_row), pattern = pat, replacement = "")
      # then save it into the list
      row_as_list <- c(row_as_list, as.list(data_row))

      # checks for selectize and radioButtons inputs - if the choices/choice names are correct
      if (row_as_list$type == "selectizeInput" || row_as_list$type == "radioButtons") {

        if ((is.null(row_as_list$choices)||is.na(row_as_list$choices)) &&
            ((is.null(row_as_list$choiceValues)||is.na(row_as_list$choiceValues)) &
             (is.null(row_as_list$choiceNames)||is.na(row_as_list$choiceNames)))) {
          stop (paste0("For ", inputId, "both choices or choiceValues and choiceNames are mandatory."))
        }

        # str_split the multiple values on ';' and '\n' separators
        row_as_list[["choices"]] <-
          stringi::stri_trim_both(unlist(stringi::stri_split(row_as_list$choices, regex = ";|\n")))
        row_as_list[["choiceValues"]] <-
          stringi::stri_trim_both(unlist(stringi::stri_split(row_as_list$choiceValues, regex = ";|\n")))
        row_as_list[["choiceNames"]] <-
          stringi::stri_trim_both(unlist(stringi::stri_split(row_as_list$choiceNames, regex = ";|\n")))

      # checks for likertRadioButtons- choiceNames and choiceValues need to be present
      } else if (row_as_list$type == "likertRadioButtons") {
        if ((is.null(row_as_list$choiceValues) || is.na(row_as_list$choiceValues)) ||
            (is.null(row_as_list$choiceNames) || is.na(row_as_list$choiceNames))) {
          stop (paste0("For ", inputId, "both choiceValues and choiceNames are mandatory."))
        }
        
        # str_split the multiple values on ';' and '\n' separators
        row_as_list[["choiceValues"]] <-
          as.numeric(stringi::stri_trim_both(unlist(stringi::stri_split(row_as_list$choiceValues, regex = ";|\n"))))
        row_as_list[["choiceNames"]] <-
          stringi::stri_trim_both(unlist(stringi::stri_split(row_as_list$choiceNames, regex = ";|\n")))
        
      }

      # save the list element with correct inputId name
      source_list[[as.character(inputId)]] <- .dropNulls(row_as_list, na.rm = T)

    }

    # strategy for optional instruction and item description
  } else if (type == "quetzio_desc") {

    # get the list with lapply
    source_list <- lapply(c(1:nrow(source_df)), \(i) {

      # save the row as a list
      x <- as.list(source_df[i,])

      # get the universal arguments
      row_as_list <- list(
        type = x$type,
        html = .null_def(x$html, FALSE),
        content = x$content,
        align = .null_def(x$align, "left")
      )

      # for every type get needed arguments

      if (row_as_list$type == "instructions_list") {

        row_as_list$order <- .null_def(x$order, FALSE)
        # split multiple values on separators ';' and '\n'
        row_as_list$content <- stringi::stri_trim_both(unlist(
          stringi::stri_split(
            str = row_as_list$content,
            regex = ";|\n")))

      } else if (row_as_list$type == "item_desc") {
        
        row_as_list$inputId <- x$inputId
        
      }

      # return value from lapplied function
      return(row_as_list)
    })

  }

  return(source_list)

}

#' Function to randomize order of source_list object
#' 
#' @param source_list list containing items to generate
#' 
#' @return List containing two objects:
#' - 'source_list', with input in randomized order
#' - 'order', with item indices
#' @noRd
#' @keywords internal
#' 
.randomize_source <- function(source_list) {
  
  order <- sample(x = c(1:length(source_list)), 
                  size = length(source_list))
  
  source_list <- source_list[order]
  
  return(
    list(source_list = source_list,
         order = order)
  )
  
}

