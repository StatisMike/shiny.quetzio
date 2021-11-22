#' Check source df for correct columns
#'
#' @param source_df source of inputs

.check_source_df <- function(source_df){

  # check if all mandatory universal columns are present

  if (!all(c("inputId", "type", "label") %in% names(source_df))) {
    stop("source: The mandatory universal columns in source are missing: one of 'inputId',
         'type' or 'label'")
  }

  # check if all inputIds are unique

  if (length(unique(source_df$inputId)) != nrow(source_df)) {
    stop("source: The specified inputIds need to be unique.")
  }

  # check if there are all required columns for input type specified

  # checks for numeric
  invisible(
    tryCatch({
      check_df <- source_df[source_df$type == "numericInput",]
      if (nrow(check_df) > 0){
        check_df[, c("num_value", "num_min", "num_max", "num_step")]
      }
    },
    error = function(e){
      stop("source: With at least one 'numericInput' type, the columns 'num_value', 'num_min', 'num_max' and 'num_step' need to be specified.",
           call. = F)
    })
  )

  # check for selectize

  invisible(
    tryCatch({
      check_df <- source_df[source_df$type == "selectizeInput",]
      if (nrow(check_df) > 0){

        if (!"mult_choices" %in% names(check_df) || !all(c("mult_choiceValues", "mult_choiceNames") %in% names(check_df))) {
          stop(call. = F)
        }

        check_df[, c("mult_selected", "select_multiple")]
      }
    },
    error = function(e){
      stop("source: With at least one 'selectizeInput' type, the columns 'mult_selected', 'select_multiple' and 'mult_choices' or both 'mult_choiceValues' and 'mult_choiceNames' need to be specified.",
           call. = F)
    })
  )

  # check for radio

  invisible(
    tryCatch({
      check_df <- source_df[source_df$type == "radioButtons",]
      if (nrow(check_df) > 0){

        if (!"mult_choices" %in% names(check_df) || !all(c("mult_choiceValues", "mult_choiceNames") %in% names(check_df))) {
          stop(call. = F)
        }

        check_df[, c("mult_selected", "radio_inline")]
      }
    },
    error = function(e){
      stop("source: With at least one 'selectizeInput' type, the columns 'mult_selected', 'select_multiple' and 'mult_choices' or both 'mult_choiceValues' and 'mult_choiceNames' need to be specified.",
           call. = F)
    })
  )

}

#' Check source list for correct columns
#'
#' @param source_list source of inputs

.check_source_list <- function(source_list){



}
