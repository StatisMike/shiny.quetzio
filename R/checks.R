#' Check source df for correct columns
#'
#' @param source_df source of inputs
#' @noRd
#' @keywords internal

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

  # check for selectize and radio
  invisible(
    tryCatch({
      check_df <- source_df[source_df$type == "selectizeInput" || source_df$type == "radioButtons",]
      if (nrow(check_df) > 0){
        if (!"mult_choices" %in% names(check_df) || !all(c("mult_choiceValues", "mult_choiceNames") %in% names(check_df))) {
          stop(call. = F)
        }
      }
    },
    error = function(e){
      stop("source: With at least one 'selectizeInput' or 'radioButtons' type, the columns 'mult_choices' or both 'mult_choiceValues' and 'mult_choiceNames' need to be specified.",
           call. = F)
    })
  )
  
  # check for likertRadioButtons
  invisible(
    tryCatch({
      check_df <- source_df[source_df$type == "likertRadioButtons",]
      if (nrow(check_df) > 0){
        if (!all(c("mult_choiceValues", "mult_choiceNames") %in% names(check_df))) {
          stop(call. = F)
        }
      }
    },
    error = function(e){
      stop("source: With at least one 'likertRadioButtons' type, the columns 'mult_choiceValues' and 'mult_choiceNames' both need to be specified.",
           call. = F)
    })
  )
}

#' Check source list for correct columns
#'
#' @param source_list source of inputs
#' @noRd
#' @keywords internal

.check_source_list <- function(source_list){

  # check if all inputIds are unique
  if (length(source_list) != length(unique(names(source_list)))) {
    stop("source: The specified inputIds need to be unique.", call. = F)
  }

  # check if all items have the types and labels defined
  tryCatch({
    types_labels <- dplyr::bind_rows(lapply(
      source_list, function(x){
        data.frame(type = x$type,
                   label = x$label)
      }))
    types_labels[, c("type", "label")]
  }, error = function(e){
    stop("source: The mandatory universal columns in source are missing: one of 'inputId',
         'type' or 'label'", call. = F)
  })

  # check if there are all required columns for input type specified

  checks <- lapply(
    source_list, function(x){

      if (x$type == "selectizeInput" || x$type == "radioButtons") {
        if (is.null(x$choices) && all(is.null(x$choiceValues), is.null(x$choiceNames))) {
          stop("'choices' or both 'choiceNames' and 'choiceValues' are mandatory for 'selectizeInput' or 'radioButtons'")
        }
      } else if (x$type == "likertRadioButtons") {
        if (all(is.null(x$choiceValues), is.null(x$choiceNames))) {
          stop("Both 'choiceNames' and 'choiceValues' are mandatory for 'likertRadioButtons'")
        }
      }

    }
  )

}
