#' Update Quetzio labels triggered by reactive value change
#' 
#' @param Quetzio object of class `Quetzio` or `Quetzio_link`
#' @param trigger reactive which will trigger the update. It needs to take
#' values linked to the changes in the source
#' @param source_method character string specifying in what form the source
#' config file will be provided. Can be either 'gsheet', 'yaml' or 'raw'.
#' Necessity of other arguments is dependent on this choice.
#' @param source_yaml path to the source yaml file
#' @param source_gsheet_id id of the source googlesheet file
#' @param source_gsheet_sheetname name of the source spreadsheet
#' @param source_object object of class `list` (similiar in structure to
#' 'yaml' source) or `data.frame` (similiar in structure to 'googlesheet'
#' source) to be the source of questions. You can create a sample data.frame
#' with \code{create_survey_source()}. Needed when `source_method == 'raw'`
#' @param name string indicating in which questionnaire the questions to update 
#' are located. Used with `QuetzioLink` objects 
#' @export
#' @rdname Quetzio_label_update


Quetzio_label_update <- function(
  Quetzio,
  trigger,
  source_method,
  source_yaml = NULL,
  source_gsheet_id = NULL,
  source_gsheet_sheetname = NULL,
  source_object = NULL,
  name = NULL
  ) {
  
  UseMethod("Quetzio_label_update", Quetzio)
  
}

#' @rdname Quetzio_label_update
#' @method Quetzio_label_update Quetzio
#' @example inst/examples/Quetzio_label_update.R
#' @export
#' 

Quetzio_label_update.Quetzio <- function(
  Quetzio,
  trigger,
  source_method,
  source_yaml = NULL,
  source_gsheet_id = NULL,
  source_gsheet_sheetname = NULL,
  source_object = NULL
) {
  
  Quetzio$update_labels(
    trigger = trigger,
    source_method = source_method,
    source_yaml = source_yaml,
    source_gsheet_id = source_gsheet_id,
    source_gsheet_sheetname = source_gsheet_sheetname,
    source_object = source_object
  )
}

#' @rdname Quetzio_label_update
#' @method Quetzio_label_update QuetzioLink
#' @example inst/examples/QuetzioLink_label_update.R
#' @export
#' 

Quetzio_label_update.QuetzioLink <- function(
  Quetzio,
  trigger,
  source_method,
  source_yaml = NULL,
  source_gsheet_id = NULL,
  source_gsheet_sheetname = NULL,
  source_object = NULL,
  name = NULL
) {
  
  Quetzio$update_labels(
    trigger = trigger,
    source_method = source_method,
    source_yaml = source_yaml,
    source_gsheet_id = source_gsheet_id,
    source_gsheet_sheetname = source_gsheet_sheetname,
    source_object = source_object,
    quetzio_name = name
  )
}

#' Update Quetzio selections based on external object
#' 
#' @param Quetzio object of class `Quetzio` or `Quetzio_link`
#' @param values list of values to update questionnaire with. List needs to be named,
#' as the names are going to be used to identify which inputId to update
#' @param name string indicating in which questionnaire the questions to update 
#' are located. Used with `QuetzioLink` objects 
#' @export
#' @rdname Quetzio_value_update

Quetzio_value_update <- function(
  Quetzio, values, name
) {
  UseMethod("Quetzio_value_update", Quetzio)
}

#' @rdname Quetzio_value_update
#' @method Quetzio_value_update Quetzio
#' @example inst/examples/Quetzio_value_update.R
#' @export
#' 

Quetzio_value_update.Quetzio <- function(
  Quetzio, values
) {
  Quetzio$update_values(values)
}


#' @rdname Quetzio_value_update
#' @method Quetzio_value_update QuetzioLink
#' @example inst/examples/QuetzioLink_value_update.R
#' @export
#' 

Quetzio_value_update.QuetzioLink <- function(
  Quetzio, values, name
) {
  Quetzio$update_values(quetzio_name = name, values = values)
}

#' Get answers from Quetzio in the form of data.frame
#' @param Quetzio object of class `Quetzio` or `Quetzio_link`
#' @param name string indicating for which questionnaire the answers to get 
#' in form of data.frame. Used with `QuetzioLink` objects - if left as NULL then, 
#' you get single data.frame with answers of all questionnaires
#' @export
#' @return data.frame
#' @rdname Quetzio_get_df

Quetzio_get_df <- function(Quetzio, name = NULL) {
  UseMethod("Quetzio_get_df", Quetzio)
}

#' @rdname Quetzio_get_df
#' @method Quetzio_get_df Quetzio
#' @export
#' 

Quetzio_get_df.Quetzio <- function(
  Quetzio
) {
  Quetzio$get_answers_df()
}

#' @rdname Quetzio_get_df
#' @method Quetzio_get_df QuetzioLink
#' @export
#' 

Quetzio_get_df.QuetzioLink <- function(
  Quetzio, name = NULL
) {
  
  if (is.null(name)) {
    Quetzio$get_answers_df()
  } else {
    if (!name %in% names(Quetzio$quetzio_list)) {
      stop(paste0("'name' argument should be a name of one of the linked Quetzio objects: '", 
           paste(names(Quetzio$quetzio_list), collapse = "', '"), "'."))
    } else {
      # check the QuetzioLink questionee_id
      questionee_id <- Quetzio$.__enclos_env__$private$questionee_id
      if (is.null(questionee_id)) {
        # if its null, return answers without any appending
        Quetzio$quetzio_list[[name]]$get_answers_df()
      } else {
        # if it is set, return answers with questionee_id appended
        cbind(data.frame(`.id` = questionee_id()),
              Quetzio$quetzio_list[[name]]$get_answers_df())
      }
    }
  }
}
