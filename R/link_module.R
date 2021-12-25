#' Create UI for linked questionnaires
#'
#' @param link_id Character string holding ID for the link module. Needs to be the
#' same as one provided in 'quetzio_link_server'
#'
#' @import shiny
#' @export
#' @seealso quetzio_link_server
#'

quetzio_link_UI <- function(link_id) {

  ns <- NS(link_id)
    uiOutput(ns("quetzio_link_UI"))
}


#' @title Quetzio Link Server class
#' @docType class
#'
#' @description Create R6-based server module to generate and hold the state of
#' multiple linked 'quetzio_server' objects
#'
#' @import R6
#' @import shiny
#' @import dplyr
#' @seealso quetzio_server
#' @seealso quetzio_link_UI
#' @export
#'


quetzio_link_server <- R6::R6Class(
  "quetzio_link_server",
  private = list(

    # names of the linked survey questionnaires
    quetzio_names = NULL,
    # googlesheets specification
    output_gsheet = FALSE,
    output_gsheet_id = NULL,
    output_gsheet_sheetname = NULL

  ),
  public = list(

    #' @field link_id character with id of the quetzio_link_module
    link_id = NULL,

    #' @field completion reactiveVal object holding the rate of linked
    #' questionnaires completion
    completion = NULL,

    #' @field message reactiveVal object holding the list of linked questionnaires
    #' error messages
    message = NULL,

    #' @field answers reactiveVal object holding the list of answers to the
    #' linked questionnaires
    answers = NULL,

    #' @field quetzio_list reactiveValues holding the linked 'quetzio_server's
    quetzio_list = NULL,


    #' @description Initialization of the 'quetzio_link_server' object
    #'
    #' @param ... 'quetzio_server' objects that are going to be connected by the
    #' link. They are all needed to be named - with the names providing the path
    #' to the answers of their questions.
    #' Moreover, they need to be in initialized state: \code{quetzio_server$new(...)}
    #' @param link_id character specifying the module ID of created object. Needs
    #' to be identical to the one specified inside every 'quetzio_server' objects
    #' provided to the object.
    #' @param output_gsheet logical: do you wish to save the answers automatically
    #' to the googlesheet. If TRUE, the 'output_gsheet_id' and 'output_gsheet_sheetname'
    #' arguments need to be specified. Defaults to FALSE
    #' @param output_gsheet_id id of the output googlesheet file
    #' @param output_gsheet_sheetname name of the output spreadsheet
    #'
    initialize = function(
      link_id,
      ...,
      output_gsheet = FALSE,
      output_gsheet_id = NULL,
      output_gsheet_sheetname = NULL
    ) {

      self$link_id <- link_id

      # catching the names of the provided quetzio_server objects
      args <- match.call(expand.dots = FALSE)
      private$quetzio_names <- names(args$...)

      # modify the arguments to the quetzio_survey calls without evaluation
      uneval <- .modify_quetzio_arg(..., link_id = self$link_id)

      # initializing checks

      # for output_gsheet method
      if (isTRUE(as.logical(output_gsheet))) {
        # check for package
        .check_package("googlesheets4")
        # check for output gsheet specification
        if (is.null(output_gsheet_id) || is.null(output_gsheet_sheetname)){
          stop("When 'output_gsheet' == TRUE, you need to specify 'output_gsheet_id' and 'output_gsheet_sheetname'")
        }
        # assign them to the private
        private$output_gsheet <- TRUE
        private$output_gsheet_id <- output_gsheet_id
        private$output_gsheet_sheetname <- output_gsheet_sheetname
      }

      # call to the moduleServer handling all the logic

      .link_backend(self, private, uneval)

    },

    #' @description method to get preprocessed answers in the form of dataframe
    #' (only if the questionnaire is done)
    #' @return data.frame

    get_answers_df = function() {

      if (isTRUE(self$completion() == 1)) {
        .merge_linked_answers_to_df(self$answers(), private$quetzio_names)
      } else {
        stop("All linked questionnaires needs to be done to get the answers in the form of data.frame")
      }
    },

    #' @description Method to update labels on the change in reactive
    #'
    #' @param quetzio_name string indicating in which questionnaire
    #' the questions to update are located
    #' @param trigger reactive which will trigger the update. It needs to take
    #' values linked to the changes in the source
    #' @param source_method character string specifying in what form the source
    #' config file will be provided. Can be either 'gsheet', 'yaml' or 'raw'.
    #' Necessity of other arguments is dependent on this choice. For more info
    #' see 'details'
    #' @param source_yaml path to the source yaml file
    #' @param source_gsheet_id id of the source googlesheet file
    #' @param source_gsheet_sheetname name of the source spreadsheet
    #' @param source_object object of class `list` (similiar in structure to
    #' 'yaml' source) or `data.frame` (similiar in structure to 'googlesheet'
    #' source) to be the source of questions. You can create a sample data.frame
    #' with \code{create_survey_source()}. Needed when `source_method == 'raw'`
    #'
    #'

    update_labels = function(
      quetzio_name,
      trigger,
      source_method,
      source_yaml = NULL,
      source_gsheet_id = NULL,
      source_gsheet_sheetname = NULL,
      source_object = NULL
    ) {

      observe(
        self$quetzio_list[[quetzio_name]]$update_labels(
          trigger,
          source_method,
          source_yaml = source_yaml,
          source_gsheet_id = source_gsheet_id,
          source_gsheet_sheetname = source_gsheet_sheetname,
          source_object = source_object
        )
      )
    },

    #' @description Method to update selected values on the change in reactive
    #'
    #' @param quetzio_name string indicating in which questionnaire
    #' the item values to update are located
    #' @param values reactive which will trigger the update and contain named list
    #' with values to update. List need to be named, as the names are going to be
    #' used to identify which inputId to update

    update_values = function(
      quetzio_name,
      values
    ){
      observe(
        self$quetzio_list[[quetzio_name]]$update_values(values)
      )
    }

  )
)

#' @name quetzio_link_server
#' @example inst/examples/quetzio_link_server.R
NULL
