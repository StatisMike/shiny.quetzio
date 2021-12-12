#' Create UI for linked questionnaires
#'
#' @param link_id Character string holding ID for the link module. Needs to be the
#' same as one provided in 'quetzio_link_server'
#'
#' @import shiny
#' @export
#'

quetzio_link_UI <- function(link_id) {

  ns <- NS(link_id)
    uiOutput(ns("quetzio_link_UI"))
}


#' Create R6-based server module to generate and hold the state of your
#' linked questionnaires
#'
#' @import R6
#' @import shiny
#' @import dplyr
#' @export
#'


quetzio_link_server <- R6::R6Class(
  "quetzio_link_server",
  private = list(

    # names of the linked survey questionnaires
    quetzio_names = NULL,
    # reactiveValues holding the linked survey objects
    quetzio_list = NULL,
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

    }
  )
)
