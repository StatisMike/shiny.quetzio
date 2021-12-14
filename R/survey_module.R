#' Create UI of your questionnaire
#'
#' @param module_id Character string holding ID for the module. Needs to be the
#' same as one provided for 'quetzio_server'
#' @import shiny
#' @export
#'

quetzio_UI <- function(module_id) {
  ns <- NS(module_id)

  uiOutput(ns("quetzio_UI"))

}

#' @title Quetzio Server class
#' @docType class
#'
#' @description Create R6-based server module to generate and hold the state
#' of your questionnaire
#'
#' @import R6
#' @import shiny
#' @import shinyjs
#' @import dplyr
#' @export
#'

quetzio_server <- R6::R6Class(
  "quetzio_server",

  private = list(

    mandatory_items = NULL,
    numeric_items = NULL,
    selectize_items = NULL,
    output_gsheet = NULL,
    output_ss = NULL,
    output_sheet = NULL,
    css = NULL,
    render_ui = NULL

  ),

  public = list(

    #' @field source_list List containing the data for all the inputs
    source_list = NULL,

    #' @field div_id ID of the div containing the survey UI
    div_id = NULL,

    #' @field module_id ID of the shiny module
    module_id = NULL,

    #' @field button_labels character vector of length two with labels for submit
    #' button in active and disabled state
    button_labels = NULL,

    #' @field is_done logical reactiveVal indicating if the survey has been completed
    is_done = NULL,

    #' @field message reactiveVal catching any warning messages
    message = NULL,

    #' @field answers reactiveValues object containing answers to questions
    answers = NULL,

    #' @description method to change the state of the UI
    #'
    #' @param x logical indicating what should be the state of the UI. If TRUE,
    #' then the UI will be rendered.
    toggle_ui = function(x) {

      private$render_ui(isTRUE(x))

    },

    #' @field module_ui_id character string used to generate UI. It needs to
    #' be modified when linking the questionnaires

    module_ui_id = NULL,

    #' @description Initializing the 'quetzio_server' object
    #'
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
    #' @param output_gsheet logical: do you wish to save the answers automatically
    #' to the googlesheet. If TRUE, the 'output_gsheet_id' and 'output_gsheet_sheetname'
    #' arguments need to be specified. Defaults to FALSE
    #' @param output_gsheet_id id of the output googlesheet file. If not specified,
    #' the same googlesheet as for 'source' will be used
    #' @param output_gsheet_sheetname name of the output spreadsheet
    #' @param module_id character string with unique id for the module. If not
    #' specified, it will be automatically generated
    #' @param div_id character string with unique id for the created div. If not
    #' specified, it will be set to 'form'
    #' @param custom_css custom css for classes 'mandatory star' and 'invalid_input'.
    #' If not specified, default look will be used:
    #' \itemize{
    #' \item{invalid_input = "outline: red; outline-style: dashed; outline-offset: 10px;"}
    #' \item{mandatory_star = "color: red;"}
    #' }
    #' @param button_labels character vector of length four with labels for submit
    #' button i all states. Defaults to:
    #' \code{c('Submit', 'Cannot submit', 'Submitted!', 'Error occured!')}
    #' @param render_ui logical indicating if the UI for questionnaire should be
    #' rendered
    #' @param link_id character specifying the 'link_id' of the 'quetzio_link_server'
    #' object, modifying its namespace. Only used internally, if the questionnaire
    #' is part of linked server. Don't set it manually.
    #'
    #' @details
    #'
    #' Currently, there are multiple methods both for source, which will generate
    #' the inputs, and for output. Mandatory arguments change depending of your
    #' choices:
    #'
    #' - for source:
    #'   - \code{source_method == 'yaml'}: 'source_yaml'
    #'   - \code{source_method == 'gsheet'}: 'source_gsheet_id,' 'source_gsheet_sheetname'
    #'   - \code{source_method == 'raw'}: 'source_object'
    #' - for output:
    #'   - \code{output_gsheet == TRUE}: 'output_gsheet_id' (if other than 'source_gsheet_id')
    #'   and 'output_gsheet_sheetname'
    #'
    #'
    #' @return the 'quetzio_server' serverModule

    initialize = function(
      source_method,
      source_object = NULL,
      source_yaml = NULL,
      source_gsheet_id = NULL,
      source_gsheet_sheetname = NULL,
      output_gsheet = FALSE,
      output_gsheet_id = NULL,
      output_gsheet_sheetname = NULL,
      module_id = NULL,
      div_id = "form",
      custom_css = NULL,
      button_labels = c("Submit", "Cannot submit", "Submitted!", "Error occured!"),
      render_ui = TRUE,
      link_id = NULL
    ){

      # initialize checks

      # check if all needed arguments are provided for source methods
      if (source_method == "gsheet") {
        #for gsheet source: if package is installed and if source ids are specified
        .check_package("googlesheets4")
        if (is.null(source_gsheet_id) || is.null(source_gsheet_sheetname)) {
          stop("When 'source_method' == 'gsheet', you need to specify 'source_gsheet_id' and 'source_gsheet_sheetname'.")
        }
        #for yaml source: if package is installed and if source file is provided
      } else if (source_method == "yaml") {
        .check_package("yaml")
        if (is.null(source_yaml)) {
          stop("When 'source_method' == 'yaml', you need to specify 'source_yaml'")
        }
        # for raw: if object is a dataframe or list
      } else if (source_method == "raw" && (is.null(source_object) && !class(source_object) %in% c("data.frame", "list"))) {
        stop("When 'source_method' == 'raw', you need to pass an object of class 'data.frame' or 'list' to 'source_object'")
        # if other source method is provided: error
      } else if (!source_method %in% c("gsheet", "yaml", "raw")) {
        stop("'source_method' must be chosen between 'gsheet', 'yaml' or raw.")
      }

      # check if all needed arguments are provided for output methods
      if (isTRUE(as.logical(output_gsheet))) {
        .check_package("googlesheets4")
        if ((is.null(source_gsheet_id) && is.null(output_gsheet_id)) || is.null(output_gsheet_sheetname)){
          stop("When 'output_gsheet' == TRUE, you need to specify 'output_gsheet_id' (if other from 'source_gsheet_id') and 'output_gsheet_sheetname'")
        }
      }

      # save the module id into environment

      self$module_id <- module_id

      if (is.null(link_id))  {
        self$module_ui_id <- self$module_id
      } else {
        self$module_ui_id <- paste(link_id, self$module_id, sep = ns.sep)
      }
      # read the file and save resulting list in the environment

      if (source_method == "gsheet"){

        source_df <- googlesheets4::read_sheet(
          ss = source_gsheet_id,
          sheet = source_gsheet_sheetname,
          col_types = "cclccnnnnccclclc"
        )

        # check df validity
        .check_source_df(source_df)

        self$source_list <- .df_to_list(
          source_df = source_df
        )

      } else if (source_method == "yaml") {
        source_list <- .yaml_to_list(
          yaml_file = source_yaml)

        # check list validity
        .check_source_list(source_list)

        self$source_list <- source_list


      } else if (source_method == "raw") {

        if (class(source_object) == "data.frame") {

          # checks if df is valid
          .check_source_df(source_object)
          self$source_list <- .df_to_list(source_object)

        } else if (class(source_object) == "list") {

          # checks if list is valid
          .check_source_list(source_object)
          self$source_list <- source_object

        } else {
          stop("Source object needs to be of class 'data.frame' or 'list'")
        }

      } else {
        stop("Error - problems with source")
      }

      # check if the submit button labels are correct
      if (class(button_labels) != "character" && length(button_labels) != 2) {
        stop("'button_labels' should be specified as character vector of length 2")
      }

      # check for mandatory and numeric inputs

      private$mandatory_items <- .get_mandatory(self$source_list)
      private$numeric_items <- .get_type(self$source_list, "numericInput")

      # save other information in private

      self$div_id <- div_id
      self$button_labels <- button_labels
      private$output_gsheet <- output_gsheet
      if(output_gsheet) {
        private$output_ss <- if (is.null(output_gsheet_id) || is.na(output_gsheet_id)) source_gsheet_id else output_gsheet_id
        private$output_sheet <- output_gsheet_sheetname
      }

      # parsing css from the lists to the correct css
      if (is.null(custom_css)) {
        private$css <- .custom_css_handler(div_id = self$div_id)
      } else {
        private$css <- .custom_css_handler(div_id = self$div_id,
                                           css = custom_css)
      }

      # initialize the status reactiveVal objects
      self$is_done <- reactiveVal(FALSE)
      self$message <- reactiveVal()
      self$answers <- reactiveVal()

      # value showing if the UI output should be rendered
      private$render_ui <- reactiveVal(render_ui)

      # calling the whole server backend logic
      .survey_backend(self, private)

    },

    #' @description method to get preprocessed answers in the form of dataframe
    #' (only if all of the questionnaires are done)
    #' @return data.frame

    get_answers_df = function() {

      if (isTRUE(self$is_done())) {
        .sanitize_answers(self$answers())
      } else {
        stop("Questionnaire needs to be done to get the answers in the form of data.frame")
      }

    },

    #' @description Method to update labels on the change in reactive
    #'
    #' @param trigger reactive which will trigger the update. It needs to take
    #' values linked to the changes in the source
    #' @param source_method character string specifying in what form the source
    #' config file will be provided. Can be either 'gsheet', 'yaml' or 'raw'.
    #' Necessity of other arguments is dependent on this choice
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
      trigger,
      source_method,
      source_yaml = NULL,
      source_gsheet_id = NULL,
      source_gsheet_sheetname = NULL,
      source_object = NULL
    ) {

      .quetzio_label_update(
        self = self,
        trigger = trigger,
        source_method = source_method,
        source_yaml = source_yaml,
        source_gsheet_id = source_gsheet_id,
        source_gsheet_sheetname = source_gsheet_sheetname,
        source_object = source_object
      )
    }

  )
)



