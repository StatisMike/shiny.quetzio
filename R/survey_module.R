#' Create UI of your questionnaire
#'
#' @import shiny
#' @export
#'

quetzio_UI <- function(module_id) {
  ns <- NS(module_id)

  uiOutput(ns("quetzio_UI"))

}

#' Create R6-based server module to generate and hold the state of your
#' questionnaire
#'
#' @import R6
#' @import googlesheets4
#' @import yaml
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
    output_gsheet = NULL,
    output_ss = NULL,
    output_sheet = NULL,
    css = NULL,
    render_ui = NULL,
    server = function() {

      output <- .survey_backend(
        id = self$module_id,
        source_list = self$source_list,
        mandatory_items = private$mandatory_items,
        numeric_items = private$numeric_items,
        output_gsheet = private$output_gsheet,
        output_ss = private$output_ss,
        output_sheet = private$output_sheet,
        button_labels = self$button_labels,
        div_id = self$div_id,
        css = private$css,
        render_ui = private$render_ui
      )

      observe({
        req(output)

        self$is_done(reactiveValuesToList(output)$is_done)
        self$message(reactiveValuesToList(output)$message)
        self$answers(reactiveValuesToList(output)$answers)

      })
    }

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

    #' @description Initializing the shiny.survey_module object
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
    #' If not specified, default look will be used
    #' @param button_labels character vector of length two with labels for submit
    #' button in active and disabled state
    #' @param render_ui logical indicating if the UI for questionnaire should be
    #' rendered
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
    #' @return the 'survey_module_server' object

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
      button_labels = c("Submit", "Cannot submit"),
      render_ui = TRUE
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
        if ((is.null(source_gsheet_id) && is.null(output_gsheet_id)) || is.null(output_gsheet_sheetname)){
          stop("When 'output_gsheet' == TRUE, you need to specify 'output_gsheet_id' (if other from 'source_gsheet_id') and 'output_gsheet_sheetname'")
        }
      }

      # save the module id into environment

      self$module_id <- .null_def(module_id, uuid::UUIDgenerate())

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
      private$numeric_items <- .get_numeric(self$source_list)

      # save other information in private

      self$div_id <- div_id
      self$button_labels <- button_labels
      private$output_gsheet <- output_gsheet
      if(output_gsheet) {
        private$output_ss <- if (is.null(output_gsheet_id) || is.na(output_gsheet_id)) source_gsheet_id else output_gsheet_id
        private$output_sheet <- output_gsheet_sheetname
      }
      private$css <- .null_def(
        custom_css,
        ".mandatory_star { color: red; } .invalid_input { outline: red; outline-style: dashed; outline-offset: 10px; }"
      )

      self$is_done <- reactiveVal(FALSE)
      self$message <- reactiveVal()
      self$answers <- reactiveVal()
      private$render_ui <- reactiveVal(render_ui)

      private$server()

    }
  )
)



