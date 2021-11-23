#' Create R6-based Shiny modules for the app
#'
#' @name survey_module
#'
#' @import R6
#' @import googlesheets4
#' @import yaml
#' @import shiny
#' @import shinyjs
#' @import dplyr
#' @export
#'

survey_module <- R6::R6Class(
  "shiny.survey_module",

  private = list(

    module_id = NULL,
    source_list = NULL,
    mandatory_items = NULL,
    numeric_items = NULL,
    div_id = NULL,
    output_gsheet = NULL,
    output_ss = NULL,
    output_sheet = NULL,
    css = NULL

  ),

  public = list(

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
    #' If not specified, default look will be used.
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
    #' @return the 'shiny.survey_module' object

    initialize = function(
      source_method,
      source_object,
      source_yaml,
      source_gsheet_id,
      source_gsheet_sheetname,
      output_gsheet = FALSE,
      output_gsheet_id = source_gsheet_id,
      output_gsheet_sheetname,
      module_id = NULL,
      div_id = "form",
      custom_css = NULL

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
        if (is.null(output_gsheet_id) || is.null(output_gsheet_sheetname)){
          stop("When 'output_gsheet' == TRUE, you need to specify 'output_gsheet_id' (if other from 'source_gsheet_id') and 'output_gsheet_sheetname'")
        }

      }

      # save the module id into environment

      private$module_id <- .null_def(module_id, uuid::UUIDgenerate())

      # read the file and save resulting list in the environment

      if (source_method == "gsheet"){

        source_df <- googlesheets4::read_sheet(
          ss = source_gsheet_id,
          sheet = source_gsheet_sheetname,
          col_types = "c"
        # col_types = "cclccnnnnccclclc"
        )

        # check df validity
       .check_source_df(source_df)

        private$source_list <- .df_to_list(
          source_df = source_df
        )

      } else if (source_method == "yaml") {
        source_list <- .yaml_to_list(
          yaml_file = source_yaml)

        # check list validity
        .check_source_list(source_list)

        private$source_list <- source_list


      } else if (source_method == "raw") {

        if (class(source_object) == "data.frame") {

          # checks if df is valid
          .check_source_df(source_object)
          private$source_list <- .df_to_list(source_object)

        } else if (class(source_object) == "list") {

          # checks if list is valid
          .check_source_list(source_object)
          private$source_list <- source_object

        } else {
          stop("Source object needs to be of class 'data.frame' or 'list'")
        }

      } else {
        stop("Error - problems with source")
      }

      # check for mandatory and numeric inputs

      private$mandatory_items <- .get_mandatory(private$source_list)
      private$numeric_items <- .get_numeric(private$source_list)

      # save other information in private

      private$div_id <- div_id
      private$output_gsheet <- output_gsheet
      private$output_ss <- output_gsheet_id
      private$output_sheet <- output_gsheet_sheetname
      private$css <- .null_def(
        custom_css,
        ".mandatory_star { color: red; } .invalid_input { outline: red; outline-style: dashed; outline-offset: 10px; }"
                               )

      },

    #' @description Method to input the UI of your survey into the shinyApp UI
    #' @return The whole UI of your survey

    ui = function() {

      .generate_ui(
        source_list = private$source_list,
        div_id = private$div_id,
        module_id = private$module_id,
        css = private$css
      )

    },

    #' @description Method to input the UI of your survey into the shinyApp UI
    #' @return The reactiveValues holding
    #' /itemize{
    #'   /item{is_done logical vector indicating if the survey has been sent. NA indicates error}
    #'   /item{message if catches error during survey sending, character string with error message. Otherwise NULL }
    #' }

    server = function() {

      .survey_backend(
        id = private$module_id,
        source_list = private$source_list,
        mandatory_items = private$mandatory_items,
        numeric_items = private$numeric_items,
        output_gsheet = private$output_gsheet,
        output_ss = private$output_ss,
        output_sheet = private$output_sheet
      )
    }
  )
)



