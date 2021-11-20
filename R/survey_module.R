#' Create R6-based Shiny modules for the app
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
    output_ss = NULL,
    output_sheet = NULL,
    css = NULL
    
  ),
  
  public = list(
    
    #' @description Initializing the shiny.survey_module object
    #' 
    #' @param source_method character string specifying where to get the source 
    #' config file. Can be either 'gsheet' or 'yaml'. 
    #' @param source_yaml path to the source yaml file
    #' @param source_gsheet_id id of the source googlesheet file
    #' @param source_gsheet_sheetname name of the source spreadsheet
    #' @param module_id character string with unique id for the module. If not
    #' specified, it will be automatically generated
    #' @param div_id character string with unique id for the created div. If not
    #' specified, it will be set to 'form'
    #' @param output_gsheet_id id of the output googlesheet file. If not specified,
    #' the same googlesheet as for 'source' will be used
    #' @param output_gsheet_sheetname name of the output spreadsheet
    #' @param custom_css custom css for classes 'mandatory star' and 'invalid_input'.
    #' If not specified, default look will be used.
    #' @return the 'shiny.survey_module' object
    
    initialize = function(
      source_method,
      source_yaml,
      source_gsheet_id,
      source_gsheet_sheetname,
      module_id = NULL,
      div_id = "form",
      output_gsheet_id = source_gsheet_id,
      output_gsheet_sheetname,
      custom_css = NULL
      
    ){
      # checks if all arguments are provided
      if (source_method == "gsheet" && (is.null(source_gsheet_id) || is.null(source_gsheet_sheetname))) {
        stop("When source_method == 'gsheet', you need to specify 'source_gsheet_id' and 'source_gsheet_sheetname'.")
      } else if (source_method == "yaml" && is.null(source_yaml)) {
        stop("When source_method == 'yaml', you need to specify 'source_yaml'")
      } else if (!source_method %in% c("gsheet", "yaml")) {
        stop("'source_method' must be chosen between 'gsheet' and 'yaml'.")
      }
      
      # save the module id into environment
      
      private$module_id <- .null_def(module_id, uuid::UUIDgenerate())
      
      # read the file and save resulting list in the environment
      
      if (source_method == "gsheet"){
        private$source_list <- .gsheet_to_list(
          ss_id = source_gsheet_id,
          ss_name = source_gsheet_sheetname
        )
      } else {
        private$source_list <- .yaml_to_list(
          yaml_file = source_yaml
        )
      }
      
      # check for mandatory and numeric inputs
      
      private$mandatory_items <- .get_mandatory(private$source_list)
      private$numeric_items <- .get_numeric(private$source_list)
      
      # save other information in private
      
      private$div_id <- div_id
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
        private$module_id,
        private$source_list,
        private$mandatory_items,
        private$numeric_items,
        private$output_ss,
        private$output_sheet
      )
    }
  )
)



