#' Create UI of your questionnaire
#'
#' @param module_id Character string holding ID for the module. Needs to be the
#' same as one provided for 'quetzio_server'
#' @import shiny
#' @export
#' @seealso quetzio_server

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
#' @seealso quetzio_UI
#'
#' @import R6
#' @import shiny
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
    render_ui = NULL,
    language = NULL,
    use_modal = NULL,
    custom_txts = NULL

  ),

  public = list(

    #' @field source_list List containing the data for all the inputs
    source_list = NULL,

    #' @field description List containing survey instruction and description
    description = NULL,

    #' @field div_id ID of the div containing the survey UI
    div_id = NULL,

    #' @field module_id ID of the shiny module
    module_id = NULL,

    #' @field is_done logical reactiveVal indicating if the survey has been completed
    is_done = NULL,

    #' @field message reactiveVal catching any messages from object
    message = NULL,

    #' @field answers reactiveVal object containing list with answers to questions
    answers = NULL,
    
    #' @field order Indices of questions in order of their appearance, if you
    #' wished to randomize their order. Otherwise NULL
    order = NULL,

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
    #' @param source_yaml_default path to the optional default options for items
    #' generated with source list. Only when `source_method == 'yaml'` or
    #' `source_method == 'raw'` and source object of class `list` is povided..
    #' @param source_gsheet_id id of the source googlesheet file
    #' @param source_gsheet_sheetname name of the source spreadsheet
    #' @param source_object object of class `list` (similiar in structure to
    #' 'yaml' source) or `data.frame` (similiar in structure to 'googlesheet'
    #' source) to be the source of questions. You can create a sample data.frame
    #' with \code{create_survey_source()}. Needed when `source_method == 'raw'`
    #' @param source_object_default list containing optional default options for
    #' items generated with source list. Only when `source_method == 'yaml'` or
    #' `source_method == 'raw'` and source object of class `list` is povided.
    #' @param desc_yaml path to the optional instruction and item
    #' descriptions.
    #' @param desc_gsheet_id id of the googlesheet to provide optional instruction
    #' and item descriptions. Defaults to 'source_gsheet_id', if not provided.
    #' @param desc_gsheet_sheetname name of source for optional instruction and
    #' item descriptions.
    #' @param desc_object object of class `list` or `data.frame` to be the source
    #' of optional instruction and item descriptions.
    #' @param randomize_order logical: do you wish to randomize order in which the
    #' items will appear? Defaults to FALSE
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
    #' \item(quetzio_submit = "color: #fff; background-color: #337ab7; border-color: #2e6da4; width: 200px;"}
    #' \item{quetzio_description = "font-size: 0.9em;"}
    #' }
    #' You can also add styles for different classes contained within the div
    #' of the questionnaire - styles will be affecting only elements within
    #' this specific questionnaire.
    #' @param lang language to use. For now only 'en' and 'pl' are supported.
    #' @param custom_txts named list with custom labels for specified language.
    #' For more information look upon documentation for 'quetzio_txt'
    #' @param use_modal logical indicating if modalDialog for invalid inputs
    #' should be triggered. Defaults to TRUE
    #' @param render_ui logical indicating if the UI for questionnaire should be
    #' rendered
    #' @param link_id character specifying the 'link_id' of the 'quetzio_link_server'
    #' object, modifying its namespace. Only used internally, if the questionnaire
    #' is part of linked server. Don't set it manually!
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
    #' There are also some optional functionalities, that can be used with sources.
    #'
    #' - optional instructions and item descriptions - they are generated only
    #' if one of the following is provided:
    #'    - `desc_yaml`: rendering from YAML file
    #'    - `desc_gsheet_sheetname`: rendering from googlesheet. If the `source_method`
    #'    isn't `gsheet` or the 'googlesheet_id' containing description is different
    #'    from source, the `desc_gsheet_id` need to be provided too
    #'    - `desc_object`: rendering from R object of classes 'data.frame' or 'list'
    #' - optional default configuration - it is used for a type of shinyInput.
    #' Need to provide `source_yaml_default` - there are no other methods ATM.
    #'
    #' @examples
    #'
    #' ## Only run example in interactive R sessions
    #'
    #'if (interactive()) {
    #'
    #'  # load libraries
    #'  library(shiny)
    #'  library(shiny.quetzio)
    #'
    #'  # create ui
    #'  ui <- fluidPage(
    #'    column(6, align = "center",
    #'           # bind the UI with correct module_id
    #'           quetzio_UI("my_quetzio")
    #'    ),
    #'    column(6,
    #'           h2("State of", tags$i("quetzio_server")),
    #'           h3("Is it done?"),
    #'           verbatimTextOutput("quetzio_is_done"),
    #'           h3("Error messages?"),
    #'           verbatimTextOutput("quetzio_message"),
    #'           h3("Answers"),
    #'           verbatimTextOutput("quetzio_answers")
    #'    )
    #'  )
    #'
    #'  server <- function(input, output, session) {
    #'
    #'    # initialize new quetzio
    #'    questionnaire <- quetzio_server$new(
    #'      # load questions from R object
    #'      source_method = "raw",
    #'      source_object = quetzio_examples$questions_lists$simple_quetzio,
    #'      # optionally add descriptions
    #'      desc_object = quetzio_examples$description_lists$simple_quetzio,
    #'      # use the same module_id as in UI binding
    #'      module_id = "my_quetzio",
    #'      # custom_css to give margin but not center options explicitly
    #'      # it will affect only elements within the form div
    #'      custom_css = list(
    #'        "shiny-options-group" = "text-align: left; margin-left: 45%"
    #'      ),
    #'      # you can also optionally give div unique id - useful for external styling
    #'      div_id = "my_questio_div_id"
    #'    )
    #'
    #'    # render objects to show your questionnaire status
    #'  output$quetzio_is_done <-
    #'      renderPrint(questionnaire$is_done())
    #'    output$quetzio_message <-
    #'      renderPrint(questionnaire$message())
    #'    output$quetzio_answers <-
    #'      renderPrint(questionnaire$answers())
    #'  }
    #'
    #'  shinyApp(ui, server)
    #'
    #'}
    #'
    #' @return the 'quetzio_server' serverModule

    initialize = function(
      source_method,
      source_yaml = NULL,
      source_yaml_default = NULL,
      source_gsheet_id = NULL,
      source_gsheet_sheetname = NULL,
      source_object = NULL,
      source_object_default = NULL,
      output_gsheet = FALSE,
      output_gsheet_id = NULL,
      output_gsheet_sheetname = NULL,
      desc_yaml = NULL,
      desc_gsheet_id = NULL,
      desc_gsheet_sheetname = NULL,
      desc_object = NULL,
      randomize_order = FALSE,
      module_id = NULL,
      div_id = NULL,
      custom_css = NULL,
      lang = "en",
      custom_txts = NULL,
      use_modal = TRUE,
      render_ui = TRUE,
      link_id = NULL
    ){

      # nocov start
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
      
      # nocov end
      # save the module id into environment

      self$module_id <- module_id

      if (is.null(link_id))  {
        self$module_ui_id <- self$module_id
      } else {
        self$module_ui_id <- paste(link_id, self$module_id, sep = ns.sep)
      }

      # save the div id if not provided

      if (is.null(div_id)) {
        self$div_id <- paste(self$module_ui_id, "form", sep = "_")
      } else {
        self$div_id <- div_id
      }
      
      # read the file and save resulting list in the environment
      # nocov start

      if (source_method == "gsheet"){

        source_df <- googlesheets4::read_sheet(
          ss = source_gsheet_id,
          sheet = source_gsheet_sheetname,
          col_types = "cclccnnnnccclclc"
        )

        # check df validity
        .check_source_df(source_df)

        source_list <- .df_to_list(
          source_df = source_df
        )

      } else if (source_method == "yaml") {
        source_list <- .yaml_to_list(
          yaml_file = source_yaml)

        # if yaml default is provided, populate source list
        if (!is.null(source_yaml_default)) {
          source_list <- .populate_from_default(
            source_list,
            yaml::read_yaml(source_yaml_default)
          )
          # if object default is provided, populate source list
        } else if (!is.null(source_object_default)) {
          source_list <- .populate_from_default(
            source_list,
            source_object_default
          )
        }

        # check list validity
        .check_source_list(source_list)


      } else if (source_method == "raw") {

        if (class(source_object) == "data.frame") {

          # checks if df is valid
          .check_source_df(source_object)
          source_list <- .df_to_list(source_object)

        } else if (class(source_object) == "list") {
          
          # if yaml default is provided, populate source list
          if (!is.null(source_yaml_default)) {
            source_list <- .populate_from_default(
              source_object,
              yaml::read_yaml(source_yaml_default)
            )
            # if object default is provided, populate source list
          } else if (!is.null(source_object_default)) {
            source_list <- .populate_from_default(
              source_object,
              source_object_default
            )
          } else {
            source_list <- source_object
          }

          # checks if list is valid
          
          .check_source_list(source_list)

        } else {
          stop("Source object needs to be of class 'data.frame' or 'list'")
        }

      } else {
        stop("Error - problems with source")
      }
      
      # nocov end
      
      # save source list into the environment
      
      self$source_list <- source_list
      
      # optional randomization of source object
      
      if (isTRUE(randomize_order)) {
        
        randomized <- .randomize_source(self$source_list)
        self$source_list <- randomized$source_list
        self$order <- randomized$order
        
      }

      # load description list
      if (!is.null(desc_yaml)) {
        # from YAML
        .check_package("yaml")
        self$description <- yaml::read_yaml(desc_yaml)
      } else if (!is.null(desc_gsheet_sheetname)) {
        # from googlesheet
        .check_package("googlesheets4")
        desc_from_gsheet <- googlesheets4::read_sheet(
          ss = if (is.null(desc_gsheet_id)) source_gsheet_id else desc_gsheet_id,
          sheet = desc_gsheet_sheetname
        )
        # turn into the list
        self$description <- .df_to_list(desc_from_gsheet, type = "quetzio_desc")
      } else if (!is.null(desc_object)) {
        if (class(desc_object) == "data.frame")
          self$description <- .df_to_list(desc_object, type = "quetzio_desc")
        else if (class(desc_object) == "list")
          self$description <- desc_object
        else stop(call. = F, "'desc_object', if provided needs to be of classes 'data.frame' or 'list'") # nocov
      }

      # check for mandatory and numeric inputs

      private$mandatory_items <- .get_mandatory(self$source_list)
      private$numeric_items <- .get_type(self$source_list, "numericInput")

      # save other information in private

      private$custom_txts <- custom_txts
      private$output_gsheet <- output_gsheet
      if(output_gsheet) {
        private$output_ss <- if (is.null(output_gsheet_id) || is.na(output_gsheet_id)) source_gsheet_id else output_gsheet_id
        private$output_sheet <- output_gsheet_sheetname
      }
      private$language <- lang
      private$use_modal <- use_modal

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
    #' @examples
    #' ## only run examples in interactive environment
    #'
    #'if (interactive()) {
    #'
    #'  library(shiny)
    #'  library(shiny.quetzio)
    #'
    #'  ui <- fluidPage(
    #'    # some input to trigger label update
    #'    selectizeInput("gender", "What is your gender?",
    #'                   choices = c("Male" = "M",
    #'                               "Female" = "F",
    #'                               "I identify as neither of above" = "O",
    #'                               "Prefer not to say" = "NI"),
    #'                   selected = "NI"),
    #'    tags$hr(),
    #'    # quetzio to update labels
    #'    quetzio_UI("updating_labels")
    #'  )
    #'
    #'  server <- function(input, output, session) {
    #'
    #'    quetzio <- quetzio_server$new(
    #'      source_method = "raw",
    #'      source_object = quetzio_examples$questions_lists$gender_update,
    #'      module_id = "updating_labels"
    #'    )
    #'
    #'    # trigger need to be reactive
    #'    gender_react <- reactive(input$gender)
    #'
    #'    # update labels method call
    #'    quetzio$update_labels(
    #'      trigger = gender_react,
    #'      source_method = "raw",
    #'      source_object = quetzio_examples$label_update$gender_update
    #'    )
    #'  }
    #'  shinyApp(ui, server)
    #'}
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
        private = private,
        trigger = trigger,
        source_method = source_method,
        source_yaml = source_yaml,
        source_gsheet_id = source_gsheet_id,
        source_gsheet_sheetname = source_gsheet_sheetname,
        source_object = source_object
      )
    },

    #' @description Method to update selected values on the change in reactive
    #'
    #' @param values list of values to update questionnaire with. List needs to be named,
    #' as the names are going to be used to identify which inputId to update
    #'
    #' @examples
    #' ## only run examples in interactive environment
    #'
    #'if (interactive()) {
    #'
    #'  library(shiny)
    #'  library(shiny.quetzio)
    #'
    #'  ui <- fluidPage(
    #'    # first questionnaire to get values from
    #'    column(6,
    #'           h1("Finish first questionnaire"),
    #'           quetzio_UI("first_questionnaire")
    #'    ),
    #'    # second questionnaire to update values
    #'    column(6,
    #'           h1("Update values of second questionnaire!"),
    #'           actionButton("update_vals", "Update values"),
    #'           tags$hr(),
    #'           quetzio_UI("second_questionnaire")
    #'    )
    #'  )
    #'
    #'  server <- function(input, output, session) {
    #'
    #'    quetzio_1st <- quetzio_server$new(
    #'      source_method = "raw",
    #'      source_object = quetzio_examples$questions_lists$simple_quetzio,
    #'      module_id = "first_questionnaire"
    #'    )
    #'    quetzio_2nd <- quetzio_server$new(
    #'      source_method = "raw",
    #'      source_object = quetzio_examples$questions_lists$simple_quetzio,
    #'      module_id = "second_questionnaire"
    #'    )
    #'
    #'    # update values on button press
    #'    observeEvent(input$update_vals, {
    #'      # you can use answers from one questionnaire to update another, though
    #'      # the used values can be any other static named list
    #'      quetzio_2nd$update_values(quetzio_1st$answers())
    #'    })
    #'  }
    #'  shinyApp(ui, server)
    #'}
    #'
    #'

    update_values = function(
      values
    ){
      .quetzio_value_update(self, values)
    }
  )
)

