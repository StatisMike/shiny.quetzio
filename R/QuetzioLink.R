#' Create UI for linked questionnaires
#'
#' @param link_id Character string holding ID for the link module. Needs to be the
#' same as one provided in 'QuetzioLink'
#'
#' @import shiny
#' @export
#' @seealso QuetzioLink
#'

QuetzioLink_UI <- function(link_id) {

  ns <- NS(link_id)
    uiOutput(ns("QuetzioLink_UI"))
}


#' @title Quetzio Link Server class
#' @docType class
#'
#' @description Create R6-based server module to generate and hold the state of
#' multiple linked 'Quetzio' objects
#'
#' @import R6
#' @import shiny
#' @seealso Quetzio
#' @seealso QuetzioLink_UI
#' @export
#'


QuetzioLink <- R6::R6Class(
  "QuetzioLink",
  private = list(

    # names of the linked survey questionnaires
    quetzio_names = NULL,
    # googlesheets specification
    output_gsheet = FALSE,
    output_gsheet_id = NULL,
    output_gsheet_sheetname = NULL,
    # optional id to append
    questionee_id = NULL

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

    #' @field quetzio_list reactiveValues holding the linked 'Quetzio's
    quetzio_list = NULL,


    #' @description Initialization of the 'QuetzioLink' object
    #'
    #' @param ... 'Quetzio' objects that are going to be connected by the
    #' link. They are all needed to be named - with the names providing the path
    #' to the answers of their questions.
    #' Moreover, they need to be in initialized state: \code{Quetzio$new(...)}
    #' @param link_id character specifying the module ID of created object. Needs
    #' to be identical to the one specified inside every 'Quetzio' objects
    #' provided to the object.
    #' @param output_gsheet logical: do you wish to save the answers automatically
    #' to the googlesheet. If TRUE, the 'output_gsheet_id' and 'output_gsheet_sheetname'
    #' arguments need to be specified. Defaults to FALSE
    #' @param output_gsheet_id id of the output googlesheet file
    #' @param output_gsheet_sheetname name of the output spreadsheet
    #' @param questionee_id reactive object containing ID of the questionee to 
    #' append to the answers list during its retrieval with `Quetzio_get_df` or sending
    #' to googlesheets. Optional.
    #' 
    #' @examples
    #'
    #' ## Only run example in interactive R sessions
    #'
    #'if (interactive()) {
    #'
    #'  #### create shinyApp
    #'
    #'  # load libraries
    #'  library(shiny)
    #'  library(shiny.quetzio)
    #'
    #'  # create ui
    #'  ui <- fluidPage(
    #'    column(6, align = "center",
    #'           # bind the UI with correct link_id
    #'           QuetzioLink_UI("my_quetzio_link")
    #'    ),
    #'    column(6,
    #'           h2("State of", tags$i("QuetzioLink")),
    #'           h3("Completion rate"),
    #'           verbatimTextOutput("quetzio_completion"),
    #'           h3("Error messages?"),
    #'           verbatimTextOutput("quetzio_message"),
    #'           h3("Answers"),
    #'           verbatimTextOutput("quetzio_answers")
    #'    )
    #'  )
    #'
    #'  server <- function(input, output, session) {
    #'
    #'    # initialize new quetzio link
    #'    linked_questionnaires <- QuetzioLink$new(
    #'      # initialize Quetzios within it (give them names!)
    #'      quetzio_first = Quetzio$new(
    #'        source_method = "raw",
    #'        source_object = quetzio_examples$questions_lists$link_quetzio_1,
    #'        desc_object = quetzio_examples$description_lists$link_quetzio_1,
    #'        module_id = "my_first_quetzio",
    #'      custom_css = list(
    #'          "quetzio_list" = "text-align: left; margin-left: 35%;"
    #'      )
    #'      ),
    #'      quetzio_second = Quetzio$new(
    #'        source_method = "raw",
    #'        source_object = quetzio_examples$questions_lists$link_quetzio_2,
    #'        desc_object = quetzio_examples$description_lists$link_quetzio_2,
    #'        module_id = "my_second_quetzio",
    #'        custom_css = list(
    #'          "shiny-options-group" = "text-align: left; margin-left: 45%;"
    #'        )
    #'      ),
    #'      quetzio_third = Quetzio$new(
    #'        source_method = "raw",
    #'        source_object = quetzio_examples$questions_lists$link_quetzio_3,
    #'        desc_object = quetzio_examples$description_lists$link_quetzio_3,
    #'        module_id = "my_thirdt_quetzio"
    #'      ),
    #'      link_id = "my_quetzio_link"
    #'    )
    #'
    #'    # render objects from quetzio_link
    #'    output$quetzio_completion <-
    #'      renderPrint(linked_questionnaires$completion())
    #'    output$quetzio_message <-
    #'      renderPrint(linked_questionnaires$message())
    #'    output$quetzio_answers <-
    #'      renderPrint(linked_questionnaires$answers())
    #'  }
    #'
    #'  shinyApp(ui, server)
    #'
    #'}
    #'
    initialize = function(
      ...,
      link_id,
      output_gsheet = FALSE,
      output_gsheet_id = NULL,
      output_gsheet_sheetname = NULL,
      questionee_id = NULL
    ) {

      self$link_id <- link_id

      # catching the names of the provided Quetzio objects
      args <- match.call(expand.dots = FALSE)
      private$quetzio_names <- names(args$...)
      
      # get the questionee ID if provided
      if (!is.null(questionee_id)) {
        if ("reactive" %in% class(questionee_id)) {
          private$questionee_id <- questionee_id 
        } else{
          stop("Object provided to `questionee_id` should be a reactive")
        }
      }

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
      } else {
        private$output_gsheet <- FALSE
      }

      # call to the moduleServer handling all the logic

      .link_backend(self, private, uneval)

    },

    #' @description method to get preprocessed answers in the form of dataframe
    #' (only if the questionnaire is done)
    #' @return data.frame

    get_answers_df = function() {

      if (isTRUE(self$completion() == 1)) {
        df_answers <- .merge_linked_answers_to_df(self$answers(), private$quetzio_names)
        # if questionee id is present, append it to answers
        if (!is.null(private$questionee_id)) {
          df_answers <- cbind(
            data.frame(`.id` = private$questionee_id()),
            df_answers
          )
        }
        return(df_answers)
        
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
    #'    QuetzioLink_UI("labels_link")
    #'  )
    #'
    #'  server <- function(input, output, session) {
    #'
    #'    quetzio_link <- QuetzioLink$new(
    #'      gender = Quetzio$new(
    #'        source_method = "raw",
    #'        source_object = quetzio_examples$questions_lists$gender_update,
    #'        module_id = "updating_labels"
    #'      ),
    #'      quetzio_2nd = Quetzio$new(
    #'        source_method = "raw",
    #'        source_object = quetzio_examples$questions_lists$simple_quetzio,
    #'        module_id = "second_in_link"
    #'      ),
    #'      link_id = "labels_link")
    #'
    #'    # trigger need to be reactive
    #'    gender_react <- reactive(input$gender)
    #'
    #'    # update labels method call
    #'    quetzio_link$update_labels(
    #'      # you need to provide the name of the Quetzio in link
    #'      # where you need to update labels
    #'      quetzio_name = "gender",
    #'      # the trigger needs to be reactive, but without the parentheses
    #'      trigger = gender_react,
    #'      source_method = "raw",
    #'      source_object = quetzio_examples$label_update$gender_update
    #'    )
    #'  }
    #'  shinyApp(ui, server)
    #'}
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
    #' @param values list of values to update questionnaire with. List needs to be named,
    #' as the names are going to be used to identify which inputId to update
    #'
    #' @examples
    #'
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
    #'           Quetzio_UI("first_questionnaire")
    #'    ),
    #'    # quetzio link to update values
    #'    column(6,
    #'           h1("Update values of quetzio link!"),
    #'           actionButton("update_vals", "Update values"),
    #'           tags$hr(),
    #'           QuetzioLink_UI("updating_link")
    #'    )
    #'  )
    #'
    #'  server <- function(input, output, session) {
    #'
    #'    quetzio_1st <- Quetzio$new(
    #'      source_method = "raw",
    #'      source_object = quetzio_examples$questions_lists$simple_quetzio,
    #'      module_id = "first_questionnaire"
    #'    )
    #'
    #'    quetzio_link <- QuetzioLink$new(
    #'      value_update = Quetzio$new(
    #'        source_method = "raw",
    #'        source_object = quetzio_examples$questions_lists$simple_quetzio,
    #'        module_id = "first_in_link"
    #'      ),
    #'      another_one = Quetzio$new(
    #'        source_method = "raw",
    #'        source_object = quetzio_examples$questions_lists$link_quetzio_2,
    #'        module_id = "second_in_link"
    #'      ),
    #'      link_id = "updating_link"
    #'    )
    #'    # update values on button press
    #'    observeEvent(input$update_vals, {
    #'      quetzio_link$update_values(
    #'        # you need to provide quetzio name in the link to update
    #'        quetzio_name = "value_update",
    #'        # you can use answers from one questionnaire to update another, though
    #'        # the used values can be any other static named list
    #'        values = quetzio_1st$answers()
    #'      )
    #'    })
    #'  }
    #'  shinyApp(ui, server)
    #'}
    #'

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


#' Create linked Quetzio
#' 
#' @param ... 'Quetzio' objects that are going to be connected by the
#' link. They are all needed to be named - with the names providing the path
#' to the answers of their questions.
#' Moreover, they need to be in initialized state: \code{Quetzio$new(...)}
#' @param link_id character specifying the module ID of created object. Needs
#' to be identical to the one specified inside every 'Quetzio' objects
#' provided to the object.
#' @param output_gsheet logical: do you wish to save the answers automatically
#' to the googlesheet. If TRUE, the 'output_gsheet_id' and 'output_gsheet_sheetname'
#' arguments need to be specified. Defaults to FALSE
#' @param output_gsheet_id id of the output googlesheet file
#' @param output_gsheet_sheetname name of the output spreadsheet
#' @param questionee_id reactive object containing ID of the questionee to 
#' append to the answers list during its retrieval with `Quetzio_get_df` or sending
#' to googlesheets. Optional.
#'
#' @return QuetzioLink object
#' @example inst/examples/QuetzioLink_create.R
#' @export
#' 

QuetzioLink_create <- function(
  ...,
  link_id,
  output_gsheet = FALSE,
  output_gsheet_id = NULL,
  output_gsheet_sheetname = NULL,
  questionee_id = NULL
) {
  
  return(
    QuetzioLink$new(
      ...,
      link_id = link_id,
      output_gsheet = output_gsheet,
      output_gsheet_id = output_gsheet_id,
      output_gsheet_sheetname = output_gsheet_sheetname,
      questionee_id = questionee_id
    )
  )
}
