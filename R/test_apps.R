#' function generating testing app with all possibilites from raw objects and
#' deauthorized googlesheet. 
#' 
#' For now handles the googlesheets saving possibilities too
#' 
#' @import shiny
#' @noRd
#' 
testthat_raw_app <- function() {
  
  googlesheets4::gs4_auth(email = Sys.getenv("G_SERVICE_MAIL"),
                          path = Sys.getenv("G_SERVICE_ACCOUNT"),
                          cache = F)
  googledrive::drive_auth(email = Sys.getenv("G_SERVICE_MAIL"),
                          path = Sys.getenv("G_SERVICE_ACCOUNT"),
                          cache = F)
  
  ui <- fluidPage(
    column(6,
           quetzio_link_UI("first_link"),
           tags$hr(),
           actionButton("update_values",
                        "Update the values"),
           tags$hr(),
           shinyjs::disabled(
             actionButton("get_df_first",
                          "Get values of one quetzio")),
           tags$hr(),
           verbatimTextOutput("df_first")
    ),
    column(6, 
           quetzio_link_UI("second_link"),
           tags$hr(),
           shinyjs::disabled(
             actionButton("get_df_second",
                          "Get values of second link")),
           tags$hr(),
           verbatimTextOutput("df_second")
    )
  )
  
  server <- function(input, output, session) {
    
    # seed saved for random order reproducibility
    
    set.seed(2137)
    
    # all quetzios have option 'use_modal' set to F, as to date there is a problem
    # in recording modal closing
    
    # first link contains gender item, which specifies the values for update
    # labels for last questionnaire of second link
    # also the simple_quetzio can update values of the questionnaire on
    # second questionnaire
    
    quetzio_w_gender <- quetzio_link_server$new(
      gender_item = quetzio_server$new(
        source_method = "raw",
        source_object = shiny.quetzio::quetzio_examples$questions_lists$gender_1_item,
        module_id = "gender_item",
        use_modal = F
      ),
      simple_quetzio = quetzio_server$new(
        source_method = "raw",
        source_object = shiny.quetzio::quetzio_examples$questions_lists$simple_quetzio,
        desc_object = shiny.quetzio::quetzio_examples$description_lists$simple_quetzio,
        module_id = "first_simple",
        use_modal = F,
        output_gsheet = TRUE,
        output_gsheet_id = Sys.getenv("QUETZIO_SS"),
        output_gsheet_sheetname = "Answers_testthat_quetzio"
      ),
      link_id = "first_link"
    )
    
    # second questionnaire to react on changes of first questionnaire
    
    quetzio_to_update <- quetzio_link_server$new(
      simple_quetzio = quetzio_server$new(
        source_method = "raw",
        source_object = shiny.quetzio::quetzio_examples$questions_lists$simple_quetzio,
        desc_object = shiny.quetzio::quetzio_examples$description_lists$simple_quetzio,
        module_id = "seconnd_simple",
        use_modal = F
      ),
      with_default = quetzio_server$new(
        source_method = "raw",
        source_object = shiny.quetzio::quetzio_examples$questions_lists$simple_default,
        source_object_default = shiny.quetzio::quetzio_examples$default_config$simple_default,
        module_id = "with_default",
        randomize_order = TRUE,
        use_modal = F
      ),
      from_googlesheet = quetzio_server$new(
        source_method = "gsheet",
        source_gsheet_id = Sys.getenv("QUETZIO_SS"),
        source_gsheet_sheetname = "Questions",
        desc_gsheet_sheetname = "Description",
        module_id = "from_gsheet",
        use_modal = F
      ),
      gender_react = quetzio_server$new(
        source_method = "raw",
        source_object = shiny.quetzio::quetzio_examples$questions_lists$gender_update,
        module_id = "gender_react",
        custom_txts = list(submit_enabled = "All is done!")
      ),
      link_id = "second_link",
      output_gsheet = TRUE,
      output_gsheet_id = Sys.getenv("QUETZIO_SS"),
      output_gsheet_sheetname = "Answers_testthat_link"
    )
    
    # label update trigger
    
    gender_trigger <- reactive(
      quetzio_w_gender$answers()$gender_item$gender_item
    )
    
    # label update method
    
    quetzio_to_update$update_labels(
      quetzio_name = "gender_react",
      trigger = gender_trigger,
      source_method = "raw",
      source_object = shiny.quetzio::quetzio_examples$label_update$gender_update
    )
    
    # values update observer
    
    observeEvent(input$update_values, {
      req(quetzio_w_gender$completion() == 1)
      quetzio_to_update$update_values(
        quetzio_name = "simple_quetzio",
        values = quetzio_w_gender$answers()$simple_quetzio
      )
    })
    
    # trigger buttons on completion
    observe({
      req(quetzio_w_gender$completion())
      if(quetzio_w_gender$completion() == 1) {
        shinyjs::enable(id = "get_df_first")
      }
    })
    
    observe({
      req(quetzio_to_update$completion())
      if(quetzio_to_update$completion() == 1) {
        shinyjs::enable(id = "get_df_second")
      }
    })
    
    # generate answers as data.frame
    observeEvent(input$get_df_first, {
      output$df_first <- renderPrint(
        dplyr::select(
          quetzio_w_gender$quetzio_list$simple_quetzio$get_answers_df(),
          -".timestamp"))
    })
    
    observeEvent(input$get_df_second, {
      output$df_second <- renderPrint(
        dplyr::select(
          quetzio_to_update$get_answers_df(),
          -ends_with(".timestamp")))
    })
    
  }
  
  shinyApp(ui, server)
  
}

#' function creating app with likert inputs
#' @import shiny
#' @noRd

testthat_likert_app <- function() {
  
  library(shiny)
  library(shiny.quetzio)
  
  likert_sources <- 
    list(defaults = list(likertRadioButtons = list(mandatory = TRUE, 
                                                 choiceNames = c("strongly disagree", "disagree", "neutral (neither agree nor disagree)", 
                                                                 "agree", "strongly agree"), choiceValues = 1:5)), 
         source1 = list(HEX_1 = list(type = "likertRadioButtons", label = "I sometimes can't help worrying about little things."), 
                        HEX_2 = list(type = "likertRadioButtons", label = "If I knew that I could never get caught, I would be willing to steal a million dollars."), 
                        HEX_3 = list(type = "likertRadioButtons", label = "I would enjoy creating a work of art, such as a novel, a song, or a painting."), 
                        HEX_4 = list(type = "likertRadioButtons", label = "When working on something, I don't pay much attention to small details."), 
                        HEX_5 = list(type = "likertRadioButtons", label = "People sometimes tell me that I'm too stubborn.")), 
       source2 = list(HEX_1 = list(type = "likertRadioButtons", 
                                   label = "I would be quite bored by a visit to an art gallery."), 
                      HEX_2 = list(type = "likertRadioButtons", label = "I plan ahead and organize things, to avoid scrambling at the last minute."), 
                      HEX_3 = list(type = "likertRadioButtons", label = "I rarely hold a grudge, even against people who have badly wronged me."), 
                      HEX_4 = list(type = "likertRadioButtons", label = "I feel reasonably satisfied with myself overall."), 
                      HEX_5 = list(type = "likertRadioButtons", label = "I would feel afraid if I had to travel in bad weather conditions.")))
  
  ui <- fluidPage(
    
    quetzio_link_UI("HEXAll")
    
  )
  
  server <- function(input, output, session) {
    
    quetzio_link_server$new(
      "toFive" = quetzio_server$new(
        source_method = "raw",
        source_object = likert_sources$source1,
        source_object_default = likert_sources$defaults,
        module_id = "HEXtoTen"
      ),
      "toTen" = quetzio_server$new(
        source_method = "raw",
        source_object = likert_sources$source2,
        source_object_default = likert_sources$defaults,
        module_id = "HEXtoTwenty"
      ),
      link_id = "HEXAll"
    )
  }
  
  shinyApp(ui, server)
  
  
}
