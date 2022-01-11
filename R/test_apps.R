#' function generating testing app with all possibilites from raw files
#' @import shiny
#' @noRd
#' 
testthat_raw_app <- function() {
  
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
        use_modal = F
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
      gender_react = quetzio_server$new(
        source_method = "raw",
        source_object = shiny.quetzio::quetzio_examples$questions_lists$gender_update,
        module_id = "gender_react",
        custom_txts = list(submit_enabled = "All is done!")
      ),
      link_id = "second_link"
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