library(shiny)
library(shiny.quetzio)

ui <- fluidPage(
  column(6,
         quetzio_link_UI("first_link"),
         tags$hr(),
         actionButton("update_values",
                      "Update the values")
         ),
  column(6, 
         quetzio_link_UI("second_link")
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
      source_object = quetzio_examples$questions_lists$gender_1_item,
      module_id = "gender_item",
      use_modal = F
    ),
    simple_quetzio = quetzio_server$new(
      source_method = "raw",
      source_object = quetzio_examples$questions_lists$simple_quetzio,
      desc_object = quetzio_examples$description_lists$simple_quetzio,
      module_id = "first_simple",
      use_modal = F
    ),
    link_id = "first_link"
  )
  
  # second questionnaire to react on changes of first questionnaire
  
  quetzio_to_update <- quetzio_link_server$new(
    simple_quetzio = quetzio_server$new(
      source_method = "raw",
      source_object = quetzio_examples$questions_lists$simple_quetzio,
      desc_object = quetzio_examples$description_lists$simple_quetzio,
      module_id = "seconnd_simple",
      use_modal = F
    ),
    with_default = quetzio_server$new(
      source_method = "yaml",
      source_yaml = "items_to_default.yaml",
      source_yaml_default = "default_config.yaml",
      module_id = "with_default",
      randomize_order = TRUE,
      use_modal = F
    ),
    gender_react = quetzio_server$new(
      source_method = "raw",
      source_object = quetzio_examples$questions_lists$gender_update,
      module_id = "gender_react",
      use_modal = F
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
    source_object = quetzio_examples$label_update$gender_update
  )
  
  # values update observer
  
  observeEvent(input$update_values, {
    req(quetzio_w_gender$completion() == 1)
    quetzio_to_update$update_values(
      quetzio_name = "simple_quetzio",
      values = quetzio_w_gender$answers()$simple_quetzio
    )
  })
  
}

shinyApp(ui, server)