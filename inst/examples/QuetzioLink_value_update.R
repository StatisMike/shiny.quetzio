
## only run examples in interactive environment

if (interactive()) {
  
  library(shiny)
  library(shiny.quetzio)
  
  ui <- fluidPage(
    # first questionnaire to get values from
    column(6,
           h1("Finish first questionnaire"),
           Quetzio_UI("first_questionnaire")
    ),
    # quetzio link to update values
    column(6,
           h1("Update values of quetzio link!"),
           actionButton("update_vals", "Update values"),
           tags$hr(),
           QuetzioLink_UI("updating_link")
    )
  )
  
  server <- function(input, output, session) {
    
    quetzio_1st <- Quetzio_create(
      source_method = "raw",
      source_object = quetzio_examples$questions_lists$simple_quetzio,
      module_id = "first_questionnaire"
    )
    
    quetzio_link <- QuetzioLink_create(
      value_update = Quetzio_create(
        source_method = "raw",
        source_object = quetzio_examples$questions_lists$simple_quetzio,
        module_id = "first_in_link"
      ),
      another_one = Quetzio_create(
        source_method = "raw",
        source_object = quetzio_examples$questions_lists$link_quetzio_2,
        module_id = "second_in_link"
      ),
      link_id = "updating_link"
    )
    # update values on button press
    observeEvent(input$update_vals, {
      Quetzio_value_update(
        Quetzio = quetzio_link,
        # you need to provide quetzio name in the link to update
        name = "value_update",
        # you can use answers from one questionnaire to update another, though
        # the used values can be any other static named list
        values = quetzio_1st$answers()
      )
    })
  }
  shinyApp(ui, server)
}
