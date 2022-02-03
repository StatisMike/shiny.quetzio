
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
    # second questionnaire to update values
    column(6,
           h1("Update values of second questionnaire!"),
           actionButton("update_vals", "Update values"),
           tags$hr(),
           Quetzio_UI("second_questionnaire")
    )
  )
  
  server <- function(input, output, session) {
    
    quetzio_1st <- Quetzio_create(
      source_method = "raw",
      source_object = quetzio_examples$questions_lists$simple_quetzio,
      module_id = "first_questionnaire"
    )
    quetzio_2nd <- Quetzio_create(
      source_method = "raw",
      source_object = quetzio_examples$questions_lists$simple_quetzio,
      module_id = "second_questionnaire"
    )
    
    # update values on button press
    observeEvent(input$update_vals, {
      Quetzio_value_update(
        Quetzio = quetzio_2nd,
        # you can use answers from one questionnaire to update another, though
        # the used values can be any other static named list
        values = quetzio_1st$answers()
      )
    })
    
  }
  shinyApp(ui, server)
}
