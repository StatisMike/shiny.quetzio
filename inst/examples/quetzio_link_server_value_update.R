## only run examples in interactive environment

if (interactive()) {

library(shiny)
library(shiny.survey)

ui <- fluidPage(
  # first questionnaire to get values from
  column(6,
         h1("Finish first questionnaire"),
         quetzio_UI("first_questionnaire")
         ),
  # quetzio link to update values
  column(6,
         h1("Update values of quetzio link!"),
         actionButton("update_vals", "Update values"),
         tags$hr(),
         quetzio_link_UI("updating_link")
         )
)

server <- function(input, output, session) {

  quetzio_1st <- quetzio_server$new(
    source_method = "raw",
    source_object = quetzio_examples$questions_lists$simple_quetzio,
    module_id = "first_questionnaire"
  )

  quetzio_link <- quetzio_link_server$new(
    value_update = quetzio_server$new(
      source_method = "raw",
      source_object = quetzio_examples$questions_lists$simple_quetzio,
      module_id = "first_in_link"
    ),
    another_one = quetzio_server$new(
      source_method = "raw",
      source_object = quetzio_examples$questions_lists$link_quetzio_2,
      module_id = "second_in_link"
    ),
    link_id = "updating_link"
  )
  # update values on button press
  observeEvent(input$update_vals, {
    quetzio_link$update_values(
      # you need to provide quetzio name in the link to update
      quetzio_name = "value_update",
      # you can use answers from one questionnaire to update another, though
      # the used values can be any other static named list
      values = quetzio_1st$answers()
      )
  })
}
shinyApp(ui, server)
}
