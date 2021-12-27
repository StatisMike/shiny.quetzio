## only run examples in interactive environment

if (interactive()) {

library(shiny)
library(shiny.survey)

ui <- fluidPage(
  # some input to trigger label update
  selectizeInput("gender", "What is your gender?",
    choices = c("Male" = "M",
                "Female" = "F",
                "I identify as neither of above" = "O",
                "Prefer not to say" = "NI"),
    selected = "NI"),
  tags$hr(),
  # quetzio to update labels
  quetzio_link_UI("labels_link")
)

server <- function(input, output, session) {

  quetzio_link <- quetzio_link_server$new(
    gender = quetzio_server$new(
      source_method = "raw",
      source_object = quetzio_examples$questions_lists$gender_update,
      module_id = "updating_labels"
    ),
    quetzio_2nd = quetzio_server$new(
      source_method = "raw",
      source_object = quetzio_examples$questions_lists$simple_quetzio,
      module_id = "second_in_link"
    ),
    link_id = "labels_link")

  # trigger need to be reactive
  gender_react <- reactive(input$gender)

  # update labels method call
  quetzio_link$update_labels(
    # you need to provide the name of the quetzio_server in link
    # where you need to update labels
    quetzio_name = "gender",
    # the trigger needs to be reactive, but without the parentheses
    trigger = gender_react,
    source_method = "raw",
    source_object = quetzio_examples$label_update$gender_update
  )
}
shinyApp(ui, server)
}
