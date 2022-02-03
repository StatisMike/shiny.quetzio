
## only run examples in interactive environment

if (interactive()) {

library(shiny)
library(shiny.quetzio)

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
  QuetzioLink_UI("labels_link")
)

server <- function(input, output, session) {

  quetzio_link <- QuetzioLink_create(
    quetzio_2nd = Quetzio_create(
      source_method = "raw",
      source_object = quetzio_examples$questions_lists$simple_quetzio,
      module_id = "second_in_link"
    ),
    gender = Quetzio_create(
      source_method = "raw",
      source_object = quetzio_examples$questions_lists$gender_update,
      module_id = "updating_labels"
    ),

    link_id = "labels_link")

  # trigger need to be reactive
  gender_react <- reactive(input$gender)

  # update labels method call
  Quetzio_label_update(
    Quetzio = quetzio_link,
    # you need to provide the name of the quetzio_server in link
    # where you need to update labels
    name = "gender",
    # the trigger needs to be reactive, but without the parentheses
    trigger = gender_react,
    source_method = "raw",
    source_object = quetzio_examples$label_update$gender_update
  )
}
shinyApp(ui, server)
}
