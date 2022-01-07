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
  quetzio_UI("updating_labels")
)

server <- function(input, output, session) {

  quetzio <- quetzio_server$new(
    source_method = "raw",
    source_object = quetzio_examples$questions_lists$gender_update,
    module_id = "updating_labels"
  )

  # trigger need to be reactive
  gender_react <- reactive(input$gender)

  # update labels method call
  quetzio$update_labels(
    trigger = gender_react,
    source_method = "raw",
    source_object = quetzio_examples$label_update$gender_update
  )
}
shinyApp(ui, server)
}
