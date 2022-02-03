## only run examples in interactive environment

if (interactive()) {

  gender_1_item_source <- list(
    gender_item = list(
      type = "selectizeInput",
      label = "What is your gender?",
      mandatory = TRUE,
      choiceNames = c("Male", "Female", "I identify as neither of above", "Prefer not to say"),
      choiceValues = c("M", "F", "O", "NI"),
      maxItems = 1
    ))


  library(shiny)
  library(shiny.quetzio)

  ui <- fluidPage(
    verbatimTextOutput("reactive_val"),
    hr(),
    # quetzio to update labels
    QuetzioLink_UI("labels_link")
  )

  server <- function(input, output, session) {

    quetzio_link <- QuetzioLink_create(
      gender_question = Quetzio_create(
        source_method = "raw",
        source_object = gender_1_item_source,
        module_id = "single_question"
      ),
      gender_update = Quetzio_create(
        source_method = "raw",
        source_object = quetzio_examples$questions_lists$gender_update,
        module_id = "update_gender"
      ),
      link_id = "labels_link")

    # trigger need to be reactive
    gender_react <- reactive({
      quetzio_link$answers()$gender_question$gender_item
    })

    output$reactive_val <- renderPrint(gender_react())

    # update labels method call
    Quetzio_label_update(
      Quetzio = quetzio_link,
      # you need to provide the name of the quetzio_server in link
      # where you need to update labels
      name = "gender_update",
      # the trigger needs to be reactive, but without the parentheses
      trigger = gender_react,
      source_method = "raw",
      source_object = quetzio_examples$label_update$gender_update
    )
  }
  shinyApp(ui, server)
}
