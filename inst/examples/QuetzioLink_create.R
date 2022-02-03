## Only run example in interactive R sessions

if (interactive()) {

#### create shinyApp

# load libraries
library(shiny)
library(shiny.quetzio)

# create ui
ui <- fluidPage(
      column(6, align = "center",
             # bind the UI with correct link_id
             QuetzioLink_UI("my_quetzio_link")
             ),
      column(6,
             h2("State of", tags$i("QuetzioLink")),
             h3("Completion rate"),
             verbatimTextOutput("quetzio_completion"),
             h3("Error messages?"),
             verbatimTextOutput("quetzio_message"),
             h3("Answers"),
             verbatimTextOutput("quetzio_answers")
             )
)

server <- function(input, output, session) {

  # initialize new quetzio link
  linked_questionnaires <- QuetzioLink_create(
    # initialize quetzio_servers within it (give them names!)
    quetzio_first = Quetzio_create(
      source_method = "raw",
      source_object = quetzio_examples$questions_lists$link_quetzio_1,
      desc_object = quetzio_examples$description_lists$link_quetzio_1,
      module_id = "my_first_quetzio",
      custom_css = list(
        "quetzio_list" = "text-align: left; margin-left: 35%;"
      )
    ),
    quetzio_second = Quetzio_create(
      source_method = "raw",
      source_object = quetzio_examples$questions_lists$link_quetzio_2,
      desc_object = quetzio_examples$description_lists$link_quetzio_2,
      module_id = "my_second_quetzio",
      custom_css = list(
        "shiny-options-group" = "text-align: left; margin-left: 40%;"
      )
    ),
    quetzio_third = Quetzio_create(
      source_method = "raw",
      source_object = quetzio_examples$questions_lists$link_quetzio_3,
      desc_object = quetzio_examples$description_lists$link_quetzio_3,
      module_id = "my_third_quetzio"
    ),
    link_id = "my_quetzio_link"
  )

  # render objects from quetzio_link
  output$quetzio_completion <-
    renderPrint(linked_questionnaires$completion())
  output$quetzio_message <-
    renderPrint(linked_questionnaires$message())
  output$quetzio_answers <-
    renderPrint(linked_questionnaires$answers())
}

shinyApp(ui, server)

}
