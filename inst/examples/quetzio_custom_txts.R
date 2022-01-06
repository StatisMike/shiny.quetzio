## Only run example in interactive R sessions

if (interactive()) {

  # load libraries
  library(shiny)
  library(shiny.quetzio)

  # create ui
  ui <- fluidPage(quetzio_UI("my_quetzio"))

  server <- function(input, output, session) {

    # initialize new quetzio
    questionnaire <- quetzio_server$new(
      source_method = "raw",
      source_object = quetzio_examples$questions_lists$simple_quetzio,
      module_id = "my_quetzio",
      # specify some custom texts to show up
      custom_txts = list(
        modal_title = "Something is not right!",
        modal_content = "Please correct your answers:",
        submit_enabled = "Do it!",
        submit_disabled = "Can't do!"
      )
    )

  }

  shinyApp(ui, server)

}
