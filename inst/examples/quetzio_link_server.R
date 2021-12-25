## Only run example in interactive R sessions

if (interactive()) {

#### define questions UI

list_of_questions1 <- list(
  shiny_positive_text = list(
    type = "textInput",
    label = "What do you like about Shiny?",
    placeholder = "Write here"
  ),
  other_langs = list(
    type = "selectizeInput",
    label = "What programming languages besides R are you using most of the time?",
    mandatory = TRUE,
    maxItems = 3,
    choiceNames = c("C/C++", "Scala", "JavaScript", "Matlab", "Python", "SPSS",
                    "Swift", "Go", "C#", "Julia", "Stata", "Ruby", "PHP", "SAS",
                    "Java", "Other"),
    choiceValues = c(1:16)
  ))

list_of_questions2 <- list(
  years_of_experience = list(
    type = "numericInput",
    label = "How many years of experience in creating Shiny apps do you have?",
    value = 0,
    min = 0,
    step = 1
  ),
  questio_need = list(
    type = "radioButtons",
    label = "Do you see yourself using shiny.survey in the future projects?",
    mandatory = TRUE,
    choices = c("Yes", "No", "Maybe")
  )
)

#### define descriptions UI

list_of_descriptions1 <- list(
  list(
    type = "instructions_title",
    content = "First linked questionnaire",
    align = "center"
  ),
  list(
   type = "instructions_para",
   content = "This is the first questionnaire linked with <i>quetzio_link_server</i>.
   Only after completing this one user is able to go to the next.",
   align = "center",
   html = TRUE
  ),
  list(
    type = "item_desc",
    content = "Choose three maximum.",
    align = "center",
    inputId = "other_langs"
  )
)

list_of_descriptions2 <- list(
  list(
    type = "instructions_title",
    content = "Second linked questionnaire",
    align = "center"
  ),
  list(
    type = "instructions_para",
    content = "Welcome to second questionnaire. You can link as many of them
    as you wish - not only two! <br> This UI won't disappear after completion.",
    align = "center",
    html = TRUE
  )
)

#### create shinyApp

# load libraries
library(shiny)
library(shiny.survey)

# create ui
ui <- fluidPage(
      column(6, align = "center",
             # bind the UI with correct link_id
             quetzio_link_UI("my_quetzio_link")
             ),
      column(6,
             h2("State of", tags$i("quetzio_link_server")),
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
  quetzio_link_output <- quetzio_link_server$new(
    # initialize quetzio_servers within it (give them names!)
    quetzio_first = quetzio_server$new(
      source_method = "raw",
      source_object = list_of_questions1,
      desc_object = list_of_descriptions1,
      module_id = "my_first_quetzio"
    ),
    quetzio_second = quetzio_server$new(
      source_method = "raw",
      source_object = list_of_questions2,
      desc_object = list_of_descriptions2,
      module_id = "my_second_quetzio",
      custom_css = list(
        "shiny-options-group" = "text-align: left; margin-left: 45%"
      )
    ),
    link_id = "my_quetzio_link"
  )

  # render objects from quetzio_link
  output$quetzio_completion <-
    renderPrint(quetzio_link_output$completion())
  output$quetzio_message <-
    renderPrint(quetzio_link_output$message())
  output$quetzio_answers <-
    renderPrint(quetzio_link_output$answers())
}

shinyApp(ui, server)

}
