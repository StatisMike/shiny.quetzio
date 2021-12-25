## Only run example in interactive R sessions

if (interactive()) {

#### define questions UI

list_of_questions <- list(
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
  ),
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

list_of_descriptions <- list(
  list(
    type = "instructions_title",
    content = "Exemplary questionnaire with quetzio",
    align = "center"
  ),
  list(
   type = "instructions_para",
   content = "This is an exemplary questionnaire that you can build using
   <b>quetzio_server</b> class. You can use <i>HTML</i> tags in instruction
   and item descriptions, including the emojis. Isn't that neat? &#128515;",
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

#### create shinyApp

# load libraries
library(shiny)
library(shiny.survey)

# create ui
ui <- fluidPage(
      column(6, align = "center",
             # bind the UI with correct module_id
             quetzio_UI("my_quetzio")
             ),
      column(6,
             h2("State of", tags$i("quetzio_server")),
             h3("Is it done?"),
             verbatimTextOutput("quetzio_is_done"),
             h3("Error messages?"),
             verbatimTextOutput("quetzio_message"),
             h3("Answers"),
             verbatimTextOutput("quetzio_answers")
             )
)

server <- function(input, output, session) {

  # initialize new quetzio
  quetzio_output <- quetzio_server$new(
    # load questions from R object
    source_method = "raw",
    source_object = list_of_questions,
    # optionally add descriptions
    desc_object = list_of_descriptions,
    # use the same module_id as in UI binding
    module_id = "my_quetzio",
    # custom_css to give margin but not center options explicitly
    # it will affect only elements within the form div
    custom_css = list(
      "shiny-options-group" = "text-align: left; margin-left: 45%"
    ),
    # you can also optionally give div unique id - useful for external styling
    div_id = "my_questio_div_id"
  )

  # render objects from quetzio
  output$quetzio_is_done <-
    renderPrint(quetzio_output$is_done())
  output$quetzio_message <-
    renderPrint(quetzio_output$message())
  output$quetzio_answers <-
    renderPrint(quetzio_output$answers())
}

shinyApp(ui, server)

}
