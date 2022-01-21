## Only run in interactive session

if (interactive()) {
  
  ui <- fluidPage(
    
    likertRadioButtons(
      inputId = "first", 
      label = "First input", 
      # you need to provide numeric value present among the choiceValues
      selected = 4,
      choiceValues = 1:7,
      # only names for min and max (left and right)
      choiceNames = c("Not much", "Many")),
    verbatimTextOutput("first_output"),
    
    likertRadioButtons(
      inputId = "second",
      label = "Second input",
      # default (character(0)) value will make no initial selection
      # selected = character(0),
      choiceValues = c(-2:2),
      choiceNames = c("Very bad", "Slightly bad", "Not bad or good",
                      "Slighty good", "Very good")),
      verbatimTextOutput("second_output")
  )
  server <- function(input, output, session) {
    output$first_output <- renderPrint(input$first)
    output$second_output <- renderPrint(input$second)
  }
  shinyApp(ui, server)
}