#' Modified Shiny numericInput
#'
#' Create an input control for entry of numeric values. Contrary to the vanilla
#' numericInput, it allows no initial value and placeholder content.
#'
#' @param inputId The `input` slot that will be used to access the value.
#' @param label Display label for the control, or `NULL` for no label.
#' @param value Initial value.
#' @param width The width of the input, e.g. `'400px'`, or `'100%'`;
#'   see [validateCssUnit()].
#' @param placeholder A character string giving the user a hint as to what can
#'   be entered into the control. Internet Explorer 8 and 9 do not support this
#'   option.
#' @param min Minimum allowed value
#' @param max Maximum allowed value
#' @param step Interval to use when stepping between min and max
#' @return A numeric input control that can be added to a UI definition. It should
#' be noted, that empty input will generate NA, not NULL, as in other shinyInputs
#'
#' @examples
#' ## Only run examples in interactive R sessions
#' if (interactive()) {
#'
#' ui <- fluidPage(
#'   numInput("obs", "Observations:", placeholder = "Enter value", min = 1, max = 100),
#'   verbatimTextOutput("value")
#' )
#' server <- function(input, output) {
#'   output$value <- renderText({ input$obs })
#' }
#' shinyApp(ui, server)
#' }
#'
#' @section Server value:
#' A numeric vector of length 1.
#' @import shiny
#' @importFrom htmltools css
#' @export
numInput <- function(inputId, label, value = NA, placeholder = NULL,
                     min = NA, max = NA, step = NA, width = NULL) {

  value <- shiny::restoreInput(id = inputId, default = value)

  # build input tag
  inputTag <- tags$input(id = inputId, type = "number", class="form-control",
                         value = if (is.null(value)) NULL
                                  else format(value, scientific = FALSE, digits = 15),
                         placeholder = placeholder)
  if(!is.na(value))
    inputTag$attribs$value = value
  if (!is.na(min))
    inputTag$attribs$min = min
  if (!is.na(max))
    inputTag$attribs$max = max
  if (!is.na(step))
    inputTag$attribs$step = step

  div(class = "form-group shiny-input-container",
      style = htmltools::css(width = validateCssUnit(width)),
      tags$label(label, class = "control-label", id = paste0(inputId, "-label"),
                 `for` = inputId),
      inputTag
  )
}
