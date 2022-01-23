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


#' Create input for Likert scale questions based on radioButtons
#' 
#' @param inputId The input slot that will be used to access the value.
#' @param label Label that will be shown to the user
#' @param choiceValues Vector with numeric values to use. Minimal length is 3. Too
#' long scale may cause the widget to look not as intended.
#' @param choiceNames Character vector, indicating meaning of each choiceValue
#' after user selects it. If you don't wish to show meaning for every value,
#' provide character vector of length 2 with meaning of min and max
#' @param selected Initially selected value. Defaults to 'character(0)', which
#' will make selection empty on init
#' @param placeholder Initial text in showing meaning of selected choiceValue.
#' Meaningful only if 'length(choiceNames) > 2' and 'selected = character(0)'.
#' Defaults to 'Select value'
#' @param width Width of the input in either relative (%) or absolute (px) values.
#' 
#' @details 
#' Generated input can have choiceNames character correlated to every choiceValues
#' number - if that is the case, then the indicator showing the character associated
#' to currently checked value will be rendered. 
#' 
#' Alternatively, choiceNames can be a character vector of length 2, showing
#' at all times the character associated to lowest (or the farthest to the left)
#' and highest (or the farthest to the right) value.
#' 
#' In that case, the character value provided to the 'placeholder' don't matter,
#' as it won't be generated.
#' 
#' @example inst/examples/likertRadioButtons.R
#' @import shiny
#' @export


likertRadioButtons <- function(
  inputId,
  label,
  choiceValues,
  choiceNames,
  selected = character(0),
  placeholder = "Select value",
  width = "500px"
) {
  
  # nocov start
  # type checkup
  
  if (!is.numeric(choiceValues)) {
    stop("Object passed to 'choiceValues' need to be a numeric vector")
  }
  
  if (!is.character(choiceNames)) {
    stop("Object passed to 'choiceNames' needto be a character vector")
  }
  
  # length checkup
  
  if (length(choiceValues) < 3 ) {
    stop("There need to be at least 3 'choiceValues' for this type of input")
  }
  
  if (!length(choiceNames) %in% c(2, length(choiceValues))) {
    stop("'choiceNames' need to contain either two labels (for 'min' and 'max')
         or a label for every 'choiceValue'")
  } 
  
  if (all(length(choiceNames) > 2, (length(choiceNames) != length(choiceValues)))) {
    stop("If 'choiceNames' isn't of length 2, it should be the same length as
         'choiceValues'")
    
  }
  
  # nocov end
  
  selected <- restoreInput(id = inputId, default = selected)
  
  selected <- if (is.null(selected)) 
    choiceValues[[1]]
  else as.character(selected)
  if (length(selected) > 1) 
    stop("The 'selected' argument must be of length 1")
  
  #generate the tag
  tagList(
    # add necessary html dependencies
    htmltools::htmlDependency(
      name = "shiny.quetzio",
      version = utils::packageVersion("shiny.quetzio"),
      package = "shiny.quetzio",
      src = "likertRadioButtons",
      script = "likertRadioButtons.js",
      stylesheet = "likertRadioButtons.css"
    ),
    # create input as a table element for responsiveness
    tags$table(
      id = inputId, class = "form-group shiny-input-likert-radiobuttons shiny-input-container",
      style = htmltools::css(width = validateCssUnit(width)),
      # append the rows generated by 'generateLikertRadioButtonsUI'
      tagList(generateLikertRadioButtonsUI(
        inputId = inputId,
        label = label,
        selected = selected,
        choiceNames = choiceNames,
        choiceValues = choiceValues,
        placeholder = placeholder
      ))
    )
  )
  
}


#' function to generate likertRadioButtons UI
#' 
#' @import shiny
#' @noRd

generateLikertRadioButtonsUI <- function(
  inputId,
  label,
  selected,
  choiceNames,
  choiceValues,
  placeholder
) {
  
  # detect if the indicator will be generated
  if (length(choiceNames) == 2) {
    indicator <- FALSE
  } else {
    indicator <- TRUE
  }

  # assign minName and maxName to indicate meaning of these values 
  minName <- choiceNames[1]
  maxName <- choiceNames[length(choiceNames)]
  
  # create a list of radio controls
  radioControlList <- lapply(
    seq_along(choiceValues), 
    \(i) {
      if (indicator) {
        # with indicator present, assign class to react on its change
        inputTag <- tags$input(type = "radio", name = inputId, class = "likert-input-radio indicator-updater",
                               style = "position: relative;",
                               value = choiceValues[i], `choice-name` = choiceNames[i])
      } else {
        # with indicator absent, don't assign this class
        inputTag <- tags$input(type = "radio", name = inputId, class = "likert-input-radio",
                               style = "position: relative;",
                               value = choiceValues[i])
      }
      
      # check the 'selected' value on start
      if ((choiceValues[i] %in% selected) || (choiceNames[i] %in% selected)) {
        inputTag$attribs$checked <- "checked"
      }
      
      # create a line connecting the inputs
      # not for last, as it is and :after pseudo-element
      tags$td(class = paste("likert-input-radio-control",
                            if (i < length(choiceValues)) "likert-radio-line"),
              inputTag)
    }
  )
  
  # create labels under inputs
  radioLabelsList <- lapply(
    seq_along(choiceValues),
    \(i) {
      tags$td(class = "likert-input-radio-label", choiceValues[i])
    }
  )
  
  # create a rows for label, inputs and input labels
  likertTag <- 
    list(
      tags$tr(
        tags$td(
          colspan = as.character(length(choiceValues) + 2),
          tags$label(
            class = "control-label", id = paste0(inputId, "-label"),
            `for` = inputId, label
          )
        )
      ),
      tags$tr(
        tags$td(class = "likert-input-radio-min", rowspan="2", minName),
        tagList(radioControlList),
        tags$td(class = "likert-input-radio-max", rowspan="2", maxName)
      ),
      tags$tr(
        tagList(radioLabelsList)
      )
    )
  
  # if indicator is present, append the row containing the indicator
  if (indicator) {
    likertTag <- c(
      likertTag,
      tagList(tags$tr(
        tags$td(
          class = "likert-input-radio-indicator",
          colspan = as.character(length(choiceValues) + 2),
          if (length(selected) == 0 || is.na(selected)) placeholder else choiceNames[choiceValues == selected]
        )
      ))
    )
  }
  
  # convert to tagList
  tagList(likertTag)
  
}
