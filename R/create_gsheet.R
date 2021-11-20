#' Create googlesheet source compatible with shiny.survey
#' 
#' @param name optional name for your googlesheet
#' @import googlesheets4
#' @export

create_survey_questions_gsheet <- function(name = NULL){
  
  if(is.null(name)){
    name <- googlesheets4::gs4_random()
  }
  
  sheet = list(
    Questions = data.frame(
      inputId = "placeholder",
      type = "textInput",
      mandatory = TRUE,
      label = "some label",
      chr_placeholder = "some text",
      num_value = 1,
      num_min = 0,
      num_max = 2,
      num_step = 0.5,
      mult_choices = "Something\nElse\nMore",
      mult_choiceValues = "1\n2\n3",
      mult_choiceNames = "One\nTwo\nThree",
      select_multiple = TRUE,
      mult_selected = "NULL",
      radio_inline = TRUE,
      width = "500px"
    )
  )
  
  id <- googlesheets4::gs4_create(
    name = name,
    sheets = sheet
  )
  
  return(id)
  
}