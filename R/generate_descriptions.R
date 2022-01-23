#' generate instructions and item descriptions for quetzio with the use of
#' source list and 'insertUI'
#'
#' @param self R6 'self' object
#' @import shiny
#' @noRd
#' @keywords internal

.generate_description <- function(
  self
) {

  # instruction rendering
  instruction <- self$description[sapply(self$description, \(x) grepl(pattern = "instruction", x = x$type))]

  if (length(instruction) > 0) {

    # initialize the list
    inst_list <- list()

    # for every element of instructions
    for (inst in instruction) {

      # if title, create h1 tag
      if (grepl(pattern = "title", x = inst$type)) {

        list_el <- tagList(tags$h1(align = .null_def(inst$align, "left"),
                                   class = "quetzio_title",
                                   if (isTRUE(inst$html)) HTML(inst$content) else inst$content))

        inst_list <- c(inst_list, list_el)

        # if paragraph, create p tag
      } else if (grepl(pattern = "para", x = inst$type)) {

        list_el <- tagList(tags$p(align = .null_def(inst$align, "left"),
                                  class = "quetzio_paragraph",
                                  if (isTRUE(inst$html)) HTML(inst$content) else inst$content))

        inst_list <- c(inst_list, list_el)

      } else if (grepl(pattern = "list", x = inst$type)) {

        list_el <- if(isTRUE(inst$order))
          tagList(tags$ol(align = .null_def(inst$align, "left"),
                          class = "quetzio_list",
                          tagList(lapply(inst$content, tags$li))))
        else
          tagList(tags$ul(align = .null_def(inst$align, "left"),
                          class = "quetzio_list",
                          tagList(lapply(inst$content, tags$li))))

        inst_list <- c(inst_list, list_el)
      }

    }

    insertUI(
      selector = paste0("#", self$div_id),
      where = "afterBegin",
      ui = tags$div(class = "quetzio_instruction",
                    inst_list))

  }

  # rendering descriptions
  descriptions <- self$description[sapply(self$description, \(x) grepl(pattern = "item_desc", x = x$type))]

  if (length(descriptions) > 0) {

    for (desc in descriptions) {

      insertUI(
        selector = paste0("#", paste(sep = ns.sep, self$module_ui_id, desc$inputId, "label")),
        where = "afterEnd",
        ui = tags$div(
          align = .null_def(desc$align, "left"),
          class = "quetzio_description",
          if (isTRUE(desc$html)) HTML(desc$content) else desc$content
        )
      )
    }
  }
}
