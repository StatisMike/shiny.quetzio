######         for everyone who want to add another language:        ####
# - add new language in 'v_language_registered'

v_language_registered = setNames(
  object = c("en", "pl"),
  nm = c("English", "Polski")
)

# - update 'lang_warning_call' with new language

lang_warning_call <- "Only supported language for now are: 'en' and 'pl'"

# In the document below:
# - look for comments saying: 'update for new language' and update accordingly
# - update documentation
##########################################################################

pkgEnv <- new.env()

pkgEnv$label_en = list(
  # submit button labels
  submit_enabled = "Submit",
  submit_disabled = "Can't submit",
  submit_done = "Submitted!",
  submit_error = "Error!",
  # modal texts
  modal_title = "Answers can't be saved",
  modal_content = "Some mandatory inputs aren't filled and/or numeric inputs aren't withing correct range:",
  modal_button = "Close the window"
)

pkgEnv$label_pl = list(
  # submit button labels
  submit_enabled = "Prześlij",
  submit_disabled = "Nie można przesłać",
  submit_done = "Przesłano!",
  submit_error = "Błąd!",
  # modal texts
  modal_title = "Odpowiedzi nie mogą być zapisane",
  modal_content = "Niektóre pola wymagane nie są wypełnione i/lub pola liczbowe są poza dozwolonymi granicami:",
  modal_button = "Zamknij okno"
)

#' @importFrom R6 R6Class
#' @importFrom utils modifyList
language <- R6::R6Class(
  classname = "shiny.quetzio_language",
  public = list(
    initialize = function() {
      invisible(self)
    },
    set_language = function(lan) {
      if (!lan %in% private$language_registered) {
        stop("Unsupported language !", call. = FALSE)
      }
      private$language <- lan
      private$labels <-   switch (lan,       # update for new language
                                  "en" = pkgEnv$label_en,
                                  "pl" = pkgEnv$label_pl
      )
    },
    get = function(label) {
      value <- private$labels[[label]]
      if(is.null(value)){
        label
      } else {
        value
      }
    },
    get_all = function() {
      private$labels
    },
    get_language_registered = function() {
      private$language_registered
    },
    get_language = function() {
      private$language
    }
  ),
  private = list(
    language = "en",
    language_registered = v_language_registered,
    labels = pkgEnv$label_en,
    length = function() base::length(private$labels)
  )
)


#' @title Use {shiny.quetzio} labels
#'
#' @description See all labels registered with \code{get_labels()},
#'  then set custom text with \code{set_labels()}.
#'
#' @param lan Language to use for labels, supported values are : "en" and "pl".
#'
#' @return A language object
#' @export
#'
#'
#' @examples
#'
#' use_language(lan = "pl")
#'
use_language <- function(lan = "en") {
  lang <- language$new()
  lang$set_language(lan)
  lang
}

#' @title Modify {shiny.quetzio} labels to use custom text
#'
#' @description See all labels registered with \code{get_labels()},
#'  then set custom text with \code{set_labels()}.
#'
#' @param language Language to use for labels, supported values are : "en", "pl".
#' @param ... A named list with labels to replace.
#'
#' @return \code{get_labels()} return a named list with all labels registered.
#' @export
#'
#' @name custom-labels
#'
#' @examples
#'
#' # In global.R for example:
#' set_labels(
#'   language = "en",
#'   "Please authenticate" = "You have to login",
#'   "Username:" = "What's your name:",
#'   "Password:" = "Enter your password:"
#' )
set_labels <- function(language, ...) {
  if (!language %in% c("en", "pl")) {        # update for new language
    stop(lang_warning_call, call. = FALSE)
  }
  args <- list(...)
  if (!all(nzchar(names(args)))) {
    stop("All arguments must be named!", call. = FALSE)
  }

  current_labels <- switch (language,        # update for new language
                            "en" = pkgEnv$label_en,
                            "pl" = pkgEnv$label_pl
  )

  update_labels <- modifyList(
    x = current_labels,
    val = lapply(args, I)
  )

  if(language %in% "en"){                    # update for new language
    pkgEnv$label_en <- update_labels
  } else if(language %in% "pl"){
    pkgEnv$label_pl <- update_labels
  }

  invisible(TRUE)
}

#' @export
#'
#' @rdname custom-labels
get_labels <- function(language = "en") {
  if (!language %in% c("en", "pl")) {         # update for new language
    warning(lang_warning_call, call. = FALSE)
    language <- "en"
  }

  switch (language,                           # update for new language
          "en" = pkgEnv$label_en,
          "pl" = pkgEnv$label_pl
  )
}


