
<!-- README.md is generated from README.Rmd. Please edit that file -->

# shiny.survey

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
<!-- badges: end -->

This package includes the way to create a survey using various Shiny
widgets from a source file: either a local YAML file, or GoogleSheet
over the net. Results of the surveys are send to GoogleSheet of your
choice.

It is all completely handled by `R6` shiny modules, so it is easy to
include even multiple independend questionnaires in your ShinyApp and
keep your code clean!

## Installation

You can install the development version of shiny.survey from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("StatisMike/shiny.survey")
```

## Current features

At the current stage of development the `shiny.survey` package consists
of two exported `R6` classes used in server and corresponding functions
to bind the UI in your shinyApp: \* `quetzio_server` to create single
questionnaire and `quetzio_UI` to input the UI of the questionnaire.
Questionnaires at the moment handles these type of inputs: - `textInput`
- `numericInput` - `selectizeInput` - `radioButtons`

-   `quetzio_link_server` to link multiple semi-independent
    questionnaires and `quetzio_link_UI` to bind the connected UI.

### In-App usage

It’s usage is very straigtforward:

1.  Simply add a `quetzio_server` object in your shinyApp `server()`
    call, and `quetzio_UI` in your `ui`:

``` r
ui <- fluidPage(
  quetzio_UI("yaml_module"),
  quetzio_UI("gsheet_module")
)

server <- function(input, output, session) {

# YAML generated survey with output automatically saved to googlesheets

yaml_quetzio <- quetzio_server$new(
  source_method = "yaml",
  source_yaml = "some_yaml",
  output_gsheet_id = "googlesheet_id",
  output_gsheet_sheetname = "sheet_name_with_questions",
  module_id = "yaml_module"
)

# survey generated from googlesheet source, with output automatically saved to
# googlesheets

gsheet_quetzio <- quetzio_server$new(
  source_method = "gsheet",
  source_gsheet_id = "googlesheet_id",
  source_gsheet_sheetname = "sheet_name_with_questions",
  # you don't need to specify another googlesheet file to save answers
  # If you don't specify it, the class assumes it is the same as source one
  output_gsheet_id = "another_googlesheet_id",
  output_gsheet_sheetname = "sheet_name_with_answers",
  module_id = "gsheet_module"
)

}
```

2.  Additionally, your ShinyApp can monitor the questionnaire status and
    react fully customizable!

``` r
  
# trigger some action after the questionnaire is completed
  observe({
    req(yaml_quetzio$is_done())
    showModal(
      modalDialog("You're done!")
    )
  })

# catch the answers provided to the questionnaire
  output$gsheet_answers <- renderPrint(
    gsheet_quetzio$answers()
  )
```

3.  There is also a possibility to link your questionnaires in a way,
    that they will appear one after another. It is now needed to
    configure your calls accordingly:

``` r
ui <- fluidPage(
  quetzio_link_UI("modules_link")
)

server <- function(input, output, session) {

# Linked questionnaires - one generated from yaml, second from googlesheets. 
# Their output won't be automatically saved to googlesheets in this example
# (though it is possible to set - their internal reactivity is independent
# to the quetzio_link in that regard)

  quetzio_link <- quetzio_link_server$new(
    yaml_quetzio = quetzio_server$new(
      source_method = "yaml",
      source_yaml = "some_yaml",
      module_id = "yaml_module"
    ),
    gsheet_quetzio = quetzio_server$new(
      source_method = "gsheet",
      source_gsheet_id = "googlesheet_id",
      source_gsheet_sheetname = "sheet_name_with_questions",
      module_id = "gsheet_module"
    ),
    link_id = "modules_link"
  )

  # and you can also trigger things based on the completion rate
  
  # trigger some action after the link is 50% completed and after completion
  # of both questionnaires
  observe({
    if (quetzio_link$completion() == 0.5) {
      showModal(
        modalDialog("You're half done!")
      )
    } else if (quetzio_link$completion() == 1) {
      showModal(
        modalDialog("You're completely done!")
      )
    }
  })

# catch the answers provided to the questionnaire
  output$all_answers <- renderPrint(
    quetzio_link$answers()
  )

}
```

### Survey configuration

You can configure your survey widely, using many of the features native
to the used shiny inputs.

#### Universal parameters:

For every input you can specify:

-   **inputId**
-   **type**
-   **label**
-   mandatory: (true/false) if the input must be filled
-   width: the same as in regular input specification. If not provided,
    defaults to 500px

> *Bold* ones are mandatory for every input

#### Type-specific parameters:

|   parameter    | textInput | numericInput | selectizeInput | radioButtons |
|:--------------:|:---------:|:------------:|:--------------:|:------------:|
|  placeholder   |     x     |              |                |              |
|     value      |           |      x       |                |              |
|      min       |           |      x       |                |              |
|      max       |           |      x       |                |              |
|      step      |           |      x       |                |              |
|   *choices*    |           |              |       x        |      x       |
| *choiceValues* |           |              |       x        |      x       |
| *choiceNames*  |           |              |       x        |      x       |
|    multiple    |           |              |       x        |              |
|    selected    |           |              |       x        |      x       |
|     inline     |           |              |                |      x       |

> *Italic* parameters are interchangeable. You can specify either
> *choices* or both *choiceValues* and *choiceNames* for single input.

## Presentation

You can check the running demo on
<a href="https://statismik.shinyapps.io/shinysurvey_sneakpeek/" target="_blank">shinyapps.io</a>

## discoRd kudos

Foundations for this package is in the ‘discoRd’ community, most notably
users who created the underlying logic for the
<a href="https://github.com/discoRd-org/discoRd-survey" target="_blank">discoRd-survey</a>:

-   Alexander Lam
    (<a href="https://github.com/acylam" target="_blank">acylam</a>)
-   Erez Shomron
    (<a href="https://github.com/eshom" target="_blank">eshom</a>)
-   Eric Fletcher
    (<a href="https://github.com/iamericfletcher" target="_blank">iamericfletcher</a>)

Feel free to join our great community at
<a href="https://discord.gg/FuTSvkSCVj" target="_blank">discoRd channel</a>!
