
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
include even multiple `shiny.survey`s in your ShinyApp and have your
code clean!

## Installation

You can install the development version of shiny.survey from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("StatisMike/shiny.survey")
```

## Current features

At the current stage of development the `shiny.survey` package consists
of one exported `R6` class: `shiny.survey_module`. It handles basic
inputs:

-   `textInput`
-   `numericInput`
-   `selectizeInput`
-   `radioButtons`

### In-App usage

It’s usage is very straigtforward:

1.  At the beginning: create new `survey_module` in the `global.R` or
    outside of the `ui` and `server` objects.

For long surveys and frequently used ShinyApps I recommend generating
the objects beforehand, saving them with `saveRDS()` or `save` in the
app directory and then loading them with `readRDS()` or `load()` to cut
the time for initializing the object during the loadtime.

``` r
# using YAML source file

yaml_survey <- survey_module$new(
  source_method = "yaml",
  source_yaml = "some_yaml",
  output_gsheet_id = "googleSheet_id",
  output_gsheet_sheetname = "sheet_name_with_questions"
)

# or googlesheet source

gsheet_survey <- survey_module$new(
  source_method = "gsheet",
  source_gsheet_id = "googleSheet_id",
  source_gsheet_sheetname = "sheet_name_with_questions",
  # you don't need to specify another googleSheet file to save answers
  # If you don't specify it, the class assumes it is the same as source one
  output_gsheet_id = "another_googleSheet_id",
  output_gsheet_sheetname = "sheet_name_with_answers"
)
```

2.  After the object was created, simply call methods in the `ui` and
    `server` of your ShinyApp!

``` r
  ui <- fluidPage(
     yaml_survey$ui()
  )
  server <- function(input, output, session){
     my_survey <- yaml_survey$server()
  }
```

3.  Additionally, your ShinyApp can monitor the survey status!

``` r
# logical checking if the survey has been send (TRUE) or no (FALSE)
  my_survey$is_done 

# if there were some errors, `is_done` will be NA, and additionally
# the error message can be retrieved also!
  my_survey$message
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
