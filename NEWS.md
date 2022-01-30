# shiny.quetzio 0.1.2

* added new question parameters for input types:
   - textInput: 
     - regex: allows for conditional checking the validity of questionee
     input. If matches the provided regex string: the input is valid
   - selectizeInput:
     - placeholder: character string to be shown before questionee inputs value
     - create: *boolean* - should the questionee be allowed to provide their own
     answer.

# shiny.quetzio 0.1.1

* fixed bug in likertRadioButtons, which made initial selection regardless
of user choice in widget without placeholder

# shiny.quetzio 0.1.0

* adding custom shiny input: `likertRadioButtons`, which is fully supported by
questionnaires created within `shiny.quetzio`, but can also be used outside of this context.
* stability guaranteed by high coverage by `testthat` and `shinytest` packages
* trio of vignettes decribing questionnaire creation with `quetzio_server`, 
updating by its class methods and linking with `quetzio_link_server`.

# shiny.quetzio 0.0.14

* added option to randomize item order with `randomize_order = T` argument
during `quetzio_server` initialization
* added possibility for default configuration in the form of raw `list` object, 
mirroring the structure of defaults from *yaml* file. Also, if the source is 
provided by feeding a `list` object in `raw_method`, both default configuration
method works also.
* created tests powered by `shinytest` and `testthat`. Used example provided
by [Ruben Fealens](https://github.com/rfaelens/exampleShinyTest) to record test coverage with `shinyTest`
with codecov workflow.

# shiny.quetzio 0.0.13

* added multiple language support for messages generated internally and
default submit button options - `lang` argument in `quetzio_server`. Currently
only 'en' for English and 'pl' for Polish are supported. All contributions to
add support for more languages are welcome!
  + `custom_txts` argument for `quetzio_server` also provide option to modify
  messages shown to end-user on the fly. Replaces previous argument of `submit_labels`,
  as it is now redundant.
  + you can also check what are the default texts for specified language and retrieve
  them outside of 'shiny_quetzio' with `quetzio_txt` function.
* added option to mute the modalDialogs generated during questionnaire fillout
* `messages()` object now contains information if there were any invalid inputs
during submit
* first vignettes detailing the usage of package are now provided
* fixed bug that caused item labels to not update when the value provided in
`trigger` was set before the questionnaire UI were rendered

# shiny.quetzio 0.0.12

* created `numInput` shiny widget - slightly modified `numericInput` to allow
no initial value and placeholder text. All `quetzio`s use it in place of vanilla
one.
* added examples of usage for main methods of `quetzio_server` and 
`quetzio_link_server`.
* provided exemplary sources built-in to the package. Check `quetzio_examples`!
* fixed wrong internal function call while during usage of `update_values` method.

# shiny.quetzio 0.0.11

* R6 methods `update_values` supports updating chosen values with external source
* support for adding instructions for questionnaire from YAML, googlesheet or R
object source has been added

# shiny.quetzio 0.0.10

* support for populating data with default values have been added, with
`source_yaml_default` argument to the `quetzio_server` class initialization

# shiny.quetzio 0.0.9

* added `update_labels` method to `quetzio_link_server` class
* polished the documentation and code for the `update_labels` methods
* fixed the bug with 'mandatory star' disappearing on mandatory items
when their label gets updated

# shiny.quetzio 0.0.8

* moved `quetzio_list` of `quetzio_link_server` from private to public. It is now
easier to access separate questionnaires from there
* added `update_labels` method to the `quetzio_survey`. You can now update labels
of questionnaire items based on some external `reactive()`.
  + works also with questionnaires linked by `quetzio_link_server`, though the call
  need to be made on the `quetzio_server` itself directly within observer:
  `observe(quetzio_link_object$quetzio_list[['quetzio_name']]$update_labels(...))`
  + the wrapper function for this functionality is considered
  
# shiny.quetzio 0.0.7

* `selectizeInputs` works better: by default they are using selectize.js and `multiple = TRUE`,
which allows for empty initial value. Instead of specifying `multiple`, user can now
specify `maxItems`, which indicates how many items can be specified. If left `NULL`,
it allows for selecting all of the options.
* *invalid_input* class is now added to the *labels*, not whole *div*. It remedies
problem with selectizeInputs, which main div isn't showing in the rendered state.
  + the default *invalid_input* css style is changed to `color: red; font-style: italic;`
  
# shiny.quetzio 0.0.6

* Made the `link_id` and `render_ui` arguments of linked `quetzio_server` modules set automatically:
no need to specify it automatically any more!

# shiny,survey 0.0.5

* Added working R6 object to link multiple questionnaires created with `quetzio_server` and corresponding
UI function `quetzio_link_UI`
* Logic behing parsing css for '.invalid_input' and '.mandatory_star' has been also redone. It is now
easier to implement and allows for independent looks (based on the `div_id` of the given questionnaire)
* Removed `googlesheets4` and `yaml` dependency:
  - `googlesheets4` needed only when taking source from gsheet or putting output into gsheet
  - `yaml` needed only when taking source from YAML file
  
# shiny.quetzio 0.0.4

* Rebuilding of the whole system for modules
* Begin rebranding to shiny.quetzio
* Removal of `survey_module` object and creating `quetzio_server` R6 object and `quetzio_UI` function

# shiny.quetzio 0.0.3

* First production-ready version.
* Added a `NEWS.md` file to track changes to the package.
