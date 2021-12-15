# shiny.survey 0.0.3

* First production-ready version.
* Added a `NEWS.md` file to track changes to the package.

# shiny.survey 0.0.4

* Rebuilding of the whole system for modules
* Begin rebranding to shiny.quetzio
* Removal of `survey_module` object and creating `quetzio_server` R6 object and `quetzio_UI` function

# shiny,survey 0.0.5

* Added working R6 object to link multiple questionnaires created with `quetzio_server` and corresponding
UI function `quetzio_link_UI`
* Logic behing parsing css for '.invalid_input' and '.mandatory_star' has been also redone. It is now
easier to implement and allows for independent looks (based on the `div_id` of the given questionnaire)
* Removed `googlesheets4` and `yaml` dependency:
  - `googlesheets4` needed only when taking source from gsheet or putting output into gsheet
  - `yaml` needed only when taking source from YAML file

# shiny.survey 0.0.6

* Made the `link_id` and `render_ui` arguments of linked `quetzio_server` modules set automatically:
no need to specify it automatically any more!

# shiny.survey 0.0.7

* `selectizeInputs` works better: by default they are using selectize.js and `multiple = TRUE`,
which allows for empty initial value. Instead of specifying `multiple`, user can now
specify `maxItems`, which indicates how many items can be specified. If left `NULL`,
it allows for selecting all of the options.
* *invalid_input* class is now added to the *labels*, not whole *div*. It remedies
problem with selectizeInputs, which main div isn't showing in the rendered state.
  + the default *invalid_input* css style is changed to `color: red; font-style: italic;`
  
# shiny.survey 0.0.8

* moved `quetzio_list` of `quetzio_link_server` from private to public. It is now
easier to access separate questionnaires from there
* added `update_labels` method to the `quetzio_survey`. You can now update labels
of questionnaire items based on some external `reactive()`.
  + works also with questionnaires linked by `quetzio_link_server`, though the call
  need to be made on the `quetzio_server` itself directly within observer:
  `observe(quetzio_link_object$quetzio_list[['quetzio_name']]$update_labels(...))`
  + the wrapper function for this functionality is considered
  
# shiny.survey 0.0.9

* added `update_labels` method to `quetzio_link_server` class
* polished the documentation and code for the `update_labels` methods
* fixed the bug with 'mandatory star' disappearing on mandatory items
when their label gets updated

# shiny.survey 0.0.10

* support for populating data with default values have been added, with
`source_yaml_default` argument to the `quetzio_server` class initialization
