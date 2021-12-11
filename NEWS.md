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
