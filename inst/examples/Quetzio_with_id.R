
if (interactive()) {
  
  library(shiny)
  library(shiny.quetzio)
  
  ui <- fluidPage(
    column(6,
           Quetzio_UI("simple_quetzio"),
           hr(),
           QuetzioLink_UI("simple_linked")
           ),
    column(6,
           actionButton("get_from_quetzio",
                        "Get from Quetzio"),
           dataTableOutput("quetzio_results"),
           hr(),
           actionButton("get_from_linked",
                        "Get from QuetzioLink"),
           dataTableOutput("linked_results"),
           hr(),
           actionButton("get_single_from_linked",
                        "Get single from linked"),
           selectInput("which_to_get",
                       "Which quetzio to get the answers from?",
                       c("first", "second", "third")),
           dataTableOutput("single_linked_results")
           )
  )
  
  server <- function(input, output, session) {
    
   id <- reactiveVal() 
    
    simple <- Quetzio_create(
      source_method = "raw",
      source_object = quetzio_examples$questions_lists$simple_quetzio,
      module_id = "simple_quetzio",
      questionee_id = id,
      output_gsheet = T,
      output_gsheet_id = "1zAqDdtfHGP1xhHNikBa9MS6zCy1JXHeAkcSVdZbtOkw",
      output_gsheet_sheetname = "simple"
    )
    
    linked <- QuetzioLink_create(
      first = Quetzio_create(
        source_method = "raw",
        source_object = quetzio_examples$questions_lists$link_quetzio_1,
        module_id = "module1"
      ),
      second = Quetzio_create(
        source_method = "raw",
        source_object = quetzio_examples$questions_lists$link_quetzio_2,
        module_id = "module2"
      ),
      link_id = "simple_linked",
      questionee_id = id,
      output_gsheet = T,
      output_gsheet_id = "1zAqDdtfHGP1xhHNikBa9MS6zCy1JXHeAkcSVdZbtOkw",
      output_gsheet_sheetname = "linked"
    )
    
    observeEvent(input$get_from_quetzio, {
      id(format(Sys.time(), format = "%s"))
      output$quetzio_results <- renderDataTable(Quetzio_get_df(simple))
    })
    
    observeEvent(input$get_from_linked, {
      id(format(Sys.time(), format = "%s"))
      output$linked_results <- renderDataTable(Quetzio_get_df(linked))
    })
    
    observeEvent(input$get_single_from_linked, {
      id(format(Sys.time(), format = "%s"))
      output$single_linked_results <- renderDataTable(
        Quetzio_get_df(linked, input$which_to_get)
      )
    })
    
  }
  
  shinyApp(ui, server)
}
