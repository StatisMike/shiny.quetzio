test_that("Creating items and their descriptions from googlesheet works", {
  
  # initializing server function
  
  googlesheets4::gs4_deauth()
  
  questions <- googlesheets4::read_sheet(
    Sys.getenv("QUETZIO_SS"), "Questions")
  descriptions <- googlesheets4::read_sheet(
    Sys.getenv("QUETZIO_SS"), "Description")
  
  gsheet_server <- function(input, output, session) {
    
    gsheet_quetzio <- Quetzio_create(
      source_method = "gsheet",
      source_gsheet_id = Sys.getenv("QUETZIO_SS"),
      source_gsheet_sheetname = "Questions",
      desc_gsheet_sheetname = "Description",
      module_id = "test_module"
    )
  }
  
  testServer(gsheet_server, {
    
    expect_equal(names(gsheet_quetzio$source_list),
                 as.character(questions$inputId))
    
    expect_equal(length(gsheet_quetzio$description),
                 nrow(descriptions))
  })
})
