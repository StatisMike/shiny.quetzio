test_that("Creating items and their descriptions from googlesheet works", {
  
  # initializing server function
  
  googlesheets4::gs4_deauth()
  
  questions <- googlesheets4::read_sheet(
    "1N4NQ_ixkOz4qPcyZgirnJN8RK-T_Qz-hwA8NU0MBjYA", "Questions")
  descriptions <- googlesheets4::read_sheet(
    "1N4NQ_ixkOz4qPcyZgirnJN8RK-T_Qz-hwA8NU0MBjYA", "Description")
  
  gsheet_server <- function(input, output, session) {
    
    gsheet_quetzio <- quetzio_server$new(
      source_method = "gsheet",
      source_gsheet_id = "1N4NQ_ixkOz4qPcyZgirnJN8RK-T_Qz-hwA8NU0MBjYA",
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
