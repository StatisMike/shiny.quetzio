test_that("Create question source to df works", {
  
  questions_source <- create_quetzio_source(
    method = "df"
  )
  
  expect_s3_class(questions_source, "data.frame")
  
  expect_equal(nrow(questions_source), 1)
  
  expect_equal(
    names(questions_source),
    c("inputId", "type", "mandatory", "label", "width", "chrnum_placeholder",
      "num_value", "num_min", "num_max", "num_step", "mult_choices", "mult_choiceValues",
      "mult_choiceNames", "mult_selected", "select_maxItems", "radio_inline")
  )
})

test_that("Create question source to gsheets works", {
  
  suppressMessages({
    googlesheets4::gs4_auth(email = Sys.getenv("G_SERVICE_MAIL"),
                            path = Sys.getenv("G_SERVICE_ACCOUNT"),
                            cache = F)
    googledrive::drive_auth(email = Sys.getenv("G_SERVICE_MAIL"),
                            path = Sys.getenv("G_SERVICE_ACCOUNT"),
                            cache = F)
  })
  
  
  questions_id <- create_quetzio_source(
    method = "gsheet"
  )
  
  questions_source <- googlesheets4::read_sheet(
    ss = questions_id,
    sheet = "Questions"
  )
  
  expect_equal(nrow(questions_source), 1)
  
  expect_equal(
    names(questions_source),
    c("inputId", "type", "mandatory", "label", "width", "chrnum_placeholder",
      "num_value", "num_min", "num_max", "num_step", "mult_choices", "mult_choiceValues",
      "mult_choiceNames", "mult_selected", "select_maxItems", "radio_inline")
  )
  
  suppressMessages({
    googledrive::drive_trash(questions_id)
  })
  
})