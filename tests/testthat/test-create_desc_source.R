test_that("Create description source to df works", {
  
  descriptions_source <- create_desc_source(
    method = "df"
  )
  
  expect_s3_class(descriptions_source, "data.frame")
  
  expect_equal(nrow(descriptions_source), 1)
  
  expect_equal(
    names(descriptions_source),
    c("type", "align", "html", "order", "inputId", "content")
  )
  
})

test_that("Create description source to gsheets works", {
  
  suppressMessages({
    googlesheets4::gs4_auth(email = Sys.getenv("G_SERVICE_MAIL"),
                            path = Sys.getenv("G_SERVICE_ACCOUNT"))
    googledrive::drive_auth(email = Sys.getenv("G_SERVICE_MAIL"),
                            path = Sys.getenv("G_SERVICE_ACCOUNT"))
  })

  
  descriptions_id <- create_desc_source(
    method = "gsheet"
  )
  
  descriptions_source <- googlesheets4::read_sheet(
    ss = descriptions_id,
    sheet = "Descriptions"
  )
  
  expect_equal(nrow(descriptions_source), 1)
  
  expect_equal(
    names(descriptions_source),
    c("type", "align", "html", "order", "inputId", "content")
  )
  
  suppressMessages({
    googledrive::drive_trash(descriptions_id)
  })
})