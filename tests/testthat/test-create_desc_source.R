test_that("Create description source", {
  
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