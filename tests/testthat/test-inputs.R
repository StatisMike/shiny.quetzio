test_that("likertRadioButtons generation without indicator works", {
  
  likertRadioTag <- likertRadioButtons(
    "some_id",
    "I am the label",
    1:4,
    c("Min", "Max")
  )
  
  # check if the number of radio controls are correct
  expect_equal(
    sum(grepl(unlist(likertRadioTag), pattern = "likert-input-radio-control")),
    4
  )
  
  # check if the min and max are correctly created
  expect_equal(
    sum(grepl(unlist(likertRadioTag), pattern = "likert-input-radio-min|likert-input-radio-max")),
    2
  )
  
  # check if the indicator isn't generation
  expect_equal(
    sum(grepl(unlist(likertRadioTag), pattern = "likert-input-radio-indicator")),
    0
  )
  
  # check if the indicator updaters aren't present
  expect_equal(
    sum(grepl(unlist(likertRadioTag), pattern = "indicator-updater")),
    0
  )
  
})


test_that("likertRadioButtons generation with indicator works", {
  
  likertRadioTag <- likertRadioButtons(
    "some_id",
    "I am the label",
    -2:2,
    c("Min", "Low", "Neither", "High", "Max")
  )
  
  # check if the number of radio controls are correct
  expect_equal(
    sum(grepl(unlist(likertRadioTag), pattern = "likert-input-radio-control")),
    5
  )
  
  # check if the min and max are correctly created
  expect_equal(
    sum(grepl(unlist(likertRadioTag), pattern = "likert-input-radio-min|likert-input-radio-max")),
    2
  )
  
  # check if the indicator is generated
  expect_equal(
    sum(grepl(unlist(likertRadioTag), pattern = "likert-input-radio-indicator")),
    1
  )
  
  # check if the indicator updaters are present
  expect_equal(
    sum(grepl(unlist(likertRadioTag), pattern = "indicator-updater")),
    5
  )
  
})