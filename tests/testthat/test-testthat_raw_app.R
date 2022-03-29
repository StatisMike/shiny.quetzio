
compare_strictly <- function(app, RDS, testapp_path) {
  
  actual <- app$getAllValues()
  expected <- readRDS(file.path(testapp_path, RDS))
  
  for (iotype in c("input", "output")) {
    
    sapply(names(expected[[iotype]]), 
           
           function(name) {
             testthat::expect_equal(actual[[iotype]][[name]],
                                    expected[[iotype]][[name]])
           })
  }}

test_that("shiny.quetzio in general works stable", {
  
  testapp_path <- testthat::test_path("../shinyTest_general/")
  
  app <- shinytest::ShinyDriver$new(testapp_path)
  app$waitForShiny()
  compare_strictly(app, "snapshot_1.RDS", testapp_path)
  app$setInputs(`first_link-gender_item-gender_item` = "F")
  app$setInputs(`first_link-gender_item-submit` = "click")
  compare_strictly(app, "snapshot_2.RDS", testapp_path)
  app$setInputs(`first_link-first_simple-shiny_positive_text` = "everything")
  app$setInputs(`first_link-first_simple-other_langs` = "3")
  app$setInputs(`first_link-first_simple-other_langs` = c("3", "5"))
  app$setInputs(`first_link-first_simple-other_langs` = c("3", "5", "16"))
  app$setInputs(`first_link-first_simple-years_of_experience` = -1)
  app$setInputs(`first_link-first_simple-questio_need` = "Yes")
  app$setInputs(`first_link-first_simple-submit` = "click")
  compare_strictly(app, "snapshot_3.RDS", testapp_path)
  app$setInputs(`first_link-first_simple-years_of_experience` = 0)
  app$setInputs(`first_link-first_simple-years_of_experience` = 1)
  app$setInputs(`first_link-first_simple-submit` = "click")
  app$setInputs(get_df_first = "click")
  app$setInputs(update_values = "click")
  compare_strictly(app, "snapshot_4.RDS", testapp_path)
  app$setInputs(`second_link-seconnd_simple-submit` = "click")
  app$setInputs(`second_link-with_default-test2` = "2")
  app$setInputs(`second_link-with_default-test1` = "whatever")
  app$setInputs(`second_link-with_default-test3` = "2")
  app$setInputs(`second_link-with_default-test5` = "3")
  app$setInputs(`second_link-with_default-test4` = "First choice")
  app$setInputs(`second_link-with_default-test4` = c("First choice", "Third choice"))
  app$setInputs(`second_link-with_default-submit` = "click", timeout_ = 5000)
  compare_strictly(app, "snapshot_5.RDS", testapp_path)
  app$setInputs(`second_link-from_gsheet-test1` = "Like")
  app$setInputs(`second_link-from_gsheet-test1` = "Like this")
  app$setInputs(`second_link-from_gsheet-test2` = 5)
  app$setInputs(`second_link-from_gsheet-test3` = "Something")
  app$setInputs(`second_link-from_gsheet-test4` = "2")
  app$setInputs(`second_link-from_gsheet-test4` = c("2", "1"))
  app$setInputs(`second_link-from_gsheet-test4` = c("2", "1", "3"))
  app$setInputs(`second_link-from_gsheet-test5` = "More")
  app$setInputs(`second_link-from_gsheet-test6` = "2")
  compare_strictly(app, "snapshot_6.RDS", testapp_path)
  app$setInputs(`second_link-from_gsheet-submit` = "click", timeout_ = 5000)
  app$setInputs(`second_link-gender_react-submit` = "click", timeout_ = 5000)
  app$waitFor("($('#shiny-modal').data('bs.modal') || {}).isShown", 1000)
  compare_strictly(app, "snapshot_7.RDS", testapp_path)
  app$executeScript("$('.modal').modal('hide');")
  app$setInputs(`second_link-gender_react-test1` = "good")
  app$setInputs(`second_link-gender_react-test2` = "Bad")
  app$setInputs(`second_link-gender_react-test3` = "1")
  app$setInputs(`second_link-gender_react-test3` = c("1", "2"))
  app$setInputs(`second_link-gender_react-test3` = c("1", "2", "3"))
  app$setInputs(`second_link-gender_react-submit` = "click", timeout_ = 5000)
  app$setInputs(get_df_second = "click", timeout_ = 5000)
  compare_strictly(app, "snapshot_8.RDS", testapp_path)
  
})