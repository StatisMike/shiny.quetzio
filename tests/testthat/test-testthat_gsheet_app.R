test_that("Quetzio can save to googlesheets", {
  
  skip("Currently this test won't be used")
  skip_on_cran()
  
  tmp_lib <- ensurePackagePresent(pkgName = "shiny.quetzio", quiet = F)
  results <- withr::with_libpaths(tmp_lib, {
    shinytest::testApp(appDir = testthat::test_path("../shinyTest_gsheet"), compareImages = FALSE)
  }, action = "prefix")
  shinytest::expect_pass(results)  
  
})