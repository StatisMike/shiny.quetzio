test_that("Quetzio App generation and reactivity works as a whole", {
  
  tmp_lib <- ensurePackagePresent(pkgName = "shiny.quetzio", quiet = F)
  results <- withr::with_libpaths(tmp_lib, {
    shinytest::testApp(appDir = testthat::test_path("../shinyTest_general"), compareImages = FALSE)
  }, action = "prefix")
  shinytest::expect_pass(results)  
  
})
