test_that("Getting all texts for language works", {
  
  langs <- shiny.quetzio:::quetzio_texts$.languages_registered
  
  lapply(langs,
         \(lang) {
           
           expect_equal(
             shiny.quetzio:::quetzio_texts[[lang]],
             quetzio_txt(lang)
           )
         })
})

test_that("Getting specific text for langauge works", {
  
  texts <- names(shiny.quetzio:::quetzio_texts$en)
  langs <- shiny.quetzio:::quetzio_texts$.languages_registered
  
  lapply(
    langs,
    \(lang) {
      lapply(texts,
             \(txt) {
               expect_equal(
                 shiny.quetzio:::quetzio_texts[[lang]][[txt]],
                 quetzio_txt(lang, txt)
               )
             })
    }
  )
})
