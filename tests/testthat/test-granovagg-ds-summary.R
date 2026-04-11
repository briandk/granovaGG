test_that("granovagg.ds prints expected summary for anorexia.sub", {
  data("anorexia.sub", package = "granovaGG")
  printed <- capture.output(
    suppressWarnings(
      suppressMessages(
        granovagg.ds(
          anorexia.sub,
          plot.theme = ggplot2::theme_minimal
        )
      )
    )
  )
  
  expect_true(any(grepl("Prewt mean\\s+83\\.229", printed)))
  expect_true(any(grepl("Postwt mean\\s+90\\.494", printed)))
  expect_true(any(grepl("mean\\(D = Prewt - Postwt\\)\\s+-7\\.265", printed)))
  expect_true(any(grepl("Lower 95% Confidence Interval\\s+-3\\.585", printed)))
  expect_true(any(grepl("Upper 95% Confidence Interval\\s+-10\\.945", printed)))
  expect_true(any(grepl("t \\(D-bar\\)\\s+-4\\.185", printed)))
})
