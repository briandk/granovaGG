quiet_contr <- function(...) {
  suppressWarnings(suppressMessages({
    result <- NULL
    capture.output({ result <- granovagg.contr(...) })
    result
  }))
}

test_that("granovagg.contr returns plots and prints weighted means for arousal", {
  data("arousal", package = "granovaGG")
  contrasts22 <- data.frame(
    c(-0.5, -0.5, 0.5, 0.5),
    c(-0.5, 0.5, -0.5, 0.5),
    c(0.5, -0.5, -0.5, 0.5)
  )
  names(contrasts22) <- c("Drug.A", "Drug.B", "Drug.A.B")
  plots <- quiet_contr(arousal, contrasts = contrasts22)
  expect_length(plots, 4)
  
  messages <- capture.output(
    granovagg.contr(arousal, contrasts = contrasts22),
    type = "message"
  )
  expect_true(any(grepl("Weighted", messages, fixed = TRUE)))
})
