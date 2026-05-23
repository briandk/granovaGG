test_that("granovagg.contr suppresses output when print.summary = FALSE", {
  data("arousal", package = "granovaGG")
  contrasts22 <- data.frame(
    c(-0.5, -0.5, 0.5, 0.5),
    c(-0.5,  0.5, -0.5, 0.5),
    c( 0.5, -0.5, -0.5, 0.5)
  )
  names(contrasts22) <- c("Drug.A", "Drug.B", "Drug.A.B")

  stdout_output <- capture.output(
    suppressWarnings(
      invisible(
        granovagg.contr(arousal, contrasts = contrasts22, print.summary = FALSE)
      )
    )
  )
  message_output <- capture.output(
    suppressWarnings(
      invisible(
        granovagg.contr(arousal, contrasts = contrasts22, print.summary = FALSE)
      )
    ),
    type = "message"
  )

  expect_length(stdout_output, 0)
  expect_length(message_output, 0)
})

test_that("granovagg.contr prints output by default", {
  data("arousal", package = "granovaGG")
  contrasts22 <- data.frame(
    c(-0.5, -0.5, 0.5, 0.5),
    c(-0.5,  0.5, -0.5, 0.5),
    c( 0.5, -0.5, -0.5, 0.5)
  )
  names(contrasts22) <- c("Drug.A", "Drug.B", "Drug.A.B")

  message_output <- capture.output(
    suppressWarnings(
      invisible(
        granovagg.contr(arousal, contrasts = contrasts22)
      )
    ),
    type = "message"
  )

  expect_true(any(grepl("Linear Model Summary", message_output, fixed = TRUE)))
})
