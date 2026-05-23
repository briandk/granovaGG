test_that("granovagg.ds suppresses output when print.summary = FALSE", {
  data("anorexia.sub", package = "granovaGG")

  stdout_output <- capture.output(
    suppressWarnings(
      invisible(
        granovagg.ds(anorexia.sub, print.summary = FALSE)
      )
    )
  )
  message_output <- capture.output(
    suppressWarnings(
      invisible(
        granovagg.ds(anorexia.sub, print.summary = FALSE)
      )
    ),
    type = "message"
  )

  expect_length(stdout_output, 0)
  expect_length(message_output, 0)
})

test_that("granovagg.ds prints output by default", {
  data("anorexia.sub", package = "granovaGG")

  stdout_output <- capture.output(
    suppressWarnings(
      invisible(
        granovagg.ds(anorexia.sub)
      )
    )
  )

  expect_true(any(grepl("Summary Statistics", stdout_output, fixed = TRUE)))
})
