test_that("tabular summaries can be suppressed", {
  data("poison", package = "granovaGG")
  args <- list(
    data = poison$SurvTime,
    group = poison$Group,
    resid = FALSE,
    print.squares = FALSE,
    print.group.summary = FALSE,
    print.model.summary = FALSE
  )
  
  message_output <- capture.output(
    suppressWarnings(invisible(do.call(granovagg.1w, args))),
    type = "message"
  )
  expect_false(any(grepl("By-group summary statistics", message_output, fixed = TRUE)))
  expect_false(any(grepl("linear model summary", message_output, fixed = TRUE)))
  expect_false(any(grepl("t-test summary", message_output, fixed = TRUE)))
  
  stdout_output <- capture.output(
    suppressWarnings(invisible(do.call(granovagg.1w, args)))
  )
  expect_false(any(grepl("trimmed.mean", stdout_output, fixed = TRUE)))
  expect_false(any(grepl("Estimate", stdout_output, fixed = TRUE)))
})

test_that("formatted summary tables provide structured output", {
  scores <- c(7.1, 6.9, 7.5, 8.2, 7.7, 8.1)
  groups <- factor(c("control", "control", "control", "treatment", "treatment", "treatment"))
  
  formatted_output <- capture.output(
    suppressWarnings(
      suppressMessages(
        invisible(
          granovagg.1w(
            data = scores,
            group = groups,
            resid = FALSE,
            print.squares = FALSE,
            summary.table.format = "formatted"
          )
        )
      )
    )
  )
  expect_true(any(grepl("statistic", formatted_output)))
  
  plain_output <- capture.output(
    suppressWarnings(
      suppressMessages(
        invisible(
          granovagg.1w(
            data = scores,
            group = groups,
            resid = FALSE,
            print.squares = FALSE,
            summary.table.format = "plain"
          )
        )
      )
    )
  )
  expect_false(any(grepl("statistic", plain_output)))
})
