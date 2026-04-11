test_that("granovagg.1w prints a t-test summary for unequal group sizes", {
  scores <- c(4.2, 3.9, 5.1, 6.5, 5.8, 6.1, 6.7)
  groups <- factor(c(rep("control", 3), rep("treatment", 4)))
  plot_obj <- suppressWarnings(suppressMessages({
    output <- NULL
    capture.output({
      output <- granovagg.1w(
        data = scores,
        group = groups,
        resid = FALSE,
        print.squares = FALSE,
        jj = 0
      )
    })
    output
  }))
  expect_s3_class(plot_obj, "ggplot")
})
