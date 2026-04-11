test_that("granovagg.ds handles ggplot2::mpg tibble input", {
  data("mpg", package = "ggplot2")
  tibble_input <- mpg[, c("cty", "hwy")]
  plot_obj <- suppressWarnings(
    suppressMessages(
      granovagg.ds(
        data = tibble_input,
        plot.theme = ggplot2::theme_minimal
      )
    )
  )
  expect_s3_class(plot_obj, "ggplot")
})
