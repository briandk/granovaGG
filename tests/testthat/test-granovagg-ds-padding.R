quiet_granovagg_ds <- function(...) {
  suppressWarnings(suppressMessages({
    plot_obj <- NULL
    capture.output({
      plot_obj <- granovagg.ds(...)
    })
    plot_obj
  }))
}

test_that("granovagg.ds padding arguments adjust the intended edges", {
  df <- data.frame(X = c(0, 1), Y = c(0, 2))
  square_range <- diff(range(c(df$X, df$Y)))
  base_plot <- quiet_granovagg_ds(df, plot.theme = ggplot2::theme_minimal)
  base_window <- attr(base_plot, "padded.window")
  
  custom_plot <- quiet_granovagg_ds(
    df,
    northeast.padding = 0.05,
    southwest.padding = 0.10,
    plot.theme = ggplot2::theme_minimal
  )
  custom_window <- attr(custom_plot, "padded.window")
  
  expect_equal(
    custom_window[1],
    base_window[1] - square_range * 0.10,
    tolerance = 1e-8
  )
  expect_equal(
    custom_window[2],
    base_window[2] + square_range * 0.05,
    tolerance = 1e-8
  )
})
