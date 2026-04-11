find_raw_layer <- function(plot_obj, target_x, target_y) {
  built <- ggplot2::ggplot_build(plot_obj)
  for (layer in built$data) {
    if (!is.null(layer$x) &&
        length(layer$x) == length(target_x) &&
        isTRUE(all.equal(layer$x, target_x)) &&
        isTRUE(all.equal(layer$y, target_y))) {
      return(layer)
    }
  }
  return(NULL)
}

test_that("granovagg.ds maps columns to axes as documented", {
  set.seed(123)
  df <- data.frame(
    X = round(runif(6, 0, 10), 3),
    Y = round(rnorm(6, 0, 1), 3)
  )
  
  base_plot <- suppressWarnings(
    suppressMessages(
      granovagg.ds(
        data = df,
        plot.theme = ggplot2::theme_minimal
      )
    )
  )
  raw_layer <- find_raw_layer(base_plot, df$X, df$Y)
  expect_false(is.null(raw_layer))
  
  reversed_plot <- suppressWarnings(
    suppressMessages(
      granovagg.ds(
        data = df,
        revc = TRUE,
        plot.theme = ggplot2::theme_minimal
      )
    )
  )
  reversed_layer <- find_raw_layer(reversed_plot, df$Y, df$X)
  expect_false(is.null(reversed_layer))
})
