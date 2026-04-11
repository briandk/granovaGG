test_that("granovagg.ds enforces numeric two-column input", {
  bad_data <- data.frame(X = letters[1:3], Y = 1:3)
  expect_error(
    granovagg.ds(bad_data, plot.theme = ggplot2::theme_minimal),
    "two-column numeric"
  )
})

test_that("granovagg.ds validates conf.level", {
  df <- data.frame(X = rnorm(5), Y = rnorm(5))
  expect_error(
    granovagg.ds(df, conf.level = 1, plot.theme = ggplot2::theme_minimal),
    "conf.level"
  )
  expect_error(
    granovagg.ds(df, conf.level = -0.1, plot.theme = ggplot2::theme_minimal),
    "conf.level"
  )
})
