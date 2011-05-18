test_that("one does not equal two", {
  expect_that(1 == 2, is_false())
  expect_that(1 == 2, is_true())
  expect_that(1 == 1, is_true())
  expect_that(1 == 1, is_false())
})

source("../../R/granova.1w.ggplot.R", chdir = TRUE)

test_that("plot code produces ggplot objects", {
  expect_that(initializeGgplot(), is_a("ggplot"))
})