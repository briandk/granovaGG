if(environmentName(environment()) == "R_GlobalEnv") {
  # load granova.1w.ggplot with unitTest=TRUE
  # which will cause this to be re-loaded, but not in the global environment
  source("granova.1w.ggplot.R")
  granova.1w.ggplot(unitTest = TRUE)
} else {
  # if we're not in the global environment, then
  # run the tests
  library(testthat)
  
  test_that("one does not equal two", {
    expect_that(1 == 2, is_false())
    expect_that(1 == 1, is_true())
    expect_that(3 == 3, is_true())
    expect_that(3 == 3, is_false())
  })
}
