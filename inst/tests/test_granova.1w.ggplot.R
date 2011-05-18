if(environmentName(environment()) == "R_GlobalEnv") {
  # load granova.1w.ggplot with unitTest=TRUE
  # which will cause this to be re-loaded, but not in the global environment

  # WEJD: I don't like this path being hard coded
  source("../../R/granova.1w.ggplot.R")

  w <- c(9,2,3,1)
  x <- c(1,2,3,4)
  y <- c(5,6,7,8)
  z <- c(9,0,1,2)
  df <- data.frame(w=w, x=x, y=y, z=z)

  granova.1w.ggplot(data=df, unitTest = TRUE)
} else {
  # if we're not in the global environment, then
  # we're being called from within the outer package itself, so...
  # run the tests
  
  # Perform any library setup here
  library(granova)
  library(testthat)
  
  # Perform any object/data setup here
  plotObject <- InitializeGgplot()
 
  # Write the rest of your tests below this line
  
  test_that("InitializeGgplot returns a ggplot2 object", {
    expect_that(plotObject, is_a("ggplot"))
  })


}

