AssertEquals <- function(expected = NULL, actual = NULL, message = "Error") {
  if (expected != actual) {
    cat( paste("FAIL:", message, "\n") )
    cat( paste("      Expected:", expected, "\n") )
    cat( paste("      Observed:", actual, "\n\n") )
    stop()
  }
}


#########################################
# BEGIN TESTS
#########################################

TestLinearModelCoefficients <- function() {
  set.seed(1001)
  x <- rnorm(100) 
  y <- rnorm(100) 
  lm1 <- lm(y~x)
  
  nn <- names(lm1)
  AssertEquals(TRUE, "coefficients" %in% nn, "Linear models should have a coefficients element")

  nn <- names(lm1$coef)
  AssertEquals("(Intercept)", nn[1], "The first element of an lm's coefficients should be the intercept")
  AssertEquals("x", nn[2], "The second element of an lm's coefficients should be the regression parameter")

  AssertEquals(0.0060, round(lm1$coef[1], 4), "unexpected value for intercept term")
  AssertEquals(-0.0407, round(lm1$coef[2], 4), "unexpected value for x term")
}





#########################################
# END TESTS
#########################################






RunTests <- function() {
  testFunctions <- ls(name=parent.frame(1), pattern="^Test*")

  for (fun in testFunctions) {
    cat( paste(fun, "\n") )
    do.call(fun, list())
  }
  
  cat("All tests passed\n\n")
}

RunTests()

