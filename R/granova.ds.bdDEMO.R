# Custom functions
source("granova.ds.bd.R", chdir = TRUE)

x <- rnorm(mean = 42, n = 25)
y <- rnorm(mean = 45, n = 25)

testFrame <- data.frame(xvals = x, yvals = y)
testFrame
pair65

granova.ds.bd(testFrame)