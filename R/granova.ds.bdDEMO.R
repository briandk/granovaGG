# For plot comparisons
library(granova)

# Custom functions
source("granova.ds.bd.R")
# trace("granova.ds.bd", browser, exit = browser)
# pdf("PruzekTestPlots.pdf", onefile = TRUE)

x <- rnorm(mean = 42, n = 25)
y <- rnorm(mean = 45, n = 25)
testFrame1 <- data.frame(MathPreTest = x, MathPostTest = y)


x <- rnorm(mean = 534, n = 25, sd = 100)
y <- rnorm(mean = 517, n = 25, sd = 100)
testFrame2 <- data.frame(SATVerbalPre = x, SATVerbalPost = y)

print(granova.ds.bd(testFrame1, tTestConfidenceLevel = 0.95))
print(granova.ds.bd(testFrame2, tTestConfidenceLevel = 0.50))

print(granova.ds.bd(testFrame2, plotTitle = "awesomeSauce"))

dev.off()
