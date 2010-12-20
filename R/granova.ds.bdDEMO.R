# For plot comparisons
library(granova)

# Custom functions
source("granova.ds.bd.R")

pdf("PruzekTestPlots.pdf", onefile = TRUE)

x <- rnorm(mean = 42, n = 25)
y <- rnorm(mean = 45, n = 25)
testFrame1 <- data.frame(xvals = x, yvals = y)


x <- rnorm(mean = 534, n = 25, sd = 20)
y <- rnorm(mean = 517, n = 25, sd = 20)
testFrame2 <- data.frame(PreTest = x, Post = y)

print(granova.ds.bd(testFrame1, conf.level = 0.95))
print(granova.ds.bd(testFrame2, conf.level = 0.50))

print(granova.ds(testFrame2))

dev.off()
