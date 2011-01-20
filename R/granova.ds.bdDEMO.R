# For plot comparisons
library(granova)
library(ggplot2)
library(DAAG)

# Custom functions
source("granova.ds.bd.R")
# trace("granova.ds.bd", browser, exit = browser)
# pdf("PruzekTestPlots.pdf", onefile = TRUE)

set.seed(1003)

x <- rnorm(mean = 42, n = 25)
y <- rnorm(mean = 45, n = 25)
caseid <- 1:25
testFrame1 <- data.frame(MathPreTest = x, MathPostTest = y, caseid)
p <- granova.ds.bd(testFrame1, conf.level = 0.95, plotTitle = "This is a test plot")
p + geom_text(aes(label = caseid), data = testFrame1, size = I(3), vjust = -1)
print(granova.ds.bd(pair65, conf.level = 0.99, plotTitle = "Second test plot"))
ggplot(aes(x = x, y = y, label = caseid), data = testFrame1) + geom_text()

z <- c((40 - 10), (46.5 + 10), 26)
testFrame1 <- rbind(testFrame1, z)

p <- granova.ds.bd(testFrame1, conf.level = 0.95, plotTitle = "This is a test plot")
p + geom_text(aes(label = caseid), data = testFrame1, size = I(3), vjust = -1)
print(granova.ds.bd(pair65, conf.level = 0.99, plotTitle = "Second test plot"))
ggplot(aes_string(x = x, y = y, label = caseid), data = testFrame1) + geom_text()

