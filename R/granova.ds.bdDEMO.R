# Custom functions
source("granova.ds.bd.R")

# pdf("testPlots8.pdf", onefile = TRUE)
x <- rnorm(mean = 42, n = 25)
y <- rnorm(mean = 45, n = 25)

testFrame1 <- data.frame(xvals = x, yvals = y)

print(granova.ds.bd(testFrame1))

x <- rnorm(mean = 534, n = 25)
y <- rnorm(mean = 517, n = 25)

testFrame2 <- data.frame(xvals = x, yvals = y)

print(granova.ds.bd(testFrame2))
print(granova.ds.bd(pair65))

dev.off()