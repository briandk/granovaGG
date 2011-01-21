# For plot comparisons
library(granova)
library(ggplot2)
library(DAAG)
library(RColorBrewer)

source("granova.ds.bd.R")

set.seed(1001)


x <- rnorm(mean = 42, n = 25)
y <- rnorm(mean = 45, n = 25)
caseid <- 1:25
testFrame1 <- data.frame(MathPreTest = x, MathPostTest = y, caseid)

granova.ds.bd(
  data               = testFrame1, 
  conf.level         = 0.95, 
  plotTitle          = "This is a test plot",
  colorBrewerPalette = "Dark2",
  noBackground       = TRUE
)

granova.ds.bd(
  data               = testFrame1, 
  conf.level         = 0.95, 
  plotTitle          = "This is a test plot",
  colorBrewerPalette = "Dark2",
  noBackground       = TRUE
)

# p + geom_text(aes(label = caseid), data = testFrame1, size = I(3), vjust = -1)
