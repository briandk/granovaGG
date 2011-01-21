# For plot comparisons
library(granova)
library(ggplot2)
library(RColorBrewer)

source("granova.ds.bd.R")

loadGranovaData <- function (filepath) {
  data   <- read.csv(filepath, header = TRUE)
  caseid <- 1:length(data[ , 1])
  return( cbind(data, caseid))
}

tobacco  <- loadGranovaData("../data/tobacco.csv")
anorexia <- loadGranovaData("../data/anorexia.csv")
lead     <- loadGranovaData("../data/lead.csv")

granova.ds.bd(
  data               = lead, 
  conf.level         = 0.95, 
  plotTitle          = "Lead Levels in the Blood of Children",
) + opts(panel.background = theme_rect(fill = "grey95", colour = NA))

granova.ds.bd(
  data               = tobacco, 
  conf.level         = 0.95, 
  plotTitle          = "Effect of Preparation on Lesions",
) + opts(panel.background = theme_rect(fill = "grey95", colour = NA))

granova.ds.bd(
  data               = anorexia, 
  conf.level         = 0.95, 
  plotTitle          = "Effect of Therapy on Anorexic Patients",
) + opts(panel.background = theme_rect(fill = "grey95", colour = NA))



# p + geom_text(aes(label = caseid), data = testFrame1, size = I(3), vjust = -1)
