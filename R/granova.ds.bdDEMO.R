# For plot comparisons
library(granova)
library(ggplot2)
library(RColorBrewer)

source("granova.ds.bd.R")
R.Version()

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
) + opts(
      panel.background = theme_rect(fill = "grey95", colour = "white"),
      panel.grid.minor = theme_line(colour = "white", size = I(1/10)))

granova.ds.bd(
  data               = tobacco, 
  conf.level         = 0.95, 
  plotTitle          = "Effect of Preparation on Lesions",
) + 
geom_text(
  aes(label = caseid), 
    data    = tobacco, 
    size    = I(3), 
    vjust   = -1,
    alpha   = I(3/4)
)


granova.ds.bd(
  data               = anorexia, 
  conf.level         = 0.95, 
  plotTitle          = "Effect of Therapy on Anorexic Patients",
) + opts(
        panel.background = theme_rect(fill = "grey95", colour = "white"),
        panel.grid.minor = theme_line(colour = "white", size = I(1/5)))


granova.ds.bd(
  data               = anorexia, 
  conf.level         = 0.95, 
  plotTitle          = "Effect of Therapy on Anorexic Patients",
)



# p + geom_text(aes(label = caseid), data = testFrame1, size = I(3), vjust = -1)
