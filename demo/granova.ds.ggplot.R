# For plot comparisons
#library(granova)
#library(ggplot2)
#library(RColorBrewer)

#source("granova.ds.bd.R")

data(tobacco)
tobacco$caseid <- 1:length(tobacco[ , 1])

data(anorexia)
anorexia$caseid <- 1:length(anorexia[ , 1])

data(lead)
lead$caseid <- 1:length(lead[ , 1])

#pdf("testplots.pdf", onefile = TRUE)

granova.ds(lead[ , c(1,2)])
granova.ds(tobacco[ , c(1,2)])
granova.ds(anorexia[ , c(1,2)])

granova.ds.bd(
  data               = lead, 
  conf.level         = 0.95, 
  plotTitle          = "Lead Levels in the Blood of Children (theme_granova)"
) 

p <- granova.ds.bd(
  data               = tobacco, 
  plotTitle          = "Effect of Preparation on Lesions (theme_granova)"
) 

caseLabels <- geom_text(
                aes(label = caseid), 
                  data    = tobacco, 
                  size    = I(3), 
                  vjust   = -1,
                  alpha   = I(3/4)
)

p + caseLabels

granova.ds.bd(
  data               = anorexia, 
  plotTitle          = "Effect of Therapy on Anorexic Patients (theme_granova)",
  plotTheme          = theme_granova()
) 

granova.ds.bd(
  data               = anorexia, 
  conf.level         = 0.95, 
  plotTitle          = "Effect of Therapy on Anorexic Patients (theme_gray)",
  plotTheme          = theme_gray()
) # Same plot as above, but with the default ggplot2 theme applied.

#dev.off()



