data(blood_lead)

granova.ds(blood_lead)

granova.ds.ggplot(
  data               = blood_lead, 
  conf.level         = 0.95, 
  plotTitle          = "Lead Levels in the Blood of Children (theme_granova)"
) 




data(anorexia.sub)

granova.ds(anorexia.sub)

anorexia.sub$caseid <- 1:length(anorexia.sub[ , 1])

granova.ds.ggplot(
  data               = anorexia.sub, 
  plotTitle          = "Effect of Therapy on Anorexic Patients (theme_granova)",
  plotTheme          = theme_granova()
) 

# Same plot as above, but with the default ggplot2 theme applied.
granova.ds.ggplot(
  data               = anorexia.sub, 
  conf.level         = 0.95, 
  plotTitle          = "Effect of Therapy on Anorexic Patients (theme_gray)",
  plotTheme          = theme_gray()
) 

# Once again, but with case labels.
p <- granova.ds.ggplot(
  data               = anorexia.sub, 
  plotTitle          = "Effect of Therapy on Anorexic Patients (theme_granova)",
  plotTheme          = theme_granova()
) 

caseLabels <- geom_text(
                aes(label = caseid), 
                  data    = anorexia.sub, 
                  size    = I(3), 
                  vjust   = -1,
                  alpha   = I(3/4)
)

p + caseLabels


