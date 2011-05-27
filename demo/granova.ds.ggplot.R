data(blood_lead)
granova.ds(blood_lead)
granova.ds.ggplot(
  data       = blood_lead, 
  conf.level = 0.95, 
  main = "Lead Levels in the Blood of Children"
) 

data(anorexia.sub)
granova.ds(anorexia.sub)
anorexia.sub$caseid <- 1:length(anorexia.sub[ , 1])

granova.ds.ggplot(
  data       = anorexia.sub, 
  main = "Effect of Therapy on Anorexic Patients",
  plot.theme = "theme_granova_1w"
) 

granova.ds.ggplot(
  data       = anorexia.sub, 
  conf.level = 0.95, 
  main = "Effect of Therapy on Anorexic Patients"
) 

# Once again, but with case labels.
p <- granova.ds.ggplot(
  data       = anorexia.sub, 
  main = "Effect of Therapy on Anorexic Patients",
) 

caseLabels <- geom_text(
                aes(label = caseid), 
                  data    = anorexia.sub, 
                  size    = I(3), 
                  vjust   = -1,
                  alpha   = I(3/4)
)

p + caseLabels


