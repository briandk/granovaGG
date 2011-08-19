### See discussion of anorexia graphic in EDSAG, J. Statistics Ed.
data(anorexia.sub)

granova.ds(anorexia.sub, revc = TRUE, 
  main = "Assessment Plot for weights to assess Family Therapy treatment 
  for Anorexia Patients")
# If labels for four unusual points at lower left are desired:
granova.ds(anorexia.sub, revc = TRUE, 
  main = "Assessment Plot for weights to assess Family Therapy treatment 
  for Anorexia Patients", ident = TRUE)


## See discussion of blood lead graphic in EDSAG, J. Statistics Ed.
data(blood_lead)

granova.ds(blood_lead, sw = .1, 
   main = "Dependent Sample Assessment Plot
   Blood Lead Levels of Matched Pairs of Children")