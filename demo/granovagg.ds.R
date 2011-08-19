### See discussion of anorexia graphic in EDSAG, J. Statistics Ed.
data(anorexia.sub)

granovagg.ds(anorexia.sub, revc = TRUE, 
  main = "Assessment Plot for weights to assess Family Therapy treatment 
  for Anorexia Patients")
# If labels for four unusual points at lower left are desired:
granovagg.ds(anorexia.sub, revc = TRUE, 
  main = "Assessment Plot for weights to assess Family Therapy treatment 
  for Anorexia Patients", ident = TRUE)


## See discussion of blood lead graphic in EDSAG, J. Statistics Ed.
data(blood_lead)

granovagg.ds(blood_lead, sw = .1, 
   main = "Dependent Sample Assessment Plot
   Blood Lead Levels of Matched Pairs of Children")
