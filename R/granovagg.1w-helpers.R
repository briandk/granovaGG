IsFSignificant <- function(model.summary) {
  f.critical  <- qf(p   = 0.95, 
                    df1 = model.summary$fstatistic["numdf"], 
                    df2 = model.summary$fstatistic["dendf"]
                 )
  return(as.logical(
           model.summary$fstatistic["value"] > f.critical
         )
  )
}
