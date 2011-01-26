getSquareDataRange <- function (data) {
  .aggregateDataRange  <- c(range(getXs(dsp$data)), range(getYs(dsp$data)))
  .extrema             <- c(max(.aggregateDataRange), min(.aggregateDataRange))    
  .squareDataRange     <- max(.extrema) - min(.extrema)
  
}
