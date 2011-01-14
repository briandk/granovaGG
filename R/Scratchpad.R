## Defining the Function

granova.ds.bd <- function( data                      = null, 
                           southwestPlotOffsetFactor = 0.4,
                           northeastPlotOffsetFactor = 0.5,
                           plotTitle                 = "Dependent Sample Scatterplot",
                           conf.level                = 0.95,
                           produceBlankPlotObject    = TRUE
                 ) 

{

  ## Functions should be grouped so that they're easy to locate and the
  ## rest of your code can be read from beginning to end.

  ## Computing t-test Statistics for the Confidence Band and Mean Difference
  getTtest <- function (data, conf.level) {
    return (   t.test(data[, 1], 
                      data[, 2], 
                      paired     = TRUE,
                      conf.level = conf.level
               )
    )
  }

  getEffectQuantiles <- function (tTest) {
    effectQuantiles <- list(
      lowerTreatmentEffect = as.numeric(tTest$conf.int[2]),
      meanTreatmentEffect  = as.numeric(tTest$estimate[1]),
      upperTreatmentEffect = as.numeric(tTest$conf.int[1])
    )  
    return(effectQuantiles)
  }

  getShadows <- function (dsp) {
    xShadow <- ( (-dsp$effect + dsp$graphic$crossbowIntercept) / 2) + dsp$graphic$shadowOffset
    yShadow <- xShadow + dsp$effect
    return (data.frame(xShadow, yShadow))
  }

  getTrails <- function (dsp) {
    ddtrails <- data.frame( xTrailStart = getXs(dsp$data), 
                            yTrailStart = getYs(dsp$data),
                            xTrailEnd   = getXs(dsp$shadow), 
                            yTrailEnd   = getYs(dsp$shadow)
                )
    return (ddtrails)
  }


  getXs <- function (data) {
    return( data[, 1])
  }

  getYs <- function (data) {
    return( data[, 2])
  }

  # getGraphicalBounds <- function(dsp) {
  #   aggregateDataRange  = c(range(getXs(dsp$data)), range(getYs(dsp$data)))
  #   extrema             = c(max(aggregateDataRange), min(aggregateDataRange))    
  #   squareDataRange     = max(extrema) - min(extrema)
  #   
  #   list(
  #     lowerGraphicalBound = min(extrema) - (1.2 * northeastPlotOffsetFactor * squareDataRange),
  #     upperGraphicalBound = max(extrema) + (0.5 * southwestPlotOffsetFactor * squareDataRange)
  #   )  
  #   graphicalBounds     = c(lowerGraphicalBound, upperGraphicalBound)
  #   
  #   return(graphicalBounds)
  # }

  getGraphicsParams <- function(dsp) {
    graphicsParams <- list(
      aggregateDataRange  = c(range(getXs(dsp$data)), range(getYs(dsp$data))),
      extrema             = c(max(aggregateDataRange), min(aggregateDataRange)),    
      squareDataRange     = max(extrema) - min(extrema),
      lowerGraphicalBound = min(extrema) - (1.2 * northeastPlotOffsetFactor * squareDataRange),
      upperGraphicalBound = max(extrema) + (0.5 * southwestPlotOffsetFactor * squareDataRange),
      graphicalBounds     = c(lowerGraphicalBound, upperGraphicalBound),
      crossbowIntercept   = mean(graphicalBounds) + min(graphicalBounds),
      shadowOffset        = squareDataRange / 50
    )
  }

  # We're going to build the plot in several pieces. First, we compute
  # statistics on the data passed in, and use them to define square graphical
  # bounds for the viewing window. 
    
  dsp <- list( data = data )

  dsp$effect <- getYs(dsp$data) - getXs(dsp$data)
  
  dsp$ttest <- getTtest(dsp$data, conf.level)
  
  dsp$stats <- getEffectQuantiles(dsp$ttest)

  CIBandText           <- paste(100 * conf.level, "% CI", sep = "")
  meanDifferenceText   <- paste("Mean Diff. =", dsp$stats$meanDifferenceRound)

  ## Setting the graphical bounds
  
  dsp$graphic <- getGraphicsParams(dsp)
  
  return(dsp$graphic)
}

library(ggplot2)
library(DAAG)

granova.ds.bd(pair65)