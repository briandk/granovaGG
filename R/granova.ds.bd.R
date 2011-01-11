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
  computeDependentSampleTtest <- function (data, conf.level) {
    return (   t.test(data[, 1], 
                      data[, 2], 
                      paired     = TRUE,
                      conf.level = conf.level
               )
    )
  }

  computeTreatmentEffectQuantiles <- function (data, conf.level) {
    dependentSampleTtestStatistics <- computeDependentSampleTtest(dd, conf.level)
  }

  computeDataPointShadows <- function () {
    xShadow <- ((-dd$effect + crossbowIntercept) / 2) + shadowOffset
    yShadow <- xShadow + dd$effect
    return (data.frame(xShadow, yShadow))
  }

  appendDataPointShadowsToDataFrame <- function () {
    dd <- data.frame(dd, computeDataPointShadows())
    return (dd)
  }

  computeDataPointTrails <- function () {
    ddtrails <- data.frame( xTrailStart = dd$xvals, 
                            yTrailStart = dd$yvals,
                            xTrailEnd   = dd$xShadow, 
                            yTrailEnd   = dd$yShadow
                )
    return (ddtrails)
  }

  appendDataPointTrailsToDataFrame <- function () {
    dd <- data.frame(dd, computeDataPointTrails())
    return (dd)
  }




  dd <- data.frame( xvals  = data[ , 1], 
                    yvals  = data[ , 2],
                    effect = data[ , 2]  - data[ , 1]
        )  


  # We're going to build the plot in several pieces. First, we compute
  # statistics on the data passed in, and use them to define square graphical
  # bounds for the viewing window. Then, we use grammar to build the plot layer
  # by layer. Because of the way ggplot2 creates plot objects, layers can be
  # added to a plot p simply by calling "p <- p + newLayer", so for now you'll
  # see that structure of code throughout.
    
  
  dependentSampleTtestStatistics <- computeDependentSampleTtest(dd, conf.level)
  
  meanTreatmentEffect  <- dependentSampleTtestStatistics$estimate
  upperTreatmentEffect <- dependentSampleTtestStatistics$conf.int[1]
  lowerTreatmentEffect <- dependentSampleTtestStatistics$conf.int[2]
  CIBandText           <- paste(100 * conf.level, "% CI", sep = "")
  meanDifferenceRound  <- round(meanTreatmentEffect, digits = 2)
  meanDifferenceText   <- paste("Mean Diff. =", meanDifferenceRound)

  ## Setting the graphical bounds
  aggregateDataRange  <- c(range(dd$xvals), range(dd$yvals))
  extrema             <- c(max(aggregateDataRange), min(aggregateDataRange))    
  squareDataRange     <- max(extrema) - min(extrema)
  
  lowerGraphicalBound <- min(extrema) - (1.2 * northeastPlotOffsetFactor * squareDataRange)
  upperGraphicalBound <- max(extrema) + (0.5 * southwestPlotOffsetFactor * squareDataRange)
  graphicalBounds     <- c(lowerGraphicalBound, upperGraphicalBound)

  crossbowIntercept   <- mean(graphicalBounds) + min(graphicalBounds)
  shadowOffset        <- squareDataRange / 50
  
  dd <- appendDataPointShadowsToDataFrame()
  dd <- appendDataPointTrailsToDataFrame()
  
  ## Setting up the ggplot object 
  p <- ggplot( aes(x = xvals, y = yvals), data = dd )
  
  ## Adding the treatment effect line. 
  # Here, I'm using a hack by specifying that treatmentLine is
  # built from a dataframe that contains the variable "Legend". The Confidence Interval will also be
  # built from a dataframe containing the variable "Legend", so that the title of the resulting
  # legend ends up being "Legend." The strategy here is to create self-contained dataframes (like
  # treatmentLine) for each object that should appear in the legend. The dataframes themselves hold
  # information for things like slopes and intercepts, etc. 
  
  treatmentLine <- data.frame( 
                     treatmentIntercept = meanTreatmentEffect, 
                     treatmentSlope     = 1, 
                     Legend             = factor(meanDifferenceText)
                   ) 
  
  p <- p + geom_abline(
             aes(
               intercept = treatmentIntercept, 
               slope     = treatmentSlope, 
               color     = Legend, 
             ),
             alpha = I(1/2), 
             size  = I(1),
             data  = treatmentLine
           )

  
  ## Plotting the raw data
  p <- p + geom_point(size = I(3)) + xlim(graphicalBounds) + ylim(graphicalBounds)

  ## Adding the y=x line
  p <- p + geom_abline(slope = 1, intercept = 0)

  ## Forcing coordinates to be equal
  p <- p + coord_equal()

  ## Adding a rugplot
  p <- p + geom_rug(
             alpha = I(2/3),
             color = "steelblue"
           )
  
  ## Adding mean marks
  p <- p + geom_rug(
             aes(
               x = mean(xvals),
               y = mean(yvals) 
             ),
             data  = dd,
             color = "red",
             size  = I(3/2),
             alpha = I(2/3)
           )  
           
  ## Adding the perpendicular crossbow
  p <- p + geom_abline(
              aes_string(
                intercept = I(crossbowIntercept),
                slope     = -1                
              ),
              alpha = I(1/2)
            )

  ## Adding the Confidence band    
  confidenceBand <- data.frame(
                      cx    = ((crossbowIntercept - lowerTreatmentEffect) / 2) 
                              - shadowOffset,
                      cy    = ((crossbowIntercept + lowerTreatmentEffect) / 2) 
                              - shadowOffset,
                      cxend = ((crossbowIntercept - upperTreatmentEffect) / 2) 
                              - shadowOffset,
                      cyend = ((crossbowIntercept + upperTreatmentEffect) / 2) 
                              - shadowOffset,
                      Legend = factor(CIBandText)
                    )

  p <- p + geom_segment(
               aes(
                 x     = cx,
                 y     = cy,
                 xend  = cxend,
                 yend  = cyend,                        
                 color = Legend
               ), 
            size  = I(2),
            alpha = I(2/3),
            data  = confidenceBand
          )
                     
  ## Adding point shadows
  p <- p + geom_point(
             aes(
               x = xShadow,
               y = yShadow
             ),
             data  = dd, 
             color = "black", 
             size  = I(3),
             alpha = I(1/4) 
           )

  ## Adding the point trails
  p <- p + geom_segment(
             aes(
               x        = xTrailStart,
               y        = yTrailStart,
               xend     = xTrailEnd,
               yend     = yTrailEnd
             ),
             data     = dd,
             size     = I(1/3),
             color    = "black",
             linetype = 1,
             alpha    = I(1/8)              
           ) 
  
  ## Adding a legend and title
  legendColors <- c("red", "darkgreen")
  p <- p + scale_color_manual(value = legendColors)
  p <- p + opts(title = plotTitle)
  
  ## Renaming the x and y scales
  print("Printing the Names of the x and y values")
  print(names(data)[1])
  print(names(data)[2])
  
  p <- p + xlab((names(data)[1]))
  p <- p + ylab((names(data)[2]))
  
  
  ## Removing the gridlines and background if the user asks
  if (produceBlankPlotObject == TRUE) {
    p <- p +
      opts(panel.grid.major = theme_blank()) +  
      opts(panel.grid.minor = theme_blank()) +
      opts(panel.background = theme_blank()) + 
      opts(axis.line = theme_segment())
  }
  
  return(p)
}

