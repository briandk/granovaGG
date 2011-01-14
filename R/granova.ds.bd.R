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
      meanTreatmentEffect  = as.numeric(tTest$estimate),
      upperTreatmentEffect = as.numeric(tTest$conf.int[1])
      meanDifferenceRound  = round(meanTreatmentEffect, digits = 2)
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
  dsp$graphic$aggregateDataRange  <- c(range(getXs(dsp$data)), range(getYs(dsp$data)))
  dsp$graphic$extrema             <- c(max(dsp$graphic$aggregateDataRange), min(dsp$graphic$aggregateDataRange))    
  dsp$graphic$squareDataRange     <- max(dsp$graphic$extrema) - min(dsp$graphic$extrema)
  
  dsp$graphic$lowerGraphicalBound <- min(dsp$graphic$extrema) - (1.2 * northeastPlotOffsetFactor * dsp$graphic$squareDataRange)
  dsp$graphic$upperGraphicalBound <- max(dsp$graphic$extrema) + (0.5 * southwestPlotOffsetFactor * dsp$graphic$squareDataRange)
  dsp$graphic$graphicalBounds     <- c(dsp$graphic$lowerGraphicalBound, dsp$graphic$upperGraphicalBound)

  dsp$graphic$crossbowIntercept   <- mean(dsp$graphic$graphicalBounds) + min(dsp$graphic$graphicalBounds)
  dsp$graphic$shadowOffset        <- dsp$graphic$squareDataRange / 50
  
  dsp$shadows <- getShadows(dsp)
  dsp$trails  <- getTrails(dsp)
  print( str(dsp) )
}
 
 xxxx <- function() { 
  
  # Now, we use grammar to build the plot layer
  # by layer. Because of the way ggplot2 creates plot objects, layers can be
  # added to a plot p simply by calling "p <- p + newLayer", so for now you'll
  # see that structure of code throughout.
  
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

