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
    return (   t.test(data[, 2], 
                      data[, 1], 
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
    xShadow <- ( (-dsp$effect + mean(dsp$graphic$graphicalBounds) + min(dsp$graphic$graphicalBounds)) / 2) + dsp$graphic$shadowOffset
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

  getCrossbow <- function(dsp) {
    crossbow <- data.frame(
      x    = min(dsp$shadows$xShadow),
      y    = max(dsp$shadows$yShadow),
      xend = max(dsp$shadows$xShadow),
      yend = min(dsp$shadows$yShadow)
    )
    return (crossbow)
  }

  getGraphicsParams <- function(dsp) {
    .aggregateDataRange  <- c(range(getXs(dsp$data)), range(getYs(dsp$data)))
    .extrema             <- c(max(.aggregateDataRange), min(.aggregateDataRange))    
    .squareDataRange     <- max(.extrema) - min(.extrema)
    .lowerGraphicalBound <- min(.extrema) - (1.2 * northeastPlotOffsetFactor * .squareDataRange)
    .upperGraphicalBound <- max(.extrema) + (0.5 * southwestPlotOffsetFactor * .squareDataRange)
    .graphicalBounds     <- c(.lowerGraphicalBound, .upperGraphicalBound)
    .shadowOffset        <- .squareDataRange / 50
    
    return ( list(
      aggregateDataRange  = .aggregateDataRange,
      extrema             = .extrema,
      squareDataRange     = .squareDataRange,    
      lowerGraphicalBound = .lowerGraphicalBound,
      upperGraphicalBound = .upperGraphicalBound,
      graphicalBounds     = .graphicalBounds,  
      shadowOffset        = .shadowOffset      
    ) )
  }

  getGraphicsText <- function(dsp) {
    .meanDifferenceRound <- round(dsp$stats$meanTreatmentEffect, digits = 2)
    .CIBandText          <- paste(100 * conf.level, "% CI", sep = "")
    .meanDifferenceText  <- paste("Mean Diff. =", .meanDifferenceRound)
    
    return ( list(
      meanDifferenceRound = .meanDifferenceRound,
      CIBandText          = .CIBandText,         
      meanDifferenceText  = .meanDifferenceText 
      )
    )  
  }
  
  # We're going to build the plot in several pieces. First, we compute
  # statistics on the data passed in, and use them to define square graphical
  # bounds for the viewing window. 
    
  dsp <- list( data = data )

  dsp$effect <- getYs(dsp$data) - getXs(dsp$data)
  
  dsp$ttest <- getTtest(dsp$data, conf.level)
  
  dsp$stats <- getEffectQuantiles(dsp$ttest)
  
  dsp$text  <- getGraphicsText(dsp)

  ## Setting the graphical bounds
  dsp$graphic <- getGraphicsParams(dsp)
  
  dsp$shadows <- getShadows(dsp)
  
  dsp$graphic$crossbow <- getCrossbow(dsp)
  
  dsp$trails  <- getTrails(dsp)

  # Now, we use grammar to build the plot layer
  # by layer. Because of the way ggplot2 creates plot objects, layers can be
  # added to a plot p simply by calling "p <- p + newLayer", so for now you'll
  # see that structure of code throughout.
  
  createGgplot <- function(dsp) {
    p <- ggplot( 
           aes_string(
             x = names(dsp$data)[1], 
             y = names(dsp$data)[2]
            ),
            data = dsp$data
          )
  }
      
  treatmentLine <- function (dsp) {
    return( geom_abline(
                     aes_string(
                       intercept = dsp$stats$meanTreatmentEffect,
                       slope     = 1
                     ),
                     alpha = I(1/2),
                     size  = I(1),
            )
    )
  }
  
  rawData <- function (dsp) {
    rawData <- geom_point(size = I(3))
    return (rawData)
  }
  
  identityLine <- function() {
    return (geom_abline(slope = 1, intercept = 0))
  }
  
  scaleX <- function (dsp) {
    return (scale_x_continuous(limits = dsp$graphic$graphicalBounds))
  }
  
  scaleY <- function (dsp) {
    return (scale_y_continuous(limits = dsp$graphic$graphicalBounds))
  }
  
  rugPlot <- function (dsp) {
    return(
      geom_rug(
        alpha = I(2/3),
        color = "steelblue",
        data  = dsp$data
      )  
    )
  }
  
  meanMarks <- function (dsp) {
    meanMarks <- geom_rug(
      aes_string(
        x = mean(dsp$data[ , 1]),
        y = mean(dsp$data[ , 2])
      ),
      data  = dsp$data,
      color = "red",
      size  = I(3/2),
      alpha = I(2/3)
    )
    
    return (meanMarks)
  }
  
  crossbow <- function (dsp) {
    crossbow <- geom_segment(
      aes(
        x     = x,
        y     = y,
        xend  = xend,
        yend  = yend
      ), 
      data  = dsp$graphic$crossbow,
      alpha = I(1/2)
    )  
      
    return (crossbow)
  }
  
  p <- createGgplot(dsp)
  
  p <- p + treatmentLine(dsp)
  
  p <- p + rawData(dsp)
  
  p <- p + identityLine()

  p <- p + scaleX(dsp) + scaleY(dsp)
  
  p <- p + rugPlot(dsp)
  
  p <- p + meanMarks(dsp)

  p <- p + crossbow(dsp)


}

  # placed here to keep working code, above, 
  # separate from code waiting to be updated, below
  
xxx <- function() {

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
