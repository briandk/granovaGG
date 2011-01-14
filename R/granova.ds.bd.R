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
    xShadow <- ( (-dsp$effect + dsp$graphic$crossbow$intercept) / 2) + dsp$graphic$shadowOffset
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

  getCrossbow <- function(bounds) {
    crossbow <- data.frame(
      intercept = mean(bounds) + min(bounds),
      slope     = -1
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
    .crossbow            <- getCrossbow(.graphicalBounds)
    .shadowOffset        <- .squareDataRange / 50
    
    return ( list(
      aggregateDataRange  = .aggregateDataRange,
      extrema             = .extrema,
      squareDataRange     = .squareDataRange,    
      lowerGraphicalBound = .lowerGraphicalBound,
      upperGraphicalBound = .upperGraphicalBound,
      graphicalBounds     = .graphicalBounds,  
      crossbow            = .crossbow, 
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
  
  p <- createGgplot(dsp)
  ## Adding the treatment effect line. 
  # Here, I'm using a hack by specifying that treatmentLine is
  # built from a dataframe that contains the variable "Legend". The Confidence Interval will also be
  # built from a dataframe containing the variable "Legend", so that the title of the resulting
  # legend ends up being "Legend." The strategy here is to create self-contained dataframes (like
  # treatmentLine) for each object that should appear in the legend. The dataframes themselves hold
  # information for things like slopes and intercepts, etc. 
    
  layerTreatmentLine <- function (plot) {
    plot <- plot + geom_abline(
                     aes(
                       intercept = dsp$stats$meanTreatmentEffect,
                       slope     = 1,
                       color     = factor("Mean Diff. = ")
                     ),
                     alpha = I(1/2),
                     size  = I(1),
    )
  }
  
  p <- layerTreatmentLine(p)

  ## Plotting the raw data
  p <- p + geom_point(size = I(3)) + xlim(dsp$graphic$graphicalBounds) + ylim(dsp$graphic$graphicalBounds)

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

