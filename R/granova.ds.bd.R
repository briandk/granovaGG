# Required Libraries
library(ggplot2)
library(DAAG) # contains the pair65 data

# Loading in the data
data(pair65)
str(pair65)

# Defining the Function

granova.ds.bd <- function(
                   data                      = pair65, 
                   southwestPlotOffsetFactor = 0.4,
                   northeastPlotOffsetFactor = 0.5,
                   title                   = "Dependent Sample Scatterplot",
                   conf.level                = 0.95
                 ) 
                 {
  dd <- data.frame(
          xvals  = data[ , 1], 
          yvals  = data[ , 2],
          effect = (data[ , 2]  - data[ , 1])
        )  

  # Computing Statistics for the Confidence Band
  effectQuantiles <- quantile(dd$effect, probs = c(0, 0.025, 0.5, 0.975, 1))

  dsttest <- t.test(dd$yvals, dd$xvals, 
                     paired     = TRUE,
                     conf.level = conf.level)

  meanTreatmentEffect  <- dsttest$estimate
  upperTreatmentEffect <- dsttest$conf.int[1]
  lowerTreatmentEffect <- dsttest$conf.int[2]

  # Setting the graphical bounds
  aggregateDataRange  <- c(range(dd$xvals), range(dd$yvals))
  extrema             <- c(max(aggregateDataRange), min(aggregateDataRange))    
  squareDataRange     <- max(extrema) - min(extrema)
  
  lowerGraphicalBound <- min(extrema) - (1.2 * northeastPlotOffsetFactor * squareDataRange)
  upperGraphicalBound <- max(extrema) + (0.5 * southwestPlotOffsetFactor * squareDataRange)
  graphicalBounds     <- c(lowerGraphicalBound, upperGraphicalBound)

  crossbowIntercept   <- mean(graphicalBounds) + min(graphicalBounds)
  shadowOffset        <- squareDataRange / 60

  ## Computing point shadows
  xshadow <- ((-dd$effect + crossbowIntercept) / 2) + shadowOffset
  yshadow <- (xshadow) + (dd$effect)

  # I have to name the resultant dataframe variables as "xvals" and "yvals" so
  # that the subsequent geom_point(data = ddshadow) can inherit the dd dataframe
  # column names and plot correctly (Wickham, ggplot2 book, p. 63)
  ddshadow <- data.frame(xvals = xshadow, yvals = yshadow)
  
  ## Computing Point Trails
  ddtrails <- data.frame(
                xTrailStart = dd$xvals,
                yTrailStart = dd$yvals,
                xTrailEnd   = xshadow,
                yTrailEnd   = yshadow
              )
  
  # Setting up the ggplot object
  p <- ggplot(aes_string(x = "xvals", y = "yvals"), 
                data = dd)
  
  # Adding the treatment effect line
  p <- p + geom_abline(
                       intercept = meanTreatmentEffect,
                       slope     = 1,
                       color     = "red",
                       alpha     = I(1/2),
                       size      = I(1),
                       title     = "main effect"
           )

  
  # Plotting the raw data
  p <- p + geom_point(size = I(3)) + xlim(graphicalBounds) + ylim(graphicalBounds)

  # Adding the y=x line
  p <- p + geom_abline(slope = 1, intercept = 0)

  # Forcing coordinates to be equal
  p <- p + coord_equal()

  # Adding a rugplot
  p <- p + geom_rug(
             alpha = I(2/3),
             color = "steelblue"
           )
  
  # Adding mean marks
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
           
  # Adding the perpendicular crossbow
  p <- p + geom_abline(
              aes_string(
                intercept = I(crossbowIntercept),
                slope     = -1                
              ),
              alpha     = I(1/2)
            )
              
  

  # Adding group mean lines
  # p <- p + geom_hline(
  #                     yintercept = mean(dd$yvals), 
  #                     colour     = "red",
  #                     alpha      = 1/2,
  #                     linetype   = 3
  #          )
  #                   
  # p <- p + geom_vline(
  #                     xintercept = mean(dd$xvals), 
  #                     colour     = "red",
  #                     alpha      = 1/2,
  #                     linetype   = 3
  #          ) 
  # Adding the 95% Confidence band
  p <- p + geom_segment(
            aes_string(
              x    = ((crossbowIntercept - lowerTreatmentEffect) / 2) 
                      - shadowOffset,
              y    = ((crossbowIntercept + lowerTreatmentEffect) / 2) 
                      - shadowOffset,
              xend = ((crossbowIntercept - upperTreatmentEffect) / 2) 
                      - shadowOffset,
              yend = ((crossbowIntercept + upperTreatmentEffect) / 2) 
                      - shadowOffset

            ), size  = I(2),
               color = "darkgreen",
               alpha = I(2/3)

          )
                     
  # Adding point shadows
  p <- p + geom_point(
             data  = ddshadow, 
             color = "black", 
             size  = I(3),
             alpha = I(1/4) 
           )

  # Adding the point trails
  p <- p + geom_segment(
             aes(
               x        = xTrailStart,
               y        = yTrailStart,
               xend     = xTrailEnd,
               yend     = yTrailEnd
             ),
             data     = ddtrails,
             size     = I(1/3),
             color    = "black",
             linetype = 1,
             alpha    = I(1/6)              
           ) 
  
  # Removing the gridlines and background
  p <- p +
    opts(panel.grid.major = theme_blank()) +  
    opts(panel.grid.minor = theme_blank()) +
    opts(panel.background = theme_blank()) + 
    opts(axis.line = theme_segment()) +
    opts(title = paste(title)) 

  return(p)
}
