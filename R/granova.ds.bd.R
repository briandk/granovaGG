# Required Libraries
library(ggplot2)
library(DAAG) # contains the pair65 data

# Loading in the data
data(pair65)
str(pair65)

# Defining the Function

granova.ds.bd <- function(
                   data = pair65, 
                   southwestPlotOffsetFactor = 0.4,
                   northeastPlotOffsetFactor = 0.5
                 ) 
                 {
  dd <- data.frame(
          xvals  = data[ , 1], 
          yvals  = data[ , 2],
          effect = (data[ , 2]  - data[ , 1])
        )  

  # Computing Some Statistics
  effectQuantiles <- quantile(dd$effect, probs = c(0, 0.025, 0.5, 0.975, 1))

  dsttest <- t.test(dd$yvals, dd$xvals, 
                     paired     = TRUE,
                     conf.level = 0.95)

  meanTreatmentEffect  <- dsttest$estimate
  upperTreatmentEffect <- dsttest$conf.int[1]
  lowerTreatmentEffect <- dsttest$conf.int[2]

  # Setting the graphical bounds
  aggregateDataRange <- c(range(dd$xvals), range(dd$yvals))
  extrema  <- c(max(aggregateDataRange), min(aggregateDataRange))
    
  offset   <- (max(extrema) - min(extrema)) / 10
  squareDataRange <- max(extrema) - min(extrema)
  
  lowerGraphicalBound <- min(extrema) - (1.2 * northeastPlotOffsetFactor * squareDataRange)
  upperGraphicalBound <- max(extrema) + (0.5 * southwestPlotOffsetFactor * squareDataRange)
  
  graphicalBounds <- c(lowerGraphicalBound, upperGraphicalBound)

  crossbowIntercept <- mean(graphicalBounds) + min(graphicalBounds)
  shadowOffset <- offset/6

  ## Computing point shadows
  xshadow <- (((dd$xvals - dd$yvals) + crossbowIntercept) /2) + shadowOffset
  yshadow <- (xshadow) + (dd$yvals - dd$xvals)

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
  
  ## Trying to get the same plot in ggplot2
  p <- ggplot(aes_string(x = "xvals", y = "yvals"), 
                data = dd)
              
  p <- p + geom_point(size = I(3)) + xlim(graphicalBounds) + ylim(graphicalBounds)

  # Adding the y=x line
  p <- p + geom_abline(slope = 1, intercept = 0)

  # Forcing coordinates to be equal
  p <- p + coord_equal()

  # Adding a rugplot
  p <- p + geom_rug(alpha = I(2/3))  
           
  # Deliberate re-adding the same perpendicular crossbow as a full line
  p <- p + geom_abline(
                intercept = crossbowIntercept,
                alpha     = I(1/2),
                slope     = -1                
           )
  

  # Adding group mean lines
  p <- p + geom_hline(
                      yintercept = mean(dd$yvals), 
                      colour     = "red",
                      alpha      = 1/2,
                      linetype   = 3
           )
                    
  p <- p + geom_vline(
                      xintercept = mean(dd$xvals), 
                      colour     = "red",
                      alpha      = 1/2,
                      linetype   = 3
           ) 
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
               alpha = I(1),

          )

  # Adding the treatment effect line
  p <- p + geom_abline(
                       intercept = meanTreatmentEffect,
                       slope     = 1,
                       color     = "red",
                       alpha     = 1,
                       linetype  = 2
           )
                     
  # Adding point shadows
  p <- p + geom_point(
             data  = ddshadow, 
             color = "black", 
             size  = I(3),
             alpha = I(1/2) 
           )

  # Adding the point trails
  p <- p + geom_segment(
             aes(
               x        = xTrailStart,
               y        = yTrailStart,
               xend     = xTrailEnd,
               yend     = yTrailEnd,
             ),
             data     = ddtrails,
             size     = I(1),
             color    = "black",
             linetype = 3,
             alpha    = I(1/4)              
           ) 
  
  # Removing the gridlines and background
  p <- p +
    opts(panel.grid.major = theme_blank()) +  
    opts(panel.grid.minor = theme_blank()) +
    opts(panel.background = theme_blank()) + 
    opts(axis.line = theme_segment()) +
    opts(title = "Dependent Sample Scatterplot for pair65 data")  

  return(p)
}
