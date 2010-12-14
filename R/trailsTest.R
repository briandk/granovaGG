library(ggplot2)

southwestPlotOffsetFactor <- 0.4
northeastPlotOffsetFactor <- 0.5

x <- rnorm(n = 25, mean = 457)
y <- rnorm(n = 25, mean = 459)

dd <- data.frame(xvals = x, yvals = y)

# Graphical Bounds Code
aggregateDataRange <- c(range(dd$xvals), range(dd$yvals))
extrema  <- c(max(aggregateDataRange), min(aggregateDataRange))
offset   <- (max(extrema) - min(extrema)) / 10
squareDataRange <- max(extrema) - min(extrema)
lowerGraphicalBound <- min(extrema) - (1.2 * northeastPlotOffsetFactor * squareDataRange)
upperGraphicalBound <- max(extrema) + (0.5 * southwestPlotOffsetFactor * squareDataRange)
graphicalBounds <- c(lowerGraphicalBound, upperGraphicalBound)
crossbowIntercept <- mean(graphicalBounds) + min(graphicalBounds)
shadowOffset <- offset/6


xshadow <- (((dd$xvals - dd$yvals) + crossbowIntercept) /2) + shadowOffset
yshadow <- (xshadow) + (dd$yvals - dd$xvals)

ddtrails <- data.frame(
              xTrailStart = dd$xvals,
              yTrailStart = dd$yvals,
              xTrailEnd   = xshadow,
              yTrailEnd   = yshadow
            )
            
p <- ggplot(aes(x = xvals, y = yvals), data = dd) + geom_point()

p <- p + geom_segment(
           aes(
             x        = xTrailStart,
             y        = yTrailStart,
             xend     = xTrailEnd,
             yend     = yTrailEnd
           ),
           data     = ddtrails,
           color    = "black",
           linetype = I(3),
           alpha    = I(1/4)           
         ) 
print(p)
