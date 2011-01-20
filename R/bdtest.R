library(ggplot2)

getTestData <- function () {
  set.seed(1003)
  x <- rnorm(mean = 42, n = 25)
  y <- rnorm(mean = 45, n = 25)
  
  return( data.frame(preTest = x, postTest = y) )
}

getXs <- function (data) {
  return( data[, 1])
}

getYs <- function (data) {
  return( data[, 2])
}

getEffect <- function(dsp) {
  return (getXs(dsp$data) - getYs(dsp$data))
}

getGraphicsParams <- function (dsp) {
  southwestPlotOffsetFactor <- 0.4
  northeastPlotOffsetFactor <- 0.5
  
  .aggregateDataRange  <- c(range(getXs(dsp$data)), range(getYs(dsp$data)))
  .extrema             <- c(max(.aggregateDataRange), min(.aggregateDataRange))    
  .squareDataRange     <- max(.extrema) - min(.extrema)
  .lowerGraphicalBound <- min(.extrema) - (1.2 * northeastPlotOffsetFactor * .squareDataRange)
  .upperGraphicalBound <- max(.extrema) + (0.5 * southwestPlotOffsetFactor * .squareDataRange)
  .bounds              <- c(.lowerGraphicalBound, .upperGraphicalBound)
  .center              <- mean(.bounds)
  .crossbowAnchor      <- mean(.bounds) + min(.bounds)
  .shadowOffset        <- .squareDataRange / 50
  .expand              <- c(0.1, 0)

  return ( list(
    squareDataRange     = .squareDataRange,    
    bounds              = .bounds,  
    shadowOffset        = .shadowOffset,
    anchor              = .crossbowAnchor,
    expand              = .expand,
    pointsize           = I(2)      
  ) )
}


getShadows <- function (dsp) {
  xShadow <- (-dsp$effect + dsp$parameters$anchor) / 2 + dsp$parameters$shadowOffset
  yShadow <- xShadow + dsp$effect
  return (data.frame(xShadow, yShadow))
}

dsp <- list(data = getTestData())

dsp$effect     <- getEffect(dsp)
dsp$parameters <- getGraphicsParams(dsp)
dsp$shadow     <- getShadows(dsp)

str (dsp)

p <- ggplot(
          aes(
            x = preTest,
            y = postTest
          ),
          data = dsp$data
)

p <- p + geom_point()


p <- p + geom_point(aes(x = xShadow, y = yShadow), data = dsp$shadow)

p

