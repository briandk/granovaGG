## Defining the Function

granova.ds.bd <- function( data                      = null, 
                           southwestPlotOffsetFactor = 0.4,
                           northeastPlotOffsetFactor = 0.5,
                           plotTitle                 = "Dependent Sample Scatterplot",
                           conf.level                = 0.95,
                           produceBlankPlotObject    = TRUE
                 ) 

{

  marshalData <- function () {

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

    getStats <- function (data, conf.level) {
      tTest <- getTtest(data, conf.level)
      return(  data.frame(
                lowerTreatmentEffect = as.numeric(tTest$conf.int[2]),
                meanTreatmentEffect  = as.numeric(tTest$estimate[1]),
                upperTreatmentEffect = as.numeric(tTest$conf.int[1]),
                tStatistic           = as.numeric(tTest$statistic[1])
              )
      )    
    }

    getShadows <- function (dsp) {
      xShadow <- (-dsp$effect + dsp$parameters$anchor) / 2 + dsp$parameters$shadowOffset
      yShadow <- xShadow + dsp$effect
      return (data.frame(xShadow, yShadow))
    }

    getTrails <- function (dsp) {
      return(  data.frame(
                 xTrailStart = getXs(dsp$data), 
                 yTrailStart = getYs(dsp$data),
                 xTrailEnd   = getXs(dsp$shadow), 
                 yTrailEnd   = getYs(dsp$shadow)
               )
      )
    }


    getXs <- function (data) {
      return( data[, 1])
    }

    getYs <- function (data) {
      return( data[, 2])
    }

    getCrossbow <- function (dsp) {
      return(  data.frame(
        x    = min(dsp$shadows$xShadow) - dsp$parameters$shadowOffset,
        y    = max(dsp$shadows$yShadow) - dsp$parameters$shadowOffset,
        xend = max(dsp$shadows$xShadow) - dsp$parameters$shadowOffset,
        yend = min(dsp$shadows$yShadow) - dsp$parameters$shadowOffset
        )
      )  
    }

    getCIBand <- function (dsp) {
      CIBand <- data.frame(
        cx    = ((dsp$parameters$anchor - dsp$stats$lowerTreatmentEffect) / 2) - 3 * (dsp$parameters$shadowOffset),
        cy    = ((dsp$parameters$anchor + dsp$stats$lowerTreatmentEffect) / 2) - 3 * (dsp$parameters$shadowOffset),
        cxend = ((dsp$parameters$anchor - dsp$stats$upperTreatmentEffect) / 2) - 3 * (dsp$parameters$shadowOffset),
        cyend = ((dsp$parameters$anchor + dsp$stats$upperTreatmentEffect) / 2) - 3 * (dsp$parameters$shadowOffset),
        color = factor(paste(100 * conf.level, "% CI", " (t = ", round(dsp$stats$tStatistic, digits = 2), ")", sep =""))
        )
      
      return (CIBand)
    }
  
    getGraphicsParams <- function (dsp) {
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

    getTreatmentLine <- function (dsp) {
      treatmentLine <- data.frame(
        intercept      = dsp$stats$meanTreatmentEffect,
        slope          = 1,
        color          = factor(paste("Mean Diff. =", round(dsp$stats$meanTreatmentEffect, digits = 2)))
      )
    
      return (treatmentLine)
    }
  
    
    dsp <- list( data = data )

    dsp$effect <- getYs(dsp$data) - getXs(dsp$data)
    
    dsp$stats <- getStats(dsp$data, conf.level)
  
    dsp$parameters <- getGraphicsParams(dsp)
  
    dsp$shadows <- getShadows(dsp)
  
    dsp$crossbow <- getCrossbow(dsp)
  
    dsp$CIBand <- getCIBand(dsp)
  
    dsp$treatmentLine <- getTreatmentLine(dsp)
  
    dsp$trails  <- getTrails(dsp)
    
    return(dsp)
  }






  generateDSggplot <- function (dsp) {

    # Now, we use ggplot2 grammar to build the plot layer
    # by layer. Because of the way ggplot2 creates plot objects, layers can be
    # added to a plot p simply by calling "p <- p + newLayer"
    
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
                       aes(
                         intercept = intercept,
                         slope     = slope,
                         color     = color
                       ),
                       alpha = I(1/2),
                       size  = I(1),
                       data  = dsp$treatmentLine
              )
      )
    }
  
    rawData <- function (dsp) {
      rawData <- geom_point(size = dsp$parameters$pointsize)
      return (rawData)
    }
  
    identityLine <- function() {
      return (geom_abline(slope = 1, intercept = 0))
    }
  
    scaleX <- function (dsp) {
      return (scale_x_continuous(limits = dsp$parameters$bounds, expand = dsp$parameters$expand))
    }
  
    scaleY <- function (dsp) {
      return (scale_y_continuous(limits = dsp$parameters$bounds, expand = dsp$parameters$expand))
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
        data  = dsp$crossbow,
        alpha = I(3/4)
      )  
      
      return (crossbow)
    }
  
    CIBand <- function (dsp) {
      CIBand <- geom_segment(
        aes(
         x     = cx,
         y     = cy,
         xend  = cxend,
         yend  = cyend,                        
         color = color
        ), 
                size  = I(2),
                alpha = I(2/3),
                data  = dsp$CIBand
              )
   
     return (CIBand) 
    }
  
    shadows <- function (dsp) {
      shadows <- geom_point(
        aes(
          x = xShadow,
          y = yShadow
        ),
        data  = dsp$shadow, 
        color = "black", 
        size  = dsp$parameters$pointsize,
        alpha = I(1/4) 
      )
    
      return (shadows)
    }
  
    trails <- function (dsp) {
      trails <- geom_segment(
        aes(
          x        = xTrailStart,
          y        = yTrailStart,
          xend     = xTrailEnd,
          yend     = yTrailEnd
        ),
        data     = dsp$trails,
        size     = I(1/3),
        color    = "black",
        linetype = 1,
        alpha    = I(1/8)              
      ) 
    
      return (trails)
    }
  
    legend <- function (dsp) {
      colors <- c("red", "darkgreen")
    
      return (scale_color_manual(value = colors))
    }
  
    title <- function () {
      return (opts(title = plotTitle))
    }
  
    blank <- function () {
      return( opts(
                panel.grid.major = theme_blank(),
                panel.grid.minor = theme_blank(),
                panel.background = theme_blank(),
                axis.line = theme_segment()
              )
      )
    }
  
    p <- createGgplot(dsp)
  
    p <- p + treatmentLine(dsp)
  
    p <- p + rawData(dsp)
  
    p <- p + blank()
  
    p <- p + identityLine()

    p <- p + scaleX(dsp) + scaleY(dsp)
  
    p <- p + rugPlot(dsp)
  
    p <- p + meanMarks(dsp)

    p <- p + crossbow(dsp)

    p <- p + CIBand(dsp)
  
    p <- p + shadows(dsp)

    p <- p + trails(dsp)
  
    p <- p + legend(dsp)
  
    p <- p + coord_equal()
  
    p <- p + title()
  
    return (p)
  }
  
  
  
  
  
  return( generateDSggplot( dsp=marshalData() ) )
  
}
