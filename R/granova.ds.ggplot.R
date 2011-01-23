## Defining the Function

granova.ds.ggplot <- function( data                      = NULL, 
                           plotTitle                 = "Dependent Sample Scatterplot",
                           conf.level                = 0.95,
                           plotTheme                 = theme_granova()
                 ) 

{
  
  getXs <- function (data) {
    return( data[, 1])
  }

  getYs <- function (data) {
    return( data[, 2])
  }

    getEffect <- function (dsp) {
      return( getXs(dsp$data) - getYs(dsp$data) )
    }
    
    getTtest <- function (data, conf.level) {
      return (   t.test(data[, 2], 
                        data[, 1], 
                        paired     = TRUE,
                        conf.level = conf.level
                 )
      )
    }
    
    getStats <- function (dsp, conf.level) {
      tTest <- getTtest(dsp$data, conf.level)
      return(  data.frame(
                lowerTreatmentEffect = as.numeric(tTest$conf.int[2]),
                meanTreatmentEffect  = as.numeric(tTest$estimate[1]),
                upperTreatmentEffect = as.numeric(tTest$conf.int[1]),
                tStatistic           = as.numeric(tTest$statistic[1])
              )
      )    
    }
    
    getGraphicsParams <- function (dsp) {
      .aggregateDataRange  <- c(range(getXs(dsp$data)), range(getYs(dsp$data)))
      .extrema             <- c(max(.aggregateDataRange), min(.aggregateDataRange))    
      .squareDataRange     <- max(.extrema) - min(.extrema)
      .southWestPadding    <- (65/100) * .squareDataRange
      .northEastPadding    <- (25/100) * .squareDataRange
      .lowerGraphicalBound <- min(.extrema) - .southWestPadding
      .upperGraphicalBound <- max(.extrema) + .northEastPadding
      .bounds              <- c(.lowerGraphicalBound, .upperGraphicalBound)
      .center              <- mean(.bounds)
      .crossbowAnchor      <- mean(.bounds) + min(.bounds)
      .shadowOffset        <- (1/100)*.squareDataRange
    
      return ( list(
        squareDataRange     = .squareDataRange,    
        bounds              = .bounds,  
        shadowOffset        = .shadowOffset,
        anchor              = .crossbowAnchor,
        pointsize           = I(2),
        meanLineSize        = I(1/10)      
      ) )
    }
    
    getShadows <- function (dsp) {
      xShadow <- (dsp$effect / 2) + 
                 (3 * dsp$parameters$bounds[1] + dsp$parameters$bounds[2]) / 4 + 
                 (4 * dsp$parameters$shadowOffset)
      yShadow <- xShadow - dsp$effect
      return (data.frame(xShadow, yShadow))
    }

    getCrossbow <- function (dsp) {
      return(  data.frame(
        x    = min(dsp$shadows$xShadow) - (2 * dsp$parameters$shadowOffset),
        y    = max(dsp$shadows$yShadow) - (2 * dsp$parameters$shadowOffset),
        xend = max(dsp$shadows$xShadow) - (2 * dsp$parameters$shadowOffset),
        yend = min(dsp$shadows$yShadow) - (2 * dsp$parameters$shadowOffset)
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
    
    getTreatmentLine <- function (dsp) {
      return( data.frame(
                intercept      = dsp$stats$meanTreatmentEffect,
                slope          = 1,
                color          = factor(paste("Mean Diff. =", round(dsp$stats$meanTreatmentEffect, digits = 2)))
              )
      )
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
    
    getColors <- function (dsp) {
      return(  list(
                 treatmentLine = "#542570",
                 rugplot       = "black",
                 meanLine      = "#542570",
                 CIBand        = "#33A02C",
                 crossbow      = "#377EB8"
               )
      )         
    }

    dsp               <- list( data = data )
    dsp$effect        <- getEffect(dsp)
    dsp$stats         <- getStats(dsp, conf.level)
    dsp$parameters    <- getGraphicsParams(dsp)
    dsp$shadows       <- getShadows(dsp)
    dsp$crossbow      <- getCrossbow(dsp)
    dsp$CIBand        <- getCIBand(dsp)
    dsp$treatmentLine <- getTreatmentLine(dsp)
    dsp$trails        <- getTrails(dsp)
    dsp$colors        <- getColors(dsp)
      
    

    # Because of the way ggplot2 creates plot objects, layers can be
    # added to a plot p simply by calling "p <- p + newLayer"

    
    initializeGgplot <- function(dsp) {
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
      rawData <- geom_point(
        size  = dsp$parameters$pointsize
      )
      return (rawData)
    }
  
    identityLine <- function() {
      return (
        geom_abline(
          slope     = 1, 
          intercept = 0,
          alpha     = I(3/4)
        )
      )
    }
  
    scaleX <- function (dsp) {
      return (scale_x_continuous(limits = dsp$parameters$bounds))
    }
  
    scaleY <- function (dsp) {
      return (scale_y_continuous(limits = dsp$parameters$bounds))
    }
  
    rugPlot <- function (dsp) {
      return(
        geom_rug(
          size  = I(1/2),
          alpha = I(1/3),
          color = dsp$colors$rugplot,
          data  = dsp$data
        )  
      )
    }
  
    xMeanLine <- function (dsp) {
      return( 
        geom_vline(
          xintercept = mean(getXs(dsp$data)),
          color      = dsp$colors$meanLine,
          size       = dsp$parameters$meanLineSize,
          alpha      = I(1)
        ) 
      )
    }
    
    yMeanLine <- function (dsp)  {
      return( 
        geom_hline(
          yintercept = mean(getYs(dsp$data)),
          color      = dsp$colors$meanLine,
          size       = dsp$parameters$meanLineSize,
          alpha      = I(1)
        ) 
      )
      
    }
  
    crossbow <- function (dsp) {
      crossbow <- geom_segment(
        aes(
          x     = x,
          y     = y,
          xend  = xend,
          yend  = yend
        ), 
        size  = I(3/4),
        alpha = I(3/4),
        color = dsp$colors$crossbow,
        data  = dsp$crossbow
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
        alpha    = I(1/10)              
      ) 
    
      return (trails)
    }
  
    legend <- function (dsp) {
      colors <- c(dsp$colors$treatmentLine, dsp$colors$CIBand)
    
      return (scale_color_manual(value = colors, name = ""))
    }
  
    title <- function () {
      return (opts(title = plotTitle))
    }
    
    theme <- function () {
      return (plotTheme)
    }
          
    p <- initializeGgplot(dsp)
    p <- p + treatmentLine(dsp)
    p <- p + xMeanLine(dsp) + yMeanLine(dsp)
    p <- p + shadows(dsp)
    p <- p + trails(dsp)
    p <- p + rawData(dsp)
    p <- p + theme()
    p <- p + identityLine()
    p <- p + rugPlot(dsp)
    p <- p + crossbow(dsp)
    p <- p + CIBand(dsp)
    p <- p + legend(dsp)
    p <- p + scaleX(dsp) + scaleY(dsp)
    p <- p + coord_fixed()
    p <- p + title()

    return( p )

}
