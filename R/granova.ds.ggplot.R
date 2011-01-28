granova.ds.ggplot <- function( data                      = NULL, 
                               plotTitle                 = "Dependent Sample Scatterplot",
                               conf.level                = 0.95,
                               plotTheme                 = theme_bw()
                     ) 

{
  
  GetXs <- function (data) {
    return( data[, 1])
  }

  GetYs <- function (data) {
    return( data[, 2])
  }

  GetEffect <- function (dsp) {
    return( GetXs(dsp$data) - GetYs(dsp$data) )
  }
  
  GetTtest <- function (data, conf.level) {
    return (   t.test(data[, 1], 
                      data[, 2], 
                      paired     = TRUE,
                      conf.level = conf.level
               )
    )
  }
  
  GetStats <- function (dsp, conf.level) {
    tTest <- GetTtest(dsp$data, conf.level)
    return(  data.frame(
              lowerTreatmentEffect = as.numeric(tTest$conf.int[2]),
              meanTreatmentEffect  = as.numeric(tTest$estimate[1]),
              upperTreatmentEffect = as.numeric(tTest$conf.int[1]),
              tStatistic           = as.numeric(tTest$statistic[1])
            )
    )    
  }
  
  GetGraphicsParams <- function (dsp) {
    .aggregateDataRange  <- c(range(GetXs(dsp$data)), range(GetYs(dsp$data)))
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
      meanLineSize        = I(1/2)      
    ) )
  }
  
  GetShadows <- function (dsp) {
    xShadow <- (dsp$effect / 2) + 
               (3 * dsp$parameters$bounds[1] + dsp$parameters$bounds[2]) / 4 + 
               (4 * dsp$parameters$shadowOffset)
    yShadow <- xShadow - dsp$effect
    return (data.frame(xShadow, yShadow))
  }

  GetCrossbow <- function (dsp) {
    return(  data.frame(
      x    = min(dsp$shadows$xShadow) - (2 * dsp$parameters$shadowOffset),
      y    = max(dsp$shadows$yShadow) - (2 * dsp$parameters$shadowOffset),
      xend = max(dsp$shadows$xShadow) - (2 * dsp$parameters$shadowOffset),
      yend = min(dsp$shadows$yShadow) - (2 * dsp$parameters$shadowOffset)
      )
    )  
  }
  
  GetCIBand <- function (dsp) {
    CIBand <- data.frame(
      cx    = ((dsp$parameters$anchor + dsp$stats$lowerTreatmentEffect) / 2) - 3 * (dsp$parameters$shadowOffset),
      cy    = ((dsp$parameters$anchor - dsp$stats$lowerTreatmentEffect) / 2) - 3 * (dsp$parameters$shadowOffset),
      cxend = ((dsp$parameters$anchor + dsp$stats$upperTreatmentEffect) / 2) - 3 * (dsp$parameters$shadowOffset),
      cyend = ((dsp$parameters$anchor - dsp$stats$upperTreatmentEffect) / 2) - 3 * (dsp$parameters$shadowOffset),
      color = factor(paste(100 * conf.level, "% CI", " (t = ", round(dsp$stats$tStatistic, digits = 2), ")", sep =""))
      )
    
    return (CIBand)
  }
  
  GetTreatmentLine <- function (dsp) {
    return( data.frame(
              intercept      = -dsp$stats$meanTreatmentEffect,
              slope          = 1,
              color          = factor(paste("Mean Diff. =", round(dsp$stats$meanTreatmentEffect, digits = 2)))
            )
    )
  }

  GetTrails <- function (dsp) {
    
    return(  data.frame(
               xTrailStart = GetXs(dsp$data), 
               yTrailStart = GetYs(dsp$data),
               xTrailEnd   = GetXs(dsp$shadow), 
               yTrailEnd   = GetYs(dsp$shadow)
             )
    )
  }
  
  GetColors <- function (dsp) {
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
  dsp$effect        <- GetEffect(dsp)
  dsp$stats         <- GetStats(dsp, conf.level)
  dsp$parameters    <- GetGraphicsParams(dsp)
  dsp$shadows       <- GetShadows(dsp)
  dsp$crossbow      <- GetCrossbow(dsp)
  dsp$CIBand        <- GetCIBand(dsp)
  dsp$treatmentLine <- GetTreatmentLine(dsp)
  dsp$trails        <- GetTrails(dsp)
  dsp$colors        <- GetColors(dsp)
    
  

  # Because of the way ggplot2 creates plot objects, layers can be
  # added to a plot p simply by calling "p <- p + newLayer"

  
  InitializeGgplot <- function(dsp) {
    p <- ggplot( 
           aes_string(
             x = names(dsp$data)[1], 
             y = names(dsp$data)[2]
            ),
            data = dsp$data
          )
  }
    
  TreatmentLine <- function (dsp) {
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

  RawData <- function (dsp) {
    RawData <- geom_point(
      size  = dsp$parameters$pointsize
    )
    return (RawData)
  }

  IdentityLine <- function() {
    return (
      geom_abline(
        slope     = 1, 
        intercept = 0,
        alpha     = I(3/4)
      )
    )
  }

  ScaleX <- function (dsp) {
    return (scale_x_continuous(limits = dsp$parameters$bounds))
  }

  ScaleY <- function (dsp) {
    return (scale_y_continuous(limits = dsp$parameters$bounds))
  }

  RugPlot <- function (dsp) {
    return(
      geom_rug(
        size  = I(1/2),
        alpha = I(1/3),
        color = dsp$colors$rugplot,
        data  = dsp$data
      )  
    )
  }

  XMeanLine <- function (dsp) {
    return( 
      geom_vline(
        xintercept = mean(GetXs(dsp$data)),
        color      = dsp$colors$meanLine,
        size       = dsp$parameters$meanLineSize,
        alpha      = I(1/2)
      ) 
    )
  }
  
  YMeanLine <- function (dsp)  {
    return( 
      geom_hline(
        yintercept = mean(GetYs(dsp$data)),
        color      = dsp$colors$meanLine,
        size       = dsp$parameters$meanLineSize,
        alpha      = I(1/2)
      ) 
    )
    
  }

  Crossbow <- function (dsp) {
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

  Shadows <- function (dsp) {
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

  Trails <- function (dsp) {
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

  Legend <- function (dsp) {
    colors <- c(dsp$colors$treatmentLine, dsp$colors$CIBand)
  
    return (scale_color_manual(value = colors, name = ""))
  }

  Title <- function () {
    return (opts(title = plotTitle))
  }
  
  Theme <- function () {
    return (plotTheme)
  }
        
  p <- InitializeGgplot(dsp)
  p <- p + TreatmentLine(dsp)
  p <- p + XMeanLine(dsp) + YMeanLine(dsp)
  p <- p + Shadows(dsp)
  p <- p + Trails(dsp)
  p <- p + RawData(dsp)
  p <- p + Theme()
  p <- p + IdentityLine()
  p <- p + RugPlot(dsp)
  p <- p + Crossbow(dsp)
  p <- p + CIBand(dsp)
  p <- p + Legend(dsp)
  p <- p + ScaleX(dsp) + ScaleY(dsp)
  p <- p + coord_fixed()
  p <- p + Title()

  return( p )

}
