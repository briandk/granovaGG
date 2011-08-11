granova.ds.ggplot <- function(data       = NULL, 
                              main       = "Dependent Sample Assessment Plot",
                              conf.level = 0.95,
                              plot.theme = "theme_bw"
                             ) 

{
  
  GetXs <- function(data) {
    return(data[, 1])
  }

  GetYs <- function(data) {
    return(data[, 2])
  }

  GetEffect <- function(dsp) {
    return(GetXs(dsp$data) - GetYs(dsp$data))
  }
  
  GetTtest <- function(data, conf.level) {
    return(t.test(data[, 1], 
                  data[, 2], 
                  paired     = TRUE,
                  conf.level = conf.level
                 )
          )
  }
  
  GetStats <- function(dsp, conf.level) {
    ttest <- GetTtest(dsp$data, conf.level)
    return(data.frame(lower.treatment.effect = as.numeric(ttest$conf.int[2]),
                      mean.treatment.effect  = as.numeric(ttest$estimate[1]),
                      upper.treatment.effect = as.numeric(ttest$conf.int[1]),
                      t.statistic            = as.numeric(ttest$statistic[1])
                     )
          )
  }
  
  GetGraphicsParams <- function(dsp) {
    .aggregate.data.range  <- c(range(GetXs(dsp$data)), range(GetYs(dsp$data)))
    .extrema               <- c(max(.aggregate.data.range), min(.aggregate.data.range))    
    .square.data.range     <- max(.extrema) - min(.extrema)
    .southwest.padding     <- (65/100) * .square.data.range
    .north.east.padding    <- (15/100) * .square.data.range
    .lower.graphical.bound <- min(.extrema) - .southwest.padding
    .upper.graphical.bound <- max(.extrema) + .north.east.padding
    .bounds                <- c(.lower.graphical.bound, .upper.graphical.bound)
    .center                <- mean(.bounds)
    .crossbow.anchor       <- mean(.bounds) + min(.bounds)
    .shadow.offset         <- (1/100)*.square.data.range
  
    return(list(square.data.range = .square.data.range,    
                bounds            = .bounds,  
                shadow.offset     = .shadow.offset,
                anchor            = .crossbow.anchor,
                point.size        = I(2),
                mean.line.size    = I(1/2)      
               )
          )
  }
  
  GetShadows <- function(dsp) {
    x.shadow <- (dsp$effect / 2) + 
                (3 * dsp$params$bounds[1] + dsp$params$bounds[2]) / 4 + 
                (4 * dsp$params$shadow.offset)
    y.shadow <- x.shadow - dsp$effect
    return(data.frame(x.shadow, y.shadow))
  }

  GetCrossbow <- function(dsp) {
    return(data.frame(x     = min(dsp$shadows$x.shadow) - (2 * dsp$params$shadow.offset),
                      y     = max(dsp$shadows$y.shadow) - (2 * dsp$params$shadow.offset),
                      x.end = max(dsp$shadows$x.shadow) - (2 * dsp$params$shadow.offset),
                      y.end = min(dsp$shadows$y.shadow) - (2 * dsp$params$shadow.offset)
                     )
          )  
  }
  
  GetCIBand <- function(dsp) {
    return(data.frame(cx     = ((dsp$params$anchor + dsp$stats$lower.treatment.effect) / 2) - 3 * (dsp$params$shadow.offset),
                      cy     = ((dsp$params$anchor - dsp$stats$lower.treatment.effect) / 2) - 3 * (dsp$params$shadow.offset),
                      cx.end = ((dsp$params$anchor + dsp$stats$upper.treatment.effect) / 2) - 3 * (dsp$params$shadow.offset),
                      cy.end = ((dsp$params$anchor - dsp$stats$upper.treatment.effect) / 2) - 3 * (dsp$params$shadow.offset),
                      color  = factor(paste(100 * conf.level, "% CI", " (t = ", round(dsp$stats$t.statistic, digits = 2), ")", sep =""))
                     )
          )
    
  }
  
  GetTreatmentLine <- function(dsp) {
    return(data.frame(intercept = -dsp$stats$mean.treatment.effect,
                      slope     = 1,
                      color     = factor(paste("Mean Diff. =", round(dsp$stats$mean.treatment.effect, digits = 2)))
                     )
          )
  }

  GetTrails <- function(dsp) {
    return(data.frame(x.trail.start = GetXs(dsp$data), 
                      y.trail.start = GetYs(dsp$data),
                      x.trail.end   = GetXs(dsp$shadow), 
                      y.trail.end   = GetYs(dsp$shadow)
                     )
          )
  }
  
  GetColors <- function(dsp) {
    return(list(treatment.line = "#542570",
                rugplot        = "black",
                mean.line      = "#542570",
                CIBand         = "#33A02C",
                crossbow       = "#377EB8"
               )
          )         
  }

  dsp                <- list(data = data)
  dsp$effect         <- GetEffect(dsp)
  dsp$stats          <- GetStats(dsp, conf.level)
  dsp$params         <- GetGraphicsParams(dsp)
  dsp$shadows        <- GetShadows(dsp)
  dsp$crossbow       <- GetCrossbow(dsp)
  dsp$CIBand         <- GetCIBand(dsp)
  dsp$treatment.line <- GetTreatmentLine(dsp)
  dsp$trails         <- GetTrails(dsp)
  dsp$colors         <- GetColors(dsp)
    
  

  # Because of the way ggplot2 creates plot objects, layers can be
  # added to a plot p simply by calling "p <- p + newLayer"

  
  InitializeGgplot <- function(dsp) {
    return(ggplot(aes_string(x = names(dsp$data)[1], 
                             y = names(dsp$data)[2]
                            ),
                  data = dsp$data
                 )
          )
  }
    
  TreatmentLine <- function(dsp) {
    return(geom_abline(aes(intercept = intercept,
                           slope     = slope,
                           color     = color
                          ),
                       alpha    = 0.5,
                       size     = I(1),
                       linetype = "dashed",
                       data     = dsp$treatment.line
                      )
          )
  }

  RawData <- function(dsp) {
    return(geom_point(size = dsp$params$point.size))
  }

  IdentityLine <- function() {
    return(geom_abline(slope     = 1, 
                       intercept = 0,
                       alpha     = 0.75,
                       size      = 1
                      )
          )
  }

  ScaleX <- function(dsp) {
    return(scale_x_continuous(limits = dsp$params$bounds))
  }

  ScaleY <- function(dsp) {
    return(scale_y_continuous(limits = dsp$params$bounds))
  }

  RugPlot <- function(dsp) {
    return(geom_rug(size  = I(1/2),
                    alpha = I(1/3),
                    color = dsp$colors$rugplot,
                    data  = dsp$data
                   )  
          )
  }

  XMeanLine <- function(dsp) {
    return(geom_vline(xintercept = mean(GetXs(dsp$data)),
                      color      = dsp$colors$mean.line,
                      size       = dsp$params$mean.line.size,
                      alpha      = I(1/2)
                     ) 
          )
  }
  
  YMeanLine <- function(dsp)  {
    return(geom_hline(yintercept = mean(GetYs(dsp$data)),
                      color      = dsp$colors$mean.line,
                      size       = dsp$params$mean.line.size,
                      alpha      = I(1/2)
                     ) 
          )
    
  }

  Crossbow <- function(dsp) {
    return(geom_segment(aes(x    = x,
                            y    = y,
                            xend = x.end,
                            yend = y.end
                           ), 
                        size  = I(3/4),
                        alpha = I(3/4),
                        color = dsp$colors$crossbow,
                        data  = dsp$crossbow
                       )  
          )
  }

  CIBand <- function(dsp) {
    return(geom_segment(aes(x     = cx,
                            y     = cy,
                            xend  = cx.end,
                            yend  = cy.end,                        
                            color = color
                           ), 
                        size = I(2),
                        data = dsp$CIBand
                       )
          )
  }

  Shadows <- function(dsp) {
    return(geom_point(aes(x = x.shadow,
                          y = y.shadow
                         ),
                      data  = dsp$shadow, 
                      size  = dsp$params$point.size,
                      alpha = I(1/4) 
                     )
          )
  }

  Trails <- function(dsp) {
    return(geom_segment(aes(x    = x.trail.start,
                            y    = y.trail.start,
                            xend = x.trail.end,
                            yend = y.trail.end
                           ),
                        data     = dsp$trails,
                        size     = I(1/3),
                        color    = "black",
                        linetype = 1,
                        alpha    = I(1/10)              
                       ) 
          )
  }

  ColorScale <- function(dsp) {
    colors <- c(dsp$colors$treatment.line, dsp$colors$CIBand)
  
    return(scale_color_manual(value = colors, name = ""))
  }

  Title <- function() {
    return(opts(title = main))
  }
  
  ForceCoordinateAxesToBeEqual <- function() {
    return(coord_fixed())
  }
  
          
  p <- InitializeGgplot(dsp)
  p <- p + TreatmentLine(dsp)
  p <- p + XMeanLine(dsp) + YMeanLine(dsp)
  p <- p + Shadows(dsp)
  p <- p + Trails(dsp)
  p <- p + RawData(dsp)
  p <- p + Theme(plot.theme)
  p <- p + IdentityLine()
  p <- p + RugPlot(dsp)
  p <- p + Crossbow(dsp)
  p <- p + CIBand(dsp)
  p <- p + ColorScale(dsp)
  p <- p + ScaleX(dsp) + ScaleY(dsp)
  p <- p + ForceCoordinateAxesToBeEqual()
  p <- p + Title()

  return(p)

}
