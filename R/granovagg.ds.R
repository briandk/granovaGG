#' Granova for Display of Dependent Sample Data
#' 
#' Plots dependent sample data beginning from a scatterplot for the X,Y pairs;
#' proceeds to display difference scores as point projections; also X and Y
#' means, as well as the mean of the difference scores. Also prints various
#' summary statistics including: effect size, means for X and Y, a 95\%
#' confidence interval for the mean difference as well as the t-statistic and
#' degrees of freedom.
#' 
#' Paired X & Y values are plotted as scatterplot. The identity reference line
#' (for Y=X) is drawn. Since the better data view often entails having X's >
#' Y's the revc argument facilitates reversal of the X, Y specifications.
#' Parallel projections of data points to (a lower-left) line segment show how
#' each point relates to its X-Y = D difference; blue `crosses' are used to
#' display the distribution of difference scores and the mean difference is
#' displayed as a heavy dashed (red) line, parallel to the identity reference
#' line. Means for X and Y are also plotted (as thin dashed vertical and
#' horizontal lines), and rug plots are shown for the distributions of X (at
#' the top of graphic) and Y (on the right side). Several summary statistics
#' are plotted as well, to facilitate both description and inference; see
#' below. The 95\% confidence interval for the population mean difference is
#' also shown graphically.  Because all data points are plotted relative to the
#' identity line, and summary results are shown graphically, clusters, data
#' trends, outliers, and possible uses of transformations are readily seen,
#' possibly to be accommodated.
#' 
#' @param data is an n X 2 dataframe or matrix. First column defines X
#'   (intially for horzontal axis), the second defines Y.
#' @param xlab optional label (as character) for horizontal axis. If not
#'   defined, axis labels are taken from colnames of data.
#' @param ylab optional label (as character) for vertical axis.
#' @param main optional main title (as character); if not supplied by user
#'   generic title is provided.
#' @return A list is returned with the following components:
#'   \item{mean(X)}{Mean of X values} \item{mean(Y)}{Mean of Y values}
#'   \item{mean(D=X-Y)}{Mean of differences D = X - Y} \item{SD(D)}{Standard
#'   deviation of differences D} \item{ES(D)}{Effect Size for differences D:
#'   mean(D)/SD(D)} \item{r(X,Y)}{Correlation based on X,Y pairs}
#'   \item{r(x+y,D)}{Correlation based on X+Y,D pairs} \item{LL 95\%CI}{Lower
#'   bound for 95\% confidence interval for population mean(D)} \item{UL
#'   95\%CI}{Upper bound for 95\% confidence interval for population mean(D)}
#'   \item{t(D-bar)}{t-statistic associated w/ test of hypothesis that
#'   population mean(D) = 0.0} \item{df.t}{Degrees of freedom for the
#'   t-statistic} \item{pval.t}{P-value for two sided t-test of null hypothesis
#'   that population mean(D) does not equal zero.}
#' @examples NULL

granovagg.ds <- function(data       = NULL, 
                              main       = "Dependent Sample Assessment Plot",
                              conf.level = 0.95,
                              plot.theme = "theme_granova_ds"
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
    return(geom_rug_alt(size  = I(1/2),
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
                      linetype   = "dashed",
                      alpha      = I(1/2)
                     ) 
          )
  }
  
  YMeanLine <- function(dsp)  {
    return(geom_hline(yintercept = mean(GetYs(dsp$data)),
                      color      = dsp$colors$mean.line,
                      size       = dsp$params$mean.line.size,
                      linetype   = "dashed",
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
                      shape = 16,
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
