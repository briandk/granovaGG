#' Graphic Display of Contrast Effect of ANOVA
#' 
#' Provides graphic displays that shows data and effects for a priori contrasts
#' in ANOVA contexts; also corresponding numerical results.
#' 
#' Function provides graphic displays of contrast effects for prespecified
#' contrasts in ANOVA. Data points are displayed as relevant for each contrast
#' based on comparing groups according to the positive and negative contrast
#' coefficients for each contrast on the horizontal axis, against response
#' values on the vertical axis. Data points corresponding to groups not being
#' compared in any contrast (coefficients of zero) are ignored. For each
#' contrast (generally as part of a 2 x 2 panel) a line segment is given that
#' compares the (weighted) mean of the response variable for the negative
#' coefficients versus the positive coefficients. Standardized contrasts are
#' used, wherein the sum of (magnitudes) of negative coefficients is unity; and
#' the same for positive coefficients. If a line is `notably' different from
#' horizontal (i.e. slope of zero), a `notable' effect has been identified;
#' however, the question of statistical significance generally depends on a
#' sound context-based estimate of standard error for the corresponding effect.
#' This means that while summary aov numerical results and test statistics are
#' presented (see below), the appropriateness of the default standard error
#' generally requires the analyst's judgment. The response values are to be
#' input in (a stacked) form, i.e. as a vector, for all cells (cf. arg. ylab).
#' The matrix of contrast vectors \code{contrasts} must have G rows (the number
#' of groups), and a number of columns equal to the number of prespecified
#' contrasts, at most G-1. If the number of columns of \code{contrasts} is G-1,
#' then the number per group, or cell size, is taken to be
#' \code{length(data)/G}, where \code{G = nrow(contrasts)}.
#' 
#' If the number of columns of \code{contrasts} is less than G-1 then the user
#' must stipulate \code{npg}, the number in each group or cell.  The function
#' is designed for the case when all cell sizes are the same, and may be most
#' helpful when the a priori contrasts are mutually orthogonal (e.g., in power
#' of 2 designs, or their fractional counterparts; also when specific row or
#' column comparisons, or their interactions (see the example below based on
#' rat weight gain data)). It is not essential that contrasts be mutually
#' orthogonal; but mutual linear independence is required. (When factor levels
#' correspond to some underlying continuum a standard application might use
#' \code{con = contr.poly(G)}, for G the number of groups; consider also
#' \code{contr.helmert(G)}.)  The final plot in each application shows the data
#' for all groups or cells in the design, where groups are simply numbered from
#' 1:G, for G the number of groups, on the horizontal axis, versus the response
#' values on the vertical axis.
#' 
#' @param data Vector of scores for all equally sized groups, or a data.fame or
#'   matrix where each column represents a group.
#' @param contrasts Matrix of column contrasts with dimensions (number of
#'   groups [G]) x (number of contrasts) [generally (G x G-1)].
#' @param ylab Character; y axis lable.
#' @param xlab Character vector of length number of contrast columns.  To name
#'   the specific contrast being made in all but last panel of graphic.
#'   Default = \code{NULL}
#' @param jj Numeric; controls \code{\link{jitter}} and confers the possibility
#'   of controlling the amount of jitter in the panel plots for the contrasts
#'   Default is 1.
#' @return Two sets of numerical results are presented: Weighted cell means for
#'   positive and negative coefficients for each a priori contrast, and summary
#'   results from \code{lm}.  \item{summary.lm}{Summary results for a linear
#'   model analysis based on the R function \code{lm} (When effects are simple,
#'   as in an equal n's power of 2 design, mean differences will generally
#'   correspond to the linear regression coefficients as seen in the \code{lm}
#'   summary results.)} \item{means.pos.neg.coeff}{table showing the (weighted)
#'   means for positive and negative coefficients for each (row) contrast, and
#'   for each row, the difference between these means in the final column}
#'   \item{means.pos.neg.coeff}{Table showing the (weighted) means for positive
#'   and negative coefficients for each (row) contrast, and for each row, the
#'   difference between these means, and the standardized effect size in the
#'   final column.} \item{contrasts}{Contrast matrix used.}
#'   \item{group.means.sds}{Group means and standard deviations.}
#'   \item{data}{Input data in matrix form.}
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
