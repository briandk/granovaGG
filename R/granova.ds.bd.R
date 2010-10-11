# Required Libraries
library(ggplot2)
library(granova)
library(DAAG)

# Loading in the data
data(pair65)
str(pair65)

# Defining the Function

# granova.ds.bd <- function(dataframe = pair65) {
  data <- pair65
  dd <- data.frame(
          xvals  = data[ , 1], 
          yvals  = data[ , 2],
          effect = (data[ , 1]  - data[ , 2])
        )  
  str(dd)

  # Computing Some Statistics
  effectQuantiles <- quantile(dd$effect, probs = c(0, 0.025, 0.5, 0.975, 1))

  dsttest <- t.test(dd$yvals, dd$xvals, 
                   paired     = TRUE,
                   conf.level = 0.95)

  (meanTreatmentEffect  <- dsttest$estimate)
  (upperTreatmentEffect <- dsttest$conf.int[1])
  (lowerTreatmentEffect <- dsttest$conf.int[2])

  lm1 <- lm(yvals ~ xvals, data = dd)
  summary(lm1)
  meanTreatmentEffect

  # Setting the graphicalbounds
  extrema  <- c(range(dd$xvals), range(dd$yvals))
  offset   <- (max(extrema) - min(extrema)) / 10
  bounds   <- c(min(extrema) - 5*offset, max(extrema) + offset)
  (perpendicularIntercept <- 2*(min(dd$yvals)) - offset)
  shadowOffset <- offset/6

  # Computing point shadows
  xshadow <- (((dd$xvals - dd$yvals) + perpendicularIntercept) /2) + shadowOffset
  yshadow <- (xshadow) + (dd$yvals - dd$xvals)

  xshadow
  yshadow
  # I have to name the resultant dataframe variables as "xvals" and "yvals" so
  # that the subsequent geom_point(data = ddshadow) can inherit the dd dataframe
  # column names and plot correctly (Wickham, ggplot2 book, p. 63)
  ddshadow <- data.frame(xvals = xshadow, yvals = yshadow)
  ddshadow

  # Plotting the standard granova plot
  granova.ds(pair65,
    main = "Dependent sample assessment plot for pair65 data, n = 9")
  
  ## Trying to get the same plot in ggplot2
  p <- ggplot(aes(x = xvals, y = yvals), 
                data = dd)
              
  p <- p + geom_point(size = I(3)) + xlim(bounds) + ylim(bounds)

  # Adding the y=x line
  p <- p + geom_abline(slope = 1, intercept = 0)

  # Forcing coordinates to be equal
  p <- p + coord_equal()

  # Adding a rugplot
  p <- p + geom_rug(alpha = I(2/3))

  # Adding a perpendicular cross-section
  p <- p + geom_abline(intercept = perpendicularIntercept, 
                       slope     = -1)
                
  # Adding group mean lines
  p <- p + geom_hline(yintercept = mean(dd$yvals), 
                      colour     = "red",
                      alpha      = 1/2,
                      linetype   = 3)
                    
  p <- p + geom_vline(xintercept = mean(dd$xvals), 
                      colour     = "red",
                      alpha      = 1/2,
                      linetype   = 3) 

  # Adding the treatment effect line
  p <- p + geom_abline(intercept = meanTreatmentEffect,
                       slope     = 1,
                       color     = "red",
                       alpha     = 1,
                       linetype  = 2)
                     
  # Plotting point shadows
  p <- p + geom_point(
             data  = ddshadow, 
             color = "black", 
             size  = I(3),
             alpha = I(1/2) 
           )

  # Plotting the point trails
  p <- p + geom_segment(
             aes(
               x = dd$xvals,
               y = dd$yvals,
               xend = ddshadow$xvals,
               yend = ddshadow$yvals           
           
             ), size     = I(1),
                color    = "black",
                linetype = 3,
                alpha    = I(1/4), 
           )


  # Plotting the 95% Confidence band
  p <- p + geom_segment(
             aes(
               x    = ((perpendicularIntercept - lowerTreatmentEffect) / 2) 
                       - shadowOffset,
               y    = ((perpendicularIntercept + lowerTreatmentEffect) / 2) 
                       - shadowOffset,
               xend = ((perpendicularIntercept - upperTreatmentEffect) / 2) 
                       - shadowOffset,
               yend = ((perpendicularIntercept + upperTreatmentEffect) / 2) 
                       - shadowOffset
                
             ), size  = I(2),
                color = "darkgreen",
                alpha = I(1),
             
           )
     
  p

  # Removing the gridlines and background
  p +
    opts(panel.grid.major = theme_blank()) +  
    opts(panel.grid.minor = theme_blank()) +
    opts(panel.background = theme_blank()) + 
    opts(axis.line = theme_segment()) +
    opts(title = "Dependent Sample Scatterplot for pair65 data")  
  
# }  
                                  