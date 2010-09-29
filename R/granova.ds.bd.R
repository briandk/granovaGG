# Required Libraries
library(ggplot2)
library(granova)
library(DAAG)

# Loading in the data
data(pair65)
str(pair65)

dd <- data.frame(
  xvals  = pair65$heated, 
  yvals  = pair65$ambient,
  effect = pair65$heated - pair65$ambient)
str(dd)

# Computing Some Statistics

effectQuantiles <- quantile(dd$effect, probs = c(0, 0.025, 0.5, 0.975, 1))

ttest <- t.test(dd$yvals, dd$xvals, 
                 paired     = TRUE,
                 conf.level = 0.95)


cint                <- ttest$conf.int
confidenceInterval  <- (range(ttest$conf.int))
confidenceInterval  <- diff(confidenceInterval)
confidenceInterval
meanTreatmentEffect <- ttest$estimate

# Setting the graphicalbounds
extrema  <- c(range(dd$xvals), range(dd$yvals))
offset   <- (max(extrema) - min(extrema)) / 10
bounds   <- c(min(extrema) - 5*offset, max(extrema) + offset)
(perpendicularIntercept <- 2*(min(dd$yvals)) - offset)
shadowOffset <- offset/6

# Computing the Confidence Interval shadow               

geom_vline

cxstart    = ((perpendicularIntercept - meanTreatmentEffect) / 2) -
             (confidenceInterval / 2)/sqrt(2)

cystart    = ((perpendicularIntercept + meanTreatmentEffect) / 2) +
             (confidenceInterval / 2)/sqrt(2)

cxend      = ((perpendicularIntercept - meanTreatmentEffect) / 2) + 10 +
             (confidenceInterval / 2)/sqrt(2)
             
cyend      = ((perpendicularIntercept + meanTreatmentEffect) / 2) - 10 -
             (confidenceInterval / 2)/sqrt(2)
             
cxstart
cystart


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
  
# Trying to get the same plot in ggplot2
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
           alpha = I(1/3) 
        )

# Plotting the 95% Confidence band
p <- p + geom_segment(
           aes(
                x    = cxstart, 
                y    = cystart, 
                xend = cxend,   
                yend = cyend   
                
           ), size  = I(1),
              color = "darkgreen",
              alpha = I(1)
         )
     
# Plotting a red test line
p <- p + geom_segment(
          aes(
               x    = perpendicularIntercept / 2, 
               y    = perpendicularIntercept / 2, 
               xend = (perpendicularIntercept / 2) + 10,   
               yend = (perpendicularIntercept / 2) - 10   

          ), size  = I(1),
             color = "red",
             alpha = I(1)
        )
        
# Plotting a test point to verify the treatment effect
ddeffect <- data.frame(
  xvals = ((perpendicularIntercept - meanTreatmentEffect) / 2), 
  yvals = ((meanTreatmentEffect + perpendicularIntercept) / 2)
)


zeropoint  <- c(perpendicularIntercept/2, perpendicularIntercept/2)
lowerbound <- c( - (cint[2]) / sqrt(2), (cint[2]) / sqrt(2))
upperbound <- c( - (cint[1]) / sqrt(2), (cint[1]) / sqrt(2))
ddeffect   <- rbind(ddeffect, zeropoint, (zeropoint + lowerbound), (zeropoint + upperbound))



zeropoint 
lowerbound
ddeffect  
p <- p + geom_point(data = ddeffect)

         
p <- p + opts(title = "Dependent Sample Scatterplot for pair65 data")  

p
# Removing the gridlines and background
p +
  opts(panel.grid.major = theme_blank()) +  
  opts(panel.grid.minor = theme_blank()) +
  opts(panel.background = theme_blank()) + 
  opts(axis.line = theme_segment()) +
  opts(title = "Dependent Sample Scatterplot for pair65 data")  
  
                                    
