# Required Libraries
library(ggplot2)
library(granova)
library(DAAG)

# Loading in the data
data(pair65)
str(pair65)

dd <- data.frame(xvals = pair65$heated, yvals = pair65$ambient)
str(dd)

# Computing Some Statistics
ttest<-t.test(dd$xvals, dd$yvals, 
              paired     = TRUE,
              conf.level = 0.95)

confidenceInterval <- (ttest$conf.int)

# Setting the graphicalbounds
extrema <- c(range(dd$xvals), range(dd$yvals))
offset  <- (max(extrema) - min(extrema)) / 10
bounds  <- c(min(extrema) - 5*offset, max(extrema) + offset)
perpendicularIntercept <- 2*(min(dd$yvals)) - offset
shadowOffset <- offset/4

# Computing point shadows

xshadow <- (((dd$xvals - dd$yvals) + perpendicularIntercept) /2) + shadowOffset
yshadow <- -(xshadow) + perpendicularIntercept + shadowOffset

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
p <- p + geom_rug(alpha = I(1/3))

# Adding a perpendicular cross-section
p <- p + geom_abline(intercept = perpendicularIntercept, 
                     slope     = -1)
                
# Adding group mean lines
p <- p + geom_hline(yintercept = mean(dd$yvals), 
                    colour     = "red",
                    alpha      = 1/3,
                    linetype   = 3)
                    
p <- p + geom_vline(xintercept = mean(dd$xvals), 
                    colour     = "red",
                    alpha      = 1/3,
                    linetype   = 3) 

# Adding the treatment effect line
p <- p + geom_abline(intercept = mean(dd$yvals) - mean(dd$xvals),
                     slope     = 1,
                     color     = "red",
                     alpha     = 1,
                     linetype  = 2)
                     
# Plotting point shadows
p <- p + geom_point(data = ddshadow, color = "blue", size = I(3))

# Plotting the Confidence Interval                  

# Removing the gridlines and background
p +
  opts(panel.grid.major = theme_blank()) +  
  opts(panel.grid.minor = theme_blank()) +
  opts(panel.background = theme_blank()) + 
  opts(axis.line = theme_segment()) +
  opts(title = "Dependent Sample Scatterplot for pair65 data")
                                    