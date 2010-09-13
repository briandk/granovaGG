# Required Libraries
library(ggplot2)
library(granova)
library(DAAG)

# Loading in the data
data(pair65)
str(pair65)

dd <- data.frame(xvals = pair65$heated, yvals = pair65$ambient)
str(dd)

# Setting the lower and upperbounds
extrema <- c(range(dd$xvals), range(dd$yvals))
offset  <- (max(extrema) - min(extrema)) / 10
bounds  <- c(min(extrema) - 5*offset, max(extrema) + offset)
bounds


# Plotting the standard granova plot
granova.ds(pair65,
  main = "Dependent sample assessment plot for pair65 data, n = 9")
  
# Trying to get the same plot in ggplot2
p <- ggplot(aes(x = xvals, y = yvals), 
              data = dd)
              
p <- p + geom_point() + xlim(bounds) + ylim(bounds)

# Adding the y=x line
p <- p + geom_abline(slope = 1, intercept = 0)

# Forcing coordinates to be equal
p <- p + coord_equal()

# Adding a rugplot
p <- p + geom_rug()

# Adding a perpendicular cross-section
p <- p + geom_abline(intercept = 2*(min(dd$yvals) - offset), 
                slope     = -1)
                
# Adding mean indicators
p <- p + geom_hline(yintercept = mean(dd$yvals), 
                    colour     = "red",
                    alpha      = 1/3) 
                    
p <- p + geom_vline(xintercept = mean(dd$xvals), 
                    colour     = "red",
                    alpha      = 1/3) 

# Removing the gridlines and background
p 
                                    