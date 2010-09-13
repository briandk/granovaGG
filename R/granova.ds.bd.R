# Required Libraries
library(ggplot2)
library(granova)
library(DAAG)

# Loading in the data
data(pair65)
str(pair65)

# Setting the lower and upperbounds
xlowerbound <- min(pair65$heated)
ylowerbound <- min(pair65$ambient)
                   
xupperbound <- max(pair65$heated)
yupperbound <- max(pair65$ambient)

xlimits <- c(xlowerbound, xupperbound)
ylimits <- c(ylowerbound, yupperbound)

# Plotting the standard granova plot
granova.ds(pair65,
  main = "Dependent sample assessment plot for pair65 data, n = 9")
  
# Trying to get the same plot in ggplot2
p <- ggplot(aes(x = heated, y = ambient), 
              data = pair65,
              xlim = xlimits, 
              ylim = ylimits)
              
p <- p + geom_point()

# Adding the y=x line
p <- p + geom_abline(slope = 1, intercept = 0)

# Forcing coordinates to be equal
p + coord_equal()