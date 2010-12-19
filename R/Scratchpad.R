library(ggplot2)

crossbow <- data.frame(slope = -1, intercept = 1, name = as.factor("line1"))

x <- rnorm(n = 25)
y <- rnorm(n = 25)

str(crossbow)

dd <- data.frame(xvals = x, yvals = y)
line1colors <- (line1 = "red")

p <- ggplot(aes(x = xvals, y = yvals), data = dd) + geom_point()

p + geom_abline(
      aes(
        slope     = slope,
        intercept = intercept,
        color     = name
      ),
      data = crossbow
    ) + scale_color_manual(value = line1colors) +coord_equal()
