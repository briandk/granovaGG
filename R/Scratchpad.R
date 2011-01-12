x <- 1
y <- 2
z <- 3

ll <- list(x, y, z)

str(ll)

dd <- data.frame(
  xvals = rnorm(n = 25),
  yvals = rnorm(n = 25)  
)

dd <- structure(dd, class = "granovaData")
ll <- list(graphicalParameters = ll, granovaData = dd)

str(ll)

ll$granovaData$xvals

