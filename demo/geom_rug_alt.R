p <- ggplot(mtcars, aes(x=wt, y=mpg))

p + geom_point() + geom_rug()     # traditional rug fringe placement
p + geom_point() + geom_rug_alt() # alternate fringe placement
