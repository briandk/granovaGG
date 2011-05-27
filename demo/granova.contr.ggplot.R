data(arousal)  
contrasts22 <- data.frame( c(-.5,-.5,.5,.5), 
  c(-.5,.5,-.5,.5), c(.5,-.5,-.5,.5) )
names(contrasts22) <- c("Drug.A", "Drug.B", "Drug.A.B")

x <- granova.contr.ggplot(arousal, contrasts = contrasts22)
x[[1]]
  
data(rat)
dat6 <- matrix(c(1, 1, 1, -1, -1, -1, -1, 1, 0, -1, 1, 0, 1, 1, -2, 
    1, 1, -2, -1, 1, 0, 1, -1, 0, 1, 1, -2, -1, -1, 2), ncol = 5)

y <- granova.contr.ggplot(rat[,1], contrasts = dat6, ylab = "Rat Weight Gain", 
  xlab = c("Amount 1 vs. Amount 2", "Type 1 vs. Type 2", 
  "Type 1 & 2 vs Type 3", "Interaction of Amount and Type 1 & 2", 
  "Interaction of Amount and  Type (1, 2), 3"))

LayoutFourPlotsPerPage(y)

#Polynomial Contrasts 
z <- granova.contr.ggplot(rat[,1],contrasts = contr.poly(6))
LayoutFourPlotsPerPage(z)

#based on random data 
data.random <- rt(64, 5)
LayoutFourPlotsPerPage(
  granova.contr.ggplot(data.random, contrasts = contr.helmert(8), 
  ylab = "Random Data", plot.theme = "theme_granova_1w_gray")
)