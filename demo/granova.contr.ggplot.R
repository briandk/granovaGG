data(arousal)  
contrasts22 <- data.frame( c(-.5,-.5,.5,.5), 
  c(-.5,.5,-.5,.5), c(.5,-.5,-.5,.5) )
names(contrasts22) <- c("Drug.A", "Drug.B", "Drug.A.B")

granova.contr.ggplot(arousal, contrasts = contrasts22) -> gg
# granova.contr(arousal, contrasts = contrasts22)
# 
# data(rat)
# dat6 <- matrix(c(1, 1, 1, -1, -1, -1, -1, 1, 0, -1, 1, 0, 1, 1, -2, 
#     1, 1, -2, -1, 1, 0, 1, -1, 0, 1, 1, -2, -1, -1, 2), ncol = 5)
# 
# y <- granova.contr.ggplot(rat[,1], contrasts = dat6)
# 
# (y)
# 
#Polynomial Contrasts 
data(rat)
gg <- granova.contr.ggplot(rat[,1],contrasts = contr.poly(6))
do.call("grid.arrange", list(gg, "nrow = 2"))

# 
# #based on random data 
# data.random <- rt(64, 5)
# LayoutFourPlotsPerPage(
#   granova.contr.ggplot(data.random, contrasts = contr.helmert(8), 
#   ylab = "Random Data")
# )