data(arousal)  
contrasts22 <- data.frame( c(-.5,-.5,.5,.5), 
  c(-.5,.5,-.5,.5), c(.5,-.5,-.5,.5) )
names(contrasts22) <- c("Drug.A", "Drug.B", "Drug.A.B")

granovagg.contr(arousal, contrasts = contrasts22, print.four.plots.per.page = TRUE, ylab = "Test 1")
granova.contr(arousal, contrasts = contrasts22)
# granova.contr(arousal, contrasts = contrasts22)
# 
# data(rat)
# dat6 <- matrix(c(1, 1, 1, -1, -1, -1, -1, 1, 0, -1, 1, 0, 1, 1, -2, 
#     1, 1, -2, -1, 1, 0, 1, -1, 0, 1, 1, -2, -1, -1, 2), ncol = 5)
# 
# y <- granovagg.contr(rat[,1], contrasts = dat6)
# 
# (y)
# 
#Polynomial Contrasts 
data(rat)
granova.contr(rat[,1], contrasts = contr.poly(6))
granovagg.contr(rat[,1], contrasts = contr.poly(6))

# 
# #based on random data 
# data.random <- rt(64, 5)
# LayoutFourPlotsPerPage(
#   granovagg.contr(data.random, contrasts = contr.helmert(8), 
#   ylab = "Random Data")
# )