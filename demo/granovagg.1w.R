data(arousal)
#Drug A
granovagg.1w(arousal[,1:2], group = arousal[,2])
granovagg.1w(arousal[,1:2])

data(poison)
##Note violation of constant variance across groups in following graphic.
granovagg.1w(poison$SurvTime,     
  group = poison$Group, resid=TRUE
)

# granovagg.1w(poison$RateSurvTime, 
#   group = poison$Group, 
#   resid = FALSE, 
# )
# 
# granovagg.1w(mtcars$hp, 
#   group = mtcars$cyl, 
#   resid = TRUE, 
# )
# 
# granovagg.1w(mtcars$hp, 
#   group      = mtcars$gear, 
#   resid      = TRUE, 
#   h.rng      = 20,
#   plot.theme = "theme_granova_1w",
#   main       = "test"
# )
# 
# random.data <- data.frame(response = rnorm(n = 100), group = rep(1:10, each = 10))
# 
# granovagg.1w(random.data$response,
#   group = random.data$group,
#   resid = TRUE,
#   plot.theme = "theme_granova_1w"
# )