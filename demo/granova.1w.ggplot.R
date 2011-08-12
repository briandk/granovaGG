data(arousal)
#Drug A
granova.1w.ggplot(arousal[,1:2], group = arousal[,2])
granova.1w.ggplot(arousal[,1:2])

data(poison)
##Note violation of constant variance across groups in following graphic.
granova.1w.ggplot(poison$SurvTime,     
  group = poison$Group, resid=TRUE
)

# granova.1w.ggplot(poison$RateSurvTime, 
#   group = poison$Group, 
#   resid = FALSE, 
# )
# 
# granova.1w.ggplot(mtcars$hp, 
#   group = mtcars$cyl, 
#   resid = TRUE, 
# )
# 
# granova.1w.ggplot(mtcars$hp, 
#   group      = mtcars$gear, 
#   resid      = TRUE, 
#   h.rng      = 20,
#   plot.theme = "theme_granova_1w",
#   main       = "test"
# )
# 
# random.data <- data.frame(response = rnorm(n = 100), group = rep(1:10, each = 10))
# 
# granova.1w.ggplot(random.data$response,
#   group = random.data$group,
#   resid = TRUE,
#   plot.theme = "theme_granova_1w"
# )