data(poison)
##Note violation of constant variance across groups in following graphic.
granova.1w(poison$RateSurvTime, group = poison$Group, resid = TRUE)
granova.1w.ggplot(poison$RateSurvTime, group = poison$Group, resid = TRUE)
granova.1w.ggplot(poison$RateSurvTime, group = poison$Group, resid = TRUE, main = "theme_bw()") + theme_bw()
qplot(x = mpg, y = hp, data = mtcars) + theme_bw()
qplot(x = mpg, y = hp, data = mtcars)
##RateSurvTime = SurvTime^-1
# granova.1w.ggplot(poison)