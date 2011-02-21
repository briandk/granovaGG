data(poison)
##Note violation of constant variance across groups in following graphic.
granova.1w(poison$SurvTime, group = poison$Group, resid = TRUE)
granova.1w.ggplot(poison$SurvTime, group = poison$Group, resid = TRUE)

granova.1w(poison$RateSurvTime, group        = poison$Group, resid = TRUE)
granova.1w.ggplot(poison$RateSurvTime, group = poison$Group, resid = TRUE)

granova.1w(mtcars$hp, group        = mtcars$cyl, resid = TRUE, v.rng = 1)
granova.1w.ggplot(mtcars$hp, group = mtcars$cyl, resid = TRUE, v.rng = 1)

granova.1w(mtcars$hp, group        = mtcars$gear, resid = TRUE, v.rng = 20)
granova.1w.ggplot(mtcars$hp, group = mtcars$gear, resid = TRUE, v.rng = 20)

##RateSurvTime = SurvTime^-1
# granova.1w.ggplot(poison)
