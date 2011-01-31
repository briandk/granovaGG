data(poison)
##Note violation of constant variance across groups in following graphic.
granova.1w(poison$RateSurvTime, group = poison$Group, resid = TRUE)
granova.1w.ggplot(poison$RateSurvTime, group = poison$Group, resid = TRUE)
granova.1w.ggplot(poison$RateSurvTime, group = poison$Group, resid = TRUE, main = "Tite of the Plot")

##RateSurvTime = SurvTime^-1
# granova.1w.ggplot(poison)