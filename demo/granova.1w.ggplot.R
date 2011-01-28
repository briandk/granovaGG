data(poison)
##Note violation of constant variance across groups in following graphic.
granova.1w.ggplot(poison$SurvTime, group = poison$Group, ylab = "Survival Time")
##RateSurvTime = SurvTime^-1
granova.1w.ggplot(poison$RateSurvTime, group = poison$Group, 
ylab = "Survival Rate = Inverse of Survival Time")