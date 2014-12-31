appeaseRCmdCheck <- function() {
  # The sole purpose of this function is to avoid
  # "no visible binding for [variable] NOTEs in
  # R cmd check
  nonVisibleBindings <- c("ymin",
                          "fill",
                          "xmax",
                          "angle",
                          "maximum.score",
                          "yend",
                          "baseline.variation",
                          "label",
                          "xmin",
                          "within.1.sd.of.the.mean.of.all.residuals",
                          "score",
                          "group.mean",
                          "within.group.residuals",
                          "ylabel",
                          "y",
                          "x",
                          "xend",
                          "overplotted",
                          "contrast",
                          "ymax"
  )
  globalVariables(names)
}

appeaseRCmdCheck()