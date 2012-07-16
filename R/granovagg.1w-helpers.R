IsFSignificant <- function(model.summary) {
  f.critical  <- qf(p   = 0.95,
                    df1 = model.summary$fstatistic["numdf"],
                    df2 = model.summary$fstatistic["dendf"]
                 )
  return(as.logical(
           model.summary$fstatistic["value"] > f.critical
         )
  )
}

## Plotting Functions
InitializeGgplot_1w <- function() {
  return(ggplot())
}

GrandMeanLine <- function(owp) {
  return(
    geom_hline(
      color      = brewer.pal(n = 8, name = "Set1")[3],
      alpha      = I(1/2),
      size       = I(0.25),
      yintercept = owp$stats$grand.mean
    )
  )
}

GrandMeanPoint <- function(owp) {
  return(
    geom_point(
      aes(
        x = 0, y = mean(score), color = factor(paste("Grand Mean"))
      ),
      size = 2.5,
      data = owp$data
    )
  )
}

JitteredScoresByGroupContrast <- function(owp) {
  only.jitter.in.x.direction <- position_jitter(height = 0, width = GetDegreeOfJitter(owp))

  return(
    geom_point(
      aes(
        x = contrast,
        y = score
      ),
      alpha    = I(1),
      size     = I(2),
      data     = owp$data,
      position = only.jitter.in.x.direction
    )
  )
}




ScaleX_1w <- function(owp) {
  return(
    scale_x_continuous(
      breaks = (owp$params$aggregate.x.breaks),
      labels = signif(owp$params$aggregate.x.breaks, digits = 2),
      limits = owp$params$x.range,
      expand = c(0.00, 0)
    )
  )
}

ScaleY_1w <- function(owp) {
  return(
    scale_y_continuous(
      breaks = (owp$params$aggregate.y.breaks),
      labels = signif(owp$params$aggregate.y.breaks, digits = 2),
      limits = owp$params$y.range,
      expand = c(0.00, 0)
    )
  )
}

