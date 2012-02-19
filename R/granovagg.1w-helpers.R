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

