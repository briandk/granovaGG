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

  GetSummary_1w <- function(owp) {
    output <- with(owp, {
        ddply(data, .(group), summarise,
          group              = unique(group),
          group.mean         = mean(score),
          trimmed.mean       = mean(score, trim = 0.2),
          contrast           = unique(contrast),
          variance           = var(score),
          standard.deviation = sd(score),
          maximum.score      = max(score),
          group.size         = length(score)
        )
      }
    )
    return(output)
  }

  PrintGroupSummary_1w <- function(data, digits.to.round) {
    groups <- subset(data, select = group)
    stats  <- subset(data, select = c(-group, -maximum.score))    
    rounded.stats <- round(stats, digits = digits.to.round)
    output <- ReorderDataByColumn(cbind(groups, rounded.stats), "group.mean")
    message("\nBy-group summary statistics for your input data (ordered by group means)")
    
    return(
       print(output)
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

