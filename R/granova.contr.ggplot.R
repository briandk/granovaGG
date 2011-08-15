granova.contr.ggplot <- function(data, 
                                 contrasts, 
                                 ylab       = "Outcome (response)", 
                                 xlab       = NULL, 
                                 jj         = 1,
                                 plot.theme = "theme_granova_contr",
                                 print.four.plots.per.page = TRUE,
                                 ...
                        ) 
{

  # Plots responses by contrasts.
  # 'data' must be vector of scores for all equal size groups.
  # 'con' must be matrix of column contrasts with dimensions (number of groups) x (number of contrasts)
  # [generally n X n-1].  The number of rows = number 'cells' or groups.
  # Basic lm (regression) results are provided; orthogonal contrasts are ideal (but not essential).
  # 'jj' controls jitter.

  # 'ctr' is shorthand for the ConTRast data object that will hold all the information for plotting
  FormatResponseData <- function(data) {
    is.data.one.dimensional <- is.null(dim(data)[2])
    if (is.data.one.dimensional) {
      return(data)
    }
    
    return(stack(as.data.frame(data))[, 1])
  }
  
  std.contr <- function(contrasts, tolerance = sqrt(.Machine$double.eps)^0.6) {
      if (!is.matrix(contrasts)) {
          contrasts <- as.matrix(contrasts)
      }
      if (sum(abs(colMeans(contrasts))) > tolerance) {
          stop("Input vector/matrix must have mean zero (for each column)")
      }
      if (ncol(contrasts) == 1) {
          contrasts <- matrix(contrasts, ncol = 1)
      }
      dg <- apply(abs(contrasts), 2, sum)
      if (length(dg) == 1) {
          dg <- as.matrix(dg)
      }
      standardized.contrasts <- round(2 * contrasts %*% diag(1/dg), 3)
      
      return(standardized.contrasts)
  }
  
  indic <- function(xx) {
             mm <- matrix(0, length(xx), length(unique(xx)))
             indx <- ifelse(xx == col(mm), 1, 0)
             indx
  }

  AdaptVariablesFromGranovaComputations <- function () {
    
    response            <- FormatResponseData(data)
    contrasts           <- as.matrix(contrasts)
    number.of.groups    <- nrow(contrasts)
    responses.per.group <- length(response)/number.of.groups
    group.identifiers   <- rep(1:number.of.groups, ea = responses.per.group)
    indicator.matrix    <- indic(group.identifiers)
    indicated.contrasts <- indicator.matrix %*% contrasts
    standardized.contrasts <- std.contr(indicated.contrasts)
    
    return(
        list(
          response                      = response,
          contrast.matrix               = contrasts,
          scaled.standardized.contrasts = standardized.contrasts * responses.per.group,
          number.of.contrasts           = dim(standardized.contrasts)[2],
          number.of.groups              = number.of.groups,
          responses.per.group           = responses.per.group
        )
    )
  }

  GetContrastPlotData <- function (ctr) {
    return(
      lapply(
        X         = 1:ctr$number.of.contrasts, 
        FUN       = ExtractDataForPlot,
        contrasts = ctr$scaled.standardized.contrasts,
        response  = ctr$response
      )
    ) 
  }

  GetLinearModel <- function(ctr) {
    Contrast <- ctr$scaled.standardized.contrasts
    Response <- ctr$response
    
    return(lm(Response ~ Contrast))
  }
  
  ExtractDataForPlot <- function (contrasts, response, index) {
      non.zero.indicators <- contrasts[, index] != 0
      x.values <- contrasts[, index][non.zero.indicators]
      y.values <- response[non.zero.indicators]   
      raw.data <- data.frame(x.values, y.values)
  
    return(
        list(
          raw.data     = raw.data, 
          summary.data = GetSummary(raw.data)
        )
    )
  }

  GetSummary <- function(data) {
    return(
      ddply(data, .(x.values > 0), summarise,
        contrasts          = mean(x.values),
        responses          = mean(y.values)
      )
    )
  }
  
  GetContrastPlots <- function (ctr) {
    return(
      lapply(
        X             = 1:ctr$number.of.contrasts,
        FUN           = ComposeContrastPlot,
        plot.data     = ctr$contrast.plot.data
      )
    )
  }
  
  ComposeContrastPlot <- function(plot.data, index) {    
    p <- ggplot()
    p <- p + MeanResponse(plot.data[[index]]$raw.data$y.values)
    p <- p + JitteredResponsesByContrast(plot.data[[index]]$raw.data)
    p <- p + EffectsOfContrasts(plot.data[[index]]$summary.data)
    p <- p + ConnectEffectMeans(plot.data[[index]]$summary.data)
    p <- p + Theme(plot.theme)
    p <- p + ContrastPlotTitle(ctr, index)
    p <- p + ContrastPlotXLabel(ctr, index)
    p <- p + ContrastPlotYLabel()
        
    return(p)
  }
  
  MeanResponse <- function(response) {
    return(
      geom_hline(
        aes_string(yintercept = mean(response)), 
        color = brewer.pal(8, "Set1")[1],
        data  = as.data.frame(data),
        alpha = 0.5,
        size  = 0.3
      )
    )
  }
  
  JitteredResponsesByContrast <- function (data) {
    return(
      geom_point(
               aes(
                 x = x.values, 
                 y = y.values
               ),
               data     = data,
               position = position_jitter(height = 0, width = 1/100)
      )
    )
  }
  
  EffectsOfContrasts <- function(data) {
    return(
      geom_point(
               aes(
                 x = contrasts, 
                 y = responses
               ),
               data  = data,
               color = brewer.pal(8, "Set1")[2],
               size  = I(3),
               alpha = 0.75
      )
    )
  }
  
  ConnectEffectMeans <- function(data) {
    return(
      geom_line(
               aes(
                 x = contrasts, 
                 y = responses
               ),
               data  = data,
               color = brewer.pal(8, "Set1")[2],
               alpha = 1
      )
    )
  }
  
  ContrastPlotTitle <- function(ctr, index) {
    return(
        opts(title = paste("Coefficients vs. Response\n", GetContrastName(ctr$contrast.matrix, index)))
    )
  }
  
  ContrastPlotXLabel <- function(ctr, index) {
    return(
        xlab(paste(GetContrastName(ctr$contrast.matrix, index)))
    )
  }
  
  ContrastPlotYLabel <- function() {
    return(
        ylab(paste(ylab))
    )
  }
  
  GetSummaryPlotData <- function(ctr) {
    raw.data <- as.data.frame(
                   matrix(ctr$response, ncol = ctr$number.of.groups)
                 )
    raw.data <- RenameSummaryColumnNames(raw.data)
    raw.data <- melt(raw.data)
    raw.data$variable <- as.numeric(raw.data$variable)
    summary.data <- GetGroupSummary(raw.data)
    
    return(list(
                raw.data     = raw.data,
                summary.data = summary.data
          )
    )                   
  }
  
  RenameSummaryColumnNames <- function(data) {
    colnames(data) <- sapply(
                        1:ncol(data), 
                        function(index) {paste(index)}
                      )
                      
    return(data)
  }
  
  GetGroupSummary <- function(data) {
    return(
      ddply(data, .(variable), summarise,
        group      = unique(variable),
        group.mean = mean(value)
      )
    )
  }
  
  ComposeSummaryPlot <- function(plot.data) {    
    p <- ggplot()
    p <- p + MeanResponse(plot.data$raw.data$value)
    p <- p + RawScoresByGroup(plot.data$raw.data)
    p <- p + MeansByGroup(plot.data$summary.data)
    p <- p + ConnectGroupResponseMeans(plot.data$summary.data)
    p <- p + Theme(plot.theme)
    p <- p + GroupSummaryPlotTitle(ctr)
    p <- p + GroupSummaryXLabel()
    p <- p + GroupSummaryYLabel()
    return(p)
  }
  
  RawScoresByGroup <- function(data) {
    return(
      geom_point(
               aes(
                 x = as.factor(variable), 
                 y = value
               ),
               data = data,
               position = position_jitter(height = 0, width = 7/100)
      )
    )
  }
  
  MeansByGroup <- function(data) {
    return(
      geom_point(
               aes(
                 x = group, 
                 y = group.mean
               ),
               data  = data,
               color = brewer.pal(8, "Set1")[2],
               size  = I(3),
               alpha = 1
      )
    )
  }
  
  ConnectGroupResponseMeans <- function(data) {
    return(
      geom_line(
               aes(
                 x = group, 
                 y = group.mean
               ),
               data  = data,
               color = brewer.pal(8, "Set1")[2],
               alpha = 1
      )
    )
  }
  
  GroupSummaryPlotTitle <- function(ctr) {
    return(
      opts(title = paste("Responses for all groups\n", "each n = ", ctr$responses.per.group))
    )
  }
  
  GroupSummaryXLabel <- function() {
    return(xlab("Group Indicator"))
  }
  
  GroupSummaryYLabel <- function() {
    return(ylab(ylab))
  }  
  
  CollateOutputPlots <- function(ctr) {
    output <- list(NULL)
    
    for (i in 1:ctr$number.of.contrasts) {
      output[[i]] <- ctr$contrast.plots[[i]]
    }
    output[[ctr$number.of.contrasts + 1]] <- ctr$summary.plot  
    
    return(output)
  }
  
  OptionalPlotPrinting <- function(output) {
    if (print.four.plots.per.page) {
      LayoutFourPlotsPerPage(output)
    }
  }
  
  PrintLinearModelSummary <- function(model) {
    model.summary <- summary(model)
    message("\nLinear Model Summary")
    print(model.summary)
  }
  
  PrintPositiveAndNegativeCoefficientMeansByContrast <- function(contrast.plot.data) {
    output <- GetSummaryDataByContrast(contrast.plot.data)
    output <- RenameSummaryRows(output)
    print(output)
  }
  
  GetSummaryDataByContrast <- function(x) {
    ExtractData <- function(x) {
      summary.data <- x$summary.data
      neg <- summary.data$responses[summary.data$contrasts <= 0]
      pos <- summary.data$responses[summary.data$contrasts > 0]
      diff <- pos - neg
      stEftSze <- NULL
      return(data.frame(neg, pos))
    }
    return(ldply(x, .fun = ExtractData))
  }
  
  GetOutput <- function(ctr) {
    four.plot.message <- "Since you selected four plots per page to print, no output was returned"
    if (print.four.plots.per.page) {
      message(four.plot.message)
      LayoutFourPlotsPerPage(ctr$output)
      output <- NULL
    }
    
    else {
      output <- ctr$output
    }
    return(output)
  }
  
  ctr                        <- AdaptVariablesFromGranovaComputations()
  ctr$linear.model           <- GetLinearModel(ctr)
  ctr$contrast.plot.data     <- GetContrastPlotData(ctr)
  ctr$contrast.plots         <- GetContrastPlots(ctr)
  ctr$summary.plot.data      <- GetSummaryPlotData(ctr)
  ctr$summary.plot           <- ComposeSummaryPlot(ctr$summary.plot.data)
  ctr$output                 <- CollateOutputPlots(ctr)
  PrintLinearModelSummary(ctr$linear.model)
    
  return(ctr$contrast.plot.data)

}
