granova.contr.ggplot <- function(data, 
                                 contrasts, 
                                 ylab       = "Outcome (response)", 
                                 xlab       = NULL, 
                                 jj         = 1,
                                 plot.theme = "theme_granova_contr",
                                 ...
                        ) 
{

  # Plots responses by contrasts.
  # 'data' must be vector of scores for all equal size groups.
  # 'con' must be matrix of column contrasts with dimensions (number of groups) x (number of contrasts)
  # [generally n X n-1].  The number of rows = number 'cells' or groups.
  # Basic lm (regression) results are provided; orthogonal contrasts are ideal (but not essential).
  # 'jj' controls jitter.

  jj<-.1*jj

  resp <- data
  con  <- contrasts
  ngrp <- nrow(con)

  if(!is.null(dim(resp))){
  	if(dim(resp)[2] > 1){resp <- stack(as.data.frame(data))[,1]}}
  npg <- length(resp)/ngrp

  con.tstr <- svd(con, nv=0, nu=0)$d
  if(min(con.tstr) < sqrt(.Machine$double.eps)){
      stop('Contrasts matrix is singular; it must be non-singular')}

  # The following two functions are used below.

  #Generates a 'standardized contrast vector'; positive & abs(negative) values sum to 1
  #cont assumed to consist of contrast(s) vector/matrix w/ mean zero; otherwise stops
      std.contr <- function(cont, tol = sqrt(.Machine$double.eps)^0.6) {
          if (!is.matrix(cont)) {
              cont <- as.matrix(cont)
          }
          if (sum(abs(colMeans(cont))) > tol) {
              stop("Input vector/matrix must have mean zero (for each column)")
          }
          if (ncol(cont) == 1) {
              cont <- matrix(cont, ncol = 1)
          }
          dg <- apply(abs(cont), 2, sum)
          if (length(dg) == 1) {
              dg <- as.matrix(dg)
          }
          s.cont <- round(2 * cont %*% diag(1/dg), 3)
          s.cont
      }

  #Generates indicator matrix w/ 1 entry per row, acc. index in vector xx
  indic <- function(xx) {
             mm <- matrix(0, length(xx), length(unique(xx)))
             indx <- ifelse(xx == col(mm), 1, 0)
             indx
  }
        
  vn <- rep(1:ngrp, ea = npg)
      N <- length(resp)
      xind <- indic(vn)
      if (!is.matrix(con)) {
          con <- as.matrix(con)
      }
  Xcon <- xind %*% con
  Xcons <- std.contr(Xcon)
  ncx <- ncol(Xcons)
  dimnames(Xcon)[2] <- list((unclass(dimnames(con))[2])[[1]])
  dmm<-dimnames(Xcon)[2][[1]]
  if(is.null(dmm))dmm<-1:ncol(con)

  # 'ctr' is shorthand for the ConTRast data object that will hold all the information for plotting
  AdaptVariablesFromGranovaComputations <- function () {
    return(
        list(
          response                      = resp,
          contrast.matrix               = contrasts,
          scaled.standardized.contrasts = Xcons * npg,
          number.of.contrasts           = dim(Xcons)[2],
          number.of.groups              = ngrp,
          number.per.group              = npg
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
    p <- p + JitteredResponsesByContrast(plot.data[[index]]$raw.data)
    p <- p + EffectsOfContrasts(plot.data[[index]]$summary.data)
    p <- p + ConnectEffectMeans(plot.data[[index]]$summary.data)
    p <- p + Theme(plot.theme)
    p <- p + ContrastPlotTitle(index)
    p <- p + ContrastPlotXLabel(index)
    p <- p + ContrastPlotYLabel()
    
    return(p)
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
               alpha = 1/2
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
               alpha = I(1/2)
      )
    )
  }
  
  ContrastPlotTitle <- function(index) {
    return(
      opts(title = paste("Coefficients vs. Response, Contrast ", index))
    )
  }
  
  ContrastPlotXLabel <- function(index) {
    return(
      xlab(paste("Contrast ", index))
    )
  }
  
  ContrastPlotYLabel <- function() {
    return(
      ylab(paste("Response"))
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
    p <- p + RawScoresByGroup(plot.data$raw.data)
    p <- p + MeansByGroup(plot.data$summary.data)
    p <- p + ConnectGroupResponseMeans(plot.data$summary.data)
    p <- p + Theme(plot.theme)
    p <- p + GroupSummaryPlotTitle()
    p <- p + GroupSummaryXLabel()
    p <- p + GroupSummaryYLabel()
    return(p)
  }
  
  RawScoresByGroup <- function(data) {
    return(
      geom_point(
               aes(
                 x = variable, 
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
               alpha = 1/2
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
               alpha = 1/2
      )
    )
  }
  
  GroupSummaryPlotTitle <- function(index) {
    return(
      opts(title = paste("Responses for all groups, each n = ", ctr$number.per.group))
    )
  }
  
  GroupSummaryXLabel <- function() {
    return(xlab("Group"))
  }
  
  GroupSummaryYLabel <- function() {
    return(ylab("Response"))
  }  
  
  CollateOutputPlots <- function(ctr) {
    output <- list(NULL)
    
    for (i in 1:ctr$number.of.contrasts) {
      output[[i]] <- ctr$contrast.plots[[i]]
    }
    output[[ctr$number.of.contrasts + 1]] <- ctr$summary.plot  
    
    return(output)
  }
  
  ctr                        <- AdaptVariablesFromGranovaComputations()
  ctr$contrast.plot.data     <- GetContrastPlotData(ctr)
  ctr$contrast.plots         <- GetContrastPlots(ctr)
  ctr$summary.plot.data      <- GetSummaryPlotData(ctr)
  ctr$summary.plot           <- ComposeSummaryPlot(ctr$summary.plot.data)
  ctr$output                 <- CollateOutputPlots(ctr)
  
  #! Change to 4x4 plotting, and return to original at end.
  #! op <- par(no.readonly = TRUE)
  #! on.exit(par(op))
  #! par(mfrow = c(2, 2))

  #! NB different scalings of columns of Xcon.s feasible  AND WORTH CONSIDERING
  #!    Xconss <- Xcons * npg
  #!    rgx <- range(Xconss)
  #!    rgy <- range(resp)
  #!    rgxd <- abs(diff(rgx))
  #!    rgyd <- abs(diff(rgx))
  #!    rgx <- rgx + c(-0.1 * rgxd, 0.1 * rgxd)
  #!    rgy <- rgy + c(-0.1 * rgyd, 0.1 * rgyd)
  #!
  #! Note: initialization of mns.cgps, will really be defined in for loop below.
  #! mns.cgps<-matrix(0,ncx,2)
  #! 
  #! for (i in 1:ncx) {
  #!           plot(jitter(Xconss[, i][Xconss[, i] != 0], jj), resp[Xconss[, 
  #!             i] != 0], xlim = rgx, ylim = rgy, xlab = xlab[i], 
  #!             ylab = "", pch = 16, cex = 1)
  #!         title(ylab = ylab)
  #!         title(main = paste("Coefficients vs. Response, Contrast", 
  #!             dmm[i]))
  #!         mnrsp <- mean(resp)
  #!         abline(h = mnrsp, lty = 3, lwd = 0.7, col = "dark red")
  #!         mns.cgps.i <- c(mean(resp[Xconss[, i] < 0]), mean(resp[Xconss[, 
  #!             i] > 0]))
  #!         mns.cgps[i, ] <- mns.cgps.i
  #!         segments(mean(Xconss[, i][Xconss[, i] < 0]), mean(resp[Xconss[, 
  #!             i] < 0]), mean(Xconss[, i][Xconss[, i] > 0]), mean(resp[Xconss[, 
  #!             i] > 0]), lwd = 2, lty = 6, col = 4)
  #!         points(mean(Xconss[, i][Xconss[, i] < 0]), mean(resp[Xconss[, 
  #!             i] < 0]), pch = 1, cex = 2, col = 4)
  #!         points(mean(Xconss[, i][Xconss[, i] > 0]), mean(resp[Xconss[, 
  #!             i] > 0]), pch = 1, cex = 2, col = 4)
  #!         if (i == 4 || i == 8 || i == 12 || i == 16 || i == 20) {
  #!             print("Examine contrast plots & consider printing")
  #!            # pause() is next three lines, taken from DAAG
  #!         if (interactive()){ 
  #!         readline("Pause. Press <Enter> to continue...")
  #!         invisible()}
  #!         }
  #!     }
  #! 
  #! datagps<-matrix(resp,ncol=ngrp)
  #! cM<-colMeans(datagps)

  #! datagps <- matrix(resp, ncol = ngrp)
  #!     cM <- colMeans(datagps)
  #!     plot(jitter(vn, amount = jj/3), resp, xlab = "Group Indicator", 
  #!         ylab = ylab, pch = 16, col = 1, axes = F)
  #!     box()
  #!     lines(x = 1:ngrp, y = cM, lwd = 2, lty = 6, col = 4)
  #!     points(x = 1:ngrp, y = cM, col = 4, pch = 1, cex = 2)
  #!     abline(h = mnrsp, lty = 3, col = 4)
  #!     axis(side = 1, at = c(1:ngrp))
  #!     axis(side = 2, at = NULL)
  #!     title(paste("Responses for all groups, each n=", npg))





  #Xcon reset to con, but now w/ 'standardized' scaling

  return(ctr$output)

}
