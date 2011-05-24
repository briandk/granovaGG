granova.contr.ggplot <- function(data, contrasts, ylab = "Outcome (response)", xlab = NULL, jj = 1) {

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
          number.of.contrasts           = dim(Xcons)[2]
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
        
      plot.data <- data.frame(x.values, y.values)
  
    return(plot.data)
  }

  GetContrastPlots <- function (ctr) {
    return(
      lapply(
        X             = 1:ctr$number.of.contrasts,
        FUN           = CreateContrastPlot,
        contrast.data = ctr$contrast.plot.data
      )
    )
  }
  
  CreateContrastPlot <- function(contrast.data, index) {
    p <- ggplot()
    p <- p + geom_point(
               aes(x = x.values, y = y.values),
               data = contrast.data[[index]],
               position = position_jitter(height = 0, width = 1/100)
             )
    return(p)
  }
  
  ctr                        <- AdaptVariablesFromGranovaComputations()
  ctr$contrast.plot.data     <- GetContrastPlotData(ctr)
  ctr$contrast.plots         <- GetContrastPlots(ctr)

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

  return(ctr$contrast.plots[[1]])


}
