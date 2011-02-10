# CASE 1: data is a df
# R> w <- c(9,2,3,1)
# R> x <- c(1,2,3,4)
# R> y <- c(5,6,7,8)
# R> z <- c(9,0,1,2)
# R> df <- data.frame(w=w, x=x, y=y, z=z)
# R> granova.1w(data=df)

# CASE 2: data is a vector
# data <- c(1,2,3,4,5,6,7,8,9)
# grouping <- c(1,1,1,2,2,3,3,3,3)
# dumbGroupingVector <- c(1, 2, 3)
# granova.1w (data=data, group=grouping)

granova.1w.ggplot <- function(data, 
                              group      = NULL, 
                              dg         = 2, 
                              h.rng      = 1.25, 
                              v.rng      = .2, 
                              box        = FALSE, 
                              jj         = 1, 
                              kx         = 1, 
                              px         = 1,
                              size.line  = -2.5, 
                              top.dot    = .15, 
                              trmean     = FALSE, 
                              resid      = FALSE,  
                              dosqrs     = TRUE, 
                              ident      = FALSE, 
                              pt.lab     = NULL, 
                              xlab       = NULL, 
                              ylab       = NULL, 
                              main       = NULL,
                              plot.theme = NULL, ...
                             )

{
# Graphic corresponds to conventional one-way ANOVA, either vector or matrix input; plots grouped data from yy.
# If yy is matrix, columns are taken to be groups of equal size (so 'group' is NULL).
# If yy is a vector, 'group' must be a vector, perhaps a factor, indicating groups.
# Group data sets are initially reordered according to sizes of their means, low (left) to high (right).
# Argument 'dg' sets number of decimal points in output display (& can have fundamental importance).
# Arguments 'h.rng' and 'v.rng' control (resp.) the horizontal spread of groups and the vertical spread group points.
# Argument 'box' places a box around graph (default FALSE).
# Argument 'jj' sets horiz. jittering level of points; when pairs of ordered means are close to one another, try jj < 1.
# Arguments 'k' and 'p' (numeric) control relative sizes of 'cex' and 'cex.axis' respectively. 
# Argument 'size.line' is numeric; controls vertical location of group size and name labels, default = -2.5.
# Argument 'trmean = TRUE' (default FALSE) marks 20% trimmed means for each group, and prints out those values in the output window.
# Argument 'resid = TRUE' (default FALSE) gives marginal distribution of residuals (as a 'rug') on right side (wrt grand mean).
# Argument 'dosqrs = TRUE' (default) overlays blue & red boxes to represent within & between mean squares;
# sides correspond to twice the corresponding standard deviations, so areas of the squares correspond to (4 times) the mean squares.
# Argument 'dosqrs = TRUE' (default) ensures inclusion of the squares; but (say, # of groups = 2) the squares can be suppressed
# Argument 'ident' allows user to identify specific points on the plot
# Argument 'pt.lab' allows user to provide labels for points, else the rownames of xdata are used (if defined), or if not labels are 1:n.
# in which case the t-statistic replaces F (see below); also if # of groups = 2, the standardized effect size is printed.
# The F statistic is reflected by the RATIO of the areas of red box to the blue box;
# and if there are only two groups, the standardized effect is printed.
# Finally, a wide variety of numerical summary statistics are generated to complement graphic results; see grandsum & stats.
# Please address questions, or make suggestions for improvements, to: rmpruzek@yahoo.com or james.helmreich@marist.edu

yy <- data

#Setting graph to square
#! op <- par(no.readonly = TRUE)
#! on.exit(par(op))
#! par(pty='s')

mtdff <- is.data.frame(yy)
ln.yy<-(length(names(yy))>1)
if(mtdff){
  # data is a data.frame
  if(ln.yy){ 
    yy <- as.matrix(yy)
  }
}

#Testing input data type
mtx <- is.matrix(yy)
if (!mtx) {
  # if this executes, yy is already a vector as handed in via data
  yr <- yy
  groupf<-factor(group)
}       
        
#If yy is matrix sans colnames, need to give them some
if(mtx & is.null(colnames(yy))) { #Note changes here;did not work before, and I thought LETTERS looked better (your thoughts?)
     dmyy2<-dim(yy)[2]
     cnms<-LETTERS[1:dmyy2]     #Note that numbers replaced by LETTERS if initial matrix yy does not have col. names
     colnames(yy)<-c(paste(" G",cnms)) 
}  #1:dim(yy)[2]))

# If yy is a matrix, the data represents a balanced case. The code below creates a yr (outcomes) vector and a groupf (factored groups) vector by repeating each of the column numbers (group) of the source matrix for each of the outcomes in that column.
if(mtx) { 
  group <- rep(1:ncol(yy), each = nrow(yy)) 
  groupf<-factor(group,labels=colnames(yy))
  yr <- as.vector(yy)
}
      
ngroups<-length(unique(groupf))

# By this point, all data have been transformed to yr and groupf - two vectors of equal length. Think of them as the "score" and "group" columns of an n x 2 dataframe, where n = total number of observations.



#
# all we now care about are yr, a vector, and groupf, a vector of group names/labels
#



#Basic stats by group; 'stats' is matrix with rows corresponding to groups, columns for effect size contrasts, means, variances & sd's
mvs <- function(x){c(mean(x),var(x),sd(x))}
stats <- matrix(unlist(tapply(yr,groupf,mvs)),byrow=T,ncol=3)
# stats will have as many rows as we have groups, one row per group
  
#      [,1]   [,2]  [,3]
# [1,] 3.75 12.917 3.594
# [2,] 2.50  1.667 1.291
# [3,] 6.50  1.667 1.291
# [4,] 3.00 16.667 4.082


groupn <- as.numeric(groupf)
# [1] 1 1 1 1 2 2 2 2 3 3 3 3 4 4 4 4

#yrm is a vector of same length as 'groupn' with the appropriate group mean in each cell
yrm <- stats[,1][groupn] 

# The second bracket notation is a way of repeatedly selecting from the stats matrix
# > x <- c("a", "b", "c")
# > y <- c(1,1,1,2,2,2,3,3,3)
# > x[y]
# [1] "a" "a" "a" "b" "b" "b" "c" "c" "c"


#  [1] 3.75 3.75 3.75 3.75 2.50 2.50 2.50 2.50 6.50 6.50 6.50 6.50 3.00 3.00 3.00 3.00


tabc <- table(groupf)
#mn.n is mean groupsize
mn.n <- mean(tabc)
tabc.dm <- tabc/mn.n
grandmean <- mean(yr)

#Stats now has 6 cols, first is group size, second group mean minus grandmean, 
#third is weighted (by group size) mean, then mean, var, sd.

stats <- cbind(tabc,stats[,1]-grandmean, tabc.dm * stats[,1],stats)
#! pt.size<-1
#! if(max(stats[,1] >= 20))pt.size <- .7

#Creating x, y ranges for graph
#Parameters h.rng, v.rng, jj for horizontal, vertical and jitter enabled.

# generate the contrasts
stats.vc <- yrm - grandmean

# We now have four vectors, each of length n = number of observations.
# yr contains the raw observations
# groupf is the vector of factors
# yrm is the vector of group means for each raw score
# stats.vc is the vector of contrasts

rng.v <- range(yr)
rng.h <- h.rng*range(stats.vc)
rng.vv <- c(rng.v[1]-v.rng*diff(rng.v), rng.v[2] + v.rng * diff(rng.v))
rng.rt <- diff(rng.vv)/diff(rng.h)
rng.sts <- range(stats[, 2])


ammt <- (jj/200) * diff(rng.sts)
stats.vcj<-jitter(stats.vc, am = ammt)
if(is.null(main))main <- paste('One-way ANOVA displaying',ngroups,'groups')


#! plot(stats.vcj, yr, pch = 16, cex = pt.size, xlab = "", ylab = "", xlim = rng.h, ylim = rng.vv, axes = F, main = main)
#! if(box)box(lwd = 1.5)

#Reordering the stats matrix by the mean of each group
statsro<-stats[order(stats[,4]),]

#Labels vertical dotted lines through groups with group sizes, names
#! mtext(side = 3,text = paste(as.character(statsro[,1])), at = statsro[,2], las = 2, 
#!         line = size.line, cex.axis = .85*px, cex = .7*kx, col = 'darkred')
#! 
#! mtext(side = 3,text = paste(rownames(statsro)), at = statsro[,2], las = 2, 
#!         line = size.line+.9, cex.axis = .85*px, cex = .7*kx, col = 'black')
#! mtext(side = 3, text = " Group Sizes:", adj=0, line = size.line, cex=.65*kx, col = 'darkred')
#! mtext(side = 3, text = "|", at = statsro[,2], las=1, adj = 0, line = size.line-.9, cex.axis = .85*px, cex = .7*kx)


#Adds green dot and horizontal green dashed line for grandmean
#! points(0, grandmean, pch = 16, cex = 1.7*kx,col=3) 
#! abline(h=grandmean,lty=3,lwd=2,col=3)

#Bottom axis values for contrast coefficients
#! axis(side = 1, at = round(statsro[,2],2), las = 2, cex.axis=.6*px)
#! axis(side = 1, at = 0, line=.75, cex.axis=.75*px)

#Leftside axis has grandmean value and max min plotted; Bob: might reset digits to dg instead of dg-1
#! axis(side = 2, at = round(grandmean, dg-1), cex=.55*kx)
#! axis(side = 2, at = round(range(yr), dg-1), cex=.55*kx)

#Rightside axis is labeled with group means
#! axis(side = 4, at = round(stats[,4],dg-1), las=2, cex.axis = .7*px)

#Horizontal and vertical lines at each group mean; line through group means
#abline(h=stats[,4],lty=2,lwd=.4,col=grey(.1))
#! segments(stats[,2],stats[,4],10^10,stats[,4],lty=2,lwd=.6,col=gray(.7))
#! for(i in 1:ngroups)lines(rep(stats[i,2],2),c(rng.vv[1]- .1* diff(range(yr)), max(yr)+top.dot*diff(range(yr))),lty=3,lwd=1,col=grey(.8))
#! lines(stats[,2],stats[,4],lwd=1.6,col=4)


#Red filled triangles at group means
#! points(stats[,2],stats[,4],pch=2,cex=1.45*kx)
#! points(stats[,2],stats[,4],pch=17,cex=1.3*kx,col='red')

#Calculation of squares etc.
r.xy.sqd<-cor(yr,stats.vc)^2
SS.tot<-(length(yr)-1)*var(yr)
SS.bet<-r.xy.sqd*SS.tot
df.b<-ngroups-1
df.w<-length(yr)-1-df.b
SS.w<-SS.tot-SS.bet
MS.w<-SS.w/df.w
MS.b<-SS.bet/df.b
residuals<-round(yr-stats.vc,3)

#Rug plot of residuals
#! if(resid)rug(residuals,side=4,ti=.02,lwd=1.3,col=4)

#sdw is standard deviation within, ie of residuals.
sdw<-sd(residuals)*sqrt((length(yr)-1)/df.w)

#This interval based on pooled standard error within.
grandmean.pm.sdw<-c(grandmean-sdw,grandmean+sdw) 
grandmean.pm.sewR<-round(grandmean.pm.sdw,dg-1)
#! axis(side=2,at=grandmean.pm.sewR,col=4,cex.axis=.7*px)
#! mtext(c('gm-sdw','gm+sdw'),at=grandmean.pm.sdw,side=2,line=2.1,col=4,cex=.7*kx) 

#! abline(h=grandmean.pm.sdw,lty=3,lwd=1,col='blue')
F.stat <- MS.b/MS.w

#x and y labels
#! if(!is.null(xlab))title(xlab=xlab,cex=.5*kx)
#! else{title(xlab = "Contrast coefficients based on group means and sizes",cex=.4*kx)}
#! if(!is.null(ylab))title(ylab=ylab,cex=.5*kx,line=3.7)
#! else{title(ylab = "Dependent variable (response)",cex=.5*kx,line=3.7)}

#Legends 
#! legend(x="bottomleft", legend = c("Group Means","Grand Mean"), 
#!         col = c(2,3), pch = c(17,16), cex = .8*kx, bty = "n")
#! 
#! if(dosqrs){legend(x = "bottomright", legend = c("MS-within", 
#!         "MS-between", paste("F-statistic = ", round(F.stat,2))), 
#!         col = c(4,2,1), pch = c(0,0,-1), cex = .8*kx, bty = "n")
#! }else{legend(x = "bottomright", legend = c(paste("F-statistic = ", round(F.stat,2))), 
#!         col = 1, pch = -1, cex = .8*kx, bty = "n")
#!         }
        
p.F <- 1 - pf(F.stat, df.b,df.w)
sqrF<-sqrt(F.stat)
sqrs<-2*sqrt(MS.w)/rng.rt
#! if(dosqrs)symbols(0,grandmean,squares=sqrs,lwd=2,inches=F,fg='blue',lty=1,,add=T,cex=.8)
#! if(dosqrs)symbols(0,grandmean,squares=sqrs*sqrt(F.stat),lwd=2,inches=F,fg='red',lty=1,,add=T,cex=.8)

#Trimmed means marked and outputted if 'trmean = TRUE'
trmd.mn<-tapply(yr,list(groupf), mean, tr=.2)
#! if(trmean){points(statsro[,2], trmd.mn[order(stats[, 4])], pch=4, cex=1.8, col='darkgreen') 
#!           legend(x = "bottom", legend = "20% trimmed means", col = 'darkgreen', pch = 4, cex = .8*kx, bty = "n")  
#!          }

gsum<-array(c(grandmean, df.b, df.w, MS.b, MS.w, F.stat, p.F, round(r.xy.sqd,3)))
dimnames(gsum)<-list(c('Grandmean', 'df.bet', 'df.with', 'MS.bet', 'MS.with', 'F.stat', 'F.prob', 'SS.bet/SS.tot'))
stats.out<-cbind(statsro[,1:4],round(as.matrix(trmd.mn[order(stats[,4])]),2),statsro[,5:6])

dimnames(stats.out)[2]<-list(c('Size','Contrast Coef',
"Wt'd Mean",'Mean', "Trim'd Mean" , 'Var.','St. Dev.'))
out<-list(grandsum = round(gsum, dg), stats = round(stats.out, dg))

if(ident){
         if(is.null(pt.lab) & !mtx & !is.null(rownames(yy))){pt.lab<-rownames(yy)}
         if(is.null(pt.lab) & !mtx & is.null(rownames(yy))){pt.lab<-1:length(yy)}
         if(is.null(pt.lab) & mtx){pt.lab<-paste(rep(1:dim(yy)[1],dim(yy)[2]),",", rep(1:dim(yy)[2],ea = dim(yy)[1]), sep="")}
         identify(stats.vcj,yr,labels = pt.lab, ...)
         }


AdaptVariablesFromGranovaComputations <- function() {
  result  <- list(data = data.frame(score             = yr,
                                    group             = groupf,
                                    group.mean        = yrm,
                                    contrast          = stats.vc
                         )
  )
  result$stats <- list(F.statistic               = F.stat,
                       SS.between                = SS.bet,
                       SS.within                 = SS.w,
                       df.between                = df.b,
                       df.within                 = df.w,
                       grand.mean                = grandmean,
                       square.side.length        = sqrs,
                       sd.within                 = sdw
  )
  result$residuals <- data.frame(residuals                 = residuals,
                                 within.1.sd.of.grand.mean = abs(residuals - grandmean) < sdw
  )
  return(result)
}

GetSummary <- function(owp) {
  return(
    ddply(owp$data, .(group), summarise,
      group             = unique(group),
      group.mean        = mean(score),
      contrast          = unique(contrast),
      weighted.variance = (length(score) - 1) * var(score)
    )
  )
}

GetMSwithinSquare <- function(owp) {
  return(
    data.frame(
      xmin = -owp$stats$sd.within,
      xmax = owp$stats$sd.within,
      ymin = owp$stats$grand.mean - (owp$stats$sd.within),
      ymax = owp$stats$grand.mean + (owp$stats$sd.within)
    )
  )
}

GetMSbetweenSquare <- function(owp) {
  return(
    data.frame(
      xmin = -owp$stats$sd.within * (1 + sqrt(owp$stats$F.statistic/2)),
      xmax = owp$stats$sd.within  * (1 + sqrt(owp$stats$F.statistic/2)),
      ymin = 1,
      ymax = 5
    )
  )
}

GetGroupMeanLine <- function(owp) {
  return(
    data.frame(
      x     = min(owp$data$contrast),
      y     = min(owp$data$group.mean),
      xend  = max(owp$data$contrast),
      yend  = max(owp$data$group.mean),
      color = "blue"      
    )
  )
}

GetGraphicalParameters <- function(owp) {
  .score.range.distance    <- (max(owp$data$score) - min(owp$data$score))
  .aggregate.breaks        <- c(owp$summary$group.mean, range(owp$data$score))
  .y.range                 <- range(owp$data$score)
  .x.range                 <- c(-.score.range.distance/2, .score.range.distance/2)
  .percent.offset          <- .score.range.distance / 100
  
  return(list(
           aggregate.breaks = .aggregate.breaks,
           y.range          = .y.range,
           x.range          = .x.range
         )
  )
}

GetColors <- function() {
  # Color Brewer Palette: 166, 206, 227; 31, 120, 180; 178, 223, 138; 51, 160, 44; 
  colors <- c(
   rgb(166, 206, 227, max = 255),
   "green",
   rgb(51, 160, 44, max = 255),
   rgb(31, 120, 180, max = 255),
   "steelblue",
   rgb(178, 223, 138, max = 255),
   "black",
   "grey50"
  )
  
  names(colors) <- c(
    "MS-between",
    "Grand Mean",
    "Group Means",
    "MS-within",
    "SE-within",
    "Group Mean Line",
    "TRUE",
    "FALSE"
  )
  
  return(colors)
}

GetStandardError <- function(owp) {
  return(
    data.frame(SE = c(owp$stats$grand.mean + owp$stats$sd.within,
                      owp$stats$grand.mean - owp$stats$sd.within)
    )
  )
}

# Pepare OWP object
owp                   <- AdaptVariablesFromGranovaComputations()
owp$summary           <- GetSummary(owp)
owp$group.mean.line   <- GetGroupMeanLine(owp)
owp$params            <- GetGraphicalParameters(owp)
owp$colors            <- GetColors()
owp$standard.error    <- GetStandardError(owp)
owp$ms.within.square  <- GetMSwithinSquare(owp)
owp$ms.between.square <- GetMSbetweenSquare(owp)


######## Plot Functions Below

InitializeGgplot <- function() {
  return(ggplot())
}

GrandMeanLine <- function(owp) {
  return(geom_hline(
    yintercept = mean(owp$data$score),
    color      = "green")
  )
}

GrandMeanPoint <- function(owp) {
  return(
    geom_point(
      aes(
        x = 0, y = mean(score), color = factor(paste("Grand Mean"))
      ), 
      data = owp$data
    )
  )
}

ScaleX <- function(owp) {
  return(scale_x_continuous(
    breaks = (owp$summary$contrast),
    labels = round(owp$summary$contrast, digits = 1),
    limits = owp$params$x.range,
    expand = c(0.1, 0))
  )
}

ScaleY <- function(owp) {
  return(
    scale_y_continuous(
      breaks = (owp$params$aggregate.breaks),
      labels = round(owp$params$aggregate.breaks, digits = 1),
      limits = owp$params$y.range,
      expand = c(0.1, 0),
    )
  )
}

ScoresByGroupContrast <- function(owp) {
  return( 
    geom_point( 
      aes(x = contrast, y = score), 
      size = I(3/2),
      data = owp$data
    )
  )
}

GroupMeanLine <- function(owp) {
  return(geom_segment(
    aes(
      x      = x,
      y      = y,
      xend   = xend,
      yend   = yend,
      color  = factor(paste("Group Mean Line"))
    ), data  = owp$group.mean.line
  ))
}

GroupMeansByContrast <- function(owp) {
  return( 
    geom_point( 
             aes(
               x     = contrast, 
               y     = group.mean, 
               fill  = factor("Group Means")), 
               data  = owp$summary, 
               color = "black",
               shape = 24,
               size  = I(3)
    )
  )
}

Residuals <- function(owp) {
  return(
    geom_rug(
           aes(
             x     = NULL, 
             y     = residuals,
             color = factor(within.1.sd.of.grand.mean) 
           ),
           data  = owp$residuals
    )
  )
}

MSwithinSquare <- function() {
  return(
    geom_rect(
            aes(
              xmin   = xmin,
              xmax   = xmax,
              ymin   = ymin,
              ymax   = ymax,
              color  = factor("MS-within")
            ), data  = owp$ms.within.square,
               fill  = NA,
    )
  )
}

MSbetweenSquare <- function() {
  return(
    geom_rect(
            aes(
              xmin   = xmin,
              xmax   = xmax,
              ymin   = ymin,
              ymax   = ymax,
              color  = factor(paste("MS-between"))
            ), data  = owp$ms.between.square,
               fill  = NA,
    )
  )
}

StandardError <- function(owp) {
  return(
    geom_hline(
             aes(color = factor("SE-within"), yintercept = SE),
             data = owp$standard.error
    )
  )
}

ColorScale <- function(owp) {
  return(scale_color_manual(value = owp$colors, name = ""))
}

FillScale <- function() {
  return(scale_fill_manual(value = owp$colors, name = ""))
}

XLabel <- function() {
  if (is.null(xlab)) {
    return(xlab("Contrast coefficients based on group means and sizes"))
  }
  
  else {
    return (xlab(xlab))
  }
}

YLabel <- function() {  
  if (is.null(ylab)) {
    return(ylab("Dependent variable (response)"))
  }
  
  else {
    return (ylab(ylab))
  }
}

RotateXTicks <- function() {
  return(opts(axis.text.x = theme_text(angle = 90)))
}

Theme <- function() {
  if (is.null(plot.theme)) {
    return(theme_granova())
  }
  
  else {
    return(plot.theme)
  }
}

ForceCoordinateAxesToBeEqual <- function(owp) {
  return(coord_fixed())
}

Title <- function() {
  classic.granova.1w.title <- paste("One-way ANOVA displaying",ngroups,"groups")
  
  if (main == classic.granova.1w.title) {
    return(
        opts(
          title = paste( "One-way ANOVA displaying ", ngroups, " groups ","(F = ", round(F.stat, digits = 2), ")", sep = "")
        )
    )
  }
  
  else {
    return(opts(title = main))
  }
}

#Plot OWP object
p <- InitializeGgplot()
p <- p + GrandMeanLine(owp)
p <- p + GrandMeanPoint(owp)
p <- p + ScaleX(owp)
p <- p + ScaleY(owp)
p <- p + ScoresByGroupContrast(owp)
p <- p + GroupMeanLine(owp)
p <- p + GroupMeansByContrast(owp)
p <- p + Residuals(owp)
p <- p + MSwithinSquare()
p <- p + MSbetweenSquare()
p <- p + StandardError(owp)
p <- p + ColorScale(owp)
p <- p + FillScale()
p <- p + XLabel()
p <- p + YLabel()
p <- p + RotateXTicks()
p <- p + Theme()
p <- p + ForceCoordinateAxesToBeEqual(owp)
p <- p + Title()

return(p)
}
