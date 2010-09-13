\name{granova.1w}
\alias{granova.1w}

\title{Graphic display for one-way ANOVA}
\description{Graphic to display data for a one-way analysis of variance, and also to help understand how 
ANOVA works, how the F statistic is computed, etc.  The graphic may be called 'elemental' or 'natural' because it is built
around the key question that drives one-way ANOVA.}
\usage{
granova.1w(yy, group = NULL, dg = 2, h.rng = 1.25, v.rng = 0.2, 
   box = FALSE, jj = 1, kx = 1, px = 1, size.line = -2.5, 
   top.dot = 0.15, trmean = FALSE, resid = FALSE, dosqrs = TRUE, 
   ident = FALSE, pt.lab = NULL, xlab = NULL, ylab = NULL, 
   main = NULL, ...)
}
\arguments{
  \item{yy}{Dataframe or vector. If a dataframe, the two or more columns are taken to be groups of equal size (whence `group' is NULL).
            If yy is a vector, `group' must be a vector, perhaps a factor, that indicates groups (unequal group sizes allowed with this option). }
  \item{group}{Group indicator, generally a factor in case yy is a vector.}
  \item{dg}{Numeric; sets number of decimal points in output display, default = 2.}
  \item{h.rng}{Numeric; controls the horizontal spread of groups, default = 1.25}
  \item{v.rng}{Numeric; controls the vertical spread of points, default = 0.25. }
  \item{box}{Logical; provides a bounding box (actually a square) to the graph; default FALSE.}
  \item{jj}{Numeric; sets horizontal jittering level of points; when pairs of ordered means are close to one another, try jj < 1; default = 1.}
  \item{kx}{Numeric; controls relative sizes of `cex', default = 1.0}  
  \item{px}{Numeric; controls relative sizes of `cex.axis', default = 1.0}
  \item{size.line}{Numeric; controls vertical location of group size and name labels, default = -2.5.}
  \item{top.dot}{Numeric; controls hight of end of vertical dotted lines through groups; default = .15.}
  \item{trmean}{Logical; marks 20\% trimmed means for each group (as green cross) and prints out those values in output window, default = FALSE.}
  \item{resid}{Logical; displays marginal distribution of residuals (as a 'rug') on right side (wrt grand mean), default = FALSE.}
  \item{dosqrs}{Logical; ensures inclusion of the squares (for variances); when FALSE and the number of groups = 2, squares will be suppressed, default = TRUE.}
  \item{ident}{Logical; allows user to identify specific points on the plot, default = FALSE.}
  \item{pt.lab}{Character vector; allows user to provide labels for points, else the rownames of xdata are used (if defined),
            or if not labels are 1:N (for N the total number of all data points), default = NULL.}
  \item{xlab}{Character; horizontal axis label, default = NULL.}
  \item{ylab}{Character; vertical axis label, default = NULL.}
  \item{main}{Character; main label, top of graphic; can be supplied by user, default = NULL, which leads to printing of a generic title for graphic.}
  \item{...}{Optional arguments to be passed to \code{identify}, for example \code{offset}}

}
\details{The central idea of the graphic is to use the fact that a one way analysis of variance F statistic is the ratio of two variances each
     of which can usefully be presented graphically. In particular, the sum of squares between (among) can be represented as the 
     sum of products of so-called effects (each being a group mean minus the grand mean) and the group means; when these effects 
     are themselves plotted against the group means a straight line necessarily ensues. The group means are plotted as red 
     triangles along this line. Additionally, data points are displayed (re: vertical axis) with respect to these 
     group means for each group. Once this elemental plot has been generated the one-way ANOVA residuals can be 
     displayed as a rug plot (on the right margin), whence it is recognized that the pooled standard deviation of 
     the residuals, when squared, is just the mean square within.  The conventional F statistic is just a ratio of 
     the between to the within mean squares, or variances, and it is straightforward to represent each variance as the 
     area of a square in the graphic. Specifically, the blue square, centered on the grand mean vertically and zero for 
     the X-axis, corresponds to the mean square within (with side based on [twice] the pooled standard deviation); 
     the second square corresponds to the mean square between, and is also centered on the grand mean. 
     The ratio of the areas of these squares is the F-statistic. By using effects to locate the groups 
     in the order of the observed means, from left to right for smallest to largest, an 'elemental' graphic is 
     defined for this commonly used statistical method.

     Groups need not be of the same sizes, nor do data need to reflect any particular distributional characteristics. 
     But skewness, outliers, clustering of scores, and various other features of the data can often be readily seen in 
     this framework. Trimmed means (20\%) can also be displayed if desired. Finally, by redisplaying the response data in 
     two or more versions of the graphic it will often be possible to visualize the effects of transformations. Finally, individual
     points can be labeled in the graphic (\code{ident=TRUE}).
}
\value{Returns a list with two components:
\item{grandsum}{Contains the basic ANOVA statistics: the grandmean, the degrees of freedom and mean sums of squares between and within groups,
 the F statistic, F probability and the ratio between the sum of squares between groups and the total sum of squares.}
\item{stats}{Contains a table of statistics by group: the size of each group, the contrast coefficients used in plotting the groups, the weighted
 means, means, and 20\% trimmed means, and the group variances and standard deviations.}
}
  \references{Fundamentals of Exploratory Analysis of Variance, Hoaglin D., Mosteller F. and Tukey J. eds., Wiley, 1991.
}

\author{Robert M. Pruzek \email{RMPruzek@yahoo.com}, 

James E. Helmreich \email{ James.Helmreich@Marist.edu}  
}
 
\seealso{\code{\link{granova.2w}}, \code{\link{granova.contr}}, \code{\link{granova.ds}} }
\examples{
library(MASS)
wt.gain <- anorexia[,3] - anorexia[,2]
granova.1w(wt.gain, group = anorexia[,1], size.line = -3)
#
attach(ChickWeight)
CW0 <- subset(ChickWeight, Time==0)[,c(1,4)]
granova.1w(yy = CW0[,1], group = CW0[,2], jj = 2)
#
CW21 <- subset(ChickWeight, Time==21)[,c(1,4)]
granova.1w(yy = CW21[,1], group = CW21[,2])

}
\keyword{hplot}
\keyword{htest}
