\name{granovagg.1w}
\alias{granovagg.1w}
\title{Elemental Graphic Display for One-Way ANOVA}
\usage{
  granovagg.1w(data, group = NULL, h.rng = 1, v.rng = 1, jj
  = NULL, dg = 2, resid = FALSE, xlab = NULL, ylab = NULL,
  main = "default_granova_title", plot.theme =
  "theme_granova_1w", ...)
}
\arguments{
  \item{data}{Dataframe or vector. If a dataframe, the two
  or more columns are taken to be groups of equal size
  (whence \code{group} is NULL).  If \code{data} is a
  vector, \code{group} must be a vector, perhaps a factor,
  that indicates groups (unequal group sizes allowed with
  this option).}

  \item{group}{Group indicator, generally a factor in case
  \code{data} is a vector.}

  \item{h.rng}{Numeric; controls the horizontal spread of
  groups, default = 1}

  \item{v.rng}{Numeric; controls the vertical spread of
  points, default = 1.}

  \item{jj}{Numeric; sets horiz. jittering level of points.
  \code{jj} gets passed as the \code{amount} parameter to
  \code{\link{jitter}}. When \code{jj = NULL} (the default
  behavior), the degree of jitter will take on a sensible
  value. In addition, if pairs of ordered means are close
  to one another and \code{jj = NULL}, the degree of jitter
  will default to the smallest difference between two
  adjacent contrasts.}

  \item{dg}{Numeric; sets number of decimal points in
  output display, default = 2}

  \item{resid}{Logical; displays marginal distribution of
  residuals (as a 'rug') on right side (wrt grand mean),
  default = FALSE.}

  \item{xlab}{Character; horizontal axis label, default =
  NULL.}

  \item{ylab}{Character; vertical axis label, default =
  NULL.}

  \item{main}{Character; main label, top of graphic; can be
  supplied by user, default =
  \code{"default_granova_title"}, which leads to printing
  of generic title for graphic.}

  \item{plot.theme}{argument indicating a ggplot2 theme to
  apply to the graphic; defaults to a customized theme
  created for the one-way graphic}

  \item{...}{Optional arguments to/from other functions}
}
\value{
  Returns a plot object of class \code{ggplot}. The
  function also provides printed output including by-group
  statistical summaries and information about groups that
  might be overplotted (if applicable): \item{group}{group
  names} \item{group means}{means for each group}
  \item{trimmed.mean}{20\% trimmed group means}
  \item{contrast}{Contrasts (group main effects)}
  \item{variance}{variances}
  \item{standard.deviation}{standard deviations}
  \item{group.size}{group sizes} \item{overplotting
  information}{Information about groups that, due to their
  close means, may be overplotted}
}
\description{
  Graphic to display data for a one-way analysis of
  variance -- that is for unstructured groups. Also to help
  understand how data play out in the context of the basic
  one-way model, how the F statistic is generated for the
  data at hand, etc. The graphic may be called 'elemental'
  or 'natural' because it is built upon the central
  question that drives one-way ANOVA (see details below).
}
\details{
  The one-way ANOVA graphic shows how the comparison of
  unstructured groups, viz. their means, entails a
  particular linear combination (L.C.) of the group means.
  In particular, we use the fact that the numerator of the
  one-way F statistic, the mean square between (MS.B), is a
  linear combination of the group means; each weight -- one
  for each group -- in the L.C. is (principally) a function
  of the difference between the group's mean and the grand
  mean, viz., (M_{j} - M..) where M_{j} denotes the jth
  group's mean, and M.. denotes the grand mean. The L.C.
  can be written as a sum of products of the form MS.B =
  Sum((1/df.B)(n_j (M_j - M..) M_j)) for j = 1...J. The
  denominator of the F-statistic, MS.W (mean square
  within), can be described as a 'scaling factor'. It is
  just the (weighted) average of the variances of the J
  groups (j = 1 ... J). (n_{j}'s are group sizes.) The
  differences (M_{j} - M..) are themselves the 'effects' in
  the analysis. When the effects are plotted against the
  group means (the horizontal and vertical axes) a straight
  line necessarily ensues. Group means are plotted as
  triangles along this line. Once the means have been
  plotted, the data points (jittered) for the groups are
  displayed (vertical axis) with respect to the respective
  contrasts. Since the group means are just the fitted
  values in one-way ANOVA, and the deviations of the scores
  within groups are the residuals (subsetted by groups),
  the graphic can be seen as showing fitted vs. residual
  values for the line that shows the locus of ordered group
  means -- from the smallest on the left) the the largest
  (on the right). If desired, the aggregate of all such
  residuals can be plotted (as a rug plot) on the right
  margin of the graphic centered on the grand mean (large
  green dot in 'middle'). The use of effects to locate
  groups this way yields what we term an 'elemental'
  graphic because it is based on the central question that
  drives one-way ANOVA.

  Note that groups need not have the same size, nor do data
  need to reflect any particular distributional
  characteristics. Finally, the gray bars (one for each
  group) at the bottom of the graphic show the relative
  sizes of the group standard deviations with referene to
  the 'average' group s.d. (more precisely, the square root
  of the MS.W). This 'average' corresponds to the thin
  white line that runs horizontally across these bars.
}
\examples{
data(arousal)
#Drug A
granovagg.1w(arousal[,1:2], h.rng = 1.6, v.rng = 0.5, top.dot = .35)

#########################

library(MASS)
wt.gain <- anorexia[, 3] - anorexia[, 2]
granovagg.1w(wt.gain, group = anorexia[, 1], size.line = -3)

##########################

data(poison)
##Note violation of constant variance across groups in following graphic.
granovagg.1w(poison$SurvTime, group = poison$Group, ylab = "Survival Time")
##RateSurvTime = SurvTime^-1
granovagg.1w(poison$RateSurvTime, group = poison$Group, 
ylab = "Survival Rate = Inverse of Survival Time")

##Nonparametric version: RateSurvTime ranked and rescaled
##to be comparable to RateSurvTime; 
##note labels as well as residual (rug) plot below.
granovagg.1w(poison$RankRateSurvTime, group = poison$Group, 
ylab = "Ranked and Centered Survival Rates",
main = "One-way ANOVA display, poison data (ignoring 2-way set-up)", 
res = TRUE)
}
\author{
  Brian A. Danielak \email{brian@briandk.com}\cr Robert M.
  Pruzek \email{RMPruzek@yahoo.com}

  with contributions by:\cr William E. J. Doane
  \email{wil@drdoane.com}\cr James E. Helmreich
  \email{James.Helmreich@Marist.edu}\cr Jason Bryer
  \email{jason@bryer.org}
}
\references{
  Fundamentals of Exploratory Analysis of Variance, Hoaglin
  D., Mosteller F. and Tukey J. eds., Wiley, 1991.

  Wickham, H. (2009). Ggplot2: Elegant Graphics for Data
  Analysis. New York: Springer.

  Wilkinson, L. (1999). The Grammar of Graphics. Statistics
  and computing. New York: Springer.
}
\seealso{
  \code{\link{granovagg.contr}},
  \code{\link{granovagg.ds}}, \code{\link{granovaGG}},
  \code{\link{geom_rug_alt}}
}
\keyword{hplot}
\keyword{htest}

