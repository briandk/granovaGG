\name{granovagg.1w}
\alias{granovagg.1w}
\title{Graphic display for one-way ANOVA}
\usage{
  granovagg.1w(data, group = NULL, h.rng = 1.25, v.rng =
  0.2, jj = NULL, resid = FALSE, xlab = NULL, ylab = NULL,
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
  groups, default = 1.25}

  \item{v.rng}{Numeric; controls the vertical spread of
  points, default = 0.25.}

  \item{jj}{Numeric; sets horiz. jittering level of points;
  when pairs of ordered means are close to one another, try
  jj < 1.}

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

  \item{...}{Optional arguments to be passed to
  \code{ggplot}}
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
  variance, and also to help understand how ANOVA works,
  how the F statistic is generated for the data in hand,
  etc. The graphic may be called 'elemental' or 'natural'
  because it is built upon the key question that drives
  one-way ANOVA.
}
\details{
  The central idea of the graphic is to use the fact that a
  one way analysis of variance F statistic is the ratio of
  two variances each of which can usefully be presented
  graphically. In particular, the sum of squares between
  (among) can be represented as the sum of products of
  so-called effects (each being a group mean minus the
  grand mean) and the group means; when these effects are
  themselves plotted against the group means a straight
  line necessarily ensues. The group means are plotted as
  triangles along this line. Data points (jittered) for
  groups are displayed (vertical axis) with respect to
  respective group means.  One-way ANOVA residuals can be
  displayed (set resid=TRUE) as a rug plot (on right
  margin); the standard deviation of the residuals, when
  squared, is just the mean square within. The conventional
  F statistic is just a ratio of the between to the within
  mean squares, or variances, each of which corresponds to
  areas of squares in the graphic. Use of effects to locate
  the groups in the order of the observed means, from left
  to right (by increasing size) yields this 'elemental'
  graphic for this commonly used statistical method.

  Groups need not be of the same sizes, nor do data need to
  reflect any particular distributional characteristics.
  Skewness, outliers, clustering of data points, and
  various other features of the data may be seen in this
  graphic, possibly identified using point labels.
}
\examples{
NULL
}
\author{
  Robert M. Pruzek \email{RMPruzek@yahoo.com}

  Brian A. Danielak \email{brian@briandk.com}

  William E. J. Doane \email{wil@drdoane.com}

  James E. Helmreich \email{James.Helmreich@Marist.edu}

  Jason Bryer \email{jason@bryer.org}
}
\references{
  Fundamentals of Exploratory Analysis of Variance, Hoaglin
  D., Mosteller F. and Tukey J. eds., Wiley, 1991.
}
\seealso{
  \code{\link{granovagg.contr}}, \code{\link{granovagg.ds}}
}
\keyword{hplot}
\keyword{htest}

