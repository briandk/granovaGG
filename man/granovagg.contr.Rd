\name{granovagg.contr}
\alias{granovagg.contr}
\title{Elemental Graphic Display for Contrast Effect of ANOVA}
\usage{
  granovagg.contr(data, contrasts,
    ylab = "default_y_label",
    plot.theme = "theme_granova_contr",
    print.four.plots.per.page = TRUE, jj = 1, ...)
}
\arguments{
  \item{data}{Vector of scores for all equally sized
  groups, or a data.fame or matrix where each column
  represents a group.}

  \item{contrasts}{Matrix of column contrasts with
  dimensions (number of groups [G]) x (number of contrasts)
  [generally (G x G-1)].}

  \item{ylab}{Character; y axis label. Defaults to a
  generic granova title.}

  \item{plot.theme}{argument indicating a ggplot2 theme to
  apply to the graphic; defaults to a customized theme
  created for the contrast graphic}

  \item{print.four.plots.per.page}{If \code{TRUE}, the
  function lays out four plots per page and sends each page
  to the graphics device. When running R interactively,
  you'll have an opportunity to review each page before
  seeing the next page. Also, when
  \code{print.four.plots.per.page} is \code{TRUE}, the
  function won't return any plot objects as output. When
  \code{print.four.plots.per.page} is set to \code{FALSE},
  the function returns a list of ggplot objects, one
  element per plot.}

  \item{jj}{Numeric; controls \code{\link{jitter}} and
  allows you to control the degree of jitter in the
  contrast plots. \code{jj} is divided by 100 and passed as
  the \code{amount} parameter to \code{\link{jitter}}.}

  \item{...}{Optional arguments to/from other functions.}
}
\value{
  If \code{print.four.plots.per.page} is set to
  \code{FALSE}, the function returns a list of ggplot
  objects, one element per plot. That allows you to access
  any individual plot or plots, then modify them as you
  wish (with ggplot2 commands, for example).  When
  \code{print.four.plots.per.page} is set to \code{TRUE}
  (the default), the function prints four plots per page on
  a graphical device but returns \code{NULL}.

  The function also provides printed output: \item{Weighted
  Means}{Table showing the (weighted) means for positive
  and negative coefficients for each (row) contrast, and
  for each row, the difference between these means, and the
  standardized effect size in the final column.}
  \item{summary.lm}{Summary results for a linear model
  analysis based on the R function \code{lm} (When effects
  are simple, as in an equal n's power of 2 design, mean
  differences will generally correspond to the linear
  regression coefficients as seen in the \code{lm} summary
  results.)} \item{Contrasts}{The contrast matrix you
  specified.}
}
\description{
  Provides graphic displays that shows data and effects for
  a priori contrasts in ANOVA contexts; also corresponding
  numerical results.
}
\details{
  Function provides graphic displays of contrast effects
  for prespecified contrasts in ANOVA. Data points are
  displayed as relevant for each contrast based on
  comparing groups according to the positive and negative
  contrast coefficients for each contrast on the horizontal
  axis, against response values on the vertical axis. Data
  points corresponding to groups not being compared in any
  contrast (coefficients of zero) are ignored. For each
  contrast (generally as part of a 2 x 2 panel) a line
  segment is given that compares the (weighted) mean of the
  response variable for the negative coefficients versus
  the positive coefficients. Standardized contrasts are
  used, wherein the sum of (magnitudes) of negative
  coefficients is unity; and the same for positive
  coefficients. If a line is `notably' different from
  horizontal (i.e. slope of zero), a `notable' effect has
  been identified; however, the question of statistical
  significance generally depends on a sound context-based
  estimate of standard error for the corresponding effect.
  This means that while summary aov numerical results and
  test statistics are presented (see below), the
  appropriateness of the default standard error generally
  requires the analyst's judgment. The response values are
  to be input in (a stacked) form, i.e. as a vector, for
  all cells (cf. arg. ylab). The matrix of contrast vectors
  \code{contrasts} must have G rows (the number of groups),
  and a number of columns equal to the number of
  prespecified contrasts, at most G-1. If the number of
  columns of \code{contrasts} is G-1, then the number per
  group, or cell size, is taken to be
  \code{length(data)/G}, where \code{G = nrow(contrasts)}.

  If the number of columns of \code{contrasts} is less than
  G-1 then the user must stipulate \code{npg}, the number
  in each group or cell.  The function is designed for the
  case when all cell sizes are the same, and may be most
  helpful when the a priori contrasts are mutually
  orthogonal (e.g., in power of 2 designs, or their
  fractional counterparts; also when specific row or column
  comparisons, or their interactions (see the example below
  based on rat weight gain data)). It is not essential that
  contrasts be mutually orthogonal; but mutual linear
  independence is required. (When factor levels correspond
  to some underlying continuum a standard application might
  use \code{con = contr.poly(G)}, for G the number of
  groups; consider also \code{contr.helmert(G)}.)  The
  final plot in each application shows the data for all
  groups or cells in the design, where groups are simply
  numbered from 1:G, for G the number of groups, on the
  horizontal axis, versus the response values on the
  vertical axis.
}
\examples{
data(arousal)  
contrasts22 <- data.frame( c(-.5,-.5,.5,.5), 
  c(-.5,.5,-.5,.5), c(.5,-.5,-.5,.5) )
names(contrasts22) <- c("Drug.A", "Drug.B", "Drug.A.B")
granovagg.contr(arousal, contrasts = contrasts22)
  
data(rat)
dat6 <- matrix(c(1, 1, 1, -1, -1, -1, -1, 1, 0, -1, 1, 0, 1, 1, -2, 
    1, 1, -2, -1, 1, 0, 1, -1, 0, 1, 1, -2, -1, -1, 2), ncol = 5)
granovagg.contr(rat[,1], contrasts = dat6, ylab = "Rat Weight Gain", 
  xlab = c("Amount 1 vs. Amount 2", "Type 1 vs. Type 2", 
  "Type 1 & 2 vs Type 3", "Interaction of Amount and Type 1 & 2", 
  "Interaction of Amount and  Type (1, 2), 3"))
#Polynomial Contrasts 
granovagg.contr(rat[,1],contrasts = contr.poly(6))

#based on random data 
data.random <- rt(64, 5)
granovagg.contr(data.random, contrasts = contr.helmert(8), 
  ylab = "Random Data")
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
  Wickham, H. (2009). Ggplot2: Elegant Graphics for Data
  Analysis. New York: Springer.

  Wilkinson, L. (1999). The Grammar of Graphics. Statistics
  and computing. New York: Springer.
}
\seealso{
  \code{\link{granovagg.1w}}, \code{\link{granovagg.ds}},
  \code{\link{granovaGG}}
}
\keyword{hplot}

