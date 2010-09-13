\name{granova.contr}
\alias{granova.contr}
\title{Graphic Display of Contrast Effect of ANOVA}
\description{
 Provides graphic displays that shows data and effects for a priori contrasts in ANOVA contexts; also corresponding numerical results.
}
\usage{
granova.contr(resp, con, ngrp = nrow(con), 
     npg = length(resp)/ngrp, jj = 1)
}
\arguments{
  \item{resp}{Vector of scores for all equally sized groups.}
  \item{con}{Matrix of column contrasts with dimensions (number of groups [G]) x (number of contrasts) [generally G X G-1].}
  \item{ngrp}{Numeric; default = \code{nrows(resp)}.  The number of rows = number `cells' or groups [G].}
  \item{npg}{Numeric; default is \code{length(resp)/ngrp}.  If the number of contrasts is NOT equal to the number
    of groups less one (i.e., to df `among'), then \code{npg} must be set equal to the number in each group
    (presumed equal for all groups).  The default \code{npg} setting assumes \code{con} has one fewer column than
    the number of rows.}
  \item{jj}{Numeric; controls \code{\link{jitter}} and confers the possibility of 
    controlling the amount of jitter in the panel plots for the contrasts  Default is 1.}
 }
\details{
  Function provides graphic displays of contrast effects for so-called a priori contrasts in anova. Data points are displayed
  as relevant for each contrast based on comparing groups according to the positive and negative contrast coefficients for each
  contrast on the horizontal axis, against response values on the vertical axis. Data points corresponding to groups not being
  compared in any contrast (coefficients of zero) are ignored. For each contrast (generally as part of a 2 x 2 panel) a line
  segment is given that compares the (weighted) mean of the response variable for the negative coefficients versus the positive
  coefficients. Standardized contrasts are used, wherein the sum of (magnitudes) of negative coefficients is unity; and the same
  for positive coefficients. If a line is `notably' different from horizontal (i.e. slope of zero), a `notable' effect has
  been identified; however, the question of statistical significance generally depends on a sound context-based estimate of
  standard error for the corresponding effect. This means that while summary aov numerical results and test statistics are presented
  (see below), the appropriateness of the default standard error generally requires the analyst's judgment. The response
  values are to be input in (a stacked) form, i.e. as a vector, for all cells (cf. arg. resp).  The matrix of contrast vectors \code{con}
  should have G rows (the number of groups), and a number of columns equal to the number of a priori contrasts, at most G-1. If
  the number of columns of \code{con} is G-1, then the number per group, or cell size, is taken to be \code{length(resp)/G}, where \code{G = nrow(con)}.

  If the number of columns of \code{con} is less than G-1 then the user must stipulate \code{npg}, the number in each group or cell.
  The function is designed for the case when all cell sizes are the same, and may be most helpful when the a priori contrasts
  are mutually orthogonal (e.g., in power of 2 designs, or their fractional counterparts; also when specific row or column comparisons,
  or their interactions (see the example below based on rat weight gain data)). It is not essential that contrasts be
  mutually orthogonal; but mutual linear independence is required. (When factor levels correspond to some underlying continuum
  a standard application might use \code{con = contr.poly(G)}, for G the number of groups; consider also \code{contr.helmert(G)}.)
  The final plot in each application shows the data for all groups or cells in the design, where groups are simply numbered from 1:G, for G the number of groups, on the horizontal axis, versus the response values on the vertical axis.
  }
\value{Two sets of numerical results are presented: Weighted cell means for positive and negative coefficients 
for each a priori contrast, and summary results from \code{lm}.
    \item{summary.lm}{Summary results for a linear model analysis based on the R function \code{lm} 
    (When effects are simple, as in an equal n's  power of 2 design, mean differences will generally correspond to the linear regression coefficients as seen in the \code{lm} summary results.)}       \item{means.pos.neg.coeff}{table showing the (weighted) means for positive and negative coefficients for each (row) contrast, and for each row, the difference between these means in the final column}
    \item{means.pos.neg.coeff}{Table showing the (weighted) means for positive and negative coefficients 
    for each (row) contrast, and for each row, the difference between these means, and the standardized effect size in the final column.}
    \item{contrasts}{Contrast matrix used.}
    \item{group.means.sds}{Group means and standard deviations.}
    \item{data}{Input data in matrix form.}
}
\author{Robert M. Pruzek \email{RMPruzek@yahoo.com}

James E. Helmreich \email{ James.Helmreich@Marist.edu}
}
\seealso{ \code{\link{granova.1w}}, \code{\link{granova.2w}}, \code{\link{granova.ds}} }
\examples{
data(rat)
dat6 <- matrix(c(1, 1, 1, -1, -1, -1, -1, 1, 0, -1, 1, 0, 1, 1, -2, 
    1, 1, -2, -1, 1, 0, 1, -1, 0, 1, 1, -2, -1, -1, 2), ncol = 5)
granova.contr(rat[,1], con = dat6)

#Polynomial Contrasts 
granova.contr(rat[,1],con = contr.poly(6))

#based on random data (not run)
resp <- rt(64, 5)
granova.contr(resp, con = contr.helmert(8))
}
\keyword{hplot}

