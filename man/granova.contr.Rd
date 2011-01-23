\name{granova.contr}
\alias{granova.contr}
\title{Graphic Display of Contrast Effect of ANOVA}
\description{
 Provides graphic displays that shows data and effects for a priori contrasts in ANOVA contexts; also corresponding numerical results.
}
\usage{
granova.contr(data, contrasts, ylab = "Outcome (response)", 
	xlab = NULL, jj = 1)
}
\arguments{
  \item{data}{Vector of scores for all equally sized groups, or a data.fame or matrix where each column represents a group.}
  \item{contrasts}{Matrix of column contrasts with dimensions (number of groups [G]) x (number of contrasts) [generally (G x G-1)].}
  \item{ylab}{Character; y axis lable.}
  \item{xlab}{Character vector of length number of contrast columns.  To name the specific contrast being made in all but last panel of graphic.  Default = \code{NULL}}
  \item{jj}{Numeric; controls \code{\link{jitter}} and confers the possibility of 
    controlling the amount of jitter in the panel plots for the contrasts  Default is 1.}
 }
\details{
  Function provides graphic displays of contrast effects for prespecified contrasts in ANOVA. Data points are displayed
  as relevant for each contrast based on comparing groups according to the positive and negative contrast coefficients for each
  contrast on the horizontal axis, against response values on the vertical axis. Data points corresponding to groups not being
  compared in any contrast (coefficients of zero) are ignored. For each contrast (generally as part of a 2 x 2 panel) a line
  segment is given that compares the (weighted) mean of the response variable for the negative coefficients versus the positive
  coefficients. Standardized contrasts are used, wherein the sum of (magnitudes) of negative coefficients is unity; and the same
  for positive coefficients. If a line is `notably' different from horizontal (i.e. slope of zero), a `notable' effect has
  been identified; however, the question of statistical significance generally depends on a sound context-based estimate of
  standard error for the corresponding effect. This means that while summary aov numerical results and test statistics are presented
  (see below), the appropriateness of the default standard error generally requires the analyst's judgment. The response
  values are to be input in (a stacked) form, i.e. as a vector, for all cells (cf. arg. ylab).  The matrix of contrast vectors \code{contrasts}
  must have G rows (the number of groups), and a number of columns equal to the number of prespecified contrasts, at most G-1. If
  the number of columns of \code{contrasts} is G-1, then the number per group, or cell size, is taken to be \code{length(data)/G}, where \code{G = nrow(contrasts)}.

  If the number of columns of \code{contrasts} is less than G-1 then the user must stipulate \code{npg}, the number in each group or cell.
  The function is designed for the case when all cell sizes are the same, and may be most helpful when the a priori contrasts
  are mutually orthogonal (e.g., in power of 2 designs, or their fractional counterparts; also when specific row or column comparisons,
  or their interactions (see the example below based on rat weight gain data)). It is not essential that contrasts be
  mutually orthogonal; but mutual linear independence is required. (When factor levels correspond to some underlying continuum
  a standard application might use \code{con = contr.poly(G)}, for G the number of groups; consider also \code{contr.helmert(G)}.)
  The final plot in each application shows the data for all groups or cells in the design, where groups are simply numbered from 1:G, 
  for G the number of groups, on the horizontal axis, versus the response values on the vertical axis.
  }
\value{Two sets of numerical results are presented: Weighted cell means for positive and negative coefficients 
for each a priori contrast, and summary results from \code{lm}.
    \item{summary.lm}{Summary results for a linear model analysis based on the R function \code{lm} 
    (When effects are simple, as in an equal n's  power of 2 design, mean differences will generally correspond to the linear regression coefficients as seen in the \code{lm} summary results.)}
    \item{means.pos.neg.coeff}{table showing the (weighted) means for positive and negative coefficients for each (row) contrast, and for each row, the difference between these means in the final column}
    \item{means.pos.neg.coeff}{Table showing the (weighted) means for positive and negative coefficients 
    for each (row) contrast, and for each row, the difference between these means, and the standardized effect size in the final column.}
    \item{contrasts}{Contrast matrix used.}
    \item{group.means.sds}{Group means and standard deviations.}
    \item{data}{Input data in matrix form.}
}
\author{Robert M. Pruzek \email{RMPruzek@yahoo.com}

James E. Helmreich \email{James.Helmreich@Marist.edu}
}
\seealso{ \code{\link{granova.1w}}, \code{\link{granova.2w}}, \code{\link{granova.ds}} }
\examples{
data(arousal)	
contrasts22 <- data.frame( c(-.5,-.5,.5,.5), 
	c(-.5,.5,-.5,.5), c(.5,-.5,-.5,.5) )
names(contrasts22) <- c("Drug.A", "Drug.B", "Drug.A.B")
granova.contr(arousal, contrasts = contrasts22)
	
data(rat)
dat6 <- matrix(c(1, 1, 1, -1, -1, -1, -1, 1, 0, -1, 1, 0, 1, 1, -2, 
    1, 1, -2, -1, 1, 0, 1, -1, 0, 1, 1, -2, -1, -1, 2), ncol = 5)
granova.contr(rat[,1], contrasts = dat6, ylab = "Rat Weight Gain", 
  xlab = c("Amount 1 vs. Amount 2", "Type 1 vs. Type 2", 
  "Type 1 & 2 vs Type 3", "Interaction of Amount and Type 1 & 2", 
  "Interaction of Amount and  Type (1, 2), 3"))
#Polynomial Contrasts 
granova.contr(rat[,1],contrasts = contr.poly(6))

#based on random data 
data.random <- rt(64, 5)
granova.contr(data.random, contrasts = contr.helmert(8), 
	ylab = "Random Data")
}
\keyword{hplot}

