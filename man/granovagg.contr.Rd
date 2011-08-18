\name{granovagg.contr}
\alias{granovagg.contr}
\title{Graphic Display of Contrast Effect of ANOVA}
\usage{
  granovagg.contr(data, contrasts, ylab = "Outcome
  (response)", xlab = NULL, plot.theme =
  "theme_granova_contr", print.four.plots.per.page = TRUE,
  ...)
}
\arguments{
  \item{data}{Vector of scores for all equally sized
  groups, or a data.fame or matrix where each column
  represents a group.}

  \item{contrasts}{Matrix of column contrasts with
  dimensions (number of groups [G]) x (number of contrasts)
  [generally (G x G-1)].}

  \item{ylab}{Character; y axis lable.}

  \item{xlab}{Character vector of length number of contrast
  columns.  To name the specific contrast being made in all
  but last panel of graphic. Default = \code{NULL}}
}
\value{
  If \code{print.four.plots.per.page} is set to
  \code{FALSE}, the function returns a list of ggplot
  objects, one element per plot. That allows you to index
  any individual plot or plots, then modify them as you
  wish. When \code{print.four.plots.per.page} is set to
  \code{FALSE} (the default), the function prints four
  plots per page on a graphical device but returns
  \code{NULL}.

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
NULL
}
\seealso{
  \code{\link{granovagg.1w}}, \code{\link{granovagg.ds}}
}
\keyword{hplot}

