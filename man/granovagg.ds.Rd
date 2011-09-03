\name{granovagg.ds}
\alias{granovagg.ds}
\title{Elemental Graphic for Display of Dependent Sample Data}
\usage{
  granovagg.ds(data = NULL, revc = FALSE, main =
  "default_granova_title", xlab = NULL, ylab = NULL,
  conf.level = 0.95, plot.theme = "theme_granova_ds", ...)
}
\arguments{
  \item{data}{is an n X 2 dataframe or matrix. First column
  defines X (intially for horzontal axis), the second
  defines Y.}

  \item{main}{optional main title (as character); can be
  supplied by user. The default value is
  \code{"default_granova_title"}, which leads to printing
  of a generic title for graphic.}

  \item{revc}{reverses X,Y specifications}

  \item{xlab}{optional label (as character) for horizontal
  axis. If not defined, axis labels are taken from colnames
  of data.}

  \item{ylab}{optional label (as character) for vertical
  axis. If not defined, axis labels are taken from colnames
  of data.}

  \item{conf.level}{The confidence level at which to
  perform a dependent sample t-test. Defaults to
  \code{0.95} (95\% Confidence)}

  \item{plot.theme}{argument indicating a ggplot2 theme to
  apply to the graphic; defaults to a customized theme
  created for the dependent sample graphic}

  \item{...}{Optional arguments to/from other functions}
}
\value{
  Returns a plot object of class \code{ggplot}.
}
\description{
  Plots dependent sample data beginning from a scatterplot
  for the X,Y pairs; proceeds to display difference scores
  as point projections; also X and Y means, as well as the
  mean of the difference scores.
}
\details{
  Paired X and Y values are plotted as scatterplot. The
  identity reference line (for Y = X) is drawn. Parallel
  projections of data points to (a lower-left) line segment
  show how each point relates to its X-Y = D difference;
  semitransparent "shadow" points are used to display the
  distribution of difference scores, with thin grey lines
  leading from each raw datapoint to its shadow projection
  on the difference distribution. The range of that
  difference score distribution is drawn as a blue line
  beneath the shadow points and the mean difference is
  displayed as a heavy dashed purple line, parallel to the
  identity reference line. Means for X and Y are also
  plotted (as thin dashed vertical and horizontal lines),
  and rug plots are shown for the distributions of X (at
  the top of graphic) and Y (on the right side). The 95\%
  confidence interval for the population mean difference is
  also shown graphically as a green band, perpendicular to
  the mean treatment effect line. Because all data points
  are plotted relative to the identity line, and summary
  results are shown graphically, clusters, data trends,
  outliers, and possible uses of transformations are
  readily seen, possibly to be accommodated.

  In summary, the graphic shows all initial data points
  relative to the identity line, adds projections (to the
  'north' and 'east') showing the marginal distributions of
  X and Y, as well as projections to the 'southwest' where
  the difference scores for each point are drawn. Means for
  all three distributions are shown using straight lines;
  the confidence interval for the population mean
  difference score is also shown. Summary statistics are
  printed as side effects of running the function for the
  dependent sample analysis.
}
\examples{
### See discussion of anorexia graphic in EDSAG, J. Statistics Ed.
data(anorexia.sub)

granovagg.ds(anorexia.sub, revc = TRUE, 
  main = "Assessment Plot for weights to assess Family Therapy treatment 
  for Anorexia Patients")
# If labels for four unusual points at lower left are desired:
granovagg.ds(anorexia.sub, revc = TRUE, 
  main = "Assessment Plot for weights to assess Family Therapy treatment 
  for Anorexia Patients", ident = TRUE)


## See discussion of blood lead graphic in EDSAG, J. Statistics Ed.
data(blood_lead)

granovagg.ds(blood_lead, sw = .1, 
   main = "Dependent Sample Assessment Plot
   Blood Lead Levels of Matched Pairs of Children")
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
  \code{\link{granovaGG}}, \code{\link{geom_rug_alt}}
}

