\name{granovagg.ds}
\alias{granovagg.ds}
\title{Granova for Display of Dependent Sample Data}
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
  Paired X & Y values are plotted as scatterplot. The
  identity reference line (for Y=X) is drawn. Parallel
  projections of data points to (a lower-left) line segment
  show how each point relates to its X-Y = D difference;
  blue `crosses' are used to display the distribution of
  difference scores and the mean difference is displayed as
  a heavy dashed (red) line, parallel to the identity
  reference line. Means for X and Y are also plotted (as
  thin dashed vertical and horizontal lines), and rug plots
  are shown for the distributions of X (at the top of
  graphic) and Y (on the right side). Several summary
  statistics are plotted as well, to facilitate both
  description and inference; see below. The 95\% confidence
  interval for the population mean difference is also shown
  graphically.  Because all data points are plotted
  relative to the identity line, and summary results are
  shown graphically, clusters, data trends, outliers, and
  possible uses of transformations are readily seen,
  possibly to be accommodated.
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
  Robert M. Pruzek \email{RMPruzek@yahoo.com}

  Brian A. Danielak \email{brian@briandk.com}

  William E. J. Doane \email{wil@drdoane.com}

  James E. Helmreich \email{James.Helmreich@Marist.edu}

  Jason Bryer \email{jason@bryer.org}
}

