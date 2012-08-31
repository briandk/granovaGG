\name{granovagg.ds}
\alias{granovagg.ds}
\title{Elemental Graphic for Display of Dependent Sample Data}
\usage{
  granovagg.ds(data = NULL, revc = FALSE,
    main = "default_granova_title", xlab = NULL,
    ylab = NULL, conf.level = 0.95,
    plot.theme = "theme_granova_ds", northeast.padding = 0,
    southwest.padding = 0, ...)
}
\arguments{
  \item{data}{is an n X 2 dataframe or matrix. First column
  defines X (intially for horzontal axis), the second
  defines Y.}

  \item{main}{optional main title (as character); can be
  supplied by user. The default value is
  \code{"default_granova_title"}, which will print a
  generic title for the graphic.}

  \item{revc}{reverses X,Y specifications}

  \item{xlab}{optional label (as character) for horizontal
  axis. If not defined, axis labels are taken from colnames
  of data.}

  \item{ylab}{optional label (as character) for vertical
  axis. If not defined, axis labels are taken from colnames
  of data.}

  \item{conf.level}{The confidence level at which to
  perform a dependent sample t-test.  Defaults to
  \code{0.95} (95\% Confidence)}

  \item{plot.theme}{argument indicating a ggplot2 theme to
  apply to the graphic; defaults to a customized theme
  created for the dependent sample graphic}

  \item{northeast.padding}{(numeric) extends axes toward
  lower left, effectively moving data points to the
  southwest. Defaults to zero padding.}

  \item{southwest.padding}{(numeric) extends axes toward
  upper right, effectively moving data points to the
  southwest. Defaults to zero padding.  Making both
  southwest and northeast padding smaller moves points
  farther apart, while making both larger moves data points
  closer together.}

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
### Using granovagg.ds to examine trends or effects for repeated measures data.

# This example corresponds to case 1b in Pruzek and Helmreich (2009). In this
# graphic we're looking for the effect of treatment on patients with anorexia.

data(anorexia.sub)
granovagg.ds(anorexia.sub, 
             revc = TRUE, 
             main = "Assessment Plot for weights to assess Family Therapy treatment for Anorexia Patients",
             xlab = "Weight after therapy (lbs.)",
             ylab = "Weight before therapy (lbs.)"
)

### Using granovagg.ds to compare two experimental treatments (with blocking)

# This example corresponds to case 2a in Pruzek and Helmreich (2009). For this
# data, we're comparing the effects of two different virus preparations on the
# number of lesions produced on a tobacco leaf.

data(tobacco)
granovagg.ds(tobacco[, c("prep1", "prep2")], 
             main = "Local Lesions on Tobacco Leaves",
             xlab = "Virus Preparation 1",
             ylab = "Virus Preparation 2"
)

### Using granovagg.ds to compare two experimental treatments (with blocking)

# This example corresponds to case 2a in Pruzek and Helmreich (2009). For this
# data, we're comparing the wear resistance of two different shoe sole
# materials, each randomly assigned to the feet of 10 boys.

library(MASS) # Contains the shoes dataset
shoes <- as.data.frame(shoes)
granovagg.ds(shoes,
             revc = TRUE,
             main = "Shoe Wear",
             xlab = "Sole Material B",
             ylab = "Sole Material A",
)

### Using granovagg.ds to compare matched individuals for two treatments

# This example corresponds to case 2b in Pruzek and Helmreich (2009). For this
# data, we're examining the level of lead (in mg/dl) present in the blood of
# children. Children of parents who had worked in a factory where lead was used
# in making batteries were matched by age, exposure to traffic, and neighborhood
# with children whose parents did not work in lead-related industries.

data(blood_lead)
granovagg.ds(blood_lead, 
             sw = .1, 
             main = "Dependent Sample Assessment Plot
             Blood Lead Levels of Matched Pairs of Children",
             xlab = "Exposed (mg/dl)",
             ylab = "Control (mg/dl)"
)
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
  Pruzek, R. M., & Helmreich, J. E. (2009). Enhancing
  Dependent Sample Analyses with Graphics. Journal of
  Statistics Education, 17(1), 21.

  Wickham, H. (2009). Ggplot2: Elegant Graphics for Data
  Analysis. New York: Springer.

  Wilkinson, L. (1999). The Grammar of Graphics. Statistics
  and computing. New York: Springer.
}
\seealso{
  \code{\link{granovagg.1w}}, \code{\link{granovagg.ds}},
  \code{\link{granovaGG}}
}

