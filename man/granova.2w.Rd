\name{granova.2w}
\alias{granova.2w}
\title{Graphical display of data for two-way analysis of variance}
\description{
  Produces a rotatable graphic (controlled by the mouse) to display all data points for any two way analysis of variance.
}
\usage{
granova.2w(formula = NULL, data.A.B, fit = "linear", ident = FALSE, 
       offset = NULL, ...)
}
\arguments{
  \item{data.A.B}{An N X 3 dataframe. (If it is a matrix, it will be converted to a df.) Column 1 must contain response values or
    scores for all groups, N in all; columns 2 and three must be vectors with integers showing levels of factors A and B, respectively.
    If rows are named uniquely, then for ident= TRUE, points can be identified with those labels, otherwise the row number of data.A.B is used.     Note that factor levels will (generally) be reordered.}
  \item{formula}{Optional formula used by \code{\link{aov}} to produce the summary 2-way ANOVA table provided as output. 
    Not used in the scatterplot.}
  \item{fit}{Defines whether the fitted surface will be 'linear' (default) or some more complicated surface, e.g., quadratic, or smooth; see below.}
  \item{ident}{Logical, if TRUE allows interactive identification of individual points using rownames of data.A.B on graphic. If rownames are not provided then 1:N is used.}
  \item{offset}{Number; if \code{NULL} then default for \code{identify3d} is used.}
  \item{...}{Optional arguments to be passed to \code{scatter3d}.}
}
\details{
    Function depicts data points graphically in a window using the row by column set-up for a two-way ANOVA; 
    the graphic is rotatable, controlled by the mouse. Data-based contrasts (cf. description for one-way ANOVA: 
    \code{\link{granova.1w}}) are used to ensure a flat surface -- corresponding to an additive fit 
    (if fit=`linear'; see below) -- for all cells. Points are displayed `vertically' 
    (initially) with respect to the fitting surface. In particular, (dark blue) spheres are used to show 
    data points for all groups. The mean for each cell is shown as a white sphere. The graphic is based on 
    \code{rgl} and \code{scatter3d}; the graphic display can be zoomed in and out by scrolling, where the mouse 
    is used to rotate the entire figure in a 3d representation where the row and column (factor A and B) effects 
    have been used for spacing of the cells on the margins of the fitting surface. As noted, the first column of 
    the input data frame must be response values (scores); the second and third columns should be integers that 
    identify levels of the A and B factors respectively. Based on the row and column means, factor levels are 
    first ordered (from small to large) separately for the row and column means; levels are assumed not to be 
    ordered at the outset. Function \code{scatter3d} is used from \code{Rcmdr} (thanks, John Fox).  
    The \code{fit} is defaulted to \code{linear} whence 
    interactions are depicted as departures of the cell means from a flat surface. It is possible to replace 
    \code{linear} with any of \code{quadratic}, \code{smooth}, or \code{additive}; see help for \code{scatter3d} 
    for details. The table of counts for the cell means is printed (with respect the the reordered rows and columns); 
    similarly, the table of cell means is printed (also, based on reordered rows and columns). Finally, numerical 
    summary results derived from function \code{\link{aov}} are also printed. Although the function accommodates 
    the case where cell counts are not all the same, or when the data are unbalanced with respect to the A & B factors, 
    the surface can be misleading, especially in highly unbalanced data.  Machine memory for this function has caused 
    problems with some larger data sets.  The authors would appreciate reports of problems or successes with 
    larger data sets.
}

\note{Right click on the graphic to terminate \code{identify} and return the output from the function.}

\value{Returns a list with four components:
  \item{A.effects}{Reordered factor A effects (deviations of A-level means from grand mean)}
  \item{B.effects}{Reordered factor B effects (deviations of B-level means from grand mean)}
  \item{CellCounts.Reordered}{Cell sizes for all A-level, B-level combinations, with rows/columns reordered according to A.effects and B.effects.}
  \item{CellMeans.Reordered}{Means for all cells, i.e., A-level, B-level combinations, with rows/columns reordered according to A.effects and B.effects}
  \item{anova.summary}{Summary \code{aov} results, based on input data}

}
\references{Fundamentals of Exploratory Analysis of Variance, Hoaglin D., Mosteller F. and Tukey J. eds., Wiley, 1991.}
\author{Robert M. Pruzek \email{RMPruzek@yahoo.com}

James E. Helmreich \email{ James.Helmreich@Marist.edu}
}
\seealso{ \code{\link{granova.1w}}, \code{\link{granova.contr}}, \code{\link{granova.ds}}}
\examples{
#Random data
resp <- rnorm(80,0,.25) + rep(c(0,.2,.4,.6), ea = 20)
f1 <- rep(1:4, ea = 20)
f2 <- rep(rep(1:5, ea = 4), 4)
rdat1 <- cbind(resp, f1, f2)
granova.2w(data.A.B = rdat1)
#
rdat2 <- cbind(rnorm(64,10,2), sample(1:4, 64, repl = TRUE), 
   sample(1:3, 64, repl = TRUE))
granova.2w(data.A.B = rdat2)
#
granova.2w(formula = breaks ~ wool * tension, data.A.B = warpbreaks)
}
\keyword{hplot}

