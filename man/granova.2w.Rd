\name{granova.2w}
\alias{granova.2w}
\title{Graphical display of data for two-way analysis of variance}
\description{
  Produces a rotatable graphic (controlled by the mouse) to display all data points for any two way analysis of variance.
}
\usage{
granova.2w(data, formula = NULL, fit = "linear", ident = FALSE, 
       offset = NULL, ...)
}
\arguments{
  \item{data}{An N x 3 dataframe. (If it is a matrix, it will be converted to a dataframe.) Column 1 must contain response values or
    scores for all groups, N in all; columns 2 and 3 should be factors (or will be coerced to factors) showing levels of the two treatments.
    If rows are named, then for \code{ident= TRUE}, points can be identified with those labels, otherwise the row number of \code{data} is used.
    Note that factor levels will (generally) be reordered.}
  \item{formula}{Optional formula used by \code{\link{aov}} to produce the summary 2-way ANOVA table provided as output. 
    Not used in the scatterplot.}
  \item{fit}{Defines whether the fitted surface will be \code{linear} (default) or some more complicated surface, e.g., \code{quadratic}, or \code{smooth}; see below.}
  \item{ident}{Logical, if TRUE allows interactive identification of individual points using rownames of \code{data} on graphic. If rownames are not provided then 1:N is used. Click and hold right mouse button while dragging over point.  Right click white space to end.}
  \item{offset}{Number; if \code{NULL} then default for \code{identify3d} is used.}
  \item{...}{Optional arguments to be passed to \code{scatter3d}.}
}
\details{
    The function depicts data points graphically in a window using the row by column set-up for a two-way ANOVA; 
    the graphic is rotatable, controlled by the mouse. Data-based contrasts (cf. description for one-way ANOVA: 
    \code{\link{granova.1w}}) are used to ensure a flat surface -- corresponding to an additive fit 
    (if \code{fit = linear}; see below) -- for all cells. Points are displayed vertically 
    (initially) with respect to the fitting surface. In particular, (dark blue) spheres are used to show 
    data points for all groups. The mean for each cell is shown as a white sphere. The graphic is based on 
    \code{rgl} and \code{scatter3d}; the graphic display can be zoomed in and out by scrolling, where the mouse 
    is used to rotate the entire figure in a 3d representation.  The row and column (factor A and B) effects 
    have been used for spacing of the cells on the margins of the fitting surface. As noted, the first column of 
    the input data frame must be response values (scores); the second and third columns should be integers that 
    identify levels of the A and B factors respectively. Based on the row and column means, factor levels are 
    first ordered (from small to large) separately for the row and column means; levels are assumed not to be 
    ordered at the outset. 
    
    The function \code{scatter3d} is used from \code{car} (thanks, John Fox).  
    The value of \code{fit} is passed to \code{scatter3d} and determines the surface fit to the data.  The default value of \code{fit} is \code{linear}, so that  
    interactions may be seen as departures of the cell means from a flat surface. It is possible to replace 
    \code{linear} with any of \code{quadratic}, \code{smooth}, or \code{additive}; see help for \code{scatter3d} 
    for details.  Note in particular that a \code{formula} specified by the user (or the default) has no direct effect on the graphic, but is reflected in the console output.
    
    For data sets above about 300 or 400 points, the default sphere size (set by \code{sphere.size}) can be quite small.  The optional 
    argument \code{sphere.size = 2} or a similar value will increase the size of the spheres.  However, the sphere
    sizes possible are discrete.
    
    The table of counts for the cell means is printed (with respect the the reordered rows and columns); 
    similarly, the table of cell means is printed (also, based on reordered rows and columns). Finally, numerical 
    summary results derived from function \code{\link{aov}} are also printed. Although the function accommodates 
    the case where cell counts are not all the same, or when the data are unbalanced with respect to the A & B factors, 
    the surface can be misleading, especially in highly unbalanced data.  Machine memory for this function has caused 
    problems with some larger data sets.  The authors would appreciate reports of problems or successes with 
    larger data sets.
}

\note{Right click on the graphic to terminate \code{identify} and return the output from the function.}

\value{Returns a list with four components:
  \item{A.effects}{Reordered factor A (second column of \code{data}) effects (deviations of A-level means from grand mean)}
  \item{B.effects}{Reordered factor B (third column of \code{data}) effects (deviations of B-level means from grand mean)}
  \item{CellCounts.Reordered}{Cell sizes for all A-level, B-level combinations, with rows/columns reordered according to A.effects and B.effects.}
  \item{CellMeans.Reordered}{Means for all cells, i.e., A-level, B-level combinations, with rows/columns reordered according to A.effects and B.effects}
  \item{anova.summary}{Summary \code{aov} results, based on input data}

}
\references{Fundamentals of Exploratory Analysis of Variance, Hoaglin D., Mosteller F. and Tukey J. eds., Wiley, 1991.}
\author{Robert M. Pruzek \email{RMPruzek@yahoo.com}

James E. Helmreich \email{James.Helmreich@Marist.edu}
}
\seealso{ \code{\link{granova.1w}}, \code{\link{granova.contr}}, \code{\link{granova.ds}}}
\examples{
 
# using the R dataset warpbreaks; see documentation 
#(first surface flat since fit = 'linear' (default); 
#second surface shows curvature)
granova.2w(warpbreaks)
granova.2w(warpbreaks, formula = breaks ~ wool + tension)
granova.2w(warpbreaks, formula = breaks ~ wool + tension, 
fit = 'quadratic')

# Randomly generated data
resp <- rnorm(80, 0, .25) + rep(c(0, .2, .4, .6), ea = 20)
f1 <- rep(1:4, ea = 20)
f2 <- rep(rep(1:5, ea = 4), 4)
rdat1 <- cbind(resp, f1, f2)
granova.2w(rdat1)
#
rdat2 <- cbind(rnorm(64, 10, 2), sample(1:4, 64, repl = TRUE), 
   sample(1:3, 64, repl = TRUE))
granova.2w(rdat2)
#
#
data(poison)
#Raw Survival Time as outcome measure:
granova.2w(poison[, c(4, 1, 2)])
# Now with quadratic surface (helpful for this poor metric):
granova.2w(poison[, c(4, 1, 2)], fit = 'quadratic') 
#
#Inverse of Survival Time as outcome measure 
#(actually rate of survival, a better version of response, clearly):
granova.2w(poison[, c(5, 1, 2)])
#Now curvature is minimal (confirming adequacy of 
#linear model fit for this metric):
granova.2w(poison[, c(5, 1, 2)], fit = 'quadratic') 
#
#Ranked Version of Inverse:
granova.2w(poison[, c(6, 1, 2)])
}
\keyword{hplot}

