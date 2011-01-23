\name{granova.ds}
\alias{granova.ds}
\title{Granova for Display of Dependent Sample Data}
\description{Plots dependent sample data beginning from a scatterplot for the X,Y pairs; proceeds to 
    display difference scores as point projections; also X and Y means, as well as the mean of the 
    difference scores. Also prints various summary statistics including: effect size, means for X and Y, 
    a 95\% confidence interval for the mean difference as well as the t-statistic and degrees of freedom.
}
\usage{
granova.ds(data, revc = FALSE, sw = 0.4, ne = 0.5, ptpch=c(19,3), 
        ptcex=c(.8,1.4), labcex = 1, ident = FALSE, 
        colors = c(1,2,1,4,2,'green3'), pt.lab = NULL,
        xlab = NULL, ylab = NULL, main = NULL, sub = NULL, 
        par.orig = TRUE)
}
\arguments{
  \item{data}{is an n X 2 dataframe or matrix. First column defines X (intially for horzontal axis), the second defines Y. }
  \item{revc}{reverses X,Y specifications.}
  \item{sw}{extends axes toward lower left, effectively moving data points to the southwest.}
  \item{ne}{extends axes toward upper right, effectively moving data points to northeast. Making both sw and ne smaller moves points farther
    apart, while making both larger moves data points closer together.}
  \item{ptpch}{controls the pch of the (X,Y) points and of differences score points.} 
  \item{ptcex}{controls the cex of the (X,Y) points and of differences score points.}
  \item{labcex}{controls size of axes labels.}
  \item{ident}{logical, default FALSE. Allows user to identify individual points.}
  \item{colors}{vector defining colors of six components of the plot: (X,Y) points, horizontal and vertical dashed lines representing 
                means of the two groups, light dashed diagonal lines connecting (X,Y) points and projections differences dotplot,  
                differences arranged as a dotplot, heavy dashed diagonal line representing the mean of differences, 
                confidence interval. }
  \item{pt.lab}{optional character vector defining labels for points.  Only used if ident is TRUE.
                If NULL, rownames(data) are used if available; if not 1:n is used.}
  \item{xlab}{optional label (as character) for horizontal axis. If not defined, axis labels are taken from colnames of data.}
  \item{ylab}{optional label (as character) for vertical axis.}
  \item{main}{optional main title (as character); if not supplied by user generic title is provided.}
  \item{sub}{optional subtile (as character).}
  \item{par.orig}{returns par to original settings; if multipanel plots it is advisable to specify FALSE.}
}
\details{Paired X & Y values are plotted as scatterplot. The identity reference line (for Y=X) is drawn. Since the better data view often entails
 having X's > Y's the revc argument facilitates reversal of the X, Y specifications.  Parallel projections of data points to (a lower-left) line 
 segment show how each point relates to its X-Y = D difference; blue
 `crosses' are used to display the distribution of difference scores and the mean difference is displayed as a heavy dashed (red) line, parallel to
 the identity reference line. Means for X and Y are also plotted (as thin dashed vertical and horizontal lines), and rug plots are shown for the
 distributions of X (at the top of graphic) and Y (on the right side). Several summary statistics are plotted as well, to facilitate both description
 and inference; see below. The 95\% confidence interval for the population mean difference is also shown graphically.  Because all data points are plotted relative to the identity
 line, and summary results are shown graphically, clusters, data trends, outliers, and possible uses of transformations are readily seen, possibly
 to be accommodated.
  }
\value{A list is returned with the following components:
  \item{mean(X)}{Mean of X values}
   \item{mean(Y)}{Mean of Y values}
    \item{mean(D=X-Y)}{Mean of differences D = X - Y}
     \item{SD(D)}{Standard deviation of differences D}
      \item{ES(D)}{Effect Size for differences D: mean(D)/SD(D)}
       \item{r(X,Y)}{Correlation based on X,Y pairs}
        \item{r(x+y,D)}{Correlation based on X+Y,D pairs}
         \item{LL 95\%CI}{Lower bound for 95\% confidence interval for population mean(D)}
          \item{UL 95\%CI}{Upper bound for 95\% confidence interval for population mean(D)}
           \item{t(D-bar)}{t-statistic associated w/ test of hypothesis that population mean(D) = 0.0}
            \item{df.t}{Degrees of freedom for the t-statistic}
             \item{pval.t}{P-value for two sided t-test of null hypothesis that population mean(D) does not equal zero.}
}
\references{
Exploratory Plots for Paired Data, Rosenbaum P., The American Statistician, May 1989, vol. 43, no. 2, pp. 108-9.
	
Enhancing Dependent Sample Analyses with Graphics, Pruzek, R. and Helmreich, J., Journal of Statistics Education, March 2009, Vol. 17, no. 1.
  
http://www.amstat.org/publications/jse/v17n1/helmreich.pdf
}
\author{Robert M. Pruzek \email{RMPruzek@yahoo.com}

James E. Helmreich \email{James.Helmreich@Marist.edu}
 }

\examples{

### See discussion of anorexia graphic in EDSAG, J. Statistics Ed.
data(anorexia.sub)

granova.ds(anorexia.sub, revc = TRUE, 
	main = "Assessment Plot for weights to assess Family Therapy treatment 
	for Anorexia Patients")
# If labels for four unusual points at lower left are desired:
granova.ds(anorexia.sub, revc = TRUE, 
	main = "Assessment Plot for weights to assess Family Therapy treatment 
	for Anorexia Patients", ident = TRUE)


## See discussion of blood lead graphic in EDSAG, J. Statistics Ed.
data(blood_lead)

granova.ds(blood_lead, sw = .1, 
   main = "Dependent Sample Assessment Plot
   Blood Lead Levels of Matched Pairs of Children")
}
\keyword{hplot}
\keyword{htest}
