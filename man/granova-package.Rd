\name{granova-package}
\alias{granova-package}
\alias{granova}
\docType{package}
\title{
Graphical Analysis of Variance
}
\description{
This small collection of functions provides distinctive graphics for display of anova
    results. The two principal functions are granova.1w (a graphic for one way anova) and
    granova.2w (a corresponding graphic for two way anova). These functions were written to display
    data for any number of groups, regardless of their sizes (however, very large data sets or numbers of
    groups can be problematic). For these two functions a specialized approach is used
    to construct data-based contrast vectors with respect to which anova data are displayed.
    The result is that the graphics use straight lines, and (when possible) flat surfaces, to help ensure clear
    interpretations while being faithful to the standard effect tests in anova; the graphic
    results are complementary to standard summary tables for these two basic kinds of analysis of variance.
    Two additional functions are granova.ds (for comparing two dependent samples of data), and granova.contr
    (which provides graphic displays for a priori contrasts). All functions provide relevant
    numerical results to supplement the graphic displays of anova data.
    The graphics based on these functions are should be especially helpful for learning how the 
    various methods have used the data to answer the question(s) posed by the various methods, 
    so they can be particularly helpful for students and non-statistician analysts. 
    The graphics can be generally helpful for identifying outliers, clusters, 
    trends or the role of non-linear transformations of data. In the case of granova.1w and granova.ds especially, 
    several arguments are provided to facilitate construction of graphics that accommodate 
    diverse features of data, and their corresponding display requirements. 
    See the help files for individual functions.
    
}
\details{
\tabular{ll}{
Package: \tab granova\cr
Version: \tab 1.3\cr
License: \tab GPL (>= 2)\cr
}
}

\author{
Robert M. Pruzek <RMPruzek@yahoo.com> 
 
James E. Helmreich <James.Helmreich@Marist.edu>

Maintainer: James E. Helmreich <James.Helmreich@Marist.edu>
}

\seealso{


\code{\link{granova.1w}}
\code{\link{granova.2w}}
\code{\link{granova.ds}}
\code{\link{granova.contr}}
}


\keyword{hplot}


