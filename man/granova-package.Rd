\name{granova-package}
\alias{granova-package}
\alias{granova}
\docType{package}
\title{
Graphical Analysis of Variance
}
\description{
This small collection of functions provides what we call elemental graphics for display of anova
    results. The term elemental derives from the fact that each function is aimed at construction of
    graphical displays that afford direct visualizations of data with respect to the fundamental 
    questions that drive the particular anova methods. The two main functions are granova.1w 
    (a graphic for one way anova) and granova.2w (a corresponding graphic for two way anova). These 
    functions were written to display data for any number of groups, regardless of their sizes 
    (however, very large data sets or numbers of groups can be problematic). For these two functions 
    a specialized approach is used to construct data-based contrast vectors for which anova data are
    displayed. The result is that the graphics use straight lines, and when appropriate flat surfaces,
    to facilitate clear interpretations while being faithful to the standard effect tests in anova. 
    The graphic results are complementary to standard summary tables for these two basic kinds of 
    analysis of variance; numerical summary results of analyses are also provided as side effects.
    Two additional functions are granova.ds (for comparing two dependent samples), and granova.contr
    (which provides graphic displays for a priori contrasts). All functions provide relevant
    numerical results to supplement the graphic displays of anova data.
    The graphics based on these functions should be especially helpful for learning how the methods have 
    been applied to answer the question(s) posed. This means they can be 
    particularly helpful for students and non-statistician analysts. But these methods should be
    quite generally helpful for work-a-day applications of all kinds, as they can help to identify
    outliers, clusters or patterns, as well as highlight the role of non-linear transformations of data. In the case 
    of granova.1w and granova.ds especially, several arguments are provided to facilitate flexibility
    in the construction of graphics that accommodate diverse features of data, according to their 
    corresponding display requirements. See the help files for individual functions.
    
}
\details{
\tabular{ll}{
Package: \tab granova\cr
Version: \tab 2.0\cr
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


