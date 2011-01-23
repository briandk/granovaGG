\name{poison}
\alias{poison}
\docType{data}
\title{Poison data from Biological Experiment}
\description{Survial times of animals in a 3 x 4 factorial experiment involving poisons (3 levels) and various treatments (four levels), as described in Chapter 8 of Box, Hunter and Hunter.
}
\usage{data(poison)}
\format{This data frame was originally \code{poison.data} from the package \code{BHH2}, but as presented here has added columns; no NAs.
\describe{
    \item{\code{Poison}}{Factor with three levels I, II, and III.}
    \item{\code{Treatment}}{Factor with four levels, A, B, C, and D.}
    \item{\code{Group}}{Factor with 12 levels, 1:12.}
    \item{\code{SurvTime}}{Numeric; survival time.}
    \item{\code{RateSurvTime}}{Numeric; inverse of SurvTime}
    \item{\code{RankRateSurvTime}}{Numeric; \code{RateSurvTime} scores have been converted to ranks, and then rescaled to have the same median as and a spread comparable to \code{RateSurvTime}}
}}
\source{Box, G. E. P. and D. R. Cox, An Analysis of Transformations (with discussion), Journal of the Royal Statistical Society, Series B, Vol. 26, No. 2, pp. 211 - 254.
}
\references{Box G. E. P, Hunter, J. S. and Hunter, W. C. (2005). Statistics for Experimenters II. New York: Wiley.}
\keyword{datasets}
