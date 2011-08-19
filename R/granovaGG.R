#' Elemental Graphics for Analysis of Variance Using ggplot2
#' 
#' This small collection of functions provides what we call elemental graphics
#' for display of anova results. The term elemental derives from the fact that
#' each function is aimed at construction of graphical displays that afford
#' direct visualizations of data with respect to the fundamental questions
#' that drive the particular anova methods. The two main functions are
#' granova.1w (a graphic for one way anova) and granova.2w (a corresponding
#' graphic for two way anova). These functions were written to display data
#' for any number of groups, regardless of their sizes (however, very large
#' data sets or numbers of groups can be problematic). For these two functions
#' a specialized approach is used to construct data-based contrast vectors for
#' which anova data are displayed. The result is that the graphics use
#' straight lines, and when appropriate flat surfaces, to facilitate clear
#' interpretations while being faithful to the standard effect tests in anova.
#' The graphic results are complementary to standard summary tables for these
#' two basic kinds of analysis of variance; numerical summary results of
#' analyses are also provided as side effects. Two additional functions are
#' granova.ds (for comparing two dependent samples), and granova.contr (which
#' provides graphic displays for a priori contrasts). All functions provide
#' relevant numerical results to supplement the graphic displays of anova
#' data.  The graphics based on these functions should be especially helpful
#' for learning how the methods have been applied to answer the question(s)
#' posed. This means they can be particularly helpful for students and
#' non-statistician analysts. But these methods should be quite generally
#' helpful for work-a-day applications of all kinds, as they can help to
#' identify outliers, clusters or patterns, as well as highlight the role of
#' non-linear transformations of data. In the case of granova.1w and
#' granova.ds especially, several arguments are provided to facilitate
#' flexibility in the construction of graphics that accommodate diverse
#' features of data, according to their corresponding display requirements.
#' See the help files for individual functions.
#' 
#' \tabular{ll}{ 
#'   Package: \tab granovaGG\cr 
#'   Version: \tab 1.0\cr 
#'   License: \tab GPL (>= 2)\cr 
#' }
#' 
#' @author Robert M. Pruzek \email{RMPruzek@@yahoo.com}
#' @author Brian A. Danielak \email{brian@@briandk.com}
#' @author William E. J. Doane \email{wil@@drdoane.com}
#' @author James E. Helmreich \email{James.Helmreich@@Marist.edu}
#' @author Jason Bryer \email{jason@@bryer.org}
#' 
#' @import ggplot2
#' @name granovaGG
#' @aliases granovaGG-package granovaGG
#' @docType package
#' @seealso
#' 
#' \code{\link{granovagg.1w}} \code{\link{granovagg.ds}}
#'   \code{\link{granovagg.contr}}
#' @keywords hplot
NULL

#' Family Treatment Weight change data for young female anorexia patients.
#' 

#' 
#' The MASS package includes the dataset \code{anorexia}, containing pre and
#' post treatment weights for young female anorexia patients.  This is a subset
#' of those data, containing only those patients who received Family Treatment.
#' 
#' 
#' @name anorexia.sub
#' @docType data
#' @format A dataframe with 17 observations on the following 2 variables, no
#'   NAs.
#' 
#' \describe{
#' 
#' \item{list("Prewt")}{Pretreatment weight of subject, in pounds.}
#' 
#' \item{list("Postwt")}{Postreatment weight of subject, in pounds.}
#' 
#' }
#' @references Venables, W. N. and Ripley, B. D. (2002) Modern Applied
#'   Statistics with S. Fourth edition. Springer.
#' @source Hand, D. J., Daly, F., McConway, K., Lunn, D. and Ostrowski, E. eds
#'   (1993) A Handbook of Small Data Sets. Chapman & Hall, Data set 285 (p.
#'   229)
#' @keywords datasets
NULL

#' Arousal in Rats
#' 

#' 
#' 40 rats were given divided randomly into four groups and assigned to one of
#' four treatments: placebo, drug A, drug B, or both drug A and drug B.
#' Response is a standard measure of physiological arousal.
#' 
#' 
#' @name arousal
#' @docType data
#' @format A data frame with 40 observations, 10 in each of 4 columns the
#'   corresponding to placebo, drug A, drug B and both drug A and drug B; no
#'   NAs.
#' 
#' \describe{
#' 
#' \item{list("Placebo")}{Rats receiving a placebo treatment.}
#' 
#' \item{list("Drug.A")}{Rats receiving only drug A.}
#' 
#' \item{list("Drug.B")}{Rats receiving only drug B.}
#' 
#' \item{list("Drug.A.B")}{Rats receiving both drug A and drug B.}
#' 
#' }
#' @source Richard Lowry. Concepts & Applications of Inferential Statistics.
#'   Vassar College, Poughkeepsie, N.Y., 2010,
#'   http://faculty.vassar.edu/lowry/webtext.html
#' @keywords datasets
NULL

#' Blood lead levels of lead workers' children matched with similar control
#' children.
#' 

#' 
#' Children of parents who had worked in a factory where lead was used in
#' making batteries were matched by age, exposure to traffic, and neighborhood
#' with children whose parents did not work in lead-related industries. Whole
#' blood was assessed for lead content yielding measurements in mg/dl
#' 
#' 
#' @name blood_lead
#' @docType data
#' @format A dataframe with 33 observations on the following 2 variables, no
#'   NAs.
#' 
#' \describe{
#' 
#' \item{list("Exposed")}{Blood lead level of exposed child, mg/dl.}
#' 
#' \item{list("Control")}{Blood lead level of exposed child, mg/dl.}
#' 
#' }
#' @references See discussion in Section 2.5 of Enhancing Dependent Sample
#'   Analyses with Graphics, Journal of Statistics Education Volume 17, Number
#'   1 (March 2009).
#' @source Morton, D., Saah, A., Silberg, S., Owens, W., Roberts, M., Saah, M.
#'   (1982). Lead absorption in children of employees in a lead related
#'   industry. American Journal of Epidemiology, 115:549-555.
#' @keywords datasets
NULL

#' Poison data from Biological Experiment
#' 
#' Survial times of animals in a 3 x 4 factorial experiment involving poisons
#' (3 levels) and various treatments (four levels), as described in Chapter 8
#' of Box, Hunter and Hunter.
#' 
#' 
#' @name poison
#' @docType data
#' @format This data frame was originally \code{poison.data} from the package
#'   \code{BHH2}, but as presented here has added columns; no NAs. \describe{
#'   \item{list("Poison")}{Factor with three levels I, II, and III.}
#'   \item{list("Treatment")}{Factor with four levels, A, B, C, and D.}
#'   \item{list("Group")}{Factor with 12 levels, 1:12.}
#'   \item{list("SurvTime")}{Numeric; survival time.}
#'   \item{list("RateSurvTime")}{Numeric; inverse of SurvTime}
#'   \item{list("RankRateSurvTime")}{Numeric; \code{RateSurvTime} scores have
#'   been converted to ranks, and then rescaled to have the same median as and
#'   a spread comparable to \code{RateSurvTime}} }
#' @references Box G. E. P, Hunter, J. S. and Hunter, W. C. (2005). Statistics
#'   for Experimenters II. New York: Wiley.
#' @source Box, G. E. P. and D. R. Cox, An Analysis of Transformations (with
#'   discussion), Journal of the Royal Statistical Society, Series B, Vol. 26,
#'   No. 2, pp. 211 - 254.
#' @keywords datasets
NULL
