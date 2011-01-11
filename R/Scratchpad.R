## Required Libraries
library(ggplot2)
library(DAAG) # contains the pair65 data

## Loading in the data
data(pair65)
str(pair65)

## Defining the Function

granova.ds.bd <- function(
                   data                      = pair65, 
                   southwestPlotOffsetFactor = 0.4,
                   northeastPlotOffsetFactor = 0.5,
                   plotTitle                 = "Dependent Sample Scatterplot",
                   tTestConfidenceLevel      = 0.95,
                   produceBlankPlotObject    = TRUE
) 

{
  dd <- data.frame(
          xvals  = data[ , 1], 
          yvals  = data[ , 2],
          effect = data[ , 2]  - data[ , 1]
        )  

  # We're going to build the plot in several pieces. First, we compute
  # statistics on the data passed in, and use them to define square graphical
  # bounds for the viewing window. Then, we use grammar to build the plot layer
  # by layer. Because of the way ggplot2 creates plot objects, layers can be
  # added to a plot p simply by calling "p <- p + newLayer", so for now you'll
  # see that structure of code throughout.
    
  ## Computing t-test Statistics for the Confidence Band and Mean Difference
  performDependentSampleTtest <- function (xValues, yValues, confidenceLevel) {
    return (
      t.test( 
              xValues, 
              yValues, 
              paired     = TRUE,
              conf.level = confidenceLevel
      )
    )
  }
  
  dependentSampleTtestStatistics <- performDependentSampleTtest(
                                      dd$xvals,
                                      dd$yvals,
                                      confidenceLevel = tTestConfidenceLevel
  )

  getTreatmentEffectQuantiles <- function (tTestStatistics) {
    effectQuantiles <- data.frame(
      lowerTreatmentEffect = as.numeric(tTestStatistics$conf.int[2]),
      meanTreatmentEffect  = as.numeric(tTestStatistics$estimate),
      upperTreatmentEffect = as.numeric(tTestStatistics$conf.int[1])
    )  
    return(effectQuantiles)
  }
  
  treatmentEffects <- getTreatmentEffectQuantiles(dependentSampleTtestStatistics)
  print((treatmentEffects$meanTreatmentEffect))
  print(treatmentEffects)
}

granova.ds.bd(pair65)
