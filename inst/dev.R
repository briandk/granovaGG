isGranovaInstalled <- function() {
  return("granova" %in% .packages(all.available = TRUE))  
}

isGranovaAttached <- function() {
  return(("package:granova" %in% search()))
}

removeCurrentVersionOfGranova <- function() {
  if (isGranovaInstalled()) {
    
    if (isGranovaAttached()) {
      print("Detaching granova package")
      detach(package:granova)
    }  
    
    print("Removing granova package")
    remove.packages("granova", lib = .libPaths())
  }
}

installGranovaDev <- function() {
  install.packages(pkgs    = '../../granova/', 
                   repos   = NULL, 
                   type    = 'source'
  )
}

runUnitTests <- function (pathToTestFile) {
  library(testthat)
  test_file(paste(pathToTestFile))
}

runUnitTests("tests/01.R")


# removeCurrentVersionOfGranova()
# installGranovaDev()
# library(granova)

# trace(granova.1w.ggplot, browser)
# pdf(file = "../temp/granova.1w.ggplot.pdf", onefile = TRUE)

#dev.off()