isGranovaInstalled <- function () {
  return( "granova" %in% .packages(all.available = TRUE))  
}

isGranovaAttached <- function () {
  return( ("package:granova" %in% search()))
}

removeCurrentVersionOfGranova <- function () {
  if ( isGranovaInstalled() ) {
    
    if (isGranovaAttached()) {
      print("Detaching granova package")
      detach(package:granova)
    }  
    
    print("Removing granova package")
    remove.packages("granova", lib = .libPaths())
  }
}

installGranovaDev <- function () {
  removeCurrentVersionOfGranova()
  install.packages(
    pkgs    = '~/Dropbox/Brian-Wil/programming/granova/', 
    repos   = NULL, 
    type    = 'source'
  )
}


installGranovaDev()
library(granova)
demo("granova.ds.bdDEMO")
