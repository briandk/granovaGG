isGranovaInstalled <- function (RLibraryLocations) {
  return( "granova" %in% .packages(all.available = TRUE, lib.loc = RLibraryLocations))  
}

isGranovaAttached <- function () {
  return( ("package:granova" %in% search()))
}

removeCurrentVersionOfGranova <- function (RLibraryLocations) {
  if ( isGranovaInstalled(RLibraryLocations) ) {
    
    if (isGranovaAttached()) {
      print("Detaching granova package")
      detach(package:granova)
    }  
    
    print("Removing granova package")
    remove.packages("granova", lib = RLibraryLocations)
  }
}

installGranovaDev <- function () {
  RLibraryLocations <- c(
    "~/Library/R/2.12/library",
    "/Library/Frameworks/R.framework/Versions/2.12/Resources/library"
  )
  
  removeCurrentVersionOfGranova(RLibraryLocations)
  install.packages(
    pkgs    = '~/Dropbox/Brian-Wil/programming/granova/', 
    repos   = NULL, 
    type    = 'source'
  )
}


installGranovaDev()
library(granova)
demo("granova.ds.bdDEMO")
