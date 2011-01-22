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
    
    remove.packages("granova")
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
demo("granova.ds.bdDEMO")
