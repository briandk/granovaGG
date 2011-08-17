isGranovaInstalled <- function() {
  return("granovaGG" %in% .packages(all.available = TRUE))  
}

isGranovaAttached <- function() {
  return(("package:granovaGG" %in% search()))
}

removeCurrentVersionOfGranova <- function() {
  if (isGranovaInstalled()) {
    
    if (isGranovaAttached()) {
      print("Detaching granovaGG package")
      detach(package:granovaGG)
    }  
    
    print("Removing granovaGG package")
    remove.packages("granovaGG", lib = .libPaths())
  }
}

installGranovaDev <- function() {
  install.packages(pkgs    = '../../granovaGG/', 
                   repos   = NULL, 
                   type    = 'source'
  )
}

removeCurrentVersionOfGranova()
installGranovaDev()
library(granovaGG)
demo(granovagg.1w)
warnings()
