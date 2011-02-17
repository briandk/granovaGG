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

removeCurrentVersionOfGranova()
installGranovaDev()
library(granova)
pdf(file = "../tests/granova.1w.pdf", onefile = TRUE)
demo(granova.1w)
dev.off()