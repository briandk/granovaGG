if ( length( .find.package("granova", quiet=TRUE) ) == 1 ) {
  if ( "package:granova" %in% search() ) {
    detach("package:granova")
  }

  remove.packages("granova")
}

install.packages('~/Dropbox/Brian-Wil/programming/granova/', repos=NULL, type='source')
library(granova)

#trace(granova.ds.bd, browser)

demo("granova.ds.bdDEMO")
