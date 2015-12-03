install.packages(
  "devtools",
  dep = TRUE,
  repos = "http://cran.rstudio.com"
)

library(devtools)
install(
    pkg = "/vagrant/granovaGG",
    dependencies = TRUE
)
