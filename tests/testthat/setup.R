set.seed(123)

# Load commonly used demo datasets once so tests can rely on them
datasets_to_load <- c(
  "arousal",
  "poison",
  "anorexia.sub"
)

for (dataset in datasets_to_load) {
  data(list = dataset, package = "granovaGG", envir = environment())
}

rm(datasets_to_load)
