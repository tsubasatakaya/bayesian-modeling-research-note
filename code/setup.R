#-------------------------------------------------------------------------------
# Install packages and specify paths
#-------------------------------------------------------------------------------
packages <-  c(
  "tidyverse", 
  "ggplot2",
  "haven",
  "cmdstanr",
  "vroom"
)

package.check <- lapply(
  packages,
  FUN <-  function(x) {
    if (!require(x, character.only = TRUE)) {
      install.packages(x, dependencies = TRUE)
      library(x, character.only = TRUE)
    }
  }
)

data_path <- "data"
output_path <- "output"
stan_path <- "stan_files"

if (!dir.exists(output_path)) {
  dir.create(output_path, recursive = TRUE)
}

if (!dir.exists(stan_path)) {
  dir.create(stan_path, recursive = TRUE)
}