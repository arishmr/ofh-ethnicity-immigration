# Define vector of package names
packages <- c(
  "dplyr", "tidyverse", "stringr", "forcats", # data wrangling
  "psych", "epiR", # statistical analysis
  "ggplot2", "RColorBrewer", "patchwork", # visualisation
  "summarytools", "htmlTable", "officer", "gtsummary", "flextable" # descriptive stats
)

# Load packages
for (package in packages) {
  if (!require(package, character.only = TRUE)) {
    install.packages(package)
    library(package, character.only = TRUE)
  } else {
    library(package, character.only = TRUE)
  }
}

rm(packages, package)