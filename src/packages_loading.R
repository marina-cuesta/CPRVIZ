#####################################
#### INSTALLING AND LOADING DATA ####
#####################################

## list of installed packages
installed_packages=installed.packages()

## list of packages to use
list.of.packages <- c(
  
  ##  data manage
  "dplyr", "tidyverse",
  
  ## graphs
  "ggplot2",
  "cowplot",
  
  # color blind palette
  "grafify",
  
  ## for kNN
  "dbscan",
  
  ## for parallel
  "doParallel",
  "parallel",
  
  ## for cross validation
  "caret",

  ## for ordinal numbers
  "english",
  
  ## for tables
  "magick",
  "flextable",
  
  ## for  datasets
  "mlbench", # breastcancer
  "pdp" ,#para pima
  
  ## for combinations
  "combinat"

)


## list of packages to use that are not installed yet
new.packages <- list.of.packages[!(list.of.packages %in% installed_packages)]

## installing the packages if needed
if(length(new.packages)) install.packages(new.packages)

## loading  the packages 
for (package in list.of.packages) {
  library(package, character.only=T)
}
