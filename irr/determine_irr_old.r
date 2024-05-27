library(readr)
library(irr)

one_kappa = function(rater1, rater2) {
  # calculates and prints Cohen's kappa for the transformed ratings (0 and 1s)
  # Apply transformation rules to coded data 
  rater1_transformed = transform_codes(data.frame(rater1, row.names = 1))
  rater2_transformed = transform_codes(data.frame(rater2, row.names = 1))
  
  # Transformed data must be in the form of a matrix with dim (n*m) rows and one column
  # with n = number of participants and m = number of tasks
  rater1_col = vector()
  for (i in 1:nrow(rater1_transformed)) {
    for (j in 1:ncol(rater1_transformed)) {
      rater1_col = append(rater1_col,rater1_transformed[i,j])
    }
  }
  
  rater2_col = vector()
  for (i in 1:nrow(rater2_transformed)) {
    for (j in 1:ncol(rater2_transformed)) {
      rater2_col = append(rater2_col,rater2_transformed[i,j])
    }
  }
  
  # Determine inter-rater reliability using Cohen's Kappa
  kappa2(cbind(rater1_col,rater2_col))
  
}

kappa_per_task = function(rater1, rater2) {
  # calcuates and prints Cohen's kappa for each task for the coded, non-transformed values
  rater1 = data.frame(rater1, row.names = 1)
  rater2 = data.frame(rater2, row.names = 1)
  avg = 0
  for (j in 1:ncol(rater1)) {
    kap = kappa2(cbind(rater1[,j],rater2[,j]))$value
    print(kap)
    avg = avg + kap
  }
  print("Average:")
  print(avg/ncol(rater1))
}

# Set working directory to folder where all R code related to the study is stored
# Note: this path must be changed if the code is used on a different computer
setwd("C:/Users/Sina-/Dropbox/Lehrstuhl/Diss/Auswertung/R")

# Load helper functions
source("helper_functions/transform_codes_to_0_1.R")

# Read coded data of the three raters for classes 17-20
marc_raw <- read_csv2("irr/input_data/marc_17_20.csv", show_col_types = FALSE)
felix_raw <- read_csv2("irr/input_data/felix_17_20.csv", show_col_types = FALSE)

one_kappa(marc_raw,felix_raw)
kappa_per_task(marc_raw,felix_raw)
