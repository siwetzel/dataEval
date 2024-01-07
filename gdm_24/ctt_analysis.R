library(readr)
library(TAM)
library(tidyverse)
library(report)
library(rstatix)
library(ggpubr)

# Set working directory to folder where all R code related to the study is stored
setwd("C:/Users/Sina-/Dropbox/Lehrstuhl/Diss/Auswertung/R")

# get data
data = read_csv2("gdm_24/output_data/class13-20_gruppe_1_3_cleaned.csv", show_col_types = FALSE)
data = data.frame(data, row.names = 1)

View(data)