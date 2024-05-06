library(readr)
library(tidyverse)
library(stringr)

# Set working directory to folder where all R code related to the study is stored
setwd("C:/Users/Sina-/Dropbox/Lehrstuhl/Diss/Auswertung/R")

# Load helper functions
source("helper_functions/transform_codes_to_0_1.R")
source("helper_functions/evaluate_mc_tasks.R")

# Read coded data of all 20 classes
data_raw = data.frame(read_csv2("input_data/data_raw_final.csv", show_col_types = FALSE), row.names = 1)
data = transform_codes(data_raw)
row.names(data) = rownames(data_raw)

# TODO: Macht das Sinn oder wie geht man damit um? Für erste werden jetzt alle unklaren Einträge durch 0 ersetzt.
# Beispiel für unklar: k na bei b2_3: Es ist nicht korrekt (weil nicht beides korrekt ist) es ist auch nicht falsch (weil na nicht falsch ist) und es ist auch nicht na (Weil nicht beides na ist)
for (i in 1:nrow(data)) {
  for (j in 1:ncol(data)) {
    if (data[i,j] != "1" & data[i,j] != "0" & !is.na(data[i,j])) {
      print(c(colnames(data)[j], rownames(data)[i], data[i,j]))
      data[i,j] = "0"
    }
  }
}


# Evaluate multiple choice tasks to one score
data = evaluate_mc_tasks(data)

# code 1 and 0 as numeric values (currently saved as characters)
# create a new dataframe so that datatypes are correct
data_clean = data.frame(matrix(0,nrow(data),ncol(data)),row.names = rownames(data))
colnames(data_clean) = colnames(data)
for (i in 1:nrow(data_clean)) {
  for (j in 1:ncol(data_clean)) {
    if (!is.na(data[i,j])) {
      if (data[i,j] == "1") {
        data_clean[i,j] = 1
      } else if (data[i,j] == "0") {
        data_clean[i,j] = 0
      } 
    } else {
      data_clean[i,j] = NA
    }
    
  }
}

# Handling of missing data
# NA values are replaced with 0 (TODO: check if makes sense) (weil Powertest, siehe Diss Tobias S. 76)
data_2 = data_clean
data_2[is.na(data_2)] = 0


# TODO: Why are colnames still wrong even though they were renamed in evaluate_mc_tasks????
write.csv2(data_clean, file = "output_data/data_final.csv")
write.csv2(data_2, file = "output_data/data_final_noNA.csv")
