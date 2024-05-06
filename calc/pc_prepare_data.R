library(readr)
library(tidyverse)
library(stringr)

# Set working directory to folder where all R code related to the study is stored
setwd("C:/Users/Sina-/Dropbox/Lehrstuhl/Diss/Auswertung/R")

# Load helper functions
source("helper_functions/pc_transform_codes_to_0_1_2.R")
source("helper_functions/pc_evaluate_mc_tasks.R")
source("helper_functions/handle_missing_values.R")

# Read coded data of all 20 classes
data_raw = data.frame(read_csv2("input_data/data_raw_final.csv", show_col_types = FALSE), row.names = 1)
data = transform_codes(data_raw)
row.names(data) = rownames(data_raw)

# TODO: Macht das Sinn oder wie geht man damit um? Für erste werden jetzt alle unklaren Einträge durch 0 ersetzt.
# Beispiel für unklar: k na bei b2_3: Es ist nicht korrekt (weil nicht beides korrekt ist) es ist auch nicht falsch (weil na nicht falsch ist) und es ist auch nicht na (Weil nicht beides na ist)
for (i in 1:nrow(data)) {
  for (j in 1:ncol(data)) {
    if (data[i,j] != "2" & data[i,j] != "1" & data[i,j] != "0" & !is.na(data[i,j])) {
      print(c(colnames(data)[j], rownames(data)[i], data[i,j]))
      data[i,j] = "0"
    }
  }
}

# code 2, 1 and 0 as numeric values (currently saved as characters)
# create a new dataframe so that datatypes are correct
data_clean = data.frame(matrix(0,nrow(data),ncol(data)),row.names = rownames(data))
colnames(data_clean) = colnames(data)
for (i in 1:nrow(data_clean)) {
  for (j in 1:ncol(data_clean)) {
    if (!is.na(data[i,j])) {
      if (data[i,j] == "2") {
        data_clean[i,j] = 2
      } else if (data[i,j] == "1") {
        data_clean[i,j] = 1
      } else if (data[i,j] == "0") {
        data_clean[i,j] = 0
      } else {
        # unknown code, handle as if wrong
        data_clean[i,j] = 0
      }
    } else {
      data_clean[i,j] = NA
    }
    
  }
}

# same for raw mc data
data_mc_raw = data # save data where mc items are not yet transformed into one score
data_mc_raw_clean = data.frame(matrix(0,nrow(data_mc_raw),ncol(data_mc_raw)),row.names = rownames(data_mc_raw))
colnames(data_mc_raw_clean) = colnames(data_mc_raw)
for (i in 1:nrow(data_mc_raw_clean)) {
  for (j in 1:ncol(data_mc_raw_clean)) {
    if (!is.na(data_mc_raw[i,j])) {
      if (data_mc_raw[i,j] == "2") {
        data_mc_raw_clean[i,j] = 2
      } else if (data_mc_raw[i,j] == "1") {
        data_mc_raw_clean[i,j] = 1
      } else if (data_mc_raw[i,j] == "0") {
        data_mc_raw_clean[i,j] = 0
      } else {
        data_mc_raw_clean[i,j] = NA
      }
    }
    
  }
}

# Evaluate multiple choice tasks to one score
data_clean = pc_evaluate_mc_tasks(data_clean, 2) # currently using version 2 as a transformation rule

# # calculate statistics for partial credit tasks how often tasks were scored with 0, 1, 2
# statistic = data.frame(matrix(0,3,ncol(data)))
# rownames(statistic) = c("2","1","0")
# colnames(statistic) = colnames(data)
# for (i in 1:nrow(data)) {
#   for (j in 1:ncol(data)) {
#     if(is.na(data[i,j])) {
#       next
#     }
#     if(data[i,j] == "2") {
#       statistic["2",j] = statistic["2",j] + 1
#     } else if(data[i,j] == "1") {
#       statistic["1",j] = statistic["1",j] + 1
#     } else if(data[i,j] == "0") {
#       statistic["0",j] = statistic["0",j] + 1
#     } 
#   }
# }


# TODO: muss noch getestet werden ob Omitted / Not reached richtig kodiert werden
# Handling of missing data
data_2 = handle_missing_values(data_clean, FALSE)


#### OLD
# NA values are replaced with 0 (TODO: check if makes sense) (weil Powertest, siehe Diss Tobias S. 76)
#data_2 = data_clean
#data_2[is.na(data_2)] = 0

#data_3 = data_mc_raw_clean
#data_3[is.na(data_3)] = 0 
###

# TODO: Why are colnames still wrong even though they were renamed in evaluate_mc_tasks????
write.csv2(data_clean, file = "output_data/data_pc_final.csv")
write.csv2(data_2, file = "output_data/data_pc_final_noNA.csv")
write.csv2(data_3, file = "output_data/data_pc_final_noNA_mc_raw.csv")
