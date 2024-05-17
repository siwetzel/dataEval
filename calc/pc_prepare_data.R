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

# Replace unknown entries with 0 (apart from certain exceptions, see below)
counter = 0
for (i in 1:nrow(data)) {
  for (j in 1:ncol(data)) {
    if (data[i,j] != "2" & data[i,j] != "1" & data[i,j] != "0" & !is.na(data[i,j])) {
      print(c(colnames(data)[j], rownames(data)[i], data[i,j]))
      # handle special cases that should not just be replaced with 0
      # task a1_3 code k NA / k ab
      if(colnames(data)[j] == "a1_3" && (data[i,j] == "k NA" || data[i,j] == "k ab")) {
        data[i,j] = "1"
        print("Replaced with 1")
        next
      }
      # task a2_3 code k nb / k NA / k ab
      if(colnames(data)[j] == "a2_3" && (data[i,j] == "k nb" || data[i,j] == "k NA" || data[i,j] == "k ab")) {
        data[i,j] = "1"
        print("Replaced with 1")
        next
      }
      # task a2_4 code k NA / k ab
      if(colnames(data)[j] == "a2_4" && (data[i,j] == "k NA" || data[i,j] == "k ab")) {
        data[i,j] = "1"
        print("Replaced with 1")
        next
      }
      # task b1_3 code k NA / k ab
      if(colnames(data)[j] == "b1_3" && (data[i,j] == "k NA" || data[i,j] == "k ab")) {
        data[i,j] = "1"
        print("Replaced with 1")
        next
      }
      # task b2_3 code k NA / k ab
      if(colnames(data)[j] == "b2_3" && (data[i,j] == "k NA" || data[i,j] == "k ab")) {
        data[i,j] = "1"
        print("Replaced with 1")
        next
      }
      
      # Otherwise replace unclear entry with 0
      print("WARNING: Replaced unclear entry with 0!")
      data[i,j] = "0"
      counter = counter + 1
    }
  }
}
print("Replaced entries with 0: ")
print(counter)

# save data where mc items are not yet transformed into one score
data_mc_raw = data

# Evaluate multiple choice tasks to one score
data = pc_evaluate_mc_tasks(data, 2) # TODO currently using version 2 as a transformation rule 

# code missing values as either omitted or not reached
data_NAcoded = code_missing_values(data) 

# Transform OM / NR values to 0 / NA
# TODO: move different versions to beginning of item difficulty calculation and person ability calculation
#data_NAhandled = handle_missing_values(data_NAcoded,FALSE,FALSE)

# code 2, 1 and 0 as numeric values (currently saved as characters)
# code omitted ("55") as 55 and not reached ("99") as 99
# create a new dataframe so that datatypes are correct
data_clean = data.frame(matrix(0,nrow(data),ncol(data)),row.names = rownames(data))
colnames(data_clean) = colnames(data)
for (i in 1:nrow(data_clean)) {
  for (j in 1:ncol(data_clean)) {
    if (data_NAcoded[i,j] == "2") {
      data_clean[i,j] = 2
    } else if (data_NAcoded[i,j] == "1") {
      data_clean[i,j] = 1
    } else if (data_NAcoded[i,j] == "0") {
      data_clean[i,j] = 0
    } else if (data_NAcoded[i,j] == "55") {
      data_clean[i,j] = 55
    } else if (data_NAcoded[i,j] == "99") {
      data_clean[i,j] = 99
    } else {
      # unknown code, handle as if wrong
      # this should not be executed so print a warning if it gets executed
      print("Warning: Executing a line that should not be reached")
      print(c(i,j,data_NAcoded[i,j]))
      data_clean[i,j] = 0
    }
  }
}

# same for raw multiple choice data
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
        # unknown code, handle as if wrong
        # this should not be executed so print a warning if it gets executed
        print("Warning: Executing a line that should not be reached")
        print(c(i,j,data_mc_raw[i,j]))
        data_mc_raw_clean[i,j] = 0
      }
    } else {
      data_mc_raw_clean[i,j] = NA
    }
  }
}

# TODO: needed?
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

write.csv2(data_clean, file = "output_data/data_pc_scored.csv")
write.csv2(data_mc_raw_clean, file = "output_data/data_pc_scored_mc_raw.csv")
