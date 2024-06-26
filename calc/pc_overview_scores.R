library(readr)
library(tidyverse)
library(dplyr)
library(rstatix)
library(ggplot2)

# Set working directory to folder where all R code related to the study is stored
setwd("C:/Users/Sina-/Dropbox/Lehrstuhl/Diss/Auswertung/R/")

# load helper function for transformation of matrix into separate columns for pre- and posttest
source("helper_functions/pre_post_transformation.R")

# Read coded data of first questionnaire
data = data.frame(read_csv2("output_data/data_pc_scored.csv", show_col_types = FALSE), row.names = 1)

task_names = c(
  "P1",
  "K1",
  "P3",
  "K3",
  "P5",
  "K5",
  "P2",
  "K2",
  "P4",
  "K4",
  "P6",
  "K6",
  "P7",
  "K7",
  "P9",
  "K9",
  "P11",
  "K11",
  "P8",
  "K8",
  "P10",
  "K10",
  "P12",
  "K12"
)

colnames(data) = task_names

# procedural items
data %>% count(P1)
data %>% count(P2)
data %>% count(P3)
data %>% count(P4)
data %>% count(P5)
data %>% count(P6)
data %>% count(P7)
data %>% count(P8)
data %>% count(P9)
data %>% count(P10)
data %>% count(P11)
data %>% count(P12)

# conceptual items
data %>% count(K1)
data %>% count(K2)
data %>% count(K3)
data %>% count(K4)
data %>% count(K5)
data %>% count(K6)
data %>% count(K7)
data %>% count(K8)
data %>% count(K9)
data %>% count(K10)
data %>% count(K11)
data %>% count(K12)

# Solution Rates
# For this purpose, replace omitted values (55) with 0 and nor reached values (99) with NA
data_rates = data
data_rates[data_rates == 55] = 0
data_rates[data_rates == 99] = NA

# Dichotomously scored items 
# a2_1, a2_5, b2_1, b2_5, a1_2, a2_2, b1_2, b2_2, b2_4
# P2, P6, P8, P12, K1, K2, K7, K8, K10
# WARNING: Values cannot be interpreted for other items!
sol_rates_dichot = sapply(data_rates,function(x)  sum(x, na.rm = TRUE) / (sum(x == 0, na.rm = TRUE) + sum(x == 1, na.rm = TRUE)))

# Polytomously scored items (other)
# WARNING: Values cannot be interpreted for other items!
sol_rates_polyt = sapply(data_rates,function(x)  (sum(x, na.rm = TRUE) / (sum(x == 0, na.rm = TRUE) + sum(x == 1, na.rm = TRUE) +  sum(x == 2, na.rm = TRUE)))/2)

# build a vector that contains the correct solution rates
sol_rates = c(sol_rates_dichot[c("P2","P6","P8","P12","K1","K2","K7","K8","K10")],sol_rates_polyt[c("P1","P3","P4","P5","P7","P9","P10","P11","K3","K4","K5","K6","K9","K11","K12")])
sol_rates_pro = sol_rates[c("P1","P2","P3","P4","P5","P6","P7","P8","P9","P10","P11","P12")]
sol_rates_con = sol_rates[c("K1","K2","K3","K4","K5","K6","K7","K8","K9","K10","K11","K12")]


# solution rates in pretest and posttest
# TODO: checken ob pre_post_transformation auch mit der Datenstruktur von data_rates klar kommt
data_prepost_rates = pre_post_transformation(data_rates)
data_pre_rates = data_prepost_rates[,1:24]
data_post_rates = data_prepost_rates[,25:48]
colnames(data_pre_rates)=sapply(task_names, function(x) paste(x,"pre",sep="_"))
colnames(data_post_rates)=sapply(task_names, function(x) paste(x,"post",sep="_"))

sol_rates_dichot_pre = sapply(data_pre_rates,function(x)  sum(x, na.rm = TRUE) / (sum(x == 0, na.rm = TRUE) + sum(x == 1, na.rm = TRUE)))
sol_rates_dichot_post = sapply(data_post_rates,function(x)  sum(x, na.rm = TRUE) / (sum(x == 0, na.rm = TRUE) + sum(x == 1, na.rm = TRUE)))
sol_rates_polyt_pre = sapply(data_pre_rates,function(x)  (sum(x, na.rm = TRUE) / (sum(x == 0, na.rm = TRUE) + sum(x == 1, na.rm = TRUE) +  sum(x == 2, na.rm = TRUE)))/2)
sol_rates_polyt_post = sapply(data_post_rates,function(x)  (sum(x, na.rm = TRUE) / (sum(x == 0, na.rm = TRUE) + sum(x == 1, na.rm = TRUE) +  sum(x == 2, na.rm = TRUE)))/2)

###### compare rate of omitted responses in pretest and posttest
data_prepost = pre_post_transformation(data)
data_pre = data_prepost[,1:24]
data_post = data_prepost[,25:48]
colnames(data_pre)=sapply(task_names, function(x) paste(x,"pre",sep="_"))
colnames(data_post)=sapply(task_names, function(x) paste(x,"post",sep="_"))

# calculate omitted rates for pretest
omit_pre = sapply(data_pre,function(x)  sum(x == 55 , na.rm = TRUE) / (sum(x == 55 , na.rm = TRUE) + sum(x == 99 , na.rm = TRUE) + sum(x == 0 , na.rm = TRUE) + sum(x == 1 , na.rm = TRUE) + sum(x == 2 , na.rm = TRUE)))
omit_post = sapply(data_post,function(x)  sum(x == 55 , na.rm = TRUE) / (sum(x == 55 , na.rm = TRUE) + sum(x == 99 , na.rm = TRUE) + sum(x == 0 , na.rm = TRUE) + sum(x == 1 , na.rm = TRUE) + sum(x == 2 , na.rm = TRUE)))

# print percentages for procedural and conceptual items
print ("Percentages omitted:")
print("Pretest procedural items:")
for (i in 1:24) {
  if (i %% 2 == 1) {
    print(omit_pre[i]*100)
  }
}
print("Pretest conceptual items:")
for (i in 1:24) {
  if (i %% 2 == 0) {
    print(omit_pre[i]*100)
  }
}
print("Posttest procedural items:")
for (i in 1:24) {
  if (i %% 2 == 1) {
    print(omit_post[i]*100)
  }
}
print("Posttest conceptual items:")
for (i in 1:24) {
  if (i %% 2 == 0) {
    print(omit_post[i]*100)
  }
}

# compare number of omitted items in pretest for four treatment groups for the following items:
# a1_3, a2_3, b1_3, b1_5, b2_3, a1_2, a1_6, a2_2, a2_4, b1_2, b1_6, b2_2, b2_4
omit_per_group = data.frame(matrix(0,4,ncol(data)*2),row.names = c("1","2","3","4"))
# make colnames
namearray = c()
for (j in 1:ncol(data)) {
  namearray = c(namearray,paste(colnames(data)[j],"omit",sep=" "),paste(colnames(data)[j],"not_omit",sep=" "))
}
colnames(omit_per_group) = namearray
# TODO: check if not NA
for (i in 1:nrow(data_pre)) {
  print(i)
  group = substr(rownames(data)[i],1,1)
  for (j in 1:ncol(data_pre)) {
    print(j)
    if (!is.na(data_pre[i,j])) {
      if(data_pre[i,j] == 55) {
        print("A")
        omit_per_group[group,paste(colnames(data)[j],"omit",sep=" ")] = omit_per_group[group,paste(colnames(data)[j],"omit",sep=" ")] + 1
      } else {
        print("B")
        print(group)
        print(paste(colnames(data_pre)[j],"not_omit",sep=" "))
        omit_per_group[group,paste(colnames(data)[j],"not_omit",sep=" ")] = omit_per_group[group,paste(colnames(data)[j],"not_omit",sep=" ")] + 1
      }
    }
  }
}

# perform chi-square test item-wise
# a1_3, a2_3, b1_3, b1_5, b2_3, a1_2, a1_6, a2_2, a2_4, b1_2, b1_6, b2_2, b2_4
chi_square_a1_3 = chisq.test(omit_per_group[,1:2])
chi_square_a2_3 = chisq.test(omit_per_group[,17:18])
chi_square_b1_3 = chisq.test(omit_per_group[,29:30])
chi_square_b1_5 = chisq.test(omit_per_group[,33:34])
chi_square_b2_3 = chisq.test(omit_per_group[,41:42])
chi_square_a1_2 = chisq.test(omit_per_group[,3:4])
chi_square_a1_6 = chisq.test(omit_per_group[,11:12])
chi_square_a2_2 = chisq.test(omit_per_group[,15:16])
chi_square_a2_4 = chisq.test(omit_per_group[,19:20])
chi_square_b1_2 = chisq.test(omit_per_group[,27:28])
chi_square_b1_6 = chisq.test(omit_per_group[,35:36])
chi_square_b2_2 = chisq.test(omit_per_group[,39:40])
chi_square_b2_4 = chisq.test(omit_per_group[,43:44])

chi_square_a1_3
chi_square_a2_3
chi_square_b1_3
chi_square_b1_5
chi_square_b2_3
chi_square_a1_2
chi_square_a1_6
chi_square_a2_2
chi_square_a2_4
chi_square_b1_2 
chi_square_b1_6
chi_square_b2_2
chi_square_b2_4
