# Compare different scorings of CMC tasks
# 
# Version 1: 
# All or nothing
#
# Version 2:
# Threshold for 1 point, all correct for 2 points
# 
# Version 3:
# Variant of 2
#
# Version 4:
# Each answer is awarded a half point
library(readr)
library(TAM)

# Set working directory to folder where all R code related to the study is stored
setwd("C:/Users/Sina-/Dropbox/Lehrstuhl/Diss/Auswertung/R")

source("helper_functions/pc_evaluate_mc_tasks.R")
source("helper_functions/handle_missing_values.R")
# load helper function for virtual person transformation
source("helper_functions/virtual_person_transformation.R")
# load helper function for Q matrix
source("helper_functions/create_Q_matrix.R")

# get unscored multiple choice data
data_raw = data.frame(read_csv2("output_data/data_pc_final_mc_raw.csv", show_col_types = FALSE), row.names = 1)

data1 = pc_evaluate_mc_tasks(data_raw,1)
data2 = pc_evaluate_mc_tasks(data_raw,2)
data3 = pc_evaluate_mc_tasks(data_raw,3)
data4 = pc_evaluate_mc_tasks(data_raw,4)

# Create a transformed dataframe for the virtual person approach for each dataset
data_vp1 = virtual_person_transformation(data1)
data_vp2 = virtual_person_transformation(data2)
data_vp3 = virtual_person_transformation(data3)
data_vp4 = virtual_person_transformation(data4)

data1 = handle_missing_values(data1, FALSE)
data3 = handle_missing_values(data3, FALSE)
data2 = handle_missing_values(data2, FALSE)
data4 = handle_missing_values(data4, FALSE)

# create Q matrix (which items are procedural and which items are conceptual)
Q = create_Q_2dim(ncol(data1))

# analysis of items with PARTIAL CREDIT MODEL
mod1 <- TAM::tam.mml(resp=data_vp1, Q=Q, irtmodel = "PCM")
mod2 <- TAM::tam.mml(resp=data_vp2, Q=Q, irtmodel = "PCM")
mod3 <- TAM::tam.mml(resp=data_vp3, Q=Q, irtmodel = "PCM")
mod4 <- TAM::tam.mml(resp=data_vp4, Q=Q, irtmodel = "PCM") # TODO: 4 funktioniert nicht

# TODO: compare item parameters
fitting1 = (tam.fit(mod1))$itemfit
fitting2 = (tam.fit(mod2))$itemfit
fitting3 = (tam.fit(mod3))$itemfit
fitting4 = (tam.fit(mod4))$itemfit