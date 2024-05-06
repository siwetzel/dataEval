library(readr)
library(TAM)

# Set working directory to folder where all R code related to the study is stored
setwd("C:/Users/Sina-/Dropbox/Lehrstuhl/Diss/Auswertung/R")

# load helper function for virtual person transformation
source("helper_functions/virtual_person_transformation.R")
# load helper function for Q matrix
source("helper_functions/create_Q_matrix.R")

# get data
data_raw = data.frame(read_csv2("input_data/data_raw_final.csv", show_col_types = FALSE), row.names = 1)
data = read_csv2("output_data/data_pc_final_noNA.csv", show_col_types = FALSE)
data = data.frame(data, row.names = 1)

# TODO: delete!
counterQ = 0
counterR = 0
counterS = 0
counterT = 0
for (i in 1:nrow(data_raw)) {
  if (substr(rownames(data_raw)[i],1,1) == "4") {
    if (substr(rownames(data_raw)[i],5,5) == "Q") {
      counterQ = counterQ + 1
    } else if (substr(rownames(data_raw)[i],5,5) == "R") {
      counterR = counterR + 1
    } else if (substr(rownames(data_raw)[i],5,5) == "S") {
      counterS = counterS + 1
    } else if (substr(rownames(data_raw)[i],5,5) == "T") {
      counterT = counterT + 1
    }
  }
}


# Create a transformed dataframe for the virtual person approach
data_vp = virtual_person_transformation(data)

# create Q matrix (which items are procedural and which items are conceptual)
Q = create_Q_2dim(ncol(data))

# analysis of items with PARTIAL CREDIT MODEL
mod <- TAM::tam.mml(resp=data_vp, Q=Q, irtmodel = "PCM") # two-dimensional model
mod_1dim <- TAM::tam.mml(resp=data_vp, irtmodel = "PCM") # one-dimensional model


# first variation: regard task B2_3 as a conceptual task
Q_v1 = Q
Q_v1[21,1] = 0
Q_v1[21,2] = 1

mod_v1 <- TAM::tam.mml(resp=data_vp, Q=Q_v1, irtmodel = "PCM")

# second variation: only score calculating in B2_3 and not set of solutions
# relevant data in column 40 of data_raw
# rules for scoring calculation as 0 / 1 / 2
data_v2 = data
v2_2 = c("k")
v2_1 = c("1","2","p")
v2_0 = c("3","4","5","kl","g","s","div","ab")

for (i in 1:nrow(data_raw)) {
  value = data_raw[i,40]
  values = unlist(strsplit(str_replace_all(value, " ", ""), ",")) 
  possible_2 = TRUE
  for (code in values) {
    if(code %in% v2_0) {
      data_v2[i,"b2_3"] = 0
      break
    } else if (code %in% v2_1) {
      data_v2[i,"b2_3"] = 1
      possible_2 = FALSE
      
    } else if (code %in% v2_2 && possible_2) {
      data_v2[i,"b2_3"] = 2
    }
  }
}

data_v2_vp = virtual_person_transformation(data_v2)

mod_v2 <- TAM::tam.mml(resp=data_v2_vp, Q=Q, irtmodel = "PCM")

counter = 0
for (i in 1:nrow(data_vp)) {
  for (j in 1:ncol(data_vp)) {
    if (!is.na(data_vp[i,j]) && !is.na(data_v2_vp[i,j])) {
      if (data_vp[i,j] != data_v2_vp[i,j]) {
        print(i)
        print(j)
        counter = counter + 1
      }
    }
  }
}
print(counter)


# compare models
logLik(mod)
logLik(mod_1dim)
logLik(mod_v1)
logLik(mod_v2)

anova(mod,mod_1dim)
anova(mod,mod_v2)
