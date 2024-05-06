library(readr)
library(TAM)

# Set working directory to folder where all R code related to the study is stored
setwd("C:/Users/Sina-/Dropbox/Lehrstuhl/Diss/Auswertung/R")

# load helper function for virtual person transformation
source("helper_functions/virtual_person_transformation.R")
# load helper function for Q matrix
source("helper_functions/create_Q_matrix.R")

# get data
data = read_csv2("output_data/data_pc_final_noNA.csv", show_col_types = FALSE)
data = data.frame(data, row.names = 1)

# Create a transformed dataframe for the virtual person approach
data_vp = virtual_person_transformation(data)

# create Q matrix (which items are procedural and which items are conceptual)
Q = create_Q_2dim(ncol(data))

# analysis of items with PARTIAL CREDIT MODEL
# TODO 
mod <- TAM::tam.mml(resp=data_vp, Q=Q, irtmodel = "PCM")


# todO: was mit den thurtonial thresholds machen?
thresh = TAM::tam.threshold(mod)





abil = tam.wle(mod)
fitting = (tam.fit(mod))$itemfit

##### CTT ANALYSIS ##### (TODO: Müssen Trennschärfen hier oder bei vierdimensionaler Analyse berechnet werden?)
ctt1 = tam.ctt(data_vp, abil$theta.Dim01)
ctt2 = tam.ctt(data_vp, abil$theta.Dim02)

# determine pointbiserial corelation (trennschärfe) with the right dimension
# procedural
pbc1 = c()
# conceptual
pbc2 = c()

# get the corresponding values from the ctt dataframes
for (i in 1:48) {
  if ((i-1) %% 4 == 1) {
    pbc1 = append(pbc1,ctt1$rpb.WLE[i])
  } else if ((i-1) %% 4 == 3) {
    pbc2 = append(pbc2,ctt2$rpb.WLE[i])
  }
}

# calculate frequencies for successful item solving
freq = matrix(0,1,24)
colnames(freq) = colnames(data_vp)
for (j in 1:ncol(freq)) {
  temp = 0
  counter = 0
  for (i in 1:nrow(data_vp)) {
    if (!is.na(data_vp[i,j])) {
      temp = temp + data_vp[i,j]
      counter = counter + 1
    }
  }
  print(temp)
  print(counter)
  freq[1,j] = temp / counter
}


# Save item difficulties
write.table(mod$xsi$xsi, file="output_data/pc_item_diffics.csv", sep=",", col.names = FALSE, row.names = FALSE)

