library(readr)
library(TAM)
library(WrightMap)

# Set working directory to folder where all R code related to the study is stored
setwd("C:/Users/Sina-/Dropbox/Lehrstuhl/Diss/Auswertung/R")

# load helper function for virtual person transformation
source("helper_functions/virtual_person_transformation.R")
# load helper function for Q matrix
source("helper_functions/create_Q_matrix.R")
# load helper function for handling of missing values (55/99)
source("helper_functions/handle_missing_values.R")

# get data
data = read_csv2("output_data/data_pc_scored.csv", show_col_types = FALSE)
data = data.frame(data, row.names = 1)

# Transform OM / NR values to 0 / NA
data = handle_missing_values(data,FALSE,TRUE)

# Create a transformed dataframe for the virtual person approach
data_vp = virtual_person_transformation(data)

# create Q matrix (which items are procedural and which items are conceptual)
Q = create_Q_2dim(ncol(data))

# analysis of items with PARTIAL CREDIT MODEL
mod <- TAM::tam.mml(resp=data_vp, Q=Q, irtmodel = "PCM")
mod2=TAM::tam.mml(resp=data_vp, Q=Q, irtmodel = "PCM2")
# <- TAM::tam.mml(resp=data_vp, irtmodel = "PCM")

g <- mod$item_irt
gg <- g[g$tau.Cat1 > g$tau.Cat2, ]
rownames(gg)

plot(mod, 
     type = "items", 
     export = FALSE, 
     package = "graphics", 
     observed = TRUE, 
     low = -6, 
     high = 6)

# todO: was mit den thurtonial thresholds machen?
thresh = TAM::tam.threshold(mod)

# determine pointbiserial corelation (trennschärfe)
abil = tam.wle(mod)

ctt1 = tam.ctt(data_vp, abil$theta.Dim01)
ctt2 = tam.ctt(data_vp, abil$theta.Dim02)

# match with the correct dimensions
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

# Analyse Infit values
fitting = (tam.fit(mod))$itemfit

# Save item difficulties
write.table(mod$xsi$xsi, file="output_data/pc_item_diffics.csv", sep=",", col.names = FALSE, row.names = FALSE)

