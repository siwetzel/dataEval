library(readr)
library(TAM)

# TODO: habe ich die Werte jetzt wirklich korrekt als virtuelle Personen behandelt? Weil ich sie nicht untereinander gehängt habe....


# Set working directory to folder where all R code related to the study is stored
setwd("C:/Users/Sina-/Dropbox/Lehrstuhl/Diss/Auswertung/R")

# get data
data = read_csv2("gdm_24/output_data/class13-20_gruppe_1_2.csv", show_col_types = FALSE)
data = data.frame(data, row.names = 1)


# code missing values with NA (not na). 
# code 1 and 0 as numeric values
# create a new dataframe so that datatypes are correct
data_clean = data.frame(matrix(0,nrow(data),ncol(data)),row.names = rownames(data))
colnames(data_clean) = colnames(data)
for (i in 1:nrow(data_clean)) {
  for (j in 1:ncol(data_clean)) {
    if (data[i,j] == "1") {
      data_clean[i,j] = 1
    } else if (data[i,j] == "na") {
      data_clean[i,j] = NA
    }
  }
}

#data[data == "na"] = NA
# code 1 and 0 as numeric values
#data[data == "0"] = 0
#data[data == "1"] = 1


# create Q matrix (which items are procedural and which items are conceptual)
# items at odd indices are procedural (item 1, 3, 5, ...)
# items at even indices are conceptual (item 2, 4, 6, ...)
# there are 24 items in total
Q <- array( 0, dim=c(24,2))
for (i in 1:nrow(Q)) {
  if (i %% 2 == 1) {
    # odd number, item is procedural
    Q[i,1] = 1
  } else {
    # even number, item is conceptual
    Q[i,2] = 1
  }
}


# analysis of items
mod <- TAM::tam.mml(resp=data_clean, Q=Q)

# Save item difficulties
write.table(mod$xsi$xsi, file="gdm_24/output_data/item_diffics_groups_1_2.csv", sep=",", col.names = FALSE, row.names = FALSE)

# Save cleaned data table
write.csv2(data_clean, file = "gdm_24/output_data/class13-20_gruppe_1_2_cleaned.csv")

