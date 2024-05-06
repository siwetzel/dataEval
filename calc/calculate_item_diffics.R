library(readr)
library(TAM)

# Set working directory to folder where all R code related to the study is stored
setwd("C:/Users/Sina-/Dropbox/Lehrstuhl/Diss/Auswertung/R")

# get data
data = read_csv2("output_data/data_final_noNA.csv", show_col_types = FALSE)
data = data.frame(data, row.names = 1)

############### VIRTUAL PERSON TRANSFORMATION ###################
# Create a transformed dataframe for the virtual person approach
# move all entries for items that were not administered at a round to the corresponding virtual person

data_vp = data.frame(matrix(NA,2*nrow(data),ncol(data)))

# create new rownames
for (i in 1:nrow(data_vp)) {
  if (i <= nrow(data_vp) / 2) {
    row.names(data_vp)[i] = paste(row.names(data)[i], "pre", sep="_")
  } else {
    row.names(data_vp)[i] = paste(row.names(data)[i-(nrow(data_vp) / 2)], "post", sep="_")
  }
}
colnames(data_vp) = colnames(data)

# iterate over entries and move them to the virtual person where necessary
# logic:
# Q: Pre -> A1,A2 Post -> B1,B2
# R: Pre -> A2,B1 Post -> B2,A1
# S: Pre -> B1,B2 Post -> A1,A2
# T: Pre -> B2,A1 Post -> A2,B1

# to move an entry to the virtual person, it has to be moved that many rows downwards
offset = nrow(data)

for (i in 1:nrow(data)) {
  for (j in 1:ncol(data)) {
    # grepl checks if a substring is in a string
    if (grepl("Q",row.names(data)[i])) {
      # move data accordingly to Q rules 
      if (substr(colnames(data)[j],1,2) == "a1") {
        data_vp[i,j] = data[i,j]
      } else if (substr(colnames(data)[j],1,2) == "a2") {
        data_vp[i,j] = data[i,j]
      } else if (substr(colnames(data)[j],1,2) == "b1") {
        data_vp[i+offset,j] = data[i,j]
      } else if (substr(colnames(data)[j],1,2) == "b2") {
        data_vp[i+offset,j] = data[i,j]
      } 
    } else if (grepl("R",row.names(data)[i])) {
      # move data accordingly to R rules 
      if (substr(colnames(data)[j],1,2) == "a1") {
        data_vp[i+offset,j] = data[i,j]
      } else if (substr(colnames(data)[j],1,2) == "a2") {
        data_vp[i,j] = data[i,j]
      } else if (substr(colnames(data)[j],1,2) == "b1") {
        data_vp[i,j] = data[i,j]
      } else if (substr(colnames(data)[j],1,2) == "b2") {
        data_vp[i+offset,j] = data[i,j]
      } 
    } else if (grepl("S",row.names(data)[i])) {
      # move data accordingly to S rules 
      if (substr(colnames(data)[j],1,2) == "a1") {
        data_vp[i+offset,j] = data[i,j]
      } else if (substr(colnames(data)[j],1,2) == "a2") {
        data_vp[i+offset,j] = data[i,j]
      } else if (substr(colnames(data)[j],1,2) == "b1") {
        data_vp[i,j] = data[i,j]
      } else if (substr(colnames(data)[j],1,2) == "b2") {
        data_vp[i,j] = data[i,j]
      } 
    } else if (grepl("T",row.names(data)[i])) {
      # move data accordingly to T rules 
      if (substr(colnames(data)[j],1,2) == "a1") {
        data_vp[i,j] = data[i,j]
      } else if (substr(colnames(data)[j],1,2) == "a2") {
        data_vp[i+offset,j] = data[i,j]
      } else if (substr(colnames(data)[j],1,2) == "b1") {
        data_vp[i+offset,j] = data[i,j]
      } else if (substr(colnames(data)[j],1,2) == "b2") {
        data_vp[i,j] = data[i,j]
      } 
    }
  }
}
################################################################


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
mod <- TAM::tam.mml(resp=data_vp, Q=Q)
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
write.table(mod$xsi$xsi, file="output_data/item_diffics.csv", sep=",", col.names = FALSE, row.names = FALSE)

