library(readr)
library(TAM)

# Set working directory to folder where all R code related to the study is stored
setwd("C:/Users/Sina-/Dropbox/Lehrstuhl/Diss/Auswertung/R")

diffics = as.matrix(read.table("gdm_24/output_data/item_diffics.csv", sep=",", header=FALSE))

# get data
data = read_csv2("gdm_24/output_data/class13-20_gruppe_1_3_cleaned_manuallySorted.csv", show_col_types = FALSE)
data = data.frame(data, row.names = 1)


# create a dataframe with 48 columns and as many rows as the data set has
# first 24 columns are values from the pretest
# second 24 columns are values from the posttest
data_prepost = data.frame(matrix(0,nrow(data),48),row.names = rownames(data))

names = c(
  "a1_1",
  "a1_2",
  "a1_3",
  "a1_4",
  "a1_5",
  "a1_6",
  "a2_1",
  "a2_2",
  "a2_3",
  "a2_4",
  "a2_5",
  "a6_1",
  "b1_1",
  "b1_2",
  "b1_3",
  "b1_4",
  "b1_5",
  "b1_6",
  "b2_1",
  "b2_2",
  "b2_3",
  "b2_4",
  "b2_5",
  "b2_6"
)
names_pre = names
names_post = names
for (i in 1:length(names)) {
  names_pre[i] = paste("pre", names[i], sep="_")
  names_post[i] = paste("post", names[i], sep="_")
}

colnames(data_prepost) = append(names_pre, names_post)

# write pre- and postest scores in the appropriate columns
#TODO: crosschecken ob alles richtig zugeordnet wird
for (i in 1:nrow(data)) {
  for (j in 1:ncol(data)) {
    if (substr(rownames(data)[i],5,5) == "Q") {
      if (substr(colnames(data)[j],1,2)=="a1") {
        data_prepost[i,j] = data[i,j]
        data_prepost[i,j+24] = NA
      } else if (substr(colnames(data)[j],1,2)=="a2") {
        data_prepost[i,j] = data[i,j]
        data_prepost[i,j+24] = NA
      } else if (substr(colnames(data)[j],1,2)=="b1") {
        data_prepost[i,j+24] = data[i,j]
        data_prepost[i,j] = NA
      } else if (substr(colnames(data)[j],1,2)=="b2") {
        data_prepost[i,j+24] = data[i,j]
        data_prepost[i,j] = NA
      }
    } else if (substr(rownames(data)[i],5,5) == "R") {
      if (substr(colnames(data)[j],1,2)=="a1") {
        data_prepost[i,j+24] = data[i,j]
        data_prepost[i,j] = NA
      } else if (substr(colnames(data)[j],1,2)=="a2") {
        data_prepost[i,j] = data[i,j]
        data_prepost[i,j+24] = NA
      } else if (substr(colnames(data)[j],1,2)=="b1") {
        data_prepost[i,j] = data[i,j]
        data_prepost[i,j+24] = NA
      } else if (substr(colnames(data)[j],1,2)=="b2") {
        data_prepost[i,j+24] = data[i,j]
        data_prepost[i,j] = NA
      }
    } else if (substr(rownames(data)[i],5,5) == "S") {
      if (substr(colnames(data)[j],1,2)=="a1") {
        data_prepost[i,j+24] = data[i,j]
        data_prepost[i,j] = NA
      } else if (substr(colnames(data)[j],1,2)=="a2") {
        data_prepost[i,j+24] = data[i,j]
        data_prepost[i,j] = NA
      } else if (substr(colnames(data)[j],1,2)=="b1") {
        data_prepost[i,j] = data[i,j]
        data_prepost[i,j+24] = NA
      } else if (substr(colnames(data)[j],1,2)=="b2") {
        data_prepost[i,j] = data[i,j]
        data_prepost[i,j+24] = NA
      }
    } else if (substr(rownames(data)[i],5,5) == "T") {
      if (substr(colnames(data)[j],1,2)=="a1") {
        data_prepost[i,j] = data[i,j]
        data_prepost[i,j+24] = NA
      } else if (substr(colnames(data)[j],1,2)=="a2") {
        data_prepost[i,j+24] = data[i,j]
        data_prepost[i,j] = NA
      } else if (substr(colnames(data)[j],1,2)=="b1") {
        data_prepost[i,j+24] = data[i,j]
        data_prepost[i,j] = NA
      } else if (substr(colnames(data)[j],1,2)=="b2") {
        data_prepost[i,j] = data[i,j]
        data_prepost[i,j+24] = NA
      }
    }
  }
}

###### 4-dimensional Rasch-Analysis ######

# first dimension: pretest procedural
# second dimension: pretest conceptual
# third dimension: posttest procedural
# fourth dimension: posttest conceptual

# create Q matrix
Q = matrix(0,48,4)
# dimension 1 and 2
for (i in 1:24) {
  if (i %% 2 == 0) { # conceptual item
    Q[i,2] = 1
  } else { # procedural item
    Q[i,1] = 1
  }
}
# dimension 3 and 4
for (i in 25:48) {
  if (i %% 2 == 0) { # conceptual item
    Q[i,4] = 1
  } else { # procedural item
    Q[i,3] = 1
  }
}

# the difficulty vector of the stroed difficulties only has 24 items however we need 48 entries
# as pretest and posttest are treated separately
# model expects array with 2 columns, first indicates index of item
diffics_model = cbind(1:48, append(diffics,diffics))

# analysis of items
mod <- TAM::tam.mml( resp=data_prepost, Q=Q,  xsi.fixed = diffics_model, pid=colnames(data_prepost), control=list(snodes=2000) )
mp <- mod$person
fitting = (tam.fit(mod))$itemfit
min(fitting$Infit_t)
max(fitting$Infit_t)

