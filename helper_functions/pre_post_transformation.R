# function takes full data matrix and transforms it into a matrix with
# twice as much columns and an item for pretest and posttest each

pre_post_transformation = function(data) {
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
    "a2_6",
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
  colnames(data) = names
  names_pre = names
  names_post = names
  for (i in 1:length(names)) {
    names_pre[i] = paste("pre", names[i], sep="_")
    names_post[i] = paste("post", names[i], sep="_")
  }
  
  colnames(data_prepost) = append(names_pre, names_post)
  
  # write pre- and postest scores in the appropriate columns
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
  
  return(data_prepost)
}
                                   
                                   
