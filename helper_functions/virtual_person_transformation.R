virtual_person_transformation = function(data) {
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
  return(data_vp)
}