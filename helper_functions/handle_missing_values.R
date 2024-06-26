code_missing_values = function(data) {
  # function expects full data matrix (i.e. before a virtual person transformation was performed or
  # separate columns were created for pretest and posttest items
  # data matrix needs participant codes as rownames and itemnames as colnames

  # check at which position item blocks start and end 
  # (can vary due to including more / less items or treating cmc items as one or multiple scores)
  a1_start = -1
  a1_end = -1
  a2_start = -1
  a2_end = -1
  b1_start = -1
  b1_end = -1
  b2_start = -1
  b2_end = ncol(data)
  for (j in 1:ncol(data)) {
    if (substr(colnames(data)[j],1,2) == "a1" && a1_start == -1) {
      a1_start = j
    } else if (substr(colnames(data)[j],1,2) == "a2" && a2_start == -1) {
      a2_start = j
      a1_end = j-1
    } else if (substr(colnames(data)[j],1,2) == "b1" && b1_start == -1) {
      b1_start = j
      a2_end = j-1
    } else if (substr(colnames(data)[j],1,2) == "b2" && b2_start == -1) {
      b2_start = j
      b1_end = j-1
    }
  }

    
  # check for each participant separately for pretest and posttest which was the
  # last item that was done. Mark all missing items before that as omitted ("55") as well as the next item after the last one done
  # mark all items after the last item that was marked as omitted as not reached ("99")
  for (i in 1:nrow(data)) {
    booklet = substr(rownames(data)[i],5,5)
    last_done_pre = 0
    last_done_post = 0
    if (booklet == "Q") {
      ### pre = A1A2 & post = B1B2 ###
      # check last tasks done in pretest and posttest
      for (j in a1_start:a2_end) {
        if(!is.na(data[i,j])) {
          last_done_pre = j
        }
      }
      for (j in b1_start:b2_end) {
        if(!is.na(data[i,j])) {
          last_done_post = j
        }
      }
      
      # now change NAs to OM 
      for (j in a1_start:(last_done_pre+1)) {
        if (j <= a2_end) {
          if(is.na(data[i,j])) {
            data[i,j] = "55"
          }
        }
      }
      for (j in b1_start:(last_done_post+1)) {
        if (j <= b2_end) {
          if(is.na(data[i,j])) {
            data[i,j] = "55"
          }
        }
      }
    } else if (booklet == "R") {
      ### pre = A2B1 & post = B2A1 ###
      # check last tasks done in pretest and posttest
      for (j in a2_start:b1_end) {
        if(!is.na(data[i,j])) {
          last_done_pre = j
        }
      }
      # two separate loops for posttest as combination b2a1 does not have increasing indices
      for (j in b2_start:b2_end) {
        if(!is.na(data[i,j])) {
          last_done_post = j
        }
      }
      for (j in a1_start:a1_end) {
        if(!is.na(data[i,j])) {
          last_done_post = j
        }
      }
      # now change NAs to OM
      for (j in a2_start:(last_done_pre+1)) {
        if (j <= b1_end) {
          if(is.na(data[i,j])) {
            data[i,j] = "55"
          }
        }
      }
      
      if (last_done_post <= a1_end) { # last item done was in block a1
        for (j in b2_start:b2_end) {
          if(is.na(data[i,j])) {
            data[i,j] = "55"
          }
        }
        
        for (j in a1_start:(last_done_post+1)) {
          if(j <= a1_end) {
            if(is.na(data[i,j])) {
              data[i,j] = "55"
            }
          }
        }
      } else { # last item done was in block b2
        for (j in b2_start:(last_done_post+1)) {
          if (j <= b2_end) {
            if(is.na(data[i,j])) {
              data[i,j] = "55"
            }
          }
        }
      }
    } else if (booklet == "S") {
      ### pre = B1B2 & post = A1A2 ###
      # check last tasks done in pretest and posttest
      for (j in b1_start:b2_end) {
        if(!is.na(data[i,j])) {
          last_done_pre = j
        }
      }
      for (j in a1_start:a2_end) {
        if(!is.na(data[i,j])) {
          last_done_post = j
        }
      }
      # now change NAs to OM
      for (j in b1_start:(last_done_pre+1)) {
        if (j <= b2_end) {
          if(is.na(data[i,j])) {
            data[i,j] = "55"
          }
        }
      }
      for (j in a1_start:(last_done_post+1)) {
        if (j <= a2_end) {
          if(is.na(data[i,j])) {
            data[i,j] = "55"
          }
        }
      }
    } else if (booklet == "T") {
      ### pre = B2A1 & post = A2B1 ###
      # check last tasks done in pretest and posttest
      # two separate loops for pretest as combination b2a1 does not have increasing indices
      for (j in b2_start:b2_end) {
        if(!is.na(data[i,j])) {
          last_done_pre = j
        }
      }
      for (j in a1_start:a1_end) {
        if(!is.na(data[i,j])) {
          last_done_pre = j
        }
      }
      # posttest
      for (j in a2_start:b1_end) {
        if(!is.na(data[i,j])) {
          last_done_post = j
        }
      }
      # now change NAs to OM
      if (last_done_pre <= a1_end) { # last item done was in block a1
        for (j in b2_start:b2_end) {
          if(is.na(data[i,j])) {
            data[i,j] = "55"
          }
        }
        for (j in a1_start:(last_done_pre+1)) {
          if (j <= a1_end) {
            if(is.na(data[i,j])) {
              data[i,j] = "55"
            }
          }
        }
      } else { # last item done was in block b2
        for (j in b2_start:(last_done_pre+1)) {
          if (j <= b2_end) {
            if(is.na(data[i,j])) {
              data[i,j] = "55"
            }
          }
        }
      }
      # posttest
      for (j in a2_start:(last_done_post+1)) {
        if (j <= b1_end) {
          if(is.na(data[i,j])) {
            data[i,j] = "55"
          }
        }
      }
    }
  }
  
  # change remaining NAs to not reached (as there are no other NAs as the input matrix is a full matrix)
  data[is.na(data)] = "99" 
  
  print("Number of ommitted NA entries changed: ")
  print(sum(apply(data, MARGIN = c(1, 2), FUN = function(x) x == "55")))
        
  
  print("Number of not reached NA entries changed: ")
  print(sum(apply(data, MARGIN = c(1, 2), FUN = function(x) x == "99")))
  
  return(data)
}

handle_missing_values = function(data, abil, useIntegers) {
  # the boolean parameter abil indicates in which step of the data analysis the missing values are handled
  # When item difficulties are calculated, omitted items are treated as incorrect (0) and not reached items as NA (abil = FALSE)
  # When the person ability parameters are calculated, not reached items are also treated as incorrect (0) (abil = TRUE)
  if (abil) {
    if (useIntegers) {
      data[data == "99" || data == "55"] = 0
    } else {
      data[data == "99" || data == "55"] = "0"
    }
  } else {
    if (useIntegers) {
      data[data == "55"] = 0
    } else {
      data[data == "55"] = "0"
    }
    
    data[data == "99"] = NA
  }
  return(data)
}