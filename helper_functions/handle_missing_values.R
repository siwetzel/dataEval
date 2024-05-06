handle_missing_values = function(data, abil) {
  # TODO: Workaround für den moment: 1048R wird geskippt (weil nur NAs im Vortest)
  # TODO: vorher Datensatz checken und, falls vorhanden, Personen (Zeilen) vollständig rauswerfen, die in einem von beiden Test nur NAs haben
  
  
  # function expects full data matrix (i.e. before a virtual person transformation was performed or
  # separate columns were created for pretest and posttest items
  # data matrix needs participant codes as rownames and itemnames as colnames
  # the boolean parameter abil indicates in which step of the data analysis the missing values are handled
  # When item difficulties are calculated, omitted items are treated as incorrect (0) and not reached items as NA (abil = FALSE)
  # When the person ability parameters are calculated, not reached items are also treated as incorrect (0) (abil = TRUE)
 
  
  if (abil) {
    # prepare data for person ability calculation
    data[is.na(data)] = 0
  } else {
    # prepare data for item difficulty calculation
    
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
    
    counter = 0
    # check for each participant separately for pretest and posttest which was the
    # last item that was done and change all NAS before that to 0
    for (i in 1:nrow(data)) {
      # TODO: später entfernen wenn Datensatz entsprechend vorbereitet wurd (hat nur NAs in einem der Tests)
      if (rownames(data)[i] == "1084R" || rownames(data)[i] == "1095S") {
        next
      }
      
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
        # now change NAs to 0
        print(i)
        print(j)
        print("c1")
        for (j in a1_start:last_done_pre) {
          if(is.na(data[i,j])) {
            data[i,j] = 0
            counter = counter + 1
          }
        }
        for (j in b1_start:last_done_post) {
          if(is.na(data[i,j])) {
            data[i,j] = 0
            counter = counter + 1
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
        # now change NAs to 0
        print(i)
        print(j)
        print("c2")
        print(a2_start)
        print(last_done_pre)
        for (j in a2_start:last_done_pre) {
          if(is.na(data[i,j])) {
            data[i,j] = 0
            counter = counter + 1
          }
        }
        print(i)
        print(j)
        print("c3")
        if (last_done_post <= a1_end) { # last item done was in block a1
          for (j in b2_start:b2_end) {
            if(is.na(data[i,j])) {
              data[i,j] = 0
              counter = counter + 1
            }
          }
          print(i)
          print(j)
          print("c4")
          for (j in a1_start:last_done_post) {
            if(is.na(data[i,j])) {
              data[i,j] = 0
              counter = counter + 1
            }
          }
        } else { # last item done was in block b2
          print(i)
          print(j)
          print("c5")
          for (j in b2_start:last_done_post) {
            if(is.na(data[i,j])) {
              data[i,j] = 0
              counter = counter + 1
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
        # now change NAs to 0
        print(i)
        print(j)
        print("c6")
        for (j in b1_start:last_done_pre) {
          if(is.na(data[i,j])) {
            data[i,j] = 0
            counter = counter + 1
          }
        }
        print(i)
        print(j)
        print("c7")
        for (j in a1_start:last_done_post) {
          if(is.na(data[i,j])) {
            data[i,j] = 0
            counter = counter + 1
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
        # now change NAs to 0
        if (last_done_pre <= a1_end) { # last item done was in block a1
          for (j in b2_start:b2_end) {
            print(i)
            print(j)
            print("c9")
            if(is.na(data[i,j])) {
              data[i,j] = 0
              counter = counter + 1
            }
          }
          for (j in a1_start:last_done_pre) {
            if(is.na(data[i,j])) {
              data[i,j] = 0
              counter = counter + 1
            }
          }
        } else { # last item done was in block b2
          print(i)
          print(j)
          print("c10")
          for (j in b2_start:last_done_pre) {
            if(is.na(data[i,j])) {
              data[i,j] = 0
              counter = counter + 1
            }
          }
        }
        # posttest
        for (j in a2_start:last_done_post) {
          print(i)
          print(j)
          print("c11")
          if(is.na(data[i,j])) {
            data[i,j] = 0
            counter = counter + 1
          }
        }
      }
    }
    
    print("Number of ommitted NA entries changed: ")
    print(counter)
  }
  
  return(data)
}