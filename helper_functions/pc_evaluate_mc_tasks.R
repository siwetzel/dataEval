pc_evaluate_mc_tasks = function(data) {
  # TODO: these transformation rules must be reworked based on a statistical analysis!
  # TODO: Was mit NAs machen? Vorher hatte ich sie einfach stupide auf 0 gesetzt
  # Ein Versuch (nochmal nachlesen / auswerten): wenn eins NA ist, alles zu NA auswerten
  for (i in 1:nrow(data)) {
    # task a1_1
    if(is.na(data[i,1]) || is.na(data[i,2]) || is.na(data[i,3])) {
      data[i,1] = NA
    } else {
      sum = as.numeric(data[i,1]) + as.numeric(data[i,2]) + as.numeric(data[i,3])
      if (sum == 3) {
        data[i,1] = "2"
      } else if (sum == 2) {
        data[i,1] = "1"
      } else {
        data[i,1] = "0"
      }
    }
    
    # task a1_4
    if(is.na(data[i,6]) || is.na(data[i,7]) || is.na(data[i,8]) || is.na(data[i,9])) {
      data[i,6] = NA
    } else {
      sum = as.numeric(data[i,6]) + as.numeric(data[i,7]) + as.numeric(data[i,8]) + as.numeric(data[i,9])
      if (sum == 4) {
        data[i,6] = "2"
      } else if (sum == 3) {
        data[i,6] = "1"
      } else {
        data[i,6] = "0"
      }
    }
    
    
    # task a2_6
    if(is.na(data[i,17]) || is.na(data[i,18]) || is.na(data[i,19]) || is.na(data[i,20]) || is.na(data[i,21])) {
      data[i,17] = NA
    } else {
      sum = as.numeric(data[i,17]) + as.numeric(data[i,18]) + as.numeric(data[i,19]) + as.numeric(data[i,20]) + as.numeric(data[i,21])
      if (sum == 5) {
        data[i,17] = "2"
      } else if (sum == 3 || sum == 4) {
        data[i,17] = "1"
      } else {
        data[i,17] = "0"
      }
    }
    
    
    # task b1_1
    if(is.na(data[i,22]) || is.na(data[i,23]) || is.na(data[i,24])) {
      data[i,22] = NA
    } else {
      sum = as.numeric(data[i,22]) + as.numeric(data[i,23]) + as.numeric(data[i,24])
      if (sum == 3) {
        data[i,22] = "2"
      } else if (sum == 2) {
        data[i,22] = "1"
      } else {
        data[i,22] = "0"
      }
    }
    
    
    # task b1_4
    if(is.na(data[i,27]) || is.na(data[i,28]) || is.na(data[i,29]) || is.na(data[i,30])) {
      data[i,27] = NA
    } else {
      sum = as.numeric(data[i,27]) + as.numeric(data[i,28]) + as.numeric(data[i,29]) + as.numeric(data[i,30])
      if (sum == 4) {
        data[i,27] = "2"
      } else if (sum == 3) {
        data[i,27] = "1"
      } else {
        data[i,27] = "0"
      }
    }
    
    
    # task b2_6
    if(is.na(data[i,38]) || is.na(data[i,39]) || is.na(data[i,40]) || is.na(data[i,41]) || is.na(data[i,42])) {
      data[i,38] = NA
    } else {
      sum = as.numeric(data[i,38]) + as.numeric(data[i,39]) + as.numeric(data[i,40]) + as.numeric(data[i,41]) + as.numeric(data[i,42])
      if (sum == 5) {
        data[i,38] = "2"
      } else if (sum == 4 || sum == 3) {
        data[i,38] = "1"
      } else {
        data[i,38] = "0"
      }
    }
    
  }
  
  # rename the relevant columns of the mc tasks
  colnames(data)[1] = "a1_1"
  colnames(data)[6] = "a1_4"
  colnames(data)[17] = "a2_6"
  colnames(data)[22] = "b1_1"
  colnames(data)[27] = "b1_4"
  colnames(data)[38] = "b2_6"
  
  # as combined values where stored in the first column each, drop the other columns
  data = subset(data, select = -c(a1_1_2,a1_1_3,a1_4_2,a1_4_3,a1_4_4,a2_6_2,a2_6_3,a2_6_4,a2_6_5,b1_1_2,b1_1_3,b1_4_2,b1_4_3,b1_4_4,b2_6_2,b2_6_3,b2_6_4,b2_6_5) )
 
  return(data)
}