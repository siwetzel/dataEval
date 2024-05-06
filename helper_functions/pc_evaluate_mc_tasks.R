pc_evaluate_mc_tasks = function(data, version) {
  # There are different version to evaluate the multiple choice tasks 
  # which are compared in compare_cmc_scorings.R
  # The final and main scoring version is version TODO
  # 1: 
  # All or nothing
  # 2:
  # Threshold 4/5, 3/4, 2/3 for 1 point, all correct for 2 points
  # 3:
  # Threshold 3/5, 2/4, 2/3 for 1 point, all correct for 2 points
  # 4:
  # Each answer is awarded a half point
  
  # TODO: handling of NA values the same in all versions??
  # TODO: Was mit NAs machen? Vorher hatte ich sie einfach stupide auf 0 gesetzt
  # Aktueller NA Ansatz (nochmal nachlesen / auswerten): wenn eins NA ist, alles zu NA auswerten
  for (i in 1:nrow(data)) {
    # task a1_1
    if(is.na(data[i,1]) || is.na(data[i,2]) || is.na(data[i,3])) {
      data[i,1] = NA
    } else {
      sum = as.numeric(data[i,1]) + as.numeric(data[i,2]) + as.numeric(data[i,3])
      data[i,j] = score(sum,3,version)
    }
    
    # task a1_4
    if(is.na(data[i,6]) || is.na(data[i,7]) || is.na(data[i,8]) || is.na(data[i,9])) {
      data[i,6] = NA
    } else {
      sum = as.numeric(data[i,6]) + as.numeric(data[i,7]) + as.numeric(data[i,8]) + as.numeric(data[i,9])
      data[i,j] = score(sum,4,version)
    }
    
    # task a2_6
    if(is.na(data[i,17]) || is.na(data[i,18]) || is.na(data[i,19]) || is.na(data[i,20]) || is.na(data[i,21])) {
      data[i,17] = NA
    } else {
      sum = as.numeric(data[i,17]) + as.numeric(data[i,18]) + as.numeric(data[i,19]) + as.numeric(data[i,20]) + as.numeric(data[i,21])
      data[i,j] = score(sum,5,version)
    }
    
    # task b1_1
    if(is.na(data[i,22]) || is.na(data[i,23]) || is.na(data[i,24])) {
      data[i,22] = NA
    } else {
      sum = as.numeric(data[i,22]) + as.numeric(data[i,23]) + as.numeric(data[i,24])
      data[i,j] = score(sum,3,version)
    }
    
    # task b1_4
    if(is.na(data[i,27]) || is.na(data[i,28]) || is.na(data[i,29]) || is.na(data[i,30])) {
      data[i,27] = NA
    } else {
      sum = as.numeric(data[i,27]) + as.numeric(data[i,28]) + as.numeric(data[i,29]) + as.numeric(data[i,30])
      data[i,j] = score(sum,4,version)
    }
    
    # task b2_6
    if(is.na(data[i,38]) || is.na(data[i,39]) || is.na(data[i,40]) || is.na(data[i,41]) || is.na(data[i,42])) {
      data[i,38] = NA
    } else {
      sum = as.numeric(data[i,38]) + as.numeric(data[i,39]) + as.numeric(data[i,40]) + as.numeric(data[i,41]) + as.numeric(data[i,42])
      data[i,j] = score(sum,5,version)
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

score = function(points, max_points, version) {
  if (version == 1) {
    # all or nothing
    if (points == max_points) {
      return (1)
    } else {
      return (0)
    }
  } else if (version == 2) {
    # threshold 4/5, 3/4, 2/3 for 1 point, all correct for 2 points
    if (points == max_points) {
      return(2)
    } else if (max_points - points == 1) {
      return(1)
    } else {
      return (0)
    }
  } else if (version == 3) {
    # Threshold 3/5, 2/4, 2/3 for 1 point, all correct for 2 points
    if (points == max_points) {
      return(2)
    } else if (max_points == 3 && points < 2) {
      return(0)
    } else if (max_points - points <= 2) {
      return(1)
    } else {
      return(0)
    } 
  } else if (version == 4) {
    # halp point per answer
    return (0.5*points)
  }
}