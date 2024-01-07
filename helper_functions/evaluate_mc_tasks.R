evaluate_mc_tasks = function(data) {
  # TODO: these transformation rules must be reworked based on a statistical analysis!
  
  
  # for the purpose of this evaluation, replace NA values with 0
  # TODO: re-evaluate if this makes sense
  copy_data = data
  data[data == "na"] = "0"
  

  for (i in 1:nrow(data)) {
    # task a1_1
    # 2 of 3 needed 
    sum = as.numeric(data[i,1]) + as.numeric(data[i,2]) + as.numeric(data[i,3])
    if (sum >= 2) {
      copy_data[i,1] = "1"
    } else {
      copy_data[i,1] = "0"
    }
    
    # task a1_4
    # 3 of 4 needed
    sum = as.numeric(data[i,6]) + as.numeric(data[i,7]) + as.numeric(data[i,8]) + as.numeric(data[i,9])
    if (sum >= 3) {
      copy_data[i,6] = "1"
    } else {
      copy_data[i,6] = "0"
    }
    
    # task a2_6
    # 4 of 5 needed
    sum = as.numeric(data[i,17]) + as.numeric(data[i,18]) + as.numeric(data[i,19]) + as.numeric(data[i,20]) + as.numeric(data[i,21])
    if (sum >= 4) {
      copy_data[i,17] = "1"
    } else {
      copy_data[i,17] = "0"
    }
    
    # task b1_1
    # 2 of 3 needed
    sum = as.numeric(data[i,22]) + as.numeric(data[i,23]) + as.numeric(data[i,24])
    if (sum >= 2) {
      copy_data[i,22] = "1"
    } else {
      copy_data[i,22] = "0"
    }
    
    # task b1_4
    # 3 of 4 needed
    sum = as.numeric(data[i,27]) + as.numeric(data[i,28]) + as.numeric(data[i,29]) + as.numeric(data[i,30])
    if (sum >= 3) {
      copy_data[i,27] = "1"
    } else {
      copy_data[i,27] = "0"
    }
    
    # task b2_6
    # 3 of 5 needed
    sum = as.numeric(data[i,38]) + as.numeric(data[i,39]) + as.numeric(data[i,40]) + as.numeric(data[i,41]) + as.numeric(data[i,42])
    if (sum >= 4) {
      copy_data[i,38] = "1"
    } else {
      copy_data[i,38] = "0"
    }
  }
  
  # as combined values where stored in the first column each, drop the other columns
  copy_data = subset(copy_data, select = -c(a1_1_2,a1_1_3,a1_4_2,a1_4_3,a1_4_4,a2_6_2,a2_6_3,a2_6_4,a2_6_5,b1_1_2,b1_1_3,b1_4_2,b1_4_3,b1_4_4,b2_6_2,b2_6_3,b2_6_4,b2_6_5) )
  
  # rename the remaining columns of the mc tasks
  # TODO: This does not work
  copy_data %>% 
    rename(
      a1_1 = a1_1_1,
      a1_4 = a1_4_1,
      a2_6 = a2_6_1,
      b1_1 = b1_1_1,
      b1_4 = b1_4_1,
      b2_6 = b2_6_1
    )
  # This does also not work
  #colnames(copy_data)[colnames(copy_data) == "a1_1"] = "a1_1"
 
   return(copy_data)
}