create_Q_2dim = function(number_of_items) {
  # items at odd indices are procedural (item 1, 3, 5, ...)
  # items at even indices are conceptual (item 2, 4, 6, ...)
  Q <- array( 0, dim=c(number_of_items,2))
  for (i in 1:nrow(Q)) {
    if (i %% 2 == 1) {
      # odd number, item is procedural
      Q[i,1] = 1
    } else {
      # even number, item is conceptual
      Q[i,2] = 1
    }
  }
  return(Q)
}

create_Q_4dim = function(number_of_items) {
  # first dimension: pretest procedural
  # second dimension: pretest conceptual
  # third dimension: posttest procedural
  # fourth dimension: posttest conceptual
  
  # create Q matrix
  Q = matrix(0,number_of_items*2,4)
  # dimension 1 and 2
  for (i in 1:number_of_items) {
    if (i %% 2 == 0) { # conceptual item
      Q[i,2] = 1
    } else { # procedural item
      Q[i,1] = 1
    }
  }
  # dimension 3 and 4
  for (i in ((number_of_items + 1):(number_of_items*2))) {
    if (i %% 2 == 0) { # conceptual item
      Q[i,4] = 1
    } else { # procedural item
      Q[i,3] = 1
    }
  }
  return(Q)
}

create_Q_2_dim_mc_raw = function (number_of_items, items_procedural) {
  
}
