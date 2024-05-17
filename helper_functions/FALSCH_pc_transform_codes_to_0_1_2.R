
check_value = function(value, value_set, check_for_zero) { # TODO: macht diese Funktion Sinn???
  # check if a coded value belongs to a certain value set
  # if check_for_zero is true (bedeutet: es wird überprüft ob dieser Eintrag zu 0 evaluiert wird), it is enough that one value is in the set to return true
  
  # value could contain a concatenated string of more than one code
  # first, any existing spaces are removed
  # next, a list with all codes is stored in values (could also be a list with just one element)
  values = unlist(strsplit(str_replace_all(value, " ", ""), ","))
  
  in_set = TRUE
  
  for (code in values) {
    if (check_for_zero && code %in% value_set) {
      return(TRUE)
    }
    if (!(code %in% value_set)) {
      in_set = FALSE
    }
  }
  
  return(in_set)
  
}

# TODO Ist es evtl. so, dass es nicht richtig funktioniert weil 2er von 1er überschrieben werden oder 1er 2ernicht überschreiben aber es sollten?

transform_codes <- function(data){
  # This function transforms coded data to values of 0, 1 and 2
  # The applied transformation rules are specified in "Transformationsregeln Kodierung.docx"
  
  # Create a result data frame with 42 columns and corresponding column titles
  df <- data.frame(matrix(ncol = 42, nrow = nrow(data)))
  custom_names <- c(
    "a1_1_1",
    "a1_1_2",
    "a1_1_3",
    "a1_2",
    "a1_3",
    "a1_4_1",
    "a1_4_2",
    "a1_4_3",
    "a1_4_4",
    "a1_5",
    "a1_6",
    "a2_1",
    "a2_2",
    "a2_3",
    "a2_4",
    "a2_5",
    "a2_6_1",
    "a2_6_2",
    "a2_6_3",
    "a2_6_4",
    "a2_6_5",
    "b1_1_1",
    "b1_1_2",
    "b1_1_3",
    "b1_2",
    "b1_3",
    "b1_4_1",
    "b1_4_2",
    "b1_4_3",
    "b1_4_4",
    "b1_5",
    "b1_6",
    "b2_1",
    "b2_2",
    "b2_3",
    "b2_4",
    "b2_5",
    "b2_6_1",
    "b2_6_2",
    "b2_6_3",
    "b2_6_4",
    "b2_6_5"
  )
  colnames(df) <- custom_names
  
  
  # rules for mapping
  
  # case 1: polytomous items
  # left column: values that are transformed to 2
  # middle column: values that are transformed to 1
  # right column: values that are transformed to 0
  
  # case 2 dichotomous items
  # left column: values that are transformed to 1
  # middle column: values that are transformed to 0
  # right column: empty
  
  # tasks in A1
  # TODO: Ist doppelnutzung der k codes (zB in Zeile 5) ein Problem?
  rules_a1 = matrix(list(
    c("2"), c("1","0"), c(""),
    c("2"), c("1","0"), c(""),
    c("1"), c("2","0"), c(""),
    c("k"), c("sp","i","g","nl","str","s","ab"), c(""),
    c("k"), c("k","1","2","sp"), c("3","4","5","kl","g","x","y","s","div","ab"),
    c("k"), c("k","fk","ff","ub","d","n"), c("f"),
    c("1"), c("2","0"), c(""),
    c("2"), c("1","0"), c(""),
    c("1"), c("2","0"), c(""),
    c("2"), c("1","0"), c(""),
    c("k"), c("d"), c("f","u"),
    c(""), c("1,2,3,4,5"), c(""), # todo: Ist diese Zeile nicht komplett irrelevant? Sie ist eh nicht vollständig. Wird sie später berücksichtigt?
    c("k","kr"), c("ks","kt","ku"), c("ab","f")
  ),3)

  
  # tasks in A2
  rules_a2 = matrix(list(
    c("5"), c("0","1","2","3","4","6"), c(""),
    c("k"), c("e","sp","p","g","nl","str","s","ab"), c(""),
    c("k"), c("k","1","2","p"), c("3","4","5","kl","g","s","div","ab"),
    c("k"), c("k","fk","ff","ub","d","n"), c("f","int"),
    c("k"), c("k"), c("p","i","ab"),
    c("k"), c("d","f"), c("p", "f"),
    c("5"), c("0","1","2","3","4","6","7","8","9","10","11","12"), c(""),
    c("2"), c("1","0"), c(""),
    c("2"), c("1","0"), c(""),
    c("1"), c("2","0"), c(""),
    c("1"), c("2","0"), c(""),
    c("1"), c("2","0"), c("")
  ),3)
  
  # tasks in B1
  rules_b1 = matrix(list(
    c("2"), c("1","0"), c(""),
    c("1"), c("2","0"), c(""),
    c("2"), c("1","0"), c(""),
    c("k"), c("p","i","g","nl","str","s","ab"), c(""),
    c("k"), c("k","1","2","sp"), c("3","4","5","kl","g","x","y","s","div","ab"),
    c("k"), c("k","fk","ff","ub","d","n"), c("f"),
    c("1"), c("2","0"), c(""),
    c("1"), c("2","0"), c(""),
    c("2"), c("1","0"), c(""),
    c("2"), c("1","0"), c(""),
    c("k"), c("ks"), c("u","f","ab"),
    c("k"), c("ks","kt","ku"), c("ab","f")
  ),3)
  
  
  # tasks in B2
  rules_b2 = matrix(list(
    c("2"), c("0","1","3","4","5","6"), c(""),
    c("k"), c("e","sp","i","g","nl","str","s","ab"), c(""),
    c("k"), c("k","1","2","p"), c("3","4","5","kl","g","s","div","ab"),
    c("k"), c("k","ff","nb"), c("f","fk","n","int"),
    c("k","fp"), c("sp","i","ab"), c(""),
    c("5","9"), c("0","1","2","3","4","6","7","8"), c(""),
    c("2"), c("1","0"), c(""),
    c("2"), c("1","0"), c(""),
    c("1"), c("2","0"), c(""),
    c("1"), c("2","0"), c(""),
    c("1"), c("2","0"), c("")
  ),3)
  
  mapping_matrix = cbind(rules_a1, rules_a2, rules_b1, rules_b2)
  rownames(mapping_matrix) = c("2","1","0")
  
  # vector that can be read as a function mapping the indexes of its entries to other indexes
  # used to map all 48 coded columns to only 42 columns with 0 and 1
  index_mapper = c(
    1,2,3,4,5,5,6,7,8,9,10,10,11,12,13,14,14,15,15,16,17,18,19,20,21,22,23,24,25,26,26,27,28,29,30,31,32,33,34,35,35,36,37,38,39,40,41,42
  )
  
  
  # Transform all data by iterating over input data row-wise
  for (i in 1:nrow(data)) {
    for (j in 1:ncol(data)) {
      # skip columns where multiple conditions must be met
      if (j %in% c(5,6,12,16,17,18,19,30,31,40,41)){ 
        next
      }
      
      # check whether task is a task that is scored with partial credits
      # (check whether third entry with row index "0" is empty)
      if (mapping_matrix["0",j] == "") {
        # 0 / 1 scoring
        # check whether entry evaluates to 1
        if (check_value(data[i,j],unlist(mapping_matrix["2",j]),FALSE)) {
          df[i,index_mapper[j]] = 1
        }
        
        # check whether entry evaluates to 0
        else if (check_value(data[i,j],unlist(mapping_matrix["1",j]),TRUE)) {
          df[i,index_mapper[j]] = 0
        }
        
        # keep original coded value if it cannot be mapped to 1 or 0
        else {
          df[i,index_mapper[j]] = data[i,j]
        }
      } else {
        # partial credit scoring
        # check whether entry evaluates to 2
        if (check_value(data[i,j],unlist(mapping_matrix["2",j]),FALSE)) {
          df[i,index_mapper[j]] = 2
        }
        
        # check whether entry evaluates to 1
        else if (check_value(data[i,j],unlist(mapping_matrix["1",j]),FALSE)) {
          df[i,index_mapper[j]] = 1
        }
        
        # check whether entry evaluates to 0
        else if (check_value(data[i,j],unlist(mapping_matrix["0",j]),TRUE)) {
          df[i,index_mapper[j]] = 0
        }
        
        # keep original coded value if it cannot be mapped to 1 or 0
        else {
          df[i,index_mapper[j]] = data[i,j]
        }
      }
    }
  }
  
  # Transform data were multiple conditions must be met
  # TODO: was wenn es nicht drin ist und nur eins von beidem nicht drin ist? dann wird check value false beim check ob es in 0 ist
  for (i in 1:nrow(data)) {
    for (j in c(5, 16, 18, 30, 40)) {
      # partial credit scoring
      # both conditions must be met to be evaluated to 2
      if(check_value(data[i,j],unlist(mapping_matrix["2",j]),FALSE) && check_value(data[i,j+1],unlist(mapping_matrix["2",j+1]),FALSE)) {
        df[i,index_mapper[j]] = 2
      }
        
      # both conditions must be met to be evaluated to 1
      else if(check_value(data[i,j],unlist(mapping_matrix["1",j]),FALSE) && check_value(data[i,j+1],unlist(mapping_matrix["1",j+1]),FALSE)) {
        df[i,index_mapper[j]] = 1
      }
        
      # only one condition must be violated to be evaluated to 0
      else if(check_value(data[i,j],unlist(mapping_matrix["0",j]),TRUE) || check_value(data[i,j+1],unlist(mapping_matrix["0",j+1]),TRUE)) {
        df[i,index_mapper[j]] = 0
      }
        
      # keep merged original coded value if it cannot be mapped to 1 or 0
      else {
        if (is.na(data[i,j]) && is.na(data[i,j+1])) {
          df[i,index_mapper[j]] = NA
        } else {
          df[i,index_mapper[j]] = paste(data[i,j],data[i,j+1])
        }
      }
      print(j)
      print(unlist(strsplit(str_replace_all(data[i,j], " ", ""), ",")))
      print(unlist(strsplit(str_replace_all(data[i,j+1], " ", ""), ",")))
      print(df[i,index_mapper[j]])
      
    }
    
    
  }
  
  return(df)
}
