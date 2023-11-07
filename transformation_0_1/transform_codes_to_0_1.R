transform_codes <- function(){
  # TODO: add data as input later to function again
  # TODO: remove later, just for debugging purposes
  data <- read_csv2("C:/Users/Sina-/Dropbox/Lehrstuhl/Diss/Auswertung/R/irr/input_data/marc_19_20.csv", show_col_types = FALSE)
  data = data.frame(data, row.names = 1)
  
  # This function transforms coded data to values of 0 and 1
  # The applied transformation rules are specified in "Transformationsregeln Kodierung.docx"
  # Input value: data of type dataframe (?)
  
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
    "b1_1_2",
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
  # left column: values that are transformed to 1
  # right column: values that are transformed to 0
  # tasks in A1
  rules_a1 = matrix(list(
    (c(2)), c(1,0),
    c(2), c(1,0),
    c(1), c(2,0),
    c("k"), c("sp","i","g","nl","str","s","ab"),
    c("k","1","2","sp"), c("2","4","kl","g","x","y","s","div","ab"),
    c("k","fk","ff","ub","d","n"), c("f"),
    c(1), c(2,0),
    c(2), c(1,0),
    c(1), c(2,0),
    c(2), c(1,0),
    c("k"), c("d","f","u"),
    c(""), c("1,2,3,4,5"), # todo: wie geh ich hier mit allen Fällen um? Muss ich das handeln?
    c("k","kr","ks"), c("kt","ku","ab","f")
  ),2)
  
  # tasks in A2
  rules_a2 = matrix(list(
    c(5), c(0,1,2,3,4,6),
    c("k"), c("e","sp","p","g","nl","str","s","ab"),
    c("k","1","2","p"), c("3","4","kl","g","s","div","ab"),
    c("k","fk","ff","ub","d","n"), c("f","int"),
    c("k"), c("p","i","ab"),
    c("k","d"), c("p", "f"),
    c(5), c(0,1,2,3,4,6,7,8,9,10,11,12),
    c(2), c(1,0),
    c(2), c(1,0),
    c(1), c(2,0),
    c(1), c(2,0),
    c(1), c(2,0)
  ),2)
  
  # tasks in B1
  rules_b1 = matrix(list(
    c(2), c(1,0),
    c(1), c(2,0),
    c(2), c(1,0),
    c("k"), c("p","i","g","nl","str","s","ab"),
    c("k","1","2","sp"), c("3","4","kl","g","x","y","s","div","ab"),
    c("k","fk","ff","ub","d","n"), c("f"),
    c(1), c(2,0),
    c(1), c(2,0),
    c(2), c(1,0),
    c(2), c(1,0),
    c("k","ks"), c("u","f"),
    c("k","ks"), c("kt","ku","ab","f")
  ),2)
  
  
  # tasks in B2
  rules_b2 = matrix(list(
    c(2), c(0,1,3,4,5,6),
    c("k"), c("e","sp","l","g","nl","str","s","ab"),
    c("k","1","2","p"), c("3","4","kl","g","s","div","ab"),
    c("k","ff","nb"), c("f","fk","n","int"),
    c("k","fp"), c("sp","i","ab"),
    c(5), c(0,1,2,3,4,6,7,8,9),
    c(2), c(1,0),
    c(2), c(1,0),
    c(1), c(2,0),
    c(1), c(2,0),
    c(1), c(2,0)
  ),2)
  
  mapping_matrix = cbind(rules_a1, rules_a2, rules_b1, rules_b2)
  rownames(mapping_matrix) = c("1","0")
  
  # vector that can be read as a function mapping the indexes of its entries to other indexes
  # used to map all 48 coded columns to only 42 columns with 0 and 1
  index_mapper = c(
    1,2,3,4,5,5,6,7,8,9,10,10,11,12,13,14,14,15,15,16,17,18,19,20,21,22,23,24,25,26,26,27,28,29,30,31,32,33,34,35,35,36,37,38,39,40,41,42
  )
  
  
  # Transform all data by iterating over input data row-wise
  for (i in 1:nrow(data)) {
    for (j in 1:ncol(data)) {
      # skip columns where multiple conditions must be met
      if (j %in% c(5,6,16,17,18,19,30,31,40,41)){
        next
      }
      
      # TODO: Seperate multiple comma seperated entries
      # check whether entry evaluates to 1
      if(data[i,j] %in% mapping_matrix["1",j]) {
        df[i,index_mapper[j]] = 1
      }
      
      # check whether entry evaluates to 0
      if(data[i,j] %in% mapping_matrix["0",j]) {
        df[i,index_mapper[j]] = 0
      }
      
    }
     
    
    return(df)
  }
}
