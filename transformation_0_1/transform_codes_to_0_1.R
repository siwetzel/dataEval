transform_codes <- function(data){
  # This function transforms coded data to values of 0 and 1
  # The applied transformation rules are specified in "Transformationsregeln Kodierung.docx"
  # Input value: data of type dataframe
  
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
  
  # Transform all data by iterating over input data row-wise
  for (i in 1:nrow(data)) {
    
  }


}


#data <- read_csv2("C:/Users/Sina-/Dropbox/Lehrstuhl/Diss/Auswertung/R/transformation_0_1/coded.csv", show_col_types = FALSE)