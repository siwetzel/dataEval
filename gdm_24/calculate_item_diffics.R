library(readr)
library(TAM)

# Set working directory to folder where all R code related to the study is stored
setwd("C:/Users/Sina-/Dropbox/Lehrstuhl/Diss/Auswertung/R")

# get data
data = read_csv2("gdm_24/output_data/class13-20_gruppe_1_3.csv", show_col_types = FALSE)
data = data.frame(data, row.names = 1)


# create Q matrix (which items are procedural and which items are conceptual)
I <- 12
Q <- array( 0, dim=c( 2*I, 2 ))
Q[cbind(1:(2*I), c( rep(1,I), rep(2,I) ))] <- 1

# analysis of items
mod <- TAM::tam.mml( resp=data, Q=Q )

# output item difficulties
mod$xsi