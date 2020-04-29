
#Capstone Project 

setwd("C:/Users/akadali/Desktop/Python Scripts/Capstone Project Srinath")

data1 <- read.csv("train Data.csv", header = TRUE, na.strings = c(""," ","  ","?","NaN"))
data2 <- data1
min_rows <- nrow(data1)*0.8

#Columns to remove
cols_rem <- NULL

# Removing the columns that have more than 20% NAs
for(i in 1:ncol(data1)){
    if(sum(is.na(data1[,i]))>min_rows){
    cols_rem <- c(cols_rem,i)
  }
}
length(cols_rem)
data1 <- data1[,-cols_rem]
ncol(data1)


# to determine the names of columns that still have NAs
col_names <- NULL
j <- NULL
miss_no <- NULL
for(j in 1:ncol(data1)){
  if(sum(is.na(data1[,j]))>0){
  #col_names <- c(col_names,names(data1[j]))
  #miss_no <- c(miss_no,j)
  #p <- is.na(data1[,j])
  if((is.numeric(data1[,j])|is.integer(data1[,j])))
    data1[,j] <- median(data1[,j])
  else 
    data1[,j] <- mode(data[,j])
  
  }
}

#Columns that still have NAs
#col_names

#str(data1)
"""
#Imputation
table(is.na(data1))

Impu_Int <- function(x){
      median(data1[,x])
}

Impu_Cat <- function(x){
     mode(data1[,x])
}

for(i in 1:ncol(data1)){
  p <- is.na(data1[,i])
  if((is.numeric(data1[,i])|is.integer(data1[,i])))
    data1[p,i] <- Impu_Int(data1[,i])
  else 
    data1[p,i] <- Impu_Cat(data1[,i])
}
"""

