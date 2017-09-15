# Setting working directory
setwd("G://MSBAPM/01 Spring 2017/Predictive modeling/Team project/Walmart basket analysis/01 Data/train")

#PLEASE INSTALL THESE LIBRARIES FOR EXECUTING THE BELOW SCRIPT

#Loading required libraries
library(ggplot2)
library(dplyr)
library(reshape2)

# Importing train dataset

train<-read.csv("train - Copy.csv",colClasses = c('character','character',
                                           'character','character',
                                           'numeric','character','character','character'))

#Using the below function to make objects within dataframes accessible
attach(train)

#Creating a function to calculate number of missing values, proportion of missing values 
#and number of unique values across each column
missing_values = function(input)
{
  n = length(colnames(input)) # number of columns
  a <- NULL
  b <- NULL 
  c <- NULL
  for(i in 1:n) 
  {
    a[i]=sum(is.na(input[,i])) 
    b=a/nrow(input) 
    c[i]=length(unique(input[,i])) 
  }
  result=data.frame(colnames(input),a,b,c) 
  colnames(result) = c("column Name", "# Missing Values", "% Missing Value", "Unique Values")
  return(result) 
}

##Applying the missing_values function on train
missing_train<-missing_values(train)

View(missing_train)


blank_values = function(input)
{
  n = length(colnames(input)) # number of columns
  a <- NULL
  b <- NULL 
  c <- NULL
  for(i in 1:n) 
  {
    a[i]=sum(ifelse(input[,i]=="",1,0)) 
    b=a/nrow(input) 
    c[i]=length(unique(input[,i])) 
  }
  result=data.frame(colnames(input),a,b,c) 
  colnames(result) = c("column Name", "# Blank Values", "% Blank Value", "Unique Values")
  return(result) 
}

blank_train<-blank_values(train)


#Recode blank space values in train $ FineLine to 999999

na_index<-which(train$FinelineNumber=="")

train$FinelineNumber[na_index]<-"999999"

#Recode NA's in Upc to 000000000000

na_upc_index<-which(train$Upc=="")

train$Upc[na_upc_index]<-"000000000000"

# Standardizing UPCs - Incomplete code

##Look at the lengths of strings
#table(nchar(train$Upc))

##Flag the UPCs that need leading zeros 
#train$is_upc_complete<-ifelse(nchar(train$Upc)==12 || train$Upc=='',1,0)

##Adding leading zeros wherever necessary
#train$upc11<-as.character(ifelse(train$is_upc_complete==0,sprintf("%011d",as.integer(train$Upc)),train$Upc))


#Creating a vector containing digits in UPC
#upc_digits <- function(x)
#{
 # floor(x / 10^((nchar(x) - 1):0)) %% 10
#}


#Function for calculating checksum

#checksum = function(x)
#{
  #a=upc_digits(x)
  #if(length(a)==12) 
  #{
    #return(0)
  #}
  #else if(length(a)== 1)
  #{
  #  odd=sum(a[1])
   # even=0
    #return(10 - ((3*odd+even)%%10))
  #}
  
  #else if(length(a)%% 2 == 0)
  #{
   # odd=sum(a[seq(1,length(a)-1,2)])
    #even=sum(a[seq(2,length(a),2)])
    #return(10 - ((3*odd+even)%%10))
  #}
  #else if(length(a)%% 2 == 1)
  #{
    #odd=sum(a[seq(1,length(a),2)])
    #even=sum(a[seq(2,length(a)-1,2)])
    #return(10 - ((3*odd+even)%%10))
  #}
  #else return(0)
#}

#Compute the checksum using 11 digit UPC
#train$checksum<-numeric(647054)
#for(i in 1:647054)
#{
# train$checksum[i]=checksum(as.numeric(train$UPC11[i]))
#}


#Compute the 12 digit UPC

#train$upc12<-numeric(647054)

#train$upc12<-ifelse(nchar(train$Upc)==12,train$Upc,paste0(train$UPC11,train$checksum))

#write.csv(train,"train_upc_cleaned.csv")

companycode_upc=data.frame(table(substr(Upc11,1,9)))

companycode2_upc=data.frame(table(substr(Upc11,1,6)))
