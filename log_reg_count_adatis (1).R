# Import data ----
rm(list=ls())

dd <- read.csv("C:\\Users\\kivanova4\\Documents\\Business Analytics\\Business Analytics\\2 semestur\\BI\\Adatis\\DataNetov\\Book11.csv", stringsAsFactors = FALSE, sep = ",", header = TRUE)

library(dplyr)

data <- select(dd, TransactionDate, TransactionAmount)

#CHeck var classes 
sapply(data, class)
#TransactionDate TransactionAmount          
#"character"       "character" 


#Fix date column and create a variable to group by
library(lubridate)

data$TransactionDate <- dmy(data$TransactionDate)

#Extract month Year
data$Month_Yr <- format(as.Date(data$TransactionDate), "%Y%m")

data$Month_Yr <- as.numeric(data$Month_Yr)

#Extract month
data$month <- month(data$TransactionDate)

sapply(data, class)
#TTransactionDate TransactionAmount Month_Yr            month 
#"Date"         "numeric"          "numeric"           "numeric" 

#Fix TransactionAmount
#change TransactionAmount to integer so it can be grouped 
library(stringr)
data$TransactionAmount <- str_sub(data$TransactionAmount, 2)

data$TransactionAmount <- gsub(",","",data$TransactionAmount)

data$TransactionAmount <- as.numeric((data$TransactionAmount))

sapply(data,class)
#   TransactionDate     TransactionAmount          Month_Yr             month 
#             "Date"         "numeric"             "numeric"         "integer"

sum(is.na(data))
#8571

#Count of Sales per month  -------

#Group by Month_Yr
library(data.table)
count_data <- as.data.table(data)

count_data[,count := .N,by="Month_Yr"]

#subset relevant variables
myvars <- c( "Month_Yr", "count")

count_data <- as.data.frame(count_data)

cdata <- count_data[myvars]

rm(myvars)

clean_cdata <- unique( cdata[ , 1:2])

clean_cdata <- clean_cdata[1:38,]

clean_cdata$month <- as.numeric(str_sub(clean_cdata$Month_Yr, -2,-1))

attach(clean_cdata)
library(tseries)

#linear regression
mv_count <- lm(count  ~ month + Month_Yr)

summary(mv_count)

#add forecasted values to the data frame
clean_cdata$fcast <- mv_count$fitted.values

#calculate accuracy
clean_cdata$accuracy <- (clean_cdata$fcast/clean_cdata$count)*100

#reorder columns
clean_cdata <- clean_cdata[c(1,3,2,4,5)]

clean_cdata_coeff <- as.data.frame(mv_count$coefficients)
colnames(clean_cdata_coeff) <- "model"
clean_cdata_coeff <- as.data.frame(t(clean_cdata_coeff))
