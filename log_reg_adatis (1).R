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
#TTransactionDate TransactionAmount Month_Yr   Currency         month 
#"Date"         "numeric"          "numeric"   "factor"         "numeric" 

#Fix TransactionAmount
#change TransactionAmount to integer so it can be grouped 
library(stringr)
data$Currency <- as.factor("GBP")

data$TransactionAmount <- str_sub(data$TransactionAmount, 2)

data$TransactionAmount <- gsub(",","",data$TransactionAmount)

data$TransactionAmount <- as.numeric((data$TransactionAmount))

sapply(data,class)
#  TransactionDate TransactionAmount          Month_Yr          Currency 
#"Date"         "numeric"       "character"          "factor" 

sum(is.na(data))
#8571

#Aggregate total TransactionAmount per month ------
total_month_group <- aggregate(data$TransactionAmount, by=list(data$Month_Yr), sum)

#Rename column names
names(total_month_group) <- c("Month_Yr", "TransactionAmount")

library(stringr)

total_month_group$month <- as.numeric(str_sub(total_month_group$Month_Yr, -2,-1))

attach(total_month_group)

#Build a linear regression model 

ts <- lm(TransactionAmount ~ month + Month_Yr)

summary(ts)

total_month_group$fcast <- ts$fitted.values

total_month_group$accuracy <- (total_month_group$fcast/total_month_group$TransactionAmount)*100

total_month_group <- total_month_group[c(1,3,2,4,5)]

#Build Arima Model ------

library(tseries)
ts.plot(TransactionAmount/10000)

#Check whether data is stationary 

adf.test(total_month_group$TransactionAmount)
#data:  total_month_group$TransactionAmount
##alternative hypothesis: stationary

#Diff data 
diff_TAmount <- diff(total_month_group$TransactionAmount)

diff_TAmount <- data.frame(diff(total_month_group$TransactionAmount))

plot(diff_TAmount[,1])

layout(matrix(c(1:4),nrow=2,ncol=2, byrow = TRUE))
acf(diff_TAmount, lag.max = 50)
pacf(diff_TAmount, lag.max = 50)

#build arima model

library(forecast)

train <- diff_TAmount[1:30,]
test <- diff_TAmount[30:35,]

auto.arima(train)
#Series: diff_TAmount 
#ARIMA(0,0,0) with zero mean 
#sigma^2 estimated as 5.034e+16:  log likelihood=-763.97
#AIC=1529.93   AICc=1530.04   BIC=1531.54

mm=list()
mm[[1]]=arima(train,order=c(0,0,0))


mm[[1]]$aic
layout(matrix(c(1:3),nrow=1,ncol=3))
acf(mm[[1]]$residuals,lag.max=30)
Box.test(mm[[1]]$residuals, type="Ljung-Box", lag=20)

fit<-auto.arima(train)

pred = predict(fit, n.ahead = 6)

total_month_group_coeff <- as.data.frame(ts$coefficients)
colnames(total_month_group_coeff) <- "model"
total_month_group_coeff <- as.data.frame(t(total_month_group_coeff))
