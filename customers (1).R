library(dplyr)
#Step 1 Loading data 
customers <- read.csv("C:/Users/user/Desktop/adatis case/customers.csv", header = TRUE, sep = ",", na.strings = c(""," ", "NA", "#NA"), stringsAsFactors = FALSE)
sales <- read.csv("C:/Users/user/Desktop/adatis case/sales.csv", header = TRUE, sep = ",", na.strings = c(""," ", "NA", "#NA"), stringsAsFactors = FALSE)
#Replacing n/a with 0 in column CreditLimit
customers$CreditLimit[is.na(customers$CreditLimit)] <- 0

max(sales$CustomerID)#1058

max(customers$п.їCustomerID)#1061
#removing customers without sales
customers <- subset(customers,customers$п.їCustomerID<1059)
colnames(customers)[colnames(customers)=='п.їCustomerID'] <-'CustomerID'
#combining datasets
dataEnd <- left_join(customers, sales, by = "CustomerID")
colnames(dataEnd)[colnames(dataEnd)=='п.їTransactionAmount'] <-'TransactionAmount'


sapply(dataEnd, class)
dataEnd$CustomerCategoryID <- as.factor(dataEnd$CustomerCategoryID)

#Linear Regression
ts <- lm(dataEnd$TransactionAmount ~ dataEnd$Year + dataEnd$Month + dataEnd$Day+ dataEnd$CreditLimit + dataEnd$CustomerCategoryID)
summary(ts)

dataEnd$fcast <- ts$fitted.values
dataEnd$accuracy <- (dataEnd$fcast/dataEnd$TransactionAmount)*100


