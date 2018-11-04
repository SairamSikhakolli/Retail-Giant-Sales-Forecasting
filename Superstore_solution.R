
# --------------------------------------Business Understanding ------------------------------------

# "Global Mart" is an online store super giant having worldwide operations. It takes orders and delivers across the globe 
# and deals with all the major product categories - consumer, corporate & home office.
# The store caters to 7 different market segments and in 3 major categories
# The company wants to finalise the plan for the next 6 months. So, they want to forecast the sales and the demand 
# for the next 6 months, that would help to manage the revenue and inventory accordingly.

# -----------------------------------Data Understanding ---------------------------------------
# The data currently has the transaction level data, where each row represents a particular order made on the 
# online store. There are 24 attributes related to each such transaction. 
# The "Market" attribute has 7-factor levels representing the geographical market sector that the customer belongs to. 
# The "Segment" attribute tells which of the 3 segments that customer belongs to.

#Loading the required libraries

library(tseries)
library(forecast)
library(graphics)
library(dplyr)
library(ggplot2)

#Reading the dataset in Superstore dataframe
Superstore<- read.csv("Global Superstore.csv")
View(Superstore)
is.na(Superstore)

##### 1. DATA CLEANING #####
which(duplicated(Superstore))                                           ## No duplicate values
which(duplicated(Superstore$Row.ID))                                    ## No duplicate values

sapply(Superstore, function(x) length(which(x=="")))                    ## No blank values

Superstore[which(is.na(Superstore)),]                                   ## NA values present
sapply(Superstore, function(x) sum(is.na(x)))                           ## Column Postal Code has NA values
#As per our analysis, we dont require the postal code so ignoring it.

Superstore$Order.Date<-as.Date(Superstore$Order.Date,"%d-%m-%Y")        ## Converting all values in Order.Date in same format
Superstore$Ship.Date<-as.Date(Superstore$Ship.Date,"%d-%m-%Y")          ## Converting all values in Ship.Date in same format
View(Superstore)
str(Superstore)

##### 2. DATA PREPARATION #####
levels(Superstore$Segment)                                              ## 3 levels in Segment
levels(Superstore$Market)                                               ## 7 levels in Market

#Here on we would only consider the required attributes for analysis and for the timeseries model.
#The below analysis is to find our the most profitable and consistent segments across markets.
## Creating Total Profit, Total sales, Average Sales,Sales Count  by Market and Segment
Profit_sum<- aggregate(Superstore$Profit,by=list(Superstore$Market,Superstore$Segment),FUN=sum)
names(Profit_sum)<-list("Market","Segment","Total_Profit")
View(Profit_sum)

Sales_sum<- aggregate(Superstore$Sales,by=list(Superstore$Market,Superstore$Segment),FUN=sum)
names(Sales_sum)<-list("Market","Segment","Total_Sales")
View(Sales_sum)

Sales_mean<- aggregate(Superstore$Sales,by=list(Superstore$Market,Superstore$Segment),FUN=mean)
names(Sales_mean)<-list("Market","Segment","Average_Sales")
View(Sales_mean)

Sales_count<- aggregate(Superstore$Sales,by=list(Superstore$Market,Superstore$Segment),FUN=length)
names(Sales_count)<-list("Market","Segment","Sales_Count")
View(Sales_count)

Total_sales_summary<-data.frame(Profit_sum,Sales_sum,Sales_mean,Sales_count) ## we can also use merge here
Total_sales_summary<-Total_sales_summary[,c(1,2,3,6,9,12)]                   ## Selecting relevant columns
Total_sales_summary$ProfitPercent<-(Total_sales_summary$Total_Profit/Total_sales_summary$Total_Sales)*100
View(Total_sales_summary)


## Creating Total Profit, Total Sales, Average Sales, Sales Count, Sales Qunatity by Month and Year
Profit_sum_month<- aggregate(Superstore$Profit,
                   by=list(Superstore$Market,Superstore$Segment,format(as.Date(Superstore$Order.Date),"%Y%m")),FUN=sum)
names(Profit_sum_month)<- list("Market","Segment","Month","Total_Profit")
View(Profit_sum_month)

Sales_sum_month<- aggregate(Superstore$Sales,
                             by=list(Superstore$Market,Superstore$Segment,format(as.Date(Superstore$Order.Date),"%Y%m")),FUN=sum)
names(Sales_sum_month)<- list("Market","Segment","Month","Total_Sales")
View(Sales_sum_month)

Sales_mean_month<- aggregate(Superstore$Sales,
                            by=list(Superstore$Market,Superstore$Segment,format(as.Date(Superstore$Order.Date),"%Y%m")),FUN=mean)
names(Sales_mean_month)<- list("Market","Segment","Month","Mean_Sales")
View(Sales_mean_month)

Sales_count_month<- aggregate(Superstore$Sales,
                             by=list(Superstore$Market,Superstore$Segment,format(as.Date(Superstore$Order.Date),"%Y%m")),FUN=length)
names(Sales_count_month)<- list("Market","Segment","Month","Count_Sales")
View(Sales_count_month)

Sales_quantity_month<- aggregate(Superstore$Quantity,
                              by=list(Superstore$Market,Superstore$Segment,format(as.Date(Superstore$Order.Date),"%Y%m")),FUN=sum)
names(Sales_quantity_month)<- list("Market","Segment","Month","Sum_Quantity")

Total_sales_month_summary<-data.frame(Profit_sum_month,Sales_sum_month,Sales_mean_month,Sales_count_month,Sales_quantity_month)
Total_sales_month_summary<-Total_sales_month_summary[,c(1,2,3,4,8,12,16,20)]
View(Total_sales_month_summary)

Total_sales_month_summary$ProfitPercentMonthly<-(Total_sales_month_summary$Total_Profit/Total_sales_month_summary$Total_Sales)*100
Total_sales_month_summary<-Total_sales_month_summary[order(Total_sales_month_summary$Total_Sales,decreasing= TRUE),]

y2<-aggregate(Total_sales_month_summary$ProfitPercentMonthly,by=list(Total_sales_month_summary$Market,Total_sales_month_summary$Segment),FUN=mean)
names(y2)<-list("Market","Segment","ProfitPercentMonthlyAverage")
View(y2)

#We find out the coefficient of consistency value which is the ratio of Standard Deviation to the mean.
sd<-aggregate(Total_sales_month_summary$ProfitPercentMonthly,by=list(Total_sales_month_summary$Market,Total_sales_month_summary$Segment),FUN=sd)
names(sd)<-list("Market","Segment","ProfitPercentMonthlySD")
View(sd)

## Adding ProfitPercent, ProfitpercentSd to Total_sales_summary

Total_sales_summary<-data.frame(Total_sales_summary,y2,sd)
Total_sales_summary<-Total_sales_summary[,-c(8,9,11,12)]
View(Total_sales_summary)

Total_sales_summary<-Total_sales_summary[order(Total_sales_summary$Total_Profit,decreasing=TRUE),]

Total_sales_summary$ConsistentMonthlyProfitPercent<-Total_sales_summary$ProfitPercentMonthlySD/Total_sales_summary$ProfitPercentMonthlyAverage

##### 3. Exploratory Data Analysis #####
##       To find 2 most profitable and consistently profitable segments

library(ggplot2)
plot_market<-ggplot(Total_sales_summary,aes(x=Total_sales_summary$Market,y=Total_sales_summary$Total_Profit,fill=Total_sales_summary$Segment))+
             geom_bar(stat="identity",position="dodge") 
plot_market  ## APAC and EU for CONSUMER with max. Total Profit

plot_segment<-ggplot(Total_sales_summary,aes(x=Total_sales_summary$Segment,y=Total_sales_summary$Total_Profit,fill=Total_sales_summary$Market))+
             geom_bar(stat="identity",position="dodge") 
plot_segment ## CONSUMER for APAC and EU with max. Total Profit

plot_consistent<-ggplot(Total_sales_summary,aes(x=Total_sales_summary$Segment,y=Total_sales_summary$ConsistentMonthlyProfitPercent,fill=Total_sales_summary$Market))+
  geom_bar(stat="identity",position="dodge")
plot_consistent ## APAC and EU are consistent also

plot_market2<-ggplot(Total_sales_summary,aes(x=Total_sales_summary$Market,y=Total_sales_summary$ProfitPercent,fill=Total_sales_summary$Segment))+
  geom_bar(stat="identity",position="dodge")
plot_market2  ## CANADA for CONSUMER, CORPORATE and HOME OFFICE with max. Profit Percent

##### 4. MODEL BUILDING #####
## APAC CONSUMER

## a. TAKING SUBSET FOR APAC CONSUMER
APAC_consumer          <- subset(Total_sales_month_summary,(Total_sales_month_summary$Market=="APAC")&(Total_sales_month_summary$Segment=="Consumer"))
APAC_consumer          <- APAC_consumer[order(APAC_consumer$Month),]
APAC_consumer$MonthNum <- c(1:nrow(APAC_consumer))
View(APAC_consumer)

APAC_consumer_sales    <- APAC_consumer[,c("MonthNum","Total_Sales")] 
APAC_consumer_quantity <- APAC_consumer[,c("MonthNum","Sum_Quantity")]

## b. CREATING TRAINING AND TEST DATA FOR SALES AND QUANTITY
APAC_salesTrain        <- APAC_consumer_sales[c(1:(nrow(APAC_consumer_sales)-6)),]                                  ## Creating train data for sales
APAC_salesTest         <- APAC_consumer_sales[c((nrow(APAC_consumer_sales)-5):nrow(APAC_consumer_sales)),]          ## Creating test data for sales
APAC_quantityTrain     <- APAC_consumer_quantity[c(1:(nrow(APAC_consumer_quantity)-6)),]                            ## Creating train data for quantity
APAC_quantityTest      <- APAC_consumer_quantity[c((nrow(APAC_consumer_quantity)-5):nrow(APAC_consumer_quantity)),] ## Creating test data for quantity
View(APAC_salesTrain)
View(APAC_salesTest)
View(APAC_quantityTrain)
View(APAC_quantityTest)

## c. CREATING TIME SERIES # APAC SALES
APAC_salesTS <- ts(APAC_salesTrain[,2])                                                   ## Creating Time Series for Sales
plot(APAC_salesTS)                                                                        ## Plotting Time Series for Sales
plot(decompose(ts(APAC_salesTrain[,2],frequency=4)))                                      
#Clear visibility in increasing trend and pattern in seasonality

## d. SMOOTHENING 
w <- 1
APAC_sales_TSSmooth <- stats::filter(APAC_salesTS, 
                                     filter=rep(1/(2*w+1),(2*w+1)), 
                                     method='convolution', sides=2)                       ## Moving Average Smoothening

difference <- APAC_sales_TSSmooth[w+2] - APAC_sales_TSSmooth[w+1]
for (i in seq(w,1,-1)) {
  APAC_sales_TSSmooth[i] <- APAC_sales_TSSmooth[i+1] - difference}                        ## Left End Smoothening

n <- length(APAC_salesTS)
difference <- APAC_sales_TSSmooth[n-w] - APAC_sales_TSSmooth[n-w-1]
for (i in seq(n-w+1, n)) {
  APAC_sales_TSSmooth[i] <- APAC_sales_TSSmooth[i-1] + difference}                        ## Right End Smoothening

plot(APAC_salesTS)
lines(APAC_sales_TSSmooth, col="blue", lwd=2)                                             ## Smoothed Time Series


#Building a model on the smoothed time series using classical decomposition
## e) CLASSICAL DECOMPOSITION 
#  e.1) Preparing the data frame 
timevals <- APAC_salesTrain$MonthNum
APAC_sales_TSSmooth_df <- as.data.frame(cbind(timevals, as.vector(APAC_sales_TSSmooth)))
colnames(APAC_sales_TSSmooth_df) <- c('Month', 'Sales')

APAC_sales_TSSmooth_df$Month<-as.numeric(APAC_sales_TSSmooth_df$Month)
APAC_sales_TSSmooth_df$Sales<-as.numeric(APAC_sales_TSSmooth_df$Sales)
View(APAC_sales_TSSmooth_df)

# e.2) Fitting model with trend and seasonality to the data using sinusoid function
lmfit <- lm(Sales ~ sin(0.5*Month) * poly(Month,3)+ cos(0.5*Month) * poly(Month,3) + Month, 
            data=APAC_sales_TSSmooth_df)
summary(lmfit)
accuracy(lmfit)

# e.3) Predicting the global part based on the above model
global_pred <- predict(lmfit, Month=timevals)
summary(global_pred)
lines(timevals, global_pred, col="red", lwd=2)

# e.4) Finding out the local part.
local_pred <- APAC_salesTS - global_pred
plot(local_pred, col='red', type = "l")
acf(local_pred)
acf(local_pred, type="partial")

armafit <- auto.arima(local_pred)
tsdiag(armafit)
armafit

# e.5) Checking if the residual series is white noise
residual <- local_pred-fitted(armafit)
adf.test(residual,alternative = "stationary") 
## p value is 0.01 which is <0.05, so we reject the NULL hypothesis.
# This implies the series is stationary i,e. pure noise
kpss.test(residual)                            
## p value is 0.1 which is >0.05, so we fail to reject the NULL hypothesis.
#In this case, NULL hypothesis is series is stationary.
# This implies the series is stationary i,e. pure noise

# e.6) Evaluating the model using MAPE
timevals.test <- APAC_salesTest$MonthNum
global_pred_out <- predict(lmfit,data.frame(Month=timevals.test))
fcast <- global_pred_out
View(timevals.test)
View(global_pred_out)

# e.7) Comparing the predicted values with the actual values using MAPE
MAPE_class_dec <- accuracy(fcast,APAC_salesTest$Total_Sales)[5]
MAPE_class_dec
#31.07429      
#The value for MAPE if low resulting in a better model.

# e.8) Plotting the predictions wrt original values
class_pred <- c(ts(global_pred),ts(global_pred_out))
plot(APAC_salesTS, col = "black")
lines(class_pred, col = "red")
#As seen in the plot, the actual and predicted are overlying with clear trend and seasonality in it.

#Forecasting APAC sales using classical decomposition.
APAC_Sales_nxt6mths <- predict(lmfit,data.frame(Month=seq(1:54)))[49:54]
APAC_Sales_nxt6mths
## f) Lets use AUTO ARIMA
APAC_autoarima <- auto.arima(APAC_salesTS)
APAC_autoarima
par(mar=c(1,1,1,1))
tsdiag(APAC_autoarima)
View(APAC_salesTS)

# f.1) Plotting the values
plot(APAC_autoarima$x, col="black")
lines(fitted(APAC_autoarima), col="red")
accuracy(APAC_autoarima)

# f.2) Check if the residual series is white noise
resi_autoarima <- APAC_salesTS - fitted(APAC_autoarima)
adf.test(resi_autoarima,alternative = "stationary")
#Based on the test, it is pure noise
kpss.test(resi_autoarima)
#Based on the test, the residual is pure white noise.

# f.3) Evaluating the model using MAPE
fcast_autoarima <- predict(APAC_autoarima, n.ahead = 6)
MAPE_autoarima <- accuracy(fcast_autoarima$pred,APAC_salesTest$Total_Sales)[5]
MAPE_autoarima 
# 27.68952    
# Lower the MAPE, better the model.
# This is low than the value we got in Classical Decomposition.
View(APAC_salesTest)

# f.4) Plotting the predictions wrt original values
autoarima_pred <- c(fitted(APAC_autoarima),ts(fcast_autoarima$pred))
plot(ts(APAC_consumer_sales$Total_Sales), col = "black")
lines(autoarima_pred, col = "green")
##As seen in the plot, the actual and predicted are overlying with clear trend and seasonality in it.

# f.5) Forecasting for next 6 months.
fcast_auto_arima_Nx6mths_APAC_Sales <- predict(APAC_autoarima, n.ahead = 12)$pred[7:12]
fcast_auto_arima_Nx6mths_APAC_Sales

# Conclusion:
# Auto ARIMA is better than Classical Decomposition for APAC Consumer for SALES


## APAC CONSUMER FOR QUANTITY
# a) Creating and plotting time series for Quantity
APAC_Qty_TS <- ts(APAC_quantityTrain[,2])
plot(APAC_Qty_TS)

# b) Decompose timeseries to see the components using decompose function
plot(decompose(ts(APAC_quantityTrain[,2],frequency=12)))
# Decomposotion shows trend and seasonlaity.

## c) Smoothening the time series 

w <-1
APAC_Qty_TSSmooth <- stats::filter(APAC_Qty_TS,filter=rep(1/(2*w+1),(2*w+1)), 
                                   method='convolution', sides=2)              ## Moving Average Smoothing

difference <- APAC_Qty_TSSmooth[w+2] - APAC_Qty_TSSmooth[w+1]                  ## Left Hand Smoothening
for (i in seq(w,1,-1)) {
  APAC_Qty_TSSmooth[i] <- APAC_Qty_TSSmooth[i+1] - difference
}

timevals <- APAC_quantityTrain$MonthNum
timevals.test <- APAC_quantityTest$MonthNum

n <- length(APAC_Qty_TS)
difference <- APAC_Qty_TSSmooth[n-w] - APAC_Qty_TSSmooth[n-w-1]                ## Right Hand Smoothening
for (i in seq(n-w+1, n)) {
  APAC_Qty_TSSmooth[i] <- APAC_Qty_TSSmooth[i-1] + difference
}

plot(APAC_Qty_TS)
lines(APAC_Qty_TSSmooth, col="blue", lwd=2)                                    ## Plotting Smoothened Time Series


## d) CLASSICAL DECOMPOSITION
# Building a model on the smoothed time series using classical decomposition
# d.1) Converting the time series to a dataframe
APAC_Qty_TSSmooth_df           <- as.data.frame(cbind(timevals, as.vector(APAC_Qty_TSSmooth)))
colnames(APAC_Qty_TSSmooth_df) <- c('Month', 'Quantity')
View(APAC_Qty_TSSmooth_df)
View(timevals)
APAC_Qty_TSSmooth_df$Month<-as.numeric(APAC_Qty_TSSmooth_df$Month)
APAC_Qty_TSSmooth_df$Quantity<-as.numeric(APAC_Qty_TSSmooth_df$Quantity)

# d.2) Fitting multiplicative model with trend and seasonality to the data using sinusoid function
lmfit <- lm(Quantity ~ sin(0.6*Month) * poly(Month,2)+cos(0.6*Month) * poly(Month,2) + sin(0.05*Month) * Month, 
            data=APAC_Qty_TSSmooth_df)
summary(lmfit)

# d.3) Predicting the global part based on the above model
global_pred <- predict(lmfit, data.frame(Month=timevals))
lines(timevals, global_pred, col="red", lwd=2)

# d.4) Finding out the local part.
local_pred <- APAC_Qty_TS - global_pred
plot(local_pred, col='red')

acf(local_pred)
acf(local_pred, type="partial")

armafit <- auto.arima(local_pred)
par(mar=c(1,1,1,1))
tsdiag(armafit)
armafit
# d.5) Checking if the residual series is white noise
residual <- local_pred-fitted(armafit)

adf.test(residual,alternative = "stationary")
## p value is 0.01 which is <0.05, so we reject the NULL hypothesis.
# This implies the series is stationary i,e. pure noise

kpss.test(residual)
## p value is 0.1 which is >0.05, so we fail to reject the NULL hypothesis.
#In this case, NULL hypothesis is series is stationary.
# This implies the series is stationary i,e. pure noise

# d.6) Evaluating the model using MAPE
timevals.test <- APAC_quantityTest$MonthNum
global_pred_out <- predict(lmfit,data.frame(Month=timevals.test))
fcast <- global_pred_out

# d.7) Comparing the predicted values with the actual values using MAPE
MAPE_class_dec <- accuracy(fcast,APAC_quantityTest$Sum_Quantity)[5]
MAPE_class_dec
#31.76748
#The value for MAPE if low resulting in a better model.
View(APAC_quantityTest)

# d.8) Plotting the predictions wrt original values
class_dec_pred <- c(ts(global_pred),ts(global_pred_out))
plot(ts(APAC_consumer_quantity$Sum_Quantity), col = "black")
lines(class_dec_pred, col = "red")
#As seen in the plot, the actual and predicted are overlying with clear trend and seasonality in it.

#Forecasting APAC sales using classical decomposition.
APAC_Demand_nxt6mths <- predict(lmfit,data.frame(Month=seq(1:54)))[49:54]

## e) Lets use AUTO ARIMA now and try to forecast.
APAC_autoarima <- auto.arima(APAC_Qty_TS)
APAC_autoarima
par(mar=c(1,1,1,1))
tsdiag(APAC_autoarima)

# e.1) Plotting the values
plot(APAC_autoarima$x, col="black")
lines(fitted(APAC_autoarima), col="red")

# e.2) Check if the residual series is white noise
resi_autoarima <- APAC_Qty_TS - fitted(APAC_autoarima)

adf.test(resi_autoarima,alternative = "stationary")
#Based on the test, it is pure noise
kpss.test(resi_autoarima)
#Based on the test, the residual is pure white noise.

# e.3) Evaluating the model using MAPE
fcast_autoarima <- predict(APAC_autoarima, n.ahead = 6)

MAPE_autoarima <- accuracy(fcast_autoarima$pred,APAC_quantityTest$Sum_Quantity)[5]
MAPE_autoarima
#26.24458
# Lower the MAPE, better the model.
# This is low than the value we got in Classical Decomposition.

# e.4) Plotting the predictions wrt original values
autoarima_pred <- c(fitted(APAC_autoarima),ts(fcast_autoarima$pred))

plot(ts(APAC_consumer_quantity$Sum_Quantity), col = "black")
lines(autoarima_pred, col = "green")

# e.5) Forecasting for next 6 months.
fcast_auto_arima_Nx6mths_APAC_Demand <- predict(APAC_autoarima, n.ahead = 12)$pred[7:12]
fcast_auto_arima_Nx6mths_APAC_Demand

# Conclusion:
# Auto ARIMA is better than Classical Decomposition for APAC Consumer for Quantity

#*************************************************************************************************************************
## EU CONSUMER

## a. TAKING SUBSET FOR EU CONSUMER  
EU_consumer          <- subset(Total_sales_month_summary,(Total_sales_month_summary$Market=="EU")&(Total_sales_month_summary$Segment=="Consumer"))
EU_consumer          <- EU_consumer[order(EU_consumer$Month),]
EU_consumer$MonthNum <- c(1:nrow(EU_consumer))
View(EU_consumer)

EU_consumer_Sales    <- EU_consumer[,c("MonthNum","Total_Sales")] 
EU_consumer_quantity <- EU_consumer[,c("MonthNum","Sum_Quantity")]

## b. CREATING TRAINING AND TEST DATA FOR SALES AND QUANTITY
EU_SalesTrain        <- EU_consumer_Sales[c(1:(nrow(EU_consumer_Sales)-6)),]                                    ## Creating train data for sales
EU_SalesTest         <- EU_consumer_Sales[c((nrow(EU_consumer_Sales)-5):nrow(EU_consumer_Sales)),]              ## Creating test data for sales
EU_quantityTrain     <- EU_consumer_quantity[c(1:(nrow(EU_consumer_quantity)-6)),]                              ## Creating train data for quantity
EU_quantityTest      <- EU_consumer_quantity[c((nrow(EU_consumer_quantity)-5):nrow(EU_consumer_quantity)),]     ## Creating test data for quantity

## c. CREATING TIME SERIES #EU sales
EU_salesTS <- ts(EU_SalesTrain[,2])                                                   ## Creating Time Series for Sales
plot(EU_salesTS)                                                                      ## Plotting Time Series for Sales
plot(decompose(ts(EU_SalesTrain[,2],frequency=4)))                                      

# Decomposotion shows:
# 1. Trend is a high wavelength sine curve
# 2. Seasonality is a low wavelength sine curve

## d. SMOOTHENING 
w <- 1
EU_sales_TSSmooth <- stats::filter(EU_salesTS, 
                                     filter=rep(1/(2*w+1),(2*w+1)), 
                                     method='convolution', sides=2)                       ## Moving Average Smoothening

difference <- EU_sales_TSSmooth[w+2] - APAC_sales_TSSmooth[w+1]
for (i in seq(w,1,-1)) {
  EU_sales_TSSmooth[i] <- EU_sales_TSSmooth[i+1] - difference}                            ## Left End Smoothening

n <- length(EU_salesTS)
difference <- EU_sales_TSSmooth[n-w] - EU_sales_TSSmooth[n-w-1]
for (i in seq(n-w+1, n)) {
  EU_sales_TSSmooth[i] <- EU_sales_TSSmooth[i-1] + difference}                            ## Right End Smoothening

plot(EU_salesTS)
lines(EU_sales_TSSmooth, col="blue", lwd=2)                                               ## Smoothed Time Series

# Building a model on the smoothed time series using classical decomposition
## e) CLASSICAL DECOMPOSITION 
#  e.1) Converting the time series to a dataframe
timevals <- EU_SalesTrain$MonthNum
EU_sales_TSSmooth_df <- as.data.frame(cbind(timevals, as.vector(EU_sales_TSSmooth)))
colnames(EU_sales_TSSmooth_df) <- c('Month', 'Sales')

EU_sales_TSSmooth_df$Month<-as.numeric(EU_sales_TSSmooth_df$Month)
EU_sales_TSSmooth_df$Sales<-as.numeric(EU_sales_TSSmooth_df$Sales)
View(EU_sales_TSSmooth_df)

# e.2) Fitting multiplicative model with trend and seasonality to the data using sinusoid function
lmfit <- lm(Sales ~ sin(0.5*Month) * poly(Month,2)+ cos(0.5*Month) * poly(Month,2) + Month, 
            data=EU_sales_TSSmooth_df)
summary(lmfit)
accuracy(lmfit)

# e.3) Predicting the global part based on the above model
global_pred <- predict(lmfit, data.frame(Month=timevals))
summary(global_pred)
lines(timevals, global_pred, col="red", lwd=2)

# e.4) Finding out the local part
local_pred <- EU_salesTS - global_pred
plot(local_pred, col='red', type = "l")
acf(local_pred)
acf(local_pred, type="partial")

armafit <- auto.arima(local_pred)
tsdiag(armafit)
armafit

# e.5) Checking if the residual series is white noise
residual <- local_pred-fitted(armafit)
adf.test(residual,alternative = "stationary")  
## p value is 0.01 which is <0.05, so we reject the NULL hypothesis.
# This implies the series is stationary i,e. pure noise
kpss.test(residual)                          
## p value is 0.1 which is >0.05, so we fail to reject the NULL hypothesis.
#In this case, NULL hypothesis is series is stationary.
# This implies the series is stationary i,e. pure noise

# e.6) Evaluating the model using MAPE
timevals.test <- EU_SalesTest$MonthNum
global_pred_out <- predict(lmfit,data.frame(Month=timevals.test))
fcast <- global_pred_out
View(timevals.test)
View(global_pred_out)

# e.7) Comparing the predicted values with the actual values using MAPE
MAPE_class_dec <- accuracy(fcast,EU_SalesTest$Total_Sales)[5]
MAPE_class_dec
#32.74
View(EU_SalesTest)

# e.8) Plotting the predictions wrt original values
class_pred <- c(ts(global_pred),ts(global_pred_out))
plot(EU_salesTS, col = "black")
lines(class_pred, col = "red")

EU_Sales_nxt6mths <- predict(lmfit,data.frame(Month=seq(1:54)))[49:54]

## f) AUTO ARIMA
EU_autoarima <- auto.arima(EU_salesTS)
EU_autoarima
par(mar=c(1,1,1,1))
tsdiag(EU_autoarima)
View(EU_salesTS)

# f.1) Plotting the values
plot(EU_autoarima$x, col="black")
lines(fitted(EU_autoarima), col="red")
accuracy(EU_autoarima)

# f.2) Check if the residual series is white noise
resi_autoarima <- EU_salesTS - fitted(EU_autoarima)
adf.test(resi_autoarima,alternative = "stationary")   
#Based on the test, it is pure noise
kpss.test(resi_autoarima)                             
#Based on the test, the residual is pure white noise.

# f.3) Evaluating the model using MAPE
fcast_autoarima <- predict(EU_autoarima, n.ahead = 6)
MAPE_autoarima  <- accuracy(fcast_autoarima$pred,EU_SalesTest$Total_Sales)[5]
MAPE_autoarima 
# 28.9226   
# Lower the MAPE, better the model.
# This is low than the value we got in Classical Decomposition.
View(EU_salesTest)

# f.4) Plotting the predictions wrt original values
autoarima_pred <- c(fitted(EU_autoarima),ts(fcast_autoarima$pred))
plot(ts(EU_consumer_Sales$Total_Sales), col = "black")
lines(autoarima_pred, col = "green")

# f.5) Forecasting for next 6 months.
fcast_auto_arima_Nx6mths_EU_Sales <- predict(EU_autoarima, n.ahead = 12)$pred[7:12]
fcast_auto_arima_Nx6mths_EU_Sales

# Conclusion:
# Auto ARIMA is better than Classical Decomposition for EU Consumer for Sales

## EU CONSUMER FOR QUANTITY
# a) Creating and plotting time series for Quantity
EU_Qty_TS <- ts(EU_quantityTrain[,2])
plot(EU_Qty_TS)

# b) Decompose timeseries to see the components using decompose function at frequency = 12
plot(decompose(ts(EU_quantityTrain[,2],frequency=12)))

# Decomposotion shows
# 1. Trend in a linear positive slope line
# 2. Seasonality is a low wavelength sine curve

## c) Smoothening the time series 

w <-1
EU_Qty_TSSmooth <- stats::filter(EU_Qty_TS,filter=rep(1/(2*w+1),(2*w+1)), 
                                   method='convolution', sides=2)              ## Moving Average Smoothing

difference <- EU_Qty_TSSmooth[w+2] - EU_Qty_TSSmooth[w+1]                      ## Left Hand Smoothening
for (i in seq(w,1,-1)) {
  EU_Qty_TSSmooth[i] <- EU_Qty_TSSmooth[i+1] - difference
}

timevals      <- EU_quantityTrain$MonthNum
timevals.test <- EU_quantityTest$MonthNum

n <- length(EU_Qty_TS)
difference <- EU_Qty_TSSmooth[n-w] - EU_Qty_TSSmooth[n-w-1]                    ## Right Hand Smoothening
for (i in seq(n-w+1, n)) {
  EU_Qty_TSSmooth[i] <- EU_Qty_TSSmooth[i-1] + difference
}

plot(EU_Qty_TS)
lines(EU_Qty_TSSmooth, col="blue", lwd=2)                                      ## Plotting Smoothened Time Series


## d) CLASSICAL DECOMPOSITION
# Building a model on the smoothed time series using classical decomposition
# d.1) Converting the time series to a dataframe
EU_Qty_TSSmooth_df           <- as.data.frame(cbind(timevals, as.vector(EU_Qty_TSSmooth)))
colnames(EU_Qty_TSSmooth_df) <- c('Month', 'Quantity')
View(EU_Qty_TSSmooth_df)
View(timevals)
EU_Qty_TSSmooth_df$Month<-as.numeric(EU_Qty_TSSmooth_df$Month)
EU_Qty_TSSmooth_df$Quantity<-as.numeric(EU_Qty_TSSmooth_df$Quantity)

# d.2) Fitting multiplicative model with trend and seasonality to the data using sinusoid function
lmfit <- lm(Quantity ~ sin(0.6*Month) * poly(Month,3)+cos(0.6*Month) * poly(Month,3) + sin(0.05*Month) * Month, 
            data=EU_Qty_TSSmooth_df)
summary(lmfit)

# d.3) Predicting the global part based on the above model
global_pred <- predict(lmfit, data.frame(Month=timevals))
lines(timevals, global_pred, col="red", lwd=2)

# d.4) Finding out the local part.
local_pred <- EU_Qty_TS - global_pred
plot(local_pred, col='red')

acf(local_pred)
acf(local_pred, type="partial")

armafit <- auto.arima(local_pred)
par(mar=c(1,1,1,1))
tsdiag(armafit)
armafit
# d.5) Checking if the residual series is white noise
residual <- local_pred-fitted(armafit)

adf.test(residual,alternative = "stationary")
## p value is 0.01 which is <0.05, so we reject the NULL hypothesis.
# This implies the series is stationary i,e. pure noise
kpss.test(residual)
## p value is 0.1 which is >0.05, so we fail to reject the NULL hypothesis.
#In this case, NULL hypothesis is series is stationary.
# This implies the series is stationary i,e. pure noise

# d.6) Evaluating the model using MAPE
timevals.test <- EU_quantityTest$MonthNum
global_pred_out <- predict(lmfit,data.frame(Month=timevals.test))
fcast <- global_pred_out

# d.7) Comparing the predicted values with the actual values using MAPE
MAPE_class_dec <- accuracy(fcast,EU_quantityTest$Sum_Quantity)[5]
MAPE_class_dec
#126.72
View(EU_quantityTest)

# d.8) Plotting the predictions wrt original values
class_dec_pred <- c(ts(global_pred),ts(global_pred_out))
plot(ts(EU_consumer_quantity$Sum_Quantity), col = "black")
lines(class_dec_pred, col = "red")

#forecast for next 6 months
EU_Demand_nxt6mths <- predict(lmfit,data.frame(Month=seq(1:54)))[49:54]

## e) AUTO ARIMA
EU_autoarima <- auto.arima(EU_Qty_TS)
EU_autoarima
par(mar=c(1,1,1,1))
tsdiag(EU_autoarima)

# e.1) Plotting the values
plot(EU_autoarima$x, col="black")
lines(fitted(EU_autoarima), col="red")

# e.2) Check if the residual series is white noise
resi_autoarima <- EU_Qty_TS - fitted(EU_autoarima)

adf.test(resi_autoarima,alternative = "stationary")
#p value is 0.045, hence it is stationary
kpss.test(resi_autoarima)
#Based on the test, the residual is pure white noise.

# e.3) Evaluating the model using MAPE
fcast_autoarima <- predict(EU_autoarima, n.ahead = 6)

MAPE_autoarima <- accuracy(fcast_autoarima$pred,EU_quantityTest$Sum_Quantity)[5]
MAPE_autoarima
#30.13
#This is better than the value we got for Classical Decomposition

# e.4) Plotting the predictions wrt original values
autoarima_pred <- c(fitted(EU_autoarima),ts(fcast_autoarima$pred))

plot(ts(EU_consumer_quantity$Sum_Quantity), col = "black")
lines(autoarima_pred, col = "green")

# e.5) Prediction for next 6 months.
fcast_auto_arima_Nx6mths_EU_Demand <- predict(EU_autoarima, n.ahead = 12)$pred[7:12]
fcast_auto_arima_Nx6mths_EU_Demand

# Conclusion:
# Auto ARIMA is better than Classical Decomposition for EU Consumer for Quantity



          
