share_price_data=sharepricedata_2023_
head(share_price_data,10)
tail(share_price_data,10)

#plot the original data
share_data=ts(share_price_data$Value,start=c(2010,01,01),end=c(2022,12,01),
              frequency = 12)

#plot data to check the presence of trends
plot.ts(share_data,xlab="share_price_data$year",
        ylab="share_price_data$Value",
        main="share price data",type="l",col="red")
nsdiffs(share_data)
ndiffs(share_data)
#plot acf and pacf
acf(share_data,lag.max = 50)
pacf(share_data,lag.max = 50)

#test for stationarity of the original data
#H0: THE DATA HAS A UNIT ROOT i.e IS NOT STATIONARY
#H1: THE DATA HAS NO UNIT ROOT i.e IS STATIONARY
#_________________________________________________
#IF THE P-VALUE IS LESS THAN 0.05 HO IS REJECTED
#IF THE P-VALUE IS GREATER THAN 0.05 H0 IS ACCEPTED
#___________________________________________________
#        OR
#IF THE STATISTIC VALUE IS SIGNIFICANTLY NEGETIVE,THE
#DATA HAS NO UNIT ROOT AND THUS IS STATIONARY
#ELSE IS NON-STATIONARY
#____________________________________________________
#the p value is greater than 0.05
#the data is not startionary at 5% level of significance
adf.test(share_data)
p.value=adf.test(share_data)$p.value
if(p.value>0.05){
  print("The data is not stationary")
}else{
  print("The data is stationary")
}

#apply log to the data
share_log_data=diff(share_data)
plot.ts(share_log_data,xlab="year",ylab="log values",
        main="log data")

#plot acf and pacf of the logged values
acf(share_log_data,lag.max=300)
pacf(share_log_data,lag.max = 300)

#test stationarity from the data
library(tseries)
library(aTSA)
adf.test(share_log_data)
p.value1=adf.test(share_log_data)$p.value
if(p.value1>0.05){
  print("The data is not stationary")
}else{
  print("The data is stationary")
}

#taking the difference of the logged data
nsdiffs(share_log_data)
share_diff_log_data=diff(share_log_data,differences = 2)

#plot the acf and pacf of the differenced logged data
plot.ts(share_diff_log_data,xlab="year",ylab="diff log values",
        main="Differenced log values",col="red")

#plot acf and pacf
acf(share_diff_log_data,lag.max=300)
pacf(share_diff_log_data,lag.max=300)

#test for stationarity
adf.test(share_diff_log_data)
p.value2=adf.test(share_diff_log_data)$p.value
if(p.value1>0.05){
  print("The data is not stationary")
}else{
  print("The data is stationary")
}

#determine the model
sarima_model = auto.arima(share_diff_log_data,allowdrift = FALSE,trace=TRUE)
plot(fitted(sarima_model))
plot(sarima_model$residuals)
summary(sarima_model)

acf(sarima_model)
###############
library(forecast)
arima
sarima_model0=arima(share_data,order =c(1,0,1),seasonal = list(order=c(0,0,1),period(12)))
plot(sarima_model0)
accuracy(sarima_model)
forecast_model=forecast(sarima_model0,100)
library(tseries)
library(“forecast”)
library(fgarch)
setwd("C:/Users/Desktop") # Setting of the work directory
data<-read.table("data.txt") # Importing data
datats<-ts(share_price_data$Value,frequency=12,start=c(1960,1)) # Converting data set into time series
plot.ts(datats) # Plot of the data set
adf.test(datats) # Test for stationarity
diffdatats<-diff(datats,differences=1) # Differencing the series
datatsacf<-acf(datats,lag.max=12) # Obtaining the ACF plot
datapacf<-pacf(datats,lag.max=12) # Obtaining the PACF plot
auto.arima(diffdatats) # Finding the order of ARIMA model
datatsarima<-arima(diffdatats,order=c(1,0,1),include.mean=TRUE) # Fitting of ARIMA model
forearimadatats<-forecast.Arima(datatsarima,h=12) # Forecasting using ARIMA model
plot.forecast(forearimadatats) # Plot of the forecast
residualarima<-resid(datatsarima) # Obtaining residuals
archTest(residualarima,lag=12) # Test for heteroscedascity
# Fitting of ARIMA-GARCH model
garchdatats<-garchFit(formula = ~ arma(2)+garch(1, 1), data = datats, cond.dist = c("norm"), include.mean = TRUE, include.delta = NULL, include.skew = NULL, include.shape = NULL, leverage = NULL, trace = TRUE,algorithm = c("nlminb"))
# Forecasting using ARIMA-GARCH model
forecastgarch<-predict(garchdatats, n.ahead = 12, trace = FALSE, mse = c("uncond"), plot=FALSE, nx=NULL, crit_val=NULL, conf=NULL)
plot.ts(forecastgarch) # Plot of the forecast