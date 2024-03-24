library(readxl)
library(forecast)

# Importing the original data from excel

sharepricedata_2023_ <- read_excel("RESEARCH DATA SET/sharepricedata (2023).xlsx", 
                                   col_types = c("text", "text", "text", 
                                                 "text", "text", "date", "numeric"))
View(sharepricedata_2023_)

#changing the name of the data
time_series_data=sharepricedata_2023_
head(time_series_data) #plotting the first 6 rows

#converting the data into a time series data using ts
time_series=ts(time_series_data$Value,frequency = 12,start=c(1960,01,01),
               end=c(2022,12,01))

#check if the data was converted to a tie series data
class(time_series)

#check when it starts
start(time_series)
#check when it ends
end(time_series)

#plot the data with ACF and PACF
plot.ts(time_series,xlab="Year",ylab="Value",
            main="share price data",col="blue",frame=FALSE)

#plot the ACF and PACf of the original data
acf(time_series)
pacf(time_series)

#plot trend on the origal data
plot.ts(time_series,xlab="Year",ylab="Value",
        main="share price data",col="blue",frame=FALSE)
abline(reg=lm(time_series~time(time_series)))

#decompose the data
plot(decompose(time_series)$trend)
plot(decompose(time_series)$seasonal)
plot(decompose(time_series)$random)



#test for stationarity of the original data using ADF
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

library(tseries)
adf.test(time_series)
kpss.test(time_series)

boxplot(time_series~cycle(time_series,xlab="Year",ylab="Value",
                          main="Box plots"))
hist(time_series)
library(outliers)
outlier(time_series)
scatter.smooth(time_series,type="l")

#transform the data
log_data=log(time_series)

#plot the log data
plot(log_data)

# The problem is with the stationarity
adf.test(log_data)

#differenced sqrt data
ndiffs(log_data)
# d=2, D=0

diff_log_data=diff(log_data,differences = 1)
acf(diff_log_data,lag.max = 100)
pacf(diff_log_data,lag.max = 100)

adf.test(diff_log_data)

#model determination using auto arima
model1=auto.arima(time_series,allowdrift = FALSE,trace=TRUE)

summary(model1)

# residuals of SARIMA(5,2,0)
model1_resduals=model1$residuals
#plot the residuals of the model
plot.ts(model1_resduals)
#plot the acf and pacf of the model
acf(model1_resduals)
pacf(model1_resduals)
#plot the fitted
plot(model1$fitted)

acf(model1$fitted)
pacf(model1$fitted)

library(aTSA)
myforecasting=forecast::forecast(model1,h=2*12)
plot(myforecasting)
   lines(model1$fitted,col="red")

Box.test(model1_resduals)

##################################################################################
library(caTools)
train_data=diff_log_data[1:604]
set.seed(123)   

arima_model1=auto.arima(train_data,stationary = TRUE,
                        ic=c("aicc"),trace=TRUE)   

summary(arima_model1)
checkresiduals(arima_model1)

ggtsdisplay(arima_model1$residuals)
arima=arima(train_data,order=c(0,0,1))
summary(arima)

forecast=forecast::forecast(arima,h=24)
plot(forecast)

checkresiduals(arima)

##########################################################################

library(readxl)
library(forecast)

# Importing the original data from excel

sharepricedata_2023_ <- read_excel("RESEARCH DATA SET/sharepricedata (2023).xlsx", 
                                   col_types = c("text", "text", "text", 
                                                 "text", "text", "date", "numeric"))
View(sharepricedata_2023_)

#changing the name of the data
time_series_data=sharepricedata_2023_
head(time_series_data) #plotting the first 6 rows

#converting the data into a time series data using ts
time_series=ts(time_series_data$Value,frequency = 12,start=c(1960,01,01),
               end=c(2022,12,01))

#check if the data was converted to a tie series data
class(time_series)

#check when it starts
start(time_series)
#check when it ends
end(time_series)

#plot the data with ACF and PACF
plot.ts(time_series,xlab="Year",ylab="Value",
        main="share price data",col="blue",frame=FALSE)

#plot the ACF and PACf of the original data
acf(time_series)
pacf(time_series)

#plot trend on the origal data
plot.ts(time_series,xlab="Year",ylab="Value",
        main="share price data",col="blue",frame=FALSE)
abline(reg=lm(time_series~time(time_series)))

#decompose the data
plot(decompose(time_series)$trend)
plot(decompose(time_series)$seasonal)
plot(decompose(time_series)$random)

time_series$year=as.Date(time_series_data$year,order="%/d/%m/%Y")
class(time_series$year)
time_series.z=zoo(x=time_series_data$Value,order.by = time_series_data$year)
plot.zoo(time_series.z,main="share prices",xlab="Year",
         ylab="Value",col="blue",type = "h",frame=FALSE)
plot(time_series,main="share prices",xlab="Year",
     ylab="Value",col="blue",type = "h",frame=FALSE)
lines(time_series,col="red")

#Although the time series data visibly shows non stationary
# statistical test is to be done 

#test for stationarity of the original data using ADF and phillipsPerron unit root
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

adf.test(time_series)
pp.test(time_series)
p.value1=adf.test(time_series)$p.value
p.value2=pp.test(time_series)$p.value

#for the ADF results
if(p.value1>0.05){
  print("The data is not stationary")
}else{
  print("The data is stationary")
}
#for the PP results
if(p.value2>0.05){
  print("The data is not stationary")
}else{
  print("The data is stationary")}

#Plot the ACF and PACF of the original data
# to determine the AR(p) and MA(q),SAR(P) and SMA(Q)

acf(time_series)
pacf(time_series)
#for the PACF plot autocorrelation is significant only at first and second lags
Box.test(time_series,lag=20,fitdf = 0)
p.value3=Box.test(time_series,lag=20,fitdf = 0)$p.value
if(p.value3>0.05){
  print("Accept the null hypothesi of no autocorrelation")
  
}else{
  print("Reject the null hypothesis of no autocorrelation")
}

#Transforming data to a stationary data
#using log and differencing

log_data=log(time_series)
#plot the log data
plot(log_data,main="Log share price",xlab="Year",ylab="Log values",col="red")
# the data shows a trend constant increasing trend

acf(log_data)
pacf(log_data)

#checking the number of d to remove the trend
ndiffs(log_data)
# d=1

#difference the logged data
diff_log_data =diff(log_data)

#plot the differenced log data
plot(diff_log_data,main="differenced logg share prices",xlab="Year",
     ylab="diff log values",col="red")

#check for stationarity and unut root
adf.test(diff_log_data)
pp.test(diff_log_data)
p.value4=adf.test(diff_log_data)$p.value
p.value5=pp.test(diff_log_data)$p.value

#for the ADF results
if(p.value4>0.05){
  print("The data is not stationary")
}else{
  print("The data is stationary")
}
#for the PP results
if(p.value5>0.05){
  print("The data is not stationary")
}else{
  print("The data is stationary")}

#plot the acf and pacf of the differenced log data
acf(diff_log_data)
pacf(diff_log_data)
# The is significant at lag 1 and some of higher order
#lags are slightly significant, we can start with MA(1)
# the PAcf shows only higher order significant lags
# then the suggested model is SARIMA(0,0,1)

Box.test(diff_log_data)

p.value6=Box.test(diff_log_data,lag=20,fitdf = 0)$p.value
if(p.value3>0.05){
  print("Accept the null hypothesi of no autocorrelation")
  
}else{
  print("Reject the null hypothesis of no autocorrelation")
}

#Estimate the model SARIMA 
model11=auto.arima(diff_log_data,allowdrift = FALSE,trace=TRUE)
model111=arima(diff_log_data,order=c(0,0,1),seasonal=c(0,0,0))
print(model111)

model11_fit=diff_log_data-residuals(model111)
acf(model11_fit)
pacf(model11_fit)
checkresiduals(model111)

#diagnostic checking
predict(model111,n.ahead=12)
plot(forecast(model111,100))
seasonplot(time_series)
monthplot(time_series)
acf(diff(log_data,differences = 1))
pacf(diff(log_data,differences = 1))

#################################################################################################
auto.arima(time_series,trace=TRUE,ic="aicc",approximation = FALSE,stepwise = FALSE)
final_model=arima(time_series,order=c(5,2,0))
final_model
summary(final_model)
#compare multiple models
AIC(arima(time_series,order=c(0,1,1)),arima(time_series,order=c(1,1,1)),arima(time_series,order=c(1,1,0)),
    arima(time_series,order=c(5,2,0)),arima(time_series,order=c(4,2,0)),
    arima(time_series,order=c(3,2,0)),arima(time_series,order=c(0,0,1)),arima(time_series,order=c(2,2,0)),
    arima(time_series,order=c(2,2,1)))
    #arima(time_series,order=c(2,2,0),seasonal=list(c(1,0,1),period=12)))
    #arima(time_series,order=c(1,2,0),seasonal=list(c(0,0,1),period=12)),
#arima(time_series,order=c(2,2,0),seasonal=c(1,0,1),frequency=12),
#arima(time_series,order=c(2,2,0),seasonal=list(c(2,0,1),period=12)),
#arima(time_series,order=c(2,2,0),seasonal=list(c(2,0,2),period=12)),
#arima(time_series,order=c(2,2,0),seasonal=list(c(1,0,0),period=12)))

model1111=arima(time_series,order=c(4,2,2))
forecast(model1111,24)
iinp=arima(log_data,
           model=model1111)
