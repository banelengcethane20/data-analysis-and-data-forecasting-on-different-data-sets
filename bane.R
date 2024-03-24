---
title: "Time Series Analysis with ARIMA-ARCH/GARCH Model"
author: "Kanika,  Jing, Bomin, Ziyao"
date: "23 November 2017"
output: slidy_presentation
---


library("tseries")
library("forecast")

appl.close=stock$adj.Close 
plot(appl.close,type='l',main='Apple Stock Price')

diff.appl=diff(appl.close)
plot(diff.appl,type='l',main='Difference Apple')

## Taking log

log.appl=log(appl.close)
plot(log.appl,type='l',main='Log Apple')

## Taking Difference of Log

difflog.appl=diff(log.appl)
plot(difflog.appl,type='l',main='Difference Log Apple')

## ARIMA 

## ACF of Logged Series

acf.appl=acf(log.appl,main='ACF Apple',lag.max=100,ylim=c(-0.5,1))

## PACF of Logged Series

pacf.appl=pacf(log.appl,main='PACF Apple',lag.max=100,ylim=c(-0.5,1))
```

## ACF of Differenced Series
acf.appl=acf(difflog.appl,main='ACF Difference Log Apple',lag.max=100,ylim=c(-0.5,1))
```

## PACF of Differenced Series

pacf.appl=pacf(difflog.appl,main='PACF Difference Log Apple',lag.max=100,ylim=c(-0.5,1))
```


## Estmating ARIMA Model 

arimatry = auto.arima(log.appl,ic="aic",allowdrift = FALSE,trace=TRUE)

## Best Fit ARIMA Model

arima011=arima(log.appl,order=c(0,1,1))
summary(arima011)
```

## Forecast the ARIMA Model

forecast011step1<-forecast(arima011,1,level=95)
forecast011=forecast(arima011,100,level=95)
plot(forecast011step1)
```


## Diagnostic Check
## Ploting resdiual

res.arima011=arima011$res
squared.res.arima011=res.arima011^2
plot(squared.res.arima011,main='Squared Residuals')
```
## ACF  of Residuals

acf.squared011=acf(squared.res.arima011,main='ACF Squared Residuals',lag.max=100,ylim=c(-0.5,1))

```

## ACF of Residuals

pacf.squared011=pacf(squared.res.arima011,main='PACF Squared Residuals',lag.max=100,ylim=c(-0.5,1))
```

## ARCH/GARCH

## Fitting Model

arch05=garch(res.arima011,order=c(0,5),trace=F)
loglik05=logLik(arch05)
AIC(arch05)
summary(arch05)

```
## Dianogstic Check
## Forecast

forecast011step1=forecast(arima011,1,level=95) 
forecast011=forecast(arima011,100,level=95)
plot(forecast011)
```

## Conditional Variance 

ht.arch05=arch05$fit[,1]^2 
plot(ht.arch05,main='Conditional variances')
```

## Fitted Values

fit011=fitted.values(arima011)
low=fit011-1.96*sqrt(ht.arch05)
high=fit011+1.96*sqrt(ht.arch05)
plot(log.appl,type='l',main='Log Apple,Low,High')
```
## QQ plot

plot(log.appl,type='l',main='Log Apple,Low,High')
lines(low,col='red')
lines(high,col='blue')
```


## ARIMA + Arch Residuals

archres=res.arima011/sqrt(ht.arch05)
qqnorm(archres,main='ARIMA-ARCH Residuals')
qqline(archres)
```


## Arima residuals

qqnorm(res.arima011,main='ARIMA Residuals')
qqline(res.arima011

