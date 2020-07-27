# 174timeseries
Used a monthly traffic fatality data set of fifteen years (1960-1974) to perform a time series analysis that can predict the traffic fatality rates in Ontario, Canada.


R Code - for output, check PDF file.

knitr::opts_chunk$set(echo = TRUE)
setwd("C:/Users/Chelsea/Desktop/Chelsea School/Pstat 174") #sets my working directory traffic.csv = read.table('data.txt', header = FALSE) #reads the txt file for the time series #head(traffic.csv)
traffic = ts(traffic.csv[,2], start = c(1960,1), frequency = 12) #converts the table to a time series data set
min(traffic) max(traffic)
ts.plot(traffic)

#increasing-ish trend, strong seasonal component, variability depends on time

#box-cox transformation
library(MASS)
t = 1:length(traffic)
fit = lm(traffic ~ t)
bcTransform = boxcox(traffic ~ t,plotit = TRUE)
#corresponds to a 95% confidence interval for the true value of lambda in the Box-Cox tranformation

#transforming with lambda from box-cox
lambda = bcTransform$x[which(bcTransform$y == max(bcTransform$y))]
lambda
traffic.bc = (1/lambda)*(traffic^lambda-1) #transforms the time series data to the best lambda

#plot of the transformed data
op <- par(mfrow = c(1,2))
ts.plot(traffic,main = "Original data",ylab = expression(X[t])) ts.plot(traffic.bc,main = "Box-Cox tranformed data", ylab = expression(Y[t]))
13    
par(op)

#comparing the variance of both original data and transformed data var(traffic)
var(traffic.bc)

#acf/pacf of the transformed data
op = par(mfrow = c(1,2))
acf(traffic.bc,lag.max = 60,main = "")
pacf(traffic.bc,lag.max = 60,main = "")
title("Box-Cox Transformed Time Series", line = -1, outer=TRUE) par(op)

#variance unstable - log transform next traffic.log <- log(traffic)
op <- par(mfrow = c(1,3))
ts.plot(traffic, main = "Original Data") ts.plot(traffic.bc, main = "Box-Cox") ts.plot(traffic.log, main = "log transform") par(op)
#checking var of both log and box-cox transform
var(traffic.bc)
var(traffic.log) #traffic.log does not LOOK much different than traffic.bc BUT var is MUCH smaller

#checking acf/pacf of log and box-cox
op = par(mfrow = c(1,2))
acf(traffic.log,lag.max = 48,main = "") pacf(traffic.log,lag.max = 48,main = "")
title("Log Transformed Time Series", line = -1, outer=TRUE) par(op)

#checking sqrt tranformation of data
traffic.sqrt <- sqrt(traffic)
op <- par(mfrow = c(1,3))
ts.plot(traffic, main = "Original Data") ts.plot(traffic.log, main = "Log Transform") ts.plot(traffic.sqrt, main = "Square-Root Transform") par(op) #again, graph doesn't LOOK much different

#checking variance of log and sqrt tranform
var(traffic.log)
var(traffic.sqrt) #traffic.sqrt has higher variance, use traffic.log for transformed data
14

#difference at lag 12 to remove seasonality traffic.12 <- diff(traffic.log, lag = 12)
op = par(mfrow = c(1,3))
ts.plot(traffic.12, main = "")
acf(traffic.12,lag.max = 48,main = "") pacf(traffic.12,lag.max = 48,main = "") title("Differenced at Lag 12", line = -1, outer=TRUE) par(op)

#difference at lag 1 to try and remove trend
traffic.1 <- diff(traffic.12, lag = 1)
op = par(mfrow = c(1,3))
ts.plot(traffic.1, main = "")
acf(traffic.1,lag.max = 48,main = "")
pacf(traffic.1,lag.max = 48,main = "")
title("Differenced at Lag 12 and Lag 1", line = -1, outer=TRUE) par(op)

#checking var of diff lag 12 and diff lag 12 and lag 1 var(traffic.12)
var(traffic.1) #shows signs of overdifferencing, use traffic.12

#splitting data into train/test set and checking var/acf/pacf to double check it's similar to original data t.train <- ts(traffic[1:162], start = c(1960,1), frequency = 12)
t.test <- traffic[163:180]
ts.plot(t.train)
ts.plot(traffic)
mean(t.train) mean(t.test)
var(t.train) var(t.test)

#transforming the training set and checking the var and acf/pacf train.log <- log(t.train)
var(train.log)

#checking stationarity
train.12 <- diff(train.log, lag = 12)#differences the training data
adf.test(train.12, alternative = "stationary")#checks for stationarity, small p-values suggest stationarity
op = par(mfrow = c(1,2)) acf(train.log,lag.max = 48,main = "")
15
pacf(train.log,lag.max = 48,main = "")
title("Log Transform - Training Set", line = -1, outer=TRUE) par(op)
library(forecast)

#fits the data with approx for best model
auto.arima(train.log, allowdrift = FALSE, trace = TRUE, allowmean = FALSE, ic = "aicc")

#best according to auto.arima
fit.202.211 <- arima(train.log, order = c(2,0,2), seasonal = list(order = c(2,1,1), period = 12), method = 'ML', include.mean = FALSE)

#next 5 best
fit.101.212 <- arima(train.log, order = c(1,0,1), seasonal = list(order = c(2,1,2), period = 12), method = "ML", include.mean = FALSE)
fit.201.212 <- arima(train.log, order = c(2,0,1), seasonal = list(order = c(2,1,2), period = 12), method = "ML", include.mean = FALSE)
fit.101.112 <- arima(train.log, order = c(1,0,1), seasonal = list(order = c(1,1,2), period = 12), method = "ML", include.mean = FALSE)
#fit.102.212 <- arima(train.log, order = c(1,0,2), seasonal = list(order = c(2,1,2), period = 12), method = "ML", include.mean = FALSE) - error code
fit.101.211 <- arima(train.log, order = c(1,0,1), seasonal = list(order = c(2,1,1), period = 12), method = "ML", include.mean = FALSE)
fit.102.211 <- arima(train.log, order = c(1,0,2), seasonal = list(order = c(2,1,1), period = 12), method = "ML", include.mean = FALSE)

library(qpcR)

#comparing AICc for best model
matrix(c(AICc(fit.202.211), AICc(fit.101.212), AICc(fit.201.212), AICc(fit.101.112), AICc(fit.101.211), AICc(fit.102.211)), nrow = 1, dimnames = list("AICc", c("fit.202.211", "fit.101.212", "fit.201.212", "fit.101.112", "fit.101.211","fit.102.211")))

#choose fit.201.212 as it has the lowest AICc #compare fit.102.211 and fit.101.212 fit.201.212
fit.102.211
res.fit.212 <- residuals(fit.201.212) res.fit.211 <- residuals(fit.102.211)
op = par(mfrow = c(1,2)) qqnorm(res.fit.212, main = "A") qqnorm(res.fit.211, main = "B") title("Q-Q Plot", line = -1, outer=TRUE) par(op)
16

op = par(mfrow = c(1,2))
acf(res.fit.212, lag.max = 48, main = "A") acf(res.fit.211, lag.max = 48, main = "B") title("ACF Plots", line = -1, outer = TRUE) par(op)
op = par(mfrow = c(1,2))
pacf(res.fit.212, lag.max = 48, main = "A") pacf(res.fit.211, lag.max = 48, main = "B") title("PACF Plots", line = -1, outer = TRUE) par(op)
op = par(mfrow = c(1,2))
hist(res.fit.212, breaks = 10, main = "SARIMA(2,0,1)x(2,1,2)[12]") hist(res.fit.211, breaks = 10, main = "SARIMA(1,0,2)x(2,1,1)[12]") par(op)
shapiro.test(res.fit.212)
shapiro.test(res.fit.211)
h1 <- min(2*12,length(res.fit.212)/5)
h2 <- min(2*12,length(res.fit.211)/5)
h1
h2
Box.test(res.fit.212, lag = 12, type = "Box-Pierce", fitdf = 2)
Box.test(res.fit.211, lag = 12, type = "Box-Pierce", fitdf = 2)
Box.test(res.fit.212, lag = h1, type = "Box-Pierce", fitdf = 2)
Box.test(res.fit.211, lag = h2, type = "Box-Pierce", fitdf = 2)
Box.test(res.fit.212, lag = 12, type = "Ljung", fitdf = 2)
Box.test(res.fit.211, lag = 12, type = "Ljung", fitdf = 2)
Box.test(res.fit.212, lag = h1, type = "Ljung", fitdf = 2)
Box.test(res.fit.211, lag = h2, type = "Ljung", fitdf = 2)

#tests at lag 12 and h to test for serial correlation, but fails to reject null as p >0.05 so lags are not serially correlated,thus residuals are independent.
#The AICc is lower on fit.201.212 but most of the p-values for the diagnostic checking are higher for fit.102.211 as some of the p-values for fit.201.212 are just above the 0.05 value to not reject the null hypothesis. Also, the Q-Q plot is not as heavy tailed on the fit.102.211 resid plot so, after comparison on the 2 best fits, we choose fit.102.211
#forecasting
pred.tr <- predict(fit.101.212, n.ahead = 18) #predicts last 18 months #95% CI
up.tr <- pred.tr$pred + 1.96*pred.tr$se
17

low.tr <- pred.tr$pred - 1.96*pred.tr$se

#plots the training data along with the predicted values
ts.plot(train.log, xlim = c(1960,1975), main = "Transformed Monthly Traffic Fatalities", ylab = "Log of Number of Fatalities", xlab = "Month")
lines(up.tr, col = "blue", lty = "dashed")
lines(low.tr, col = "blue", lty = "dashed")
points(pred.tr$pred, col = "red", type = "l")

#plots all the data along with the predicted values
ts.plot(traffic.log, xlim = c(1960,1975), main = "Transformed Monthly Traffic Fatalities", ylab = "Log of Number of Fatalities", xlab = "Month")
lines(up.tr, col = "blue", lty = "dashed")
lines(low.tr, col = "blue", lty = "dashed")
points(pred.tr$pred, col = "red", type = "l")

#the predicted values fall in line with the 95% CI and are very similar to real data with slight differences

#bringing predicted values back to original data pred.og <- exp(pred.tr$pred)
up.og <- exp(up.tr)
low.og <- exp(low.tr)
ts.plot(t.train, xlim = c(1960,1975), main = "Monthly Traffic Fatalities", ylab = "Number of Fatalities", xlab = "Month")
lines(up.og, col = "blue", lty = "dashed")
lines(low.og, col = "blue", lty = "dashed")
points(pred.og, col = "red", type = "l")
ts.plot(traffic, xlim = c(1960,1975), main = "Monthly Traffic Fatalities", ylab = "Number of Fatalities", xlab = "Month")
lines(up.og, col = "blue", lty = "dashed")
lines(low.og, col = "blue", lty = "dashed")
points(pred.og, col = "red", type = "l")

#zooming in on the last 5 years
ts.plot(traffic, xlim = c(1970,1975), main = "Monthly Traffic Fatalities", ylab = "Number of Fatalities", xlab = "Month")
lines(up.og, col = "blue", lty = "dashed")
lines(low.og, col = "blue", lty = "dashed")
points(pred.og, col = "red", type = "l")
