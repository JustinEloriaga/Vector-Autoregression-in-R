#VAR and SVAR in R
#Justin S. Eloriaga

#Install the vars package

install.packages("vars")

#Load required packages for running VAR

library(urca)
library(vars)
library(mFilter)
library(tseries)
library(TSstudio)
library(forecast)
library(tidyverse)

#Load the Dataset


mp <- read_csv(file.choose())
head(mp)

#Declare and Graph our Time Series Variables

lnIP <- ts(mp$lnIP, start = c(2003,1,1), frequency = 12)
lnM1 <- ts(mp$lnM1, start = c(2003,1,1), frequency = 12)
M1 <- ts(mp$M1, start = c(2003,1,1), frequency = 12)
CPI <- ts(mp$CPI, start = c(2003,1,1), frequency = 12)
RRP <- ts(mp$RRP, start = c(2003,1,1), frequency = 12)


#Graphing the Series

ts_plot(lnIP)
ts_plot(lnM1)
ts_plot(M1)
ts_plot(CPI)
ts_plot(RRP)

#Testing for Non-stationarity

pp.test(lnIP)
pp.test(M1)
pp.test(lnM1)
pp.test(CPI)
pp.test(RRP)

#Finding the Optimal Lags

v1 <- cbind(RRP, lnM1, CPI, lnIP)
colnames(v1) <- cbind("RRP","M1","CPI", "lnIP")

lagselect <- VARselect(v1, lag.max = 15, type = "both")
lagselect$selection

#Building VAR
Model1 <- VAR(v1, p = 2, type = "const", season = NULL, exog = NULL)
summary(Model1)

#Diagnosing the VAR

#Serial Correlation

Serial1 <- serial.test(Model1, lags.pt = 5, type = "PT.asymptotic")
Serial1

#Heteroscedasticity

Arch1 <- arch.test(Model1, lags.multi = 15, multivariate.only = TRUE)
Arch1

#Normal Distribution of the Residuals

Norm1 <- normality.test(Model1, multivariate.only = TRUE)
Norm1

#Testing for Structural Breaks in the Residuals

Stability1 <- stability(Model1, type = "OLS-CUSUM")
plot(Stability1)

#Granger Causality

GrangerRRP<- causality(Model1, cause = "RRP")
GrangerRRP

GrangerM1<- causality(Model1, cause = "M1")
GrangerM1

GrangerCPI<- causality(Model1, cause = "CPI")
GrangerCPI

GrangerlnIP<- causality(Model1, cause = "lnIP")
GrangerlnIP

#Impulse Response Functions

RRPirf <- irf(Model1, impulse = "RRP", response = "RRP", n.ahead = 20, boot = TRUE)
plot(RRPirf, ylab = "RRP", main = "RRP's shock to RRP")

M1irf <- irf(Model1, impulse = "RRP", response = "M1", n.ahead = 20, boot = TRUE)
plot(M1irf, ylab = "M1", main = "RRP's shock to M1")

CPIirf <- irf(Model1, impulse = "RRP", response = "CPI", n.ahead = 20, boot = TRUE)
plot(CPIirf, ylab = "CPI", main = "RRP's shock to CPI")

lnIPirf <- irf(Model1, impulse = "RRP", response = "lnIP", n.ahead = 20, boot = TRUE)
plot(lnIPirf, ylab = "lnIP", main = "RRP's shock to lnIP")


#Variance Decomposition

FEVD1 <- fevd(Model1, n.ahead = 10)
FEVD1
plot(FEVD1)

#VAR Forecasting

forecast <- predict(Model1, n.ahead = 12, ci = 0.95)
fanchart(forecast, names = "RRP", main = "Fanchart for RRP", xlab = "Horizon", ylab = "RRP")
fanchart(forecast, names = "M1", main = "Fanchart for M1", xlab = "Horizon", ylab = "M1")
fanchart(forecast, names = "CPI", main = "Fanchart for CPI", xlab = "Horizon", ylab = "CPI")
fanchart(forecast, names = "lnIP", main = "Fanchart for lnIP", xlab = "Horizon", ylab = "lnIP")
forecast

