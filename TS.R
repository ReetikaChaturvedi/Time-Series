library(factoextra)
library(ggplot2)
library(ggcorrplot)
library(dplyr)
library(tseries)
library(forecast)
library(astsa)

austres
class(austres)
start(austres)
end(austres)
frequency(austres)

#Finding summary, structure, regression and lag instance
summary(austres)
str(austres)
reg_austres <-  lm(austres ~ time(austres))
cycle(austres)
lag.plot(austres,4, main = "Lag Plot for 4 Quarter")

#To obtain a  boxplot by cycle
boxplot(austres ~ cycle(austres), main = "Four Quarter Boxplot")

# Finding the Stationarity of data
plot(austres, xlab = "Year" , ylab = "Number of Australian Residents", main = "Population of Australia in thousand")

#Time Series function on austres
ts_austres <- ts(austres, frequency = 4)
decompose_austres <- decompose(ts_austres, "multiplicative")
plot(decompose_austres)

#Building Arima Model
model_austres <- auto.arima(austres)
model_austres
auto.arima(austres, ic= "aic", trace = TRUE)
plot.ts(model_austres$residuals, main = "Time Series Residuals")

#ACF- Auto Coreelation Function
acf(ts(model_austres$residuals), main = 'ACF Residuals')

#PACF- Partial Auto Correlation Funtion
pacf(ts(model_austres$residuals), main = 'PACF Residuals')

#Forecast Function
forecast_population <- forecast(model_austres, level = c(95), h=10*4) #level of accuracy and h is time of forecasting
plot(forecast_population, main = "Australia Population Forecast for 10 Years")
summary(forecast_population)

#validate model
Box.test(model_austres$residuals, lag = 3, type = "Ljung-Box")
Box.test(model_austres$residuals, lag = 8, type = "Ljung-Box")
Box.test(model_austres$residuals, lag = 12, type = "Ljung-Box")
