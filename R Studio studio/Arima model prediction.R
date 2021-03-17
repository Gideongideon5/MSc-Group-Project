setwd("C:/Users/parckardbell/Desktop/Bradford/Second Semester/Group Project/Dataset/Air Pollution Data")


air_quality <- read.csv(file = 'PM2.5.csv')



head(air_quality)
str(air_quality)
summary(air_quality)

library(forecast)
library(ggplot2)
library(MLmetrics)
library(Metrics)

train_windTS <- ts(airquality$Wind[1:123])
train_pol_fit_arima <- arima(train_windTS, order = c(10,1,38), seasonal = c(0, 0, 0),include.mean = TRUE)
plot(forecast(train_pol_fit_arima, h=30))
train_forecast_arima<-forecast(train_pol_fit_arima,30)
test_data = ts(airquality$Wind[124:153])

train_forecast_arima_df<-data.frame(train_forecast_arima)
train_forecast_arima_pt_forecast<-train_forecast_arima_df$Point.Forecast

forecast_train_arima<-data.frame("x"= c(1:30), "y"= train_forecast_arima_pt_forecast)
actual_train_arima<-data.frame("x"=c(1:30),"y"= test_data)

ggplot(forecast_train_arima,aes(x,y))+geom_line(aes(color="First line")) + geom_line(data = actual_train_arima,aes(color="Second line"))+xlab("day")+ylab("Unit")+  labs(colors="Series")+ggtitle("Training Vs Testing plot")+
  scale_colour_manual(values = c("red","green"),   labels=c("Forecast", "Actual"))


MAPE(test_data,train_forecast_arima_pt_forecast)
Box.test(train_forecast_arima$residuals, lag=24, type="Ljung-Box")


