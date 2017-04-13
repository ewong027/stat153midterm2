#Loading the Data:
data5 <- read.csv("q5_train.csv", as.is = TRUE)
plot(data5$activity, type = 'l')
data5_df <- data.frame(ts(data5))
data5_df$Date <- 1:nrow(data5)

#Writing data in much more accessible way 
ts_5 <- data5_df$activity

#EDA
plot(log(ts_5 + 1.5), type = "l")
acf(diff(ts_5), lag.max = 100)
pacf(diff(ts_5))

acf(ts_5, lag.max = 100)
pacf(ts_5)

plot(diff(diff(ts_5), lag = 25), type = "l")

plot((ts_5+ 1), type = 'l')
ts2 <- log(ts1+ 1.5)

auto.arima(ts_5)
#3,1,5

auto.arima(log(ts_5 +1.5))