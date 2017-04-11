data1 <- read.csv("q1_train.csv", as.is = TRUE)
plot(data1$activity, type = 'l')
data1_df <- data.frame(ts(data1))
data1_df$Date <- 1:nrow(data1)

ts1 <- data1$activity

stl(data1_df)
plot(ts1, type = 'l')

ts1_test = NULL
for (i in 1:length(ts1)){
  ts1_test[i] <- ts1[i] +1.5
}

ts1.log <- log(ts1_test)
plot(ts1.log, type = 'l')

ts1_diff <- diff(ts1.log, lag = 52)
plot(ts1_diff, type = 'l')

ts1_diff2 <- diff(ts1_diff)
plot(ts1_diff2, type = 'l')

ts1.log <- exp(ts1_diff)
plot(ts1.log, type = 'l')


#adding 1.5v to data and took the log transform
plot(log(ts1+ 1.5))
ts2 <- log(ts1+ 1.5)

#took difference twice so data looked reasonably stationary 
ts3 <- diff(ts2)
ts4 <- diff(ts3)

plot(ts4, type = "l")

#plotted acf
acf(ts4) #MA(1) leading 
pacf(ts4)

auto.arima(ts4)

#-------------#

data2 <- read.csv("q2_train.csv", as.is = TRUE)
plot(data2$activity, type = 'l')
data2_df <- data.frame(ts(data2))
data2$Date <- 1:nrow(data2)
ts2 <- data2$activity
plot(ts2, type = 'l')

ts2_diff <- diff(ts2)
plot(ts2_diff, type = 'l')
