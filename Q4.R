#Loading the Data:
data4 <- read.csv("q4_train.csv", as.is = TRUE)
plot(data4$activity, type = 'l')
data4_df <- data.frame(ts(data4))
data4_df$Date <- 1:nrow(data1)

#Cleaning up data 
ts_4 <- data4_df$activity  

#data appears to be made stationary by differencing
plot(diff(ts_4), type = "l")

#plotting acf and pacf to see what kind of model the resulting differenced 
#data follows
acf(diff(ts_4))
pacf(diff(ts_4))


#MA(1), AR(2) with the diffferenced data

auto.arima(ts_4)