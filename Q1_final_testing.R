#Loading the Data:
data1 <- read.csv("q1_train.csv", as.is = TRUE)
plot(data1$activity, type = 'l')
data1_df <- data.frame(ts(data1))
data1_df$Date <- 1:nrow(data1)

#Cleaning the Data:
ts1 <- data1$activity

#Taking the log to get rid of the trend
plot(log(ts1+ 1.5), type = 'l')
ts2 <- log(ts1+ 1.5)

#running auto.arima to get a better idea of what the model is
auto.arima(ts2)

#Doing Different Transformations to get a better idea of the wanted model
#took difference twice so data looked reasonably stationary 
ts3 <- diff(ts2)
plot(ts3, type = "l")

ts4 <- diff(ts3)
plot(ts4, type = "l")

#plotted acf
acf(ts3)
pacf(ts3)

acf(ts4) #MA(1) leading 
pacf(ts4)

#Cross-Validation as a Function:
CV <- function(test_order, test_seasonality, test_period){
  mse <- NULL
  leng <- length(ts2)
  for (i in 4:1){
    train <- ts2[1:(leng - i*104)]
    test <- ts2[(leng - i*104 + 1):(leng - (i-1)*104)]
    mod_train <- arima(ts2, order = test_order, seasonal = list(order = test_seasonality, 
                                                                period= test_period))
    forcast <- predict(mod_train, n.ahead = 104)
    mse[i] <- mean((exp(forcast$pred) - exp(test))^2)
  }
  return(mse)
}

