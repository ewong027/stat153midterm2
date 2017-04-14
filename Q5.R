#Loading the Data:
data5 <- read.csv("q5_train.csv", as.is = TRUE)
plot(data5$activity, type = 'l')
data5_df <- data.frame(ts(data5))
data5_df$Date <- 1:nrow(data5)

#Writing data in much more accessible way 
ts_5 <- data5_df$activity

#EDA
plot(log(log(ts_5 + 1.5)+1.5), type = "l")
acf(diff(ts_5), lag.max = 100)
pacf(diff(ts_5))

#transforing the data
log_ts5 <- log(ts_5 + 1.5)

#auto arima functions
auto.arima(ts_5)
#3,1,5

auto.arima(log(ts_5 +1.5))
#2,1,1

CV5 <- function(test_order, test_seasonality, test_period){
  mse <- NULL
  leng <- length(log_ts5)
  for (i in 4:1){
    train <- log_ts5[1:(leng - i*105)]
    test <- log_ts5[(leng - i*105 + 1):(leng - (i-1)*105)]
    mod_train <- arima(log_ts5, order = test_order, seasonal = list(order = test_seasonality, 
                                                                    period= test_period))
    forcast <- predict(mod_train, n.ahead = 105)
    mse[i] <- mean((exp(forcast$pred) - exp(test))^2)
  }
  return(mse)
}

#Running Cross-Validation on different models
mse1 <- CV4(c(3,1,5),c(1, 0, 1),52)
mse2 <- CV4(c(3,1,5),c(1, 1, 1),52)
mse3 <- CV4(c(2,1,1),c(1, 0, 1),52)
mse4 <- CV4(c(2,1,1),c(1, 1, 1),52)
mse5 <- CV4(c(0,1,1),c(1, 0, 1),52)
mse6 <- CV4(c(0,1,1),c(1, 1, 1),52)


sum(mse1)/4
sum(mse2)/4
sum(mse3)/4
sum(mse4)/4
sum(mse5)/4
sum(mse6)/4

#Let us check AIC and BIC to see what model is chosen
mod_test1 <- arima(log_ts5, order = c(3,1,5), seasonal = list(order = c(1, 0, 1), period = 52))
mod_test2 <- arima(log_ts5, order = c(3,1,5), seasonal = list(order = c(1, 1, 1), period = 52))
mod_test3 <- arima(log_ts5, order = c(2,1,1), seasonal = list(order = c(1, 0, 1), period = 52))
mod_test4 <- arima(log_ts5, order = c(2,1,1), seasonal = list(order = c(1, 1, 1), period = 52))
mod_test5 <- arima(log_ts5, order = c(0,1,1), seasonal = list(order = c(1, 0, 1), period = 52))
mod_test6 <- arima(log_ts5, order = c(0,1,1), seasonal = list(order = c(1, 1, 1), period = 52))


AIC(mod_test1)
AIC(mod_test2)
AIC(mod_test3)
AIC(mod_test4)
AIC(mod_test5)
AIC(mod_test6)


BIC(mod_test1)
BIC(mod_test2)
BIC(mod_test3)
BIC(mod_test4)
BIC(mod_test5)
BIC(mod_test6)

#The second model is the best 

preds <- predict(mod_test2, n.ahead = 104)
preds2 <- exp(preds$pred) - 1.5
plot(c(ts_5, preds2), type = "l")

write.table(preds2, sep = ",", col.names = FALSE, row.names = FALSE, file = "Q5_Bryana_Gutierrez_24504003.txt")
