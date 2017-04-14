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
    train <- ts2[1:(leng - i*105)]
    test <- ts2[(leng - i*105 + 1):(leng - (i-1)*105)]
    mod_train <- arima(ts2, order = test_order, seasonal = list(order = test_seasonality, 
                                                                period= test_period))
    forcast <- predict(mod_train, n.ahead = 105)
    mse[i] <- mean((exp(forcast$pred) - exp(test))^2)
  }
  return(mse)
}

#Running Cross-Validation on different models: The best model we found via auto.arima was ARIMA(4,1,2)
#but from our graph we can see that there is some seasonality, so we wanted to test that
mse1 <- CV(c(4,1,2),c(1, 0, 1),52)
mse2 <- CV(c(4,2,2),c(1, 0, 1),52)
mse3 <- CV(c(4,1,2),c(0, 0, 1),52)
mse4 <- CV(c(5,1,2),c(1, 0, 1),52)
mse5 <- CV(c(4,1,3),c(1, 0, 1),52)
mse6 <- CV(c(4,1,1),c(1, 0, 1),52)

sum(mse1)/4
sum(mse2)/4
sum(mse3)/4
sum(mse4)/4
sum(mse5)/4
sum(mse6)/4

#the smallest MSE is mse1, so the model choosen is CV(c(4,1,2),c(1, 0, 1),52), models 4 and 5
#produced errors which may imply that it is not the best idea to use them

#Let us check AIC and BIC to see what model is chosen
mod_test1 <- arima(ts2, order = c(4,1,2), seasonal = list(order = c(1, 0, 1), period = 52))
mod_test2 <- arima(ts2, order = c(4,2,2), seasonal = list(order = c(1, 0, 1), period = 52))
mod_test3 <- arima(ts2, order = c(4,1,2), seasonal = list(order = c(0, 0, 1), period = 52))
mod_test4 <- arima(ts2, order = c(5,1,2), seasonal = list(order = c(1, 0, 1), period = 52))
mod_test5 <- arima(ts2, order = c(4,1,3), seasonal = list(order = c(1, 0, 1), period = 52))
mod_test6 <- arima(ts2, order = c(4,1,1), seasonal = list(order = c(1, 0, 1), period = 52))

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

#Both AIC and BIC pick mod_test6 as the best model, which is
#arima(ts2, order = c(4,1,1), seasonal = list(order = c(1, 0, 1), period = 52))

#forcasting
preds <- predict(mod_test6, n.ahead = 104)
preds2 <- exp(preds$pred) - 1.5
plot(c(ts1, preds2), type = "l")

preds_1 <- predict(mod_test1, n.ahead = 104)
preds2_1 <- exp(preds_1$pred) - 1.5
plot(c(ts1, preds2_1), type = "l")

#there was not a difference between the graphs, but we ended up choosing model 1 because we found that
#the AIC and BIC were fairly close to that of model 6

write.table(preds2, sep = ",", col.names = FALSE, row.names = FALSE, file = "Q1_Bryana_Gutierrez_24504003.txt")

