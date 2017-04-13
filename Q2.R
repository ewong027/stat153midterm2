#Loading the Data:
data2 <- read.csv("q2_train.csv", as.is = TRUE)
plot(data2$activity, type = 'l')
data2_df <- data.frame(ts(data2))
data2_df$Date <- 1:nrow(data2)

#Cleaning the Data:
ts2 <- data2$activity
min(ts2) # the minimum is about -1.5 and since I want to be able to take the log to get
#rid of the trend, I am going to add in 1.5 to shift the data upward and make it postive

#Taking the log to get rid of the trend
plot(log(ts2+ 1.5), type = 'l')
ts2_adj <- log(ts2+ 1.5)

#using auto.arima to see if there is some good fit
auto.arima(ts2_adj)
which.min(acf(ts2_adj, lag.max = 100)$acf[-1])

#Cross-Validation as a Function:
CV <- function(test_order, test_seasonality, test_period){
  mse <- NULL
  leng <- length(ts2_adj)
  for (i in 4:1){
    train <- ts2_adj[1:(leng - i*105)]
    test <- ts2_adj[(leng - i*105 + 1):(leng - (i-1)*105)]
    mod_train <- arima(ts2_adj, order = test_order, seasonal = list(order = test_seasonality, 
                                                                period= test_period))
    forcast <- predict(mod_train, n.ahead = 105)
    mse[i] <- mean((exp(forcast$pred) - exp(test))^2)
  }
  return(mse)
}

mse1 <- CV(c(2,1,1),c(1, 0, 1),52)
mse2 <- CV(c(2,1,0),c(1, 0, 1),52)
mse3 <- CV(c(2,1,2),c(0, 0, 0),NA)
mse4 <- CV(c(2,1,1),c(0, 0, 0),NA)
mse5 <- CV(c(3,1,2),c(0, 0, 0),NA)
mse6 <- CV(c(1,1,2),c(1, 0, 1),52)

sum(mse1)/4
sum(mse2)/4
sum(mse3)/4
sum(mse4)/4
sum(mse5)/4
sum(mse6)/4

mod_test1 <- arima(ts2_adj, order = c(2,1,1), seasonal = list(order = c(1, 0, 1), period = 52))
mod_test2 <- arima(ts2_adj, order = c(2,1,0), seasonal = list(order = c(1, 0, 1), period = 52))
mod_test3 <- arima(ts2_adj, order = c(2,1,2))
mod_test4 <- arima(ts2_adj, order = c(2,1,1))
mod_test5 <- arima(ts2_adj, order = c(3,1,2))
mod_test6 <- arima(ts2_adj, order = c(1,1,2), seasonal = list(order = c(1, 0, 1), period = 52))

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

#forcasting both of these models look the same, i think the second one looks slightly better
preds <- predict(mod_test2, n.ahead = 105)
preds2 <- exp(preds$pred) - 1.5
plot(c(ts2, preds2), type = "l")

preds_1 <- predict(mod_test1, n.ahead = 105)
preds2_1 <- exp(preds_1$pred) - 1.5
plot(c(ts2, preds2_1), type = "l")
