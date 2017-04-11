
#adding 1.5v to data and took the logg transform
plot(log(ts1+ 1.5))
ts2 <- log(ts1+ 1.5)

#took difference twice so data looked reasonably stationary 
ts3 <- diff(ts2)
ts4 <- diff(ts3)

plot(ts4, type = "l")

#plotted acf
acf(ts4) #MA(1) leading 
pacf(ts4)

mod <- arima(ts4, order = c(5, 0, 1))
AIC(mod)
BIC(mod)
length(ts4)

#CV score
mse1 <- NULL
leng <- length(ts4)
for (i in 4:1){
  train <- ts4[1:(leng - i*104)]
  test <- ts4[(leng - i*104 + 1):(leng - (i-1)*104)]
  mod_train <- arima(train, order = c(5, 0, 1))
  forcast <- predict(mod_train, n.ahead = 104)
  mse1[i] <- mean((exp(forcast$pred) - exp(test))^2)
}
  
mse2 <- NULL
for (i in 4:1){
  train <- ts4[1:(leng - i*104)]
  test <- ts4[(leng - i*104 + 1):(leng - (i-1)*104)]
  mod_train <- arima(train, order = c(5, 0, 0))
  forcast <- predict(mod_train, n.ahead = 104)
  mse2[i] <- mean((exp(forcast$pred) - exp(test))^2)
}

mse3 <- NULL
for (i in 4:1){
  train <- ts4[1:(leng - i*104)]
  test <- ts4[(leng - i*104 + 1):(leng - (i-1)*104)]
  mod_train <- arima(train, order = c(6, 0, 1))
  forcast <- predict(mod_train, n.ahead = 104)
  mse3[i] <- mean((exp(forcast$pred) - exp(test))^2)
}

mse4 <- NULL
for (i in 4:1){
  train <- ts4[1:(leng - i*104)]
  test <- ts4[(leng - i*104 + 1):(leng - (i-1)*104)]
  mod_train <- arima(train, order = c(4, 0, 1))
  forcast <- predict(mod_train, n.ahead = 104)
  mse4[i] <- mean((exp(forcast$pred) - exp(test))^2)
}

mse5 <- NULL
for (i in 4:1){
  train <- ts4[1:(leng - i*104)]
  test <- ts4[(leng - i*104 + 1):(leng - (i-1)*104)]
  mod_train <- arima(train, order = c(5, 0, 2))
  forcast <- predict(mod_train, n.ahead = 104)
  mse5[i] <- mean((exp(forcast$pred) - exp(test))^2)
}

sum(mse1)/4
sum(mse2)/4
sum(mse3)/4
sum(mse4)/4
sum(mse5)/4

#forcasting
preds <- predict(mod, n.ahead = 104)

preds1 <- cumsum(cumsum(preds$pred))
preds2 <- exp(preds1) - 1.5



plot(preds2, type = "l")
plot(ts1)
