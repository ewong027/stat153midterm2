
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
sum((m1$residuals^2)/((1 - influence(m1)$hat)^2))

