data1 <- read.csv("q1_train.csv", as.is = TRUE)
data1_df <- data.frame(ts(data1))
data1$Date <- 1:nrow(data1)

ts1 <- data1$activity
plot(ts1, type = 'l')

ts1_diff <- diff(ts1)
plot(ts1_diff, type = 'l')



ts1.log <- exp(ts1)
plot(ts1.log)


yahoo <- read.csv('yahoo.csv', header = TRUE) 
ts_yahoo <- data.frame(ts(yahoo)) 
colnames(ts_yahoo)[2] <- 'Value'