data <- read.table("data.csv", header = TRUE, sep = ",")
data <- data[,-1]
data$Y <- factor(data$Y)
set.seed(22)
index <- sample(1:nrow(data), ceiling(0.8*nrow(data)), replace=F)
data.train <- data[index,]
data.test <- data[-index,]
summary(data.train)
library(randomForest)
sum(data.train$Y==1)/24000
sum(data.train$Y==0)
####### bagging ntree=500 #######
set.seed(1)
bag.data = randomForest(Y~., data=data, subset=index, mtry=23, ntree=500)
mean(bag.data$predicted==data.train$Y)
bag.data$confusion
bag.data$importance
yhat.bag <- predict(bag.data,newdata=data.test)
mean(yhat.bag == data.test$Y)
table(yhat.bag, data.test$Y)
sum(data.test$Y==1)
##### X6 Status in September #####
a <- c(-2:8)
b <- c(-2:8)
count <- 1
for(i in c(-2:8)){
  X6_index <- which(data.train$X6 %in% i)
  default_rate <- (summary(data.train[X6_index,]$Y)/length(X6_index))[2]
  a[count] <- default_rate
  b[count] <- length(X6_index)
  count <- count + 1
}
plot(c(1:23), bag.data$importance, xlab="Predictors", ylab="MeanDecreaseGini", main="Importance of each predictors", xaxt="n", yaxt="n")
axis(1, c(1:23))
axis(2, bag.data$importance)
plot(c(-2:8), a, type="b", xlab="Payment status in September", ylab="Default Rate", main="Default vs Status on September", xaxt="n", yaxt="n")
axis(1, c(-2:8))
axis(2, a)
b
##### X5 Age #####
a <- c(1:6)
b <- c(1:6)
count <- 1
for(i in c(0:5)){
  X5_index <- which(data.train$X5>=(20+i*10) & data.train$X5<(30+i*10))
  default_rate <- (summary(data.train[X5_index,]$Y)/length(X5_index))[2]
  a[count] <- default_rate
  b[count] <- length(X5_index)
  count <- count + 1
}
plot(c(1:6), a, type="b", xlab="Age", ylab="Default Rate", main="Default vs Age", xaxt="n", yaxt="n")
axis(1, at=c(1:6), label=c("20-30","30-40","40-50","50-60","60-70","70-80"))
axis(2, a)
b
##### X12 bill in September #####
max(data.train$X12)
min(data.train$X12)
a <- c(1:11)
b <- c(1:11)
count <- 1
X12_index <- which(data.train$X12<0)
default_rate <- (summary(data.train[X12_index,]$Y)/length(X12_index))[2]
a[count] <- default_rate
b[count] <- length(X12_index)
count <- count + 1
X12_index <- which(data.train$X12>=0 & data.train$X12<500)
default_rate <- (summary(data.train[X12_index,]$Y)/length(X12_index))[2]
a[count] <- default_rate
b[count] <- length(X12_index)
count <- count + 1
X12_index <- which(data.train$X12>=500 & data.train$X12<3000)
default_rate <- (summary(data.train[X12_index,]$Y)/length(X12_index))[2]
a[count] <- default_rate
b[count] <- length(X12_index)
count <- count + 1
X12_index <- which(data.train$X12>=3000 & data.train$X12<8500)
default_rate <- (summary(data.train[X12_index,]$Y)/length(X12_index))[2]
a[count] <- default_rate
b[count] <- length(X12_index)
count <- count + 1
X12_index <- which(data.train$X12>=8500 & data.train$X12<17500)
default_rate <- (summary(data.train[X12_index,]$Y)/length(X12_index))[2]
a[count] <- default_rate
b[count] <- length(X12_index)
count <- count + 1
X12_index <- which(data.train$X12>=17500 & data.train$X12<29000)
default_rate <- (summary(data.train[X12_index,]$Y)/length(X12_index))[2]
a[count] <- default_rate
b[count] <- length(X12_index)
count <- count + 1
X12_index <- which(data.train$X12>=29000 & data.train$X12<47800)
default_rate <- (summary(data.train[X12_index,]$Y)/length(X12_index))[2]
a[count] <- default_rate
b[count] <- length(X12_index)
count <- count + 1
X12_index <- which(data.train$X12>=47800 & data.train$X12<73000)
default_rate <- (summary(data.train[X12_index,]$Y)/length(X12_index))[2]
a[count] <- default_rate
b[count] <- length(X12_index)
count <- count + 1
X12_index <- which(data.train$X12>=73000 & data.train$X12<125000)
default_rate <- (summary(data.train[X12_index,]$Y)/length(X12_index))[2]
a[count] <- default_rate
b[count] <- length(X12_index)
count <- count + 1
X12_index <- which(data.train$X12>=125000 & data.train$X12<300000)
default_rate <- (summary(data.train[X12_index,]$Y)/length(X12_index))[2]
a[count] <- default_rate
b[count] <- length(X12_index)
count <- count + 1
X12_index <- which(data.train$X12>=300000)
default_rate <- (summary(data.train[X12_index,]$Y)/length(X12_index))[2]
a[count] <- default_rate
b[count] <- length(X12_index)
count <- count + 1
a
b

a <- c(1:61)
b <- c(1:61)
count <- 1
for(i in c(0:60)){
  X12_index <- which(data.train$X12>=(i*5000) & data.train$X12<((i+1)*5000))
  default_rate <- (summary(data.train[X12_index,]$Y)/length(X12_index))[2]
  a[count] <- default_rate
  b[count] <- length(X12_index)
  count <- count + 1
}
plot(c(1:61), a, type="b", xlab="Bill in September($5000)", ylab="Default Rate", main="Default vs Bill in sep")

####### bagging ntree=1000 #######
set.seed(1)
bag.data_t1000 = randomForest(Y~., data=data, subset=index, mtry=23, ntree=1000)
yhat.bag_t1000 <- predict(bag.data_t1000,newdata=data.test)
table(yhat.bag_t1000, data.test$Y)
mean(yhat.bag_t1000 == data.test$Y)
sum(bag.data_t1000$predicted==data.train$Y)/24000
bag.data_t1000$confusion
bag.data_t1000$importance
####### bagging ntree=5000 #######
set.seed(1)
bag.data_t5000 = randomForest(Y~., data=data, subset=index, mtry=23, ntree=5000)
yhat.bag_t5000 <- predict(bag.data_t5000,newdata=data.test)
table(yhat.bag_t5000, data.test$Y)
mean(yhat.bag_t5000 == data.test$Y)
sum(bag.data_t5000$predicted==data.train$Y)/24000
bag.data_t5000$confusion
bag.data_t1000$importance
####### random forest mtry=5 ntree=500 #######
set.seed(1)
ranfor.data_m5_t500 = randomForest(Y~., data=data, subset=index, mtry=5, ntree=500)
yhat.ranfor_m5_t500 <- predict(ranfor.data_m5_t500,newdata=data.test)
table(yhat.ranfor_m5_t500, data.test$Y)
mean(yhat.ranfor_m5_t500 == data.test$Y)
sum(ranfor.data_m5_t500$predicted==data.train$Y)/24000
ranfor.data_m5_t500$confusion
ranfor.data_m5_t500$importance
####### random forest mtry=15 ntree=500 #######
set.seed(1)
ranfor.data_m15_t500 = randomForest(Y~., data=data, subset=index, mtry=15, ntree=500)
yhat.ranfor_m15_t500 <- predict(ranfor.data_m15_t500,newdata=data.test)
table(yhat.ranfor_m15_t500, data.test$Y)
mean(yhat.ranfor_m15_t500 == data.test$Y)
sum(ranfor.data_m15_t500$predicted==data.train$Y)/24000
ranfor.data_m15_t500$confusion
