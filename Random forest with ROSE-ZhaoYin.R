data <- read.table("data.csv", header = TRUE, sep = ",")
data <- data[,-1]
data$Y <- factor(data$Y)
set.seed(22)
index <- sample(1:nrow(data), ceiling(0.8*nrow(data)), replace=F)
data.train <- data[index,]
data.test <- data[-index,]

library(randomForest)
library(ROSE)
Obalanced_train <- ovun.sample(Y~.,data=data.train,method="over",N=37260)$data
sum(Obalanced_train$Y==1)
sum(Obalanced_train$Y==0)

####### bagging ntree=500 #######
set.seed(1)
bag.data_n2 = randomForest(Y~., data=Obalanced_train, mtry=23, ntree=500)
yhat.bag_n2 <- predict(bag.data_n2,newdata=data.test)
mean(bag.data_n2$predicted==Obalanced_train$Y)
bag.data_n2$confusion
bag.data_n2$importance
## test ##
mean(yhat.bag_n2 == data.test$Y)
table(yhat.bag_n2, data.test$Y)
sum(data.test$Y==1)
####### bagging ntree=1000 #######
set.seed(1)
bag.data_n2_t1000 = randomForest(Y~., data=Obalanced_train, mtry=23, ntree=1000)
yhat.bag_n2_t1000 <- predict(bag.data_n2_t1000,newdata=data.test)
sum(bag.data_n2_t1000$predicted==Obalanced_train$Y)/37260
bag.data_n2_t1000$confusion
bag.data_n2_t1000$importance
## test ##
mean(yhat.bag_n2_t1000 == data.test$Y)
table(yhat.bag_n2_t1000, data.test$Y)
sum(data.test$Y==1)
####### Random Forest mtry=5 ntree=500 #######
set.seed(1)
ranfor.data_n2_m5_t500 = randomForest(Y~., data=Obalanced_train, mtry=5, ntree=500)
yhat.ranfor_n2_m5_t500 <- predict(ranfor.data_n2_m5_t500, newdata=data.test)
mean(ranfor.data_n2_m5_t500$predicted==Obalanced_train$Y)
ranfor.data_n2_m5_t500$confusion
ranfor.data_n2_m5_t500$importance
## test ##
mean(yhat.ranfor_n2_m5_t500 == data.test$Y)
table(yhat.ranfor_n2_m5_t500, data.test$Y)
sum(data.test$Y==1)
####### Random Forest mtry=15 ntree=500 #######
set.seed(1)
ranfor.data_n2_m15_t500 = randomForest(Y~., data=Obalanced_train, mtry=15, ntree=500)
yhat.ranfor_n2_m15_t500 <- predict(ranfor.data_n2_m15_t500, newdata=data.test)
mean(ranfor.data_n2_m15_t500$predicted==Obalanced_train$Y)
ranfor.data_n2_m15_t500$confusion
ranfor.data_n2_m15_t500$importance
## test ##
mean(yhat.ranfor_n2_m15_t500 == data.test$Y)
table(yhat.ranfor_n2_m15_t500, data.test$Y)
sum(data.test$Y==1)

