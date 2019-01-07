library(glmnet)
dataread<-read.csv(file="C:/Users/Anthony/Desktop/Credit Card Default.csv", header=T, sep=",")
data<-dataread[-1,-1]

dim(data)

set.seed(22)
train = sample(1:nrow(data),ceiling(0.8*nrow(data)))



##create response and X matrix
#y = as.numeric(as.character(data[,24]))

library(ROSE)
train_data <- data[train,]
Obalanced_train <- ovun.sample(Y~.,data = train_data,method="over",N=18705*2)$data
y = as.numeric(as.character(Obalanced_train[,24]))


#selectx = randomForest(x∼.,data=data , subset=train ,mtry=10 , importance =TRUE)


X = model.matrix(lm(y~X1+X2+X3+X4+X5+X6+X7+X8+X9+X10,data=Obalanced_train))

#set lambda seq to find best lambda
grid <- 10^seq(0, -4, length=100)
lasso <- glmnet(X[train,],y[train],family = "binomial",alpha=1,lambda=grid)
cv.out <- cv.glmnet(X[train,],y[train],family = "binomial", alpha=1)
bestlam <- cv.out$lambda.min
bestlam

#solution path
par(mfrow=c(1,1))
plot(lasso, xvar='lambda', main="Solution path")
abline(v=log(bestlam), col="blue", lty=5.5 )

#. mean =0, which is not selected
coef(cv.out, s = "lambda.min")

#predict
pred_lasso <- predict(lasso, newx = X[-train,], s=bestlam)

#mse
mean((pred_lasso-y[-train])^2)




#true_predict
inlogit=exp(pred_lasso)/(1+exp(pred_lasso))

for (i in 1:13410){
  if(inlogit[i] > 0.5){
    inlogit[i] = 1
  } else {
    inlogit[i] = 0
  }
}




k = as.matrix(table(y[-train],inlogit))