#install.packages("readxl")
#install.packages("np")
#install.packages("corrplot")
#install.packages("polspline")
#install.packages("ROSE")
library(ROSE);library(readxl);library(np);library(corrplot);library(glmnet);library(polspline)


####get data####
creditcard <- read_excel("R/creditcard.xls", skip = 1)
View(creditcard)

summary(creditcard)
summary(creditcard[which(creditcard[,25]==1),])

###splitting####
set.seed(5)
index = sample(1:nrow(creditcard),ceiling(0.8*nrow(creditcard)))
train = creditcard[index,]
test = creditcard[-index,]


####basic expolore####
corr<-cor(train)
corrplot(corr)
subset(train,train$Y==1)
corr2<-cor(train[which(train[,25]==1),c(1:24)])
corrplot(corr2)
summary(train)
#pairs(train)
cor(train[,2],train[,5])
for(i in c(1:24000)){
  if(train[i,5]==3){train[1,5]=0}
  else if(train[i,5]==2){train[1,5]=-1}
}
cor(train[,2],train[,5])


####character engineering####
z_score <- function(x){
  x<-as.matrix(x)
  return((x-mean(x))/sd(x))
}


stab <-creditcard[,2]
active <-creditcard[,2]
stab1 <-creditcard[,2]
for(i in c(1:30000)){
  stab[i,1]<- ifelse(is.nan(sum(abs(z_score(creditcard[i,c(13:18)])))),0,sum(abs(z_score(creditcard[i,c(13:18)])))^0.5);
  active[i,1]<- mean(as.matrix(creditcard[i,c(13:18)]));
  stab1[i,1]<- ifelse(is.nan(1-median(as.matrix(creditcard[i,c(13:18)]))/mean(as.matrix(creditcard[i,c(13:18)]))),0,1-median(as.matrix(creditcard[i,c(13:18)]))/mean(as.matrix(creditcard[i,c(13:18)])));
  
}
cor(active,creditcard$Y)


creditcard <- cbind(creditcard,stab);creditcard <- cbind(creditcard,active);creditcard <- cbind(creditcard,abs(stab1)^0.11)
creditcard <- subset(creditcard,select = -c(13:18))

new_train <- creditcard[index,];new_test = creditcard[-index,];
new_train <- subset(new_train,select = -ID);new_test <- subset(new_test,select = -ID);



####unbalanced solution####
table(new_train$Y)
roc.curve(train$Y,Ypred)
Obalanced_train <- ovun.sample(Y~.,data = new_train,method="over",N=37260)$data
table(Obalanced_train$Y)
Ubalanced_train <- ovun.sample(Y~.,data = new_train,method="under",N=10740)$data
table(Ubalanced_train$Y)
Bbalanced_train <- ovun.sample(Y~.,data = new_train,method="both",N=24000)$data
table(Bbalanced_train$Y)
Rbalanced_train <- ROSE(Y~.,data = new_train)$data
table(Rbalanced_train$Y)

####models####
#first two lasso are just for fun...
lambdas <- seq(0,0.2,length=200)
baseline1 <- glmnet(as.matrix(train[,c(2:24)]),as.matrix(train[,25]),family = "gaussian",alpha=1,lambda=lambdas,standardize = TRUE)
plot(baseline1,label = "true") 
CVLASSO = cv.glmnet(as.matrix(train[,c(2:24)]),as.matrix(train[,25]),family = "gaussian",alpha=1,lambda=lambdas,standardize = TRUE);
CVLASSO$lambda.min
Yhat<-predict(baseline1,s=CVLASSO$lambda.min,newx = as.matrix(test[,c(2:24)]))
for(i in c(1:6000)){
  if(Yhat[i,1]<=0.5) {Yhat[i,1]<-0;}
  else {Yhat[i,1] <- 1;}
}
table(Yhat,as.matrix(test[,25]))

lambdas <- seq(0,3,length=200)
baseline2 <- glmnet(as.matrix(train[,c(7:12)]),as.matrix(train[,25]),family = "gaussian",alpha=1,lambda=lambdas,standardize = TRUE)
plot(baseline2,label = "true") 
CVLASSO = cv.glmnet(as.matrix(train[,c(7:12)]),as.matrix(train[,25]),family = "gaussian",alpha=1,lambda=lambdas,standardize = TRUE);
CVLASSO$lambda.min
Yhat<-predict(baseline2,s=CVLASSO$lambda.min,newx = as.matrix(test[,c(7:12)]))
for(i in c(1:6000))
{
  if(Yhat[i,1]<=0.45) {Yhat[i,1]<-0;}
  else {Yhat[i,1] <- 1;}
}
table(Yhat,as.matrix(test[,25]))

spline <- polyclass(new_train[,18],new_train[,c(1:17,19,20,21)])
spline
Yhat<-cpolyclass(new_test[,c(1:17,19,20,21)],spline)
Ypred<-cpolyclass(new_train[,c(1:17,19,20,21)],spline)
table(Yhat,as.matrix(test$Y))

splineO <- polyclass(Obalanced_train[,18],Obalanced_train[,c(1:17,19,20,21)],cv=4)
YhatO <- cpolyclass(new_test[,c(1:17,19,20,21)],splineO)
table(YhatO,as.matrix(test$Y))
splineU <- polyclass(Ubalanced_train[,18],Ubalanced_train[,c(1:17,19,20,21)])
YhatU <- cpolyclass(new_test[,c(1:17,19,20,21)],splineU)
table(YhatU,as.matrix(test$Y))
splineB <- polyclass(Bbalanced_train[,18],Bbalanced_train[,c(1:17,19,20,21)],cv=4)
YhatB <- cpolyclass(new_test[,c(1:17,19,20,21)],splineB)
table(YhatB,as.matrix(test$Y))
splineR <- polyclass(Rbalanced_train[,18],Rbalanced_train[,c(1:17,19,20,21)],cv=4)
YhatR <- cpolyclass(new_test[,c(1:17,19,20,21)],splineR)
table(YhatR,as.matrix(test$Y))
roc.curve(test$Y,Yhat)
table(test$Y,Yhat)
roc.curve(test$Y,YhatO)
roc.curve(test$Y,YhatU)
roc.curve(test$Y,YhatB)
roc.curve(test$Y,YhatR)

Y_ <- cbind(Yhat,YhatB,YhatO,YhatR,YhatU,Yhat)
for(i in c(1:6000)){
  Y_[i,6]<- round(sum(Y_[i,c(1:5)])/5,0)
}
roc.curve(test$Y,Y_[,6])
table(Y_[,6],as.matrix(test$Y))
par(mfrow = c(2,2))
roc.curve(test$Y,YhatO,main="SplineO",sub="area under the curve:0.729")
roc.curve(test$Y,YhatU,main="SplineU",sub="area under the curve:0.720")
roc.curve(test$Y,YhatB,main="SplineB",sub="area under the curve:0.724")
roc.curve(test$Y,YhatR,main="SplineR",sub="area under the curve:0.698")

