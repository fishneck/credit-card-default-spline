library(class)
library(e1071)
library(MASS)
library(tidyverse)
library(xgboost)
library(ROSE)

setwd("D:/大學課程/大四上/統計學習/final project")
credit=read.csv("default of credit card clients.csv")
credit=credit[-1,]
indx <- sapply(credit, is.factor)
credit[indx] <- lapply(credit[indx], function(x) as.numeric(as.character(x)))
credit<-as.data.frame(credit)
set.seed(22)
index = sample(1:nrow(credit),ceiling(0.8*nrow(credit)))
train = credit[index,]
test = credit[-index,]

#naive bayse
nb_model <- naiveBayes(factor(Y)~., data = credit, subset = index)
nb_pred<-predict(nb_model, test)
table(nb_pred, test$Y)
mean(nb_pred==test$Y)#0.7251667
roc.curve(nb_pred, test$Y, main="naive bayse ROC curve", sub="Area under the curve (AUC):0.650")

#logistic regression
log_m<-glm(Y~., data = credit, family = binomial, subset = index)
log_yes<-predict(log_m,test, type = "response")
log_no <- 1-log_yes
log_prob <- as.factor(ifelse(log_no>log_yes, 0, 1))
table(log_prob,test$Y)
mean(log_prob==test$Y)#0.8066667
roc.curve(log_prob, test$Y, main="logistic regression ROC curve", sub="Area under the curve (AUC): 0.755")

#LDA
model_lda <- lda(Y~., data = credit, subset = index)
lda_pred<- predict(model_lda,test)
table(lda_pred$class, test$Y)
mean(lda_pred$class==test$Y)#0.8071667
roc.curve(lda_pred$class, test$Y, main="LDA ROC curve",sub="Area under the curve (AUC): 0.749")

#QDA
model_qda <- qda(Y~., data = credit, subset = index)
qda_pred<- predict(model_qda,test)
table(qda_pred$class, test$Y)
mean(qda_pred$class==test$Y)#0.5548333
roc.curve(qda_pred$class, test$Y, main="QDA ROC curve",sub="Area under the curve (AUC): 0.600")

#KNN
set.seed(1)
knn.pred=knn(train[,-25],test[,-25],train[,25],k=1,prob = T)
table(knn.pred, test$Y)
mean(knn.pred==test$Y)#0.69
roc.curve(knn.pred, test$Y,main="KNN K=1 ROC curve", sub="Area under the curve (AUC): 0.551")

set.seed(1)
knn.pred0=knn(train[,-25],test[,-25],train[,25],k=10,prob = T)
table(knn.pred0, test$Y)
mean(knn.pred0==test$Y)#0.7566667
roc.curve(knn.pred0, test$Y,main="KNN K=10 ROC curve", sub="Area under the curve (AUC): 0.584")
par(mfrow=c(2,3))
#xgboost
xgb_traindata = xgb.DMatrix(data = as.matrix(train[,-25]) , label = train[,25])
xgb_testdata  = xgb.DMatrix(data = as.matrix(test[,-25]) , label = test[,25])

model=xgboost(data = xgb_traindata, max.depth = 4,
              eta = 1, nthread = 2, nrounds = 10,objective = "binary:logistic")
importance <- xgb.importance(feature_names = NULL, model = model_xgb)
head(importance)
xgb.plot.importance(importance_matrix = importance)

casecredit1<-credit %>% select(c(7,8,14,15,25))
casecredit1<-as.data.frame(casecredit1)

set.seed(22)
index1 = sample(1:nrow(casecredit1),ceiling(0.8*nrow(casecredit1)))
train1 = casecredit1[index1,]
test1 = casecredit1[-index1,]

#naive bayse
nb_model1 <- naiveBayes(factor(Y)~., data = casecredit1, subset = index1)
nb_pred1<-predict(nb_model1, test1)
table(nb_pred1, test1$Y)
mean(nb_pred1==test1$Y)#0.802
roc.curve(nb_pred1, test1$Y, main="naive bayse ROC curve",sub="Area under the curve (AUC):0.712")


#logistic regression
log_m1<-glm(Y~., data = casecredit1, family = binomial, subset = index1)
log1_yes<-predict(log_m1,test1, type = "response")
log1_no <- 1-log1_yes
log_prob1 <- as.factor(ifelse(log1_no>log1_yes, 0, 1))
table(log_prob1,test1$Y)
mean(log_prob==test$Y)#0.8066667
roc.curve(log_prob1, test1$Y, main="logistic regression ROC curve",sub="Area under the curve (AUC):0.763")

#LDA
model_lda1 <- lda(Y~., data = casecredit1, subset = index1)
lda_pred1<- predict(model_lda1,test1)
table(lda_pred1$class, test1$Y)
mean(lda_pred1$class==test1$Y)#0.8091667
roc.curve(lda_pred1$class, test1$Y, main="LDA ROC curve",sub="Area under the curve (AUC):0.762")

#QDA
model_qda1 <- qda(Y~., data = casecredit1, subset = index1)
qda_pred1<- predict(model_qda1,test1)
table(qda_pred1$class, test1$Y)
mean(qda_pred1$class==test1$Y)#0.7806667
roc.curve(qda_pred1$class, test1$Y, main="QDA ROC curve",sub="Area under the curve (AUC):0.681")

#KNN
set.seed(1)
knn.pred1=knn(train1[,-5],test1[,-5],train1[,5],k=1,prob = T)
knn.pred1=knn(train1[,-11],test1[,-11],train1[,11],k=1,prob = T)
table(knn.pred1, test1$Y)
mean(knn.pred1==test1$Y)#0.6836667
roc.curve(knn.pred1, test1$Y, main="KNN K=1 ROC curve",sub="Area under the curve (AUC):0.529")

#k=10
set.seed(1)
knn.pred10=knn(train1[,-5],test1[,-5],train1[,5],k=10,prob = T)
knn.pred10=knn(train1[,-11],test1[,-11],train1[,11],k=10,prob = T)
table(knn.pred10, test1$Y)
mean(knn.pred10==test1$Y)#0.7615
roc.curve(knn.pred10, test1$Y, main="KNN K=10 ROC curve",sub="Area under the curve (AUC):0.573")

#lm
a<-lm(Y~., data = credit, subset = index)
summary(a)

casecredit2<-credit %>% select(c(2,4:8,13,19,25))
casecredit2<-as.data.frame(casecredit2)

set.seed(22)
index2 = sample(1:nrow(casecredit2),ceiling(0.8*nrow(casecredit2)))
train2 = casecredit2[index2,]
test2 = casecredit2[-index2,]

#naive bayse
nb_model2 <- naiveBayes(factor(Y)~., data = casecredit2, subset = index2)
nb_pred2<-predict(nb_model2, test2)
table(nb_pred2, test2$Y)
mean(nb_pred2==test2$Y)#0.7998333
roc.curve(nb_pred2, test2$Y, main="naive bayse ROC curve",sub="Area under the curve (AUC):0.708")

#logistic regression
log_m2<-glm(Y~., data = casecredit2, family = binomial, subset = index2)
log2_yes<-predict(log_m2,test2, type = "response")
log2_no <- 1-log2_yes
log_prob2 <- as.factor(ifelse(log2_no>log2_yes, 0, 1))
table(log_prob2,test2$Y)
mean(log_prob2==test2$Y)#0.8073333
roc.curve(log_prob2, test2$Y, main="logistic regression ROC curve",sub="Area under the curve (AUC):0.757")

#LDA
model_lda2 <- lda(Y~., data = casecredit2, subset = index2)
lda_pred2<- predict(model_lda2,test2)
table(lda_pred2$class, test2$Y)
mean(lda_pred2$class==test2$Y)#0.8086667
roc.curve(lda_pred2$class, test2$Y, main="LDA ROC curve",sub="Area under the curve (AUC):0.752")

#QDA
model_qda2 <- qda(Y~., data = casecredit2, subset = index2)
qda_pred2<- predict(model_qda2,test2)
table(qda_pred2$class, test2$Y)
mean(qda_pred2$class==test2$Y)#0.7763333
roc.curve(qda_pred2$class, test2$Y, main="QDA ROC curve",sub="Area under the curve (AUC):0.679")

#KNN
set.seed(1)
knn.pred2=knn(train2[,-9],test2[,-9],train2[,9],k=1,prob = T)
table(knn.pred2, test2$Y)
mean(knn.pred2==test2$Y)#0.6736667
roc.curve(knn.pred2, test2$Y, main="KNN K=1 ROC curve",sub="Area under the curve (AUC):0.534")
#k=10
set.seed(1)
knn.pred20=knn(train2[,-9],test2[,-9],train2[,9],k=10,prob = T)
table(knn.pred20, test2$Y)
mean(knn.pred20==test2$Y)#0.7583333
roc.curve(knn.pred20, test2$Y, main="KNN K=10 ROC curve",sub="Area under the curve (AUC):0.584")

#rose
Obalanced_train <- ovun.sample(Y~.,data = train,method="over",N=18705*2)$data

#naive bayse
nb_modelr <- naiveBayes(factor(Y)~., data = Obalanced_train)
nb_predr<-predict(nb_modelr, test)
table(nb_predr, test$Y)
mean(nb_predr==test$Y)#0.5025
roc.curve(nb_predr, test$Y, main="naive bayse ROC curve",sub="Area under the curve (AUC):0.588")

#logistic regression
log_mr<-glm(Y~., data = Obalanced_train, family = binomial)
logr_yes<-predict(log_mr,test, type = "response")
logr_no <- 1-logr_yes
logr_prob <- as.factor(ifelse(logr_no>logr_yes, 0, 1))
table(logr_prob,test$Y)
mean(logr_prob==test$Y)#0.6836667
roc.curve(logr_prob, test$Y, main="logistic regression ROC curve",sub="Area under the curve (AUC):0.626")

#LDA
model_ldar <- lda(Y~., data = Obalanced_train)
lda_predr<- predict(model_ldar,test)
table(lda_predr$class, test$Y)
mean(lda_predr$class==test$Y)#0.6946667
roc.curve(lda_predr$class, test$Y, main="LDA ROC curve",sub="Area under the curve (AUC):0.630")

#QDA
model_qdar <- qda(Y~., data = Obalanced_train)
qda_predr<- predict(model_qdar,test)
table(qda_predr$class, test$Y)
mean(qda_predr$class==test$Y)#0.3961667
roc.curve(qda_predr$class, test$Y, main="QDA ROC curve",sub="Area under the curve (AUC):0.578")

#KNN
set.seed(1)
knn.predr=knn(Obalanced_train[,-25],test[,-25],Obalanced_train[,25],k=1,prob = T)
table(knn.predr, test$Y)
mean(knn.predr==test$Y)#0.6926667
roc.curve(knn.predr, test$Y, main="KNN K=1 ROC curve",sub="Area under the curve (AUC):0.552")

set.seed(1)
knn.pred0r=knn(Obalanced_train[,-25],test[,-25],Obalanced_train[,25],k=10,prob = T)
table(knn.pred0r, test$Y)
mean(knn.pred0r==test$Y)#0.5595
roc.curve(knn.pred0r, test$Y, main="KNN K=10 ROC curve",sub="Area under the curve (AUC):0.554")

#xgboost
Obalanced_train1 <- ovun.sample(Y~.,data = train1,method="over",N=18705*2)$data

#naive bayse
nb_model1r <- naiveBayes(factor(Y)~., data = Obalanced_train1)
nb_pred1r<-predict(nb_model1r, test1)
table(nb_pred1r, test1$Y)
mean(nb_pred1r==test1$Y)#0.78
roc.curve(nb_pred1r, test1$Y, main="naive bayse ROC curve",sub="Area under the curve (AUC):0.684")

#logistic regression
log_m1r<-glm(Y~., data = Obalanced_train1, family = binomial)
log1r_yes<-predict(log_m1r,test1, type = "response")
log1r_no <- 1-log1r_yes
log_prob1r <- as.factor(ifelse(log1r_no>log1r_yes, 0, 1))
table(log_prob1r,test1$Y)
mean(log_prob1r==test1$Y)#0.7528333
roc.curve(log_prob1r, test1$Y, main="logistic regression ROC curve",sub="Area under the curve (AUC):0.657")

#LDA
model_lda1r <- lda(Y~., data = Obalanced_train1)
lda_pred1r<- predict(model_lda1r,test1)
table(lda_pred1r$class, test1$Y)
mean(lda_pred1r$class==test1$Y)#0.7655
roc.curve(lda_pred1r$class, test1$Y, main="LDA ROC curve",sub="Area under the curve (AUC):0.666")

#QDA
model_qda1r <- qda(Y~., data = Obalanced_train1)
qda_pred1r<- predict(model_qda1r,test1)
table(qda_pred1r$class, test1$Y)
mean(qda_pred1r$class==test1$Y)#0.7683333
roc.curve(qda_pred1r$class, test1$Y, main="QDA ROC curve",sub="Area under the curve (AUC):0.671")
par(mfrow=c(2,2))

#lm
Obalanced_train2 <- ovun.sample(Y~.,data = train2,method="over",N=18705*2)$data

#naive bayse
nb_model2r <- naiveBayes(factor(Y)~., data = Obalanced_train2)
nb_pred2r<-predict(nb_model2r, test2)
table(nb_pred2r, test2$Y)
mean(nb_pred2r==test2$Y)#0.596
roc.curve(nb_pred2r, test2$Y, main="naive bayse ROC curve",sub="Area under the curve (AUC):0.600")

#logistic regression
log_m2r<-glm(Y~., data = Obalanced_train2, family = binomial)
log2r_yes<-predict(log_m2r,test2, type = "response")
log2r_no <- 1-log2r_yes
log_prob2r <- as.factor(ifelse(log2r_no>log2r_yes, 0, 1))
table(log_prob2r,test2$Y)
mean(log_prob2r==test2$Y)#0.688
roc.curve(log_prob2r, test2$Y, main="logistic regression ROC curve",sub="Area under the curve (AUC):0.622")

#LDA
model_lda2r <- lda(Y~., data = Obalanced_train2)
lda_pred2r<- predict(model_lda2r,test2)
table(lda_pred2r$class, test2$Y)
mean(lda_pred2r$class==test2$Y)#0.6913333
roc.curve(lda_pred2r$class, test2$Y, main="LDA ROC curve",sub="Area under the curve (AUC):0.623")

#QDA
model_qda2r <- qda(Y~., data = Obalanced_train2)
qda_pred2r<- predict(model_qda2r,test2)
table(qda_pred2r$class, test2$Y)
mean(qda_pred2r$class==test2$Y)#0.5853333
roc.curve(qda_pred2r$class, test2$Y, main="QDA ROC curve",sub="Area under the curve (AUC):0.599")

