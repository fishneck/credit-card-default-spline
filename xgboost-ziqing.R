# team report 

#install.packages("xlsx")
library(xlsx)
#install.packages("gbm")
library(gbm)
#install.packages("xgboost")
library(xgboost)

library(readxl)
default_of_credit_card_clients <- read_excel("C:/Users/Jessica/Desktop/default of credit card clients.xls")
#View(default_of_credit_card_clients)

# install.packages("tidyverse")
# library(tidyverse)
credit=default_of_credit_card_clients
credit=credit[-1,]
str(credit)
head(credit)
#-----------------------divide the data---------------------------
set.seed(22)
index = sample(1:nrow(credit),ceiling(0.8*nrow(credit)))
train_xgboost1 = credit[index,]
test_xgboost1 = credit[-index,]

train_xgboost<-sapply(train_xgboost1,as.numeric)
test_xgboost<-sapply(test_xgboost1,as.numeric)

xgb_traindata = xgb.DMatrix(data = as.matrix(train_xgboost[,-25]) , label = train_xgboost[,25])
xgb_testdata  = xgb.DMatrix(data = as.matrix(test_xgboost[,-25]) , label = test_xgboost[,25])

###---------------------set parameter-----------------------
param = list(
  "objective" = "reg:linear",
  "eval_metric" = "rmse",
  eta = 0.1,                    #learn rate
  max_depth = 2,                #tree depth
  subsample = 1,                #percentage data use in every iteration
  colsample_bytree = 1,         #percentage covariate use in every iteration , other : colsample_bynode colsample_bylevel
  num_parallel_tree = 1  )      #if set this same as nrounds，and set subsample and colsample , will like Randomforest

#------------XG boost --------------

###xgb data

###start train!
model_xgb=xgb.train(
  params = param,
  data = xgb_traindata,
  nrounds = 200,                                                 #number of iteration
  print_every_n = 10,                                           #every n print rmse
  watchlist = list(train = xgb_traindata ,test = xgb_testdata  ) #caculate rmse
)

###predict
xgb_predic=predict(model_xgb,newdata =xgb_testdata)

xgb_predic[(xgb_predic>0.5)]=1
xgb_predic[(xgb_predic<=0.5)]=0

mean((test_xgboost[,25]-xgb_predic)^2)
mean((test_xgboost[,25]-xgb_predic)^2)^(1/2)

###see tree structure

xgb.plot.tree(model=model_xgb , trees = 0)

table1=table(xgb_predic,test_xgboost1$Y)
sum(diag(table1)/sum(table1))