data <- read.table("data.csv", header = TRUE, sep = ",")
data <- data[,-1]
data$Y <- factor(data$Y)
set.seed(22)
index <- sample(1:nrow(data), ceiling(0.8*nrow(data)), replace=F)
data.train <- data[index,]
data.test <- data[-index,]
summary(data.train)[,1]

##### Y #####
sum(data$Y==1)
sum(data$Y==1)
##### Sex X2 #####
sum(data$X2==1)
sum(data$X2==2)
##### Education X3 #####
hist(data$X3,main="Histogram of Education",xlab="Education Level",xaxt="n")
axis(1, at=c(0:6), label=c("unknown","Graduate School","University","High school","Others","unknown","unknown"), cex.axis=0.7)
##### Marriage X4 #####
hist(data$X4,main="Histogram of Marriage",xlab="Marriage Level", xaxt="n")
axis(1, at=c(0:3), label=c("unknown","Married","Single","others"))
##### Age X5 #####
max(data$X5)
min(data$X5)
a <- c(1:6)
b <- c(1:6)
count <- 1
for(i in c(0:5)){
  X5_index <- which(data$X5>=(20+i*10) & data$X5<(30+i*10))
  default_rate <- (summary(data[X5_index,]$Y)/length(X5_index))[2]
  a[count] <- default_rate
  b[count] <- length(X5_index)
  count <- count + 1
}
plot(c(1:6), a, type="b", xlab="Age", ylab="Default Rate", main="Default vs Age", xaxt="n", yaxt="n")
axis(1, at=c(1:6), label=c("20-30","30-40","40-50","50-60","60-70","70-80"))
axis(2, a)
c <- hist(data$X5, breaks=12, main="Histogram of Age", xlab="Age")
c$counts
##### Status in 9 #####
a <- c(-2:9)
count <- 1
for(i in c(-2:9)){
  a[count] <- sum(data$X6==i) 
  count <- count + 1
}
frame <- data.frame(status = factor(c(-2:9)), Frequency = a)
ggplot(frame, aes(x = status, y = Frequency)) + geom_bar(stat = "identity")
sum(data$X6==(-1))
c$counts
##### Bill in 9 #####
max(data$X12)
min(data$X12)
mean(data$X12)
l <- seq(-170000,970000,10000)
c <- hist(data$X12,breaks=l,main="Histogram of Bill in Sep",xlab="Bill")
bill_index <- which(data$X12>(-15000) & data$X12<435000)
l <- seq(-15000,435000,10000)
c <- hist(data[bill_index,]$X12,breaks=l,main="Histogram of Bill in Sep(focus)",xlab="Bill")
##### Payment in 9 #####
max(data$X18)
min(data$X18)
mean(data$X18)
l <- seq(0,880000,10000)
c <- hist(data$X18,breaks=l,main="Histogram of Payment in Sep",xlab="Payment")
pay_index <- which(data$X18>0 & data$X18<100000)
l <- seq(0,100000,5000)
c <- hist(data[pay_index,]$X18,breaks=l,main="Histogram of Payment in Sep",xlab="Payment")

##### default data #####
default_index <- which(data$Y==1)
data_default <- data[default_index,]
##### sex #####
sum(data_default$X2==1)
sum(data_default$X2==2)
##### Education X3 #####
hist(data_default$X3,main="Histogram of Education",xlab="Education Level",xaxt="n")
axis(1, at=c(0:6), label=c("unknown","Graduate School","University","High school","Others","unknown","unknown"), cex.axis=0.7)
sum(data_default$X3==4)+sum(data_default$X3==5)+sum(data_default$X3==6)
##### Marriage X4 #####
hist(data_default$X4,main="Histogram of Marriage",xlab="Marriage Level", xaxt="n")
axis(1, at=c(0:3), label=c("unknown","Married","Single","others"))
sum(data_default$X4==0)
sum(data_default$X4==1)
sum(data_default$X4==2)
sum(data_default$X4==3)
##### Age X5 #####
max(data_default$X5)
min(data_default$X5)
a <- c(1:6)
b <- c(1:6)
count <- 1
for(i in c(0:5)){
  X5_index <- which(data_default$X5>=(20+i*10) & data_default$X5<(30+i*10))
  default_rate <- (summary(data_default[X5_index,]$Y)/length(X5_index))[2]
  a[count] <- default_rate
  b[count] <- length(X5_index)
  count <- count + 1
}
plot(c(1:6), a, type="b", xlab="Age", ylab="Default Rate", main="Default vs Age", xaxt="n", yaxt="n")
axis(1, at=c(1:6), label=c("20-30","30-40","40-50","50-60","60-70","70-80"))
axis(2, a)
c <- hist(data_default$X5, breaks=12, main="Histogram of Age", xlab="Age")
c$counts
##### Status in 9 #####
c <- hist(data_default$X6,main="Histogram of Pay Status in Sep",xlab="Pay State",xaxt="n")
axis(1, at=c(-2:8), label=c("no consumption","duly","revolving credit","delay 1","delay 2","delay 3","delay 4","delay 5","delay 6","delay 7","delay 8"), cex.axis=0.4)
index <- which(data_default$X6==(0))
length(data_default[index,]$X6)
a <- c(-2:9)
count <- 1
for(i in c(-2:9)){
  a[count] <- sum(data_default$X6==i) 
  count <- count + 1
}
frame <- data.frame(status = factor(c(-2:9)), Frequency = a)
ggplot(frame, aes(x = status, y = Frequency)) + geom_bar(stat = "identity")
##### Bill in 9 #####
max(data_default$X12)
min(data_default$X12)
l <- seq(-10000,620000,10000)
c <- hist(data_default$X12,breaks=l,main="Histogram of Bill in Sep",xlab="Bill")
bill_index <- which(data_default$X12>(-10000) & data_default$X12<300000)
l <- seq(-10000,300000,10000)
c <- hist(data_default[bill_index,]$X12,breaks=l,main="Histogram of Bill in Sep(focus)",xlab="Bill")
##### Payment in 9 #####
max(data_default$X18)
min(data_default$X18)
l <- seq(0,300000,10000)
c <- hist(data_default$X18,breaks=l,main="Histogram of Payment in Sep",xlab="Payment")
pay_index <- which(data_default$X18>0 & data_default$X18<50000)
l <- seq(0,50000,5000)
c <- hist(data_default[pay_index,]$X18,breaks=l,main="Histogram of Payment in Sep",xlab="Payment")
