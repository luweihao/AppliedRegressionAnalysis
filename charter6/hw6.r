##6.5
mydata6.5<-read.csv("data6.5.csv",header = TRUE)
data6.5<-mydata6.5[,-1]
library(car)
lm6.5<-lm(y~., data=data6.5)
vif(lm6.5)

lm6.5_drop2<-lm(y~.-x2, data=data6.5)
summary(lm6.5_drop2)
vif(lm6.5_drop2)

lm6.5_drop12<-lm(y~.-x1-x2, data=data6.5)
summary(lm6.5_drop12)
vif(lm6.5_drop12)

lm6.5_drop126<-lm(y~.-x1-x2-x6, data=data6.5)
summary(lm6.5_drop126)
vif(lm6.5_drop126)

lm6.5_drop1236<-lm(y~.-x1-x2-x3-x6, data=data6.5)
summary(lm6.5_drop1236)
vif(lm6.5_drop1236)
 #x3²»ÏÔÖø

##6.6
data5.9<-read.csv("ex5.9.csv",header = TRUE)
library(car)
lm5.9<-lm(y~., data=data5.9)
vif(lm5.9)

XX<-cor(data5.9[,1:6])
## kappa
kappa(XX, exact = TRUE)
## eigen
eigen(XX)

lm5.9_drop2<-lm(y~.-x2, data=data5.9)
summary(lm5.9_drop2)
vif(lm5.9_drop2)

lm5.9_drop25<-lm(y~.-x2-x5, data=data5.9)
summary(lm5.9_drop25)
vif(lm5.9_drop25)

lm5.9_drop125<-lm(y~.-x1-x2-x5, data=data5.9)
summary(lm5.9_drop125)
vif(lm5.9_drop125)

lm5.9_125<-lm(y~x1+x2+x5, data=data5.9)
vif(lm5.9_125)
