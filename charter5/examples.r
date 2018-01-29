data3.1<-read.csv("data3.1.csv",header = TRUE)
data5.5<-read.csv("data5.5.csv",header = TRUE)
library(leaps)

exps<-regsubsets(y~x1+x2+x3+x4+x5+x6+x7+x8+x9,data= data3.1,
                 nbest= 1,really.big=T)
expres<-summary(exps)

##adjr2
plot(exps, scale = "adjr2")
res.adjr2<-data.frame(expres$outmat, 调整R平方= expres$adjr2)
res.adjr2

##cp
plot(exps, scale = "Cp")
res.cp<-data.frame(expres$outmat, Cp= expres$cp)
res.cp

##step(scale=0 AIC, scale=1 Cp)

##step.forward
lmo3.1<-lm(y~1,data=data3.1)
lm3.1.for<-step(lmo3.1,scope=list(upper=~x1+x2+x3+x4+x5+x6+x7+x8+x9
                                  ,lower=~1) ,direction = "forward")
summary(lm3.1.for)

##step.backward
lm3.1<-lm(y~x1+x2+x3+x4+x5+x6+x7+x8+x9,data=data3.1)
lm3.1.back<-step(lm3.1, direction = "backward")
summary(lm3.1.back)

##step.both
lm5.5<-lm(y~.,data=data5.5)
lm5.5.both<-step(lm5.5, direction = "both")
summary(lm5.5.both)
