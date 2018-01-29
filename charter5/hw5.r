##5.8 逐步回归
data3.3<-read.csv("data3.3.csv",header = TRUE)
lm3.3<-lm(y~., data=data3.3)
lm3.3.both<-step(lm3.3, direction = "both")
summary(lm3.3.both)

##5.9
data5.9<-read.csv("ex5.9.csv",header = TRUE)
lm5.9<-lm(y~., data=data5.9)
##后退法
lm5.9.back<-step(lm5.9, direction = "backward")
summary(lm5.9.back)
## 逐步回归
lm5.9.both<-step(lm5.9, direction = "both")
summary(lm5.9.both)

##5.10
data5.10<-read.csv("ex5.10.csv",header = TRUE)
lm5.10<-lm(y~., data=data5.10)
summary(lm5.10)
cor(data5.10)
##后退法
lm5.10.back<-step(lm5.10, direction = "backward")
summary(lm5.10.back)
## 逐步回归
lmo5.10<-lm(y~1,data=data5.10)
lm5.10.both<-step(lmo5.10,scope=list(upper=~x2+x3+x4+x5+x6
                  ,lower=~1) ,direction = "both")
summary(lm5.10.both)
