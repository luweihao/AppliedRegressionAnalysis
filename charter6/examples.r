data3.3<-read.csv("data3.3.csv",header = TRUE)
library(car)
lm3.3<-lm(y~.,data=data3.3)

## vif
vif3.3<-vif(lm3.3)
vif3.3
 #>10 严重
mean(vif3.3)
 #远远>1 严重
cor(data3.3$x1, data3.3$x2)

XX<-cor(data3.3[,2:6])
## eigen
eigen(XX)
## kappa
kappa(XX, exact = TRUE)
 #0<.<100    很小
 #100<.<1000 较强
 #.>1000     严重

## 标准化回归方程
library(QuantPsyc)
lm.beta(lm3.3)