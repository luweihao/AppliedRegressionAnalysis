data3.3<-read.csv("data3.3.csv",header = TRUE)
library(car)
lm3.3<-lm(y~.,data=data3.3)

## vif
vif3.3<-vif(lm3.3)
vif3.3
 #>10 ����
mean(vif3.3)
 #ԶԶ>1 ����
cor(data3.3$x1, data3.3$x2)

XX<-cor(data3.3[,2:6])
## eigen
eigen(XX)
## kappa
kappa(XX, exact = TRUE)
 #0<.<100    ��С
 #100<.<1000 ��ǿ
 #.>1000     ����

## ��׼���ع鷽��
library(QuantPsyc)
lm.beta(lm3.3)