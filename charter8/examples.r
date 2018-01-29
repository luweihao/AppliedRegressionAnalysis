data3.3<-read.csv("data3.3.CSV")
pr3.3<-princomp(~., data=data3.3[,1:4], cor=T)
summary(pr3.3, loadings = TRUE)
pr3.3$scores[,1:2]

pre=predict(pr3.3)
data3.3$F1=pre[,1]
data3.3$F2=pre[,2]
lm.sol=lm(y~F1+F2, data=data3.3)
summary(lm.sol)

beta=coef(lm.sol)
A=loadings(pr3.3)
x.bar=pr3.3$center
x.sd=pr3.3$scale
if(length(beta)==2){
  coef=(beta[2]*A[,1])/x.sd
}
if(length(beta)>=3){
  coef=beta[2]*A[,1]
  for(i in 2:(length(beta)-1)){
    coef=coef + beta[i+1]*A[,i]
  }
  coef=coef/x.sd
}
beta0=beta[1]-sum(x.bar*coef)
c(beta0, coef)


rm(list = ls())
## partial least squared
data3.3<-read.csv("data3.3.CSV")
datas<-data.frame(scale(data3.3))
library(pls)
pls1<-plsr(y~., data=datas, validation="LOO", jackknife=TRUE, method="widekernelpls")
summary(pls1, what = "all")

pls3<-plsr(y~., data=datas, ncomp=3, validation="LOO", jackknife=TRUE)
coef(pls3)
