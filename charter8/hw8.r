data5.5<-read.csv("data5.5.CSV")
pr5.5<-princomp(~., data=data5.5[,1:4], cor=T)
summary(pr5.5, loadings = TRUE)
pr5.5$scores[,1:2]

pre=predict(pr5.5)
data5.5$F1=pre[,1]
data5.5$F2=pre[,2]
lm.sol=lm(y~F1+F2, data=data5.5)
summary(lm.sol)

beta=coef(lm.sol)
A=loadings(pr5.5)
x.bar=pr5.5$center
x.sd=pr5.5$scale
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
library(pls)
data5.5<-read.csv("data5.5.CSV")
datas<-data.frame(scale(data5.5))
pls1<-plsr(y~., data=datas, validation="LOO", jackknife=TRUE, method="widekernelpls")
summary(pls1, what = "all")

pls3<-plsr(y~., data=datas, ncomp=3, validation="LOO", jackknife=TRUE)
coef(pls3)
beta=c(Intercept=0, x=t(data.frame(coef(pls3))))
xx1=data5.5[,c(5,1:4)]
x.mean=apply(xx1, 2, mean)
x.var=apply(xx1, 2, var)
x.bar=sqrt((nrow(data5.5)-1)*x.var)
for(i in 2:length(beta)){
  beta[i]=beta[i]*x.bar[1]/x.bar[i]
}
beta[1]=x.mean[1]-x.mean[-1]%*%beta[-1]
beta
