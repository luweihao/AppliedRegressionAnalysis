### 9.3
data9.3=read.csv("data9.3.csv")
attach(data9.3)
ly=log(y)
lx=1/x
lm9.3=lm(ly~lx)
summary(lm9.3)
beta=coef(lm9.3)
c(exp(beta[1]), beta[2])
yhat=exp(fitted(lm9.3))
rss_lm=t(y-yhat) %*% (y-yhat)
rss_lm
nls9.3=nls(y~a*exp(b/x), start=list(a=0.02,b=6))
summary(nls9.3)
SE=deviance(nls9.3)
SST=sum((y-mean(y))^2)
Rsquare=1-SE/SST
Rsquare
detach(data9.3)




rm(list = ls())
### 9.4
data9.4=read.csv("data9.4.csv")
attach(data9.4)
ly=log(1/y-1/100)
lm9.4=lm(ly~t)
summary(lm9.4)
beta=coef(lm9.4)
c(exp(beta[1]), exp(beta[2]))
yhat=exp(fitted(lm9.4))
e_lm=t(y-yhat) %*% (y-yhat)
e_lm
nls9.4=nls(y~1/(1/u+b0*b1^t), start=list(u=100,b0=0.16,b1=0.77))
summary(nls9.4)
SE=deviance(nls9.4)
SST=sum((y-mean(y))^2)
Rsquare=1-SE/SST
Rsquare
detach(data9.4)




rm(list = ls())
### 9.5
data9.5=read.csv("data9.5.csv")
attach(data9.5)
ly=log(GDP)
lk=log(k)
ll=log(l)
lm9.5=lm(ly~lk+ll)
summary(lm9.5)
beta=coef(lm9.5)
c(exp(beta[1]), beta[2], beta[3])
yhat=exp(fitted(lm9.5))
e_lm=t(GDP-yhat) %*% (GDP-yhat)
e_lm
nls9.5=nls(GDP~A*((k^a)*(l^b)), start=list(A=0.17,a=0.8,b=0.4),
           lower=c(0,0,0),upper=c(100,10,10),algorithm="port",
           control=nls.control(maxiter=1000,tol=1e-1000))
summary(nls9.5)
SE=deviance(nls9.5)
SST=sum((GDP-mean(GDP))^2)
Rsquare=1-SE/SST
Rsquare

detach(data9.5)


#DW
library(lmtest)
dwtest(lm9.5,alternative = "two.sided")
#迭代法
rho<-1-0.71544/2
n<-length(ly)
y1<-c(0,ly[1:(n-1)])
yy<-(ly-rho*y1)[2:n]
lk1<-c(0,lk[1:(n-1)])
lk2<-(lk-rho*lk1)[2:n]
ll1<-c(0,ll[1:(n-1)])
ll2<-(ll-rho*ll1)[2:n]

lm9.5new<-lm(yy~lk2+ll2)
summary(lm9.5new)

dwtest(lm9.5new,alternative = "two.sided")

beta1=coef(lm9.5new)
c(exp(beta1[1]), beta1[2], beta1[3])


#vif
library(car)
vif(lm9.5)
kappa(lm9.5)

##岭回归
data9.5ridge=cbind(ly,lk,ll)
datas<-data.frame(scale(data9.5ridge))
library(MASS)
ridge9.5<-lm.ridge(ly~lk+ll-1, data = datas, lambda = seq(0,7,0.1))
sbeta<-coef(ridge9.5)
write.csv(sbeta, "sbeta9.5.csv")
k=ridge9.5$lambda
jpeg("9.5.jpeg", height=400, width=700, quality = 100)
plot(k, k, type = "n", xlab = "岭参数k", ylab = "岭回归系数", ylim = c(-0.5,1))          
for(i in 1:2){
  lines(k, sbeta[,i], type = "o", lty=i, pch=i+17, cex=0.75)
}
legend(6.5,-0.3, inset=0.5, legend=colnames(datas[,2:3]), cex=0.8, pch=18:19, lty=1:2)
dev.off()

#取k=5
beta2=c(Intercept=0, sbeta[rownames(sbeta)=="5.0",])
xx1=data9.5ridge  #
x.mean=apply(xx1, 2, mean)
x.var=apply(xx1, 2, var)
x.bar=sqrt((nrow(data9.5ridge)-1)*x.var)
for(i in 2:length(beta2)){
  beta2[i]=beta2[i]*x.bar[1]/x.bar[i]
}
beta2[1]=x.mean[1]-x.mean[-1]%*%beta2[-1]
c(exp(beta2[1]), beta2[2], beta2[3])

library(ridge)
summary(linearRidge(ly~lk+ll-1, data=datas, lambda=5, scaling="scale"))
