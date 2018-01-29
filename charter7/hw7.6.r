#7.6
data7.6=read.csv("ex7.6.CSV")

cor(data7.6)

lm7.6<-lm(y~., data = data7.6)
summary(lm7.6)

library(car)
vif(lm7.6)

##backward
lm7.6.back<-step(lm7.6, direction = "backward")
summary(lm7.6.back)
vif(lm7.6.back)

##step.both
lm7.6.both<-step(lm7.6, direction = "both")
summary(lm7.6.both)
vif(lm7.6.both)


##岭回归
datas<-data.frame(scale(data7.6))
library(MASS)
ridge7.6<-lm.ridge(y~.-1, data = datas, lambda = seq(0,1,0.05))
sbeta<-coef(ridge7.6)
write.csv(sbeta, "sbeta7.6.csv")
k=ridge7.6$lambda
jpeg("7.6.jpeg", height=500, width=700, quality = 200)
plot(k, k, type = "n", xlab = "岭参数k", ylab = "岭回归系数", ylim = c(-0.5,1))          
for(i in 1:4){
  lines(k, sbeta[,i], type = "o", lty=i, pch=i+17, cex=0.75)
}
legend(0.8,0.7, inset=0.5, legend=colnames(datas[,2:5]), cex=0.8, pch=18:21, lty=1:4)
dev.off()


##删除x4
ridge7.6_4<-lm.ridge(y~.-x4-1, data = datas, lambda = seq(0,1,0.05))
sbeta_4<-coef(ridge7.6_4)
write.csv(sbeta_4, "sbeta7.6_4.csv")
k=ridge7.6_4$lambda
jpeg("7.6_4.jpeg", height=500, width=700, quality = 200)
plot(k, k, type = "n", xlab = "岭参数k", ylab = "岭回归系数", ylim = c(-0.5,1))          
for(i in 1:3){
  lines(k, sbeta_4[,i], type = "o", lty=i, pch=i+17, cex=0.75)
}
legend(0.9,-0.3, inset=0.5, legend=colnames(datas[,2:4]), cex=0.8, pch=18:20, lty=1:3)
dev.off()

#取k=0.4
beta=c(Intercept=0, sbeta_4[rownames(sbeta_4)=="0.40",])
xx1=data7.6[,-5]  #
x.mean=apply(xx1, 2, mean)
x.var=apply(xx1, 2, var)
x.bar=sqrt((nrow(data7.6)-1)*x.var)
for(i in 2:length(beta)){
  beta[i]=beta[i]*x.bar[1]/x.bar[i]
}
beta[1]=x.mean[1]-x.mean[-1]%*%beta[-1]
beta
library(ridge)
summary(linearRidge(y~.-x4-1, data=datas, lambda=0.4, scaling = "corrForm"))


##删除x3
ridge7.6_3<-lm.ridge(y~.-x3-1, data = datas, lambda = seq(0,1,0.05))
sbeta_3<-coef(ridge7.6_3)
write.csv(sbeta_3, "sbeta7.6_3.csv")
k=ridge7.6_3$lambda
jpeg("7.6_3.jpeg", height=500, width=700, quality = 200)
plot(k, k, type = "n", xlab = "岭参数k", ylab = "岭回归系数", ylim = c(-0.5,1))          
for(i in 1:3){
  lines(k, sbeta_3[,i], type = "o", lty=i, pch=i+17, cex=0.75)
}
legend(0.9,-0.3, inset=0.5, legend=colnames(datas[,-4]), cex=0.8, pch=18:20, lty=1:3)
dev.off()

#取k=0.4
beta=c(Intercept=0, sbeta_3[rownames(sbeta_3)=="0.40",])
xx1=data7.6[,-4]  #
x.mean=apply(xx1, 2, mean)
x.var=apply(xx1, 2, var)
x.bar=sqrt((nrow(data7.6)-1)*x.var)
for(i in 2:length(beta)){
  beta[i]=beta[i]*x.bar[1]/x.bar[i]
}
beta[1]=x.mean[1]-x.mean[-1]%*%beta[-1]
beta
library(ridge)
summary(linearRidge(y~.-x3-1, data=datas, lambda=0.40, scaling = "corrForm"))




#### principal components analysis ################
conomy=data7.6[,-5]
conomy.pr=princomp(~x1+x2+x3, data=conomy, cor=T)
summary(conomy.pr, loadings=TRUE)

#### principal components regression ##############
pre=predict(conomy.pr)
conomy$F1=pre[,1]
conomy$F2=pre[,2]
lm.sol=lm(y~F1+F2, data=conomy)
summary(lm.sol)

#### transformation ###############################
beta=coef(lm.sol)
A=loadings(conomy.pr)
x.bar=conomy.pr$center
x.sd=conomy.pr$scale
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