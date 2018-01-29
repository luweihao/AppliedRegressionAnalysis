#7.5
data5.9=read.csv("ex5.9.fixed.CSV")
datas<-data.frame(scale(data5.9))
library(MASS)
ridge5.9<-lm.ridge(y~.-1, data = datas, lambda = seq(0,3,0.1))
sbeta<-coef(ridge5.9)
write.csv(sbeta, "sbeta.csv")
k=ridge5.9$lambda
jpeg("5.9.jpeg", height=400, width=400, quality = 100)
plot(k, k, type = "n", xlab = "岭参数k", ylab = "岭回归系数", ylim = c(-2,4))          
for(i in 1:3){
  lines(k, sbeta[,i], type = "o", lty=i, pch=i+17, cex=0.75)
}
legend(2.5,3, inset=0.5, legend=colnames(datas[,2:4]), cex=0.8, pch=1:3, lty=18:20)
dev.off()

#取k=0.5
beta=c(Intercept=0, sbeta[rownames(sbeta)=="0.5",])
xx1=data5.9  #
x.mean=apply(xx1, 2, mean)
x.var=apply(xx1, 2, var)
x.bar=sqrt((nrow(data5.9)-1)*x.var)
for(i in 2:length(beta)){
  beta[i]=beta[i]*x.bar[1]/x.bar[i]
}
beta[1]=x.mean[1]-x.mean[-1]%*%beta[-1]
beta
library(ridge)
summary(linearRidge(y~.-1, data=datas, lambda=0.5, scaling = "corrForm"))

