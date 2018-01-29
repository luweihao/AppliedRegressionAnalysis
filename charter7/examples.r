data3.3<-read.csv("data3.3.CSV")
datas<-data.frame(scale(data3.3[,1:6]))
library(MASS)
ridge3.3<-lm.ridge(y~.-1, data = datas, lambda = seq(0,3,0.1))
sbeta<-coef(ridge3.3)
write.csv(sbeta, "sbeta.csv")
k=ridge3.3$lambda
jpeg("3.3.jpeg", height=400, width=400, quality = 100)
plot(k, k, type = "n", xlab = "岭参数k", ylab = "岭回归系数", ylim = c(-2.5,2.5))          
for(i in 1:5){
  lines(k, sbeta[,i], type = "o", lty=i, pch=i+17, cex=0.75)
}
legend(2.5,2, inset=0.5, legend=colnames(datas[,2:6]), cex=0.8, pch=1:5, lty=18:22)
dev.off()

ridge13.3<-lm.ridge(y~.-x1-1, data = datas, lambda = seq(0,2,0.2))
sbeta1<-coef(ridge13.3)
write.csv(sbeta1, "sbeta1.csv")
k1=ridge13.3$lambda
jpeg("3.31.jpeg", height=400, width=400, quality = 100)
plot(k1, k1, type = "n", xlab = "岭参数k", ylab = "岭回归系数", ylim = c(-1,1))          
for(i in 1:4){
  lines(k1, sbeta1[,i], type = "o", lty=i, pch=i+17, cex=0.75)
}
legend(1.7,-0.5, inset=0.5, legend=colnames(datas[,3:6]), cex=0.8, pch=1:4, lty=18:21)
dev.off()

#取k1=1.4
beta=c(Intercept=0, sbeta1[rownames(sbeta1)=="1.4",])
xx1=data3.3[,-2]  #
x.mean=apply(xx1, 2, mean)
x.var=apply(xx1, 2, var)
x.bar=sqrt((nrow(data3.3)-1)*x.var)
for(i in 2:length(beta)){
  beta[i]=beta[i]*x.bar[1]/x.bar[i]
}
beta[1]=x.mean[1]-x.mean[-1]%*%beta[-1]
beta


#取k=2
beta=c(Intercept=0, sbeta[rownames(sbeta)=="2.0",])
xx1=data3.3  #
x.mean=apply(xx1, 2, mean)
x.var=apply(xx1, 2, var)
x.bar=sqrt((nrow(data3.3)-1)*x.var)
for(i in 2:length(beta)){
  beta[i]=beta[i]*x.bar[1]/x.bar[i]
}
beta[1]=x.mean[1]-x.mean[-1]%*%beta[-1]
beta
