

#######################################################
#
#高次元データ分析の方法：付録（朝倉書店）
#Ｒプログラム：ch4-02.r
#
#######################################################

#O-リングデータの読み込み
library(vcd)
data("SpaceShuttle")
Data <- SpaceShuttle[-4,]
Data <- Data[order(Data[,2]),1:5]
x <- Data[,2]
y <- Data[,5]

m <- rep(6,len=nrow(Data))
z <- cbind(y,m-y)
temp <- seq(15,85,len=100)

#モデルM1の推定
fit <- glm(z~x,family=binomial(link="logit"))
b1 <- fit$coefficients
O <- cbind(1,temp)%*%b1
p1 <- exp(O)/(1+exp(O)) #モデルM1に基づき推定された故障確率
AIC1 <- fit$aic #推定されたモデルM1のAIC
print(b1) #推定されたモデルM1の回帰係数
print(summary(fit))

#モデルM2の推定
fit <- glm(z~x,family=binomial(link="probit"))
b2 <- fit$coefficients
O <- cbind(1,temp)%*%b2
p2 <- pnorm(O) #モデルM2に基づき推定された故障確率
AIC2 <- fit$aic  #推定されたモデルM2のAIC
print(b2) #推定されたモデルM2の回帰係数
print(summary(fit))

#モデルM3の推定
fit <- glm(z~x,family=binomial(link="cloglog"))
b3 <- fit$coefficients
O <- cbind(1,temp)%*%b3
p3 <- 1-exp(-exp(O)) #モデルM3に基づき推定された故障確率
AIC3 <- fit$aic  #推定されたモデルM3のAIC
print(b3) #推定されたモデルM3の回帰係数
print(summary(fit))

#推定結果のプロット
par(cex.lab=1.2)
par(cex.axis=1.2)
plot(x,y/6,xlab="Temperature",ylab="Probability of failure of the O-rings",cex=1,ylim=c(0,1),xlim=c(min(temp),max(temp)))
lines(temp,p1,lwd=1,col=1)
lines(temp,p2,lwd=3,col=1,lty=2)
lines(temp,p3,lwd=3,col=1)
dev.copy2eps(file="Oring-fit.ps")

#重みベクトルの計算
w1 <- exp(-2*AIC1)/(exp(-2*AIC1)+exp(-2*AIC2)+exp(-2*AIC3))
w2 <- exp(-2*AIC2)/(exp(-2*AIC1)+exp(-2*AIC2)+exp(-2*AIC3))
w3 <- exp(-2*AIC3)/(exp(-2*AIC1)+exp(-2*AIC2)+exp(-2*AIC3))
print(c(w1,w2,w3))

#AICに基づくモデル統合推定量
p<- p1*w1+p2*w2+p3*w3

#推定結果のプロット
par(cex.lab=1.2)
par(cex.axis=1.2)
plot(x,y/6,xlab="Temperature",ylab="Probability of failure of the O-rings",cex=1,ylim=c(0,1),xlim=c(min(temp),max(temp)))
lines(temp,p,lwd=2,col=1)
dev.copy2eps(file="Oring-MAIC.ps")

