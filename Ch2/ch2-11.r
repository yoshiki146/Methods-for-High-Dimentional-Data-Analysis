
#######################################################
#
#高次元データ分析の方法：付録（朝倉書店）
#Ｒプログラム：ch2-11.r
#
#######################################################

library(monomvn)
p <- 80 #説明変数の個数
n <- 100 #データ数
x <- matrix(rnorm(n*p),nrow=n,ncol=p) #計画行列の作成
b <- c(1,-1,rep(0,len=p-2)) #真の回帰係数
z <- x%*%b #真の確率構造
y <- z+rnorm(n,0,0.5) #観測データの生成

#Bayesian lasso 推定の実行
library(blasso)
fit <- blasso(x,y,T=10000)

#MCMC推定量
beta <- fit$beta
beta <- beta[-(1:1000),]

#MCMCサンプルのトレースプロット
par(cex.lab=1.2)
par(cex.axis=1.2)
plot(beta[,1],type="n",xlab="Number of Iteration",ylab="")
lines(beta[,1],lwd=2)
dev.copy2eps(file="BLasso-traceplot-b1.ps")

plot(beta[,2],type="n",xlab="Number of Iteration",ylab="")
lines(beta[,2],lwd=2)
dev.copy2eps(file="BLasso-traceplot-b2.ps")

plot(beta[,3],type="n",xlab="Number of Iteration",ylab="")
lines(beta[,3],lwd=2)
dev.copy2eps(file="BLasso-traceplot-b3.ps")

plot(beta[,4],type="n",xlab="Number of Iteration",ylab="")
lines(beta[,4],lwd=2)
dev.copy2eps(file="BLasso-traceplot-b4.ps")

#MCMCサンプルの密度関数
par(cex.lab=1.2)
par(cex.axis=1.2)
plot(density(beta[,1]),lwd=3,xlab="",main="")
dev.copy2eps(file="BLasso-density-b1.ps")

plot(density(beta[,2]),lwd=3,xlab="",main="")
dev.copy2eps(file="BLasso-density-b2.ps")

plot(density(beta[,3]),lwd=3,xlab="",main="")
dev.copy2eps(file="BLasso-density-b3.ps")

plot(density(beta[,4]),lwd=3,xlab="",main="")
dev.copy2eps(file="BLasso-density-b4.ps")

#説明変数が含まれる事後確率
for(j in 1:p){print(sum(beta[,j]!=0)/nrow(beta))}
