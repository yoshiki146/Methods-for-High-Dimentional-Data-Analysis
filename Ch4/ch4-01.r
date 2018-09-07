
#######################################################
#
#高次元データ分析の方法：付録（朝倉書店）
#Ｒプログラム：ch4-01.r
#
#######################################################

#データ発生
n <- 100
p <- 10
X <- matrix(runif(n*p,-2,2),nrow=n,ncol=p)
b <- c(2,6,-3,1,4,0,0,0,0,0)
y <- X%*%b+rnorm(n,0,1)

#各モデルの推定，AICの計算
M <- 10
AIC <- rep(0,len=M) #各モデルのAIC
MU <- matrix(0,nrow=n,ncol=M) #各モデルの条件付き期待値
for(k in 1:M){
Xk <- X[,1:k]
Bk <- solve(t(Xk)%*%Xk)%*%t(Xk)%*%y
Sk <- sqrt((sum((Xk%*%Bk-y)^2))/n)
AICk <- n*log(2*pi*Sk^2)+n+2*(length(Bk)+1)
AIC[k] <- AICk
MU[,k] <- Xk%*%Bk
}

#重みベクトルの計算
W <- exp(-2*AIC)/sum(exp(-2*AIC))
print(W)

#AICに基づくモデル統合推定量
M <- MU%*%W
