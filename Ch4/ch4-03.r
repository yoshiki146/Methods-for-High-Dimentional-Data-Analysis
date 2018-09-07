

#######################################################
#
#高次元データ分析の方法：付録（朝倉書店）
#Ｒプログラム：ch4-03.r
#
#######################################################

#データ発生
n <- 100
x <- runif(n,-2,2)
b <- c(3,2,-4,3,2,0,0,0)
X <- cbind(1,x,x^2,x^3,x^4,x^5,x^6,x^7)
y <- X%*%b+rnorm(n,0,1)

#各モデルの推定，AICの計算
M <- 7
ML <- rep(0,len=M) #各モデルの周辺尤度
MU <- matrix(0,nrow=n,ncol=M) #各モデルの条件付き期待値

for(k in 1:M){
#事前分布のパラメータ
p <- k+1
Ak <- 10^-3*diag(1,p)
nu0 <- 10^-10
lam0 <- 10^-10
#パラメータ推定
Xk <- X[,1:(k+1)]
Bkmle <- solve(t(Xk)%*%Xk)%*%t(Xk)%*%y
Bk <- solve(t(Xk)%*%Xk+Ak)%*%t(Xk)%*%y
nu <- nu0+n
lam <- lam0+t(y-Xk%*%Bkmle)%*%(y-Xk%*%Bkmle)+t(Bkmle)%*%solve(solve(t(Xk)%*%Xk)+solve(Ak))%*%Bkmle
Sk  <- as.numeric((lam/2)/(nu/2+1))

#周辺尤度の計算
PriorB <- -p/2*log(2*pi*Sk)+1/2*log(det(Ak))-t(Bk)%*%Ak%*%Bk/(2*Sk)
PriorS <- nu0/2*log(lam0/2)-log(gamma(nu0/2))-(nu0/2+1)*log(Sk)-lam0/(2*Sk)
Dk <- solve(t(Xk)%*%Xk+Ak)
PostB <- -p/2*log(2*pi*Sk)-1/2*log(det(Dk))
PostS <- nu/2*log(lam/2)-log(gamma(nu/2))-(nu/2+1)*log(Sk)-lam/(2*Sk)
Like  <- -(n/2)*log(2*pi*Sk)-as.numeric(t(y-Xk%*%Bk)%*%(y-Xk%*%Bk))/(2*Sk)
ML[k] <- exp(Like+PriorB+PriorS)/exp(PostB+PostS)
MU[,k] <- Xk%*%Bk
}

#重みベクトルの計算
PriorM <- 2:8/sum(2:8)
W <- PriorM*ML/sum(PriorM*ML)
print(W)

#ベイズアプローチに基づくモデル統合推定量
M <- MU%*%W