
#######################################################
#
#高次元データ分析の方法：付録（朝倉書店）
#Ｒプログラム：ch4-05.r
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
PL1 <- rep(0,len=M) #各モデルの予測尤度1
PL2 <- rep(0,len=M) #各モデルの予測尤度2
MU <- matrix(0,nrow=n,ncol=M) #各モデルの条件付き期待値
Bias <- rep(0,len=M)

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

#予測尤度の計算
V <- as.numeric(lam/nu)*(diag(1,n)+Xk%*%(t(Xk)%*%Xk+Ak)%*%t(Xk))
LogConst <- log(gamma((nu+n)/2))-log(gamma(nu/2))-(n/2)*log(pi*nu) 
PredLike <- LogConst-1/2*log(det(V))-(nu+n)/2*log(1+as.numeric(t(y-Xk%*%Bk)%*%solve(V)%*%(y-Xk%*%Bk))/nu)

#バイアス
In <- rep(1,len=n)
S <- Sk
f1 <- -1/(2*S)+(y-Xk%*%Bk)^2/(2*S^2)+as.numeric(-p/(2*S)+t(Bk)%*%Ak%*%Bk/(2*S^2)-(nu0/2+1)/S+lam0/(2*S^2))/(2*n)
f2 <- 1/(2*S^2)-(y-Xk%*%Bk)^2/(S^3)+as.numeric(p/(2*S^2)-t(Bk)%*%Ak%*%Bk/(2*S^3)+(nu0/2+1)/S^2-lam0/(2*S^3))/(2*n)
L <- diag(as.vector(y-Xk%*%Bk))
I1 <- rbind(t(Xk)%*%L/S-Ak%*%Bk%*%t(In)/(S*2*n),t(f1))
I2 <- cbind(L%*%Xk/S-t(Ak%*%Bk%*%t(In))/(S*2*n),f1)
I <- I1%*%I2/n
J.BB  <- t(Xk)%*%Xk/S+Ak/(2*S)
J.SB  <- t(In)%*%L%*%Xk/(S^2)-t(Bk)%*%Ak/(2*S^2)
J.BS  <- t(J.SB)
J.SS  <- -t(f2)%*%In
J <- rbind(cbind(J.BB,J.BS),cbind(J.SB,J.SS))/n
Bias <- sum(diag(I%*%(solve(J))))/2

PL1[k] <- PredLike-Bias
PL2[k] <- PredLike-(ncol(Xk)+1)/2
MU[,k] <- Xk%*%Bk
}

#重みベクトルの計算
W1 <- exp(PL1)/sum(exp(PL1))
print(W1)
W2 <- exp(PL2)/sum(exp(PL2))
print(W2)

#予測尤度に基づくモデル統合推定量
M1 <- MU%*%W1
M2 <- MU%*%W2
