
#######################################################
#
#高次元データ分析の方法：付録（朝倉書店）
#Ｒプログラム：ch4-07.r
#
#######################################################

#データ発生
n <- 100
p <- 8
X <- matrix(runif(n*p,-2,2),nrow=n,ncol=p)
b <- c(2,-6,3,1,0,0,0,0)
y <- X%*%b+rnorm(n,0,1)

#各モデルの推定
M <- 8
MU <- matrix(0,nrow=n,ncol=M) #各モデルの条件付き期待値
E <- matrix(0,n,M)
for(k in 1:M){
Xk <- X[,1:k]
Bk <- solve(t(Xk)%*%Xk)%*%t(Xk)%*%y
Hk <- Xk%*%solve(t(Xk)%*%Xk)%*%t(Xk)
Dk <- diag(1/as.vector(diag(diag(1,n)-Hk)))
HH  <- Dk%*%(Hk-diag(1,n))+diag(1,n)
MU[,k] <- Hk%*%y
E[,k] <- y-HH%*%y
}
S <- mean(E[,p]^2)

#重みベクトルの計算

d <- rep(0,len=M)
b <- c(1, rep(0,len=M))
D <- 2*t(E)%*%E
A <- cbind(rep(1,len=M),diag(1,M))
fit <- solve.QP(D,d,A,b,meq=1)
w <- fit$solution
print(w)

#CVに基づくモデル統合推定量
M <- MU%*%w
