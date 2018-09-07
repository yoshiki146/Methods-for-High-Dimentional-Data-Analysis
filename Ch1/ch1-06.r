
#######################################################
#
#高次元データ分析の方法：付録（朝倉書店）
#Ｒプログラム：ch1-06.r
#
#######################################################

#データ発生
n <- 100
X <- cbind(1,matrix(runif(4*n,0,2),n,4))
b0 <- c(1,-2,4,6,3)

D <- X%*%b0+rnorm(n,0,1)
S <- trunc(X%*%b0+runif(n,0.5,3))
for(i in 1:n){
if(D[i]>S[i]){D[i] <-S[i]}
}

i1 <- subset(1:n,D<S)
i2 <- subset(1:n,S<=D)

#パラメータ推定
fit1 <- lm(D~0+X)
beta <- (fit1$coefficients)
p <- length(beta)
theta <- c(beta,1) #パラメータの初期値

Like <- function(theta){
beta <- theta[1:p]
sigma <- theta[p+1]
L1 <- -sum( -log(sigma^2) - ((D-X%*%beta)[i1])^2/sigma^2 )
j <- pnorm((X%*%beta-S)[i2]/sigma); 
if(sum(j==0)!=0){i <- subset(1:length(j),j==0); j[i] <- 10^-100}
L2 <- sum( log( j ) )
sum(L1+L2)
}

fit <- optim(theta,Like,hessian=T)
theta <- fit$par

#推定されたパラメータの統計的検定
beta <- theta[1:p]
sigma <- theta[p+1]

#Rの計算
R <- -fit$hessian/n

#Gammaの計算
library(numDeriv)
Grad <- matrix(0,n,p+1)
for(i in 1:n){
Likegrad <- function(theta){
like <- rep(0,len=n)
beta <- theta[1:p]
sigma <- theta[p+1]
L1 <- -( -log(sigma^2) - ((D-X%*%beta)[i1])^2/sigma^2 )
j <- pnorm((X%*%beta-S)[i2]/sigma); 
if(sum(j==0)!=0){k <- subset(1:length(j),j==0); j[k] <- 10^-100}
L2 <- log(j)
like[i1] <- L1; like[i2] <- L2
like[i]
}
Grad[i,] <- grad(Likegrad,theta)
}
Gamma <- t(Grad)%*%Grad/n

#Hの計算
H <- solve(R)%*%Gamma%*%solve(R)

#t値の計算
Tvalue <- sqrt(n)*theta/sqrt( diag(H) )

#p値の計算
Pvalue <- 1-2*abs(pnorm(Tvalue)-0.5)

#結果の表示
Result <-cbind(theta,trunc(Tvalue*10^3)/10^3,trunc(Pvalue*10^3)/10^3)
print(Result)
