
#######################################################
#
#高次元データ分析の方法：付録（朝倉書店）
#Ｒプログラム：ch5-01.r
#
#######################################################

#データ発生
n <- 100
p <- 2000
s <- 50
X <- matrix(runif(n*p,-1,1),nrow=n,ncol=p)
b <- rep(0,len=p)
b[1:s] <- runif(s,-5,5)
y <- X%*%b+rnorm(n,0,1)

#相関の計算
COR <- rep(0,len=p)
for(i in 1:p){COR[i] <- cor(X[,i],y)}

par(cex.lab=1.2)
par(cex.axis=1.2)
plot(COR,xlab="Index of predictors",ylab="Correlation")
dev.copy2eps(file="MA-Corr-Plot.ps")

#重みの推定

Index <- cbind(1:p,COR)
Index <- Index[order(Index[,2],decreasing=T),1:2]

MaxM <- 20
Q <- c(3*(1:15))
minCV.score <- 10^10

CV.matrix <- matrix(0,MaxM,length(Q))

for(Qind in 1:length(Q)){

q <- Q[Qind]

for(M in 2:MaxM){

MU <- matrix(0,n,M)

for(k in 1:M){
USE <- Index[(q*(k-1)+1):(q*k)]
Zk <- X[,USE]
Hk  <- Zk%*%solve(t(Zk)%*%Zk)%*%t(Zk)
Dk <- diag(1/as.vector(diag(diag(1,n)-Hk)))
Hk  <- Dk%*%(Hk-diag(1,n))+diag(1,n)
MU[,k] <- Hk%*%y
}

CV <- function(w){
sum( (MU%*%w-y)^2 )
}

w <- rep(0.1,len=M)
fit <- optim(w,fn=CV,method="L-BFGS-B",lower=rep(0,len=M),upper=rep(1,len=M))
w <- fit$par
CV.score <- fit$value

CV.matrix[M,Qind] <- CV.score

if(CV.score<=minCV.score){
minCV.score <- CV.score
Opt.M <- M
Opt.w <- w
Opt.q <- q
}

}}

par(cex.lab=1.2)
par(cex.axis=1.2)
contour(2:MaxM,Q,CV.matrix[-1,],col=terrain.colors(12),xlab="M",ylab="q")
dev.copy2eps(file="MA-CV-Plot.ps")



#選択されたモデルの推定
M <- Opt.M
q <- Opt.q
w <- Opt.w

MU <- matrix(0,n,M)
for(k in 1:M){
USE <- Index[(q*(k-1)+1):(q*k)]
Zk <- X[,USE]
Hk  <- Zk%*%solve(t(Zk)%*%Zk)%*%t(Zk)
MU[,k] <- Hk%*%y
}

#CVに基づくモデル統合推定量
M <- MU%*%w
