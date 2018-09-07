

#######################################################
#
#高次元データ分析の方法：付録（朝倉書店）
#Ｒプログラム：ch1-03.r
#
#ch1-01.rを事前に実行してデータの読み込みをしてください。
#######################################################


m <- ncol(R) #金融資産の種類
kappa <- 1.05 #達成する超過収益率
n <- 60 #推定に利用する過去データのサイズ
Portfolio <- matrix(0,nrow=nrow(R),ncol=m) #ポートフォリオ
Risk <- rep(0,len=nrow(R)) #ポートフォリオの分散（リスク）

#ローリングによるポートフォリオの構築
for(i in (n+1):(nrow(R))){

E <- as.matrix(cbind(1,X[(i-n):(i-1),1]))
Z <- as.matrix(R[(i-n):(i-1),])
B <- solve(t(E)%*%E)%*%t(E)%*%Z
xt <- as.matrix(cbind(1,X[i,1]),ncol=1)

r <- as.vector(1+xt%*%B/100) #期待超過収益率
S <- t(Z-E%*%B)%*%(Z-E%*%B)/n #分散共分散行列
meanr <- mean(r)

d <- rep(0,m)
b <- c(1, meanr, rep(0,len=m))
D <- S
A <- cbind(rep(1,len=m),r,diag(1,m))
fit <- solve.QP(S,d,A,b,meq=2) #二次計画法によるポートフォリオ組成
w <- fit$solution #投資金額比率

Portfolio[i,] <- t(w)
Risk[i] <- t(w)%*%S%*%w

}

#ポートフォリオの図示

par(cex.lab=1.2)
par(cex.axis=1.2)
a <- (n+1):nrow(R)
b <- nrow(R):(n+1)
ss <- terrain.colors(m)

plot(c(n+1,nrow(R)),c(0,1),type="n",xlab="Months since Jan. 1927",ylab="")

xx <- c(a,b)
yy <- c(rep(0,len=nrow(R)-n),Portfolio[b,1])
polygon(xx, yy, col=ss[1], border="black",lwd=10^-4)
yy <- c(Portfolio[a,1],Portfolio[b,1]+Portfolio[b,2])
polygon(xx, yy, col=ss[2], border="black",lwd=10^-4)
for(i in 3:m){
yy <- c(Portfolio[a,1:(i-1)]%*%rep(1,len=i-1),Portfolio[b,1:i]%*%rep(1,len=i))
polygon(xx, yy, col=ss[i], border="black",lwd=10^-4)
}

dev.copy2eps(file="PortfolioCAPM.ps")
