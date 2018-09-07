
#######################################################
#
#高次元データ分析の方法：付録（朝倉書店）
#Ｒプログラム：ch1-05.r
#
#ch1-04.rを事前に実行してください。
#######################################################

Location <- (fit$coefficients)[-(1:2)]

#価格水準の範囲設定
MINp <-  min(exp(P))
MAXp <- max(exp(P))

#販売プロモーション水準の範囲設定
MINprom <- min(Prom)
MAXprom <- max(Prom)

#利益関数
profit <- function(x){
prom <- x[89]
Quantity <- rep(0,len=88)
for(i in 1:88){
price <- x[i]
Quantity[i] <- exp(-2.388*log(price)+0.914*prom+Location[i])
}
Pi <- sum( Quantity*(x[1:88]-1.00)-sum(Quantity)*prom/100 )
-Pi
}

#利益を最大とするように最適化
Int.para <- c(rep(mean(exp(P)),len=88),mean(Prom))
fit <- optim(Int.para,fn=profit,method="L-BFGS-B",lower=c(rep(MINp,len=88),MINprom),upper=c(rep(MAXp,len=88),MAXprom))

#最適化された設定
print(fit$par)

#作図
p <- seq(MINp,MAXp,len=100)
q <- seq(MINprom,MAXprom,len=100)
Profit <- matrix(0,100,100)
for(Q1 in 1:length(p)){
for(Q2 in 1:length(q)){
price <- p[Q1]
prom <- q[Q2]
Quantity <- exp(-2.388*log(price)+0.914*prom+Location[3])
Pi <- sum( Quantity*(price-1.00)-sum(Quantity)*prom/100 )
Profit[Q1,Q2] <- Pi
}}
par(cex.lab=1.2)
par(cex.axis=1.2)
contour(p,q,Profit,col=terrain.colors(12),xlab="Price",ylab="Promotion",lwd=2)
dev.copy2eps(file="Cheese-Plot.ps")
