
#######################################################
#
#高次元データ分析の方法：付録（朝倉書店）
#Ｒプログラム：ch2-09.r
#
#######################################################

library(ncvreg)
p <- 50 #説明変数の個数
n <- 100 #データ数
x <- matrix(rnorm(n*p),nrow=n,ncol=p) #計画行列の作成
b <- c(1,-1,1,0.5,-1,rep(0,len=p-5)) #真の回帰係数
z <- x%*%b #真の確率構造
y <- z+rnorm(n,0,0.5) #観測データの生成
lambda <- c(100,10,1,0.1,0.01,0.001) #使用する正則化パラメータの値

#以下で交差検証法を実行
cv.score <- rep(0,len=length(lambda))
for(i in 1:length(lambda)){
lam <- lambda[i]
for(k in 1:10){
l <- ((k-1)*10+1):(k*10)
x.cv <- x[-l,]
y.cv <- as.vector(y[-l])
fit <- ncvreg(X=x.cv,y=y.cv,family="gaussian", penalty="MCP",lambda=lam) #MC+推定量
b.est <- fit$beta #回帰係数の推定値
pred <- cbind(1,x[l,])%*%b.est #目的変数の予測
er <- sum( (y[l]-pred)^2 ) #予測二乗誤差
cv.score[i] <- cv.score[i]+er 
}
}

#最適な正則化パラメータの選択
opt.lambda <- lambda[subset(1:length(lambda),cv.score==min(cv.score))] 

#最終的な MC+ 推定量
fit <- ncvreg(X=x,y=y,family="gaussian", penalty="MCP",lambda=opt.lambda)
print(fit$beta)　#最終的なMC+推定量の表示

