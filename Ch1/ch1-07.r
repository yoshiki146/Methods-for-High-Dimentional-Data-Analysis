
#######################################################
#
#高次元データ分析の方法：付録（朝倉書店）
#Ｒプログラム：ch1-07.r
#
#作業ディレクトリを、付録データが保存されているフォルダへと変更してください。
#例えば、付録データがフォルダ「C:\Data」に保存されている場合、
#Ｒコマンド「setwd("C:/Data")」を最初に入力してください。
#######################################################

#作業ディレクトリの変更
#setwd("C:/Data")

#オリジナル価格
OP <- as.vector(scan("GP-OP.txt"))
#最大供給クーポン数
S <- as.vector(scan("GP-S.txt"))
#計画行列
E <- X <- matrix(scan("GP-X.txt"),ncol=21,byrow=T)
#推定パラメータ
beta <- as.vector(scan("GP-Beta-est.txt"))
#クーポンのインデックス
I1 <- 1:8 #すでに供給しているクーポン
I2 <- 9:11 #今回供給するクーポン
#すでに供給しているクーポンの割引率
DC1 <- X[I1,4]

#割引率の初期値
DISC <- rep(0.5,len=length(I2))

#割引率の範囲設定
MIN <- 0.30
MAX <- 0.75

#利益関数
Profit <- function(DISC){
E[I2,3] <- log(OP[I2]*(1-DISC))
E[I2,4] <- DISC
E[,ncol(E)] <- max(max(E[,ncol(E)]),DISC)
Q <- exp(E%*%beta)
Q <- (Q<S)*Q+(S<=Q)*S
Q1 <- Q[I1]
Q2 <- Q[I2]
Markup <- sum(OP[I1]*(1-DC1)*Q1)+sum(OP[I2]*(1-DISC)*Q2)
-Markup
}

#利益を最大とするように割引率を最適化
profit <- optim(DISC,fn=Profit,method="L-BFGS-B",lower=rep(MIN,len=length(DISC)),upper=rep(MAX,len=length(DISC)))

#最適化された割引率
print(profit$par)

#最適化された割引率のもとでの期待利益
print(abs(profit$value))

