

#######################################################
#
#高次元データ分析の方法：付録（朝倉書店）
#Ｒプログラム：ch1-04.r
#
#######################################################



library(PERregress)
data(cheese)
Q <- log(cheese$VOLUME)
P <- log(cheese$PRICE)
Prom <- cheese$DISP
Stores <- unique(cheese[,1])
L <- matrix(0,nrow=nrow(cheese),ncol=88)
for(i in 1:nrow(cheese)){
ind <- subset(1:88,(cheese[i,1]==Stores)==1)
L[i,ind] <- 1
}
fit <- lm(Q~0+P+Prom+L)
print(summary(fit))

