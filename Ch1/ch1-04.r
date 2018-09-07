

#######################################################
#
#�������f�[�^���͂̕��@�F�t�^�i���q���X�j
#�q�v���O�����Fch1-04.r
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

