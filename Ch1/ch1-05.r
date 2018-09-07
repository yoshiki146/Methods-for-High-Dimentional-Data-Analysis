
#######################################################
#
#�������f�[�^���͂̕��@�F�t�^�i���q���X�j
#�q�v���O�����Fch1-05.r
#
#ch1-04.r�����O�Ɏ��s���Ă��������B
#######################################################

Location <- (fit$coefficients)[-(1:2)]

#���i�����͈̔͐ݒ�
MINp <-  min(exp(P))
MAXp <- max(exp(P))

#�̔��v�����[�V���������͈̔͐ݒ�
MINprom <- min(Prom)
MAXprom <- max(Prom)

#���v�֐�
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

#���v���ő�Ƃ���悤�ɍœK��
Int.para <- c(rep(mean(exp(P)),len=88),mean(Prom))
fit <- optim(Int.para,fn=profit,method="L-BFGS-B",lower=c(rep(MINp,len=88),MINprom),upper=c(rep(MAXp,len=88),MAXprom))

#�œK�����ꂽ�ݒ�
print(fit$par)

#��}
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
