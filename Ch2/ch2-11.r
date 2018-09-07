
#######################################################
#
#�������f�[�^���͂̕��@�F�t�^�i���q���X�j
#�q�v���O�����Fch2-11.r
#
#######################################################

library(monomvn)
p <- 80 #�����ϐ��̌�
n <- 100 #�f�[�^��
x <- matrix(rnorm(n*p),nrow=n,ncol=p) #�v��s��̍쐬
b <- c(1,-1,rep(0,len=p-2)) #�^�̉�A�W��
z <- x%*%b #�^�̊m���\��
y <- z+rnorm(n,0,0.5) #�ϑ��f�[�^�̐���

#Bayesian lasso ����̎��s
library(blasso)
fit <- blasso(x,y,T=10000)

#MCMC�����
beta <- fit$beta
beta <- beta[-(1:1000),]

#MCMC�T���v���̃g���[�X�v���b�g
par(cex.lab=1.2)
par(cex.axis=1.2)
plot(beta[,1],type="n",xlab="Number of Iteration",ylab="")
lines(beta[,1],lwd=2)
dev.copy2eps(file="BLasso-traceplot-b1.ps")

plot(beta[,2],type="n",xlab="Number of Iteration",ylab="")
lines(beta[,2],lwd=2)
dev.copy2eps(file="BLasso-traceplot-b2.ps")

plot(beta[,3],type="n",xlab="Number of Iteration",ylab="")
lines(beta[,3],lwd=2)
dev.copy2eps(file="BLasso-traceplot-b3.ps")

plot(beta[,4],type="n",xlab="Number of Iteration",ylab="")
lines(beta[,4],lwd=2)
dev.copy2eps(file="BLasso-traceplot-b4.ps")

#MCMC�T���v���̖��x�֐�
par(cex.lab=1.2)
par(cex.axis=1.2)
plot(density(beta[,1]),lwd=3,xlab="",main="")
dev.copy2eps(file="BLasso-density-b1.ps")

plot(density(beta[,2]),lwd=3,xlab="",main="")
dev.copy2eps(file="BLasso-density-b2.ps")

plot(density(beta[,3]),lwd=3,xlab="",main="")
dev.copy2eps(file="BLasso-density-b3.ps")

plot(density(beta[,4]),lwd=3,xlab="",main="")
dev.copy2eps(file="BLasso-density-b4.ps")

#�����ϐ����܂܂�鎖��m��
for(j in 1:p){print(sum(beta[,j]!=0)/nrow(beta))}
