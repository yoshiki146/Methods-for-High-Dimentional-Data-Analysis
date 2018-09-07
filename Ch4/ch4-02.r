

#######################################################
#
#�������f�[�^���͂̕��@�F�t�^�i���q���X�j
#�q�v���O�����Fch4-02.r
#
#######################################################

#O-�����O�f�[�^�̓ǂݍ���
library(vcd)
data("SpaceShuttle")
Data <- SpaceShuttle[-4,]
Data <- Data[order(Data[,2]),1:5]
x <- Data[,2]
y <- Data[,5]

m <- rep(6,len=nrow(Data))
z <- cbind(y,m-y)
temp <- seq(15,85,len=100)

#���f��M1�̐���
fit <- glm(z~x,family=binomial(link="logit"))
b1 <- fit$coefficients
O <- cbind(1,temp)%*%b1
p1 <- exp(O)/(1+exp(O)) #���f��M1�Ɋ�Â����肳�ꂽ�̏�m��
AIC1 <- fit$aic #���肳�ꂽ���f��M1��AIC
print(b1) #���肳�ꂽ���f��M1�̉�A�W��
print(summary(fit))

#���f��M2�̐���
fit <- glm(z~x,family=binomial(link="probit"))
b2 <- fit$coefficients
O <- cbind(1,temp)%*%b2
p2 <- pnorm(O) #���f��M2�Ɋ�Â����肳�ꂽ�̏�m��
AIC2 <- fit$aic  #���肳�ꂽ���f��M2��AIC
print(b2) #���肳�ꂽ���f��M2�̉�A�W��
print(summary(fit))

#���f��M3�̐���
fit <- glm(z~x,family=binomial(link="cloglog"))
b3 <- fit$coefficients
O <- cbind(1,temp)%*%b3
p3 <- 1-exp(-exp(O)) #���f��M3�Ɋ�Â����肳�ꂽ�̏�m��
AIC3 <- fit$aic  #���肳�ꂽ���f��M3��AIC
print(b3) #���肳�ꂽ���f��M3�̉�A�W��
print(summary(fit))

#���茋�ʂ̃v���b�g
par(cex.lab=1.2)
par(cex.axis=1.2)
plot(x,y/6,xlab="Temperature",ylab="Probability of failure of the O-rings",cex=1,ylim=c(0,1),xlim=c(min(temp),max(temp)))
lines(temp,p1,lwd=1,col=1)
lines(temp,p2,lwd=3,col=1,lty=2)
lines(temp,p3,lwd=3,col=1)
dev.copy2eps(file="Oring-fit.ps")

#�d�݃x�N�g���̌v�Z
w1 <- exp(-2*AIC1)/(exp(-2*AIC1)+exp(-2*AIC2)+exp(-2*AIC3))
w2 <- exp(-2*AIC2)/(exp(-2*AIC1)+exp(-2*AIC2)+exp(-2*AIC3))
w3 <- exp(-2*AIC3)/(exp(-2*AIC1)+exp(-2*AIC2)+exp(-2*AIC3))
print(c(w1,w2,w3))

#AIC�Ɋ�Â����f�����������
p<- p1*w1+p2*w2+p3*w3

#���茋�ʂ̃v���b�g
par(cex.lab=1.2)
par(cex.axis=1.2)
plot(x,y/6,xlab="Temperature",ylab="Probability of failure of the O-rings",cex=1,ylim=c(0,1),xlim=c(min(temp),max(temp)))
lines(temp,p,lwd=2,col=1)
dev.copy2eps(file="Oring-MAIC.ps")

