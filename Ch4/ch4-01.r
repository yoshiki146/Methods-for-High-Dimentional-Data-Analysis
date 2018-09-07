
#######################################################
#
#�������f�[�^���͂̕��@�F�t�^�i���q���X�j
#�q�v���O�����Fch4-01.r
#
#######################################################

#�f�[�^����
n <- 100
p <- 10
X <- matrix(runif(n*p,-2,2),nrow=n,ncol=p)
b <- c(2,6,-3,1,4,0,0,0,0,0)
y <- X%*%b+rnorm(n,0,1)

#�e���f���̐���CAIC�̌v�Z
M <- 10
AIC <- rep(0,len=M) #�e���f����AIC
MU <- matrix(0,nrow=n,ncol=M) #�e���f���̏����t�����Ғl
for(k in 1:M){
Xk <- X[,1:k]
Bk <- solve(t(Xk)%*%Xk)%*%t(Xk)%*%y
Sk <- sqrt((sum((Xk%*%Bk-y)^2))/n)
AICk <- n*log(2*pi*Sk^2)+n+2*(length(Bk)+1)
AIC[k] <- AICk
MU[,k] <- Xk%*%Bk
}

#�d�݃x�N�g���̌v�Z
W <- exp(-2*AIC)/sum(exp(-2*AIC))
print(W)

#AIC�Ɋ�Â����f�����������
M <- MU%*%W
