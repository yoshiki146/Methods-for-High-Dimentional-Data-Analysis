
#######################################################
#
#�������f�[�^���͂̕��@�F�t�^�i���q���X�j
#�q�v���O�����Fch4-06.r
#
#######################################################

#�f�[�^����
n <- 100
p <- 8
X <- matrix(runif(n*p,-2,2),nrow=n,ncol=p)
b <- c(2,-6,3,1,0,0,0,0)
y <- X%*%b+rnorm(n,0,1)

#�e���f���̐���
M <- 8
MU <- matrix(0,nrow=n,ncol=M) #�e���f���̏����t�����Ғl
E <- matrix(0,n,M)
for(k in 1:M){
Xk <- X[,1:k]
Bk <- solve(t(Xk)%*%Xk)%*%t(Xk)%*%y
MU[,k] <- Xk%*%Bk
E[,k] <- y-Xk%*%Bk
}
S <- mean(E[,p]^2)

#�d�݃x�N�g���̌v�Z

d <- -(2*S*(1:M))
b <- c(1, rep(0,len=M))
D <- 2*t(E)%*%E
A <- cbind(rep(1,len=M),diag(1,M))
fit <- solve.QP(D,d,A,b,meq=1)
w <- fit$solution
print(w)

#Cp�Ɋ�Â����f�����������
M <- MU%*%w
