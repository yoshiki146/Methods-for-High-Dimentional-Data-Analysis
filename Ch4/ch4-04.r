

#######################################################
#
#�������f�[�^���͂̕��@�F�t�^�i���q���X�j
#�q�v���O�����Fch4-04.r
#
#######################################################

library(mlogit)
data(Cracker)
X1 <- as.matrix(Cracker[,2:5])
X2 <- as.matrix(Cracker[,6:9])
X3 <- as.matrix(Cracker[,10:13])
y <- as.numeric(Cracker[,14])

#�e���f���̐���CAIC�̌v�Z
n <- length(y)
M <- 7
ML <- rep(0,len=M) #�e���f���̎��Ӗޓx
Pi1 <- matrix(0,nrow=n,ncol=M) #�e���f����sunshine�u�����h�I���m��
Pi2 <- matrix(0,nrow=n,ncol=M) #�e���f����keebler�u�����h�I���m��
Pi3 <- matrix(0,nrow=n,ncol=M) #�e���f����nabisco�u�����h�I���m��
Pi4 <- matrix(0,nrow=n,ncol=M) #�e���f����private�u�����h�I���m��

#���O���z�̃p�����[�^
lambda <- 10^-1
Targets <- matrix(0,nrow=n,ncol=4)
for(i in 1:n){Targets[i,y[i]] <- 1}

for(a in 1:M){

if(a==1){X <- cbind(1,X1)}
if(a==2){X <- cbind(1,X2)}
if(a==3){X <- cbind(1,X3)}
if(a==4){X <- cbind(1,X1,X2)}
if(a==5){X <- cbind(1,X1,X3)}
if(a==6){X <- cbind(1,X2,X3)}
if(a==7){X <- cbind(1,X1,X2,X3)}

g <- 4
d <- ncol(X)
ig <- rep(1,g-1)
iN <- rep(1,n)
IN <- matrix(1,nrow=n,ncol=1)

Beta <- matrix(runif(d*(g-1),-10^-5,10^-5),nrow=d,ncol=(g-1))
DB <- matrix(0,nrow=d*(g-1),ncol=1)
H <- matrix(0,nrow=d*(g-1),ncol=d*(g-1))

for(ITERE in 1:100){
Beta.old <- Beta
O <- X%*%Beta
Pi <- exp(O)/(1+(exp(O)%*%ig)%*%t(ig))
for(gg in 1:(g-1)){
DB[(d*(gg-1)+1):(d*gg),1] <- (t(X)%*%(Targets[,gg]-Pi))[,gg]-n*lambda*Beta[,gg]
}
for(j in 1:(g-1)){for(k in 1:(g-1)){
L1 <- diag( as.vector(Pi[,j]) ); L2 <- diag( as.vector(Pi[,k]) )
H[(d*(j-1)+1):(d*j),(d*(k-1)+1):(d*k)] <- t(X)%*%L1%*%L2%*%X
}}
for(k in 1:(g-1)){
L <- diag( as.vector(Pi[,k]) )
H[(d*(k-1)+1):(d*k),(d*(k-1)+1):(d*k)] <- H[(d*(k-1)+1):(d*k),(d*(k-1)+1):(d*k)]-t(X)%*%L%*%X
}
SVDH <- svd(H-n*lambda*diag(1,d*(g-1)))
solveH <- (SVDH$v)%*%solve(diag(as.vector(SVDH$d)))%*%t(SVDH$u)
for(i in 1:(g-1)){Beta[,i] <- Beta[,i]-(solveH%*%DB)[(d*(i-1)+1):(d*i)]}
if(sum((Beta.old-Beta)^2)<=10^-7){break}
}

#�I���m��
Pi1[,a] <- Pi[,1]
Pi2[,a] <- Pi[,2]
Pi3[,a] <- Pi[,3]
Pi4[,a] <- 1-Pi[,1]-Pi[,2]-Pi[,3]

#���Ӗޓx�̌v�Z
O <- X%*%Beta
Pi <- c(1/(exp(O)%*%ig+1))*exp(O)
S <-  matrix(0,nrow=d*(g-1),ncol=d*(g-1))
for(j in 1:(g-1)){for(k in 1:(g-1)){
L1 <- diag( as.vector(Pi[,j]) ); L2 <- diag( as.vector(Pi[,k]) )
I.small <- t(X)%*%L1%*%L2%*%X
S[(d*(j-1)+1):(d*j),(d*(k-1)+1):(d*k)] <- I.small
if(j==k){
L <- diag( as.vector(Pi[,k]) )
S[(d*(j-1)+1):(d*j),(d*(k-1)+1):(d*k)]-t(X)%*%L%*%X+n*lambda*diag(1,d)
}
}}
S <- S/n

Pi <- cbind(Pi,1-Pi%*%ig)
LogLikelihood <- sum(log(Pi^Targets))
LogPrior <- (g-1)*d*log(2*pi/(n*lambda))/2-n*lambda*sum(Beta^2)/2
LogMarLike <- LogLikelihood+LogPrior+(g-1)*d*log(2*pi)/2+log(det(S))/2
ML[a] <- LogMarLike
}

#�d�݃x�N�g���̌v�Z
W <- exp(ML-min(ML))/sum(exp(ML-min(ML)))
print(W)

#�x�C�Y�A�v���[�`�Ɋ�Â����f�����������
P1 <- Pi1%*%W #sunshine�u�����h�I���m��
P2 <- Pi2%*%W #keebler�u�����h�I���m��
P3 <- Pi3%*%W #nabisco�u�����h�I���m��
P4 <- Pi4%*%W #private�u�����h�I���m��
print(cbind(P1,P2,P3,P4))

#���ʂ̃v���b�g
par(cex.lab=1.2)
par(cex.axis=1.2)
hist(P1,xlim=c(0,1),br=0:100/100,xlab="",main="")
dev.copy2eps(file="Chice-Pr1-est.ps")
hist(P2,xlim=c(0,1),br=0:100/100,xlab="",main="")
dev.copy2eps(file="Chice-Pr2-est.ps")
hist(P3,xlim=c(0,1),br=0:100/100,xlab="",main="")
dev.copy2eps(file="Chice-Pr3-est.ps")
hist(P4,xlim=c(0,1),br=0:100/100,xlab="",main="")
dev.copy2eps(file="Chice-Pr4-est.ps")

#�d�݃x�N�g���̌v�Z
W <- exp(ML-min(ML))/sum(exp(ML-min(ML)))
print(W)
