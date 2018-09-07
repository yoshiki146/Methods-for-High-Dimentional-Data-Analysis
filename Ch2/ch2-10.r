
#######################################################
#
#�������f�[�^���͂̕��@�F�t�^�i���q���X�j
#�q�v���O�����Fch2-10.r
#
#######################################################

library(flare)
p <- 80 #�����ϐ��̌�
n <- 100 #�f�[�^��
x <- matrix(rnorm(n*p),nrow=n,ncol=p) #�v��s��̍쐬
b <- c(1,-1,1,0.5,-1,rep(0,len=p-5)) #�^�̉�A�W��
z <- x%*%b #�^�̊m���\��
y <- z+rnorm(n,0,0.5) #�ϑ��f�[�^�̐���
lambda <- c(100,10,1,0.1,0.01,0.001) #�g�p���鐳�����p�����[�^�̒l

#�ȉ��Ō������ؖ@�����s
cv.score <- rep(0,len=length(lambda))
for(i in 1:length(lambda)){
lam <- lambda[i]
for(k in 1:10){
l <- ((k-1)*10+1):(k*10)
x.cv <- x[-l,]
y.cv <- as.vector(y[-l])
fit <- flare.slim(X=x.cv,Y=y.cv,method="dantzig",lambda=lam) #Dantzig selector�����
b.est <- fit$beta #��A�W���̐���l
pred <- x[l,]%*%b.est #�ړI�ϐ��̗\��
er <- sum( (y[l]-pred)^2 ) #�\�����덷
cv.score[i] <- cv.score[i]+er 
}
}

#�œK�Ȑ������p�����[�^�̑I��
opt.lambda <- lambda[subset(1:length(lambda),cv.score==min(cv.score))] 

#�ŏI�I��Dantzig selector�����
fit <- flare.slim(X=x.cv,Y=y.cv,method="dantzig",lambda=opt.lambda)
print(fit$beta)�@#�ŏI�I��Dantzig selector����ʂ̕\��

