
#######################################################
#
#�������f�[�^���͂̕��@�F�t�^�i���q���X�j
#�q�v���O�����Fch1-07.r
#
#��ƃf�B���N�g�����A�t�^�f�[�^���ۑ�����Ă���t�H���_�ւƕύX���Ă��������B
#�Ⴆ�΁A�t�^�f�[�^���t�H���_�uC:\Data�v�ɕۑ�����Ă���ꍇ�A
#�q�R�}���h�usetwd("C:/Data")�v���ŏ��ɓ��͂��Ă��������B
#######################################################

#��ƃf�B���N�g���̕ύX
#setwd("C:/Data")

#�I���W�i�����i
OP <- as.vector(scan("GP-OP.txt"))
#�ő募���N�[�|����
S <- as.vector(scan("GP-S.txt"))
#�v��s��
E <- X <- matrix(scan("GP-X.txt"),ncol=21,byrow=T)
#����p�����[�^
beta <- as.vector(scan("GP-Beta-est.txt"))
#�N�[�|���̃C���f�b�N�X
I1 <- 1:8 #���łɋ������Ă���N�[�|��
I2 <- 9:11 #���񋟋�����N�[�|��
#���łɋ������Ă���N�[�|���̊�����
DC1 <- X[I1,4]

#�������̏����l
DISC <- rep(0.5,len=length(I2))

#�������͈̔͐ݒ�
MIN <- 0.30
MAX <- 0.75

#���v�֐�
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

#���v���ő�Ƃ���悤�Ɋ��������œK��
profit <- optim(DISC,fn=Profit,method="L-BFGS-B",lower=rep(MIN,len=length(DISC)),upper=rep(MAX,len=length(DISC)))

#�œK�����ꂽ������
print(profit$par)

#�œK�����ꂽ�������̂��Ƃł̊��җ��v
print(abs(profit$value))

