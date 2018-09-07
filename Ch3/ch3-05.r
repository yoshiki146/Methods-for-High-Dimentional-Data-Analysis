
#######################################################
#
#高次元データ分析の方法：付録（朝倉書店）
#Ｒプログラム：ch3-05.r
#
#######################################################

#データ発生
N <- 200
P <- 400
r <- 4
L <- matrix(rnorm(P*r,0,1),nrow=P,ncol=r)
F <- matrix(rnorm(N*r,0,1),nrow=N,ncol=r)
E <- matrix(rnorm(N*P,0,3),nrow=N,ncol=P)
X <- F%*%t(L)+E
Beta <- c(-2,1,5,1)
z <- F[,1]*Beta[1]+F[,2]*Beta[2]+F[,3]*Beta[3]+F[,4]*Beta[4]
VARe <- 1
y <- z+rnorm(N,0,sqrt(VARe))

#推定
tau <- 0.01
IC <- 10^10

Kmax <- r+4

for(k in 1:Kmax){
VEC <- eigen(X%*%t(X)/(P*N))$vectors
F <- sqrt(N)*(VEC)[,1:k]
L <- t(t(F)%*%X/N)
if(k==1){F <- matrix(F,ncol=1)}
if(k==1){V <- diag( as.vector(eigen(X%*%t(X)/(P*N))$values)[1],1)}
if(k!=1){V <- diag( as.vector(eigen(X%*%t(X)/(P*N))$values)[1:k] )}
E <- cbind(1,F)
H <- solve(V)%*%(t(F)%*%F/N)%*%(t(L)%*%L/P)
obj <- function(B){
er <- y-E%*%B
rho <- (abs(er)+(2*tau-1)*er)/2
sum(rho)
}
u <- runif(ncol(F)+1,-1,1)
a <- optim(u,obj,method="BFGS"); 
B <- a$par
O <- as.vector(E%*%B)
Obj <- a$value
S <- sqrt(mean( (y-E%*%solve(t(E)%*%E)%*%t(E)%*%y)^2 ))
LL <- diag( dnorm(O,E%*%solve(t(E)%*%E)%*%t(E)%*%y,sd=S) )
I <- tau*(1-tau)*t(F)%*%F/(N)
J <- t(F)%*%LL%*%F/(N)
Bias1 <- sum( diag( solve(J)%*%I ) )
K <- B[-1]%*%t(B[-1])
Er <- (X-F%*%t(L))^2
Q <- matrix(0,k,k)
for(i in 1:length(y)){
Q <- Q+LL[i,i]*L[i,]%*%t(L[i,])*sum(Er[,i])/(N*P)
}
Sz <- solve(V)%*%Q%*%solve(V)
Bias2 <- sum( diag( K%*%Sz ) )/P
PIC <- 2*Obj+2*(Bias1+Bias2)
if(PIC<=IC[1]){IC[1] <- PIC; OC[1] <- k}
print(trunc(10^3*c(k,PIC,Obj,Bias1,Bias2))/10^3)
}#k
