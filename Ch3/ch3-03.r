
#######################################################
#
#???????f?[?^???͂̕??@?F?t?^?i???q???X?j
#?q?v???O?????Fch3-03.r
#
#######################################################

### Approximate Factor Model (PCA) ### 

# setup 
rm(list=ls())
library(ncvreg)
# generate data
N <- 100 # nr of observatins
P <- 200 # nr of variables in X, affects X and factor
p <- 1000 # nr of variables in w, affects y but not X
r <- 2 # nr of factors

L <- matrix(rnorm(P*r,0,1),nrow=P,ncol=r) # factor loadings, P x r
F <- matrix(rnorm(N*r,0,1),nrow=N,ncol=r) # factor, N x r
E <- matrix(rnorm(N*P,0,1),nrow=N,ncol=P) # error, N x P
X <- F%*%t(L)+E # design matrix (obs), N x P
W <- matrix(runif(N*p,-1,1),nrow=N,ncol=p) # variables affects y (y is on factor and W determines), N x p
Z <- cbind(F,W) # combines (unobservable) factor and (observable) var's 

Alpha <- c(-0.8,1.6) # (true) factor loadings
Beta  <- c(0.9,-0.9,rep(0,len=p-2)) # (true) coef for w (observable var)
Theta <- c(Alpha,Beta)

y <- Z%*%Theta +rnorm(N,0,sd=1) 
# y== (F %*% Alpha) + (W %*% Beta) + rnorm(N, 0,1) # alternatively (some F's probably b/c of rounding)

#SCAD Estimation
lambda <- 0.1 # regularisation param, you can CV to find better lambda (implimented in next chunk)
VEC <- eigen(X%*%t(X)/(P*N))$vectors
Fhat <- sqrt(N)*(VEC)[,1:r] # First step of estimation
Zhat <- cbind(Fhat,W)
fit <- ncvreg(X=Zhat,y=y,family="gaussian", penalty="SCAD",lambda=lambda) #SCAD
b.est <- fit$beta 
# length(b.est): 1003 = cosnt(1) + factor loadings(2) + coef(1000) 

# CV to find optimal lambda
VEC <- eigen(X%*%t(X)/(P*N))$vectors
Fhat <- sqrt(N)*(VEC)[,1:r] 
Zhat <- cbind(Fhat,W)

lambda <- c(100,10,1,0.1,0.01,0.001) # lambda opitons
cv.score <- rep(0,len=length(lambda)) # placeholder

for(i in 1:length(lambda)){
  lam <- lambda[i]
    for(k in 1:10){
    l <- ((k-1)*(N/10)+1):(k*(N/10)) # indeces for test set
    x.cv <- Zhat[-l,] # training set
    y.cv <- as.vector(y[-l])
    fit <- ncvreg(X=x.cv,y=y.cv,family="gaussian", penalty="SCAD",lambda=lam) # SCAD
    b.est <- fit$beta # fitted factor loadings and coef
    pred <- cbind(1,Zhat[l,])%*%b.est # prediction for test set 
    er <- sum( (y[l]-pred)^2 ) # prediction error (test set)
    cv.score[i] <- cv.score[i]+er # add up all cv folds 
  }
}

# Vis lambda-cvScore
# par(cex.lab=1.2)
# par(cex.axis=1.2)
plot(log(lambda),cv.score,xlab="log(Regularization parameter)",ylab="CV.score",type="l",lwd=2) 

# find optim lambda
opt.lambda <- lambda[which.min(cv.score)] 

# fit SCAD with optim lambda
fit <- ncvreg(X=Zhat,y=y,family="gaussian", penalty="SCAD",lambda=opt.lambda)
print(fit$beta[1:10])
