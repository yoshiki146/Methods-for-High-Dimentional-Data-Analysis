
#######################################################
#
#???????f?[?^???͂̕??@?F?t?^?i???q???X?j
#?q?v???O?????Fch2-08.r
#
#######################################################
#### SCAD ####

# setup
rm(list=ls())
library(ncvreg)
p <- 50 
n <- 100
x <- matrix(rnorm(n*p),nrow=n,ncol=p) 
b <- c(1,-1,1,0.5,-1,rep(0,len=p-5)) 
z <- x%*%b 
y <- z+rnorm(n,0,0.5) 

lambda <- c(100,10,1,0.1,0.01,0.001) # lambda options

# cross validation with nfold=10
cv.score <- rep(0, len=length(lambda)) # placeholder 
for(i in 1:length(lambda)){
  lam <- lambda[i]
  for(k in 1:10){
    l <- ((k-1)*(n/10)+1):(k*(n/10)) # select test set
    x.cv <- x[-l,] # training set
    y.cv <- as.vector(y[-l])
    fit <- ncvreg(X=x.cv,y=y.cv,family="gaussian", penalty="SCAD",lambda=lam) # SCAD
    b.est <- fit$beta
    pred <- cbind(1,x[l,])%*%b.est # test set pred 
    er <- sum( (y[l]-pred)^2 ) # sqErr test set
    cv.score[i] <- cv.score[i]+er # sum up all 10 folds
  }
}

# Viz lambda-CVscore
par(cex.lab=1.2)
par(cex.axis=1.2)
plot(log(lambda),cv.score,xlab="log(Regularization parameter)",ylab="CV.score",type="l",lwd=2) 

# find optimal lambda
opt.lambda <- lambda[which.min(cv.score)] 

# fit SCAD w/ optim lambda
fit <- ncvreg(X=x,y=y,family="gaussian", penalty="SCAD",lambda=opt.lambda)
print(fit$beta)
