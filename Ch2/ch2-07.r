
#######################################################
#
#???????f?[?^???͂̕??@?F?t?^?i???q???X?j
#?q?v???O?????Fch2-07.r
#
#######################################################

#### group lasso ####
# setup
rm(list=ls())
library(grplasso)
p <- 30 
n <- 100 
x <- matrix(rnorm(n*p),nrow=n,ncol=p) 
b <- c(1,-1,1,0.5,-1,rep(0,len=p-5))
z <- x%*%b 
y <- z+rnorm(n,0,0.5) 
index <- c(rep(1,len=2), rep(2,len=3), rep(3,len=p-5)) # assign groups
lambda <- c(100,10,1,0.1,0.01,0.001) # lambda options

# cross validation (k=10)
cv.score <- rep(0,len=length(lambda)) # placeholder

for(i in 1:length(lambda)){
  lam <- lambda[i]
  for(k in 1:10){
    l <- ((k-1)*(n/10)+1):(k*(n/10)) # indeces for test set 
    x.cv <- x[-l,] # remove test set, i.e. training set
    y.cv <- as.vector(y[-l])
    fit <- grplasso(x=x.cv, y=y.cv, index=index,
                    lambda=lam,model=LinReg(),center=F) # Group lasso
    b.est <- fit$coef
    pred <- x[l,]%*%b.est # prediction for test set 
    er <- sum( (y[l]-pred)^2 ) # sq-pred err for test set
    cv.score[i] <- cv.score[i]+er # Note: cv.score is sum of sqErr of each cv-fold
  }
}

# Viz lambda-CVscore relation
# par(cex.lab=1.2)
# par(cex.axis=1.2)
plot(log(lambda),cv.score,xlab="log(Regularization parameter)",ylab="CV.score",type="l",lwd=2) 
dev.copy2eps(file="GRL-CVplot.ps")

# find best-fit lambda
opt.lambda <- lambda[which.min(cv.score)]
# # opt.lambda <- lambda[subset(1:length(lambda),cv.score==min(cv.score))]  # altenatively

# Use optimal lambda
fit <- grplasso(x=x.cv,y=y.cv,index=index,lambda=opt.lambda,model=LinReg(),center=F) 
# print(fit$coefficients) # Notice that the coef's for last group (6-30) are all zero
