
#######################################################
#
#???????f?[?^???͂̕??@?F?t?^?i???q???X?j
#?q?v???O?????Fch2-04.r
#
#######################################################

# lambda selection based on k-fold-cv (k=5)
rm(list = ls())
# true DGP 
p <- 30 # nr of explanatory variables 
n <- 100 # nr of obs
x <- matrix(rnorm(n*p),nrow=n,ncol=p) # randomly generated design matrix (n x p) 
b <- c(1,-2,1.5,0.5,rep(0,len=p-4)) # true coef's. only first 4 coef's are non-zero
z <- x%*%b # deterministic part of y (outcome). i.e. expected value or mean structure 
y <- z+rnorm(n,0,2) # add randamness to z

# k-fold-cv, k=5
library(glmnet)
lambda <- 10^(seq(-2,0,len=100)) # lambda choices
K <- 5
cv.fit <- cv.glmnet(x=x,y=y,lambda=lambda,family="gaussian",alpha=1,nfolds=K) 
## alpha: The elasticnet mixing parameter, bw 0/1. Lasso if 1 and ridge if 0
cv.score <- cv.fit$cvm # mean of cross validation error for each lambda (vector of the same length as lambda_choise)
opt.lam <- lambda[subset(1:length(lambda),cv.score==min(cv.score))] # find optim lambda. alternatively below
opt.lam2 <- lambda[which.min(cv.score)] # Same as the one above. I prefer this
fit <- glmnet(x=x,y=y,lambda=opt.lam,family="gaussian",alpha=1)
beta <- fit$beta #???肳?ꂽ???A?W??
print(beta) 

# Vidualise lambda - CV.score relation
par(mfrow=c(1,1))  # Just undo the division of panel in ch2-03 
# par(cex.lab=1.2)
# par(cex.axis=1.2)
plot(log(lambda),cv.score,xlab="log(Regularization parameter)",ylab="CV.score",type="l",lwd=2) 
dev.copy2eps(file="Lasso-CVplot.ps")

