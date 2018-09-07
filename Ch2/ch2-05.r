
#######################################################
#
#???????f?[?^???͂̕??@?F?t?^?i???q???X?j
#?q?v???O?????Fch2-05.r
#
#######################################################


### Adaptive lasso
library(lars)
p <- 30 # nr of explanatory variables 
n <- 100 # nr of obs
x <- matrix(rnorm(n*p),nrow=n,ncol=p) # randomly generated design matrix (n x p) 
b <- c(1,-2,1.5,0.5,rep(0,len=p-4)) # true coef's. only first 4 coef's are non-zero
z <- x%*%b # deterministic part of y (outcome). i.e. expected value or mean structure 
y <- z+rnorm(n,0,0.2) # add randamness to z

# Step 1 
ls.fit <- lm(y~x) # recall that adaptive lasso requires n > p
coef.ols <- ls.fit$coef[-1] # save coef except intercept
w <- abs(coef.ols) # create weight as abs values of coef's of OLS
xs <- scale(x,center=FALSE,scale=1/w) # modify design matrix

# Step 2 
fit <- lars(xs,y,type="lasso") 
lambda <- fit$lambda 

# Step 3 
coef <- (fit$beta)[5,]*w # estimated beta under lambda = lambda[5]
print(coef)
coef <- (fit$beta)[10,]*w  
print(coef)