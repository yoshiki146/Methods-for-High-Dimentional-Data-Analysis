
#######################################################
#
#???????f?[?^???͂̕??@?F?t?^?i???q???X?j
#?q?v???O?????Fch2-02.r
#
#######################################################
rm(list=ls())
library(lars)
# generate data 
p <- 30 # nr of explanatory variables 
n <- 100 # nr of obs
x <- matrix(rnorm(n*p),nrow=n,ncol=p) # randomly generated design matrix (n x p) 
b <- c(1,-2,1.5,0.5,rep(0,len=p-4)) # true coef's. only first 4 coef's are non-zero
z <- x%*%b # deterministic part of y (outcome). i.e. expected value or mean structure 
y <- z+rnorm(n,0,2) # add randamness to z

# estimation
fit <- lars(x,y,type="lasso") # fit lasso
lambda <- fit$lambda # show lambda's for multiple estimations
beta <- fit$beta # show betas corresponding to estim'ed lambdas
# print(fit$beta) 
# print(lambda)  
