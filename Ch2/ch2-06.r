
#######################################################
#
#???????f?[?^???͂̕??@?F?t?^?i???q???X?j
#?q?v???O?????Fch2-06.r
#
#######################################################

##### Elastic net 
library(glmnet)

## data generating process 
p <- 30 # nr of variables
n <- 100 # nr of observations 
x <- matrix(rnorm(n*p),nrow=n,ncol=p) # design matrix of n x p 
b <- c(1,-2,2,rep(0,len=p-3)) # true parameters (first 3 are non-zero)
pr <- exp(x%*%b)/(1+exp(x%*%b)) # true probability 
y <- rbinom(n, size=1,p=pr) # realised value (0/1) based on probability computed above (pr)

cv.fit <- cv.glmnet(x,y,alpha=0.99,family="binomial") # CV, default nfold=10
lambda <- cv.fit$lambda 
cv.score <- cv.fit$cvm # mean cv-score, same length as lambda
opt.lambda <- lambda[which.min(cv.score)] # alternatively command below
# opt.lambda <- lambda[subset(1:length(lambda),cv.score==min(cv.score))] # find optimal lambda

fit <- glmnet(x,y,family="binomial",alpha=0.99,lambda=opt.lambda) # Run elastic net w/ optimal lambda
# print(fit$beta) # optimal estimate
