
#######################################################
#
#???????f?[?^???͂̕??@?F?t?^?i???q???X?j
#?q?v???O?????Fch3-02.r
#
#######################################################

### sure independent screening ###

# setup
rm(list=ls())
library(SIS)
# generate data
p <- 1000
n <- 50 
x <- matrix(rnorm(n*p),nrow=n,ncol=p) 
y <- 0.6*x[,1]+rnorm(n,0,1)

#SIS+SCAD
fit <- SIS(x=x, y=y, family="gaussian")
b.est <- fit$coef.est
print(b.est)
