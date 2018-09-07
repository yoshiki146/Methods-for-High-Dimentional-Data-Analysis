
#######################################################
#
#???????f?[?^???͂̕??@?F?t?^?i???q???X?j
#?q?v???O?????Fch2-01.r
#
#######################################################
rm(list=ls())
library(lars)
library(bayesm)
data(tuna)
x <- as.matrix(cbind(tuna[,16:22],tuna[,9:15],log(tuna[,30]))) # explanatory vars
y <- log(tuna[,2]) # Sales, Star Kist 6
fit <- lars(x,y,type="lasso") # Lasso estim, tries different lambdas 
lambda <- fit$lambda # regularisation param
beta <- fit$beta # estimated coefs (matrix)
print(fit$beta) 
print(lambda)  

# viz
p <- ncol(x)
range.lam <- c(min(lambda),max(lambda))
range.beta <- c(min(beta),max(beta))
par(cex.lab=1.2)
par(cex.axis=1.2)
plot(range.lam,range.beta,xlab=expression(lambda),ylab="Estimated Coefficients",type="n")
for(i in 1:p){lines(lambda,beta[-1,i],pch=1,cex=0.5,type="p")}
for(i in 1:p){lines(lambda,beta[-1,i],lwd=2,lty=i,col=i)}

#?}?̕ۑ?
dev.copy2eps(file="Lasso-tuna-plot.ps") 
