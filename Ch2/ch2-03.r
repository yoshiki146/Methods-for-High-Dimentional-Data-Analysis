
#######################################################
# 
#???????f?[?^???͂̕??@?F?t?^?i???q???X?j
#?q?v???O?????Fch2-03.r
#
#######################################################

### vidualise the result of ch2-2.r to see
### how different values of lambda affects the output

library(lars)
source("ch2-02.r")

lambda <- fit$lambda
coef <- fit$beta

coef1 <- coef[2,] # estim'ed coef's when lambda = lambda[2] 
coef2 <- coef[5,] 
coef3 <- coef[10,] 
coef4 <- coef[20,] 

mu.est1 <- x%*%coef1 # predicted values 
mu.est2 <- x%*%coef2 
mu.est3 <- x%*%coef3
mu.est4 <- x%*%coef4

# vidualise
par(mfrow=c(2,2))  # divide panel
par(mar=c(4,4,4,4)) # spec margin 

plot(mu.est1,z,xlab=expression(hat(mu)),ylab=expression(mu),xlim=c(-6,6),ylim=c(-6,6))
lines(c(-6,6),c(-6,6),lwd=2) # 45 degrees line. perfect match

plot(mu.est2,z,xlab=expression(hat(mu)),ylab=expression(mu),xlim=c(-6,6),ylim=c(-6,6))
lines(c(-6,6),c(-6,6),lwd=2)

plot(mu.est3,z,xlab=expression(hat(mu)),ylab=expression(mu),xlim=c(-6,6),ylim=c(-6,6))
lines(c(-6,6),c(-6,6),lwd=2)

plot(mu.est4,z,xlab=expression(hat(mu)),ylab=expression(mu),xlim=c(-6,6),ylim=c(-6,6))
lines(c(-6,6),c(-6,6),lwd=2)

#?}?̕ۑ?
dev.copy2eps(file="Lasso-plot.ps")

# mean squared error
print(mean( (z-mu.est1)^2)) # error defined as z(true) - mu (pred). 
print(mean( (z-mu.est2)^2)) 
print(mean( (z-mu.est3)^2))
print(mean( (z-mu.est4)^2))
