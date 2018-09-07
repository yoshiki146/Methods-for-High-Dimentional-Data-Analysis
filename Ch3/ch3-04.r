
#######################################################
#
#???????f?[?^???͂̕??@?F?t?^?i???q???X?j
#?q?v???O?????Fch3-04.r
#
#######################################################

### Number of Factors ###
# Based on Bai and Ng (2002)

# generate data (only X, not y)
N <- 200
P <- 1000
r <- 4
L <- matrix(rnorm(P*r,0,1),nrow=P,ncol=r) # factor loadings 
F <- matrix(rnorm(N*r,0,1),nrow=N,ncol=r) # factor
E <- matrix(rnorm(N*P,0,3),nrow=N,ncol=P) # error
X <- F%*%t(L)+E


# Max (possible) factors
Kmax <- 20
# Estimation
Cp <- rep(0,len=Kmax) # placeholder
SS <- rep(0,len=Kmax) 
PEN <- rep(0,len=Kmax)

for(k in Kmax:1){
  VEC <- eigen(X%*%t(X)/(P*N))$vectors 
  Fhat <- sqrt(N)*(VEC)[,1:k] 
  Lhat <- t(Fhat)%*%X/N #?t?@?N?^?[???]?s???̐???
  Sk <- sum(diag(t(X-Fhat%*%Lhat)%*%(X-Fhat%*%Lhat)))/(N*P) # fitness to data, similar idea to squared error
  if(k==Kmax){ # this applies only for the first loop
    Smax <- Sk
  }
  Cp[k] <- Sk+k*Smax*((N+P)/(N*P))*log((N*P)/(N+P)) # penalty, sum of the two terms below
  SS[k] <- Sk # 1st term. Sq err. monotonically decreases as NrFac increases (see p.94)
  PEN[k] <- k*Smax*((N+P)/(N*P))*log((N*P)/(N+P)) # 2nd term, increases as factor
}

# Viz Number of Factors - PC score
plot(1:Kmax,Cp,xlab="Numb. of factors",ylab="",ylim=c(0,max(Cp)+5),pch=1, type='b') 
lines(1:Kmax,SS,lwd=1,col=4,pch=1,type="b") 
lines(1:Kmax,PEN,lwd=3,col=2,pch=1,type="b") 

# par(cex.lab=1.2)
# par(cex.axis=1.2)
# plot(1:Kmax,Cp,xlab="Numb. of factors",ylab="",ylim=c(0,max(Cp)+5),pch=1) 
# lines(1:Kmax,SS,lwd=1,col=4,pch=1,type="p") 
# lines(1:Kmax,PEN,lwd=3,col=2,pch=1,type="p") 
# lines(1:Kmax,Cp,lwd=3) 
# lines(1:Kmax,SS,lwd=1,col=4)
# lines(1:Kmax,PEN,lwd=1,col=2,lty=3)

dev.copy2eps(file="PCA-Cp.score-Plot.ps")

