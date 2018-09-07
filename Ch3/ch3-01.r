
#######################################################
#
#???????f?[?^???͂̕??@?F?t?^?i???q???X?j
#?q?v???O?????Fch3-01.r
#
#######################################################

# Problems of ultra high dimention data 

# generate data 
p <- 10000 # nr of variables
n <- 50 # nr of observations 
x <- matrix(rnorm(n*p),nrow=n,ncol=p) # design matrix
y <- 0.6*x[,1]+rnorm(n,0,1) # Notice only fist varible affects y

# check for correlations bw X's
Cor <- rep(0,len=p-1) # placeholder
for(i in 1:(p-1)){
  Cor[i] <- cor(x[,1],x[,i+1])
}
print(range(Cor)) 

# Viz
# par(cex.lab=1.2)
# par(cex.axis=1.2)
hist(Cor,xlim=c(-1,1))
# dev.copy2eps(file="Cor-histplot.ps")
### --> x[,1] is the only variable that matters. 
###     However, x[,i] (i=2,3,...,10k) are corr'd to x[,1]


# check for corr bw y and x's
Cory <- rep(0,len=p) # place holder
for(i in 1:p){Cory[i] <- cor(y,x[,i])}
print(order(abs(Cory)))
### --> There are variables with higher corr with y
###     although x[,1] is the only variable corr'd to y
