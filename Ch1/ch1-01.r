
#######################################################
#
#???????f?[?^???͂̕??@?F?t?^?i???q???X?j
#?q?v???O?????Fch1-01.r
#
#???ƃf?B???N?g?????A?t?^?f?[?^???ۑ??????Ă????t?H???_?ւƕύX???Ă????????B
#?Ⴆ?΁A?t?^?f?[?^???t?H???_?uC:\Data?v?ɕۑ??????Ă????ꍇ?A
#?q?R?}???h?usetwd("C:/Data")?v???ŏ??ɓ??͂??Ă????????B
#######################################################

#???ƃf?B???N?g???̕ύX
#setwd("C:/Data")

# Import Datasets (http://mba.tuck.dartmouth.edu/pages/faculty/ken.french/data_library.html)
IndustryPortfolio <-  read.table("~/Documents/GitHub/Methods-for-High-Dimentional-Data-Analysis/dat/10_Industry_Portfolios.txt",header=T)
FF3 <-  read.table("~/Documents/GitHub/Methods-for-High-Dimentional-Data-Analysis/dat/F-F_Research_Data_Factors.txt",header=T)
MM <-  read.table("~/Documents/GitHub/Methods-for-High-Dimentional-Data-Analysis/dat/F-F_Momentum_Factor.txt",header=T)

X <- cbind(FF3[,2:4],MM[,2]) # Factor Return
R <- IndustryPortfolio[,2:11]-FF3[,5]%*%t(rep(1,len=10)) # excess return of 10 industries

# Accumulated Return
AccumR <- matrix(0,nrow=nrow(R),ncol=ncol(R))
AccumR[1,] <- t(as.vector(1+R[1,]/100))

for(j in 1:ncol(R)){
  for(i in 2:nrow(R)){
    AccumR[i,j] <- AccumR[i-1,j]*(1+R[i,j]/100)
  }
}

# Vidualise accumulated return
par(cex.lab=1.2)
par(cex.axis=1.2)
plot(c(1,nrow(R)),c(min(AccumR),max(AccumR)),ylab="",xlab="Months since Jan. 1927",type="n")
for(j in 1:ncol(R)){
  lines(AccumR[,j],lwd=2,lty=j,col=j)
}
dev.copy2eps(file="Accum-plot.ps")

