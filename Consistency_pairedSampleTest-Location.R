rm(list=ls())
library(BSDA)
library(stats)

#consistency of Paired Sample Test for Location

n=seq(5,100,length=10)

R=100
power.SIGN = power.wsr = power.t = array(0)

a=10
pvalue_SIGN_wsr_t=function(a){
  s1=rnorm(a,5,1)
  s2=rnorm(a,5-theta,1)
  p1=SIGN.test(s1,s2,alternative = "greater",md=0)$p.value
  p2=wilcox.test(s1,s2,alternative = "greater")$p.value
  p3=t.test(s1,s2,alternative = "greater")$p.value
  c(p1,p2,p3)
}

par(mfrow=c(2,2),oma=c (0,0,2,0))

  for(theta in c(1.1,1.3,1.4,2)){
  for(i in 1:length(n)){
    p.value=replicate(R,pvalue_SIGN_wsr_t(n[i]))
    power.SIGN[i]=mean(p.value[1,]<0.05)
    power.wsr[i]=mean(p.value[2,]<0.05)
    power.t[i]=mean(p.value[3,]<0.05)
  }
  plot(n,power.SIGN,type="l",col=1,ylab="Power",xlab="n",main=paste("theta = ",theta),sub="Paired sample Test for Location")
  lines(n,power.wsr,type="l",col=3)
  lines(n,power.t,type="l",col=4)
  legend("bottomright",legend=c("Sign Test","Wilcoxon Rank Test","t test"),lwd=1,col=c(1,3,4),cex=0.5)
}
mtext("Power vs N curve", line=0, side=3, outer=TRUE, cex=1.5)

