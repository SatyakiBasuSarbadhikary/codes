rm(list=ls())

library(actuar)#for ZTP
library(nleqslv)#for nleqslv

n=20
R=1000
lambda=1.5

ZTP_mom=function(lambda)
{
  return((lambda/(1-exp(-lambda)))-xbar)
}

lambda.hat=array(0)
for(i in 1:R)
{
  sample=rztpois(n, lambda)
  xbar=mean(sample)
  lambda.hat[i]=nleqslv(xbar,ZTP_mom,method="Newton")
}
lambda.hat

plot(1:R,lambda.hat)
abline(h=lambda,col=2,lwd=3)
