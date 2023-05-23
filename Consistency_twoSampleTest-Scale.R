rm(list=ls())

#consistency of Two Sample Test for Scale

n=seq(5,60,length=10)
m=seq(6,72,length=10)
N=m+n
theta=3
R=100
power.ansari = power.mood = array(0)


pvalue_ansari_mood=function(a,b){
  s1=rnorm(a,5,1)
  s2=rnorm(b,5,1/theta)
  p1=ansari.test(s1,s2,alternative="greater")$p.value
  p2=mood.test(s1,s2,alternative = "greater")$p.value
  c(p1,p2)
}

par(mfrow=c(2,2),oma=c (0,0,2,0))

for(theta in c(1.2,4,5,7)){
  for(i in 1:length(n)){
    p.value=replicate(R,pvalue_ansari_mood(n[i],m[i]))
    power.ansari[i]=mean(p.value[1,]<0.05)
    power.mood[i]=mean(p.value[2,]<0.05)
  }
  plot(N,power.ansari,type="l",col=1,ylab="Power",xlab="N=m+n",main=paste("theta = ",theta),sub="Two sample Test for scale")
  lines(N,power.mood,type="l",col=4)
  legend("bottomright",legend=c("Mood Rank Test","Ansari Bradley Test"),lwd=1,col=c(3,1),cex=0.8)
}
mtext("Power vs N curve", line=0, side=3, outer=TRUE, cex=1.5)

