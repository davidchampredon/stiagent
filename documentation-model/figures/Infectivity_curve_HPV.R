pdf("./IC_HPV.pdf")

tmax <- 2

Tl <- 0.33
t<- seq(0,tmax,by=0.001)
hpv <- 0.9
kk<-5

eta0 <- exp(-1)/kk
IC <- (pmax(t-Tl,0)/hpv)^1*exp(-kk*(t-Tl)/hpv)/eta0

plot(t,IC,typ="l",
     xlab="Time since infection", ylab="HPV infectivity curve",
     lwd=6)

b<- 1/eta0
yy <- 1-exp(-b*pmax(t-Tl,0)/hpv)
lines(t,yy,lwd=6,lty=2)

text(x=tmax*0.9,y=0.2,labels=paste("TlatHPV=",Tl))
text(x=tmax*0.9,y=0.3,labels=paste("kHPV=",kk))
text(x=tmax*0.9,y=0.4,labels=paste("tauHPV=",hpv))

grid()

dev.off()