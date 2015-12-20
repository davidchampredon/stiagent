
### Syphilis (Tp)

piv = 2
a = 3

xx = seq(0,5,by=0.01)

fx = 0.9/(1+exp(a*(xx-piv)))
par(mfrow=c(1,1))
plot(xx,fx,typ="l",lwd=3,ylim=c(0,1))
abline(v=seq(0,2,by=0.5),col="gray")
grid()