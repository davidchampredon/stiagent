TREstar <- function(t,A,od=1){
	return(A*(t-od)^2/od/od+1-A)
}
od <-1
t <- seq(0,1,length.out = 100)
lwd <- 6

pdf("TRE.pdf",width = 8, height = 8)

plot(t,TREstar(t,A=1,od), typ="l", 
	 lwd=lwd,
	 xlim=c(0,od*1.2),
	 ylim = c(0,1.2),
	 las=1,
	 xaxt='n', xlab="Treatment duration",
	 ylab="TRE")
lines(t,TREstar(t,A=0.5,od), lwd=lwd)
lines(t,TREstar(t,A=0,od), lwd=lwd)
abline(v=1,lty=2,lwd=lwd/3)


text(x=od/2,y=1.06, labels = "A=0 or TMS=0")
text(x=od/2,y=0.75, labels = "0<A<1 or TMS=1")
text(x=od/2,y=0.4, labels = "A=1 or TMS=1")
text(x=od,y=0.0, labels = "TD*", pos=4)

dev.off()
