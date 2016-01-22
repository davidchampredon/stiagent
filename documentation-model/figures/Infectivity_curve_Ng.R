pdf("IC_Ng.pdf")

t <- seq(0,2,by=0.001)

t.latent <- 1/52

a.sympt <- 10
nn.sympt <- exp(-1/2)/sqrt(2*a.sympt)

a.asympt <- 10*a.sympt
nn.asympt <- exp(-1/2)/sqrt(2*a.asympt)

s <- pmax(t-t.latent,0)

IC.sympt <- s^0.5*exp(-a.sympt*s)/nn.sympt

plot(t,IC.sympt,typ="l",lwd=6, 
     ylab="Gonorrohea Infectivity Curve",
     xlab= "time since infection")

#abline(v=1/2/a.sympt+t.latent,lty=2)
grid()

dev.off()