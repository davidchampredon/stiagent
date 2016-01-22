a <- 0.9
b <- 3.0
tt <- 2.0

tvec <- seq(0,5,by=0.1)

p <- a/(1+exp(b*(tvec-tt)))

pdf("proba_MTCT_Tp.pdf")
plot(tvec,p,type="l", lwd=6, las=1,
	 ylim=c(0,1),
	 xlab="Duration of infection (in years)", ylab="probability",
	 main="Syphilis MTCT Probability")
grid()
dev.off()