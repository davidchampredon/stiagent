rmf = c(0:4)
rmax= 4

k1 = 0.2
k2 = 3

p0 = k1*exp(-k2*rmf/rmax)
p0

#pdf("sexAct_condom.pdf")
plot(rmf,p0,ylim=c(0,1),typ="o",pch=16,
	 main="Probability to use condom",
	 xlab = "Sum of risk group both partners")
grid()
#dev.off()
