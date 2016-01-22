
### FORMATION - AGE

age.min = 12
age.start = 15
age.piv = 45
slope = 0.33
pmin = 0.1

aa = seq(age.min,80,by=0.01)

crv1 = pmin + (1-pmin)*(aa-age.min)/(age.start-age.min)
crv1 = crv1*(aa<age.start)
crv2 = pmin+(1-pmin)/(1+exp(slope*(aa-age.piv)))
crv2 = crv2*(aa>=age.start)

pdf("formation_age.pdf")
plot(aa,crv1+crv2,
	 typ="l",lwd=2,
	 xlab="Female age",
	 ylab = "f_age",
     ylim=c(0,1),main="Formation - Female Age Component")
grid()
dev.off()


### FORMATION - AGE GAP

pmin = 0.05
gapmin = -10
gap = seq(gapmin-10,50,by=0.01)

gapavg=7
a=1.5
d=1.5
b=a/(d*(gapavg-gapmin)^d)

fmax = (a/(b*d*exp(1)))^(a/d)

xx = gap - gapmin
g.gap.tmp = xx^a*exp(-b*xx^d)/fmax
g.gap = pmin + (1-pmin)*g.gap.tmp

pdf("formation_agegap.pdf")
plot(gap,g.gap,
	 typ="l",lwd=2, 
	 xlab="Age Gap",
	 ylab="f_agegap",
     ylim=c(0,1), main="Formation - Age gap Component")
abline(v=gapavg,lty=2)
abline(h=pmin,lty=2)
grid()
dev.off()


### FORMATION - RISK GROUP

s0 <- 0.4 #0.18
s1 <- 0.8 #0.09

form_risk <- function(s0,s1,rstar,rf,rm)
{
  return(exp(-s0*(2*rstar-rf-rm)-s1*abs(rf-rm)))
}

rf.rng <- c(0,1,2)
rm.rng <- c(0,1,2)
nn <- length(rf.rng)*length(rm.rng)

rstar <- max(rf.rng,rm.rng)
rf <- numeric(nn)
rm <- numeric(nn)
f <- numeric(nn)

cnt<-1
for(i in 1:length(rf.rng))
{
	for(j in 1:length(rm.rng))
	{
		rf[cnt] <- rf.rng[i]
		rm[cnt] <- rm.rng[j]
		f[cnt] <- form_risk(s0,s1,rstar,rf[cnt],rm[cnt])
		cnt <- cnt+1
	}
}

df <- data.frame(rf,rm,f)
df$riskGroup.f <- factor(df$rf)
df$riskGroup.m <- factor(df$rm)

library(ggplot2) ; theme_set(theme_bw())
g <- ggplot(df,aes(x=riskGroup.f,y=f,fill=riskGroup.m))
g <- g + geom_bar(position="dodge",stat="identity",col='gray')
g <- g + scale_fill_brewer(palette="Reds")
g <- g + ggtitle("Formation probability - Risk Group Component")
g <- g + ylab("f_riskgroup")
pdf("formation_riskgroup.pdf")
plot(g)
dev.off()


### FORMATION - PARTNERSHIP DEFICIT

def1 = c(0,0.3,0.5,0.7,1)
def2 = seq(0,1,by=0.01)
pp = 0.4
g.min = 0.0

g.def = matrix(nrow = length(def2), ncol=length(def1))

pdf("formation_deficit.pdf")
for (i in 1:length(def1))
{
  g.def[,i] = g.min +(1-g.min)*((def1[i])^pp) * ((def2)^pp)
  
  if(i==1) plot(def2,g.def[,i],typ="l",
                main = "Formation - Partnership Deficit",
                ylab="",xlim=c(0,1.6), xlab="deficit partner #2",
                ylim=c(0,1))
  if(i>1) lines(def2,g.def[,i])
  text(1,g.def[nrow(g.def),i],labels = paste("deficit partner #1 =",def1[i]),pos=4)
}
dev.off()

