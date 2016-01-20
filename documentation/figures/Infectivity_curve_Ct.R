pdf("IC_Ct.pdf")

pseudo.beta <- function(x,alpha,beta)
{
  if(x<0 || x>1) res=0
  if (x>=0 & x<=1){
    cst<- ((alpha-1)/(alpha+beta-2))^(alpha-1)*((beta-1)/(alpha+beta-2))^(beta-1)
    res = x^(alpha-1)*(1-x)^(beta-1)/cst
  }
  return(res)
}

t <- seq(0,1,by=0.001)

L <- 2/52

# asymptomatic
a<-1.5
b<-4
doi<-0.75

# symptomatic
as<-1.1
bs<-1.8
dois<-1/12


tt<-pmax(0,t-L)/doi
IC.asympt <- sapply(tt,pseudo.beta,a,b)

tts<-pmax(0,t-L)/dois
IC.sympt <- sapply(tts,pseudo.beta,as,bs)


plot(t,IC.sympt,typ="l",lwd=6, ylab="Chlamydia Infectivity Curve")
lines(t,IC.asympt,lty=2,lwd=6)
grid()

dev.off()