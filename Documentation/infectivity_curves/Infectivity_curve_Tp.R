### Infectivity curve for Syphilis (Tp) ###

savetofile = 0#TRUE

pseudo.beta <- function(x,alpha,beta)
{
  cst<- ((alpha-1)/(alpha+beta-2))^(alpha-1)*((beta-1)/(alpha+beta-2))^(beta-1)
  return(x^(alpha-1)*(1-x)^(beta-1)/cst)
}

tt<-seq(0,2,by=0.001)



### Primary syphilis

# Parameters
latent <- 20/365
dur.primary.chancre <- 5/52
primary.virulence <- 0.7
a1 <- 1.5
b1 <- 4

IC.Tp.1 <- function(t, a1,b1,
                    latent,
                    dur.primary.chancre,
                    primary.virulence)
{
  res <- 0
  
  if (t>latent & t<dur.primary.chancre+latent)
  {
    x<-t-latent
    res <- pseudo.beta(x/dur.primary.chancre,a1,b1)
    res <- res*primary.virulence
  }
  return(res)
}



### Secondary Syphilis

dur.from.infection <- latent + 7/52
dur.second.lesion <- 8/52
second.virulence <- 0.7
second.virulence.condylomata <- 1
a2 <- 1.5
b2 <- 4
# For condylomata (highly infectious)
a2.condy <- 1.1
b2.condy <- 1.8

IC.Tp.2 <- function(t, a2,b2,
                    dur.from.infection,
                    dur.second.lesion,
                    second.virulence)
{
  res <- 0
  
  if (t>dur.from.infection & t<dur.from.infection+dur.second.lesion)
  {
    x<-t-dur.from.infection
    res <- pseudo.beta(x/dur.second.lesion,a2,b2)
    res <- res*second.virulence
  }
  return(res)
}


### Early Latent Syphilis

dur.from.infection.earlyLatent <- latent + 11/12
dur.earlyLatent.lesion <- 4/52
#num.recurrence <- 2
gap.recurrence <- 1/12
earlyLatent.virulence <- 0.5
a3 <- a2
b3 <- b2


### Merge all syphilis stages

ic1 <- numeric(length(tt))
ic2 <- numeric(length(tt))
ic2condy <- numeric(length(tt))
ic3.a <- numeric(length(tt))
#ic3.b <- numeric(length(tt))

for(i in 1:length(tt))
{
  ic1[i]<- IC.Tp.1(tt[i],a1,b1,latent,dur.primary.chancre,primary.virulence)
  
  ic2[i]<- IC.Tp.2(tt[i],a2,b2,dur.from.infection,dur.second.lesion,second.virulence)
  ic2condy[i]<- IC.Tp.2(tt[i],a2.condy,b2.condy,
                        dur.from.infection,dur.second.lesion,second.virulence.condylomata)
  
  ic3.a[i] <- IC.Tp.2(tt[i],a3,b3,
                      dur.from.infection.earlyLatent,
                      dur.earlyLatent.lesion,earlyLatent.virulence)
#   ic3.b[i] <- IC.Tp.2(tt[i],a3,b3,
#                       dur.from.infection.earlyLatent+(dur.earlyLatent.lesion+gap.recurrence),
#                       dur.earlyLatent.lesion,earlyLatent.virulence)
  
  
}

### Plot all infectivity curves

mylwd=6
ymax=1.1
textcex=0.75

if (savetofile) pdf("IC_Tp.pdf")
plot(tt,ic1,typ="l",lwd=mylwd,
     ylim=c(0,ymax),
     main="Infectivity Curve of Syphilis",
	 xlab="Time since infection (years)",ylab="IC syphilis")
lines(tt,ic2,lwd=mylwd)
lines(tt,ic2condy,lwd=mylwd/2)#,lty=5)
lines(tt,ic3.a,lwd=mylwd)
#lines(tt,ic3.b,lwd=mylwd)

end.1 <- latent+dur.primary.chancre
end.2 <- dur.from.infection + dur.second.lesion
end.all <- c(end.1,end.2,dur.from.infection.earlyLatent)
#abline(v=end.all,lty=2,col="gray")

text(x = 0,y=ymax,labels = "Primary",pos = 4,cex = textcex)
text(x = end.1,y=ymax,labels = "Secondary",pos = 4,cex = textcex)
text(x = dur.from.infection.earlyLatent,y=ymax,labels = "Early Latent",pos = 4,cex = textcex)
if (savetofile)  dev.off()

#grid()




