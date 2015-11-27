library(ggplot2)

simul = read.csv("simul.out",header=TRUE)
deaths = read.csv("deaths.out",header=TRUE)

par(mfrow=c(3,1))

# ====== Population size =======

plot(x=simul$time, y=simul$nAlive, typ="l", lwd=3, col="darkgreen",
     main = "Population", ylim=c(0,max(simul$nAlive)))
lines(x=simul$time, y=simul$nDead, typ="l", lwd=3, col="red")
legend(x="bottomright",legend=c("Total","Deaths"),col=c("darkgreen","red"),lwd=c(3,3))
grid()
# ====== PARTNERSHIPS =======

plot(x=simul$time,y=simul$nPartn, typ="l", lwd=3,
     main = "Number of partnerships")
lines(x=simul$time,y=simul$nSp,typ="l", col="red",lwd=3)
legend(x="topleft",legend=c("Total","Spousal"),col=c("black","red"),lwd=c(3,3))
grid()

prop.sp = simul$nSp/simul$nPartn
plot(x=simul$time, y=prop.sp,typ="l",ylim=c(0,1),col="blue",lwd=6,
     main="Proportion of spousal partnerships") 
grid()

#hist(deaths$Age,col="gray", breaks=c(15:80))

# ====== STIs =======

par(mfrow=c(1,1))
# Transmission events
stiTrans = read.csv("transmissionEvents.out", header=FALSE)
names(stiTrans) = c("time","STIname","UIDinfect","UIDsuscep")

time = stiTrans$time
sti = stiTrans$STIname
uid.i = stiTrans$UIDinfect
uid.s = stiTrans$UIDsuscep

library(plyr)
stievent = ddply(.data=stiTrans, .(time, STIname), function(df) length(df$time) )
names(stievent)[3]="nEvents"

plot(x=stievent$time, y=stievent$nEvents, 
     main = "Transmission Events",
     type="h", lwd=6)

# STI prevalence

c.sti = which(colnames(simul)=="HIV")
themax = max(simul[,c.sti:(c.sti+8)])

mylwd = 3
mycol = c(1:9)
h = colnames(simul)

plot(x=simul$time, y=simul[,c.sti], typ="l", 
     ylim=c(0,themax), lwd=mylwd,
     main = "STI prevalences")

text(x=0,y=simul[1,c.sti],labels=h[c.sti],col=mycol[1],pos=3)

for (i in 1:8)
{
  lines(x=simul$time, y=simul[,c.sti+i],
      lwd=mylwd, col=mycol[i+1])
  
  text(x=0,y=simul[1,c.sti+i],labels=h[c.sti+i],col=mycol[i+1],pos=3)
}
grid()


# ======== INDIVIDUALS ===========

indivSimul = read.csv("simul_indiv.out",header=TRUE)


# Monitor 2 specific individuals
u1 =  indivSimul[indivSimul$UID==303,] 
u2 =  indivSimul[indivSimul$UID==545,]
names(u2) = paste0(names(u2),rep(".p",n=length(names(u2))))
uu = cbind(u1,u2)

# HIV positives
hiv = indivSimul[indivSimul$HIVdur>0 & !is.na(indivSimul$UIDpartner0),]

# Ct positives
Ct = indivSimul[indivSimul$HIVdur>0 & !is.na(indivSimul$UIDpartner0),]


