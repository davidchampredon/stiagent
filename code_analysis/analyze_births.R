#####################################
### Read nursery from simulation
#####################################

library(plyr)
library(ggplot2);theme_set(theme_bw())
library(gridExtra)
source("analyze_UTILS.R")
source("analyze_simulation_FCT.R")
DIR_OUT="../OUT/"

savetofile =TRUE

### Read data files
nMC = as.integer(system(paste0("ls -l ",DIR_OUT,"nursery_* | wc -l"),intern = TRUE))
nurs = read.simulation.file("nursery_")
mtct.attempts = read.csv(paste0(DIR_OUT,"mtct_attempts.out"),header=FALSE)


### Reformat with binary variable (duration meaningless in nursery)
stinames = c("HIV","HSV2","HPV","Ct","Ng","Tp","Hd","Bv","Tv")
colidx = which(names(nurs) %in% stinames)
nurs[,colidx] = as.integer(nurs[,colidx]>0)

### Birth time series
nurs$dob.round <- round(nurs$dob,digits = 1)
nurs2 = ddply(nurs,c("dob.round","iMC"),summarize,n=length(uid_mother))
nurs3 = ddply(nurs2,c("dob.round"),summarize,mean.births=mean(n))

g.ts = ggplot(nurs3,aes(x=dob.round,y=mean.births))+geom_line()+geom_smooth(method="auto",size=2)
g.ts = g.ts+ggtitle("Birth time series")+xlab("Date of birth")

### MTCT global
mg = ddply(mtct.attempts,"V1",summarize,mean=mean(V2))
g.mg = ggplot(mg,aes(x=V1,y=mean))+geom_bar(stat="identity")+ coord_cartesian(ylim = c(0,1))
g.mg = g.mg + ggtitle("Mean MTCT rate by STI")+xlab("")+ylab("")

### MTCT incidence
nurs$HIVpos <- nurs$HIV>0
nurs$Tppos <- nurs$Tp>0

nurs.melt <- melt(nurs,measure.vars = c("HIVpos","Tppos"))

z <- ddply(nurs.melt,c("dob", "variable"),summarize,m=sum(value))

g.inc = ggplot(z,aes(x=dob,y=m))+geom_line()+facet_wrap(~variable)+geom_smooth(size=2)
g.inc = g.inc + ggtitle("Mean MTCT incidence by STI")+xlab("Date of birth (years)")


### Number of birth per mother
tmp = ddply(nurs,"uid_mother",summarize,n=length(dob))
g.nb = ggplot(tmp,aes(x=n))+geom_histogram(binwidth = 1,fill="grey",colour="darkgrey")
g.nb = g.nb + ggtitle("Number of births per mother")+xlab("Number of births")+ylab("")

### Age of mothers
g.age = ggplot(nurs.melt)+geom_histogram(aes(x=age_mother,y=..density..),binwidth = 1,
                                         fill="grey",colour="darkgrey")
g.age = g.age + ggtitle("Mothers' age at child birth")+xlab("Age")
g.age

#### Final plot

if(savetofile) pdf("analyze_births.pdf",width=20,height=15)

grid.arrange(g.ts,
             g.nb,
             g.age,
             g.mg,
             g.inc,
             nrow=2)

if(savetofile) dev.off()
