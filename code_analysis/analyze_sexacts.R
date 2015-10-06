### ANALYZE SEX ACTS FROM SIMULATIONS

DIR_OUT="../OUT/"
DIR_INPUTS = "../"
save.to.file = TRUE

if (save.to.file) pdf("analyze_sexActs.pdf",width=20,height=15)

library(ggplot2)
library(gridExtra)
library(plyr)
library(reshape)
source("analyze_sexacts_FCT.R")
theme_set(theme_bw())


### Reading output data from simulation

# dat = read.csv(paste0(DIR_OUT,"sexReduction.out"))
# dat2 = dat[dat$nPartners>0,]
# young.age = 35
# dat2.young=dat2[dat2$age<young.age,]

dat.dp = read.csv(paste0(DIR_OUT,"sexDistribPartn.out"),header=FALSE)
names(dat.dp)=c("time","uid","rskgrp","anysti","nsex.s","nsex.c","nsex.w","Ns","Nc")

dat.nsex <- read.csv(paste0(DIR_OUT,"sexacts.out"),header=FALSE)
names(dat.nsex)=c("time","uid","gender", "rskgrp","anysti","symptom","age", "nsex")

last.pop = read.csv(paste0(DIR_OUT,"last_population.out"))




### Sex distribution among partners
dat.dp$nsex.all <- dat.dp$nsex.s+dat.dp$nsex.c+dat.dp$nsex.w
dat.dp$rs <- dat.dp$Ns/(dat.dp$Ns+dat.dp$Nc)

dat.dp$ps <- dat.dp$nsex.s/dat.dp$nsex.all
dat.dp$pc <- dat.dp$nsex.c/dat.dp$nsex.all
dat.dp$pw <- dat.dp$nsex.w/dat.dp$nsex.all

# Individuals who have at least on casual and one spouse
dat.dp.c <- dat.dp[dat.dp$Ns>0 & dat.dp$Nc>0,]


g.firstSex <- plot.ageFirstSex(last.pop)
### AGE AT FIRST SEX FOR ONLY 1 MC ITERATION


g.nsex <- plot.nsex(dat.nsex)
g.nsex.age <- plot.nsex.age(dat.nsex)
g.nsex.rsk <- plot.nsex.rsk(dat.nsex)
g.alpha <- plot.check.spouse.prop(dat.dp.c)
g.sympt <- plot.nsex.symptom(dat.nsex)
g.sex.partnertype <- plot.sex.partnertype(dat.dp.c)

grid.arrange(g.firstSex[[1]],
             g.firstSex[[2]],
             g.nsex,
             g.nsex.age,
             g.nsex.rsk,
             g.sympt,
             g.sex.partnertype,
             g.alpha)


if (save.to.file) dev.off()




# plot.Rsex = ggplot(dat2,aes(x=Rsex,fill=factor(h_sti)))+geom_histogram(binwidth=10)+facet_wrap(~riskGroup,scales="free_y")
# plot.Rsex = plot.Rsex + ggtitle("Male Sex rate by risk group\n whole population")
# plot.Rsex = plot.Rsex + scale_fill_manual(values = c("red","blue"),
#                                           labels=c("STI symptoms","asymptomatic"))
# 
# plot.Nsex = ggplot(dat2,aes(x=NsexActs,fill=factor(h_sti)))
# plot.Nsex = plot.Nsex + geom_histogram(binwidth=1)+facet_wrap(~riskGroup)
# plot.Nsex = plot.Nsex + ggtitle("Male number sex acts per period by risk group\n whole population")
# plot.Nsex = plot.Nsex + scale_fill_manual(values = c("red","blue"),
#                                           labels=c("STI symptoms","asymptomatic"))
# 

# plot.Rsex.young = ggplot(dat2.young,aes(x=Rsex,fill=factor(h_sti)))+geom_histogram(binwidth=10)+facet_wrap(~riskGroup)
# plot.Rsex.young = plot.Rsex.young + ggtitle(paste("Male Sex Rate\n young population only (<",young.age,")"))
# plot.Rsex.young = plot.Rsex.young + scale_fill_manual(values = c("red","blue"),
#                                                       labels=c("STI symptoms","asymptomatic"))


# plot.Nsex.young = ggplot(dat2.young,aes(x=NsexActs,fill=factor(h_sti)))+geom_histogram(binwidth=1)+facet_wrap(~riskGroup)
# plot.Nsex.young = plot.Nsex.young + ggtitle(paste("Male Number sex acts\n young population only (<",young.age,")"))
# plot.Nsex.young = plot.Nsex.young + scale_fill_manual(values = c("red","blue"),
#                                                       labels=c("STI symptoms","asymptomatic"))



# plot.Rm = ggplot(dat2,aes(x=Rm,fill=factor(h_sti)))+geom_histogram(binwidth=0.05)+facet_wrap(~riskGroup)
# plot.Rm = plot.Rm +ggtitle("Male Sex acts reduction factor\n(by risk group)")
# 
# 
# dat2$age2 <- ceiling(dat2$age/2)*2
# dat3 <- ddply(dat2,.variables = c("age2","riskGroup"),
#               summarize,rsex.m=mean(Rsex),rsex.s=sd(Rsex))
# 
# plot.Rsex.age = ggplot(dat3,aes(x=age2,y=rsex.m,colour=factor(riskGroup)))
# plot.Rsex.age = plot.Rsex.age+geom_line(size=3)+ggtitle("Male Sex rate and age")
# plot.Rsex.age = plot.Rsex.age+xlab("Age")+ylab("Sex Acts Rate")
# 
# plot.Nsex.nPrtn = ggplot(dat2,aes(x=factor(nPartners),y=NsexActs,fill=factor(riskGroup)))+geom_boxplot()+ggtitle("Male Sex acts and number of partners")

