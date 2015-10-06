

plot.ageFirstSex <- function(last.pop){
  # Age distribution age first sex
  last.pop.sex = subset(last.pop,ageFirstSex>0)
  g.firstsex = ggplot(last.pop.sex)+geom_density(aes(x=ageFirstSex,
                                                     colour=factor(gender)),
                                                 size=1 )+ ggtitle("Age at First Sex")
  
  g.firstsex.rsk = ggplot(last.pop.sex)+geom_density(aes(x=ageFirstSex,
                                                         colour=factor(riskGroup)),
                                                     size=2 )
  g.firstsex.rsk = g.firstsex.rsk + facet_wrap(~gender)+scale_color_brewer(palette = "Reds")
  g.firstsex.rsk = g.firstsex.rsk + ggtitle("Age at First Sex")
  return(list(g.firstsex,g.firstsex.rsk))
}



plot.nsex <- function(dat.nsex){
  
  # Number of sex acts per period
  
  dat.nsex.rskgrp <- ddply(dat.nsex,.variables = c("uid","gender","rskgrp"),#c("time","uid","gender","rskgrp"),
                           summarize,nsex=mean(nsex))
  dt <- unique(dat.nsex$time)[1:2]
  timestep <- dt[2]-dt[1]
  dat.nsex.rskgrp$nsex.per.month <- dat.nsex.rskgrp$nsex/timestep/12
  dat.nsex.rskgrp$rskgrp <- paste("risk",dat.nsex.rskgrp$rskgrp,sep="_")
  
  g.nsex.month <- ggplot(dat.nsex.rskgrp)
  g.nsex.month <- g.nsex.month + geom_density(aes(x=nsex.per.month,
                                                  fill=factor(rskgrp),
                                                  col=factor(rskgrp)),
                                              size =2 ,
                                              alpha=0.3)
  g.nsex.month
  g.nsex.month <- g.nsex.month + facet_wrap(~gender,ncol=4,scales = "free_y") + scale_color_brewer(palette="Reds")+ scale_fill_brewer(palette="Reds")
  g.nsex.month <- g.nsex.month + ggtitle("Number of sex acts per month by gender and risk group")
  return(g.nsex.month)
}

plot.nsex.rsk <- function(dat.nsex){
  tvec <- sort(unique(dat.nsex$time))
  dt <- tvec[2]-tvec[1]
  dat.nsex$nsex.month <- dat.nsex$nsex/dt/12
  g = ggplot(dat.nsex)
  g = g +geom_violin(aes(y=nsex.month,x=factor(rskgrp),fill=factor(rskgrp)),adjust=1.8)
  g = g +geom_boxplot(aes(y=nsex.month,x=factor(rskgrp)),alpha=0.1)#,fill=factor(rskgrp)))
  g = g +facet_wrap(~gender)+scale_fill_brewer(palette = "Reds")
  g = g + scale_y_log10()+annotation_logticks(sides="lr")
  g = g + ggtitle("Number of sex act per month by risk factor")+xlab("Risk Group")
  return(g)
}



plot.nsex.symptom <- function(dat.nsex){
  
  df <- ddply(dat.nsex,.variables = c("uid","gender","symptom"),
              summarize,nsex=mean(nsex))
  dt <- unique(dat.nsex$time)[1:2]
  timestep <- dt[2]-dt[1]
  df$nsex.per.month <- df$nsex/timestep/12
  df$symptom <- paste("symptom",df$symptom,sep="_")
  
  g <- ggplot(df)
  g <- g + geom_density(aes(x=nsex.per.month,
                            fill=factor(symptom),
                            col=factor(symptom)),
                        size =2 ,
                        alpha=0.15,
                        adjust=2)
  g <- g + facet_wrap(~gender,ncol=4,scales = "free_y") 
  g <- g + ggtitle("Number of sex acts per month by symptomatic status")
  return(g)
}

plot.nsex.age <- function(dat.nsex){
  
  # Number of sex acts per period
  
  dat.nsex$ageGroup <- round(dat.nsex$age/1)*1
  
  df <- ddply(dat.nsex,.variables = c("uid","gender","ageGroup","rskgrp"),
              summarize,nsex=mean(nsex))
  
  dt <- unique(dat.nsex$time)[1:2]
  timestep <- dt[2]-dt[1]
  df$nsex.per.month <- df$nsex/timestep/12
  
  df2 <- ddply(df, c("gender","ageGroup","rskgrp"), summarize,
               m = mean(nsex.per.month))
  
  g <- ggplot(df2)+geom_line(aes(x=ageGroup,y=m,colour=factor(rskgrp)),size=2)
  g <- g + facet_wrap(~gender) + scale_color_brewer(palette="Reds")+ scale_fill_brewer(palette="Reds")
  g <- g + ggtitle("Mean Number of sex acts per month by age")
  g
  return(g)
}




plot.sex.partnertype <- function(dat.dp.c){
  # Distribution of proportion of sex acts  with partner type
  
  dat.dp.c.m <- melt(dat.dp.c,measure.vars = c("ps","pc","pw"))
  
  g.nsex <- ggplot(dat.dp.c.m)+geom_histogram(aes(x=value,y=1+..count..,fill=factor(rskgrp)),
                                              binwidth=0.1)
  g.nsex <- g.nsex + facet_wrap(~variable+rskgrp,ncol = 3,scales="free_y")+scale_y_log10()+scale_fill_brewer(palette = "Reds")
  g.nsex <- g.nsex + ggtitle("Distribution of proportion of sex acts \nby partner type and risk group")+ylab("")+xlab("Proportion")
  g.nsex
  
}

plot.check.spouse.prop <- function(dat.dp.c){
  alpha.sim = mean(dat.dp.c$ps/dat.dp.c$rs)
  tmp = read.csv(paste0(DIR_INPUTS,"in_paramSexActivity.csv"),header=FALSE)
  alpha.desired = tmp[tmp$V1=="_sexAct_proba_distribute_partnerTypes_prefSpouse",2]
  
  df.alpha <- data.frame(x=c(0,0),a=c(alpha.sim,alpha.desired))
  
  g.alpha = ggplot(df.alpha)+geom_point(aes(x=x,
                                            y=a,
                                            col=factor(a),
                                            shape=factor(a)),
                                        size=4)
  g.alpha = g.alpha +scale_y_continuous(limits=c(0,3)) 
  g.alpha = g.alpha + scale_color_manual(values = c(1,2),label=c("Simulation","Desired"))
  g.alpha = g.alpha + ggtitle("Comparison spouse preference\n Simulation vs Desired")
  return(g.alpha)
}
