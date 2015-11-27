plot.infector.gender <- function(transm){
  t.gender <- ddply(transm,c("stiname","gender_from"),summarize,n=length(gender_from))
  t.gender
  g = ggplot(t.gender)+geom_bar(aes(x=factor(gender_from),y=n,fill=factor(gender_from)),stat="identity")+facet_wrap(~stiname,scales="free_y")
  g = g + ggtitle("Transmission by infector's gender")+xlab("Infector Gender")
  return(g)
}


plot.infector.riskGroup<- function(transm){
  x <- ddply(transm,c("stiname","riskGroup_from","riskGroup_to"),summarize,n=length(stiname))
  x
  g = ggplot(x)+geom_bar(aes(x=factor(riskGroup_from),y=n,fill=factor(riskGroup_to)),stat="identity")+facet_wrap(~stiname,scales="free_y")
  g = g + scale_fill_brewer(palette="Reds")
  g = g + ggtitle("Transmission by infector's risk group")+xlab("Infector's risk group")
  return(g)
}

plot.generation.interval <- function(transm)
{
  transm$GI <- round(transm$stiduration_from,digits = 2)
  x <- ddply(transm,c("stiname","GI"),summarize,n=length(stiname))
  x
  g = ggplot(x)+geom_density(aes(x=GI,fill=stiname))+facet_wrap(~stiname,scales="free")
  g = g+ggtitle("Generation Interval") + xlab("Years")
}

plot.incid.risk.sti.time <- function(transm){
  
  z = ddply(transm,c("time","riskGroup_from","stiname"),summarize,n=length(time))

  g = ggplot(z)+geom_step(aes(x=time,y=n,colour=factor(riskGroup_from)),size=1.5)
  g = g +facet_wrap(~riskGroup_from+stiname,ncol=2)
  g = g+scale_colour_brewer(palette="Reds")
  g = g + ggtitle("Incidence by risk group and STI")
  return(g)
}
