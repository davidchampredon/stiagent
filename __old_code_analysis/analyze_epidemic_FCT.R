

STIinfection.label <- function(last.pop){
  res <- rep(x = "none", times= nrow(last.pop))
  for(i in 1:nrow(last.pop)){
    if(last.pop$Tppos[i] & last.pop$HIVpos[i]) res[i] <- "HIVTp"
    if(last.pop$Tppos[i] & !last.pop$HIVpos[i]) res[i] <- "Tp"
    if(!last.pop$Tppos[i] & last.pop$HIVpos[i]) res[i] <- "HIV"
  }
  return(res)
}




plot.STIincid.Tp <- function(){
  incid = read.simulation.file("incid_mc")
  incid$time2 <- round(incid$time)
  incid2 <- ddply(incid,c("time2","iMC"), summarize, n = sum(Tp))
  incid3 <- ddply(incid2,c("time2"),summarize,
                  inc = mean(n),
                  inc.min=min(n),
                  inc.max=max(n))
  g <- ggplot(incid3)+geom_pointrange(aes(x=time2,
                                          y=inc,
                                          ymin=inc.min,
                                          ymax=inc.max),
                                      size=1.5, colour="grey")
  g <- g + geom_line(aes(x=time2,y=inc),size=2)
  g <- g + ggtitle("Incidence Tp")+xlab("Time")+ylab("Incidence")
  return(g)
}

plot.STIincid.HIV <- function(){
  incid = read.simulation.file("incid_mc")
  incid$time2 <- round(incid$time)
  incid2 <- ddply(incid,c("time2","iMC"), summarize, n = sum(HIV))
  incid3 <- ddply(incid2,c("time2"),summarize,
                  inc = mean(n),
                  inc.min=min(n),
                  inc.max=max(n))
  g <- ggplot(incid3)+geom_pointrange(aes(x=time2,
                                          y=inc,
                                          ymin=inc.min,
                                          ymax=inc.max),
                                      size=1.5, colour="grey")
  g <- g + geom_line(aes(x=time2,y=inc),size=2)
  g <- g + ggtitle("Incidence HIV")+xlab("Time")+ylab("Incidence")
  return(g)
}


plot.STI_prev_timeseries <- function(simul, stinames)
{
  # stinames = vector of STI names to be plotted, i.e c("HIV","Ct")
  
  simul1 = melt(simul,measure.vars=stinames)
  simul1$prev = simul1$value/simul1$nAlive
  
  simul2 = ddply(simul1,.variables = c("time","variable"),summarize,
                 prev.mean = mean(prev),
                 prev.min = min(prev),
                 prev.max = max(prev),
                 prev.q25 = quantile(prev,probs = 0.25),
                 prev.q75 = quantile(prev,probs = 0.75)
  )
  n.sim <- length(unique(simul$iMC))
  title <- paste0("STI prevalences (",n.sim," non-extinct MC)")
  g.sti.prev = ggplot(simul2,aes(x=time))+geom_line(aes(y=prev.mean,colour=variable),size=3)
  g.sti.prev = g.sti.prev + ggtitle(title)
  # g.sti.prev = g.sti.prev + geom_ribbon(aes(ymin=prev.min,ymax=prev.max,fill=variable),alpha=0.1)
  g.sti.prev = g.sti.prev + geom_ribbon(aes(ymin=prev.q25,ymax=prev.q75,fill=variable),alpha=0.2)
  g.sti.prev = g.sti.prev + scale_color_brewer(palette="Set2")+ scale_fill_brewer(palette="Set2")
  g.sti.prev = g.sti.prev + geom_line(aes(y=prev.min,colour=variable),size=0.25)
  g.sti.prev = g.sti.prev + geom_line(aes(y=prev.max,colour=variable),size=0.25)
  return(g.sti.prev)
}


plot.STIprev_riskgroup <- function(simul,sti.name)
{
  idx = which(names(simul)==sti.name)
  simul$prevsti = simul[,idx]/simul$nAlive
  
  simul$ratio_0 <- simul$nRskGrp0/simul$nAlive
  simul$ratio_1 <- simul$nRskGrp1/simul$nAlive
  simul$ratio_2 <- simul$nRskGrp2/simul$nAlive
  
  simul2 = ddply(simul,.variables = c("time"),summarize,
                 prev = mean(prevsti),
                 r0 = mean(ratio_0),
                 r1 = mean(ratio_1),
                 r2 = mean(ratio_2))
  col=c(1,"orange","red")
  g.sti.rg = ggplot(simul2,aes(x=prev))+geom_line(aes(y=r0),colour=col[1])
  g.sti.rg = g.sti.rg + geom_line(aes(y=r1),colour=col[2])
  g.sti.rg = g.sti.rg + geom_line(aes(y=r2),colour=col[3])
  g.sti.rg = g.sti.rg + ggtitle(paste(sti.name, "prevalence and risk group proportion"))
  return(g.sti.rg)
}



plot.coSTI.riskgroup <- function(simul)
{
  ### Prevalence of co-infection within risk group
  ### (just for HIV and Tp)
  
  simul$coSTI_0 <- simul$nHIVTp0/simul$nRskGrp0
  simul$coSTI_1 <- simul$nHIVTp1/simul$nRskGrp1
  simul$coSTI_2 <- simul$nHIVTp2/simul$nRskGrp2
  simul$coSTI_9 <- simul$nHIVTp9/simul$nCSW
  
  simul2 = ddply(simul,.variables = c("time"),summarize,
                 r0 = mean(coSTI_0),
                 r1 = mean(coSTI_1),
                 r2 = mean(coSTI_2),
                 r9 = mean(coSTI_9))
  
  col=c("lightgrey","pink","red","darkred")
  g.sti.rg = ggplot(simul2,aes(x=time))+geom_line(aes(y=r0),colour=col[1],size=2)
  g.sti.rg = g.sti.rg + geom_line(aes(y=r1),colour=col[2],size=2)
  g.sti.rg = g.sti.rg + geom_line(aes(y=r2),colour=col[3],size=2)
  g.sti.rg = g.sti.rg + geom_line(aes(y=r9),colour=col[4],size=2)
  g.sti.rg = g.sti.rg + ggtitle(paste("Co-infection (HIV+Tp) prevalence and risk group"))
  return(g.sti.rg)
}


plot.STIprev_CSW <- function(simul,sti.name)
{
  idx = which(names(simul)==sti.name)
  simul$prevsti = simul[,idx]/simul$nAlive
  simul$pcsw <- simul$nCSW/simul$nAlive
  
  g.sti.csw.allmc = ggplot(simul,aes(x=prevsti))+geom_point(aes(y=pcsw,
                                                                colour=factor(iMC)),
                                                            size=2)
  g.sti.csw.allmc = g.sti.csw.allmc + ggtitle(paste(sti.name, "prevalence and CSW proportion\n All MC trials"))
  g.sti.csw.allmc = g.sti.csw.allmc + geom_smooth(aes(x=prevsti,y=pcsw,
                                                      colour=factor(iMC),
                                                      fill=factor(iMC)),
                                                  method="lm",
                                                  alpha=0.2)
  
  simul2 = ddply(simul,.variables = c("time"),summarize,
                 prev = mean(prevsti),
                 cswprop = mean(pcsw))
  
  g.sti.csw = ggplot(simul2,aes(x=prev))+geom_point(aes(y=cswprop),size=2)
  g.sti.csw = g.sti.csw + geom_line(aes(y=cswprop))
  g.sti.csw = g.sti.csw + ggtitle(paste(sti.name, "prevalence and CSW proportion"))
  
  g.sti.csw.allmc = ggplot(simul,aes(x=prevsti))+geom_point(aes(y=pcsw,colour=factor(iMC)),size=2)
  
  dfstart = data.frame(prev=simul2$prev[simul2$time==0],
                       pcsw=simul2$cswprop[simul2$time==0])
  
  g.sti.csw = g.sti.csw + geom_point(data=dfstart,aes(prev,pcsw),colour="red",size=6)
  return(list(g.sti.csw.allmc,g.sti.csw))
  
}


plot.HIVprev.risk.time <- function(simul,doCSW=FALSE)
{
  simul2 <- ddply(simul,c("time"), summarize, 
                  m.r0=mean(HIVprevRisk0),
                  sd.r0=sd(HIVprevRisk0),
                  m.r1=mean(HIVprevRisk1),
                  sd.r1=sd(HIVprevRisk1),
                  m.r2=mean(HIVprevRisk2),
                  sd.r2=sd(HIVprevRisk2),
                  m.r9=mean(HIVprevRisk9),
                  sd.r9=sd(HIVprevRisk9))
  
  linesize=3
  
  g = ggplot(simul2,aes(x=time)) + ggtitle("HIV mean prevalence by risk group")+ylab("Prevalence")
  g = g + geom_line(aes(y=m.r0),colour="lightgrey",size=linesize)
  g = g + geom_ribbon(aes(ymin=m.r0-sd.r0,ymax=m.r0+sd.r0),fill="lightgrey",size=linesize, alpha=0.1)
  g = g + geom_line(aes(y=m.r1),colour="pink",size=linesize)
  g = g + geom_ribbon(aes(ymin=m.r1-sd.r1,ymax=m.r1+sd.r1),fill="pink",size=linesize, alpha=0.1)
  g = g + geom_line(aes(y=m.r2),colour="red",size=linesize)
  g = g + geom_ribbon(aes(ymin=m.r2-sd.r2,ymax=m.r2+sd.r2),fill="red",size=linesize, alpha=0.1)
  g = g + geom_line(aes(y=m.r9),colour="darkred",size=linesize)
  g = g + geom_ribbon(aes(ymin=m.r9-sd.r9,ymax=m.r9+sd.r9),fill="darkred",size=linesize, alpha=0.1)
  g = g + coord_cartesian(ylim=c(0,1))
  g
  return(g)
}

plot.TPprev.risk.time <- function(simul,doCSW=FALSE)
{
  simul2 <- ddply(simul,c("time"), summarize, 
                  m.r0=mean(TpprevRisk0),
                  sd.r0=sd(TpprevRisk0),
                  m.r1=mean(TpprevRisk1),
                  sd.r1=sd(TpprevRisk1),
                  m.r2=mean(TpprevRisk2),
                  sd.r2=sd(TpprevRisk2),
                  m.r9=mean(TpprevRisk9),
                  sd.r9=sd(TpprevRisk9))
  
  linesize=3
  
  g = ggplot(simul2,aes(x=time)) + ggtitle("Tp mean prevalence by risk group")+ylab("Prevalence")
  g = g + geom_line(aes(y=m.r0),colour="lightgrey",size=linesize)
  g = g + geom_ribbon(aes(ymin=m.r0-sd.r0,ymax=m.r0+sd.r0),fill="lightgrey",size=linesize, alpha=0.1)
  g = g + geom_line(aes(y=m.r1),colour="pink",size=linesize)
  g = g + geom_ribbon(aes(ymin=m.r1-sd.r1,ymax=m.r1+sd.r1),fill="pink",size=linesize, alpha=0.1)
  g = g + geom_line(aes(y=m.r2),colour="red",size=linesize)
  g = g + geom_ribbon(aes(ymin=m.r2-sd.r2,ymax=m.r2+sd.r2),fill="red",size=linesize, alpha=0.1)
  g = g + geom_line(aes(y=m.r9),colour="darkred",size=linesize)
  g = g + geom_ribbon(aes(ymin=m.r9-sd.r9,ymax=m.r9+sd.r9),fill="darkred",size=linesize, alpha=0.1)
  g = g + coord_cartesian(ylim=c(0,1))
  return(g)
}


plot.Reff <- function(simul){
  
  #### Effective Reproductive Number ####
  
  df <- ddply(simul,c("time"), summarize,
              R.HIV=mean(Reff_HIV,na.rm=T),
              R.HIV.min = min(Reff_HIV,na.rm=T),
              R.HIV.max = max(Reff_HIV,na.rm=T),
              R.HIV.q25 = quantile(Reff_HIV,probs = 0.25,na.rm=T),
              R.HIV.q75 = quantile(Reff_HIV,probs = 0.75,na.rm=T),
              R.Tp=mean(Reff_Tp,na.rm=T),
              R.Tp.min=min(Reff_Tp,na.rm=T),
              R.Tp.max=max(Reff_Tp,na.rm=T),
              R.Tp.q25 = quantile(Reff_Tp,probs = 0.25,na.rm=T),
              R.Tp.q75 = quantile(Reff_Tp,probs = 0.75,na.rm=T))
  
  g = ggplot(df)+geom_line(aes(x=time,y=R.HIV),size=2,colour="black")
  g = g + geom_line(aes(x=time,y=R.Tp),size=2,colour="blue")
  g = g + geom_line(aes(x=time,y=R.HIV.min),size=1,colour="lightgrey")
  g = g + geom_line(aes(x=time,y=R.HIV.max),size=1,colour="lightgrey")
  g = g + geom_line(aes(x=time,y=R.Tp.min),size=1,colour="lightblue")
  g = g + geom_line(aes(x=time,y=R.Tp.max),size=1,colour="lightblue")
  
  g = g + geom_text(aes(x=time[length(time)],y=R.Tp[[length(time)]]+0.1),label="Tp",colour="blue")
  g = g + geom_text(aes(x=time[length(time)],y=R.HIV[[length(time)]]+0.1),label="HIV")
  g = g + geom_ribbon(aes(x=time, ymin=R.HIV.q25, ymax=R.HIV.q75),alpha=0.1)
  g = g + geom_ribbon(aes(x=time, ymin=R.Tp.q25, ymax=R.Tp.q75),alpha=0.1,fill="blue")
  g = g + ggtitle("Effective reproductive number (cum. sec. cases)")+ylab("")
  g = g + geom_hline(yintercept=1,colour="orange")
  g
  return(g)
}


add.STI.flag <-  function(last.pop){
  last.pop$HIVpos <- last.pop$HIVduration>0
  last.pop$Tppos <- last.pop$Tpduration>0
  return(last.pop)
}

plot.STI.lftprtn <- function(last.pop){
  
  z = melt(last.pop,measure.vars = c("HIVpos","Tppos"))
  spline.order = 5
  g = ggplot(z)
  g = g + geom_smooth(aes(x=nLifetimeSexPartner+1,# <-- need '+1' for log scale plot...
                          y=as.numeric(value),
                          colour=variable),
                      method="glm", family="binomial", 
                      formula=y~ns(x, spline.order),
                      alpha=0.1,size=2)
  g = g +scale_x_log10()+annotation_logticks(sides="bt")
  g = g +scale_color_brewer(palette="Set1")
  g = g +ggtitle("Proba STI+ and\n lifetime number of partners(+1)")+xlab("# lifetime partners")+ylab("")
  return(g)
}

plot.STI.age <- function(last.pop){
  
  z = melt(last.pop,measure.vars = c("HIVpos","Tppos"))
  z$agegroup <- round(z$age/3)*3
  spline.order = 5
  g = ggplot(z)
  g = g + geom_smooth(aes(x=agegroup,
                          y=as.numeric(value),
                          colour=variable),
                      method="glm", family="binomial", 
                      formula=y~ns(x, spline.order),
                      alpha=0.1,size=2)
  g = g +scale_color_brewer(palette="Set1")
  g = g +ggtitle("Proba STI+ and Age")+xlab("Age Group")+ylab("")
  return(g)
}


plot.symptomatic<-function(last.pop){
  ### Plot symptomatic infections (for HIV and syphilis only)

  zz = ddply(last.pop,c("gender","STIinfection"), summarize,
             #n = sum(),
             p.tp = mean(Tpsymptom),
             p.hiv = mean(HIVsymptom))
  zz = subset(zz, STIinfection!="none")
  zz <- melt(zz,measure.vars = c("p.tp","p.hiv"))
  
  g = ggplot(zz)+geom_bar(aes(x=STIinfection,y=value,fill=factor(gender)),
                          position="dodge",
                          stat="identity")
  g = g + ggtitle("Symptomatic proportion")+xlab("STI")+ylab("Proportion")
  return(g)
}
