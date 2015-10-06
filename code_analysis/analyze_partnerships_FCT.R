library(reshape2)
library(splines)


last.cdate.output <- function(filerootname){
	fnames <- system(paste0("ls ",folder.out,filerootname,n.ct,"*"),intern=T)
	
	### Retrieve output at last calibration date
	for(i in 1:length(fnames)){
		print(paste("reading",fnames[i]))
		tmp <- read.csv(fnames[i],
						header = F)
		tmp$mc <- i
		if(i==1) x <- tmp
		if(i>1) x <- rbind(x,tmp)
	}
	return(x)
}



plot.hist.mc <- function(x,title=""){
  ### Plot histogram for several MC iterations
  
  x <- ddply(x,c("n"),summarize, 
             freq.m=mean(freq),
             freq.min=min(freq),
             freq.max=max(freq))
  
  # mean lifetime number of partners:
  a <- sum(x$freq.m*x$n)
  
  g.x <- ggplot(x)+geom_pointrange(aes(x=n,y=freq.m,
                                       ymin=freq.min,
                                       ymax=freq.max),
                                   size=1)
  g.x <- g.x + geom_line(aes(x=n,y=freq.m))
  g.x <- g.x + ggtitle(title)
  g.x <- g.x + geom_vline(xintercept=a,linetype=2,size=3) + geom_text(x=a+0.2,aes(y=max(freq.m)),
                                                                      label=round(a,digit=2))
  return(g.x)
}

merge.last.pop <- function(last.pop.fname){
  last.pop = data.frame()
  for(i in 1:length(last.pop.fname)){
    tmp <- read.csv(last.pop.fname[i],header = T)
    tmp$iMC <- i
    if(i==1) last.pop = tmp
    if(i>1) last.pop = rbind(last.pop,tmp)
  }
  return(last.pop)
}

calc.age.group <- function(x,age.bucket){
  x$ageGroup <- round(x$age/age.bucket)*age.bucket
  return(x)
}


plot.everVisitCSW <- function(last.pop){

  x <- subset(last.pop,subset=(isAlive==1 & gender==1))
  age.bucket <- 1
  x$ageGroup = round(x$age/age.bucket)*age.bucket
  
  x <- ddply(x,c("riskGroup", "ageGroup"),summarize,
  		   mvcsw=mean(everVisitCSW),
  		   mvcsw.min=quantile(everVisitCSW,probs = 0.05),
  		   mvcsw.max=quantile(everVisitCSW,probs = 0.95),
  		   n = length(everVisitCSW))

  g.age <- ggplot(x)+geom_line(aes(x=ageGroup,y=mvcsw, colour=factor(riskGroup)),size=2)
  g.age <- g.age+geom_ribbon(aes(x=ageGroup,ymin=mvcsw.min,ymax=mvcsw.max,
                                 fill=factor(riskGroup)),alpha=0.05)
  g.age <- g.age + ggtitle("Proportion ever visited CSW")+xlab("Age")+ylab("Proportion")
  g.age <- g.age + scale_colour_brewer(palette = "Reds")
  return(g.age)
}


calc.partnership.status <- function(last.pop){
  last.pop <- last.pop[last.pop$isAlive==1,]
  last.pop$hasSpouseOnly <- (last.pop$nCurrSpouse == last.pop$nCurrSexPartner & last.pop$nCurrSpouse>0)
  last.pop$hasSpouseAndCasual <- (last.pop$nCurrSpouse < last.pop$nCurrSexPartner & last.pop$nCurrSpouse>0)
  last.pop$hasCasualOnly <- (last.pop$nCurrSpouse==0 & last.pop$nCurrSexPartner>0)
  last.pop$partnershipStatus <- rep("Single",nrow(last.pop))
  last.pop$partnershipStatus[last.pop$hasSpouseOnly]<-("Spouse only")
  last.pop$partnershipStatus[last.pop$hasSpouseAndCasual]<-("Spouse and casual")
  last.pop$partnershipStatus[last.pop$hasCasualOnly]<-("Casual only")
  last.pop$partnershipStatus <- as.factor(last.pop$partnershipStatus)
  return(last.pop)
}


plot.partnership.status <- function(last.pop){
  leg.col <- c("orange","gray","blue","lightblue")
  g <- ggplot(last.pop)+geom_bar(aes(fill=(partnershipStatus),x=factor(1)), width=1)+ coord_polar(theta = "y")
  g <- g +xlab("")+ylab("")+ggtitle("Partnership Status (all)")
  g <- g + scale_fill_manual(values=leg.col,
                             name="Partnership status",
                             label=c("Casual","Single","Spouse and casual","Spouse only"))
  
  g.rsk <- ggplot(last.pop)+geom_bar(aes(x=factor(riskGroup),
                                         fill=partnershipStatus),
                                     position="fill")
  g.rsk <- g.rsk +xlab("")+ylab("")+ggtitle("Partnership Status by Risk Group")
  g.rsk <- g.rsk+ scale_fill_manual(values=leg.col,
                                    name="Partnership status",
                                    label=c("Casual","Single","Spouse and casual","Spouse only"))
  return(list(g,g.rsk))
}


plot.partnership.duration <- function(last.pop){
  pop.melt.pDur <- melt(last.pop,
                        #variable.name="durationType",
                        measure.vars=c("pDuration0","pDuration1","pDuration2","pDuration3"))
  
  g.partnDur = ggplot(pop.melt.pDur,aes(x=value,y=..density..))
  g.partnDur = g.partnDur + geom_histogram(binwidth=2,fill='darkgray')
  g.partnDur = g.partnDur + xlab("Years")+ylab("")+facet_wrap(~variable,scales="free_y")
  g.partnDur = g.partnDur + ggtitle("Partnerships durations distribution \n (by partner order)")
  return(g.partnDur)
}


plot.currnt.age <- function(last.pop){
  
  last.pop <- last.pop[last.pop$riskGroup<9,]
  x <- ddply(last.pop,c("ageGroup","riskGroup","gender"),summarize,m=mean(nCurrSexPartner))
  
  g <- ggplot(x)+geom_line(aes(x=ageGroup,y=m,colour=factor(riskGroup)),size=2)
  g <- g + scale_colour_brewer(palette="Reds")
  g <- g + ggtitle("Mean Current Number of Partners by Age")
  g <- g + facet_wrap(~gender)
  g
}


plot.partnership.concur <- function(last.pop, include.all=TRUE){
  ### Concurrent Partners (Degree) distribution
  
  # remove CSW for this graph
  last.pop <- last.pop[last.pop$riskGroup<9,]
  if(!include.all) last.pop <- last.pop[last.pop$nCurrSexPartner>0,]
  x.f <- round(mean(last.pop$nCurrSexPartner[last.pop$gender==0]),digits = 2)
  x.m <- round(mean(last.pop$nCurrSexPartner[last.pop$gender==1]),digits=2)
  title <- paste0("Concurrent Partners Distribution by gender\n (F=",x.f," ; M=",x.m,")")
  if(!include.all) title <- paste0(title ," - excluding singles")
  g = ggplot(last.pop)
  g = g + geom_histogram(aes(x=factor(nCurrSexPartner),
                             y=..count../sum(..count..),fill=factor(gender)))
  
  g = g + ggtitle(title)+xlab("Number current Sex Partners")+ylab("proportion")
  g = g + facet_wrap(~gender)
  g = g + scale_fill_manual(values=c("pink","lightblue"),
                            name="Gender",label=c("female","male"))
  return(g)
}

plot.partnership.concur.rsk <- function(last.pop, include.all=FALSE){
  ### Concurrent Partners (Degree) distribution by risk group
  
  # remove CSW for this graph
  last.pop <- last.pop[last.pop$riskGroup<9,]
  if(!include.all) last.pop <- last.pop[last.pop$nCurrSexPartner>0,]
  
  title <- paste0("Degree distribution by risk group")
  if(!include.all) title <- paste0(title ,"\n excluding singles")
  
  g = ggplot(last.pop)+scale_fill_brewer(palette="Reds")+scale_colour_brewer(palette="Reds")
  g = g + geom_density(aes(x=nCurrSexPartner,
                           colour=factor(riskGroup) ),
                       size=3,
                       alpha=0.3, adjust=2.3)
  g = g + scale_x_continuous(breaks=1:10, labels=1:10)
  g = g + ggtitle(title)+xlab("Number current Sex Partners")+ylab("proportion")
  return(g)
}


plot.lft.prtn.rsk <- function(last.pop, include.all=FALSE){
  ### Lifetime number partners distribution by risk group
  
  if(!include.all) last.pop <- last.pop[last.pop$nLifetimeSexPartner>0,]
  # remove CSW 
  last.pop.nocsw <- last.pop[last.pop$riskGroup<9,]
  
  title <- paste0("Lifetime Number Sex Partners by risk group")
  if(!include.all) title <- paste0(title ,"\n excluding virgins")
  
  g = ggplot(last.pop.nocsw)+scale_fill_brewer(palette="Reds")+scale_colour_brewer(palette="Reds")
  g = g + geom_density(aes(x=nLifetimeSexPartner,
                           colour=factor(riskGroup),
                           fill=factor(riskGroup)),
                       size=1,
                       alpha=0.5)
  g = g + coord_cartesian(xlim=c(0,30))
  g = g + ggtitle(title)+xlab("Lifetime Number Sex Partners")+ylab("proportion")
  g = g + facet_wrap(~gender)
  
  g2 = ggplot(last.pop.nocsw,aes(x=factor(riskGroup),y=nLifetimeSexPartner))
  g2 = g2 + geom_violin(aes(fill=factor(gender)),col='lightgray',alpha=0.2)
  g2 = g2 + geom_boxplot(aes(fill=factor(gender)),alpha=0.7,width=0.7) 
  g2 = g2 + scale_y_log10() + scale_fill_manual(values=c("pink","lightblue"),
                                                labels=c("Female","Male"),
                                                name="Gender") 
  g2 = g2 + ggtitle("Lifetime number of sex partners")+ xlab("Risk Group")
  g2 = g2+ annotation_logticks(sides="lr")
  
  df <- ddply(last.pop,c("ageGroup","riskGroup","gender"),
              summarize,
              m=mean(nLifetimeSexPartner))
  
  g3 = ggplot(df)+scale_fill_brewer(palette="Reds")+scale_colour_brewer(palette="Reds")
  g3 = g3 + geom_line(aes(x=ageGroup,y=m,
                          colour=factor(riskGroup)),
                      size=1)
  g3 = g3 + ggtitle("Lifetime number of sex partners")+xlab("Age")
  g3 = g3 + facet_wrap(~gender)+ scale_y_log10()+ annotation_logticks(sides="lr")
  g3
  return(list(g,g2,g3))
}


plot.lft.prtn.conc <- function(last.pop){
  last.pop <- last.pop[last.pop$nLifetimeSexPartner>0,]
  last.pop <- last.pop[last.pop$nCurrSexPartner>0,]
  # remove CSW 
  last.pop <- last.pop[last.pop$riskGroup<9,]
  
  g = ggplot(last.pop,aes(x=nCurrSexPartner,
                          y=nLifetimeSexPartner,
                          col=factor(gender)))
  g = g + geom_smooth(method="lm", formula=y~ns(x, 5),size=3,alpha=0.1)
  g = g + scale_y_log10()+annotation_logticks(sides="lr") 
  g = g + scale_color_manual(values=c("pink","lightblue"),
                             labels=c("Female","Male"),
                             name="Gender")
  g = g + ggtitle("Current vs Lifetime number of partners")
  g = g + scale_x_continuous(breaks=c(1:10),labels=c(1:10))
  g
}


plot.spouse <- function(last.pop){
  
  last.pop <- calc.age.group(last.pop,10)
  x = last.pop[last.pop$gender==1 & last.pop$nCurrSpouse>0,]
  
  df = ddply(x,c("riskGroup","ageGroup"), summarize, m=mean(nCurrSpouse))
  
  g = ggplot(df) + geom_line(aes(x=ageGroup, y=m, colour=factor(riskGroup)),size=2)
  g = g + ggtitle("Mean number of spouse (males)")+ylab("")+xlab("age group")
  g = g + scale_colour_brewer(palette="Reds")
  
  g2 = ggplot(last.pop)+geom_histogram(aes(x=factor(nLifeTimeSpouse),
                                           fill=factor(gender)),
                                       position="dodge")
  g2 = g2+ scale_fill_manual(values=c("pink","lightblue"),
                             labels=c("Female","Male"),
                             name="Gender")
  g2 = g2+ggtitle("Distribution of lifetime number of spouses")+xlab("Lifetime number of spouses")+ylab("")
  g2
  
  return(list(g,g2))
}


plot.prtn.duration <- function(last.pop){
  
  x <- melt(last.pop,
            #variable.name="durationType",
            measure.vars=c("pDuration0","pDuration1"
                           ,"pDuration2","pDuration3"))
  g = ggplot(x,aes(x=value,y=..density..))
  g = g + geom_histogram(binwidth=2,fill='darkgray')
  g = g + xlab("Years")+ylab("")+facet_wrap(~variable)#,scales="free_y")
  g = g + ggtitle("Partnerships durations distribution \n (by partner order)")
  return(g)
}


unique.pair <- function(x){
  res <- x
  if(x=="10") res <- "01"
  if(x=="21") res <- "12"
  if(x=="20") res <- "02"
  if(grepl(x,pattern = "NA")) res<- NA
  return(res)
}

riskGroup.partner.oneMC <- function(last.pop,iMC){
  
  x <- last.pop[last.pop$iMC==iMC,] 
  
  # Extract UID and risk group only (will be searched)
  uidrg <- x[,c("UID","riskGroup")]
  
  # Extract UIDs of first partnership
  p0 <- as.data.frame(x[,"partnerUID0"])
  names(p0) <- "UID"
  # look up its risk group:
  p0.rg <- join(x = p0, y=uidrg, by="UID")
  names(p0.rg) <- c("partnerUID0","riskGroup.p0")
  # add riskgroup
  x$riskGroup.p0 <- p0.rg$riskGroup.p0
  
  # Do the same with second and third partnership
  p1 <- as.data.frame(x[,"partnerUID1"])
  names(p1) <- "UID"
  p1.rg <- join(x = p1, y=uidrg, by="UID")
  names(p1.rg) <- c("partnerUID1","riskGroup.p1")
  x$riskGroup.p1 <- p1.rg$riskGroup.p1
  
  p2 <- as.data.frame(x[,"partnerUID2"])
  names(p2) <- "UID"
  p2.rg <- join(x = p2, y=uidrg, by="UID")
  names(p2.rg) <- c("partnerUID2","riskGroup.p2")
  x$riskGroup.p2 <- p2.rg$riskGroup.p2
  
  # Identify pair of risk groups
  x$pair0 <- paste0(x$riskGroup,x$riskGroup.p0)
  x$pair0 <- sapply(x$pair0,FUN = unique.pair)
  x$pair1 <- paste0(x$riskGroup,x$riskGroup.p1)
  x$pair1 <- sapply(x$pair1,FUN = unique.pair)
  x$pair2 <- paste0(x$riskGroup,x$riskGroup.p2)
  x$pair2 <- sapply(x$pair2,FUN = unique.pair)
  
  return(x)
}

plot.partnership.pairs <- function(last.pop, n.MC){
  
  x <- riskGroup.partner.oneMC(last.pop,iMC=1)
  for(i in 2:n.MC) x <- rbind(x,riskGroup.partner.oneMC(last.pop,iMC=i))
  # remove NAs
  x <- x[!is.na(x$pair0),]
  
  g <- ggplot(x)+geom_bar(aes(x=pair0))
  g <- g + ggtitle("First Partnership risk group pairs")+xlab("Risk Group Pair")
  
  x <- x[!is.na(x$pair1),]
  g1 <- ggplot(x)+geom_bar(aes(x=pair1))
  g1 <- g1 + ggtitle("Second Partnership risk group pairs")+xlab("Risk Group Pair")
  
  return(list(g,g1))
}

plot.max.nConcPrtn <- function(last.pop){
	x <- subset(last.pop,subset=( riskGroup<9))
	g = ggplot(x)+geom_boxplot(aes(y=nMaxCurrSexPartner,x=factor(riskGroup),fill=factor(riskGroup)))
	g = g +facet_wrap(~gender)+scale_y_log10()+annotation_logticks(sides="lr")
	g = g + ggtitle("Maximum number of concurrent sex partners") + xlab("Risk Group")
	g = g + scale_fill_brewer(palette = "Reds")
	return(g)
}



  
  


