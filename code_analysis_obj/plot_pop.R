###
###   PLOT POPULATION FEATURES
###

library(splines)
source("utils.R")

calc.density.summ <- function(sim,
							  outcome, 
							  categ,
							  excl.zero = FALSE,
							  qlo = 0.1,
							  qhi = 0.9){
	
	### CALCULATE THE MEAN DENSITY OF AN OUTCOME
	### ACROSS ALL MC (summarize), SPLIT BY CATEGORIES
	
	P.all <- get.population(sim)
	nMC <- get.nMC(sim)
	
	if(excl.zero) P.all <- P.all[P.all[,outcome]>0,]
	
	D <- list()
	k=1
	for(i in 1:nMC){
		P <- P.all[P.all$mc==i,c(outcome,categ)]   
		
		u.categ <- unique(unlist(P[,categ]))
		
		for(g in 1:length(u.categ)){
			Pg <- P[P[,categ]==u.categ[g],]
			z <- unlist(Pg[,outcome])
			h <- hist(x=z, plot = F,breaks = seq(0,max(z)+1,by=1))
			D[[k]] <- data.frame(outcome=h$breaks, dens=c(h$density,0), mc=i, categ=u.categ[g])
			#names(D[[k]])[names(D[[k]])=="categ"] <- categ
			k <- k+1
		}
	}
	
	D.all <- dplyr::rbind_all(D)
	
	D.summ <- ddply(D.all,c("outcome","categ"),summarize,
					m=mean(dens),
					qLo = quantile(dens,probs=qlo),
					qHi = quantile(dens,probs=qhi))
	return(D.summ)
}


plot.density.summ <- function(sim,outcome,categ,title,
							  plotline=FALSE,
							  excl.zero=FALSE){
	
	theme_set(theme_bw())
	
	A <- calc.density.summ(sim,outcome,categ,excl.zero)
	
	if(!plotline){
		g <- ggplot(A)+geom_line(aes(x=outcome,
									 y=m,
									 colour=factor(categ)))
		g <- g + geom_pointrange(aes(x=outcome+categ/15,
									 y=m,
									 ymin=qLo,
									 ymax=qHi,
									 colour=factor(categ)),
								 size=1)
	}
	
	if(plotline){
		g <-ggplot(A) + geom_line(aes(x=outcome+categ/15,
									  y=m,
									  colour=factor(categ)),
								  size=1)
		g <- g + geom_ribbon(aes(x=outcome+categ/15,
								 ymin=qLo,
								 ymax=qHi,
								 fill=factor(categ)),
							 alpha=0.1)
	}
	g <- g + ggtitle(title) + xlab(outcome)+ylab("")
	return(g)
}



plot.lifePtrn <- function(sim){
	P.all <- get.population(sim)
	P.all <- subset(P.all,riskgroup<9)
	g <- ggplot(P.all)+geom_boxplot(aes(x=factor(riskgroup),
										y=nLifetimePartner,
										colour=factor(gender),
										fill=factor(gender)),alpha=0.5)
	g <- g + scale_y_log10() + annotation_logticks(sides="lr")
	return(g)
}

plot.prtn.distrib <- function(sim){
	grid.arrange(
		plot.density.summ(sim,outcome="nCurrPartn",categ="gender",title="Concurrent partnerships by gender",excl.zero=T),
		plot.density.summ(sim,outcome="nCurrPartn",categ="riskgroup",title="Concurrent partnerships by riskgroup",excl.zero=T)
	)
}

plot.prtn.distrib.age <- function(sim){
	P.all <- get.population(sim)
	P.all <- subset(P.all,nCurrPartn>0)
	P.all$age2 <- round(P.all$age)
	
	P.summ <- ddply(P.all,c("age2","riskgroup","Gender"),summarize, m=mean(nCurrPartn))
	g <- ggplot(P.summ) + geom_line(aes(x=age2,y=m,colour=factor(riskgroup)),size=2)+facet_wrap(~Gender)
	g <- g + scale_color_brewer(palette = "Reds")
	g1<-g
	
	P.summ.life <- ddply(P.all,c("age2","riskgroup","Gender"),summarize, m=mean(nLifetimePartner))
	g <- ggplot(P.summ.life) + geom_line(aes(x=age2,y=m,colour=factor(riskgroup)),size=2)+facet_wrap(~Gender)
	g <- g + scale_color_brewer(palette = "Reds")
	g <- g + scale_y_log10() + annotation_logticks(sides="lr")
	g2 <- g
	grid.arrange(g1,g2)
}


plot.curr.life.prtn <- function(sim){
	P.all <- get.population(sim)
	P.all <- subset(P.all,riskgroup<9)
	g <- ggplot(P.all,aes(x=nCurrPartn,
						  y=nLifetimePartner,
						  col=factor(Gender)))
	g <- g + geom_smooth(method="lm", formula=y~poly(x, degree=5),size=3,alpha=0.1)
	# try(expr = g <- g + scale_y_log10()+annotation_logticks(sides="lr"),silent = T )
	g <- g + ggtitle("Current vs Lifetime number of partners")
	g <- g + scale_x_continuous(breaks=c(1:10),labels=c(1:10))
	return(g)
}


plot.durPrtn.rskgrp <- function(sim){
	grid.arrange(
		plot.density.summ(sim,outcome="durPrtn1",categ="riskgroup",title="Duration partnership #1",excl.zero=T,plotline=T),
		plot.density.summ(sim,outcome="durPrtn2",categ="riskgroup",title="Duration partnership #2",excl.zero=T,plotline=T),
		plot.density.summ(sim,outcome="durPrtn3",categ="riskgroup",title="Duration partnership #3",excl.zero=T,plotline=T),
		plot.density.summ(sim,outcome="durPrtn4",categ="riskgroup",title="Duration partnership #4",excl.zero=T,plotline=T)
	)
}


plot.everVisitCSW <- function(sim){
	
	P.all <- get.population(sim)
	P.all <- subset(P.all,gender==1)
	age.bucket <- 3
	P.all$ageGroup = round(P.all$age/age.bucket)*age.bucket
	
	x <- ddply(P.all,c("riskgroup", "ageGroup"),summarize,
			   mvcsw=mean(everVisitCSW),
			   mvcsw.min=quantile(everVisitCSW,probs = 0.05),
			   mvcsw.max=quantile(everVisitCSW,probs = 0.95),
			   n = length(everVisitCSW))
	
	g.age <- ggplot(x)+geom_line(aes(x=ageGroup,y=mvcsw, colour=factor(riskgroup)),size=2)
	g.age <- g.age + scale_colour_brewer(palette = "Reds")
	g.age <- g.age+geom_ribbon(aes(x=ageGroup,ymin=mvcsw.min,ymax=mvcsw.max,
								   fill=factor(riskgroup)),alpha=0.05)
	g.age <- g.age + ggtitle("Proportion ever visited CSW")+xlab("Age")+ylab("Proportion")
	return(g.age)
}



plot.stipos.age <- function(sim,stiname,excl.csw=TRUE){
	
	P.all <- get.population(sim)
	if(excl.csw) P.all <- subset(P.all,riskgroup<9)
	
	P.all$stipos <- 0
	z <- P.all[,paste0(stiname,"duration")]
	P.all$stipos[z>0] <- 1
	
	spline.order <- 5
	g1 <- ggplot(P.all)+ geom_smooth(aes(x=age,y=stipos,colour=Gender),
									 method="glm", 
									 family="binomial",
									 formula=y~ns(x, spline.order),
									 alpha=0.1, size=2)
	g2 <- ggplot(P.all)+ geom_smooth(aes(x=age,y=stipos,colour=factor(riskgroup)),
									 method="glm", 
									 family="binomial",
									 formula=y~ns(x, spline.order),
									 alpha=0.1, size=2)
	g2 <- g2 + scale_color_brewer(palette = "Reds")
	
	g1 <- g1 + ggtitle(paste("Proba",stiname," positive v.s. age"))
	g2 <- g2 + ggtitle(paste("Proba",stiname," positive v.s. risk group"))
	
	grid.arrange(g1,g2,ncol=2)
}


plot.stipos.prtn <- function(sim,stiname,excl.csw=TRUE){
	
	P.all <- get.population(sim)
	if(excl.csw) P.all <- subset(P.all,riskgroup<9)
	
	P.all$stipos <- 0
	z <- P.all[,paste0(stiname,"duration")]
	P.all$stipos[z>0] <- 1
	
	spline.order <- 3
	g1 <- ggplot(P.all)+ geom_smooth(aes(x=1+nLifetimePartner,y=stipos,colour=Gender),
									 method="glm", 
									 family="binomial",
									 formula=y~ns(x, spline.order),
									 alpha=0.1, size=2)
	
	g1 <- g1 + scale_x_log10(limits=c(1,100))  + annotation_logticks(sides="b")
	g1 <- g1 + ggtitle(paste("Proba",stiname," positive v.s. lifetime number partners"))
	return(g1)
}



