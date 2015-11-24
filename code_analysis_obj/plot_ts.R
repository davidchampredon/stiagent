#
#   FUNCTIONS TO PLOT SIMULATION's TIME SERIES
#

source("utils.R")
library(ggplot2)


draw.intervention <- function(interv.info, g, ymx){
	# Draw vertical lines where intervention
	# start/end for all STIs (relevant for time series only)
	
	for(i in 1:length(interv.info)){
		v = interv.info[[i]]$sched
		v <- v[v<998] # <-- avoid drawing when intervention does not end
		g <- g + geom_vline(xintercept=v, colour="orange",lty=2,size=2)
		g <- g + annotate("text", 
						  x = v*0.96, 
						  y = rep(ymx,length(v)), 
						  label = interv.info[[i]]$sti,
						  col="orange")
	}
	return(g)
}


plot.timeseries <- function(sim,
							varname,
							title,
							do.summary=TRUE,
							qLo=0.1,qHi=0.9,
							interv.info = NULL){
	
	DF.all <- get.timeseries(sim)
	DF <- DF.all[,c("time","mc",varname)]
	
	if(do.summary){
		DF.summ <- ddply(DF,c("time"), 
						 function(x,ind){c(m=mean(x[,ind]),
						 				  qLo=unname(quantile(x[,ind],probs=qLo)),
						 				  qHi=unname(quantile(x[,ind],probs=qHi))
						 )}, 
						 varname)
		
		g <- ggplot(DF.summ)+geom_line(aes(x=time,y=m),size=2)
		g <- g + geom_ribbon(aes(x=time,ymin=qLo, ymax=qHi), alpha=0.1)
		
		g <- g + ggtitle(title) + ylab("")
	}	
	
	if(!do.summary){
		g <- ggplot(DF)+geom_line(aes(x=time),aes_string(y=varname),colour=factor(mc))
		g <- g + ggtitle(title) + ylab("")
	}
	
	# interventions
	ymx <- max(DF[,varname])
	if(!is.null(interv.info)) g <- draw.intervention(interv.info,g,ymx)

	return(g)
}




plot.timeseries.aggreg <- function(sim,
								   period,
								   varname,
								   title,
								   do.summary=TRUE,
								   qlo=0.1, qhi=0.9){
	### PLOT TIME SERIES
	### AGGREGATING DATA ACCORDING TO 
	### A SPECIFIED PERIOD
	
	stopifnot(period %in% c("year","month"))
	
	DF.all <- get.timeseries(sim)
	DF0 <- DF.all[,c(period,"mc",varname)]
	
	DF <- ddply(DF0,c(period,"mc"),function(x,ind){c(sv=sum(x[,ind]))},varname)
	
	if(do.summary){
		DF.summ <- ddply(DF,period,summarize,
						 m=mean(sv),
						 qLo=(quantile(sv,probs=qlo)),
						 qHi=(quantile(sv,probs=qhi)))
		
		names(DF.summ)[grepl(period,names(DF.summ))] <- "x"
		g <- ggplot(DF.summ)+geom_step(aes(x=x,y=m),size=2)
		g <- g + geom_ribbon(aes(x=x,ymin=qLo, ymax=qHi), alpha=0.1)
		g <- g + ggtitle(title) + ylab("")+xlab(period)
	}	
	
	if(!do.summary){
		names(DF)[grepl(period,names(DF))] <- "x"
		g <- ggplot(DF)+geom_line(aes(x=x,y=sv,colour=factor(mc)))
		g <- g + ggtitle(title) + ylab("")+xlab(period)
	}
	return(g)
}


plot.incidence <- function(sim,period, stiname, 
						   type, 
						   title,
						   do.summary=TRUE,
						   qLo=0.1, qHi=0.9,
						   interv.info=NULL){
	
	stopifnot(type %in% c("cases","rate"))
	
	z <- calc.incidence.rate(sim,period,stiname)
	
	idx.rate <- which(grepl("incrate.", names(z)))
	idx.cases <- which(grepl(paste0("inc.",period), names(z)))
	
	if(type=="cases") z$y <- z[,idx.cases]
	if(type=="rate") z$y <- z[,idx.rate]
	
	z$t <- z[,period]
	
	zz <- ddply(z,"t",summarize,
				m=mean(y),
				qLo = quantile(y,probs=qLo),
				qHi = quantile(y,probs=qHi)
	)
	
	if(do.summary){
		g <- ggplot(zz)+geom_step(aes(x=t, y=m),size=2)
		g <- g + geom_ribbon(aes(x=t,ymin=qLo,ymax=qHi),alpha=0.1)
	}
	
	if(!do.summary){
		g <- ggplot(z)+geom_step(aes(x=t, y=y,colour=factor(mc)))
	}
	
	if(!is.null(interv.info)) g <- draw.intervention(interv.info,g,ymx=max(z$y)*0.9)
	
	g <- g + ggtitle(title)+xlab(period)+ylab("")
	return(g)
}


plot.proportion.timeseries <- function(sim,varname,
									   title, palette="Paired",
									   interv.info=NULL){
	
	DF.all <- get.timeseries(sim)
	DF0 <- DF.all[,c("time","mc",varname)]
	
	DF <- ddply(DF0, "time", function(x,ind){apply(x[,ind],MARGIN = 2, FUN = mean)},varname )
	
	DF2 <- gather(DF,key=time)
	names(DF2)[2]<-"var"  # <- change that
	
	g <- ggplot(DF2)+geom_bar(aes(x=time,y=value,fill=var,colour=var),
							  position="fill",
							  stat = "identity")
	g <- g + scale_fill_brewer(palette = palette)
	g <- g + scale_colour_brewer(palette = palette) + ylab("Proportion")
	g <- g + ggtitle(title)
	if(!is.null(interv.info)) g <- draw.intervention(interv.info,g,ymx = 0.9) # <-- yscale in [0;1]
	return(g)
}


plot.prev.risk <- function(sim, stiname,interv.info=NULL){
	### PLOT PREVALENCE BY RISK GROUPS
	###
	DF.all <- get.timeseries(sim)
	DF.all$time2 <- round(DF.all$time)
	z <- paste0(stiname,"prevRisk",c(0,1,2,9))
	DF0 <- DF.all[,c("time2","mc",z)]
	
	DF <- ddply(DF0,c("time2"),function(x,ind){apply(x[,ind],MARGIN = 2, FUN = mean)},z )
	DF2 <- gather(DF,key=time2)
	names(DF2)[2]<-"var"  # <- change that
	
	g <- ggplot(DF2)+geom_line(aes(x=time2,y=value,colour=var),size=2)
	g <- g + scale_colour_brewer(palette = "Reds")
	g <- g + ggtitle(paste(stiname,"prevalence by risk group"))+xlab("")+ylab("")
	
	if(!is.null(interv.info)) g <- draw.intervention(interv.info,g,ymx = 0.9*max(DF2$value)) 
	
	return(g)
}



