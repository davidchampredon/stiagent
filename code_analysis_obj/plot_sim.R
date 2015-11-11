#####
#####   FUNCTIONS TO PLOT SIMULATIONS
#####

library(ggplot2)
library(dplyr)
library(plyr)
library(tidyr)


get.timeseries <- function(sim){
	### RETRIEVE ALL TIME SERIES (FOR EVERY MC ITER)
	### IN A DATA FRAME FORMAT
	
	stinames <- sim[[1]]$STInames
	n.sti <- length(stinames)
	n.mc <- (length(sim)-1)
	
	D <- list()
	for(i in 1:n.mc){
		
		# from list to data frame:
		D[[i]]<- as.data.frame(sim[[i]]$df_sim)
		D[[i]]$mc <- i
		
		# add usefull transformed variables:
		D[[i]]$month <- ceiling(D[[i]]$time*12)
		D[[i]]$year <- ceiling(D[[i]]$time)
		D[[i]]$fem.ratio <- D[[i]]$nFemale/D[[i]]$nAlive
		D[[i]]$partn.ratio <- D[[i]]$nPartn/D[[i]]$nAlive
		D[[i]]$sp.ratio <- D[[i]]$nSp/D[[i]]$nAlive
		D[[i]]$csw.prop <- D[[i]]$nSp/D[[i]]$nAlive
		
		# STIs specifics:
		for(k in 1:n.sti) {
			# prevalence (percentage)
			D[[i]]$tmp <- D[[i]][,stinames[k]]/D[[i]]$nAlive
			names(D[[i]])[length(names(D[[i]]))] <- paste0("prev",stinames[k])
			
			# incidence:
			D[[i]]$tmp <- c(0,pmax(0,diff(D[[i]][,stinames[k]])))
			names(D[[i]])[length(names(D[[i]]))] <- paste0("inc",stinames[k])
		}
	}	
	return(dplyr::rbind_all(D))
}


calc.incidence.rate <- function(sim,period,stiname){
	### CALCULATE INCIDENT CASES AND RATE FOR A GIVEN PERIOD
	
	DF.all <- get.timeseries(sim)
	DF <- DF.all[,c("time",period,"mc",stiname,"nAlive")]
	
	z = unlist(c(DF[,stiname]))
	DF$inc <- c(0,pmax(0,diff(z)))
	# Manage the transition b/w 2 MC iterations
	# (the 'diff' did not managa that):
	DF$inc[DF$time==0] <- 0
	
	DF.summ <- ddply(DF,c(period,"mc"),summarize, 
					 sinc = sum(inc), 
					 avgpop = mean(nAlive))
	
	DF.summ$incrate <- DF.summ$sinc/DF.summ$avgpop
	
	names(DF.summ)[names(DF.summ)=="sinc"] <- paste("inc",period,stiname,sep=".")
	names(DF.summ)[names(DF.summ)=="incrate"] <- paste("incrate",period,stiname,sep=".")
	
	return(DF.summ)
}




plot.timeseries <- function(sim,
							varname,
							title,
							do.summary=TRUE,
							qLo=0.1,qHi=0.9){
	
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
	return(g)
}




plot.timeseries.aggreg <- function(sim,
								   aggreg.name,
								   varname,
								   title,
								   do.summary=TRUE,
								   qLo=0.1,qHi=0.9){
	### PLOT TIME SERIES
	### AGGREGATING DATA ACCORDING TO 
	### A SPECIFIED FREQUNCY
	
	stopifnot(aggreg.name %in% c("year","month"))
	
	DF.all <- get.timeseries(sim)
	DF0 <- DF.all[,c(aggreg.name,"mc",varname)]
	
	DF <- ddply(DF0,c(aggreg.name,"mc"),function(x,ind){c(sv=sum(x[,ind]))},varname)
	
	if(do.summary){
		DF.summ <- ddply(DF,aggreg.name,summarize,
						 m=mean(sv),
						 qLo=unname(quantile(sv,probs=qLo)),
						 qHi=unname(quantile(sv,probs=qHi)))
		
		names(DF.summ)[grepl(aggreg.name,names(DF.summ))] <- "x"
		g <- ggplot(DF.summ)+geom_step(aes(x=x,y=m),size=2)
		g <- g + geom_ribbon(aes(x=x,ymin=qLo, ymax=qHi), alpha=0.1)
		g <- g + ggtitle(title) + ylab("")+xlab(aggreg.name)
	}	
	
	if(!do.summary){
		names(DF)[grepl(aggreg.name,names(DF))] <- "x"
		g <- ggplot(DF)+geom_line(aes(x=x,y=sv,colour=factor(mc)))
		g <- g + ggtitle(title) + ylab("")+xlab(aggreg.name)
	}
	return(g)
}


plot.incidence <- function(sim,period, stiname, 
						   type, 
						   title,
						   do.summary=TRUE,
						   qLo=0.1, qHi=0.9,
						   interv.date=NULL){
	
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
	
	if(!is.null(interv.date)){
		g <- g + geom_vline(xintercept=interv.date,colour="orange",size=2,linetype=2)
	}
	
	g <- g + ggtitle(title)+xlab(period)+ylab("")
	return(g)
}


plot.proportion.timeseries <- function(sim,varname,
									   title, palette="Paired"){
	
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
	return(g)
}





