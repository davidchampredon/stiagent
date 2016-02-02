source("plot_ts.R")
source("plot_pop.R")


get.interv.time <- function(folder_inputs,scenario_file){
	
	scen <- as.character(read.csv(paste0(folder_inputs,scenario_file),header = F)[,1])
	scen <- paste0(folder_inputs,scen)
	
	res <- list()
	for(i in 1:length(scen)){
		x <- read.csv(scen[i],header=F,as.is = T)
		sti <- x[grepl("stiname",x[,1]),2]
		type <- x[grepl("type",x[,1]),2]
		sched <- as.numeric(x[grepl("schedule",x[,1]),2])
		res[[i]] <- list(sti = sti, type=type, sched=sched)
	}
	return(res)
}
	

plot.final.prev <- function(sim){
	### PLOT FINAL PREVALENCE ONLY
	### (mostly used to test baseline intervention is on target prevalence)
	
	n <- sum(grepl(pattern = "MC_",x = names(sim)))
	for(i in 1:n){
		if(i==1) x <- sim[[i]]$prev_final
		if(i>1) x <- rbind(x,sim[[i]]$prev_final)
	}	
	colnames(x) <- sim[[1]]$STInames
	m <- apply(x, MARGIN = 2,FUN = mean)
	
	boxplot(x,col="lightgrey", main = "Prevalence at horizon")
	points(x=factor(c(1,2)),y=m, cex=2,lwd=6, col="red")
	abline(h=m,lty=2,col="red")
	text(x=factor(c(1,2)),y=m,labels = round(m,4),col="red",pos = 3,font = 2)
	grid()
}


plot.ts <- function(sim, folder_inputs, scenario_file){
	
	### COLLECTION OF PLOTS 
	### FOR TIME SERIES OF VARIOUS OUTCOMES
	
	interv.info <- get.interv.time(folder_inputs,scenario_file)
	
	### Population ###
	grid.arrange(
		plot.timeseries(sim,varname="nAlive",title="Alive",interv.info=interv.info),
		plot.timeseries(sim,varname="fem.ratio",title="Female ratio",interv.info=interv.info),
		plot.timeseries(sim,varname="sp.ratio",title="Spousal ratio",interv.info=interv.info),
		plot.timeseries(sim,varname="partn.ratio",title="Partneship ratio",interv.info=interv.info),
		plot.timeseries(sim,varname="nCSW",title="CSW",interv.info=interv.info),
		plot.timeseries.aggreg(sim=sim,
							   period="year",
							   varname="nNewBorn",
							   title="Annual number of new born"),
		plot.timeseries(sim,varname="mtctHIV",title="Cumulative MTCT HIV",interv.info=interv.info),
		plot.timeseries(sim,varname="mtctTp",title="Cumulative MTCT Tp",interv.info=interv.info),
		plot.proportion.timeseries(sim,varname=c("mtctHIV","mtctTp"),
								   title="Cumulative MTCT proportions",
								   interv.info=interv.info),
		plot.proportion.timeseries(sim,varname=c("nRskGrp0","nRskGrp1","nRskGrp2"),
								   title="Risk group proportions",
								   interv.info=interv.info)
	)
	 
	grid.arrange(
		plot.timeseries(sim,varname="nSexActRisk0",title="nSexActsRisk0",interv.info=interv.info),
		plot.timeseries(sim,varname="nSexActRisk1",title="nSexActsRisk1",interv.info=interv.info),
		plot.timeseries(sim,varname="nSexActRisk2",title="nSexActsRisk2",interv.info=interv.info),
		plot.timeseries(sim,varname="nSexActRisk9",title="nSexActsRisk9",interv.info=interv.info)
	)
	
	### Diseases ###
	
	grid.arrange(
		plot.timeseries(sim,varname="HIV",title="Number of HIV infected indiv",interv.info=interv.info),
		plot.timeseries(sim,varname="Tp",title="Number of Syphilis infected indiv",interv.info=interv.info),
		plot.timeseries(sim,varname="HIVprev",title="HIV prevalence",interv.info=interv.info),
		plot.timeseries(sim,varname="Tpprev",title="Tp prevalence",interv.info=interv.info),
		plot.incidence(sim,
					   period="year", 
					   stiname="HIV", 
					   type="rate", 
					   title="Incidence rate HIV",
					   interv.info=interv.info),
		plot.incidence(sim,
					   period="year", 
					   stiname="Tp", 
					   type="rate", 
					   title="Incidence rate Tp",
					   interv.info=interv.info))
	grid.arrange(
		plot.proportion.timeseries(sim,varname=c("HIV","Tp"),
								   title="Prevalence proportions",
								   interv.info=interv.info),
		plot.proportion.timeseries(sim,varname=paste0("HIVprevRisk",c(0,1,2)),
								   title="HIV by risk group",
								   interv.info=interv.info),
		plot.proportion.timeseries(sim,varname=paste0("TpprevRisk",c(0,1,2)),
								   title="Tp by risk group",
								   interv.info=interv.info),
		plot.prev.risk(sim, stiname="HIV",interv.info=interv.info),
		plot.prev.risk(sim, stiname="Tp",interv.info=interv.info)
	)
	try(plot.interv(sim), silent = TRUE)
}


plot.pop.all <- function(sim){
	
	
	try(plot.durPrtn.rskgrp(sim),silent = TRUE)
	try(plot.prtn.distrib(sim), silent = TRUE)
	grid.arrange(
		plot.lifePtrn(sim),
		plot.curr.life.prtn(sim),
		plot.everVisitCSW(sim)
	)
	
	try(plot.stipos.age(sim,stiname="HIV",excl.csw=TRUE), silent = TRUE)
	try(plot.stipos.age(sim,stiname="Tp",excl.csw=TRUE), silent = TRUE)
	
	try(
		grid.arrange(ncol=2,
					 plot.stipos.prtn(sim,stiname="HIV",excl.csw=TRUE),
					 plot.stipos.prtn(sim,stiname="Tp",excl.csw=TRUE)
		),
		silent = TRUE)
	
	try(plot.immunity(sim,stiname="Tp"), silent=TRUE)
}
