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
	

plot.ts <- function(sim){
	
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
								   title="Cumulative MTCT proportions"),
		plot.proportion.timeseries(sim,varname=c("nRskGrp0","nRskGrp1","nRskGrp2"),
								   title="Risk group proportions")
	)
	
	### Diseases ###
	
	grid.arrange(
		plot.timeseries(sim,varname="HIV",title="Number of HIV infected indiv",interv.info=interv.info),
		plot.timeseries(sim,varname="Tp",title="Number of Syphilis infected indiv",interv.info=interv.info),
		plot.incidence(sim,
					   period="year", 
					   stiname="HIV", 
					   type="rate", 
					   title="Incidence rate HIV"),
		plot.incidence(sim,
					   period="year", 
					   stiname="Tp", 
					   type="rate", 
					   title="Incidence rate Tp",
					   interv.date = c(30,40)),
		plot.proportion.timeseries(sim,varname=c("HIV","Tp"),
								   title="Prevalence proportions"),
		plot.proportion.timeseries(sim,varname=paste0("HIVprevRisk",c(0,1,2)),
								   title="HIV by risk group"),
		plot.proportion.timeseries(sim,varname=paste0("TpprevRisk",c(0,1,2)),
								   title="Tp by risk group"),
		plot.prev.risk(sim, stiname="HIV"),
		plot.prev.risk(sim, stiname="Tp")
	)
}



plot.pop.all <- function(sim){
	
	
	plot.durPrtn.rskgrp(sim)
	plot.prtn.distrib(sim)
	grid.arrange(
		plot.lifePtrn(sim),
		plot.curr.life.prtn(sim),
		plot.everVisitCSW(sim)
	)
	
	plot.stipos.age(sim,stiname="HIV",excl.csw=TRUE)
	plot.stipos.age(sim,stiname="Tp",excl.csw=TRUE)
	
	grid.arrange(ncol=2,
				 plot.stipos.prtn(sim,stiname="HIV",excl.csw=TRUE),
				 plot.stipos.prtn(sim,stiname="Tp",excl.csw=TRUE)
	)
	plot.immunity(sim,stiname="Tp")
}
