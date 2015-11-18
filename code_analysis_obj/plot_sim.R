source("plot_ts.R")
source("plot_pop.R")

plot.ts <- function(sim){
	
	### COLLECTION OF PLOTS 
	### FOR TIME SERIES OF VARIOUS OUTCOMES
	
	pdf("plot_ts.pdf",width=20,height=12)
	
	
	### Population ###
	
	
	grid.arrange(
		plot.timeseries(sim,varname="nAlive",title="Alive"),
		plot.timeseries(sim,varname="fem.ratio",title="Female ratio"),
		plot.timeseries(sim,varname="sp.ratio",title="Spousal ratio"),
		plot.timeseries(sim,varname="partn.ratio",title="Partneship ratio"),
		plot.timeseries(sim,varname="nCSW",title="CSW"),
		plot.timeseries.aggreg(sim=sim,
							   period="year",
							   varname="nNewBorn",
							   title="Annual number of new born"),
		plot.timeseries(sim,varname="mtctHIV",title="Cumulative MTCT HIV"),
		plot.timeseries(sim,varname="mtctTp",title="Cumulative MTCT Tp"),
		plot.proportion.timeseries(sim,varname=c("mtctHIV","mtctTp"),
								   title="MTCT proportions"),
		plot.proportion.timeseries(sim,varname=c("nRskGrp0","nRskGrp1","nRskGrp2"),
								   title="Risk group proportions")
	)
	
	
	### Diseases ###
	
	grid.arrange(
		plot.timeseries(sim,varname="HIV",title="Number of HIV infected indiv"),
		plot.timeseries(sim,varname="Tp",title="Number of Syphilis infected indiv"),
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
	dev.off()
	
}



plot.pop.all <- function(sim){
	pdf("plot_pop.pdf",width=20,height=12)
	
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
	dev.off()
}
