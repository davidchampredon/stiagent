
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
								   title="Tp by risk group")
	)
	dev.off()
	
}
