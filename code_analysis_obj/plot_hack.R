library(gridExtra)
# load an existing .RData of scenario comparison first!
load("/Users/davidchampredon/Dropbox/MyStudies/Syphilis_vax/simul-results/2016-01-10/baseline/compScen_B_0.2_1_0p05.RData")

source("plot_sim.R")

folder_inputs <- "../inputs/"

NN <- length(all.scen)

for (i in 1:NN) {
	message(paste(i,"/",NN))
	sim <- all.scen[[i]] 
	n<-length(sim)
	scen <- sim[[n]]
	
	# Remove info on scenario name
	# becauseplot functions do not handle this
	sim[[n]] <- NULL
	
	pdf(paste0("plotscen_",scen,".pdf"),width=15,height = 20)
	# plot time series:
	plot.ts(sim,folder_inputs, scenario_file=scen)
	# plot population:
	plot.pop.all(sim)
	dev.off()
}



