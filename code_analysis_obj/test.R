#######################################
###
###  SCRIPT TO PERFORM VARIOUS TESTS
###
#######################################

t0 <- as.numeric(Sys.time())

source("run_one_scenario.R")
# source("reformat_obj.R")
# source("plot_comp_scen.R")

library(gridExtra)
library(parallel)
cpumax <- parallel::detectCores()


#################################################################
### MODEL SET-UP
#################################################################

### paths
path.stiagent.lib <- "../Rlibrary/lib"
folder_inputs <- "../inputs/"
folder_calib <- "../calibration/"

### Founder population parameters:
founder_file <- "in_populationFeatures_TEST.csv"

# Scenario file defining interventions
# that will be run during this simulation:
scenario_file <-  "in_scenario_test.csv"

ps <- read.csv("prm_simul.csv",header = FALSE)
n.mc <- ps[ps[,1]=="mc_iter",2]
n.cpu <- ps[ps[,1]=="ncpu",2]
if (n.cpu<=0) n.cpu <- max(1,cpumax-n.cpu)



#################################################################
### Run simulation
#################################################################

sim <- stiagent_runsim_one_scen(folder_inputs,
								folder_calib,
								founder_file,
								scenario_file,
								n.mc = 5,
								n.cpu,
								path.stiagent.lib,
								displayProgress=0)


#################################################################
### Plots
#################################################################

pdf("test_plot.pdf",width=20,height=12)
grid.arrange(
	plot.timeseries(sim,varname="nAlive",title="Alive"),
	plot.timeseries(sim,varname="fem.ratio",title="Female ratio"),
	plot.timeseries(sim,varname="sp.ratio",title="Spousal ratio"),
	plot.timeseries(sim,varname="partn.ratio",title="Partneship ratio"),
	plot.timeseries(sim,varname="nCSW",title="CSW"),
	plot.timeseries.aggreg(sim,
						   aggreg.name="year",
						   varname="nNewBorn",
						   title="Annual number of new born"),
	plot.proportion.timeseries(sim,varname=c("nRskGrp0","nRskGrp1","nRskGrp2"),
							   title="Risk group proportions")
)


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

# -----------------------------------------------------------------

t1 <- as.numeric(Sys.time())
message(paste("----- Time elapsed:",round((t1-t0)/60,1)),"minutes -----")
save.image()