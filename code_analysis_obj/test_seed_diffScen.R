#######################################
###
###  TEST THAT DIFFERENT SCENARIO
###  ARE STARTED WITH THE SAME SEED 
###  FOR A GIVEN MC ITERATION
###
#######################################

t0 <- as.numeric(Sys.time())

source("run_one_scenario.R")
source("plot_sim.R")

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
founder_file <- "in_populationFeatures.csv"

# Scenario file defining interventions
# that will be run during this simulation:
scenario_file <-  "in_scenario_test2.csv"
scenario_file0 <-  "in_scenario_test.csv"

ps <- read.csv("prm_simul.csv",header = FALSE)
n.mc <- ps[ps[,1]=="mc_iter",2]
n.cpu <- ps[ps[,1]=="ncpu",2]
if (n.cpu<=0) n.cpu <- max(1,cpumax+n.cpu)



#################################################################
### Run simulation
#################################################################
t0 <- as.numeric(Sys.time())
sim <- stiagent_runsim_one_scen(folder_inputs,
								folder_calib,
								founder_file,
								scenario_file,
								n.mc = n.mc,
								n.cpu,
								path.stiagent.lib,
								displayProgress=0)

sim0 <- stiagent_runsim_one_scen(folder_inputs,
								folder_calib,
								founder_file,
								scenario_file0,
								n.mc = n.mc,
								n.cpu,
								path.stiagent.lib,
								displayProgress=0)


t1 <- as.numeric(Sys.time())
message(paste("Duration simulations:",round((t1-t0)/60,2),"minutes"))


for (i in 1:3) {
	x <- sim[[i]]$df_sim$time
	y <- sim[[i]]$df_sim$Tp
	if (i==1) plot(x,y,typ="l",ylim=c(0,200),lwd=3)
	if (i>1) lines(x,y,lwd=3)
	
	x0 <- sim0[[i]]$df_sim$time
	y0 <- sim0[[i]]$df_sim$Tp
	lines(x0,y0,col="red")
	
}
