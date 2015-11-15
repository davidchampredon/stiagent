#######################################
###
###  SCRIPT TO PERFORM VARIOUS TESTS
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
founder_file <- "in_populationFeatures_TEST.csv"

# Scenario file defining interventions
# that will be run during this simulation:
scenario_file <-  "in_scenario_test.csv"

ps <- read.csv("prm_simul.csv",header = FALSE)
n.mc <- ps[ps[,1]=="mc_iter",2]
n.cpu <- ps[ps[,1]=="ncpu",2]
if (n.cpu<=0) n.cpu <- max(1,cpumax+n.cpu)



#################################################################
### Run simulation
#################################################################

sim <- stiagent_runsim_one_scen(folder_inputs,
								folder_calib,
								founder_file,
								scenario_file,
								n.mc = 3,
								n.cpu,
								path.stiagent.lib,
								displayProgress=0)


#################################################################
### Plots (plot_sim.R)
#################################################################

# plot time series:
plot.ts(sim)

# plot population:
plot.pop.all(sim)

# -----------------------------------------------------------------

t1 <- as.numeric(Sys.time())
message(paste("----- Time elapsed:",round((t1-t0)/60,1))," minutes -----")
save.image(file = "test.RData")