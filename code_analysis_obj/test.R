#######################################
###
###  SCRIPT TO PERFORM VARIOUS TESTS
###
#######################################

t0 <- as.numeric(Sys.time())

source("run_one_scenario.R")
# source("reformat_obj.R")
# source("plot_comp_scen.R")

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
scenario_file <- list()
scenario_file[[1]] <- "in_scenario_baseline.csv"
scenario_file[[2]] <- "in_scenario_VaxMass.csv"
scenario_file[[3]] <- "in_scenario_TrSympt.csv"

ps <- read.csv("prm_simul.csv",header = FALSE)
n.mc <- ps[ps[,1]=="mc_iter",2]
n.cpu <- ps[ps[,1]=="ncpu",2]
if (n.cpu<=0) n.cpu <- max(1,cpumax-n.cpu)



#################################################################
### Run simulation
###
sim <- stiagent_runsim_one_scen(folder_inputs,
									 folder_calib,
									 founder_file,
									 scenario_file[[1]],
									 n.mc = 2,
									 n.cpu,
									 path.stiagent.lib,
									 displayProgress=1)
