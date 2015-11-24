#######################################
###
###  SCRIPT TO PERFORM VARIOUS TESTS
###
#######################################

t0 <- as.numeric(Sys.time())

args <- commandArgs(trailingOnly = TRUE)
pop <- args[1]  # Population name (e.g. "A","B" or "C")

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
folder_inputs <- paste0("../inputs_",pop,"/")
folder_calib <- "../calibration/"

### Founder population parameters:
founder_file <- "in_populationFeatures.csv"

# Scenario file defining interventions
# that will be run during this simulation:
scenario_file <-  "in_scenario_baseline.csv"

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
t1 <- as.numeric(Sys.time())
message(paste("Duration simulations:",round((t1-t0)/60,2),"minutes"))

#################################################################
### Plots (plot_sim.R)
#################################################################
message("plotting...")
# plot time series:
pdf(paste0("plot_ts_",pop,".pdf"),width=20,height=12)
plot.ts(sim)
dev.off()

# plot population:
pdf(paste0("plot_pop_",pop,".pdf"),width=20,height=12)
plot.pop.all(sim)
dev.off()
message("... plotting done.")

# -----------------------------------------------------------------

t2 <- as.numeric(Sys.time())
message(paste("----- Time elapsed:",round((t2-t0)/60,1))," minutes -----")
save.image(file = paste0("test-baseline-",pop,".RData"))
message(" === END ===")