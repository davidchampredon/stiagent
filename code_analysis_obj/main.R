
source("run_one_scenario.R")
source("reformat_obj.R")
source("plot_comp_scen.R")

library(parallel)
cpumax <- parallel::detectCores()

t0 <- as.numeric(Sys.time())

path.stiagent.lib <- "../Rlibrary/lib"

### path to model input files:
folder_inputs = "../inputs/"
folder_calib = "../calibration/"


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


### Run each scenario
###

all.scen <- lapply(X = scenario_file, 
				   FUN = stiagent_runsim_one_scen,
				   folder_inputs=folder_inputs,
				   folder_calib=folder_calib,
				   founder_file = founder_file,
				   n.mc=n.mc,
				   n.cpu=n.cpu,
				   path.stiagent.lib=path.stiagent.lib)


### Calculate summary statistics for all scenario
###
summ.scen <- summary.scenarios(all.scen, qLo = 0.1, qHi=0.9)


### Plots
###
pdf("comp_scen.pdf", width=10)
plot.comp.scen(summ.scen)
dev.off()

# =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=

t1 <- as.numeric(Sys.time())
save.image(file = "main.RData")
message(paste("time elapsed:",round((t1-t0)/60,1),"minutes"))

