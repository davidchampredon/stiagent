
source("run_one_scenario.R")
source("reformat_obj.R")
source("plot_comp_scen.R")


t0 <- as.numeric(Sys.time())

path.stiagent.lib <- "../Rlibrary/lib"

### path to model input files:
folder_inputs = "../inputs/"
folder_calib = "../calibration/"

# Scenario file defining interventions
# that will be run during this simulation:
scenario_file1 <- "in_scenario_baseline.csv"
scenario_file2 <- "in_scenario_vaxMass.csv"
scenario_file3 <- "in_scenario_TrSympt.csv"

### run in parallel using snowfall:
n.mc <- 10
n.cpu <- 4


### Run each scenario
###
res1 <- stiagent_runsim_one_scen(folder_inputs,
								 folder_calib,
								 scenario_file1,
								 n.mc,
								 n.cpu,
								 path.stiagent.lib)

res2 <- stiagent_runsim_one_scen(folder_inputs,
								 folder_calib,
								 scenario_file2,
								 n.mc,
								 n.cpu,
								 path.stiagent.lib)

res3 <- stiagent_runsim_one_scen(folder_inputs,
								 folder_calib,
								 scenario_file3,
								 n.mc,
								 n.cpu,
								 path.stiagent.lib)

all.scen <- list(res1,res2,res3)


### Calculate summary statistics for all scenario
###
summ.scen <- summary.scenarios(all.scen, qLo = 0.1, qHi=0.9)


### Plots
###
plot.comp.scen(summ.scen)


# =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=

t1 <- as.numeric(Sys.time())
save.image(file = "main.RData")
message(paste("time elapsed:",round(t1-t0,1)/60,"minutes"))

