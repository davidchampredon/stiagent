
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
scenario_file <- list()
scenario_file[[1]] <- "in_scenario_baseline.csv"
scenario_file[[2]] <- "in_scenario_vaxMass.csv"
scenario_file[[3]] <- "in_scenario_TrSympt.csv"

### run in parallel using snowfall:
n.mc <- 4
n.cpu <- 4


### Run each scenario
###

# all.scen <- list()
# 
# for(i in 1:length(scenario_file)){
# 	all.scen[[i]] <- stiagent_runsim_one_scen(folder_inputs,
# 									 folder_calib,
# 									 scenario_file[[i]],
# 									 n.mc,
# 									 n.cpu,
# 									 path.stiagent.lib)
# }


# res1 <- stiagent_runsim_one_scen(folder_inputs,
# 								 folder_calib,
# 								 scenario_file1,
# 								 n.mc,
# 								 n.cpu,
# 								 path.stiagent.lib)
# 
# res2 <- stiagent_runsim_one_scen(folder_inputs,
# 								 folder_calib,
# 								 scenario_file2,
# 								 n.mc,
# 								 n.cpu,
# 								 path.stiagent.lib)
# 
# res3 <- stiagent_runsim_one_scen(folder_inputs,
# 								 folder_calib,
# 								 scenario_file3,
# 								 n.mc,
# 								 n.cpu,
# 								 path.stiagent.lib)

all.scen <- lapply(X = scenario_file, 
				   FUN = stiagent_runsim_one_scen,
				   folder_inputs=folder_inputs,
				   folder_calib=folder_calib,
				   n.mc=n.mc,
				   n.cpu=n.cpu,
				   path.stiagent.lib=path.stiagent.lib)

# all.scen <- list(res1,res2,res3)


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

