#####################################################################
###
###   COMPARE OUTCOMES FROM DIFFERENT INTERVENTION SCENARIOS
###
#####################################################################

t0 <- as.numeric(Sys.time())

source("run_one_scenario.R")
source("reformat_obj.R")
source("plot_comp_scen.R")

library(parallel)
cpumax <- parallel::detectCores()

### Command line arguments ------------------
###
args <- commandArgs(trailingOnly = TRUE)
pop <- args[1]  # Population "A", "B" or "C"  # args<- "A"
### -----------------------------------------

### paths:
path.stiagent.lib <- "../Rlibrary/lib"
folder_inputs = paste0("../inputs_",pop,"/")
folder_calib = "../calibration/"

### Founder population parameters:
founder_file <- "in_populationFeatures.csv"

# Scenario file defining interventions
# that will be run during this simulation:
scenario_file <- list()

scenario_file[[1]] <- "in_scenario_baseline.csv"
scenario_file[[2]] <- "in_scenario_TrMass.csv"
scenario_file[[3]] <- "in_scenario_VaxMass.csv"
scenario_file[[4]] <- "in_scenario_VaxYoung.csv"
scenario_file[[5]] <- "in_scenario_VaxHiRisk.csv"

# uncomment below and comment above for test simulation:
# scenario_file[[1]] <- "in_scenario_baseline.csv"
# scenario_file[[2]] <- "in_scenario_VaxMass.csv"

### Simulation parameters:
ps <- read.csv("prm_simul.csv",header = FALSE)
n.mc <- ps[ps[,1]=="mc_iter",2]
n.cpu <- ps[ps[,1]=="ncpu",2]
if (n.cpu<=0) n.cpu <- max(1,cpumax+n.cpu)

### Information on this run:
message("Scenarios comparison started...")
print(" -- Running scenarios comparison --")
print(paste0("  Population:   ",pop))
print("  Scenario files: ")
print(paste("  ",scenario_file,collapse = ";"))
print(paste0("  MC iter: ",n.mc))
print(paste0("  CPUs:    ",n.cpu))

### Retrieve value of important parameters ###
###

# Vaccine features:
vaxdata <- read.csv(file = paste0(folder_inputs,"in_STI_vaccine.csv"),header = F)
vax.fail <- vaxdata[vaxdata$V1=="Tp_vaccine_fail",2]
vax.VRE <- vaxdata[vaxdata$V1=="Tp_VRE",2]
vax.wane <- vaxdata[vaxdata$V1=="Tp_vacc_waneRate",2]
### File name to save results to
###
vax.wane2 <- gsub(pattern = ".",
				  replacement = "p",
				  x = vax.wane,fixed = T)
fname <- paste("compScen",
			   pop,
			   vax.fail,
			   vax.VRE,
			   vax.wane2,
			   sep="_")

### Run each scenario ###
###
print(paste(">> starting simulations",fname))

all.scen <- lapply(X = scenario_file, 
				   FUN = stiagent_runsim_one_scen,
				   folder_inputs = folder_inputs,
				   folder_calib = folder_calib,
				   founder_file = founder_file,
				   n.mc = n.mc,
				   n.cpu = n.cpu,
				   path.stiagent.lib = path.stiagent.lib
				   ,displayProgress = 0 
				   )  

x<- ls()
N=length(x)
### Retrieve their size:
mem <- vector()
for(i in 1:N){
	# print(paste(x[i],"-->",object.size(get(x[i]))/1000,"Mb"))
	mem[i] <- object.size(get(x[i]))
}
print(paste("memory comparison scenarios:",log10(sum(mem,na.rm = TRUE))))
# ----

print(" * * * * * * * * * * * * * * * * * * * * * * * * * * * * ")
print(paste("* * * SCENARIO COMPARISON COMPLETED FOR:",fname,"* * * "))

### Calculate summary statistics for all scenario
###
summ.scen <- summary.scenarios(all.scen, qLo = 0.1, qHi=0.9)
write.csv(x = summ.scen,
		  quote = FALSE,
		  file = paste0(fname,".csv"))

t1 <- as.numeric(Sys.time())
save.image(file = paste0(fname,".RData"))

### Plots
###
pdf( paste0(fname,".pdf"), width=10)
plot.comp.scen(summ.scen)
dev.off()

# =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=

t2 <- as.numeric(Sys.time())
print("")
print(paste("||=== Time elapsed:",round((t2-t0)/60,1),"minutes ===||"))

