###
### Parallel run a Monte-Carlo simulation with one scenario
###

t0 <- Sys.time()

path.stiagent.lib <- "../Rlibrary/lib"

library(snowfall)
library(stiagent, lib.loc = path.stiagent.lib)
# library(dplyr)
# library(plyr)
# library(ggplot2)
# library(reshape2)


### path to model input files:
folder_inputs = "../inputs/"
folder_calib = "../calibration/"

# Scenario file defining interventions
# that will be run during this simulation:
scen.file <- "in_scenario_vaxMass.csv"

### run in parallel using snowfall:
n.mc <- 4
n.cpu <- 2
sfInit(parallel = TRUE, cpu = n.cpu)
sfLibrary(stiagent, lib.loc=path.stiagent.lib)

stiagent_runsim_snowWrap <- function(i){
    # snowfall wrap function
    x <- stiagent_runsim(params = list(folder_inputs = folder_inputs,
                                       folder_calib = folder_calib,
                                       scenario_file = scen.file,
                                       MC_id = i))
    return(x)
}

### Parallel execution:
sfExportAll()
idx.apply <- 1:n.mc
res <- sfSapply(idx.apply, 
                stiagent_runsim_snowWrap, 
                simplify = FALSE)
sfStop()





# ====================================================================
# ====================================================================
t1 <- Sys.time()
message(paste("time elapsed:",round(t1-t0,1),"sec"))
