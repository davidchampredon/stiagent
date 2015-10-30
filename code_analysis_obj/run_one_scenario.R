###
### Parallel run a Monte-Carlo simulation with one scenario
###

library(snowfall)


stiagent_runsim_one_scen <- function(folder_inputs,
                                     folder_calib,
                                     scenario_file,
                                     n.mc,
                                     n.cpu,
                                     path.stiagent.lib)
{
    #### Initialize snowfall (parallel execution)
    sfInit(parallel = TRUE, cpu = n.cpu)
    sfLibrary(stiagent, lib.loc=path.stiagent.lib)
    
    # snowfall wrap function:
    stiagent_runsim_snowWrap <- function(i,
                                         folder_inputs,
                                         folder_calib,
                                         scenario_file){
        
        x <- stiagent_runsim(params = list(folder_inputs = folder_inputs,
                                           folder_calib = folder_calib,
                                           scenario_file = scenario_file,
                                           MC_id = i))
        return(x)
    }
    
    ### Parallel execution:
    sfExportAll()
    idx.apply <- 1:n.mc
    res <- sfSapply(idx.apply, 
                    fun = stiagent_runsim_snowWrap,
                    folder_inputs = folder_inputs,
                    folder_calib = folder_calib,
                    scenario_file = scenario_file,
                    simplify = FALSE)
    sfStop()
    
    names(res)<-paste0("MC_",1:n.mc)
    
    return(c(res,list(scenario_file=scenario_file)))   
}


# ====================================================================
# ====================================================================


### ==== TEST =====
if(FALSE){
    
    t0 <- Sys.time()
    
    path.stiagent.lib <- "../Rlibrary/lib"
    
    # library(stiagent, lib.loc = path.stiagent.lib)
    
    ### path to model input files:
    folder_inputs = "../inputs/"
    folder_calib = "../calibration/"
    
    # Scenario file defining interventions
    # that will be run during this simulation:
    scenario_file <- "in_scenario_vaxMass.csv"
    
    ### run in parallel using snowfall:
    n.mc <- 4
    n.cpu <- 2
    
    res <- stiagent_runsim_one_scen(folder_inputs,
                                    folder_calib,
                                    scenario_file,
                                    n.mc,
                                    n.cpu,
                                    path.stiagent.lib)
    
    save.image(file = "onescen.RData")
    t1 <- Sys.time()
    message(paste("time elapsed:",round(t1-t0,1),"sec"))
}
# ====================================================================
# ====================================================================


