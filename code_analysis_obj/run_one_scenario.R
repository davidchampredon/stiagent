###
### Parallel run a Monte-Carlo simulation with one scenario
###

library(snowfall)

stiagent_runsim_one_scen <- function(folder_inputs,
                                     folder_calib,
									 founder_file,
                                     scenario_file,
                                     n.mc,
                                     n.cpu,
                                     path.stiagent.lib,
									 displayProgress=0)
{
	### Information messages
	prmsim <- read.csv(paste0(folder_inputs,"in_simulation.csv"),header = F)
	
	print("----")
	print(" STIAGENT started for one scenario...")
	print(paste("  Inputs read in      :     ",folder_inputs))
	print(paste("  Calibration read in :     ",folder_calib))
	print(paste("  Founder pop file :        ",founder_file))
	print(paste("  Scenario defined in file: ",scenario_file))
	print(paste("  Horizon   :",prmsim[prmsim[,1]=="horizon_years",2],"years"))
	print(paste("  Time step :",prmsim[prmsim[,1]=="timestep_days",2],"days"))
	print(paste("  MC iters:",n.mc))
	print(paste("  CPU used:",n.cpu))
	print("----")
	
	ff <- read.csv(paste0(folder_inputs,founder_file),header = F)
	founder.size <- ff[ff[,1]=="founder_size",2]
	founder.fem.prop <- ff[ff[,1]=="founder_femprop",2]
	founder.csw.prop <- ff[ff[,1]=="founder_cswprop",2]
	
    #### Initialize snowfall (parallel execution)
    sfInit(parallel = TRUE, cpu = n.cpu)
    sfLibrary(stiagent, lib.loc=path.stiagent.lib)
    
    # snowfall wrap function:
    stiagent_runsim_snowWrap <- function(i,
                                         folder_inputs,
                                         folder_calib,
                                         scenario_file,
    									 displayProgress){

    	        x <- stiagent_runsim(params = list(folder_inputs = folder_inputs,
                                           folder_calib = folder_calib,
                                           scenario_file = scenario_file,
        								   founder_size = founder.size,
        								   founder_femprop = founder.fem.prop,
        								   founder_cswprop = founder.csw.prop,
        								   displayProgress = displayProgress,
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
    				displayProgress = displayProgress,
                    simplify = FALSE)
    sfStop()
    
    names(res)<-paste0("MC_",1:n.mc)
    
    # ----
    
    x<- ls()
    N=length(x)
    ### Retrieve their size:
    mem <- vector()
    for(i in 1:N){
    	# print(paste(x[i],"-->",object.size(get(x[i]))/1000,"Mb"))
    	mem[i] <- object.size(get(x[i]))
    }
    print(paste("memory one scenario:",log10(sum(mem,na.rm = TRUE))))
    # ----
    
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
    print(paste("time elapsed:",round(t1-t0,1),"sec"))
}
# ====================================================================
# ====================================================================


