source("run_one_scenario.R")

t0 <- Sys.time()

path.stiagent.lib <- "../Rlibrary/lib"


### path to model input files:
folder_inputs = "../inputs/"
folder_calib = "../calibration/"

# Scenario file defining interventions
# that will be run during this simulation:
scenario_file1 <- "in_scenario_baseline.csv"
scenario_file2 <- "in_scenario_vaxMass.csv"

### run in parallel using snowfall:
n.mc <- 4
n.cpu <- 2

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



all.scen <- list(res1,res2)

n.scen <- length(all.scen)


calc.incidence <- function(df,stinames){
    # Calculate incidence from prevalence time series
    # output from "df_sim"
    inc <- list()
    for(i in 1:length(stinames)){
        y <- df[stinames[i]][,1]
        diff(y)
        z <- unlist(c(y[1],diff(y)))
        z[z<0] <- 0
        inc[[i]] <- z 
        names(inc)[i] <- names(df[stinames[i]])
    }
    return(inc)
}


calc.cuminc.final <- function(df,stinames){
    # Calculate final cumulative incidence
    # for all STIs
    inc <- as.data.frame(calc.incidence(df,stinames))
    cuminc <- apply(X = inc, MARGIN = 2, sum)
    return(cuminc)
}


#---
s=1
i=1
x <- all.scen[[1]] 
stinames <- x[[1]]$STInames
df <- as.data.frame(x[[1]]$df_sim)
#---


# STOPPED HERE:
# put the loop below into a function


mean.prev <- list()
mean.cuminc <- list()
mean.mtct <- list()
mean.popsize <-list()

for(s in 1:n.scen){
    
    x <- all.scen[[s]] 
    
    n.mc <- sum(grepl("MC_",names(x)))
    n.sti <- length(x[[1]]$STInames)
    
    # Build matrices where:
    # rows = MC iter
    # col = STI
    #
    # then average across all MC iterations (i.e. rows)
    
    
    # Merge all response variables 
    # into their respective matrix:
    #
    M.prev <- matrix(nrow=n.mc,ncol=n.sti)
    M.cuminc <- matrix(nrow=n.mc,ncol=n.sti)
    M.mtct <- matrix(nrow=n.mc,ncol=n.sti)
   
    M.popsize <- matrix(nrow=n.mc,ncol=1)
    
    colnames(M.prev) <- stinames
    colnames(M.cuminc) <- stinames
    colnames(M.mtct) <- stinames
    
    for(i in 1:n.mc) {
        df <- as.data.frame(x[[i]]$df_sim)
        stinames <- x[[i]]$STInames
        
        M.prev[i,] <- x[[i]][["prev_final"]] 
        M.cuminc[i,] <- calc.cuminc.final(df = df, stinames=stinames)
        M.mtct[i,] <- x[[i]][["cuminc_mtct_final"]]
        M.popsize[i,] <- x[[i]][["popsize_alive"]]
    }
   
    
    mean.prev[[s]] <- apply(M.prev,MARGIN = 2, FUN = mean)
    mean.cuminc[[s]] <- apply(M.cuminc,MARGIN = 2, FUN = mean)
    mean.mtct[[s]] <- apply(M.mtct,MARGIN = 2, FUN = mean)
    mean.popsize[[s]] <- apply(M.popsize,MARGIN = 2, FUN = mean)
 
    scen.names <- all.scen[[s]]$scenario_file   
    names(mean.prev)[s] <- scen.names
    ### TO DO: names(...)<- scen.names
    
}

#### end loop to put into function




save.image(file = "test.RData")
t1 <- Sys.time()
message(paste("time elapsed:",round(t1-t0,1),"sec"))