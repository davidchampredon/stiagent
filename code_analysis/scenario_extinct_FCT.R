DIR.OUT <- "../OUT/"


source("analyze_simulation_FCT.R")
require(plyr)

mc.Tp.extinct.before.interv <- function(intervFileName){
	
	### RETURN MC ITERATIONS WHERE Tp WENT 
	### EXTINCT BEFORE INTERVENTION STARTS
	
  # retrieve prevalence time series
	f <- read.csv(paste0("../",intervFileName),header=F)
	prev <- read.simulation.file("prev_mc")
	
	# retrieve date of intervention start
	start.interv <- as.numeric(as.character((f$V2[as.character(f$V1)=="_schedule_0"])))
	
	prev$beforeInterv <- FALSE
	prev$beforeInterv[prev$time<start.interv] <- TRUE
	x <- ddply(prev,c("iMC","beforeInterv"),summarize, minprev = min(Tp))
	x <- subset(x,(beforeInterv==TRUE & minprev==0))
	
	res <- NA
	if(nrow(x)>0) res = x$iMC
	cat(intervFileName,":",length(res),"MC went extinct before intervention (->removed):",res)
	return(res)
}

mc.HIV.extinct.before.interv <- function(intervFileName){
  
  ### RETURN MC ITERATIONS WHERE Tp WENT 
  ### EXTINCT BEFORE INTERVENTION STARTS
  
  # retrieve prevalence time series
  f <- read.csv(paste0("../",intervFileName),header=F)
  prev <- read.simulation.file("prev_mc")
  
  # retrieve date of intervention start
  start.interv <- as.numeric(as.character((f$V2[as.character(f$V1)=="_schedule_0"])))
  
  prev$beforeInterv <- FALSE
  prev$beforeInterv[prev$time<start.interv] <- TRUE
  x <- ddply(prev,c("iMC","beforeInterv"),summarize, minprev = min(HIV))
  x <- subset(x,(beforeInterv==TRUE & minprev==0))
  
  res <- NA
  if(nrow(x)>0) res = x$iMC
  if(!is.na(res)) cat(intervFileName,":",length(res),"MC went extinct before intervention (->removed):",res)
  if(is.na(res))  cat(intervFileName,": no extinction before intervention")
  return(res)
}


intervFileName <- "interv_base_Tp.csv"
yy <- mc.Tp.extinct.before.interv(intervFileName)
