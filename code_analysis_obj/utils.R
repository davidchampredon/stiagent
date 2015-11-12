


library(plyr)
library(dplyr)
library(tidyr)

get.nMC <- function(sim){
	### RETURN THE NUBER OF MONTE CARLO ITERATIONS 
	return(sum(grepl("MC_",names(sim))))
}

get.timeseries <- function(sim){
	### RETRIEVE ALL TIME SERIES (FOR EVERY MC ITER)
	### IN A DATA FRAME FORMAT
	
	stinames <- sim[[1]]$STInames
	n.sti <- length(stinames)
	n.mc <- get.nMC(sim)
	
	D <- list()
	for(i in 1:n.mc){
		
		# from list to data frame:
		D[[i]]<- as.data.frame(sim[[i]]$df_sim)
		D[[i]]$mc <- i
		
		# add usefull transformed variables:
		D[[i]]$month <- ceiling(D[[i]]$time*12)
		D[[i]]$year <- ceiling(D[[i]]$time)
		D[[i]]$fem.ratio <- D[[i]]$nFemale/D[[i]]$nAlive
		D[[i]]$partn.ratio <- D[[i]]$nPartn/D[[i]]$nAlive
		D[[i]]$sp.ratio <- D[[i]]$nSp/D[[i]]$nAlive
		D[[i]]$csw.prop <- D[[i]]$nSp/D[[i]]$nAlive
		
		# STIs specifics:
		for(k in 1:n.sti) {
			# prevalence (percentage)
			D[[i]]$tmp <- D[[i]][,stinames[k]]/D[[i]]$nAlive
			names(D[[i]])[length(names(D[[i]]))] <- paste0("prev",stinames[k])
			
			# incidence:
			D[[i]]$tmp <- c(0,pmax(0,diff(D[[i]][,stinames[k]])))
			names(D[[i]])[length(names(D[[i]]))] <- paste0("inc",stinames[k])
		}
	}	
	return(dplyr::rbind_all(D))
}


calc.incidence.rate <- function(sim,period,stiname){
	### CALCULATE INCIDENT CASES AND RATE FOR A GIVEN PERIOD
	
	DF.all <- get.timeseries(sim)
	DF <- DF.all[,c("time",period,"mc",stiname,"nAlive")]
	
	z = unlist(c(DF[,stiname]))
	DF$inc <- c(0,pmax(0,diff(z)))
	# Manage the transition b/w 2 MC iterations
	# (the 'diff' did not managa that):
	DF$inc[DF$time==0] <- 0
	
	DF.summ <- ddply(DF,c(period,"mc"),summarize, 
					 sinc = sum(inc), 
					 avgpop = mean(nAlive))
	
	DF.summ$incrate <- DF.summ$sinc/DF.summ$avgpop
	
	names(DF.summ)[names(DF.summ)=="sinc"] <- paste("inc",period,stiname,sep=".")
	names(DF.summ)[names(DF.summ)=="incrate"] <- paste("incrate",period,stiname,sep=".")
	
	return(DF.summ)
}


get.population <- function(sim,alive.only=TRUE){
	### RETRIEVE ALL POPULATION (ONE FOR EVERY MC ITER)
	### IN A DATA FRAME FORMAT
	
	n.mc <- get.nMC(sim)
	D <- list()
	for(i in 1:n.mc){
		D[[i]] <- as.data.frame(sim[[i]]$population)
		D[[i]]$mc <- i
		
		D[[i]]$Gender <- "female"
		D[[i]]$Gender[D[[i]]$gender==1] <- "male"
	}	
	D.all <- dplyr::rbind_all(D)
	if(alive.only) D.all <- subset(D.all,isalive>0)
	return(D.all)
}



