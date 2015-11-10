library(plyr)

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


stat.in.df <- function(scen.names,stinames,response,stat,val){
	return(data.frame(scen = scen.names, 
					  sti = stinames, 
					  response = response,
					  stat = stat,
					  val = val))
}


calc.diff.baseline <- function(x,name.baseline="baseline"){
	### Difference of response variables means
	### to "baseline" scenario:
	
	x$diff.baseline <- NA
	x$reldiff.baseline <- NA
	
	for(i in 1:nrow(x)){
		if(!grepl(name.baseline,x$scen[i]) & x$stat[i]=="mean"){
			
			# Retrieve the associated value from baseline scenario   
			idx.i <- which(x$sti==x$sti[i] &
						   	x$response==x$response[i] & 
						   	x$stat == "mean" &
						   	grepl(name.baseline,x$scen))
			# Calculate differences:
			base.i <- x$val[idx.i]
			x$diff.baseline[i] <- x$val[i] - base.i
			x$reldiff.baseline[i] <- x$val[i]/base.i-1
		}
	}
	return(x)
}

summary.scenarios <- function(all.scen, name.baseline = "baseline", qLo=0.025, qHi=0.975){
	###
	###  SUMMARIZE SEVERAL RESPONSE VARIABLES (E.G. PREVALENCE,POP SIZE,...) 
	###  FOR MULTIPLE SCENARIOS INTO A DATA FRAME
	###
	###  all.scen = list(scen1,scen2,scen3,...)
	###
	
	n.scen <- length(all.scen)
	D <- list()
	
	for(s in 1:n.scen){
		
		x <- all.scen[[s]] 
		
		n.mc <- sum(grepl("MC_",names(x)))
		n.sti <- length(x[[1]]$STInames)
		
		### Build matrices where:
		### rows = MC iter
		### col = STI
		### Then average across all MC iterations (i.e. rows)
		###
		M.prev <- matrix(nrow=n.mc,ncol=n.sti)
		M.cuminc <- matrix(nrow=n.mc,ncol=n.sti)
		M.mtct <- matrix(nrow=n.mc,ncol=n.sti)
		M.popsize <- matrix(nrow=n.mc,ncol=1)
		
		stinames <- x$MC_1$STInames
		colnames(M.prev) <- stinames
		colnames(M.cuminc) <- stinames
		colnames(M.mtct) <- stinames
		
		# Merge all response variables 
		# into their respective matrix:
		#
		for(i in 1:n.mc) {
			df <- as.data.frame(x[[i]]$df_sim)
			stinames <- x[[i]]$STInames
			
			M.prev[i,] <- x[[i]][["prev_final"]] 
			M.cuminc[i,] <- calc.cuminc.final(df = df, stinames=stinames)
			M.mtct[i,] <- x[[i]][["cuminc_mtct_final"]]
			M.popsize[i,] <- x[[i]][["popsize_alive"]]
		}
		
		# Stats across all MC iterations:
		#
		mean.prev <- apply(M.prev,MARGIN = 2, FUN = mean)
		mean.cuminc <- apply(M.cuminc,MARGIN = 2, FUN = mean)
		mean.mtct <- apply(M.mtct,MARGIN = 2, FUN = mean)
		mean.popsize <- apply(M.popsize,MARGIN = 2, FUN = mean)
		
		qLo.prev <- apply(M.prev,MARGIN = 2, FUN = quantile, probs= qLo)
		qLo.cuminc <- apply(M.cuminc,MARGIN = 2, FUN = quantile, probs= qLo)
		qLo.mtct <- apply(M.mtct,MARGIN = 2, FUN = quantile, probs= qLo)
		qLo.popsize <- apply(M.popsize,MARGIN = 2, FUN = quantile, probs= qLo)
		
		qHi.prev <- apply(M.prev,MARGIN = 2, FUN = quantile, probs= qHi)
		qHi.cuminc <- apply(M.cuminc,MARGIN = 2, FUN = quantile, probs= qHi)
		qHi.mtct <- apply(M.mtct,MARGIN = 2, FUN = quantile, probs= qHi)
		qHi.popsize <- apply(M.popsize,MARGIN = 2, FUN = quantile, probs= qHi)
		
		# Clean-up scenario names
		scen.names <- rep(x = all.scen[[s]]$scenario_file,times=n.sti)
		scen.names <- gsub(x=scen.names,pattern = "in_scenario_",replacement = "")
		scen.names <- gsub(x=scen.names,pattern = ".csv",replacement = "")
		
		
		D[[s]] <- stat.in.df(scen.names, stinames,"prev_final","mean",mean.prev)
		D[[s]] <- rbind(D[[s]],stat.in.df(scen.names, stinames,"prev_final","qLo",qLo.prev))
		D[[s]] <- rbind(D[[s]],stat.in.df(scen.names, stinames,"prev_final","qHi",qHi.prev))
		
		D[[s]] <- rbind(D[[s]],stat.in.df(scen.names, stinames,"cuminc","mean",mean.cuminc))
		D[[s]] <- rbind(D[[s]],stat.in.df(scen.names, stinames,"cuminc","qLo",qLo.cuminc))
		D[[s]] <- rbind(D[[s]],stat.in.df(scen.names, stinames,"cuminc","qHi",qHi.cuminc))
		
		D[[s]] <- rbind(D[[s]],stat.in.df(scen.names, stinames,"mtct","mean",mean.mtct))
		D[[s]] <- rbind(D[[s]],stat.in.df(scen.names, stinames,"mtct","qLo",qLo.mtct))
		D[[s]] <- rbind(D[[s]],stat.in.df(scen.names, stinames,"mtct","qHi",qHi.mtct))
		
		D[[s]] <- rbind(D[[s]],stat.in.df(scen.names, stinames,"popsize","mean",mean.popsize))
		D[[s]] <- rbind(D[[s]],stat.in.df(scen.names, stinames,"popsize","qLo",qLo.popsize))
		D[[s]] <- rbind(D[[s]],stat.in.df(scen.names, stinames,"popsize","qHi",qHi.popsize))
		
	} # end for 1:nscen
	
	df.all <- dplyr::rbind_all(D)
	
	df.all <- calc.diff.baseline(x = df.all, name.baseline)
	
	return(df.all)
}





	
	

