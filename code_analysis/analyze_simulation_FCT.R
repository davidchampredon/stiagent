### FUNCTIONS FOR READ_SIMULATION ###

library(reshape2)


read.simulation.file <- function(filenameroot)
{
  DIR_OUT = "../outputs/"
	fnames <- system(paste0("ls ",DIR_OUT,filenameroot,"*"),intern = T)
	
	simul = data.frame()
	for (i in 1:length(fnames))
	{
		print(paste("reading file ",fnames[i]))
		
		if (i==1) {
			simul = read.csv(fnames[i],header=TRUE)  
			simul$iMC = i
		}
		if (i>1) {
			tmp = read.csv(fnames[i],header=TRUE)  
			tmp$iMC = i
			simul = rbind(simul,tmp)
		}
	}
	return(simul)
}


read.simulation.file.old <- function(filenameroot,nMC)
{
	simul = data.frame()
	for (i in 1:nMC)
	{
		filename <- paste0(DIR_OUT,filenameroot,i,".out")
		print(paste("reading file ",filename))
		
		if (i==1) {
			simul = read.csv(filename,header=TRUE)  
			simul$iMC = i
		}
		if (i>1) {
			tmp = read.csv(filename,header=TRUE)  
			tmp$iMC = i
			simul = rbind(simul,tmp)
		}
	}
	return(simul)
}


cleanup.file <- function(df)
{
	toremove=which(df$time=="time")
	if(length(toremove)>0){
		df=df[-toremove,]
		df <- data.frame(lapply(df, as.character), stringsAsFactors=FALSE)
		df <- data.frame(lapply(df, as.numeric), stringsAsFactors=FALSE)
	}
	return(df)
}

##########################
###   DEMOGRAPHICS   ###
##########################


# Global variables

par(mfrow=c(3,3))
mylwd = 6

# ====== Population size =======


plot.population <- function(df)
{
	### Time series of population
	g.pop = ggplot(df,aes(x=time,y=nAlive)) + geom_line(aes(dummy=factor(iMC)),size=1.2)
	
	
	### Time series of population
	### by risk groups
	df.rsk = ddply(df,.variables = "time",
				   summarize,
				   risk0 = mean(nRskGrp0),
				   risk1 = mean(nRskGrp1),
				   risk2 = mean(nRskGrp2),
				   risk0min = min(nRskGrp0),
				   risk0max = max(nRskGrp0),
				   risk1min = min(nRskGrp1),
				   risk1max = max(nRskGrp1),
				   risk2min = min(nRskGrp2),
				   risk2max = max(nRskGrp2))
	
	rcol = c("pink","red","darkred")
	
	g.pop.rsk = ggplot(df.rsk,aes(x=time)) + geom_line(aes(y=risk0),size=2,colour=rcol[1])
	g.pop.rsk = g.pop.rsk + geom_line(aes(y=risk1),size=2,colour=rcol[2]) 
	g.pop.rsk = g.pop.rsk + geom_line(aes(y=risk2),size=2,,colour=rcol[3])
	g.pop.rsk = g.pop.rsk +geom_ribbon(aes(ymin=risk0min,ymax=risk0max),fill=rcol[1],alpha=0.2)
	g.pop.rsk = g.pop.rsk +geom_ribbon(aes(ymin=risk1min,ymax=risk1max),fill=rcol[2],alpha=0.2)
	g.pop.rsk = g.pop.rsk +geom_ribbon(aes(ymin=risk2min,ymax=risk2max),fill=rcol[3],alpha=0.2)
	g.pop.rsk = g.pop.rsk + ggtitle("Population by Risk Group")
	
	return(list(g.pop,g.pop.rsk))
}

plot.newborn <- function(simul)
{
	simul$timestep = diff(simul$time)[1] # retrieve the timestep
	
	simul2 = ddply(simul,"time",summarize,
				   mean = mean(nNewBorn/nAlive/timestep),
				   min = min(nNewBorn/nAlive/timestep),
				   max = max(nNewBorn/nAlive/timestep))
	
	
	g.newb = ggplot(simul2)+geom_line(aes(x=time,
										  y=mean))
	g.newb = g.newb + geom_ribbon(aes(x=time,ymin=min,ymax=max),alpha=0.2)
	g.newb = g.newb + ggtitle("Number of newborn per individual per year")+ylab("")
	
	return(g.newb)
}

plot.sexratio<-function(simul)
{
	simul$sexratio = with(simul,(nAlive-nFemale)/nFemale)
	
	simul2 = ddply(simul,"time",summarize,
				   mean = mean(sexratio),
				   min = min(sexratio),
				   max = max(sexratio))
	
	g.sexr = ggplot(simul2)+geom_line(aes(x=time,y=mean))
	g.sexr = g.sexr + geom_ribbon(aes(x=time,ymin=min,ymax=max),alpha=0.2)
	g.sexr = g.sexr + ggtitle("Sex Ratio (male/female)")+ylab("")
	return(g.sexr)
}


# ====== PARTNERSHIPS =======

plot.partnerships <- function(simul)
{
	simul$prop.sp = simul$nSp/simul$nPartn
	
	# Total number of partnerships
	
	simul2 = ddply(simul,.variables = "time",summarize,
				   partnTot = mean(nPartn),
				   partnSp = mean(nSp),
				   partnTot.min = min(nPartn),
				   partnTot.max = max(nPartn),
				   partnSp.min = min(nSp),
				   partnSp.max = max(nSp),
				   prop.spousal = mean(prop.sp),
				   prop.spousal.min = min(prop.sp),
				   prop.spousal.max = max(prop.sp)
	)
	
	mycol=c("black","red")
	g.prtn = ggplot(simul2, aes(x=time))
	g.prtn = g.prtn + geom_line(aes(y=partnTot),col=mycol[1])
	g.prtn = g.prtn + geom_line(aes(y=partnSp),col=mycol[2])
	g.prtn = g.prtn + geom_ribbon(aes(ymin=partnTot.min,ymax=partnTot.max),fill=mycol[1],alpha=0.2)
	g.prtn = g.prtn + geom_ribbon(aes(ymin=partnSp.min,ymax=partnSp.max),fill=mycol[2],alpha=0.2)
	g.prtn = g.prtn + ggtitle("Number of Partnerships (all and spousal only)")
	
	g.prtn.prop = ggplot(simul2, aes(x=time))+geom_line(aes(y=prop.spousal))
	g.prtn.prop = g.prtn.prop + geom_ribbon(aes(ymin=prop.spousal.min,ymax=prop.spousal.max),alpha=0.2)
	g.prtn.prop = g.prtn.prop + ggtitle("Proportion of Spousal Partnerships")+ylab("")
	return(list(g.prtn,g.prtn.prop))
}


# ==== CIRUMCISED PROPORTION ====
plot.circum <- function(simul)
{
	simul$pc = simul$nCircum/simul$nAlive
	
	simul2 = ddply(simul,.variables = "time",summarize,
				   pc.avg = mean(pc),
				   pc.min = min(pc),
				   pc.max = max(pc)
	)
	
	g.circ = ggplot(simul2, aes(x=time))+geom_line(aes(y=pc.avg),size=1)
	g.circ = g.circ + geom_ribbon(aes(ymin=pc.min,ymax=pc.max),alpha=0.2)
	g.circ = g.circ + ggtitle("Proportion of circumcised in Population")+ylab("")
	return(g.circ)
}


#############################


plot.CSW <-function(simul)
{
	simul$CSWprop = simul$nCSW/simul$nAlive
	
	simul2 = ddply(simul,.variables = "time",summarize,
				   CSWprop.avg = mean(CSWprop),
				   CSWprop.min = min(CSWprop),
				   CSWprop.max = max(CSWprop)
	)
	
	g.csw = ggplot(simul2, aes(x=time))+geom_line(aes(y=CSWprop.avg),size=1)
	g.csw = g.csw + geom_ribbon(aes(ymin=CSWprop.min,ymax=CSWprop.max),alpha=0.2)
	g.csw = g.csw + ggtitle("Proportion of CSW in Population")+ylab("")
	return(g.csw)
}


# ==== DEGREE DISTRIBUTION =====

plot.degree.dist <- function(degreeDist)
{
	degreeDist.m = melt(degreeDist,id.vars = c("time","iMC"))
	
	degreeDist2 = ddply(degreeDist.m,.variables = c("time","variable"),summarize,
						P.avg = mean(value),
						P.min = min(value),
						P.max = max(value)
	)
	
	g.deg = ggplot(degreeDist2)+geom_line(aes(x=time,y=P.avg,colour=variable))
	g.deg = g.deg + geom_ribbon(aes(x=time,ymin=P.min,ymax=P.max,fill=variable),alpha=0.2)
	g.deg = g.deg + ggtitle("Degree Time Series")+ylab("")
	return(g.deg)
}


