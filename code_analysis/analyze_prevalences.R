##########################################################
#### 
#### ANALYZE PREVALENCE ONLY FROM SIMULATIONS (several MC iterations)
####
##########################################################


library(ggplot2) ; theme_set(theme_bw())
library(plyr)
library(gridExtra)

source("analyze_UTILS.R")
source("analyze_simulation_FCT.R")

save.to.file <- TRUE

# Directory where the output files are saved
DIR_OUT = "../OUT/"
DIR_INPUTS = "../"
if(save.to.file) pdf("analyze_prevalences.pdf",width=20,height=15)

# Simulation time series
nMC = as.integer(system("ls -l ../OUT/simul_mc*.out | wc -l", intern = TRUE))
simul = read.simulation.file("simul_mc")
### === Clean-up ===
simul <- cleanup.file(simul)
stinames = read.table(file = paste0(DIR_OUT,"stinames.out"),stringsAsFactors = FALSE,quote = )$V1

# Remove MC iteration where at least one STI goes extinct:
remove.extinct <- FALSE
idx.extinct <- find.MC.STI.extinction(simul,stinames)
if(length(idx.extinct)>0 & remove.extinct){
  simul <- subset(simul,subset=!(iMC %in% idx.extinct))
}

simul$HIVprev <- simul$HIV/simul$nAlive
simul$Tpprev <- simul$Tp/simul$nAlive

thetitle = paste0("Mean prevalences ; nMC=",nMC, " ; nExtinct=",length(idx.extinct))

simul.m = melt(simul,measure.vars = c("HIVprev","Tpprev"))
s2 = ddply(simul.m,c("time","variable"), summarize, mp=mean(value) )
g = ggplot(s2)+geom_line(aes(x=time, y=mp, colour=variable),size=2) + ggtitle(thetitle)
plot(g)
