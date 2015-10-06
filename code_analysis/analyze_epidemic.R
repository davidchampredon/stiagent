##########################################################
#### 
#### ANALYZE EPIDEMIC FROM SIMULATION (several MC iterations)
####
##########################################################


library(ggplot2) ; theme_set(theme_bw())
library(plyr)
library(gridExtra)

source("analyze_UTILS.R")
source("analyze_simulation_FCT.R")
source("analyze_epidemic_FCT.R")
source("analyze_partnerships_FCT.R")

save.to.file <- TRUE

# Directory where the output files are saved
DIR_OUT = "../OUT/"
DIR_INPUTS = "../"
if(save.to.file) pdf("analyze_epidemic.pdf",width=20,height=15)

# Simulation time series
nMC = as.integer(system("ls -l ../OUT/simul_mc*.out | wc -l", intern = TRUE))
simul = read.simulation.file("simul_mc")

# Last population from all MC iterations
last.pop.fname <- system(paste0("ls ",DIR_OUT,"last_population_job*.out"), intern=T)
last.pop <- merge.last.pop(last.pop.fname)
last.pop <- add.STI.flag(last.pop)  

### === Clean-up ===

simul <- cleanup.file(simul)

stinames = read.table(file = paste0(DIR_OUT,"stinames.out"),stringsAsFactors = FALSE,quote = )$V1
col.sti <- which(names(simul) %in% stinames)
tmp.df <- simul[,col.sti]
toremove = which(tmp.df$HIV=="HIV")

simul$nAllSTI <- rowSums(tmp.df)

# Remove MC iteration where at least one STI goes extinct:
idx.extinct <- find.MC.STI.extinction(simul,stinames)
if(length(idx.extinct)>0){
simul <- subset(simul,subset=!(iMC %in% idx.extinct))
last.pop <- subset(last.pop,subset=!(iMC %in% idx.extinct))
}

last.pop$STIinfection <- STIinfection.label(last.pop)


### ===== PLOTS ===== ###

g.sti = plot.STI_prev_timeseries(simul, stinames)
g.costi = plot.STI_prev_timeseries(simul, "nHIVTp")
g.costi.rsk = plot.coSTI.riskgroup(simul)

g.rg.hiv = plot.STIprev_riskgroup(simul,"HIV")
g.rg.tp = plot.STIprev_riskgroup(simul,"Tp")

g.csw.hiv = plot.STIprev_CSW(simul,"HIV")
g.csw.tp = plot.STIprev_CSW(simul,"Tp")

g.hivprev.rskgrp = plot.HIVprev.risk.time(simul,FALSE)
g.tpprev.rskgrp = plot.TPprev.risk.time(simul,FALSE)

g.hiv.inc <- plot.STIincid.HIV()
g.tp.inc <- plot.STIincid.Tp()

g.Reff <- plot.Reff(simul)

g.sti.lftprtn <- plot.STI.lftprtn(last.pop)
g.sti.age <- plot.STI.age(last.pop)
g.symptom <- plot.symptomatic(last.pop)

grid.arrange(#ncol=3,
             g.sti,
             g.hiv.inc,
             g.tp.inc,
             g.costi,
             g.costi.rsk,
             g.hivprev.rskgrp,
             g.tpprev.rskgrp,
             g.Reff,
             g.sti.lftprtn,
             g.sti.age,
             g.symptom)

# grid.arrange(#ncol=3,
#              g.rg.hiv,
#              g.rg.tp,
#              g.csw.hiv[[1]],
#              g.csw.hiv[[2]]
# )


if(save.to.file) dev.off()