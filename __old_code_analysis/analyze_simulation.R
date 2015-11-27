##########################################################
#### TIME SERIES OF VARIABLES FROM SIMULATION
#### 
#### Manages several Monte Carlo runs
####
##########################################################


library(ggplot2) ; theme_set(theme_bw())
library(plyr)
library(gridExtra)

source("analyze_simulation_FCT.R")
source("analyze_UTILS.R")

save.to.file <- TRUE

# Directory where the output files are saved
DIR_OUT = "../outputs/"
DIR_INPUTS = "../inputs/"

if(save.to.file) pdf("analyze_simulation.pdf",width=20,height=15)

nMC = as.integer(system(paste0("ls -l ",DIR_OUT,"simul_mc*.out | wc -l"), intern = TRUE))
simul = read.simulation.file("simul_mc")

# Clean-up
simul <- cleanup.file(simul)

stinames = read.table(file = paste0(DIR_OUT,"stinames.out"),stringsAsFactors = FALSE,quote = )$V1
col.sti <- which(names(simul) %in% stinames)
tmp.df <- simul[,col.sti]
toremove = which(tmp.df$HIV=="HIV")

simul$nAllSTI <- rowSums(tmp.df)

# Remove MC iteration where at least one STI goes extinct:
remove.extinct <- FALSE
idx.extinct <- find.MC.STI.extinction(simul,stinames)
if(length(idx.extinct)>0 & remove.extinct) simul <- subset(simul,subset=!(iMC %in% idx.extinct))

### DEBUG
simul$tot <- with(simul,nRskGrp0+nRskGrp1+nRskGrp2+nCSW)
sum(simul$tot!=simul$nAlive)


# ====== PLOTS =======

g.pop = plot.population(simul)
g.sexratio = plot.sexratio(simul)
g.newb = plot.newborn(simul)
g.partn = plot.partnerships(simul)
g.csw = plot.CSW(simul)

grid.arrange(g.pop[[1]],g.pop[[2]],
             g.sexratio,
             g.newb,
			 g.partn[[1]],g.partn[[2]],
			 g.csw)

if(save.to.file) dev.off()
